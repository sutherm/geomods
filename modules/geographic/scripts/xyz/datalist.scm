;;; datalist.scm - concatenate datalists.--------------------
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Usage: datalist [ -ghlv -C cmd -H zmin/zmax -L /path/to/datalists -R xmin/xmax/ymin/ymax ] [ file ]
;;
;; Concatenate, filter, glob, thunk and snarf datalists.
;;
;; d a t a - (hook ...) - l i s t
;;
;; -l - glob the xyz-files in the current-directory to a datalist.
;; This dumps to std-out. Push it to a .datalist file if you want to save it.
;;
;; -L - glob the data-lists found in the given directory to a recursive datalist.
;;
;; If an infos-blob exists for a any of the xyz files found in the data-list. 
;; The -R option will prompt us to read that file and determine if the xyz file is even worth opening. 
;; note: (use xyz snarf to create an infos-blob for a specific dataset).
;;
;; Use the -g switch to glob the xyz files contained in the datalist to stdout:
;; e.g. datalist -g test.datalist
;; ../data/filename1.xyz 168
;; ../data/filename2.xyz 168
;; ...
;; ../data/processed/filename444.xyz 168
;;
;; Use the -C switch to traverse through the datalist and run a system command on each 
;; xyz file found therein.
;; e.g.
;;
;; To run GMT's blockmedian command on every xyz file found in test.datalist:
;; datalist test.datalist -C "gmt blockmedian ~a -I.1111111s \$(gmt gmtinfo ~a -I-) > ~a_bm.xyz"
;; where the `~a` represents the xyz filename.
;;
;; To generate a vector of the boundaries of all the files found in a datalist:
;; datalist test.datalist -C "gms xyz-snarf -x ~a >> test_hulls.gmt"
;;
;; Zip up all the data files:
;; datalist test.datalist -C "gzip ~a"
;; Unzip all the data files:
;; datalist test.datalist -C "gunzip ~a.gz"
;;
;; The -G switch will include datalist support for GDAL supported grid files.
;; Adding -G will include GDAL files in all other operations, if they exist.
;; note: use 200 for the GDAL file code
;; e.g. datalist -g test.datalist
;; ../data/filename1.xyz 168
;; ../data/filename2.xyz 168
;; ../data/processed/filename001.tif 200
;; ../data processed/filename002.tif 200
;; note: info-blobs aren't needed for gdal files as we can scan that information quickly on-the-fly.
;; note: nodata values are witheld when dumping the data from the GDAL file.
;;
;; Running with no switches (or just -G) will dump all the xyz data found in the datalist.
;;
;; The -I switch, in conjunction with the -R switch, will send the xyz data found in the datalist
;; through GMT's blockmedian command, using the given region and increment.
;; e.g. '-R -70/-60/10/20 -I 1s' will send the data through the blockmedian command:
;; gmt blockmedian -R-70/-60/10/20 -I1s -V
;;
;; Make a hook and put it in your .xyz file to run custom hooks on the datalist.
;;
;;; Code:

(define-module (geographic scripts xyz datalist)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (xyz xyz)
  #:use-module (xyz datalists)
  #:use-module (xyz infos)
  #:use-module (geographic regions)
  #:use-module (geographic spatial)
  #:use-module (geographic util regex)
  #:use-module (geographic dem gdal)
  #:use-module (geographic dem gmt)
  #:export (datalist))

(define datalist-version "0.2.9")

(define %summary "Hook into a datalist.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (cmd (single-char #\C) (value #t))
    (glob (single-char #\g) (value #f))
    (glob-list (single-char #\l) (value #f))
    (glob-directory (single-char #\L) (value #t))
    (height (single-char #\H) (value #t))
    (block (single-char #\I) (value #t))
    (gdal (single-char #\G) (value #f))
    (region (single-char #\R) (value #t))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t a - (hook ...) - l i s t

usage: datalist [ -ghlvCGILR [args] ] [ files ]
" %summary))

;; Display version information
(define (display-version)
  (format #t "\
datalist (XYZ) ~a
Copyright (C) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" datalist-version))

;; Run datalist with user args
(define (datalist . args)
  (let ((options (getopt-long (cons "datalist" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (cmd (option-ref options 'cmd #f))
	  (glob (option-ref options 'glob #f))
	  (datalist-wanted (option-ref options 'glob-list #f))
	  (datadir-wanted (option-ref options 'glob-directory #f))
	  (height (option-ref options 'height #f))
	  (block-wanted (option-ref options 'block #f))
	  (gdal-wanted (option-ref options 'gdal #f))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (datalist-wanted 
	(glob->datalist))
       (datadir-wanted 
	(display datadir-wanted))
       ((and block-wanted (not region))
	(display-help))
       (else
	(let* ((input (option-ref options '() #f))
	       (invert #f)
	       (region-list
		(if region (gmt-region->region region) #f))
	       (file-in-region? 
		(lambda (xyz-file)
		  (if (and region-list (file-exists? (string-append xyz-file ".scm")))
		      (let ((infos (read (open-file (string-append xyz-file ".scm") "r"))))
			(if (not (pair? infos)) #f
			    (if (region-inside-region? (infos->region infos) region-list)
				#t #f)))
		      #t)))
	       (gdal-in-region?
		(lambda (gdal-file)
		  (if region-list
		      (let ((gdal-region (gdal->region gdal-file)))
			(if (region-inside-region? gdal-region region-list)
			    #t #f))
		      #t))))
				 
	  (define glob-datalist-hook
	    (lambda (xyz-file) 
	      (if (file-in-region? xyz-file)
		  (format #t "~a 168\n" xyz-file))))

	  (define glob-datalist-gdal-hook
	    (lambda (gdal-file) 
	      (if (gdal-in-region? gdal-file)
		  (format #t "~a 200\n" gdal-file))))	  

	  (define cmd-datalist-hook
	    (lambda (xyz-file) 
	      (if (file-in-region? xyz-file)
		  (begin
		    (format #t "thunking ~a~%" (basename xyz-file))
		    (let* ((ta-match (match:replace (string-match "~a" cmd) xyz-file))
			   (sys-cmd (string-append "bash -c \"" 
						   (if (string? ta-match) ta-match "") 
						   "\"")))
		      (system sys-cmd))))))

	  (define blockmedian-datalist-hook
	    (lambda (xyz-file)
	      (if (file-in-region? xyz-file)
		  (begin
		    (format (current-error-port) "blockmedianing ~a~%" xyz-file)
		    (gmt-cmd->scm 
		     (string-append "gmt blockmedian -I" block-wanted " -R" (region->gmt-region region-list) " -V ") 
		     (xyz->port (open-file xyz-file "r")))))))

	  (define blockmedian-datalist-gdal-hook
	    (lambda (gdal-file)
	      (if (gdal-in-region? gdal-file)
		  (begin
		    (format (current-error-port) "blockmedianing ~a~%" gdal-file)
		    (gmt-cmd->port
		     (string-append "gmt blockmedian -I" block-wanted " -R" (region->gmt-region region-list) " -V ") 
		     (gdal->port gdal-file))))))

	  (define dump-datalist-hook
	    (lambda (xyz-file)		
	      (if (file-in-region? xyz-file)
		  (begin
		    (format (current-error-port) "snarfing ~a~%" xyz-file)
		    (xyz-concat (open-file xyz-file "r"))))))

	  (define dump-datalist-gdal-hook
	    (lambda (gdal-file)		
	      (if (gdal-in-region? gdal-file)
		  (begin
		    (format (current-error-port) "snarfing ~a~%" gdal-file)
		    (gdal2xyz gdal-file)))))

	  ;; Reset the datalist hook. We'll be setting our own.
	  (reset-hook! %data-list-hook)
	  (reset-hook! %data-list-gdal-hook)

	  ;; If there appears to be a file mentioned, hook into the datalist.
	  (if (pair? input)
	      (let loop ((infile (open-file (car input) "r")))
		(cond
		 ;; glob the files from the datalist(s) to std-out.
		 (glob
		  (add-hook! %data-list-hook glob-datalist-hook)
		  (if gdal-wanted
		      (add-hook! %data-list-gdal-hook glob-datalist-gdal-hook)))
		 ;; run cmd on each file in the datalist.
		 (cmd
		  (add-hook! %data-list-hook cmd-datalist-hook)
		  (if gdal-wanted
		      (add-hook! %data-list-gdal-hook cmd-datalist-hook)))
		 ;; dump the xyz data from the datalist through gmt blockmedian
		 (block-wanted
		  (add-hook! %data-list-hook blockmedian-datalist-hook)
		  (if gdal-wanted
		      (add-hook! %data-list-gdal-hook blockmedian-datalist-gdal-hook)))
		 ;; dump the xyz data from the datalist
		 (else
		  (add-hook! %data-list-hook dump-datalist-hook)
		  (if gdal-wanted
		      (add-hook! %data-list-gdal-hook dump-datalist-gdal-hook))))
		(data-list infile)
		(close infile)
		;; if there appears to be another file mentioned, hook into that datalist as well.
		(if (pair? (cdr input))
		    (loop (open-file (cadr input) "r"))))
	      (display-help))))))))

(define main datalist)
	      
;; Dump a file line-by-line.
(define* (xyz-concat port #:optional (oport (current-output-port)))
  "Dump port to oport by line."
  (while (not (eof-object? (peek-char port)))
	 (display (read-line port 'concat) oport)))

;;; End

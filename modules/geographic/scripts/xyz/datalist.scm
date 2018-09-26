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
;;; Todo:
;;
;; make datalist it's own script set and move it out of xyz.
;; add laz/las support.
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
;; note: (MB-System .inf files are also supported).
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
;; Running with no switches will dump all the data found in the datalist as xyz.
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
  #:use-module (geographic dem lastools)
  #:export (datalist))

(define datalist-version "0.2.10")

(define %summary "Hook into a datalist.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (cmd (single-char #\C) (value #t))
    (glob (single-char #\g) (value #f))
    (glob-list (single-char #\l) (value #f))
    (glob-directory (single-char #\L) (value #t))
    (weight (single-char #\W) (value #t))
    (region (single-char #\R) (value #t))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t a - (hook ...) - l i s t

usage: datalist [ -ghlvCLRW [args] ] [ files ]
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
	  (weight (option-ref options 'weight 1))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (datalist-wanted 
	(glob->datalist #:weight weight))
       (datadir-wanted 
	(display datadir-wanted))
       (else
	(let* ((input (option-ref options '() #f))
	       (invert #f)
	       (region-list
		(if region (gmt-region->region region) #f))
	       (file-in-region? 
		(lambda (xyz-file)
		  (if (and region-list (file-exists? (string-append xyz-file ".inf")))
		      (let ((mbinfos (inf->infos (open-file (string-append xyz-file ".inf") "r")))
			    (infos (read (open-file (string-append xyz-file ".inf") "r"))))
			;;(let ((infos (read (open-file (string-append xyz-file ".scm") "r"))))
			(if (pair? mbinfos)
			    (if (region-inside-region? (infos->region mbinfos) region-list)
				#t #f)
			    (if (region-inside-region? (infos->region infos) region-list)
				#t #f)))
		      #t)))
	       (gdal-in-region?
		(lambda (gdal-file)
		  (if region-list
		      (let ((gdal-region (gdal->region gdal-file)))
			(if (region-inside-region? gdal-region region-list)
			    #t #f))
		      #t)))
	       (las-in-region?
		(lambda (las-file)
		  (if region-list
		      (let ((las-region (las->region las-file)))
			(if (region-inside-region? las-region region-list)
			    #t #f))
		      #t))))
				 
	  (define format-datalist-hook
	    (lambda (xyz-file weight) 
	      (if (file-in-region? xyz-file)
		  (format #t "~a 168 ~a\n" xyz-file weight))))

	  (define glob-datalist-gdal-hook
	    (lambda (gdal-file weight) 
	      (if (gdal-in-region? gdal-file)
		  (format #t "~a 200 ~a\n" gdal-file weight))))

	  (define glob-datalist-las-hook
	    (lambda (las-file weight) 
	      (if (las-in-region? las-file)
		  (format #t "~a 300 ~a\n" las-file weight)))) 

	  (define cmd-datalist-hook
	    (lambda (xyz-file weight) 
	      (if (file-in-region? xyz-file)
		  (begin
		    (format #t "datalist: thunking ~a~%" (basename xyz-file))
		    (let* ((ta-match (match:replace (string-match "~a" cmd) xyz-file))
			   (sys-cmd (string-append "bash -c \"" 
						   (if (string? ta-match) ta-match "") 
						   "\"")))
		      (system sys-cmd))))))

	  (define cmd-datalist-gdal-hook
	    (lambda (gdal-file weight) 
	      (if (gdal-in-region? gdal-file)
		  (begin
		    (format #t "datalist: thunking ~a~%" (basename gdal-file))
		    (let* ((ta-match (match:replace (string-match "~a" cmd) gdal-file))
			   (sys-cmd (string-append "bash -c \"" 
						   (if (string? ta-match) ta-match "") 
						   "\"")))
		      (format (current-error-port) "datalist: ~a~%" sys-cmd)
		      (system sys-cmd))))))
	  
	  (define dump-datalist-hook
	    (lambda (xyz-file weight)
	      (if (file-in-region? xyz-file)
		  (if region-list
		      (xyz->port (open-file xyz-file "r") #:verbose #t #:weight weight #:test-fun (lambda (xyz) (xyz-inside-region? xyz region-list)))
		      (xyz->port (open-file xyz-file "r") #:weight weight)))))
	
	  (define dump-datalist-gdal-hook
	    (lambda (gdal-file weight)		
	      (if (gdal-in-region? gdal-file)
		  (if region-list
		      (gdal2xyz gdal-file #:infos (acons 'name gdal-file '()) #:weight weight #:verbose #t #:test-fun (lambda (xyz) (xyz-inside-region? xyz region-list)))
		      (gdal2xyz gdal-file #:weight weight)))))

	  (define dump-datalist-las-hook
	    (lambda (las-file weight)		
	      (if (las-in-region? las-file)
		  (if region-list
		      (las->xyz las-file #:infos (acons 'name las-file '()) #:weight weight #:verbose #t #:test-fun (lambda (xyz) (xyz-inside-region? xyz region-list)))
		      (las->xyz las-file #:weight weight)))))
	
	  ;; Reset the datalist hook. We'll be setting our own.
	  (reset-hook! %data-list-hook)
	  (reset-hook! %data-list-gdal-hook)
	  (reset-hook! %data-list-las-hook)

	  ;; If there appears to be a file mentioned, hook into the datalist.
	  (if (pair? input)
	      (let loop ((infile (open-file (car input) "r")))
		(cond
		 ;; glob the files from the datalist(s) to std-out.
		 (glob
		  (add-hook! %data-list-hook format-datalist-hook)
		  (add-hook! %data-list-gdal-hook glob-datalist-gdal-hook)
		  (add-hook! %data-list-las-hook glob-datalist-las-hook))
		 ;; run cmd on each file in the datalist.
		 (cmd
		  (add-hook! %data-list-hook cmd-datalist-hook)
		  (if gdal-wanted
		      (add-hook! %data-list-gdal-hook cmd-datalist-gdal-hook)))
		 ;; dump the xyz data from the datalist
		 (else
		  (add-hook! %data-list-hook dump-datalist-hook)
		  (add-hook! %data-list-gdal-hook dump-datalist-gdal-hook)
		  (add-hook! %data-list-las-hook dump-datalist-las-hook)))
		(data-list infile #:weight (if weight weight))
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

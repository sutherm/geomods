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
  #:export (datalist))

(define datalist-version "0.2.6")

(define %summary "Hook into a datalist.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (cmd (single-char #\C) (value #t))
    (glob (single-char #\g) (value #f))
    (glob-list (single-char #\l) (value #f))
    (glob-directory (single-char #\L) (value #t))
    (height (single-char #\H) (value #t))
    (region (single-char #\R) (value #t))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t a - (hook ...) - l i s t

usage: datalist [ -ghlvCLR [args] ] [ files ]
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
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (datalist-wanted 
	(glob->datalist))
       (datadir-wanted 
	(display datadir-wanted))
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
				#t #f)) #t)))))
	  
	  ;; Reset the datalist hook. We'll be setting our own.
	  (reset-hook! %data-list-hook)
	  
	  ;; If there appears to be a file mentioned, hook into the datalist.
	  (if (pair? input)
	      (let loop ((infile (open-file (car input) "r")))
		(cond
		 ;; glob the files from the datalist(s) to std-out.
		 (glob
		  (add-hook! %data-list-hook 
			     (lambda (xyz-file) 
			       (if (file-in-region? xyz-file)
				   (format #t "~a 168\n" xyz-file))))
		  (data-list infile))
		 ;; run cmd on each file in the datalist.
		 (cmd
		  (add-hook! %data-list-hook 
			     (lambda (xyz-file) 
			       (if (file-in-region? xyz-file)
				   (begin
				     (format #t "thunking ~a~%" (basename xyz-file))
				     (let* ((ta-match (match:replace (string-match "~a" cmd) xyz-file))
					    (sys-cmd (string-append "bash -c \"" 
								    (if (string? ta-match) ta-match "") 
								    "\"")))
				       (system sys-cmd))))))
		  (data-list infile))
		 (else
		  (add-hook! %data-list-hook
			     (lambda (xyz-file)		
			       (if (file-in-region? xyz-file)
				   (begin
				     ;;(format (current-error-port) "snarfing ~{~a~^ ~}~%" (infos->list infos))
				     (format (current-error-port) "snarfing ~a~%" xyz-file)
				     (xyz-concat (open-file xyz-file "r"))))))
		  (data-list infile)))
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

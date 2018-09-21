;;; cat.scm - concatenate datalists.--------------------
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
;; Usage: datalist cat [ -hv [ -R xmin/xmax/ymin/ymax [ -I increment ] ] ] [ file ]
;;
;; Concatenate the data files found in a datalist.
;;
;; d a t l i s t C A T
;;
;; If an infos-blob exists for a any of the xyz files found in the data-list. 
;; The -R option will prompt us to read that file and determine if the xyz file is even worth opening. 
;; note: (use xyz snarf to create an infos-blob for a specific dataset).
;;
;; The -I switch, in conjunction with the -R switch, will send the xyz data found in the datalist
;; through GMT's blockmedian command, using the given region and increment.
;; e.g. '-R -70/-60/10/20 -I 1s' will send the data through the blockmedian command:
;; gmt blockmedian -R-70/-60/10/20 -I1s -V
;;
;; Make a hook and put it in your .xyz file to run custom hooks on the datalist.
;;
;;; Code:

(define-module (geographic scripts datalist cat)
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
  #:use-module (geographic dem mbio)
  #:use-module (geographic dem lastools)
  #:use-module (geographic dem gmt)
  #:export (datalist))

(define datalist-version "0.1.0")

(define %summary "concatenate the data files found in a datalist.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (block (single-char #\I) (value #t))
    (region (single-char #\R) (value #t))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t l i s t C A T

usage: datalist cat [ -hvIR [args] ] [ files ]
" %summary))

;; Display version information
(define (display-version)
  (format #t "\
cat (DATALIST) ~a
Copyright (C) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" datalist-version))

;; Run datalist with user args
(define (cat . args)
  (let ((options (getopt-long (cons "cat" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (block-wanted (option-ref options 'block #f))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       ((and block-wanted (not region))
	(display-help))
       (else
	(let* ((input (option-ref options '() #f))
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
		      #t)))
	       (mbio-in-region?
		(lambda (mbio-file)
		  (if region-list
		      (let ((mb-region (mb->region mbio-file)))
			(if (region-inside-region? mb-region region-list)
			    #t #f))
		      #t)))
	       (las-in-region?
		(lambda (las-file)
		  (if region-list
		      (let ((las-region (las->region las-file)))
			(if (region-inside-region? las-region region-list)
			    #t #f))
		      #t))))
				 
	  (define blockmedian-datalist-hook
	    (lambda (xyz-file)
	      (if (file-in-region? xyz-file)
		  (begin
		    (format (current-error-port) "datalist: blocking ~a~%" xyz-file)
		    (gmt-cmd->scm 
		     (string-append "gmt blockmedian -I" block-wanted " -R" (region->gmt-region region-list)) 
		     (xyz->port (open-file xyz-file "r")))))))

	  (define blockmedian-datalist-gdal-hook
	    (lambda (gdal-file)
	      (if (gdal-in-region? gdal-file)
		  (begin
		    (format (current-error-port) "datalist: blocking ~a~%" gdal-file)
		    (gmt-cmd->port
		     (string-append "gmt blockmedian -I" block-wanted " -R" (region->gmt-region region-list)) 
		     (gdal->port gdal-file))))))

	  (define blockmedian-datalist-las-hook
	    (lambda (las-file)
	      (if (las-in-region? las-file)
		  (begin
		    (format (current-error-port) "datalist: blocking ~a~%" las-file)
		    (gmt-cmd->port
		     (string-append "gmt blockmedian -I" block-wanted " -R" (region->gmt-region region-list)) 
		     (las->port las-file))))))

	  (define dump-datalist-hook
	    (lambda (xyz-file)		
	      (if (file-in-region? xyz-file)
		  (begin
		    (format (current-error-port) "datalist: concatenating ~a~%" xyz-file)
		    (xyz-concat (open-file xyz-file "r"))))))

	  (define dump-datalist-gdal-hook
	    (lambda (gdal-file)		
	      (if (gdal-in-region? gdal-file)
		  (begin
		    (format (current-error-port) "datalist: concatenating ~a~%" gdal-file)
		    (gdal2xyz gdal-file)))))

	  (define dump-datalist-mbio-hook
	    (lambda (mbio-file)		
	      (if (mbio-in-region? mbio-file)
		  (begin
		    (format (current-error-port) "datalist: concatenating ~a~%" mbio-file)
		    (mb->xyz mbio-file)))))

	  (define dump-datalist-las-hook
	    (lambda (las-file)		
	      (if (las-in-region? las-file)
		  (begin
		    (format (current-error-port) "datalist: concatenating ~a~%" las-file)
		    (las->xyz las-file)))))

	  ;; Reset the datalist hook. We'll be setting our own.
	  (reset-hook! %data-list-hook)
	  (reset-hook! %data-list-gdal-hook)
	  (reset-hook! %data-list-las-hook)

	  ;; If there appears to be a file mentioned, hook into the datalist.
	  (if (pair? input)
	      (let loop ((infile (open-file (car input) "r")))
		(cond
		 ;; dump the xyz data from the datalist through gmt blockmedian
		 (block-wanted
		  (add-hook! %data-list-hook blockmedian-datalist-hook)
		  (add-hook! %data-list-gdal-hook blockmedian-datalist-gdal-hook)
		  (add-hook! %data-list-las-hook blockmedian-datalist-las-hook))
		 ;; dump the xyz data from the datalist
		 (else
		  (add-hook! %data-list-hook dump-datalist-hook)
		  (add-hook! %data-list-gdal-hook dump-datalist-gdal-hook)
		  (add-hook! %data-list-mbio-hook dump-datalist-mbio-hook)
		  (add-hook! %data-list-las-hook dump-datalist-las-hook)))
		(data-list infile)
		(close infile)
		;; if there appears to be another file mentioned, hook into that datalist as well.
		(if (pair? (cdr input))
		    (loop (open-file (cadr input) "r"))))
	      (display-help))))))))

(define main cat)
	      
;; Dump a file line-by-line.
(define* (xyz-concat port #:optional (oport (current-output-port)))
  "Dump port to oport by line."
  (while (not (eof-object? (peek-char port)))
	 (display (read-line port 'concat) oport)))

;;; End

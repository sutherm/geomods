;;; info.scm - get some datalist info.--------------------
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
;; Usage: datalist info [ -dhv [ -R xmin/xmax/ymin/ymax ] ] [ file ]
;;
;; Get some datalist info.
;;
;; d a t l i s t I N F O
;;
;;; Code:

(define-module (geographic scripts datalist info)
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
  #:use-module (geographic dem lastools)
  #:use-module (geographic dem gmt)
  #:export (datalist))

(define datalist-version "0.1.0")

(define %summary "get some datalist info.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (dump (single-char #\d) (value #f))
    (region (single-char #\R) (value #t))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t l i s t I N F O

usage: datalist info [ -dhvR [args] ] [ files ]
" %summary))

;; Display version information
(define (display-version)
  (format #t "\
info (DATALIST) ~a
Copyright (C) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" datalist-version))

;; Run datalist with user args
(define (info . args)
  (let ((options (getopt-long (cons "info" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (dump (option-ref options 'dump #f))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let* ((input (option-ref options '() #f))
	       (wc 0)
	       (gwc 0)
	       (lwc 0)
	       (dwc 0)
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
	       (las-in-region?
		(lambda (las-file)
		  (if region-list
		      (let ((las-region (las->region las-file)))
			(if (region-inside-region? las-region region-list)
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

	  (define glob-datalist-las-hook
	    (lambda (las-file) 
	      (if (las-in-region? las-file)
		  (format #t "~a 300\n" las-file))))

	  (define datalist-info-dl-hook
	    (lambda (dlf)
	      (set! dwc (+ 1 dwc))
	      (if (file-exists? dlf) 
		  (data-list (open-file dlf "r")))))

	  (define datalist-info-hook
	    (lambda (dlf)
	      (set! wc (+ 1 wc))))

	  (define datalist-info-gdal-hook
	    (lambda (dlf)
	      (set! gwc (+ 1 gwc))))

	  (define datalist-info-las-hook
	    (lambda (dlf)
	      (set! lwc (+ 1 lwc))))

	  ;; Reset the datalist hook. We'll be setting our own.
	  (reset-hook! %data-list-hook)
	  (reset-hook! %data-list-gdal-hook)
	  (reset-hook! %data-list-las-hook)

	  ;; If there appears to be a file mentioned, hook into the datalist.
	  (if (pair? input)
	      (let loop ((infile (open-file (car input) "r")))
		(cond
		 ;; glob the files from the datalist(s) to std-out.
		 (dump
		  (add-hook! %data-list-hook glob-datalist-hook)
		  (add-hook! %data-list-gdal-hook glob-datalist-gdal-hook)
		  (add-hook! %data-list-las-hook glob-datalist-las-hook)))
		;(else
		(reset-hook! %data-list-dl-hook)
		(add-hook! %data-list-dl-hook datalist-info-dl-hook)
		(add-hook! %data-list-hook datalist-info-hook)
		(add-hook! %data-list-gdal-hook datalist-info-gdal-hook)
		(add-hook! %data-list-las-hook datalist-info-las-hook)
		(data-list infile)
		(close infile)
		(format (current-error-port) "~a: ~d datalist files~%" (car input) dwc)
		(format (current-error-port) "~a: ~d xyz files~%" (car input) wc)
		(format (current-error-port) "~a: ~d gdal files~%" (car input) gwc)
		(format (current-error-port) "~a: ~d lidar files~%" (car input) lwc)
		;; if there appears to be another file mentioned, hook into that datalist as well.
		(if (pair? (cdr input))
		    (loop (open-file (cadr input) "r"))))
	      (display-help))))))))

(define main info)
	      
;; Dump a file line-by-line.
(define* (xyz-concat port #:optional (oport (current-output-port)))
  "Dump port to oport by line."
  (while (not (eof-object? (peek-char port)))
	 (display (read-line port 'concat) oport)))

;;; End

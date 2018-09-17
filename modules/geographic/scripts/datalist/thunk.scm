;;; thunk.scm - concatenate datalists.--------------------
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
;; Concatenate a datalist.
;;
;; d a t l i s t T H U N K
;;
;;; Code:

(define-module (geographic scripts datalist thunk)
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

(define %summary "thunk the data files found in a datalist.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (cmd (single-char #\C) (value #t))
    (region (single-char #\R) (value #t))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t l i s t T H U N K

usage: datalist cat [ -hvCR [args] ] [ files ]
" %summary))

;; Display version information
(define (display-version)
  (format #t "\
thunk (DATALIST) ~a
Copyright (C) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" datalist-version))

;; Run datalist with user args
(define (thunk . args)
  (let ((options (getopt-long (cons "thunk" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (cmd (option-ref options 'cmd "ls -l ~a"))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let* ((input (option-ref options '() #f)))
	  (define cmd-datalist-hook
	    (lambda (xyz-file) 
	      (format #t "datalist: thunking ~a~%" (basename xyz-file))
	      (let* ((ta-match (match:replace (string-match "~a" cmd) xyz-file))
		     (sys-cmd (string-append "bash -c \"" 
					     (if (string? ta-match) ta-match "") 
					     "\"")))
		(system sys-cmd))))

	  ;; Reset the datalist hook. We'll be setting our own.
	  (reset-hook! %data-list-hook)
	  (reset-hook! %data-list-gdal-hook)
	  (reset-hook! %data-list-las-hook)

	  ;; If there appears to be a file mentioned, hook into the datalist.
	  (if (pair? input)
	      (let loop ((infile (open-file (car input) "r")))
		;; run cmd on each file in the datalist.
		(add-hook! %data-list-hook cmd-datalist-hook)
		(add-hook! %data-list-gdal-hook cmd-datalist-hook)
		(add-hook! %data-list-las-hook cmd-datalist-hook)
		(data-list infile)
		(close infile)
		;; if there appears to be another file mentioned, hook into that datalist as well.
		(if (pair? (cdr input))
		    (loop (open-file (cadr input) "r"))))
	      (display-help))))))))

(define main thunk)

;;; End

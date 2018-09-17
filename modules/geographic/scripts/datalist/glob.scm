;;; glob.scm - concatenate datalists.--------------------
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
;; Usage: datalist glob [ -hv ] [ file ]
;;
;; glob data into a datalist.
;;
;; d a t l i s t G L O B
;;
;;; Code:

(define-module (geographic scripts datalist glob)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (xyz datalists)
  #:export (datalist))

(define datalist-version "0.1.0")

(define %summary "glob files into a datalist")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))))

;; Display help infomation
(define (display-help)
  (format #t "\
~a
d a t l i s t G L O B

usage: datalist glob [ -hv [args] ] [ files ]
" %summary))

;; Display version information
(define (display-version)
  (format #t "\
glob (DATALIST) ~a
Copyright (C) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" datalist-version))

;; Run datalist with user args
(define (glob . args)
  (let ((options (getopt-long (cons "glob" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(glob->datalist))))))

(define main glob)
	      
;;; End

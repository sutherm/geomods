;;; tnm.scm - Fetch data from The National Map
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
;; Usage: tnm [ args ]
;;
;; Fetch data from The National Map
;;
;; try fetch tnm --help for more detailed usage information.
;;
;;; Code:

(define-module (geographic scripts fetch tnm)
  #:use-module (ice-9 getopt-long)
  #:export (tnm))

;; cxhull version number
(define tnm-version "0.0.1")

(define %summary "Fetch data from The National Map.")
(define %include-in-fetch-list #t)

(define command-synopsis
  '((version (single-char #\V) (value #f))))

;; Display help information
(define (display-help)
  (format #t "\
~a

usage: tnm [ -hvV [args] ] [ file ]
" %summary))

;; Display Version information
(define (display-version)
  (format #t "\
tnm (FETCH) version ~a

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" tnm-version))

;; gm-li mainline
(define (tnm . args)
  (system (string-append "tnmfetch.py " (string-join args " "))))

(define main tnm)
;;; End

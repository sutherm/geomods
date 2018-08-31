;;; nos.scm - Fetch NOS hydrographic data from NOAA
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
;; Usage: nos [ args ]
;;
;; Fetch NOS hydrographic data from NOAA
;;
;; try fetch nos --help for more detailed usage information.
;;
;;; Code:

(define-module (geographic scripts fetch nos)
  #:use-module (ice-9 getopt-long)
  #:export (nos))

;; cxhull version number
(define nos-version "0.0.1")

(define %summary "Fetch NOS hydrographic data from NOAA.")
(define %include-in-fetch-list #t)

(define command-synopsis
  '((version (single-char #\V) (value #f))))

;; Display help information
(define (display-help)
  (format #t "\
~a

usage: nos [ -hvV [args] ] [ file ]
" %summary))

;; Display Version information
(define (display-version)
  (format #t "\
nos (FETCH) version ~a

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" nos-version))

;; gm-li mainline
(define (nos . args)
  (system (string-append "nosfetch.py " (string-join args " "))))

(define main nos)
;;; End

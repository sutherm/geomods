;;; digital-coast.scm - Fetch data from NOAA's Digital Coast
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
;; Usage: digital-coast [ args ]
;;
;; Fetch data from NOAA's Digital Coast
;;
;; try fetch digital-coast --help for more detailed usage information.
;;
;;; Code:

(define-module (geographic scripts fetch digital-coast)
  #:use-module (ice-9 getopt-long)
  #:export (digital-coast))

;; cxhull version number
(define digital-coast-version "0.0.6")

(define %summary "Fetch data from NOAA's Digital Coast.")
(define %include-in-fetch-list #t)

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (list-only (single-char #\l) (value #f))
    (update (single-char #\u) (value #f))
    (index (single-char #\x) (value #f))
    (region (single-char #\R) (value #t))
    (filter (single-char #\f) (value #t))))

;; Display help information
(define (display-help)
  (format #t "\
~a

usage: digital-coast [ -hflRuvx [args] ]
" %summary))

;; Display Version information
(define (display-version)
  (format #t "\
digital-coast (FETCH) version ~a

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" digital-coast-version))

;; gm-li mainline
(define (digital-coast . args)
  (let ((options (getopt-long (cons "digtial-coast" args) command-synopsis 
			      #:stop-at-first-non-option #t)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (filter (option-ref options 'filter #f))
	  (region (option-ref options 'region #f))
	  (list-only (option-ref options 'list-only #f))
	  (index (option-ref options 'index #f))
	  (update (option-ref options 'update #f)))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(system (string-append 
		 "dcfetch.py " 
		 (if update "-u " "")
		 (if list-only "-l " "")
		 (if index "-x " "")
		 (if region (string-append "-R " region " ") "")
		 (if filter (string-append "-f \"" filter "\" ") ""))))))))

(define main digital-coast)
;;; End

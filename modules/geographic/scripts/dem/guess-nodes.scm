;;; guess-nodes.scm
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
;; Usage: guess-nodes [-L file]
;;
;; g u e s s - . . . - n o d e s
;;
;; Interpolate points along a line or multiple lines.
;; The input file should be a GMT formatted vector file where each node has a corresponding z value.
;; If no file is given, will ask for line values from standard input.
;;
;;; Code:

(define-module (geographic scripts dem guess-nodes)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (geodesic geod)
  #:use-module (geodesic formulae)
  #:use-module (xyz xyz)
  #:use-module (xyz infos)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic spatial))

;; cxhull version number
(define guess-nodes-version "0.0.2")

(define %include-in-dem-list #f)

(define %summary "Interpolate xyz points along a line/polygon.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (line (single-char #\L) (value #t))))

;; Display help information
(define (display-help)
  (format #t "\
~a
g u e s s - . . . - n o d e s

usage: guess-nodes [ -hvL [args] ] [ file ]
" %summary))

;; Display Version information
(define (display-version)
  (format #t "\
guess-nodes (GeoMODs) version ~a

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" guess-nodes-version))

;; guess-nodes mainline
(define (guess-nodes . args)
  (let ((options (getopt-long (cons "guess-nodes" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (line (option-ref options 'line #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(cond
	 ((ogr-gmt-file? line)
	  ;; from file GMT vector
	  (let ((gmtf (open-file line "r")))
	    (with-ogr-gmt-port gmtf
			       (lambda (x)
				 (let loop ((l x))
				   (let ((p1 (car l))
					 (p2 (cadr l)))
				     (xyz-display p1 (current-output-port))
				     (gds-interp (car p1) (cadr p1) (caddr p1) 
						 (car p2) (cadr p2) (caddr p2) 10 (current-output-port))
				     (if (pair? (cddr l))
					 (loop (cdr l))
					 (xyz-display p2 (current-output-port)))))))))
	 (else
	  (let ((l1 '())
		(l2 '())
		(d1 0)
		(output (current-error-port)))
	    (format #t "Welcome to GeoMODs!
Guess-Nodes xyz values along a line (2 points).
")
	    (format #t "point-1(x y z): ")
	    (set! l1 (xyz-read-line))
	    (format #t "point-2(x y z): ")
	    (set! l2 (xyz-read-line))
	    (format #t "distance(meters): ")
	    (set! d1 (string->number (read-line)))
	    (format #t "output(filename): ")
	    (set! output (open-file (read-line) "w"))
	    (if (not (= d1 0))
		(gds-interp (xyz:x l1) (xyz:y l1) (xyz:z l1) 
			    (xyz:x l2) (xyz:y l2) (xyz:z l2) d1 output))
	    (close-port output)))))))))

(define main guess-nodes)

;;; End

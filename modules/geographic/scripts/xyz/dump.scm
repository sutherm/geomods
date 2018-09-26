;;; dump.scm
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
;; Usage: xyz dump [ -hivDPT [ args ] ] [ file ]
;;
;; "x,y,z" -> '(d u m p)
;;
;; Using -T add a dump test for each xyz line. This should be scheme code that accepts a point '(x y ...) as an argument and returns #t or #f.
;; -T "(lambda (xyz) (> (caddr xyz) 0))" to return points with z values greater than zero (0).
;; -T "(lambda (xyz) (xyz-inside-region? xyz '(-67 -65 18 19)))" to return only points in the given region.
;;
;; You can also write a function in your .xyz file and use that.
;; e.g.
;; (define (z-is-nan xyz)
;;    (= z (nan)))
;; xyz dump -T z-is-nan
;;
;; Or combine many tests into one, as long as the function accepts an xyz point '(x y z ...) as input and returns #t or #f.
;;
;; With -P, dump xyz points inside the given polygon.
;; Snarf an xyz data set and dump the points inside the given polygon.
;; note: xyz data along the border of the polygon are considered outside.
;; note: only ogr-gmt formatted polygons are supported.
;;
;; -i will invert the test; returning points that fail, rather than pass the given test(s).
;; e.g. 
;; xyz dump file.xy -P my-poly.gmt -i
;; will return points from file.xy that fall outside the polygon(s) in my-poly.gmt
;;
;;; Code:

(define-module (geographic scripts xyz dump)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 rdelim)
  #:use-module (hulls convex-hull)
  #:use-module (xyz xyz)
  #:use-module (xyz datalists)
  #:use-module (geographic spatial)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (geographic dem lastools)
  #:use-module (geographic dem gdal)
  #:use-module (geographic dem mbio)
  #:export (dump))

(define dump-version "0.0.7")

(define %summary "Dump lines snarfed from xyz data.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (test (single-char #\T) (value #t))
    (invert (single-char #\i) (value #f))
    (poly (single-char #\P) (value #t))))

(define (display-help)
  (format #t "\
~a
\"x,y,z\" -> '(d u m p)

usage: dump [ hivDPT [ args ] ] [ file ]
" %summary))

(define (display-version)
  (format #t "\
dump (XYZ) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" dump-version))

(define (dump . args)
  (let* ((options (getopt-long (cons "dump" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (invert (option-ref options 'invert #f))
	  (test (option-ref options 'test #f))
	  (poly (option-ref options 'poly #f)))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let ((input (option-ref options '() #f)))
	  (let* ((infile (if (not (pair? input)) 
			     (current-input-port) 
			     (open-file (car input) "r")))
		 (ply-test 
		  (if poly 
		      (lambda (xyz)
			(let ((ply (ogr-gmt->scm (open-file poly "r"))))
			  (xyz-inside-multi-polygon? xyz ply)))
		      #f)))

	    (if ply-test
		(if test
		    (set! test (and (eval-string test) (lambda (xyz) (ply-test xyz))))
		    (set! test ply-test))
		(if test
		    (set! test (eval-string test))))

	    (let ((this-test (if invert (lambda (xyz) (not (test xyz))) test)))
	      (if (pair? input)
		  (cond 
		   ((find-data-entry (car input) las-exts)
		    (las->xyz (car input) (current-output-port)
			      #:test-fun (if test this-test test)))
		   ((find-data-entry (car input) gdal-exts)
		    (gdal2xyz (car input) (current-output-port)
			      #:test-fun (if test this-test test)))
		   (else
		    (xyz->port
		     infile (current-output-port)
		     #:test-fun (if test this-test test))))
		  ;; no file - read from stdin
		  (xyz->port
		   infile (current-output-port)
		   #:test-fun (if test this-test test)))))))))))

(define main dump)

;;; End

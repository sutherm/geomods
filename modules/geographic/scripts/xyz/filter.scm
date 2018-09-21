;; filter.scm - Glob xyz files in the current directory to a datalist.
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
;; Usage: filter [ -dfhrxuvT [ args ] ] [ files ]
;;
;; filter <-> infos
;;
;; -u - filter and dump the file delimiter.
;;
;; Using -T add a filter test for each xyz line. This should be scheme code that accepts a point '(x y ...) as an argument and returns #t or #f.
;; -T "(lambda (xyz) (> (caddr xyz) 0))" to return points with z values greater than zero (0).
;; -T "(lambda (xyz) (xyz-in-region? xyz '(-67 -65 18 19)))" to return only points in the given region.
;;
;; You can also write a function in your .xyz file and use that.
;; e.g.
;; (define (z-is-nan xyz)
;;    (= z (nan)))
;; filter -T z-is-nan
;;
;; Or even combine many tests into one, as long as the function accepts an xyz point '(x y z ...) as input and returns #t or #f.
;;
;; Any further filtering will be performed only on points that pass the test(s).
;;
;; -r - only return the x/y region information.
;;
;; -x - Find the convex-hull points from the x/y data.
;;
;; -d - Filter the density of the x/y data.
;;
;; By default, filter will return a scheme a-list to standard-output of standard-information.
;; '(name xmin xmax ymin ymax zmin zmax count)
;;
;; Use -f to format the output.
;; e.g.
;; xyz filter -rf file.xyz
;; ==> xmin/xmax/ymin/ymax
;;
;; for i in *.xyz; do if [ ! -f $i.scm ]; then xyz filter $i > $i.scm; fi; done
;;
;;; Code:

(define-module (geographic scripts xyz filter)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (xyz xyz)
  #:use-module (xyz infos)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (geographic rasters)
  #:export (filter))

(define filter-version "0.0.1")

(define %summary "Filter information from xyz data.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))))

(define (display-help)
  (format #t "\
~a
 f i l t e r

usage: filter [ hv [ args ] ] [ files ]
" %summary))

(define (display-version)
  (format #t "\
filter (XYZ) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" filter-version))

(define (filter . args)
  (let* ((options (getopt-long (cons "filter" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f)))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let ((input (option-ref options '() #f)))
	  (let ((infile (if (not (pair? input)) 
			    (current-input-port) 
			    (open-file (car input) "r"))))
	    (let* ((these-xyz (xyz->scm infile))
		   (these-infos (xyz->infos these-xyz)))
	      (display these-infos)
	      (newline)
	      (display (infos->region these-infos))
	      (newline)
	      (let* ((nan-array (make-raster (infos->region these-infos) 10 10))
		     (xyz-array (xyz->raster these-xyz nan-array)))
	      	(display nan-array)
	      	(newline)
	      	(display xyz-array)
	      	;;(display this-array)
	      	(newline)
		(display (raster-array xyz-array))
		(newline)
		)))))))))
	    ;;(xyz-port->raster infile 

(define main filter)

;; point-list is a list of 2 or more xyz pairs '(x y z).
(define (xyzs:z-avg point-set)
  (let ((count (length point-set))
	(sums 0))
    (map (lambda (x)
	   (set! sums (+ sums x)))
	 point-set)
    (/ sums count)))

;; Makes a raster from an xyz data port.
;; make a raster with make-raster-with-nan-array
;; Move this function so this module isn't dependent on (xyz xyz)
;; (define* (xyz-port->raster #:optional 
;; 			   (port (current-input-port))
;; 			   (raster #f) 
;; 			   #:key (test #f))
;;   (define xyz-raster
;;     (lambda (xyz)
;;       (display "hello")
;;       (newline)
;;       (let ((this-location (raster-point->pixel xyz raster))
;; 	    (current-array (raster-array raster)))

;; 	(when (array-in-bounds? current-array (car this-location) (cadr this-location))
;; 	      (let ((this-value (array-ref current-array (car this-location) (cadr this-location))))
;; 		(array-set! current-array 
;; 			    (if (nan? this-value) (caddr xyz)
;; 				(xyzs:z-avg (list xyz this-value)))
;; 			    (car this-location) (cadr this-location))))
;; 	(set-raster-array! raster current-array))))
	
;;   (xyz->scm port 
;; 	    #:index "xyz" 
;; 	    #:list/vector #f 
;; 	    #:test-fun test 
;; 	    #:data-fun xyz-raster
;; 	    #:loop-fun (lambda (a b) (display "hi"))))

(define (xyz->raster xyzs raster)
  (if (null? xyzs) raster
      (let ((current-array (raster-array raster))
	    (xyz (car xyzs)))
	(let ((this-location (raster-point->pixel xyz raster)))
	  (when (array-in-bounds? current-array (car this-location) (cadr this-location))
		(let ((this-value (array-ref current-array (car this-location) (cadr this-location))))
		  (array-set! current-array 
			      (if (nan? this-value) (caddr xyz)
				  (xyzs:z-avg (list (caddr xyz) this-value)))
			      (car this-location) (cadr this-location)))))
	(raster-set-array! raster current-array)
	(xyz->raster (cdr xyzs) raster))))

;; Insert a xyz point '(x y z) into a raster, averaging the raster cell value.
(define (raster-insert xyz raster)
  (let* ((current-array (raster-array raster))
	 (this-location (raster-point->pixel xyz raster))
	 (this-value (array-ref current-array (car this-location) (cadr this-location))))
    (array-set! 
     current-array 
     (if (nan? this-value) (caddr xyz)
	 (xyzs:z-avg (list xyz this-value)))
     (car this-location) (cadr this-location))
    (set-raster-array! raster current-array)))

;;; End

;;-*-scheme-*-
;;; vectors.scm
;;
;; Copyright (c) 2018 Matthew Love <matthew.love@colorado.edu>
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
;;
;; Spatial Functions, etc.
;;
;;; Code:

(define-module (geographic spatial)
  #:use-module (geodesic geod)
  #:use-module (geodesic formulae)
  #:use-module (xyz xyz)
  #:export
  (xyz-inside-polygon?
   xyz-inside-multi-polygon?
   polygon?
   polygon-perimiter
   polygon->region
   gds-grade
   gds-interp))

(define (polygon? poly)
  "Return #t if the list appears the be a polygon"
  (if (null? poly) #f
      (let ((poly-tail (car (list-tail poly (- (length poly) 1))))
	    (poly-head (car poly)))
	(if (or (not (number? (car poly-head)))
		(not (number? (cadr poly-head)))) #f
		(and (= (car poly-head) (car poly-tail))
		     (= (cadr poly-head) (cadr poly-tail)))))))

;; '(x y) in '((x1 y1 ...) (x2 y2 ...) ... (x1 y1 ...))
;; xys that fall on the border are considered outside the polygon.
(define* (xyz-inside-polygon? xyz polygon #:optional (inside? #f))
  "Return #t if xyz is inside polygon."
  (if (null? (cdr polygon)) inside?
      (let* ((p1 (car polygon)) (p2 (cadr polygon))
	     (x (car xyz)) (y (cadr xyz))
	     (x1 (car p1)) (y1 (cadr p1))
	     (x2 (car p2)) (y2 (cadr p2)))
	(if (not (and (>= y (min y1 y2)) (<= y (max y1 y2))
		      (<= x (max x1 x2)) (not (= y y1))))
	    (xyz-inside-polygon? xyz (cdr polygon) inside?)
	    (if (or (= x1 x2)
		    (<= x (+ x1 
			     (/ (* (- y y1)
				   (- x2 x1))
				(- y2 y1)))))
		(xyz-inside-polygon? xyz (cdr polygon) (not inside?))
		(xyz-inside-polygon? xyz (cdr polygon) inside?))))))

;; do some initial checks, such as if a point is inside the region of the multi-polygon to begin with
;; and maybe do a polygo->region and check that before checking against polygon.
(define (xyz-inside-multi-polygon? xyz polygon)
  (if (null? polygon) #f
      (let ((this-polygon (car polygon)))
	(if (xyz-inside-polygon? xyz this-polygon) #t
	    (xyz-inside-multi-polygon? xyz (cdr polygon))))))

(define (xyz-inside-multi-polygon2? xyz polygon)
  (if (null? polygon) #f
      (let* ((this-polygon (car polygon))
	     (this-region (polygon->region this-polygon)))
	(if (xyz-in-region? xyz this-region)
	    (if (xyz-inside-polygon? xyz this-polygon) #t
		(xyz-inside-multi-polygon? xyz (cdr polygon)))
	    (xyz-inside-multi-polygon? xyz (cdr polygon))))))

;; polygons-overlap?

;; Calculate the perimiter of a polygon.
(define* (polygon-perimiter xyzs #:optional (sum 0))
  (if (null? (cdr xyzs)) sum
      (let ((p0 (car xyzs))
	    (p1 (cadr xyzs)))
	(let ((this-distance (gds-distance (car p0) (cadr p0) (car p1) (cadr p1))))
	  (hull-perimiter (cdr xyzs) (+ this-distance sum))))))

;; return the region of the polygon.
(define* (polygon->region poly #:optional (region '()))
  (if (null? poly) region
      (if (null? region)
	  (polygon->region (cdr poly) 
			   (list (caar poly)
				 (caar poly) 
				 (cadar poly) 
				 (cadar poly)))
	  (let ((xmin (car region)) (xmax (cadr region))
		(ymin (caddr region)) (ymax (cadddr region))
		(x (caar poly))	(y (cadar poly)))
	    (polygon->region (cdr poly) 
			     (list (if (< x xmin) x xmin)
				   (if (> x xmax) x xmax)
				   (if (< y ymin) y ymin)
				   (if (> y ymax) y ymax)))))))

(define (gds-grade x1 y1 z1 x2 y2 z2)
  ; will output a pair, '(percent-slope . degree-slope)
  (let* ((h-dist (gds-distance x1 y1 x2 y2))
	(v-diff (- z2 z1))
	(g-ratio (/ v-diff h-dist)))
    (cons (* 100 g-ratio)
	  (atan g-ratio))))

(define* (gds-interp x1 y1 z1 x2 y2 z2 dist #:optional port)
  (let* ((h-inv (gds-inverse x1 y1 x2 y2))
	 (h-dist (car h-inv))
	 (z-slope (car (gds-grade x1 y1 z1 
				  x2 y2 z2))))
    (let loop ((d dist))
      (let ((d-dir (gds-direct x1 y1 (cadr h-inv) d)))
	(let ((z3 (+ z1 (/ (* z-slope d) 100))))
	  (xyz-display (list (car d-dir) (cadr d-dir) z3) port))
	(if (< (+ d dist) h-dist)
	    (loop (+ d dist)))))))
;;; End


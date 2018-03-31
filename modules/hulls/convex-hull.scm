;;*-scheme-*
;;; convex-hull.scm
;;
;; Copyright (c) 2011, 2012, 2013, 2016, 2018 Matthew Love <matthew.love@colorado.edu>
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
;; Commentary:
;;
;; Algorithms for discovering the convex hull of a set of points.
;; The function `convex-hull uses the default alogrithm `amc-convex-hull.
;; This implements Andrew's Monotone Chain algorithm to discover the hull boundary of a set of points.
;;
;; `pw-convex-hull implements a 'package-wrap' algorithm to discover the hull. 
;;
;; Use `convex-hull point-list to return a point-list of the hull.
;; an input point-list is a list of point-vectors `(#(x y ...) #(x y ...) ... #(x y ...))
;;
;;; Code:

(define-module (hulls convex-hull)
  #:version (0 0 4)
  #:export
  (convex-hull amc-convex-hull pw-convex-hull))

(define ch-pi 355/113)

(define (ch-theta point-a point-b)
  "- Scheme Procedure: ch-theta point-a point-b
    Return the angle between point-a and point-b"
  (let ((t (atan (- (vector-ref point-b 1) (vector-ref point-a 1))
		 (- (vector-ref point-b 0) (vector-ref point-a 0)))))
    (if (>= t 0) t
    	(+ t (* 2 ch-pi)))))

(define (ch-cross point-a point-b point-c)
  "- Scheme Procedure: ch-cross point-a point-b point-c
    Three points are counter-clockwise turn if ch-cross > 0,
    clockwise if ch-cross < 0 and collinear if ch-cross = 0."
  (- (* (- (vector-ref point-b 0) (vector-ref point-a 0))
	(- (vector-ref point-c 1) (vector-ref point-a 1)))
     (* (- (vector-ref point-b 1) (vector-ref point-a 1))
	(- (vector-ref point-c 0) (vector-ref point-a 0)))))

(define (amc-convex-hull points)
  "- Scheme Procedure: amc-convex-hull points
    Implements Andrew's monotone chain algorithm. O(n log n) complexity.
    `points is a list of `#(x y ...) vectors `(#(x . y) #(x . y) ... #(x .y)).
    Returns the x/y points that make up the hull."
  (define (<=vr0 p0 p1)
    (<= (vector-ref p0 0) (vector-ref p1 0)))
  (define (amc-find-hull xys hull)
    (cond
     ((null? (cdr xys)) hull)
     ((< (length hull) 2)
      (amc-find-hull (cdr xys) (append (list (car xys)) hull)))
     ((<= (ch-cross (cadr hull) (car hull) (car xys)) 0)
      (amc-find-hull (cdr xys) (cdr hull)))
     (else
      (amc-find-hull (cdr xys) (append (list (car xys)) hull)))))
  (if (not (pair? points)) points
      (let* ((xys (sort points <=vr0))
	     (lower (amc-find-hull xys '())) 
	     (upper (amc-find-hull (reverse xys) '())))
	(append (reverse (cdr lower)) (reverse (cdr upper))))))

(define (pw-convex-hull points)
  "- Scheme Procedure: pw-convex-hull points
    Implements a package-wrap algorithm to discover the convex hull of a set of points.
    `points is a list of `#(x y ...) vectors `(#(x . y) #(x . y) ... #(x .y)).
    Returns a point-list of the hull nodes."
  (define (<=vr1 p0 p1) 
    (<= (vector-ref p0 1) (vector-ref p1 1)))
  (define* (pw-next-node point xys 
			 #:optional 
			 (last-theta 0) 
			 (this-theta (* 2 ch-pi)) 
			 (theta-point '()))
    (if (null? xys) (cons theta-point this-theta)
	(let* ((this-point (car xys))
	       (angle (ch-theta point this-point)))
	  (if (and (> angle last-theta) (< angle this-theta))
	      (pw-next-node point (cdr xys) last-theta angle this-point)
	      (pw-next-node point (cdr xys) last-theta this-theta theta-point)))))
  (if (not (pair? points)) points
      (let find-hull ((xys (sort points <=vr1))
		      (hull '()) 
		      (last-theta 0))
	(cond 
	 ((null? xys) hull)
	 ((null? hull)
	  (find-hull (cdr xys) (append (list (car xys)) hull) 0))
	 (else
	  (let ((next-point (pw-next-node (car hull) (cdr xys) last-theta)))
	    (if (null? (car next-point)) hull
		(find-hull (cdr xys) 
			   (append (list (car next-point)) hull) 
			   (cdr next-point)))))))))

(define convex-hull amc-convex-hull)

;;; End

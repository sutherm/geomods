;;-*-scheme-*-
;;; region.scm
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
;;; Commentary:
;;
;; Functions for regions '(xmin xmax ymin ymax)
;;
;; Regions are a simple data type. Just 4 values in a list so this type
;; is already defined in scheme as a 4-part list '(xa xb ya yb).
;; Simple checks can be done on such a list to validate it if necessary.
;;
;; So a 'region' is just a scheme list with 4 numerical values, where the first
;; value is less than the second value and the third value is less than the 
;; fourth value.
;;
;; To make a new region-list just use 'list:
;; (list -90.2 -90 30 32)
;;
;; The functions in this module handle and manipulate such region-lists.
;;
;;; Code:

(define-module (geographic regions)
  #:use-module (srfi srfi-1)
  #:export 
  (region?
   region->gmt-region
   gmt-region->region
   region->polygon
   regions->polygon
   merge-regions
   region-expand
   region-extend
   region-quad
   xyz-inside-region?
   region-inside-region?))

;; Make a guess if a region is actually a region-list
(define (region? region)
  "- Scheme Procedure: region? region
    Return #t if the input list is a region-list '(xmin xmax ymin ymax)"
  (cond
   ((not (pair? region)) #f)
   ((not (eq? 4 (length region))) #f)
   ((not (null? (remove! number? region))) #f)
   ((not (< (car region) (cadr region))) #f)
   ((not (< (caddr region) (cadddr region))) #f)
   (else #t)))
		
;; Convert a region-list to a gmt formatted region string
;; e.g. -R1/2/3/4
(define (region->gmt-region region)
  "- Scheme Procedure: region->gmt-region region
    Returns a string where the region is delimited with a /
    This string is useful in GMT and reglated programs."
  (string-join (map number->string region) "/"))

;; Convert a region-string to a region-list
;; e.g. '(1 2 3 4)
(define (gmt-region->region region)
  "- Scheme Procedure: gmt-region->region gmt-region
    Return a region-list from a GMT style region string.
    i.e. '1/2/3/4' -> '(1 2 3 4)."
  (let ((reg-lst (string-split region #\/)))
    (if (not (pair? (cdr reg-lst))) '()
	(map (lambda (x)
	       (let ((nx (string->number x)))
		 (if (exact? nx)
		     (exact->inexact nx)
		     nx)))
	     reg-lst))))

;; should the polygon have the first point at the end as well?
;; region-list is '(xmin xmax ymin ymax)
(define (region->polygon region)
  "- Scheme Procedure: region->polygon region
    Return a polygon '((x y) (x y) ... (x y)) from
    a region-list '(xmin xmax ymin ymax)."
  (list (list (car region) (caddr region))
	(list (cadr region) (caddr region))
	(list (cadr region) (cadddr region))
	(list (car region) (cadddr region))
	(list (car region) (caddr region))))

;; convert a list of regions to a polygon.
(define (regions->polygon regions plys)
  "convert a region to a polygon."
  (if (not (null? regions))
      (regions->polygon (cdr regions) (append (list (region->polygon (car regions))) plys))
      plys))

(define (merge-regions region-a region-b)
  "Merge two regions together and return the merged region."
  (let ((x1a (car region-a)) (y1a (caddr region-a))
	(x2a (cadr region-a)) (y2a (cadddr region-a))
	(x1b (car region-b)) (y1b (caddr region-b))
	(x2b (cadr region-b)) (y2b (cadddr region-b)))
    (list (if (> x1a x2b) x1a x1b) 
	  (if (< x2a x2b) x2a x2b)
	  (if (< y1a y2b) y1a y1b) 
	  (if (> y2a y2b) y2a y2b))))

;; inc should be in degrees; pre-process with gmt-inc->inc if in arc-seconds etc.
(define* (region-expand region inc #:key (cells 6))
  "Expand region based on world-raster and increment."
  (let ((cv (* inc cells)))
    (list (- (car region) cv) (+ (cadr region) cv) (- (caddr region) cv) (+ (cadddr region) cv))))

;; Extend a region to fit the xyz point.
(define (region-extend region xyz)
  "Extend region to fit xyz."
  (let ((x1 (car region)) (y1 (caddr region))
	(x2 (cadr region)) (y2 (cadddr region))
	(x3 (car xyz)) (y3 (cadr xyz)))
    (list
     (if (< x3 x1) x3 x1)
     (if (> x3 x2) x3 x2)
     (if (< y3 y1) y3 y1)
     (if (> y3 y2) y3 y2))))

;; Returns a list of 4 regions, which are each a quarter of the input region.
(define (region-quad region)
  "Quarter the given `region'"
  (let ((x-mid (/ (+ (car region) (cadr region)) 2.0))
	(y-mid (/ (+ (caddr region) (cadddr region)) 2.0)))
    (list
     (list (car region) x-mid y-mid (cadddr region))
     (list x-mid (cadr region) y-mid (cadddr region))
     (list (car region) x-mid (caddr region) y-mid)
     (list x-mid (cadr region) (caddr region) y-mid))))

;; Return #t is point 'xy is inside of 'region
(define (xyz-inside-region? xy region)
  "Return `#t' if `xy' is in `region'"
  (if (or (not xy) (not (pair? xy))) #f
      (let ((x1 (car xy)) (y1 (cadr xy)))
	(and (>= x1 (car region)) (<= x1 (cadr region))
	     (>= y1 (caddr region)) (<= y1 (cadddr region))))))

;; Return #t if a region overlaps another region.
;; one of the region corners must be inside the other region...
;;
;;      __
;;   __|__|__
;;  |__|__|__| Fails. (fixed with hack below)
;;     |__|
;;
(define (region-inside-region? region-a region-b)
  "Regions overlap?."
  (let ((x1a (car region-a)) (y1a (caddr region-a))
	(x2a (cadr region-a)) (y2a (cadddr region-a))
	(x1b (car region-b)) (y1b (caddr region-b))
	(x2b (cadr region-b)) (y2b (cadddr region-b)))
    (or (xyz-inside-region? (list x1a y1a) region-b) (xyz-inside-region? (list x1a y2a) region-b)
	(xyz-inside-region? (list x2a y2a) region-b) (xyz-inside-region? (list x2a y1a) region-b)
	(xyz-inside-region? (list x1b y1b) region-a) (xyz-inside-region? (list x1b y2b) region-a)
	(xyz-inside-region? (list x2b y2b) region-a) (xyz-inside-region? (list x2b y1b) region-a)
	(and (< x1a x1b) (> x2a x2b) (or (> y1a y1b) (< y2a y2b)))
	(and (< y1a y1b) (> y2a y2b) (or (> x1a x1b) (< x2a x2b))))))

;;; End

;;*-scheme-*
;;; rasters.scm
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
;;; Commentary:
;;
;; A raster/geographic grid record definition and related functions.
;;
;;; Code:

(define-module (geographic rasters)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:export 
  (<raster>
   raster?
   make-raster
   raster-point->pixel
   raster-pixel->point
   raster-dimensions
   region-dimensions
   raster-region
   raster-xinc
   raster-yinc
   raster-array
   raster-set-array!
   raster-index->region
   raster-gather-info))

(define (float->int float)
  (let ((flt-str (format #f "~,0f" float)))
    (string->number (substring flt-str 0 (- (string-length flt-str) 1)))))

;; Raster Record
;; The Raster Record Type holds the region/increments and the array.
;; these values should not be edited manually without updating related
;; entries.
(define-record-type <raster>
  (make-raster-type region xinc yinc array)
  raster?
  (region raster-region)
  (xinc raster-xinc)
  (yinc raster-yinc)
  (array raster-array raster-set-array!))

(set-record-type-printer! 
 <raster>
 (lambda (record port)
   (write-char #\< port)
   (format port "<raster> ~{~a~^/~} ~a/~a ~{~a~^/~}" 
	   (raster-region record) 
	   (raster-xinc record) 
	   (raster-yinc record) 
	   (raster-dimensions record))
   (write-char #\> port)))

;; Make a new raster record based on a region and increments.
(define* (make-raster region xinc yinc 
		      #:optional (raster-array #f) 
		      #:key (dummy #f))
  "- Scheme Procedure: make-raster region xinc yinc [ raster-array  #:dummy ]
    Make a new raster-type. 'region '(xmin xmax ymin ymax) is the geographic region 
    of the new array with increments of 'xinc and 'yinc which should be in the same 
    units as the region. If 'raster-array is not specified, this will initialize an 
    array of nan values. The raster-array must be the same dimensions as set by the 
    region and increments. Set #:dummy #t to create a raster-record with no array."
  (let ((rsize (region-dimensions region xinc yinc)))
    (cond
     (dummy
      (set! raster-array #f))
     ((not (raster-array))
      (set! raster-array (make-array (nan) (car rsize) (cadr rsize))))
     ((not (array? raster-array)) 
      (error "invalid array: " raster-array))
     (else
      (let ((array-dims (array-dimensions raster-array)))
	(if (not (and (= (car array-dims) (car rsize))
		      (= (cadr array-dims) (cadr rsize))))
	    (error "invalid array: " raster-array)))))
    (make-raster-record record xinc yinc raster-array)))

;; Return the region of the given index extents
(define (raster-index->region raster index)
  "- Scheme Procedure: raster-index->region <raster> index
    Return the region '(xmin xmax ymin ymax) of the cell in 
    the `raster` given by `index` `(x y)`"
  (let ((region (raster-region raster))
    	(xinc (raster-xinc raster))
	(yinc (raster-yinc raster)))
    (list (+ (car region) (* xinc (car index)))
	  (+ (car region) (* xinc (+ 1 (car index))))
	  (+ (caddr region) (* yinc (cadr index)))
	  (+ (caddr region) (* yinc (+ 1 (cadr index))))
	  (+ 1 (car index)) (+ 1 (cadr index)))))

;; Return the size of a region as xdim/ydim.
(define (region-dimensions region xinc yinc)
  "- Scheme Procedure: region-dimensions region xinc yinc
    Return the dimensions of a region given `region' `xinc` and `yinc`"
  (list (float->int (ceiling (/ (- (cadr region) (car region)) xinc)))
	(float->int (ceiling (/ (- (cadddr region) (caddr region)) yinc)))))

;; Return the size of a raster as xdim/ydim.
(define (raster-dimensions raster)
  "- Scheme Procedure: raster-dimensions <raster>
    Return the dimensions `(xdim ydim)` of `raster`"
  (let ((region (raster-region raster))
	(xinc (raster-xinc raster))
	(yinc (raster-yinc raster)))
    (region-dimensions region xinc yinc)))

;; Return a raster-pixel location based on a point value.
(define (raster-point->pixel point raster)
  "- Scheme Procedure raster-point->pixel point <raster>
    Return the pixel location as '(row col) of 'point '(x y) of <raster>"
  (let ((x-location 
	 (floor (abs (/ (- (car (raster-region raster)) 
			     (car point)) 
			  (raster-xinc raster)))))
	(y-location 
	 (floor (abs (/ (- (cadr point) 
			     (caddr (raster-region raster))) 
			  (raster-yinc raster))))))
    (list (float->int x-location) (float->int y-location))))

;; Return a point value based on a raster-pixel location.
(define (raster-pixel->point pixel raster)
  "- Scheme Procedure: raster-pixel->point pixel <raster>
    Return the point location as '(x y) of 'pixel '(row col) of <raster>"
  (let* ((index-region (raster-index->region raster pixel))
	 (x-mid (/ (+ (car index-region) (cadr index-region)) 2.0))
	 (y-mid (/ (+ (caddr index-region) (cadddr index-region)) 2.0)))
    (list x-mid y-mid)))

(define (raster-gather-info raster yc proc info-list)
  "Gather info about <raster> going column by column."
  (define (gather-xraster-info raster xc yc proc info-list)
    (if (<= xc 0) info-list
	(gather-xraster-info 
	 raster (- xc 1) yc proc 
	 (append 
	  info-list 
	  (apply proc (list raster xc yc))))))
  (if (<= yc 0) info-list
      (let ((raster-count (raster-dimensions raster)))
	(raster-gather-info 
	 raster (- yc 1) proc 
	 (append info-list 
		 (gather-xraster-info raster (car raster-count) yc proc '()))))))
   
;;; End

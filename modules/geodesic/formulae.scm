;;; formulae.scm
;;
;; This file is part of GUILE-GEODESIC
;;
;; Copyright (C) 2011, 2012, 2013, 2018 by Matthew Love <matthew.love@colorado.edu>
;; GUILE-GEODESIC is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GUILE-GEODESIC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GUILE-GEODESIC.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module holds the geodesic functions for discovering the distance/azimuth/etc in geographic space.
;;
;;
;;
;;; Code:

(define-module (geodesic formulae)
  #:version (0 2 2)
  #:export
  (gds-ellps
   gds-units
   gds-pi
   degree->radian
   radian->degree
   degree->sine
   gds-flattening
   gds-minor
   gds-azimuth
   gds-bearing
   gds-midpoint
   gds-degree-length
   gds-positions
   gds-distance-euclidean
   gds-distance-haversine0
   gds-distance-haversine1
   gds-inverse-haversine
   gds-distance-slc
   gds-inverse-slc
   gds-inverse-vincenty
   gds-direct-vincenty))

;;-------------;
;; Ellipsoids -;
;; <http://www.colorado.edu/geography/gcraft/notes/datum/elist.html>
;; `gds-ellipsoids` is an alist of ellipsoids and their values.
;; Access with `assoc`, e.g. (assoc "WGS84" gds-ellps)
;;-------------;
(define gds-ellps
  '(("airy" 6377563.396 299.3249646 573.604 0.11960023)
    ("mod_airy" 6377340.189 299.3249646 796.811 0.11960023)
    ("aust_SA" 6378160.0 298.25 -23.0 -0.00081204)
    ("bessel" 6377397.155 299.1528128 739.845 0.10037483)
    ("bess_nam" 6377483.865 299.1528128 653.135 0.10037483)
    ("clrk66" 6378206.4 294.9786982 -69.4 -0.37264639)
    ("clrk80" 6378249.145 293.465 -112.145 -0.54750714)
    ("evrst30" 6377276.345 300.8017 860.655 0.28361368)
    ("evrst48" 6377304.063 300.8017 832.937 0.28361368)
    ("fschr60" 6378166.0 298.3 -29.0 0.00480795)
    ("fschr68" 6378150.0 298.3 -13..0 0.00480795)
    ("fschr60m" 6378155.0 298.3 -18.0 0.00480795)
    ("helmert" 6378200.0 298.3 -63.0 0.00480795)
    ("hough" 6378270.0 297.0 -133.0 -0.14192702)
    ("intl" 6378388.0 297.0 -251.0.0 -0.14192702)
    ("krass" 6378245.0 298.3 -108.0 0.00480795)
    ("GRS67" 6378160.0 298.247167427 -23.0 -0.00113048)
    ("GRS80" 6378137 298.257222101 0.0 -0.00000016)
    ("WGS60" 6378165.0 298.3 -28.0 0.00480795)
    ("WGS66" 6378145.0 298.25 -8.0 -0.00081204)
    ("WGS72" 6378135.0 298.26 2.0 0.0003121057)
    ("WGS84" 6378137.0 298.257223563 0.0 0.0)))

;;--------;
;; Units -;
;; The units value is its conversion from meters.
;; Access with `assoc`, e.g. (assoc "mi" gds-units)
;;--------;
(define gds-units
  '(("km" 1000. "Kilometer")
    ("m" 1. "Meter")
    ("dm" 1/10 "Decimeter")
    ("cm" 1/100 "Centimeter")
    ("mm" 1/1000 "Millimeter")
    ("kmi" 1852.0 "International Nautical Mile")
    ("in" 0.0254 "International Inch")
    ("ft" 0.3048 "International Foot")
    ("yd" 0.9144 "International Yard")
    ("mi" 1609.344 "International Statute Mile")
    ("fath" 1.8288 "International Fathom")
    ("ch" 20.1168 "International Chain")
    ("link" 0.201168 "International Link")
    ;;("us-in" 1/39.37 "U.S. Surveyor's Inch") ;; this crashes guile
    ("us-in" 0.025400050800102 "U.S. Surveyor's Inch")
    ("us-ft" 0.304800609601219 "U.S. Surveyor's Foot")
    ("us-yd" 0.914401828803658 "U.S. Surveyor's Yard")
    ("us-ch" 20.11684023368047 "U.S. Surveyor's Chain")
    ("us-mi" 1609.347218694437 "U.S. Surveyor's Statute Mile")
    ("ind-yd" 0.91439523 "Indian Yard")
    ("ind-ft" 0.30479841 "Indian Foot")
    ("ind-ch" 20.11669506 "Indian Chain")))

;;(define gds-pi 3.14159265358979323846)
(define gds-pi 355/113)

;; (define (degree->radian deg)
;;   (* 0.017453292519943295 deg))

;; (define (radian->degree rad)
;;   (* 57.29577951308232 rad))

(define (degree->radian deg)
  "- Scheme Procedure: degree->radian degree
    Return the value of DEGREE in radians"
  (* gds-pi (/ deg 180.0)))

(define (radian->degree rad)
  "- Scheme Procedure: radian->degree radian
    Return the value of RADIAN in degrees"
  (* 180.0 (/ rad gds-pi)))

(define (degree-sine deg)
  (sin (degree->radian deg)))

(define (gds-flattening major-axis minor-axis)
  "- Scheme Procedure: gds-flattening major-axis minor-axis
    Using the MAJOR-AXIS and MINOR-AXIS, discover the flattening"
  (/ (- major-axis minor-axis) major-axis))

(define (gds-minor major-axis flattening)
  "- Scheme Procedure: gds-minor major-axis flattening
    Using the MAJOR-AXIS and the FLATTENING, discover the minor-axis"
  (* major-axis (- 1 (/ 1 flattening))))

;; Azimuth
(define* (gds-azimuth x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-azimuth lon-1 lat-1 lon-2 lat-2 [#:ellps key]
    Calculate the forward azimuth on the specified ellipsoid given 
    two points LON-1 LAT-1 and LON-2 LAT-2.
    See `gds-ellps' for ellipsoid key values."
  (let* ((fl (/ 1 (caddr (assoc ellps gds-ellps))))
	 (xd (degree->radian (- x2 x1)))	 
	 (y1 (degree->radian y1))
	 (y2 (degree->radian y2))
	 (e2 (* fl (- 2 fl)))
	 (e12 (expt (- 1 fl) 2))
	 (lam (+ (* e12 (/ (tan y2)
			   (tan y1)))
		 (* e2 (sqrt
			(/ (+ 1 (* e12 (expt (tan y2) 2)))
			   (+ 1 (* e12 (expt (tan y1) 2)))))))))
    (if (eq? y1 0)
	(radian->degree
	 (atan (/ (sin xd)
		  (* e12 (tan y2)))))
	(radian->degree
	 (atan (/ (sin xd)
		  (* (- lam (cos xd))
		     (sin y1))))))))
;; Bearing
(define (gds-bearing x1 y1 x2 y2)
  "- Scheme Procedure: gds-bearing lon-1 lat-1 lon-2 lat-2
    Return the bearing (forward azimuth) between two points."
  (let ((diff-lon (degree->radian (- x2 x1))))
    (radian->degree (atan 
		     (* (sin diff-lon)
			(cos (degree->radian y2)))
		     (- (* (cos (degree->radian y1))
			   (sin (degree->radian y2)))
			(* (sin (degree->radian y1))
			   (cos (degree->radian y2))
			   (cos diff-lon)))))))

;; Midpoint
(define (gds-midpoint x1 y1 x2 y2)
  "- Scheme Procedure: gds-midpoint lon-1 lat-1 lon-2 lat-2
    Return the midpoint between two points"
  (let* ((diff-lon (degree->radian (- x2 x1)))
	 (bx (* (cos (degree->radian y2))
		(cos diff-lon)))
	 (by (* (cos (degree->radian y2))
		(sin diff-lon)))
	 (mid-lat (atan (+ (sin (degree->radian y1))
			   (sin (degree->radian y2)))
			(sqrt (+ (expt (+ (cos (degree->radian y1))
					  bx) 2)
				 (expt by 2)))))
	 (mid-lon (+ (degree->radian x1)
		     (atan by
			   (+ (cos (degree->radian x1))
			      bx)))))
    (cons (radian->degree mid-lon) (radian->degree mid-lat))))

;; Distance of degree
(define* (gds-degree-length x y #:key (ellps "WGS84") (units "m"))
  "- Scheme Procedure: gds-degree-length lon lat [#:ellps key #:units key]
    Return the distance in the specified unots of 1 degree of 
    latitude and longitude on the specified ellipsoid at the 
    coordinates LON LAT.
    Returns a cons `(degree-lon-distance degree-lat-distance)'
    See `gds-ellps' and `gds-units' for key values."
  (let* ((x1 (degree->radian x))
	 (y1 (degree->radian y))
	 (lon-dist (gds-distance x1 y1 (+ (degree->radian 1) x1) y1 #:ellps ellps #:units units))
	 (lat-dist (gds-distance x1 y1 x1 (+ (degree->radian 1) y1) #:ellps ellps #:units units)))
    (cons lon-dist lat-dist)))

(define* (gds-positions x1 y1 x2 y2 dist)
  "Return points along the line `x1 `y1 `x2 `y2 at every `dist distance."
  (let* ((h-inv (gds-inverse x1 y1 x2 y2))
	 (h-dist (car h-inv))
	 (pos '()))
    (if (> dist h-dist) pos
	(let loop ((d dist))
	  (let ((d-dir (gds-direct x1 y1 (cadr h-inv) d)))
	    (set! pos (append (list (list (car d-dir) (cadr d-dir))) pos))
	    (if (< (+ d dist) h-dist)
		(loop (+ d dist))
		pos))))))

;;-------------------------------------------;
;; Euclidean Distance / Pythagorean Theorem -;
;; http://en.wikipedia.org/wiki/Geographical_distance
;; Note: if geographic units (WGS), the x and y values
;; should be first converted to radians.
;;-------------------------------------------;
(define* (gds-distance-euclidean x1 y1 x2 y2)
  "- Scheme Procedure: gds-distance-euclidean x1 y1 x2 y2
    Calculate the euclidean distance between two points and 
    return the result in the same units as the input x/y data."
  (let ((dy (- y2 y1))
	(dx (- x2 x1)))
    (sqrt (+ (expt dy 2)
	     (expt dx 2)))))

;;--------------------;
;; Haversine Formula -;
;; Calculate distance using constant Radius.
;; http://en.wikipedia.org/wiki/Haversine_formula
;;--------------------;
(define (gds-a x1 y1 x2 y2)
  (let ((dy (- y2 y1))
	(dx (- x2 x1)))
    (+ (expt (sin (/ dy 2)) 2)
       (* (cos y1)
	  (cos y2)
	  (expt (sin (/ dx 2)) 2)))))

;; Haversine distance formula 0
(define* (gds-distance-haversine0 x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-distance-slc lon-1 lat-1 lon-2 lat-2 [#:ellps key]
    Calculate the distance between two points LON-1 LAT-1 and LON-2 LAT-2
    on the specified ellipsoid using the Haversine Formula.
    Returns the distance in meters.
    See `gds-ellps' for optional ellipsoid key values."
  (define (gds-c0 a) (* 2 (atan (sqrt a) (sqrt (- 1 a)))))
  (let ((ma (gds-minor (cadr (assoc ellps gds-ellps)) (caddr (assoc ellps gds-ellps))))
	(x1 (degree->radian x1))
	(x2 (degree->radian x2))
	(y1 (degree->radian y1))
	(y2 (degree->radian y2)))
    (* ma (gds-c0 (gds-a x1 y1 x2 y2)))))

;; Haversine distance formula 1
(define* (gds-distance-haversine1 x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-distance-slc lon-1 lat-1 lon-2 lat-2 [#:ellps key]
    Calculate the distance between two points LON-1 LAT-1 and LON-2 LAT-2
    on the specified ellipsoid using the Haversine Formula.
    Returns the distance in meters.
    See `gds-ellps' for optional ellipsoid key values."
  (define (gds-c1 a) (* 2 (asin (sqrt a))))
  (let ((ma (gds-minor (cadr (assoc ellps gds-ellps)) (caddr (assoc ellps gds-ellps))))
	(x1 (degree->radian x1))
	(x2 (degree->radian x2))
	(y1 (degree->radian y1))
	(y2 (degree->radian y2)))
  (* ma (gds-c1 (gds-a x1 y1 x2 y2)))))

;; Calculate haversine distance and azimuth seperately and combine.
(define* (gds-inverse-haversine  x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-inverse-slc lon-1 lat-1 lon-2 lat-2 [:ellps key]
    Get the inverse geodetic using the Haversine Formula. 
    Given points LON-1 LAT-1 and and LON-2 LAT-2 return the distance 
    between the points in meters as well as the forward and return azimuth.
    See `gds-ellps' for optional ellipsoid key values."
  (let ((fwda (gds-azimuth x1 y1 x2 y2 #:ellps ellps))
	(rtna (gds-azimuth x2 y2 x1 y1 #:ellps ellps))
	(shav (gds-distance-haversine1 x2 y2 x1 y1 #:ellps ellps)))
    (list shav fwda rtna)))

;;---------------------------;
;; Spherical Law of Cosines -;
;;---------------------------;
(define* (gds-distance-slc x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-distance-slc lon-1 lat-1 lon-2 lat-2 [#:ellps key]
    Calculate the distance between two points LON-1 LAT-1 and LON-2 LAT-2
    on the specified ellipsoid using the Spherical Law of Cosines.
    Returns the distance in meters.
    See `gds-ellps' for optional ellipsoid key values."
  (let ((ma (gds-minor (cadr (assoc ellps gds-ellps))
		       (caddr (assoc ellps gds-ellps))))
	(x1 (degree->radian x1)) (x2 (degree->radian x2))
	(y1 (degree->radian y1)) (y2 (degree->radian y2)))
    (* ma (acos (+ (* (sin y1) (sin y2))
		   (* (cos y1) (cos y2) (cos (- x2 x1))))))))

(define* (gds-inverse-slc  x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-inverse-slc lon-1 lat-1 lon-2 lat-2 [:ellps key]
    Get the inverse geodetic using the Spherical Law of Cosines. 
    Given points LON-1 LAT-1 and and LON-2 LAT-2 return the distance 
    between the points in meters as well as the forward and return azimuth.
    See `gds-ellps' for optional ellipsoid key values."
  (let ((fwda (gds-azimuth x1 y1 x2 y2 #:ellps ellps))
	(rtna (gds-azimuth x2 y2 x1 y1 #:ellps ellps))
	(shav (gds-distance-slc x2 y2 x1 y1 #:ellps ellps)))
    (list shav fwda rtna)))

;;-------------------;
;; Vincenty Inverse -;
;; http://en.wikipedia.org/wiki/Vincenty%27s_formulae
;;-------------------;
(define (gds-vincenty-converge diff-lon diff-lonp reduced-lat1 reduced-lat2 f limit)
  (let* ((sin-sigma (sqrt 
		     (+ (expt (* (cos reduced-lat2) 
				 (sin diff-lon)) 2)
			(expt (- (* (cos reduced-lat1)
				    (sin reduced-lat2))
				 (* (sin reduced-lat1) 
				    (cos reduced-lat2) 
				    (cos diff-lon))) 2))))
	 (cos-sigma (+ (* (sin reduced-lat1)
			  (sin reduced-lat2))
		       (* (cos reduced-lat1)
			  (cos reduced-lat2)
			  (cos diff-lon))))
	 (sigma (atan sin-sigma cos-sigma))
	 (sin-alpha (/ (* (cos reduced-lat1)
			  (cos reduced-lat2) 
			  (sin diff-lon))
		       sin-sigma))
	 (cos-sq-alpha (- 1 (expt sin-alpha 2)))
	 (cos-to-sigma-m (- cos-sigma 
			    (/ (* 2 (sin reduced-lat1) (sin reduced-lat2)) 
			       cos-sq-alpha))))
    (if (not (number? cos-to-sigma-m))
	(set! cos-to-sigma-m 0))
    (let* ((c (* (/ f 16)
		 (* cos-sq-alpha
		    (+ 4 (* f (- 4 (* 3 cos-sq-alpha)))))))
	   (diff-lam (+ diff-lonp
			(* (* (- 1 c) f sin-alpha)
			   (+ sigma (* (* c sin-sigma)
				       (+ cos-to-sigma-m
					  (* (* c cos-sigma)
					     (+ -1 (* 2 (expt cos-to-sigma-m 2)))))))))))
      ;; Check for convergence
      (if (or (< (abs (- diff-lam diff-lon)) 1e-12)
	      (< limit 0))
	  (list sin-sigma cos-sq-alpha cos-to-sigma-m cos-sigma diff-lam)
	  (gds-vincenty-converge diff-lam diff-lonp reduced-lat1 reduced-lat2 f (- limit 1))))))

(define* (gds-inverse-vincenty x1 y1 x2 y2 #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-inverse-vincenty lon-1 lat-1 lon-2 lat-2 [:ellps key]
    Perform Vincenty's Inverse formula. Given points LON-1 LAT-1 and
    and LON-2 LAT-2 return the distance between the points in meters as
    well as the forward and return azimuth.
    See `gds-ellps' for optional ellipsoid key values."
  (let* 
      ((fl (/ 1 (caddr (assoc ellps gds-ellps))))
       (mm (cadr (assoc ellps gds-ellps)))
       (ma (gds-minor (cadr (assoc ellps gds-ellps))
		      (caddr (assoc ellps gds-ellps))))
       (diff-lon (- (degree->radian x2) (degree->radian x1)))
       (diff-lat (- (degree->radian y2) (degree->radian y1)))
       (diff-lonp diff-lon)
       (reduced-lat1 (atan (* (- 1 fl)
			      (tan (degree->radian y1)))))
       (reduced-lat2 (atan (* (- 1 fl)
			      (tan (degree->radian y2)))))
       ;;--------------;
       ;; Convergence -;
       ;; `gds-vincenty-converge
       ;; Limit the number of iterations with the last argument to `gds-vincenty-converge
       ;;--------------;
       (conlist
	(gds-vincenty-converge diff-lon diff-lonp reduced-lat1 reduced-lat2 fl 40))
       (sin-sigma (car conlist))
       (cos-sq-alpha (car (cdr conlist)))
       (cos-to-sigma-m (car (cdr (cdr conlist))))
       (cos-sigma (car (cdr (cdr (cdr conlist)))))
       (usq (* cos-sq-alpha
	       (/ (- (expt mm 2)
		     (expt ma 2))
		  (expt ma 2))))
       (aa (+ 1 (* (/ usq 16384)
		   (+ 4096
		      (* usq
			 (+ -768
			    (* usq (- 320 (* 175 usq)))))))))
       (bb (* (/ usq 1024)
	      (+ 256 (* usq (+ -128 (* usq (- 74 (* 47 usq))))))))
       (delta-sigma
	(* bb sin-sigma
	   (+ cos-to-sigma-m
	      (/ bb (* 4 (* (- cos-sigma
			       (+ -1 (* 2 (expt cos-to-sigma-m 2)))
			       (/ bb (* 6 cos-to-sigma-m
					(* (+ -3 (* 4 (expt sin-sigma 2)))
					   (+ -3 (* 4 (expt cos-to-sigma-m 2))))))))))))))
    ;;---------;
    ;; Output -;
    ;; will output a list such that '(distance azi1 azi2)
    ;;---------;
    (list (* ma aa (- (atan sin-sigma cos-sigma) delta-sigma))
	  (+ (if (< diff-lat 0) 180 0)
	     (radian->degree
	      (atan (/ (* (cos reduced-lat2)
			  (sin (list-ref conlist 4)))
		       (- (* (cos reduced-lat1) (sin reduced-lat2))
			  (* (sin reduced-lat1) (cos reduced-lat2) (cos (list-ref conlist 4))))))))
	  (radian->degree
	   (atan (/ (* (cos reduced-lat1)
		       (sin (list-ref conlist 4)))
		    (+ (* (* -1 (sin reduced-lat1))
			  (cos reduced-lat2))
		       (* (cos reduced-lat1) (sin reduced-lat2) (cos (list-ref conlist 4))))))))))
;;------------------;
;; Vincenty Direct -;
;; http://en.wikipedia.org/wiki/Vincenty%27s_formulae
;;------------------;
(define (gds-vincenty-dc sigma sigma1 s bb ma limit)
  (let* ((sig2m (+ (* 2 sigma1)
		   sigma))
	 (sig-delta (* bb
		       (* (sin sigma)
			  (+ (cos sig2m)
			     (* (/ 1 4)
				(* bb
				   (* (cos sigma)
				      (- (+ -1
					    (* 2
					       (expt (cos sig2m) 2)))
					 (* (/ bb 6)
					    (cos sig2m)
					    (+ -3
					       (* 4 (expt (sin sigma) 2)))
					    (+ -3
					       (* 4 (expt (cos sig2m) 2))))))))))))
	 (sigma-l (+ (/ s ma)
		     sig-delta)))
    ;;-----------------------;
    ;; Test for convergence -;
    ;;-----------------------;
    (if (or (< (abs (- sigma-l sigma)) 1e-12)
	    (< limit 0))
	sigma-l
	(gds-vincenty-dc sigma-l sigma1 s bb ma (- limit 1)))))

(define* (gds-direct-vincenty x1 y1 a1 s #:key (ellps "WGS84"))
  "- Scheme Procedure: gds-direct-vincenty lon lat fwd-azimuth distance [#:ellps key]
    Perform Vincenty's direct formula. Given point LON LAT, 
    FWD-AZIMUTH and DISTANCE  return the point that is DISTANCE 
    away from point LON LAT in the direction of FWD-AZIMUTH.
    See `gds-ellps' for optional ellipsoid key values."
  (let* ((fl (/ 1 (caddr (assoc ellps gds-ellps))))
	 (mm (cadr (assoc ellps gds-ellps)))
	 (ma (gds-minor (cadr (assoc ellps gds-ellps)) (caddr (assoc ellps gds-ellps))))
	 (a1 (degree->radian a1))
	 (reduced-lat1 (atan (* (- 1 fl)
				(tan (degree->radian y1)))))
	 (sigma1 (atan (/ (tan reduced-lat1)
			  (cos a1))))
	 (sin-alpha (* (cos reduced-lat1)
		       (sin a1)))
	 (cos-sq-alpha (- 1 (expt sin-alpha 2)))
	 (usq (* cos-sq-alpha
		 (/ (- (expt mm 2)
		       (expt ma 2))
		    (expt ma 2))))
	 (aa (+ 1 (* (/ usq 16384)
		     (+ 4096
			(* usq
			   (+ -768
			      (* usq (- 320 (* 175 usq)))))))))
	 (bb (* (/ usq 1024)
		(+ 256 (* usq (+ -128 (* usq (- 74 (* 47 usq))))))))
	 ;;--------------;
	 ;; Convergence -;
	 ;; `gds-vincenty-dc
	 ;; Limit the number of iterations with the last argument to `gds-vincenty-dc
	 ;;--------------;
	 (sigma (gds-vincenty-dc (/ s (* ma aa)) sigma1 s bb ma 40))
	 (y2 (atan (/ (+ (* (sin reduced-lat1)
			    (cos sigma))
			 (* (cos reduced-lat1)
			    (sin sigma)
			    (cos a1)))
		      (* (- 1 fl)
			 (sqrt (+ (expt sin-alpha 2)
				  (expt (- (* (sin reduced-lat1)
					      (sin sigma))
					   (* (cos reduced-lat1)
					      (cos sigma)
					      (cos a1))) 2)))))))
	 (lamda (atan (/ (* (sin sigma)
			    (sin a1))
			 (- (* (cos reduced-lat1)
			       (cos sigma))
			    (* (sin reduced-lat1)
			       (sin sigma)
			       (cos a1))))))
	 (cc (* (* (/ fl 16)
		   cos-sq-alpha)
		(+ 4 (* fl
			(- 4 (* 3 (expt (cos a1) 2)))))))
	 (sig2m (+ (* 2 sigma1) sigma))
	 (ll (- lamda
		(* (- 1 cc)
		   fl
		   sin-alpha
		   (+ sigma
		      (* cc
			 (sin sigma)
			 (* (+ (cos sig2m)
			       (* cc
				  (cos sigma)
				  (+ -1 (* 2 (expt sig2m 2)))))))))))
	 (x2 (+ ll (degree->radian x1)))
	 (a2 (atan (/ sin-alpha
		      (+ (* (* -1 (sin reduced-lat1))
			    (sin sigma))
			 (* (cos reduced-lat1)
			    (cos sigma)
			    (cos a1)))))))
    ;;---------;
    ;; Output -;
    ;; will output a list such that '(x2 y2 azimuth)
    ;;---------;
    (list (radian->degree x2) (radian->degree y2) (+ 180 (radian->degree a2)))))

;;; End

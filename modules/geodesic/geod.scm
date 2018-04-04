;;; geod.scm
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
;; Some geodesic formalas for discovering the inverse/direct geodesic problem.
;; The default formula used is Vincenty's geodesic formula.
;;
;; This module provides public functions to the formulas found in `(geodesic formulae)
;;
;;; Code:

(define-module (geodesic geod)
  #:version (0 0 3)
  #:use-module (geodesic formulae)
  #:export
  (gds-distance gds-inverse gds-direct))

;; Return the distance between two points
(define* (gds-distance x1 y1 x2 y2 #:key (ellps "WGS84") (units "m") (formula 'vincenty))
  "- Scheme Procedure: gds-distance lon1 lat1 lon2 lat2 #:ellps #:units #:formula
    Return the distance between the points LON-1 LAT-1 and LON-2 LAT-2, 
    using the specified input ellipsoid and output units.
    The default ellipsoid is WGS84 and the default units are Meters (m)
    The default formula used is the inverse Vincenty formula.
    See `gds-ellps' and `gds-units' for key values."
  (let ((uass (assoc units gds-units))
	(eass (assoc ellps gds-ellps)))
    (if (and (pair? uass) (pair? eass))
	(case formula
	  ((vincenty)
	   (/ (car (gds-inverse-vincenty x1 y1 x2 y2 #:ellps ellps))
	      (cadr uass)))
	  ((haversine)
	   (/ (car (gds-inverse-haversine x1 y1 x2 y2 #:ellps ellps))
	      (cadr uass)))
	  ((slc)
	   (/ (car (gds-inverse-slc x1 y1 x2 y2 #:ellps ellps))
	      (cadr uass)))
	  (else
	   (error "invalid formula value: " formula)))
	(error "invalid key/pair: "(cons uass eass)))))

;; Solve the inverse geodesic problem given two points x1 y1 x2 y2
(define* (gds-inverse x1 y1 x2 y2 #:key (ellps "WGS84") (units "m") (formula 'vincenty))
  "- Scheme Procedure gds-inverse lon1 lat1 lon2 lat2 #:ellps #:units #:formula
    Solve the inverse geodesic problem given two points x1 y1 x2 y2
    The default ellipsoid is WGS84 and the default units are Meters (m)
    Return a list: `(distance forward-azimuth return-azimuth)'
    The default formula used is the inverse Vincenty formula.
    See `gds-ellps' and `gds-units' for key values."
  (let ((uass (assoc units gds-units))
	(eass (assoc ellps gds-ellps)))
    (if (and (pair? uass) (pair? eass))
	(case formula
	  ((vincenty)
	   (let ((return (gds-inverse-vincenty x1 y1 x2 y2 #:ellps ellps)))
	     (cons (/ (car return) (cadr uass)) (cdr return))))
	  ((slc)
	   (let ((return (gds-inverse-slc x1 y1 x2 y2 #:ellps ellps)))
	     (cons (/ (car return) (cadr uass)) (cdr return))))
	  ((haversine)
	   (let ((return (gds-inverse-haversine x1 y1 x2 y2 #:ellps ellps)))
	     (cons (/ (car return) (cadr uass)) (cdr return))))
	  (else
	   (error "invalid formula value: " formula)))
	(error "invalid key/pair: "(cons uass eass)))))

;; Solve the direct geodesic problem given an origin point x1 y1, a foward azimuth a1 and a distance s1.
(define* (gds-direct x1 y1 a1 s1 #:key (ellps "WGS84") (units "m") (formula 'vincenty))
  "- Scheme Procedure: gds-direct lon1 lat1 lon2 lat2 #:ellps #:units #:formula
    Solve the direct geodesic problem given an origin point x1 y1, a foward azimuth a1 and a distance s1.
    The default ellipsoid is WGS84 and the default input units are Meters (m).
    Return a list: `(lon-2 lat-2 return-azimuth)'
    The default formula used is the inverse Vincenty formula.
    See `gds-ellps' and `gds-units' for key values."
  (let ((uass (assoc units gds-units))
	(eass (assoc ellps gds-ellps)))
    (if (and (pair? uass) (pair? eass))
	(let ((s2 (* s1 (cadr uass))))
	  (case formula
	    ((vincenty)
	     (gds-direct-vincenty x1 y1 a1 s2 #:ellps ellps))
	    (else
	     (error "invalid formula value: " formula))))
	(error "invalid key/pair: "(cons uass eass)))))
;;; End

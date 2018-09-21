;;; vdatum.scm
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
;;; Code:

(define-module (geographic dem vdatum)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (geographic util popen)
  #:use-module (xyz xyz)
  #:export
  (vdatum-pt
   vdatum-pt->scm
   xyz->vdatum))

(define (vdatum-pt pt-line)
  (let ((vdi (open-input-pipe (string-append "dem vdatum ihoriz:nad83 ivert:nad83:m:height ohorz:nad83 overt:mhw:m:height -pt:" pt-line))))
    (acons "point" pt-line (vdatum-pt->scm vdi #t))))

(define* (vdatum-pt->scm vdatum-port #:optional (close? #f) (infos '()))
  (if (eof-object? (peek-char vdatum-port)) 
      (begin 
	(if close? (close-port vdatum-port)) 
	(reverse infos))
      (let ((info-line (read-line vdatum-port)))
	(let ((lon (string-match "Longitude/Easting/X:" info-line))
	      (lat (string-match "Latitude/Northing/Y:" info-line))
	      (height (string-match "Height/Z: " info-line)))
	  (cond
	   ((regexp-match? lon)
	    (let ((lons (string-split (string-trim-both (match:suffix lon)) #\sp)))
	      (vdatum-pt->scm vdatum-port close? (acons "lon" (list (string->number (car lons)) (string->number (car (reverse lons)))) infos))))
	   ((regexp-match? lat)
	    (let ((lats (string-split (string-trim-both (match:suffix lat)) #\sp)))
	      (vdatum-pt->scm vdatum-port close? (acons "lat" (list (string->number (car lats)) (string->number (car (reverse lats)))) infos))))
	   ((regexp-match? height)
	    (let ((heightss (string-split (string-trim-both (match:suffix height)) #\sp)))
	      (vdatum-pt->scm vdatum-port close? (acons "height" (list (string->number (car heightss)) (string->number (car (reverse heightss)))) infos))))
	   (else
	    (vdatum-pt->scm vdatum-port close? infos)))))))

(define* (xyz->vdatum xyz #:optional (port (open-output-string)))
  "- Scheme Procedure: xyz->scm [ port #:delim ]"
  (if (pair? xyz)
      (if (pair? (car xyz))
	  (let loop ((these-xyz xyz))
	    (when (pair? these-xyz)
		  (xyz->vdatum (car these-xyz) port)
		  (loop (cdr these-xyz))))
	  (begin
	    (let ((port (open-output-string)))
	      (let loop ((this-point xyz))
		(display (car this-point) port)
		(when (not (null? (cdr this-point)))
		      (display #\, port)
		      (loop (cdr this-point))))
	      (let ((vd (vdatum-pt (get-output-string port))))
		(xyz-display (list (cadr (assoc-ref vd "lon")) (cadr (assoc-ref vd "lat")) (cadr (assoc-ref vd "height"))))
		(close-output-port port)))))))
;;; End

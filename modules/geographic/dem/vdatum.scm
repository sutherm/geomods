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
  (find-vdatum
   vdatum-pt
   vdatum-file
   vdatum-pt->scm
   xyz->vdatum
   port->vdatum))

(define (find-vdatum)
  (format (current-error-port) "dem: Attempting to locate vdatum.~%")
  (let ((fdi (open-input-pipe "find / -type f 2> /dev/null | grep 'vdatum\\.jar'")))
    (read-line fdi)))

(define* (vdatum-pt pt-line 
		    #:key 
		    (vdatum (find-vdatum))
		    (ihorz "nad83") 
		    (ivert "navd88:m:height") 
		    (ohorz "nad83") 
		    (overt "mhw:m:height"))
  (let ((vdi (open-input-pipe (format #f "java -jar ~a ihorz:~a ivert:~a ohroz:~a overt:~a -pt:~a" vdatum ihorz ivert ohorz overt pt-line))))
    (acons "point" pt-line (vdatum-pt->scm vdi #t))))

(define* (vdatum-file filename
		      #:key 
		      (vdatum (find-vdatum))
		      (ihorz "nad83") 
		      (ivert "navd88:m:height") 
		      (ohorz "nad83") 
		      (overt "mhw:m:height"))
  (let ((vdi (open-input-pipe (format #f "java -jar ~a ihorz:~a ivert:~a ohroz:~a overt:~a -nodata -file:txt:space,0,1,2:~a:result" vdatum ihorz ivert ohorz overt filename))))
    (acons "name" filename (vdatum-pt->scm vdi #t))))

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

(define* (xyz->vdatum xyz 
		      #:optional 
		      (port (open-output-string)) 
		      (oport (current-output-port))
		      #:key
		      (vdatum (find-vdatum))
		      (ihorz "nad83") 
		      (ivert "navd88:m:height") 
		      (ohorz "nad83") 
		      (overt "mhw:m:height"))
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
	      (let ((vd (vdatum-pt (get-output-string port) #:vdatum vdatum #:ihorz ihorz #:ivert ivert #:ohorz ohorz #:overt overt)))
		(xyz-display (list (cadr (assoc-ref vd "lon")) (cadr (assoc-ref vd "lat")) (cadr (assoc-ref vd "height"))) oport)
		(close-output-port port)))))))

(define* (port->vdatum #:optional 
		       (port (current-input-port))
		       (oport (current-output-port))
		       #:key
		       (vdatum (find-vdatum))
		       (ihorz "nad83") 
		       (ivert "navd88:m:height") 
		       (ohorz "nad83") 
		       (overt "mhw:m:height"))
  "- Scheme Procedure: xyz->port [ input-port output-port ]"
  (xyz->scm port #:data-fun (lambda (xyz) (xyz->vdatum xyz oport #:vdatum vdatum #:ihorz ihorz #:ivert ivert #:ohorz ohorz #:overt overt)) #:loop-fun (lambda (a b) b)))

;;; End

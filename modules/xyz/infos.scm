;;*-scheme-*
;;
;;; infos.scm
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
;; Get some information about an xyz-data port/list.
;;
;;; Code:

(define-module (xyz infos)
  #:version (0 1 2)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (xyz xyz)
  #:export
  (infos->region xyz->infos xyz-port->infos infos->list inf->infos))

(define (infos->region infos)
  "Convert an infos-blob to a region-list."
  (list (assq-ref infos 'xmin )
	(assq-ref infos 'xmax )
	(assq-ref infos 'ymin )
	(assq-ref infos 'ymax )))

(define (infos->list infos)
  (if (not (null? infos))
      (list
       (assq-ref infos 'name)
       (list
        (assq-ref infos 'xmin)
        (assq-ref infos 'xmax)
        (assq-ref infos 'ymin)
        (assq-ref infos 'ymax))
       (list
        (assq-ref infos 'zmin)
        (assq-ref infos 'zmax)))))

(define (xyz-make-infos name x1 y1 z1)
  (acons 'name name
	 (acons 'xmin x1
		(acons 'xmax x1
		       (acons 'ymin y1
			      (acons 'ymax y1 
				     (acons 'zmin z1
					    (acons 'zmax z1
						   (acons 'count 1 '())))))))))

(define (xyz-set-infos! infos x1 y1 z1)
  (let ((xmin (assq-ref infos 'xmin)) (xmax (assq-ref infos 'xmax))
	(ymin (assq-ref infos 'ymin)) (ymax (assq-ref infos 'ymax))
	(zmin (assq-ref infos 'zmin)) (zmax (assq-ref infos 'zmax)))
    (if (< x1 xmin) (assq-set! infos 'xmin x1)
	(if (> x1 xmax) (assq-set! infos 'xmax x1)))
    (if (< y1 ymin) (assq-set! infos 'ymin y1)
	(if (> y1 ymax) (assq-set! infos 'ymax y1)))
    (if (< z1 zmin) (assq-set! infos 'zmin z1)
	(if (> z1 zmax) (assq-set! infos 'zmax z1))))
  (assq-set! infos 'count (1+ (assq-ref infos 'count))))

(define* (xyz->infos xyz-list #:optional (infos #f))
  (cond
   ((null? xyz-list) infos)
   ((not infos)
    (xyz->infos (cdr xyz-list) 
		(xyz-make-infos 
		 "xyz-info"
		 (list-ref (car xyz-list) 0) 
		 (list-ref (car xyz-list) 1) 
		 (list-ref (car xyz-list) 2))))
   (else
    (xyz->infos (cdr xyz-list) 
		(xyz-set-infos! 
		 infos
		 (list-ref (car xyz-list) 0) 
		 (list-ref (car xyz-list) 1) 
		 (list-ref (car xyz-list) 2))))))

(define* (xyz-port->infos #:optional 
			  (port (current-input-port))
			  (infos #f) 
			  #:key (test #f))
  (define xyz-infos
    (lambda (xyz)
      (if (or (not test) (test xyz))
	  (let ((x1 (vector-ref xyz 0)) 
		(y1 (vector-ref xyz 1)) 
		(z1 (vector-ref xyz 2)))
	    (set! infos 
		  (if infos
		      (xyz-set-infos! infos x1 y1 z1)
		      (xyz-make-infos (port-filename port) x1 y1 z1)))))))
  (xyz->scm port 
	    #:index "xyz" 
	    #:list/vector #f 
	    #:test-fun test 
	    #:data-fun xyz-infos 
	    #:loop-fun (lambda (a b) infos)))

(define* (inf->infos info-port #:optional (close? #f) (infos '()))
  (if (eof-object? (peek-char info-port)) 
      (begin 
	(if close? (close-port info-port))
	infos)
      (let ((info-line (read-line info-port)))
	(let ((count (string-match "Number of Records:" info-line))
	      (name (string-match "Swath Data File:" info-line))
	      (lon (string-match "Minimum Longitude:" info-line))
	      (lat (string-match "Minimum Latitude:" info-line))
	      (dep (string-match "Minimum Depth:" info-line)))
	  (cond
	   ((regexp-match? name)
	    (let ((nm
		   (string-trim-both (match:suffix name))))
	      (inf->infos info-port close? (acons 'name nm infos))))
	   ((regexp-match? count)
	    (let ((cnt
		   (string-trim-both (match:suffix count))))
	      (inf->infos info-port close? (acons 'count (string->number cnt) infos))))
	   ((regexp-match? lon)
	    (let ((lons
		   (string-split (string-trim-both (match:suffix lon)) #\sp)))
	      (inf->infos info-port close? (acons 'xmin (string->number (car lons)) (acons 'xmax (string->number (car (reverse lons))) infos)))))
	   ((regexp-match? lat)
	    (let ((lats
		   (string-split (string-trim-both (match:suffix lat)) #\sp)))
	      (inf->infos info-port close? (acons 'ymin (string->number (car lats)) (acons 'ymax (string->number (car (reverse lats))) infos)))))
	   ((regexp-match? dep)
	    (let ((deps
		   (string-split (string-trim-both (match:suffix dep)) #\sp)))
	      (inf->infos info-port close? (acons 'zmin (string->number (car deps)) (acons 'zmax (string->number (car (reverse deps))) infos)))))
	   (else
	    (inf->infos info-port close? infos)))))))

;;; End

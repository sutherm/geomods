;;; lastools.scm
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
;;; Code:

(define-module (geographic dem lastools)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (geographic util popen)
  #:use-module (xyz xyz)
  #:export
  (lasinfo
   lasinfo->scm
   las->region
   las->xyz
   las->port))

(define* (las->xyz filename #:optional (oport (current-output-port)))
  (let ((lasti (lasinfo filename)))
    (let ((lastx (open-input-pipe (string-append "las2txt -parse xyz -keep_class 2 29 -stdout -i " filename))))
      (xyz->port lastx oport))))

(define* (las->port filename #:optional (oport (current-output-port)))
  (let ((lasti (lasinfo filename)))
    (open-input-pipe (string-append "las2txt -parse xyz -keep_class 2 29 -stdout -i " filename))))

;; Run gdalinfo and parse the output to an association-list
;; with keys: "filename" "driver" "origin" "size" "pixel-size" "z-range"
(define (lasinfo filename)
  (let ((lasti (open-input-pipe (string-append "lasinfo -nc -nv -stdout -i " filename))))
    (acons "filename" filename (lasinfo->scm lasti #t))))

(define (las->region filename)
  (let* ((las-infos (lasinfo filename))
	 (mins (assoc-ref las-infos "min"))
	 (maxs (assoc-ref las-infos "max")))
    (list (car mins) (car maxs) (cadr mins) (cadr maxs))))

(define* (lasinfo->scm las-port #:optional (close? #f) (infos '()))
  (if (eof-object? (peek-char las-port)) 
      (begin 
	(if close? (close-port las-port)) 
	(reverse infos))
      (let ((info-line (read-line las-port)))
	(let ((min (string-match "min x y z:" info-line))
	      (max (string-match "max x y z:" info-line)))
	  (cond
	   ((regexp-match? min)
	    (let ((mins
		   (map string->number 
			(map string-trim-both 
			     (string-split (string-trim-both (match:suffix min)) #\sp)))))
	      (lasinfo->scm las-port close? (acons "min" mins infos))))
	   ((regexp-match? max)
	    (let ((maxs
		   (map string->number 
			(map string-trim-both 
			     (string-split (string-trim-both (match:suffix max)) #\sp)))))
	      (lasinfo->scm las-port close? (acons "max" maxs infos))))
	   (else
	    (lasinfo->scm las-port close? infos)))))))
;;; End

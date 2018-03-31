;;; gdal.scm
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

(define-module (geographic dem gdal)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (geographic util popen)
  #:use-module (xyz xyz)
  #:export
  (gdalinfo))

;; Run gdalinfo and parse the output to an association-list
;; with keys: "filename" "driver" "origin" "size" "pixel-size" "z-range"
(define (gdalinfo filename)
  (let ((gdi (open-input-pipe (string-append "gdalinfo -stats " filename))))
    (acons "filename" filename (gdalinfo->scm gdi #t))))

(define* (gdalinfo->scm gdal-port #:optional (close? #f) (infos '()))
  (if (eof-object? (peek-char gdal-port)) 
      (begin 
	(if close? (close-port gdal-port)) 
	(reverse infos))
      (let ((info-line (read-line gdal-port)))
	(let ((driver (string-match "Driver: " info-line))
	      (origin (string-match "Origin = " info-line))
	      (size (string-match "Size is " info-line))
	      (pixel-size (string-match "Pixel Size = " info-line))
	      (min (string-match "Minimum=" info-line)))
	  (cond
	   ((regexp-match? driver)
	    (gdalinfo->scm gdal-port close? (acons "driver" (list (match:suffix driver)) infos)))
	   ((regexp-match? size)
	    (let ((sizes 
		   (map string->number 
			(map string-trim-both 
			     (string-split (match:suffix size) #\,)))))
	      (gdalinfo->scm gdal-port close? (acons "size" sizes infos))))
	   ((regexp-match? origin)
	    (let ((origins 
		   (map string->number 
			(string-split 
			 (string-trim-both 
			  (match:suffix origin) 
			  (lambda (x) 
			    (or (eq? #\( x) (eq? #\) x)))) 
			 #\,))))
	      (gdalinfo->scm gdal-port close? (acons "origin" origins infos))))
	   ((regexp-match? pixel-size)
	    (let ((pixel-sizes
		   (map string->number 
			(string-split 
			 (string-trim-both 
			  (match:suffix pixel-size) 
			  (lambda (x) 
			    (or (eq? #\( x) (eq? #\) x)))) 
			 #\,))))
	      (gdalinfo->scm gdal-port close? (acons "pixel-size" pixel-sizes infos))))
	   ((regexp-match? min)
	    (let* ((min-line (match:string min))
		   (ls (string-split min-line #\,))
		   (minz (string->number (cadr (string-split (car ls) #\=))))
		   (maxz (string->number (cadr (string-split (cadr ls) #\=)))))
	    (gdalinfo->scm gdal-port close? (acons "z-range" (list minz maxz) infos))))
	   (else
	    (gdalinfo->scm gdal-port close? infos)))))))
;;; End

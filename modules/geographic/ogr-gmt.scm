;;-*-scheme-*-
;;
;;; ogr-gmt.scm
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

(define-module (geographic ogr-gmt)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (xyz xyz)
  #:export
  (ogr-gmt-version
   ogr-gmt-file?
   ogr-gmt-generate-header
   ogr-gmt-scantofeature
   ogr-gmt-read-feature
   ogr-gmt-read-features
   ogr-gmt-readxy-line
   ogr-gmt->scm
   with-ogr-gmt-port
   xyz->ogr-gmt
   polygon->ogr-gmt
   polygons->ogr-gmt))

(define ogr-gmt-version "0.1.4")

(define (feature-line? line)
  (equal? (car (string->list line)) #\>))

(define (comment-line? line)
  (equal? (car (string->list line)) #\#))

(define (comment-char? char)
  (equal? char #\#))

(define (feature-char? char)
  (equal? char #\>))

(define (split-comment line)
  (map string-trim-both (string-split line #\@)))

(define (ogr-gmt-file? input)
  (if (not input) #f
      (let ((infile (open-file input "r")))
	(if (eof-object? (peek-char infile)) #f
	    (let* ((lc (peek-char infile))
		   (inline (read-line infile))
		   (header (split-comment inline)))
	      (if (not (comment-char? lc)) #f
		  (or (string-ci=? (string-trim-both (cadr header)) "VGMT-1.0")
		      (string-ci=? (string-trim-both (cadr header)) "VGMT1.0"))))))))

(define* (ogr-gmt-generate-header geometry #:key names types projection-ogr-gmt projection-proj projection-wkt projection-epsg region)
  (display (string-append "# @VGMT-1.0 @G" geometry))
  (newline)
  (if (and names types)
      (begin
	(display (string-append "# @N" names "\n# @T" types))
	(newline)))
  (if projection-ogr-gmt
      (begin
	(display (string-append "# @Jg" projection-ogr-gmt))
	(newline)))
  (if projection-proj
      (begin
	(display (string-append "# @Jp" projection-proj))
	(newline)))
  (if projection-epsg
      (begin
	(display (string-append "# @Je" projection-epsg))
	(newline)))
  (if projection-wkt
      (begin
	(display (string-append "# @Jw" projection-wkt))
	(newline)))
  (if region
      (begin
	(display (string-append "# @R" region))
	(newline))))
		 
;; Scan to the next feature in the ogr-gmt vector port.
(define (ogr-gmt-scantofeature port)
  (if (eof-object? (peek-char port)) #f
      (if (feature-char? (peek-char port))
	  (begin
	    (read-line port)
	    #t)
	  (begin
	    (read-line port)
	    (ogr-gmt-scantofeature port)))))
      
;; todo: use xyz:? to import feature-line...
(define (ogr-gmt-read-feature input-port this-feature)
  (if (not (eof-object? (peek-char input-port)))
      (let ((inline (read-line input-port)))
	(cond 
	 ((comment-line? inline)
	  (ogr-gmt-read-feature input-port this-feature))
	 ((feature-line? inline)
	  this-feature)
	 (else 
	  (ogr-gmt-read-feature input-port (append (list (map string->number (string-split inline #\sp))) this-feature)))))
      this-feature))

;; Read in all the ogr-gmt features.
(define (ogr-gmt-read-features input-port features)
  (if (not (eof-object? (peek-char input-port)))
      (ogr-gmt-read-features input-port (append (list (ogr-gmt-read-feature input-port '())) features))
      features))

;; Read in all the ogr-gmt features from port to a scheme list of the vector.
(define (ogr-gmt->scm port)
  (ogr-gmt-scantofeature port)
  (ogr-gmt-read-features port '()))

(define (with-ogr-gmt-port port thunk)
  (ogr-gmt-scantofeature port)
  (let loop ((feature (ogr-gmt-read-feature port '())))
    (if (procedure? thunk)
	(thunk feature))
    (if (not (eof-object? (peek-char port)))
	(loop (ogr-gmt-read-feature port '())))))

(define* (xyz->ogr-gmt input-port #:optional (output-port (current-output-port)))
  "xyz-port -> gmt-point-vector."
  (with-xyz-port
   input-port
   (lambda (xyz)
     (display ">\n# @D" output-port)
     (display-xyz xyz output-port #:delim #\| #:trim #t)
     (newline)
     (display-xyz xyz output-port))))

;; display the gmt vector....
(define* (display-xys xys #:optional (port (current-output-port)))
  "print xys to stdout"
  (if (pair? xys)
      (format #t "~a ~a~%" (car xys) (cadr xys))))

(define* (polygon->ogr-gmt ply name #:key (header? #t) (connect? #f))
  "Generate a ogr-gmt formatted polygon of ply."
  (if header?
      (ogr-gmt-generate-header "POLYGON"))
  (format #t ">~%# @D~a~%" name)
  (map display-xys ply)
  (if connect?
      (display-xys (car ply))))

(define* (polygons->ogr-gmt plys name #:key (header? #t) (connect? #f))
  "Generate a ogr-gmt formatted polygon of ply."
  (if header?
      (ogr-gmt-generate-header "MULTIPOLYGON"))
  (map (lambda (ply)
	 (format #t ">\n# @D~a\n" name)
	 (map display-xys ply)
	 (if connect?
	     (display-xys (car ply))))
       plys))

;;---END

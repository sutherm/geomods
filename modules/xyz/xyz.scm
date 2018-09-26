;;*-scheme-*
;;; xyz.scm
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
;;; Commentary:
;;
;; XYZ module influenced by nala ginrut's guile-csv, but uses (ice-9 rdelim) and has per/line delimiter detection for mixed files
;; and is only concerned with numerical data from the csv.
;;
;; Maybe use hooks instead?
;;
;;; Code:

(define-module (xyz xyz)
  #:use-module (ice-9 rdelim)
  #:export (xyz-exts xyz-read-line make-xyz-reader xyz-display xyz->scm xyz->port))

(define xyz-exts
  '("xyz"))

(define* (xyz-read-line 
	  #:optional 
	  (port (current-input-port))
	  #:key 
	  (data-fun (lambda (data-line) 
		      (map string->number data-line))))
  "- Scheme Procedure: xyz-read-line [ port #:data-fun ]"
  (define %known-delimiters 
    '(#\tab #\sp #\, #\ht #\; #\| #\/ #\:))
  (define string-split/delims 
    (lambda (str)
     (let lp ((delims %known-delimiters))
	(let ((l (string-split str (car delims))))
	  (cond
	   ((not (null? (cdr l))) l)
	   ((not (null? (cdr delims)))
	    (lp (cdr delims)))
	   (else '()))))))
  (let ((this-char (peek-char port)))
    (cond 
     ((eof-object? this-char)
      this-char)
     (else
      (let ((data-line (map string-trim-both (string-split/delims (string-trim-both (read-line port))))))
	(data-fun data-line))))))

(define* (make-xyz-reader 
	  #:optional 
	  (port (current-input-port))
	  #:key 
	  (data-fun (lambda (data-list) 
		      (map string->number data-list)))
	  (loop-fun (lambda (current-line out-data)
		      (cons current-line out-data))))
  "- Scheme Procedure: make-xyz-reader [ port #:data-fun ]"
  (lambda (port)
    (let lp ((this-port port) (out-data '()))
      (let ((this-line (xyz-read-line port #:data-fun data-fun)))
	(if (eof-object? this-line) out-data
	    (lp this-port (loop-fun this-line out-data)))))))

;; Read in xyz data from port. Return a list of xy* point(s).
;; Use #:index to set the column index; i.e. "xyz" if the columns of the xyz data are x,y,z; "yxz" if xy are flipped, etc.
;; If #:header is #t will discard the first record.
;; Set #:list/vector to either #t (list) of #f (vector) to output either a list of point-lists '((x y z) (x y z) ... (x y z)) or a list
;; of vector-points '(#(x y z) #(x y z) ... #(x y z)).
;; The #:data-fun key sets how to process a xyz-record.
;; The #:loop-fun key sets what to do with the xyz-record (default is to cons them all together)
;; The #:test-fun key sets a boolean test on each xyz-record and will only return the xyz-point if it passes the test. 
;; i.e. (lambda (xyz) (< (cadddr xyz) 0)) to return only points which have a z value greater than 0.
;; (xyz->scm (open-file "test.yxz" "r") 
;;    #:list/vector #f
;;    #:data-fun (lambda (xyz) (map string->number xyz)) 
;;    #:loop-fun (lambda (xyz xyzs) (cons xyz xyzs))
;;    #:index "yxz"
;; or:
;; (xyz->scm (open-file "test.xyz" "r"))
(define* (xyz->scm #:optional 
		   (port (current-input-port)) 
		   #:key 
		   (header #f) 
		   (list/vector #t)
		   (data-fun #f) 
		   (loop-fun #f) 
		   (test-fun #f) 
		   (index "xyz"))
  "- Scheme Procedure: xyz->scm [ port #:header #:test-fun #:index ]"
  (define xyz-line->xyz-list
    (lambda (xyz-line)
      (let ((xpos (string-index index #\x))
	    (ypos (string-index index #\y))
	    (zpos (string-index index #\z)))
	((if list/vector list vector)
	 (if xpos (string->number (list-ref xyz-line xpos)) (nan))
	 (if ypos (string->number (list-ref xyz-line ypos)) (nan))
	 (if zpos (string->number (list-ref xyz-line zpos)) (nan))))))
  (define this-data-fun
    (if data-fun (lambda (xyz-line) 
		   (data-fun (xyz-line->xyz-list xyz-line)))
	xyz-line->xyz-list))
  (define this-loop-fun
    (if loop-fun loop-fun
	(lambda (xyz xyzs)
	  (if (or (not test-fun)
		  (test-fun xyz))
	      (cons xyz xyzs) xyzs))))
  (define reader (make-xyz-reader port 
				  #:data-fun this-data-fun 
				  #:loop-fun this-loop-fun))
  (let ((xyz (reader port)))
    (if header (cdr xyz) xyz)))

;; Read in xyz data from port and display it to oport.
;; Set the #:test-fun key to test the points before displaying them to oport.
(define* (xyz->port #:optional 
		    (port (current-input-port)) 
		    (oport (current-output-port))
		    #:key
		    (verbose #f)
		    (infos #f)
		    (test-fun #f)
		    (weight #f))
  "- Scheme Procedure: xyz->port [ input-port output-port ]"
  (let ((count 0))
    (xyz->scm port 
	      #:data-fun (lambda (xyz) 
			   (if (or (not test-fun) (test-fun xyz))
			       (begin
				 (set! count (+ count 1))
				 (xyz-display xyz oport #:weight weight))))
	      #:loop-fun (lambda (a b) b))
    (if verbose
	(format (current-error-port) "xyz: ~a points passed in ~a~%" count (if infos (assq-ref infos 'name) (port-filename port))))))

(define* (xyz-display xyz
		      #:optional (port (current-output-port))
		      #:key 
		      (delim #\sp)
		      (weight #f))
  "- Scheme Procedure: xyz->scm [ port #:delim ]"
  (if (pair? xyz)
      (if (pair? (car xyz))
	  (let loop ((these-xyz xyz))
	    (when (pair? these-xyz)
		  (xyz-display (car these-xyz) port)
		  (loop (cdr these-xyz))))
	  (begin
	    (let loop ((this-point xyz))
	      (display (car this-point) port)
	      (when (not (null? (cdr this-point)))
		    (display delim port)
		    (loop (cdr this-point))))
	    (if weight (begin
			 (display delim port)
			 (display weight port)))
	    (newline port)))))

;;; End

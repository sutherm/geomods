;; snarf.scm - Glob xyz files in the current directory to a datalist.
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
;; Usage: snarf [ -dfhrxuvT [ args ] ] [ files ]
;;
;; snarf <-> infos
;;
;; -u - snarf and dump the file delimiter.
;;
;; Using -T add a snarf test for each xyz line. This should be scheme code that accepts a point '(x y ...) as an argument and returns #t or #f.
;; -T "(lambda (xyz) (> (caddr xyz) 0))" to return points with z values greater than zero (0).
;; -T "(lambda (xyz) (xyz-in-region? xyz '(-67 -65 18 19)))" to return only points in the given region.
;;
;; You can also write a function in your .xyz file and use that.
;; e.g.
;; (define (z-is-nan xyz)
;;    (= z (nan)))
;; snarf -T z-is-nan
;;
;; Or even combine many tests into one, as long as the function accepts an xyz point '(x y z ...) as input and returns #t or #f.
;;
;; Any further snarfing will be performed only on points that pass the test(s).
;;
;; -r - only return the x/y region information.
;;
;; -x - Find the convex-hull points from the x/y data.
;;
;; -d - Snarf the density of the x/y data.
;;
;; By default, snarf will return a scheme a-list to standard-output of standard-information.
;; '(name xmin xmax ymin ymax zmin zmax count)
;;
;; Use -f to format the output.
;; e.g.
;; xyz snarf -rf file.xyz
;; ==> xmin/xmax/ymin/ymax
;;
;; for i in *.xyz; do if [ ! -f $i.scm ]; then xyz snarf $i > $i.scm; fi; done
;;
;;; Code:

(define-module (geographic scripts xyz snarf)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (xyz xyz)
  #:use-module (xyz infos)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (hulls convex-hull)
  #:export (snarf))

(define snarf-version "0.0.2")

(define %summary "Snarf information from xyz data.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (region (single-char #\r) (value #f))
    (format (single-char #\f) (value #f))
    (density (single-char #\d) (value #f))
    (convex-hull (single-char #\x) (value #f))
    (test (single-char #\T) (value #t))
    (snarf-delim (single-char #\u) (value #f))))

(define (display-help)
  (format #t "\
~a
 snarf <-> infos

usage: snarf [ dfhrxuvT [ args ] ] [ files ]
" %summary))

(define (display-version)
  (format #t "\
snarf (XYZ) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" snarf-version))

(define (snarf . args)
  (let* ((options (getopt-long (cons "snarf" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (region-wanted (option-ref options 'region #f))
	  (format-wanted (option-ref options 'format #f))
	  (density-wanted (option-ref options 'density #f))
	  (hull-wanted (option-ref options 'convex-hull #f))
	  (test (option-ref options 'test #f))
	  (snarf-delim (option-ref options 'snarf-delim #f)))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let ((input (option-ref options '() #f)))
	  (let* ((infile (if (not (pair? input)) 
			     (current-input-port) 
			     (open-file (car input) "r"))))
	    (cond
	     ;; snarf the delimiter
	     (snarf-delim
	      (write (guess-file-delimiter infile) (current-output-port))
	      (newline (current-output-port)))
	     ;; snarf the density
	     (density-wanted
	      (let* ((my-xys (xyz->scm infile #:list/vector #f))
		     (xy-count (length my-xys))
		     (hull (amc-convex-hull my-xys))
		     (area (hull-area (append hull (list (car hull))))))
		(write (/ xy-count area))
		(newline)))
	     ;; snarf the hull
	     (hull-wanted
	      (let ((my-xys (xyz->scm infile #:list/vector #f)))
		(if format-wanted
		    (polygon->ogr-gmt (map vector->list (amc-convex-hull my-xys)) (basename (port-filename infile)))
		    (write (amc-convex-hull my-xys)))))
	     ;; snarf the extent infos.
	     (else
	      (let ((these-infos (xyz-port->infos infile)))
		(cond
		 (region-wanted
		  (if format-wanted
		      (display (region->gmt-region (infos->region these-infos)))
		      (write (infos->region these-infos))))
		 (else
		  (if format-wanted
		      (format #t "(~{~a~^~% ~})" these-infos)
		      (write these-infos))))
		(newline)))))))))))

(define main snarf)

;; Guess the delimiter in an xyz file port.
;; does not rewind port...
;; though the only real use of this is to run it on a file
;; just for curiousity. with-xyz-port guesses the port automatically.
(define (guess-file-delimiter port)
  "Guess the delimiter of port."
  (define %known-delimiters 
    '(#\sp #\, #\ht #\; #\| #\/ #\:))
  (define* (guess-line-delimiter xyz-line delims)
    (let ((l (string-split (string-trim-both xyz-line) (car delims))))
      (if (> (length l) 1) (car delims)
	  (if (null? (cdr delims)) #f
	      (guess-line-delimiter xyz-line (cdr delims))))))
  (let ((this-char (peek-char port)))
    (cond
     ((eq? this-char #\#)
      (read-line port)
      (guess-file-delimiter port))
     ((eof-object? this-char)
      this-char)
     (else
      (let ((l (read-line port)))
	(guess-line-delimiter l %known-delimiters))))))

;; Calculate the area of a convex hull return value in meters.
;; TODO: add ellps and units options.
;; TODO: move this to geodesia or elsewhere.
(define* (hull-area xyzs #:optional (sums 0))
  (if (null? (cdr xyzs)) (/ 6378137.0 (* (abs sums)) 2)
      (let ((p0 (car xyzs))
	    (p1 (cadr xyzs)))
	(hull-area (cdr xyzs) 
		   (+ sums (* (+ (vector-ref p0 0) 
				 (vector-ref p1 1))
			      (- (vector-ref p1 1) 
				 (vector-ref p0 0))))))))	     

;;; End

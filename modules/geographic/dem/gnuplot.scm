;;; gnuplot.scm
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

(define-module (geographic dem gnuplot)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (gms ice-9 popen)
  #:use-module (xyz xyz)
  #:use-module (geographic regions)
  #:use-module (geographic spatial)
  #:use-module (geographic quad-tree)
  #:export
  (gms-start-gnuplot
   gms-stop-gnupolot
   gms-start-io-gnuplot
   gms-catch-mouse
   gms-box-mouse
   gms-encircle-nearest-to-mouse
   gms-read-line-gnuplot/not
   gms-quickview
   gnuplot-mode
   xyz->gnuplot
   line->gnuplot
   gms-plot-xyz 
   gms-plot-xyz-scatter 
   gms-plot-xyz-3d 
   gms-plot-xyz-h3
   gms-plot-xyz-contour))

;; set up gnuplot pipe

;;(use-modules (ice-9 popen))

(define (gms-start-gnuplot)
  (open-output-pipe "gnuplot"))

(define (gms-stop-gnuplot port)
  (close-port port))

(define (gms-start-io-gnuplot)
  (let ((gp (open-input-output-pipe "gnuplot -p")))
    (format (cadr gp) "set print '-'~%")
    gp))

(define (gms-catch-mouse port)
  (format (cadr port) "pause mouse any
if( MOUSE_BUTTON==3 ) print MOUSE_X,MOUSE_Y; else print 'null' 
")
  (xyz-string->scm (read-line (car port))))

(define (gms-box-mouse port)
  (let ((mp (gms-catch-mouse port)))
    (format (cadr port) "set object 1 circle center ~a,~a size scr 0.1 fc rgb 'navy'
replot
" (car mp) (cadr mp)))) 

(define* (gms-encircle-nearest-to-mouse port xyzs-tree #:optional (obj "1"))
  (let* ((mp (gms-catch-mouse port))
	 (xyz_n (car (quad-tree-nearest mp xyzs-tree))))
    (format (cadr port) "set object ~a circle center ~a,~a size scr 0.01 fc rgb 'navy'
replot
" obj (car xyz_n) (cadr xyz_n))
    xyz_n))

(define (gms-read-line-gnuplot/not port)
  (if (char-ready? port)
      (read-line port)
      #f))

(define (gms-quickview vfile)
  (let ((gp (open-output-pipe "gnuplot")))
    (format gp "plot \"~a\"~%" vfile)))

(define (with-gnuplot-port port thunk)
  (if (procedure? thunk)
      (thunk port)
      (display thunk)))

(define* (gnuplot-mode #:optional (iport #f) (str (if iport "refresh~%" "clear~%")))
  (if (not iport) (format #t "Welcome to gnuplot-mode

Run a scheme function on the gnuplot port using a labmda function: (lambda (port) (display 'clear' port))
or a function that takes a port as argument: (lambda (x) (gm-replot x))

Regular commands will be send to gnuplot as gnuplot commands.

type 'exit' to exit.
"))
  (let ((port (if iport iport (open-input-output-pipe "gnuplot"))))
    (if (and (not (eof-object? str))
	     (not (string-ci=? str "exit")))
	(if (> (string-length str) 0)
	    (begin
	      (cond ((equal? (list-ref (string->list str) 0) #\()
		     (with-gnuplot-port (cadr port) (eval-string str))
		     (newline))
		    (else
		     (let ((gpstr (string-append str " ~%")))
		       (format (cadr port) gpstr))))
	      (gnuplot-mode port (readline "gnuplot>")))
	    (gnuplot-mode port (readline "gnuplot> ")))
	(format (cadr port) "exit\n"))))

(define (xyz->gnuplot xyzs port)
  (format port "plot '-' using 1:2:3 with points~%")
  (if (port? xyzs)
      (xyz->port xyzs port)
      (xyz-format xyzs port))
  (format port "e~%"))

(define* (line->gnuplot xys port #:key (polygon #f))
  (format port "plot '-' using 1:2 with line~%")
  (xyz-format xys port)
  (if polygon
      (xyz-format (list (car xys)) port))
  (format port "e~%"))

(define (gms-plot-xyz port xyz-file)
  (format port "set view map
set size ratio .5
set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
splot \"~a\" using 1:2:3 with points pointtype 5 pointsize 1 palette linewidth 30
" xyz-file))

(define (gms-plot-xyz-contour port xyz-file)
  (format port "set dgrid3d ,,16
set contour
set style data lines
splot '~a'
" xyz-file))

(define (gms-plot-xyz-h3 port xyz-file)
  (format port "set hidden3d
set dgrid3d 50,50 qnorm 2
set contour
set style data lines
splot '~a'
" xyz-file))

(define (gms-plot-xzy-scatter port xyz-file)
    (format port "set style line 1 lt 3 pt 4
set grid
set timestamp
set view map
splot \"~a\" using 1:2:3 with points pointsize 1 pointtype 3 palette
" xyz-file))

(define (gms-plot-xyz-3d port xyz-file)
  (format port "splot \"~a\" using 1:2:3 with points palette pointsize 2 pointtype 7\n" xyz-file))

;;; End

;;-*-scheme-*-
;;
;;; gmt.scm
;; Copyright (c) 2018 Matthew Love
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
;;; Commentary
;;
;; Functions for building DEMs and related tasks.
;;
;;; Code:

(define-module (geographic dem gmt)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (xyz xyz)
  #:use-module (geographic util popen)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (geographic rasters)
  #:use-module (geographic spatial)
  #:export 
  (dem-inc->gmt-inc
   gmt-inc->inc
   dem-output-name
   dem-make-mbgrid-cmd
   dem-make-gmt-cmd
   gmt-cmd->scm
   gmt-cmd->port))

;; Convert a increment pair to gmt formatted string:
;; e.g. '(xi yi) ==> xi/yi
(define (dem-inc->gmt-inc inc u)
  (if (pair? inc)
      (string-append (string-join (map number->string inc) "/") u)
      (string-append (number->string inc) u)))

;; transform increment to degrees or meters
;; e.g. .3333333s ==> 0.0000945
(define (gmt-inc->inc increment)
  (let ((inc-match (string-match "[a-z]" increment)))
    (if (not (regexp-match? inc-match)) #f
	(let ((inc-l (string->symbol (match:substring inc-match)))
	      (inc (string->number (match:prefix inc-match))))
	  (case inc-l
	    ((c)
	     (/ inc 3600))
	    ((s)
	     (/ inc 3600))
	    ((m)
	     (/ inc 360))
	    ((i)
	     (gds-degree-length 0 0 #:units "mi"))
	    ((n)
	     (gds-degree-length 0 0 #:units "kmi"))
	    ((k)
	     (gds-degree-length 0 0 #:units "km"))
	    ((e)
	     (gds-degree-length 0 0 #:units "m")))))))

(define (gmt-inc->title-inc inc)
  (let ((i (string-match "[a-z]" inc)))
    (if (not (regexp-match? i)) #f
	(let* ((iv (inexact->exact (string->number (match:prefix i))))
	       (riv (string-join (string-split (number->string (rationalize iv 1/10)) #\/) "")))
	  riv))))

;; generate output file name for DEM
(define* (dem-output-name region #:key (inc ".1111111s") (name "ncei") (version "1"))
  (let ((n (string-join (string-split (number->string (cadddr region)) #\.) "x"))
	(w (string-join (map (lambda (x) (string-trim x #\-)) (string-split (number->string (car region)) #\.)) "x"))
	(d (number->string (+ 1900 (tm:year (localtime (current-time))))))
	(i (gmt-inc->title-inc inc)))
    (string-append name "_" i "_n" n "_w" w "_" d "_v" version)))

(define (format-degrees degree)
  (format #f " -E~f/~f/degrees!" degree degree))

;; Make a mbraster command with some sane defaults.
(define* (dem-make-mbgrid-cmd region datalist 
			      #:key 
			      (inc ".3333333s")
			      (out-name #f) 
			      (verbose #f))
  (let ((gmt-region (region->gmt-region region)))
    (string-append "mbgrid -I" datalist " -R" gmt-region (format-degrees (gmt-inc->inc inc))
		   (if out-name (string-append " -O" out-name) 
		       (string-append " -O" (basename datalist ".datalist")))
		   (if verbose " -V" "")
		   " -A2 -G3 -N -C40/3 -F1 -T35 -X0.2 -M")))

;; Make a GMT command...only really good for gmtselect/blockm*/*surface
(define* (dem-make-gmt-cmd cmd-name region 
			   #:key 
			   (inc ".3333333s") 
			   (out-name #f) 
			   (verbose #f)
			   (extra #f))
  (let ((gmt-region (region->gmt-region region)))
    (string-append "gmt " cmd-name " -R" gmt-region (if inc (string-append " -I" inc) "")
		   (if out-name (string-append " -G" out-name ".grd") "")
		   (if extra (string-append " " extra " ") "") (if verbose " -V" ""))))

;; Open a port to a gmt command and feed it pts.
;; Catch the return points as well.
(define (gmt-cmd->scm gmt-cmd pts)
  (let* ((gmt-procs (open-input-output-pipe gmt-cmd))
	 (rp (car gmt-procs))
	 (wp (cadr gmt-procs)))
    (xyz-display pts wp)
    (close-port wp)
    (let ((xyzs (xyz->scm rp)))
      (close-port rp)
      xyzs)))

(define* (gmt-cmd->port gmt-cmd xyz-port #:optional (oport (current-output-port)) #:key (weight #f))
  (let* ((gmt-procs (open-input-output-pipe gmt-cmd))
	 (rp (car gmt-procs))
	 (wp (cadr gmt-procs)))
    (xyz->port xyz-port wp)
    (close-port wp)
    (xyz->port rp oport #:weight weight)
    (close-port rp)))

;;; End

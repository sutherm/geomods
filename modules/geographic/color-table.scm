;;-*-scheme-*-
;;
;;; color-table.scm
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
;; Generate a ETOPO1 style color-table given the min and max z-range.
;;
;;; Code:

;; Define the module
(define-module (geographic color-table)
  #:version (0 0 1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:export (cpt:scale cpt:read cpt:format cpt:etopo1 cpt:terra cpt->scm))

(define (make-tr-list min max int trs)
  (if (>= min max) (reverse (append (list max) trs))
      (make-tr-list (+ min int) max int (append (list min) trs))))

;; Scale a color-table to min/max
(define (cpt:scale colors min max)
  (let ((out-colors '())
	(color-minmax (color-scale-minmax colors)))
    (let loop ((these-colors colors))
      (let* ((this-color (car these-colors))
	     (se (scale-elev (car this-color) min max (car color-minmax) (cadr color-minmax))))
	(set! out-colors (append (list (list se (cadr this-color))) out-colors))
	(if (not (null? (cdr these-colors)))
	    (loop (cdr these-colors))
	    (reverse out-colors))))))

;; Get the min and max values from a color-table
(define (color-scale-minmax colors)
  (list (caar colors) (caar (reverse colors))))

;; Scale a value to min/max
(define (scale-elev val min max rmin rmax)
  (if (and (> val 0) (> max 0))
      (/ (* max val) rmax)
      (if (and (< val 0) (< min 0))
	  (/ (* min val) rmin)
	  (if (= val 0)
	      0
	      #f))))

;; s_elev r g b d_elev r g b
;; s_elev r g b d_elev r g b
;; ...
(define* (cpt:format color-table #:optional (port #t))
  (if (pair? (cdr color-table))
      (let ((this-color-table (car color-table)))
	(format port "~a ~{~a~^ ~}" (car this-color-table) (cadr this-color-table))
	(if (pair? (cdr color-table))
	    (begin
	      (format port " ~a ~{~a~^ ~}~%" (caadr color-table) (cadr this-color-table))
	      (cpt:format (cdr color-table) port))))))

(define (string-split/whitespace str)
  (let ((str-list (string-split str #\sp)))
    (let loop ((strs str-list)
	       (outstr '()))
      (if (null? strs) (reverse outstr)
	  (if (string->number (car strs))
	      (loop (cdr strs) (append (list (string->number (car strs))) outstr))
	      (loop (cdr strs) outstr))))))

(define (cpt-split-list cpt-list)
  (let ((head (list-head cpt-list 4))
	(tail (list-tail cpt-list 4)))
    (list (list (car head) (cdr head))
	  (list (car tail) (cdr tail)))))

(define* (cpt:read #:optional (port (current-input-port)))
  (let ((next-char (peek-char port)))
    (cond
     ((eq? next-char #\#)
      (read-line port)
      (cpt:read port))
     ((eof-object? next-char)
      (read-char port))
     ;; These are usually the same all the time and unimportant for us atm.
     ((eq? next-char #\B)
      (read-line port)
      (cpt:read port))
     ((eq? next-char #\F)
      (read-line port)
      (cpt:read port))
     ((eq? next-char #\N)
      (read-line port)
      (cpt:read port))
     ;; read-in-color-table val r g b val r g b\n
     (else
      (let ((val (read-line port)))
	(string-split/whitespace val))))))

(define* (cpt->scm cpt-port #:optional (cpt-scm '()))
  (if (eof-object? (peek-char cpt-port)) (reverse cpt-scm)
      (let ((cpt-line (cpt:read cpt-port)))
	(if (pair? cpt-line)
	    ;;(cpt->scm cpt-port (cons (cadr (cpt-split-list cpt-line)) (cons (car (cpt-split-list cpt-line)) cpt-scm)))
	    (cpt->scm cpt-port (cons (cpt-split-list cpt-line) cpt-scm))
	    (cpt->scm cpt-port cpt-scm)))))

;;; Color-Tables

(define cpt:etopo1 '((-20 (10 0 121)) (-19.09090909 (26 0 137)) (-18.18181818 (38 0 152)) (-17.27272727 (27 3 166)) (-16.36363636 (16 6 180)) (-15.45454545 (5 9 193)) (-14.54545455 (0 14 203)) (-13.63636364 (0 22 210)) (-12.72727273 (0 30 216)) (-11.81818182 (0 39 223)) (-10.90909091 (12 68 231)) (-10 (26 102 240)) (-9.090909091 (19 117 244)) (-8.181818182 (14 133 249)) (-7.272727273 (21 158 252)) (-6.363636364 (30 178 255)) (-5.454545455 (43 186 255)) (-4.545454545 (55 193 255)) (-3.636363636 (65 200 255)) (-2.727272727 (79 210 255)) (-1.818181818 (94 223 255)) (-0.909090909 (138 227 255)) (-0.001 (138 227 255)) (0 (51 102 0)) (48.06865898 (51 204 102)) (96.13731797 (187 228 146)) (240.3432949 (255 220 185)) (480.6865898 (243 202 137)) (721.0298848 (230 184 88)) (961.3731797 (217 166 39)) (1201.716475 (168 154 31)) (1442.05977 (164 144 25)) (1682.403064 (162 134 19)) (1922.746359 (159 123 13)) (2163.089654 (156 113 7)) (2403.432949 (153 102 0)) (2643.776244 (162 89 89)) (2884.119539 (178 118 118)) (3124.462834 (183 147 147)) (3364.806129 (194 176 176)) (3605.149424 (204 204 204)) (3845.492719 (229 229 229))))

(define cpt:terra '((-7000 (105 0 182))	(-6708.33333333	(110 0 188)) (-6416.66666667 (115 0 193)) (-6125 (120 0 201)) (-5833.33333333 (109 14 204)) (-5541.66666667 (97 28 207)) (-4958.33333333 (84 42 210)) (-4666.66666667 (71 57 214)) (-4375 (58 72 217)) (-4083.33333333 (44 88 220)) (-3791.66666667 (30 104 223)) (-3208.33333333 (15 121 226)) (-2916.66666667 (0 138 230)) (-2625 (25 150 233)) (-2333.33333333 (50 162 236)) (-2041.66666667	(76 174 239)) (-1750 (103 187 242)) (-1166.66666667 (129 200 245)) (-875 (157 214 249)) (-583.333333331	(186 227 252)) (-291.666666669 (215 241 255)) (0 (105 152 133)) (50 (118 169 146)) (200 (131 181 155)) (600 (165 192 167)) (999.999999999 (211 201 179)) (2000 (212 184 164)) (3000 (212 192 181)) (4000 (214 209 206)) (5000 (222 221 220)) (6000 (238 237 236)) (7000 (247 246 245))))


;;; End

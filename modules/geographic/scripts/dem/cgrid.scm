;;; cgrid.scm - generate a vdatum-based conversion grid
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
;; usage: cgrid [ -chiorvzEOR [args] ] [file]
;;
;; c g r i d
;;
;;; Code:

(define-module (geographic scripts dem cgrid)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:use-module (xyz infos)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (geographic rasters)
  #:use-module (geographic spatial)
  #:use-module (geographic dem gmt)
  #:use-module (geographic dem gdal)
  #:use-module (geographic dem world-raster)
  #:export (dem-auto))

(define cgrid-version "0.0.2")

(define %summary "Generate a vdatum-based conversion grid.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (ivert (single-char #\i) (value #t))
    (overt (single-char #\o) (value #t))
    (ihorz (single-char #\r) (value #t))
    (ohorz (single-char #\z) (value #t))
    (inc (single-char #\E) (value #t))
    (constrain (single-char #\c) (value #f))
    (output (single-char #\O) (value #t))
    (region (single-char #\R) (value #t))))

(define (display-help)
  (format #t "\
~a
c g r i d

usage: cgrid [ -chiorvzEOR [args] ] [file]
" %summary))

(define (display-version)
  (format #t "\
dem-auto (guile-geographic) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" dem-auto-version))

(define (cgrid . args)
  (let* ((options (getopt-long (cons "cgrid" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (region (option-ref options 'region #f))
	  (output (option-ref options 'output #f))
	  (inc (option-ref options 'inc ".3333333s"))
	  (want-constrain (option-ref options 'constrain #f))
	  (ivert (option-ref options 'ivert "mllw:m:height"))
	  (overt (option-ref options 'overt "navd88:m:height"))
	  (ihorz (option-ref options 'ihorz "nad83"))
	  (ohorz (option-ref options 'ohorz "nad83")))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let* ((input (option-ref options '() #f))
	       (inc (if (pair? input)
			(format #f "~,7fs" (* 3600 (car (assoc-ref (gdalinfo (car input)) "pixel-size"))))
			inc))
	       (region-list (if (pair? input)
				(gdal->region (car input))
				(if region
				    (gmt-region->region region) #f)))
	       (proc-region (if region-list (region-expand region-list (gmt-inc->inc inc) #:cells 10) #f))
	       (out-name (format #f "~a_~a2~a" (if output output (if (pair? input) (car input) "cgrid")) (car (string-split ivert #\:)) (car (string-split overt #\:)))))
	  
	  (if region-list
	      (begin
		;; Create empty grid and transform to xy0
		(system (format #f "gdal_null.py -overwrite -region ~{~,5f ~} -cell_size 0.00083333 empty.tif~%" proc-region))
		(system (format #f "gdal_edit.py -a_nodata -9999 empty.tif~%"))
		(system (format #f "gdal_translate empty.tif empty.xyz -of XYZ~%"))
		;; run xy0 through vdatum to get the offset between input and output vertical datums
		(system (format #f "dem vdatum -F --ivert ~a --overt ~a --ihorz ~a --ohorz ~a empty.xyz~%" ivert overt ihorz ohorz))
		(let* ((empty-infos (xyz-port->infos (open-file "./result/empty.xyz" "r")))
		       (upper (if (> (assq-ref empty-infos 'zmax) 0) "-Lud" "-Lu0"))
		       (lower (if (< (assq-ref empty-infos 'zmin) 0) "-Lld" "-Ll0")))
		  ;; Get the minmax from result/empty.
		  (system (format #f "~a ./result/empty.xyz | ~a~% " 
				  (dem-make-gmt-cmd 
				   "blockmean" 
				   region-list
				   #:inc inc
				   #:verbose #t)
				  (dem-make-gmt-cmd 
				   "surface" 
				   region-list
				   #:inc (format #f "~a+e" inc )
				   #:out-name out-name
				   #:verbose #t 
				   #:extra (format #f "-r -T0 ~a ~a" 
						   (if want-constrain "-Lud" upper) 
						   (if want-constrain "-Lld" lower)))))
		  
		  (system (format #f "gmt grdconvert ~a.grd ~a.tif=gd:GTiff~%" out-name out-name)))))))))))

(define main cgrid)
;;---END

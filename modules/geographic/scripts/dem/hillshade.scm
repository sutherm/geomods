;;; hillshade.scm
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
;; Usage: hillshade [ -cdhv ] [ -A azimuth -T altitude -Z z-scale -E epsg -C cpt -O outfile ] file
;;
;; ( s h a d e  ' h i l l )
;;
;; Current cpt options: 'terra 'etopo1 (default)
;; Default values: -A 315 -T 45 -Z 1 -E 4326 -C etopo1 -O (basename file)_hs.tif
;;
;; Generate a hillshade image of the given grid file using gdal and imagemagick.
;;
;; Use -d to output the script commands instead of running them.
;; With -c only output the color-table for the given grid file (with -C).
;;
;;; Code:

(define-module (geographic scripts dem hillshade)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (geographic color-table)
  #:use-module (geographic dem gdal)
  #:export (hillshade))

;; cxhull version number
(define hillshade-version "0.0.2")

(define %summary "Generate a hillshade image of the given gdal file.")

(define command-synopsis
  '((help (single-char #\h) (value #f))
    (version (single-char #\v) (value #f))
    (display (single-char #\d) (value #f))
    (azimuth (single-char #\A) (value #t))
    (altitude (single-char #\T) (value #t))
    (zscale (single-char #\Z) (value #t))
    (epsg (single-char #\E) (value #t))
    (cpt (single-char #\C) (value #t))
    (colors (single-char #\c) (value #f))
    (outfile (single-char #\O) (value #t))))

;; Display help information
(define (display-help)
  (format #t "\
~a
( s h a d e  ' h i l l )

usage: hillshade [ -hvACEOTZ [args] ] [ file ]
" %summary))

;; Display Version information
(define (display-version)
  (format #t "\
hillshade (geo-8) version ~a

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" hillshade-version))

;; gm-li mainline
(define (hillshade . args)
  (let ((options (getopt-long (cons "hillshade" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (display-wanted (option-ref options 'display #f))
	  (colors-wanted (option-ref options 'colors #f))
	  (azimuth (option-ref options 'azimuth "315"))
	  (altitude (option-ref options 'altitude "45"))
	  (zscale (option-ref options 'zscale "1"))
	  (epsg (option-ref options 'epsg "4326"))
	  (cpt (option-ref options 'cpt "etopo"))
	  (outfile (option-ref options 'outfile #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let ((input (option-ref options '() #f)))
	  ;; TODO fails on bad gdalinfo output...check to make sure the info was returned correctly.
	  (if (pair? input)
	      (let* ((outfile (if outfile outfile (string-append (car (string-split (car input) #\.)) "_hs.tif")))
		     (gms-hillshade (string-append "dem hillshade -C " cpt " -c " (car input)))
		     (gdal-dem-hillshade (string-append "gdaldem hillshade -s 111120 -z " zscale " -az " azimuth " -alt " altitude " " (car input) " _hillshade.tif"))
		     (gdal-dem-color-relief (string-append "gdaldem color-relief " (car input) " colors.cpt _colors.tif"))
		     (im-composite (string-append "composite -compose multiply -depth 8 _colors.tif _hillshade.tif " outfile))
		     (im-mogrify (string-append "mogrify -modulate 115 -depth 8 " outfile))
		     (gdal-edit (string-append "gdal_edit.py -a_srs epsg:" epsg " " outfile))
		     (rm-cmd (string-append "rm _hillshade.tif _colors.tif")))
		(cond
		 (colors-wanted
		  (let ((gdmm (cdr (assoc "z-range" (gdalinfo (car input)))))
			(cpt-file (open-output-file "colors.cpt"))
			(cpt-palette (if (eqv? (string->symbol cpt) 'terra) cpt:terra cpt:etopo1)))
		    ;; make cpt
		    (cpt:format (cpt:scale cpt-palette (car gdmm) (cadr gdmm)) cpt-file)
		    (close-port cpt-file)))
		 (display-wanted
		  (display gms-hillshade)
		  (newline)
		  (display gdal-dem-hillshade)
		  (newline)
		  (display gdal-dem-color-relief)
		  (newline)
		  (display im-composite)
		  (newline)
		  (display im-mogrify)
		  (newline)
		  (display gdal-edit)
		  (newline)
		  (display rm-cmd)
		  (newline))
		 (else
		  (system gms-hillshade)
		  (system gdal-dem-hillshade)
		  (system gdal-dem-color-relief)
		  (system im-composite)
		  (system im-mogrify)
		  (system gdal-edit)
		  (system rm-cmd))))
	  (display-help))))))))

(define main hillshade)

;;; End

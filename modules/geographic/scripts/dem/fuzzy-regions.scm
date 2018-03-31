;;; fuzzy-regions.scm - generate a gmt formatted vector bounding box of the given region
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
;; Usage: fuzzy-regions [ -ghtvMNETX [ args ] ] -R xmin/xmax/ymin/ymax
;;
;;  ( c o n s  ' f u z z y  .  ' r e g i o n s )
;;
;; Generate a bounding box using the given region.
;; Will return a GMT formatted polygon of the given region to standard output.
;;
;; Use the -g switch to output GMT-style region strings instead of polygons, such as: xmin/xmax/ymin/ymax
;;
;; The increment, specified with the -E swich, should be in the style of GMT commands, e.g. "1s" for 1 arc second, etc.
;;
;; Using the -X switch, the region will be expanded by 'cells' cells.
;; e.g. -X 6.5 will extend the region by 6.5 cells. This option requires the -E option to be set.
;;
;; With the -T switch, extend the given region to fit the xy point.
;; Specify the point to extend the region after the -T switch as an xyz-string
;; e.g. fuzzy-regions -R1/2/3/4 -T0/10 -g
;;      ==> -R0/2/3/10
;;
;; With -M, merge the main region with a second region, specified with the M switch.
;; e.g. fuzzy-regions -R1/2/3/4 -M-2/1/1/3 -g
;;      ==> -R-2/1/1/4
;;
;; Optionally, using the `-t switch, fuzzy-regions will output a set of tiled 
;; fuzzy-regions which contain the input region. The tiled fuzzy-regions are based on a world-wide 
;; tile set in WGS84 of .25 degree tiles.
;;
;; e.g. fuzzy-regions -t -N Tiled -R-66.39/-65.862/18.287/18.673 will output 6 tiled fuzzy-regions:
;; ((-66.5 -66.25 18.25 18.5 455 434) (-66.5 -66.25 18.5 18.75 455 435) (-66.25 -66.0 18.25 18.5 456 434) 
;;  (-66.25 -66.0 18.5 18.75 456 435) (-66.0 -65.75 18.25 18.5 457 434) (-66.0 -65.75 18.5 18.75 457 435))
;;
;; Note: The tiled regions will be influenced by the -T and -M switches.
;;
;;; Code:

(define-module (geographic scripts dem fuzzy-regions)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (xyz xyz)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (geographic rasters)
  #:use-module (geographic spatial)
  #:use-module (geographic dem gmt)
  #:use-module (geographic dem world-raster)
  #:export (fuzzy-regions))

(define fuzzy-regions-version "0.0.7")

(define %summary "Fuzz the given region, or not.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (gmt (single-char #\g) (value #f))
    (merge (single-char #\M) (value #t))
    (expand (single-char #\X) (value #t))
    (extend (single-char #\T) (value #t))
    (inc (single-char #\E) (value #t))
    (name (single-char #\N) (value #t))
    (tile (single-char #\t) (value #f))
    (region (single-char #\R) (value #t))))

(define (display-help)
  (format #t "\
~a
( c o n s  ' f u z z y  .  ' r e g i o n s )

usage: fuzzy-regions [ -ghtvEMNRTX [args] ] [ region ]
" %summary))

(define (display-version)
  (format #t "\
fuzzy-regions (GeoMODs) version ~a
Copyright (c) 2011, 2012, 2013, 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" fuzzy-regions-version))

(define (fuzzy-regions . args)
  (let* ((options (getopt-long (cons "fuzzy-regions" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (gmt-wanted (option-ref options 'gmt #f))
	  (expand (option-ref options 'expand "0"))
	  (extend (option-ref options 'extend #f))
	  (merge (option-ref options 'merge #f))
	  (inc (option-ref options 'inc "1s"))
	  (name (option-ref options 'name "fuzzy-regions"))
	  (tile-wanted (option-ref options 'tile #f))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       ((not region) (display-help))
       (else
	(if (and (region? (gmt-region->region region))
		 (gmt-inc->inc inc))
	    ;; Expand and set the region-list
	    ;; Expands by zero (0) by default.
	    (let ((region-list (region-expand
				(gmt-region->region region) 
				(gmt-inc->inc inc) 
				#:cells (string->number expand))))
	      ;; Reset the region if the merge option was set.
	      (if (region? merge) 
		  (set! region-list (merge-regions region-list (gmt-region->region merge))))
	      (if (and extend (xy? (xyz-string->scm extend)))
		  (set! region-list (region-extend region-list (xyz-string->scm extend))))
	      (cond
	       ;; Tile the regions according to the wgs84 .25 degree grid.
	       (tile-wanted 
		(let ((these-fuzzy-regions 
		       (map (lambda (r) 
			      (region-expand r (gmt-inc->inc inc) #:cells (string->number expand))) 
			    (tiled-region region-list))))
		  (if gmt-wanted
		      ;; Output a gmt string instead of a polygon.
		      (format #t "~{-R~a~^~%~}~%" (map region->gmt-region these-fuzzy-regions))
		      ;; Output a polygon of the fuzzed region.
		      (polygons->ogr-gmt (regions->polygon these-fuzzy-regions '()) name))))
	       ;; Output a gmt string instead of a polygon.
	       (gmt-wanted (format #t "-R~a~%" (region->gmt-region region-list)))
	       ;; Output a polygon of the region.
	       (else (polygon->ogr-gmt (region->polygon region-list) name))))
	    (display-help)))))))

(define main fuzzy-regions)
;;---END

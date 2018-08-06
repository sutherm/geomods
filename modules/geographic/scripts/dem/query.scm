;; query.scm - convert xyz data to a point shapefile
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
;; Usage: gdal_query.py [-delimiter char] [-s_format "0,1,2"] 
;;                      [-d_format "xyzgdps"] [-d_nodata value] [-header]
;;                      [-return_all] [-verbose]
;;                      grdfile [srcfile]
;;
;; Input options:
;;   grdfile       Specifies which gdal-compatible grid file in which to 
;;                 query values.
;;   srcfile       Specifies which xy* file in which to query grdfile with,
;;                 if not given, will read from standard input.
;;   -delimiter    Specifies the input xy* delimiter
;;   -s_format     Specifies the input xy* format, a quoted triplet specifying
;;                 where the x,y and z values are located.
;;                 If there is no input z value, specify with a -, i.e. 
;;                 -s_format "0,1,-"
;;
;; Output options:
;;   -d_format     Specifies the output xy* format, a quoted string specifying 
;;                 which output values to return.
;;                 Options are [x][y][z][g][d][p][c][s]; where z is the input z value, g is the grid 
;;                 z value, d is the difference between the input z value and grid
;;                 z value, p is the product of the input z value and the grid z vallue,
;;                 c is the percetage difference and s is the scaled difference.
;;   -d_nodata     Specifies the output nodata value, will use the nodata value from
;;                 the input grid if not specified.
;;   -header       Indicates that the output will have a header included, default is False.
;;   -return_all   Will prompt the return of all input points, regardless of location
;;                 when compared to the input grid file, with grid values considered as nodata.
;;
;; General options:
;;   -verbose      Will increase verbosity.

;; Example:
;; To query grd.tif with an ascii xy file (values.xy) with comma delimited fields and 
;; no z-value and return the xy values from the input xy file and the z values
;; from grd.tif:
;; gdal_query.py -delimiter "," -s_format "0,1,-" -d_format "xyg" grd.tif values.xy > values.xyz

;; gdal_query v.1.8.4
;;
;;; Code:

(define-module (geographic scripts dem query)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:export (query))

(define gdal-query-version "0.0.1")

(define %summary "Query a gdal-compatible grid using gdal_query.py")

(define command-synopsis
  '((version (single-char #\v) (value #f))O))

(define %include-in-dem-list #f)

(define (display-help)
  (format #t "\
~a
 query

Usage: gdal_query.py [-delimiter char] [-s_format \"0,1,2\"] 
                     [-d_format \"xyzgdps\"] [-d_nodata value] [-header]
                     [-return_all] [-verbose]
                     grdfile [srcfile]
" %summary))

(define (display-version)
  (format #t "\
gdal-query (DEM) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" gdal-query-version))

(define (gdal-query . args)
  (system (string-append "gdal_query.py " (string-join args " "))))

(define main gdal-query)

;;; End

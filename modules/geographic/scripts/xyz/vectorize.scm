;; vectorize.scm - vectorize xyz data to a point shapefile
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
;; Usage: vectorize [-d/-delimiter char] [-s/-s_format 0,1,2] [-p/-d_epsg epsg_code]
;;                  [-verbose] [-overwrite] outfile [srcfile]
;;
;; vectorize (xyz2shp.py) v. 1.3.1
;;
;;; Code:

(define-module (geographic scripts xyz vectorize)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:export (vectorize))

(define vectorize-version "0.0.1")

(define %summary "Vectorize xyz data to a point shapefile.")

(define command-synopsis
  '((version (single-char #\v) (value #f))O))

(define %include-in-xyz-list #f)

(define (display-help)
  (format #t "\
~a
 xyz2shp

Usage: xyz2shp.py [-d/-delimiter char] [-s/-s_format 0,1,2] [-p/-d_epsg epsg_code]
                 [-verbose] [-overwrite] outfile [srcfile]
" %summary))

(define (display-version)
  (format #t "\
vectorize (XYZ) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" vectorize-version))

(define (vectorize . args)
  (system (string-append "xyz2shp.py " (string-join args " "))))

(define main vectorize)

;;; End

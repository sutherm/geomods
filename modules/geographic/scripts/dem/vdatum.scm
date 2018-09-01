;; vdatum.scm - convert xyz data to a point shapefile
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
;;; Code:

(define-module (geographic scripts dem vdatum)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (geographic util popen)
  #:export (vdatum))

(define dem-config 
  (cond 
   ((file-exists? (string-append (getenv "HOME") "/.dem.conf"))
    (string-append (getenv "HOME") "/.dem.conf"))
   ((file-exists? (string-append (getenv "HOME") "/.dem.scm"))
    (string-append (getenv "HOME") "/.dem.scm"))
   (else
    (string-append (getenv "HOME") "/.dem"))))

(if (file-exists? dem-config)
    (load dem-config))

(define vdatum-version "0.0.1")

(define %summary "Launch the VDatum application if installed")

(define command-synopsis
  '((version (single-char #\v) (value #f))O))

(define %include-in-dem-list #t)

(define (display-help)
  (format #t "\
~a
vdatum

usage: vdatum [ -hvV [args] ] [ args ]
" %summary))

(define (display-version)
  (format #t "\
vdatum (DEM) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" vdatum-version))

(define (find-vdatum)
  (format #t "Attempting to locate vdatum.~%")
  (let ((fdi (open-input-pipe "find / -type f 2> /dev/null | grep 'vdatum\\.jar'")))
    (read-line fdi)))

(define (vdatum . args)
  (let ((vdatum-path (if (not (defined? '%vdatum-path)) 
			 (find-vdatum) 
			 %vdatum-path)))
    (system (string-append "java -jar " vdatum-path (string-join args " ")))))
  
(define main vdatum)

;;; End

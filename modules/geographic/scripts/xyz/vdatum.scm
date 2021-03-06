;; vdatum.scm - run NOAA's vdatum from the command-line
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
;; usage: vdatum [ -fghiorvz [ args ] ] [ file ]
;;
;; vdatum
;;
;; Run vdatum from command-line.
;;
;; Use -g to run the VDatum GUI
;;
;; Example:
;;  vdatum input.xyz -f --ivert mhw --overt navd88
;;
;;; Code:

(define-module (geographic scripts xyz vdatum)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (geographic util popen)
  #:use-module (geographic dem vdatum)
  #:use-module (geographic dem gdal)
  #:use-module (geographic dem lastools)
  #:use-module (geographic dem mbio)
  #:export (vdatum))

(define dem-config 
  (cond 
   ((file-exists? (string-append (getenv "HOME") "/.xyz.conf"))
    (string-append (getenv "HOME") "/.xyz.conf"))
   ((file-exists? (string-append (getenv "HOME") "/.xyz.scm"))
    (string-append (getenv "HOME") "/.xyz.scm"))
   (else
    (string-append (getenv "HOME") "/.xyz"))))

(if (file-exists? dem-config)
    (load dem-config))

(define vdatum-version "0.0.3")

(define %summary "Run NOAA's VDatum from command-line.")

(define command-synopsis
  '((version (single-char #\v) (value #f))O))

(define %include-in-xyz-list #t)

(define (display-help)
  (format #t "\
~a
vdatum

usage: vdatum [ -fghiorvz [ args ] ] [ file ]
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
  (format (current-error-port) "xyz: Attempting to locate vdatum.~%")
  (let ((fdi (open-input-pipe "find / -type f 2> /dev/null | grep 'vdatum\\.jar'")))
    (read-line fdi)))

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (gui (single-char #\g) (value #f))
    (file (single-char #\f) (value #f))
    (ivert (single-char #\i) (value #t))
    (overt (single-char #\o) (value #t))
    (ihorz (single-char #\r) (value #t))
    (ohorz (single-char #\z) (value #t))))

(define (vdatum . args)
  (let* ((options (getopt-long (cons "vdatum" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (gui (option-ref options 'gui #f))
	  (want-file (option-ref options 'file #f))
	  (ivert (option-ref options 'ivert "mllw:m:height"))
	  (overt (option-ref options 'overt "navd88:m:height"))
	  (ihorz (option-ref options 'ihorz "nad83"))
	  (ohorz (option-ref options 'ohorz "nad83")))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(let ((input (option-ref options '() #f)))
	  (let* ((infile (if (not (pair? input)) 
			     (current-input-port) 
			     (open-file (car input) "r"))))

		 (let ((vdatum-path (if (not (defined? '%vdatum-path)) 
					(find-vdatum) 
					%vdatum-path)))
		   (if gui
		       (system (string-append "java -jar " vdatum-path))
		       (if (pair? input)
			   (if want-file
			       (vdatum-file (car input)  #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt)
			       (port->vdatum infile #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt))
			   (port->vdatum infile #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt)))))))))))
			   ;;  (lidar
			   ;;   (port->vdatum (las->port (car input)) #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt))
			   ;;  (gdal
			   ;;   (port->vdatum (gdal->port (car input)) #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt))
			   ;;  (mbio
			   ;;   (port->vdatum (mbio->port (car input)) #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt))
			   ;;  (else
			   ;;   (port->vdatum infile #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt)))
		       
		       ;; (port->vdatum infile #:vdatum vdatum-path #:ihorz ihorz #:ohorz ohorz #:ivert ivert #:overt overt)))))))))))

	    
(define main vdatum)

;;; End

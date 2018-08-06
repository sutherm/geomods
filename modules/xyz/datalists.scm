;;-*-scheme-*-
;;; datalists.scm
;;
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
;; Datalists
;; MB-System-style datalists
;; data-path data-format data-wegiht
;; data-format follows MB-System, i.e. -1 is a datalist, 168 is xyz, etc.
;;
;;; Code:

(define-module (xyz datalists)
  #:use-module (ice-9 rdelim)
  #:use-module (xyz xyz)
  #:use-module (xyz infos)
  #:export
  (%data-list-hook 
   data-list->scm 
   data-list
   glob-xyzs
   glob-datalists
   glob->infos
   glob->datalist
   format-datalist))

;; Output the files in a datalist to a list.
(define* (data-list->scm port #:optional (lines '()))
  "Return a scheme list of the contents of the given data-list."
  (if (eof-object? (peek-char port)) lines
      (let ((this-line (read-line port)))
	(if (equal? #\# (car (string->list this-line)))
	    (data-list->scm port lines)
	    (data-list->scm port (cons (string-split this-line #\sp) lines))))))

;; The datalist hook. This runs on every xyz file in a datalist. (datalist value 168).
(define %data-list-hook (make-hook 1))

;; The default data-list-hook (xyz-file). Will send the file to xyz->port.
(add-hook! %data-list-hook 
	   (lambda (xyz) 
	     (if (file-exists? xyz) 
		 (xyz->port (open-file xyz "r")))))

;; Recurse a datalist. Run the various hooks on the various data-types.
(define* (data-list dl)
  "Recurse through datalists and run the data-list-hook on the xyz file from 
each datafile in the given datalist."
  (let ((this-data-list (data-list->scm dl))
	(this-directory (dirname (port-filename dl))))
    (map (lambda (l)
	   (let ((infile (string-append this-directory "/" (car l))))
	     (case (string->number (cadr l))
	       ((-1)
		(if (file-exists? infile) 
		    (data-list (open-file infile "r"))))
	       ((168)
		(if (not (hook-empty? %data-list-hook))
		    (run-hook %data-list-hook infile))))))
	 this-data-list)))

;; glob the xyz file names in the directory dir.
(define* (glob-xyzs #:optional (dir (opendir "./")) (xyzs '()))
  "Glob all xyz files (*.xyz) in the given directory and return a list of the file-names."
  (let ((entry (readdir dir)))
    (if (eof-object? entry) xyzs
	(if (not (= (string-length entry) (string-length (basename entry ".xyz"))))
	    (glob-xyzs dir (cons entry xyzs))
	    (glob-xyzs dir xyzs)))))

;; glob the datalists in the directory dir.
(define* (glob-datalists #:optional (dir (opendir "./")) (dls '()))
  "Glob all datalist files (*.datalist) in the given directory and return a list of the file-names."
  (let ((entry (readdir dir)))
    (if (eof-object? entry) dls
	(if (not (= (string-length entry) (string-length (basename entry ".datalist"))))
	    (glob-datalists dir (cons entry dls))
	    (glob-datalists dir dls)))))

(define* (glob-datalists #:optional (dir (opendir "./")) (dls '()))
  "Glob all datalist files (*.datalist) in the given directory and return a list of the file-names."
  (let ((entry (readdir dir)))
    (if (eof-object? entry) dls
	(if (not (= (string-length entry) (string-length (basename entry ".datalist"))))
	    (glob-datalists dir (cons entry dls))
	    (glob-datalists dir dls)))))

(define (glob->infos)
  "Glob the xyz files in the current-directory and snarf each one, generating an infos blob."
  (let ((xyzs (glob-xyzs)))
    (let loop ((xyz-files xyzs))
      (format (current-error-port) "snarfing ~a~%" (car xyz-files))
      (if (pair? xyz-files)
	  (let* ((inxyz (open-file (car xyz-files) "r"))
		 (infos (xyz-port->infos inxyz))
		 (outinfo (if infos (open-file (string-append (assq-ref infos 'name) ".scm") "w")
			      (current-error-port))))
	    (write infos outinfo)
	    ;;(close-port outinfo)
	    (if (not (null? (cdr xyz-files)))
		(loop (cdr xyz-files))))))))

;; format datalist xyzfs to port
(define (format-datalist port xyzfs)
  (if (not (null? xyzfs))
      (let ((f (car xyzfs)))
	(format port "~a 168\n" f)
	(format-datalist port (cdr xyzfs)))))

;; glob the xyz files in the current directory and output to name.datalist.
(define* (glob->datalist #:optional (name #f))
  "Glob the xyz data files in the current-directory and output to 
std-out the file-names in datalist-format."
  (let ((xyzs (glob-xyzs))
	(out-list (if name (open-file (string-append name ".datalist") "w") #t)))
    (format-datalist out-list xyzs)))
  
;;; End

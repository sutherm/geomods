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
  #:use-module (geographic dem gdal)
  #:use-module (geographic dem lastools)
  #:use-module (geographic dem mbio)
  #:export
  (%data-list-hook 
   %data-list-gdal-hook 
   %data-list-mbio-hook 
   %data-list-las-hook 
   %data-list-dl-hook 
   data-list->scm 
   data-list
   find-data-entry
   glob-datas
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

;; This runs on every datalist file in a datalist. (datalist value -1).
(define %data-list-dl-hook (make-hook 2))
;; The datalist hook. This runs on every xyz file in a datalist. (datalist value 168).
(define %data-list-hook (make-hook 2))
;; The datalist gdal hook. This runs on every gdal file in a datalist. (datalist value 200).
(define %data-list-gdal-hook (make-hook 1))
;; The datalist gdal hook. This runs on every lastools file in a datalist. (datalist value 300).
(define %data-list-las-hook (make-hook 1))
;; The datalist gdal hook. This runs on every mb file in a datalist. (datalist value 11).
(define %data-list-mbio-hook (make-hook 1))

;; The default data-list-dl-hook (datalist-file). Will open the file and process it further.
(add-hook! %data-list-dl-hook 
	   (lambda (dl wt) 
	     (if (file-exists? dl) 
		    (data-list (open-file dl "r") #:weight wt))))

;; The default data-list-hook (xyz-file). Will send the file to xyz->port.
(add-hook! %data-list-hook 
	   (lambda (xyz wt) 
	     (if (file-exists? xyz) 
		 (xyz->port (open-file xyz "r") #:weight wt))))

;; The default data-list-gdal-hook (gdal-file). Will send the file to gdal->port.
(add-hook! %data-list-gdal-hook 
	   (lambda (gdal) 
	     (if (file-exists? gdal) 
		 (gdal2xyz gdal))))

;; The default data-list-las-hook (las-file). Will send the file to las->port.
(add-hook! %data-list-las-hook 
	   (lambda (las) 
	     (if (file-exists? las) 
		 (las->xyz las))))

;; The default data-list-las-hook (las-file). Will send the file to las->port.
(add-hook! %data-list-mbio-hook 
	   (lambda (mbio) 
	     (if (file-exists? mbio) 
		 (mbio->xyz mbio))))

;; Recurse a datalist. Run the various hooks on the various data-types.
(define* (data-list dl #:key (weight 1))
  "Recurse through datalists and run the data-list-hook on the xyz file from 
each datafile in the given datalist."
  (let ((this-data-list (reverse (data-list->scm dl)))
	(this-directory (dirname (port-filename dl))))
    (map (lambda (l)
	   (let ((infile (string-append this-directory "/" (car l))))
	     (case (string->number (cadr l))
	       ((-1)
		(if (not (hook-empty? %data-list-dl-hook))
		    (let ((wt (if (> (length l) 2) (caddr l) 10)))
		      (run-hook %data-list-dl-hook infile wt))))
	       ((11) ;; mbio
		(if (not (hook-empty? %data-list-mbio-hook))
		    (run-hook %data-list-mbio-hook infile)))
	       ((168) ;; XYZ
		(if (not (hook-empty? %data-list-hook))
		    (run-hook %data-list-hook infile weight)))
	       ((200) ;; gdal
		(if (not (hook-empty? %data-list-gdal-hook))
		    (run-hook %data-list-gdal-hook infile)))
	       ((300) ;; las-tools
		(if (not (hook-empty? %data-list-las-hook))
		    (run-hook %data-list-las-hook infile))))))
	 this-data-list)))

;; glob the xyz file names in the directory dir.
(define* (glob-xyzs #:optional (dir (opendir "./")) (xyzs '()))
  "Glob all xyz files (*.xyz) in the given directory and return a list of the file-names."
  (let ((entry (readdir dir)))
    (if (eof-object? entry) xyzs
	(if (not (= (string-length entry) (string-length (basename entry ".xyz"))))
	    (glob-xyzs dir (cons entry xyzs))
	    (glob-xyzs dir xyzs)))))

(define (find-data-entry entry exts)
  (if (null? exts) #f
      (if (= (string-length entry) (string-length (basename entry (car exts))))
	  (find-data-entry entry (cdr exts)) entry)))

;; glob the gdal file names in the directory dir.
(define* (glob-datas #:optional (exts xyz-exts) (dir (opendir "./")) (datas '()))
  (let ((entry (readdir dir)))
    (if (eof-object? entry) datas
	(if (find-data-entry entry exts)
	    (glob-datas exts dir (cons entry datas))
	    (glob-datas exts dir datas)))))


;; glob the datalists in the directory dir.
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
(define (format-datalist port xyzfs datatype weight)
  (if (not (null? xyzfs))
      (let ((f (car xyzfs)))
	(format port "~a ~a ~a\n" f datatype weight)
	(format-datalist port (cdr xyzfs) datatype weight))))

;; glob the xyz files in the current directory and output to name.datalist.
;; glob the gdal files in the current directory and output to name.datalist.
(define* (glob->datalist #:optional (name #f) #:key (weight 1))
  "Glob the xyz data files in the current-directory and output to 
std-out the file-names in datalist-format."
  (let ((xyzs (glob-datas xyz-exts))
	(gdals (glob-datas gdal-exts))
	(lass (glob-datas las-exts))
	(out-list (if name (open-file (string-append name ".datalist") "w") #t)))
    (format-datalist out-list xyzs 168 weight)
    (format-datalist out-list gdals 200 weight)
    (format-datalist out-list lass 300 weight)))
  
;;; End

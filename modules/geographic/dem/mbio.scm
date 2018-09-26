;;; mbio.scm
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
;;; Code:

(define-module (geographic dem mbio)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (geographic util popen)
  #:use-module (xyz xyz)
  #:export
  (mbinfo
   mbinfo->scm
   mb->region
   mbinfos->region
   mb->xyz
   mb->port))

(define* (mb->xyz filename 
		   #:optional (oport (current-output-port))
		   #:key (test-fun #f))
  (let ((mbti (mbinfo filename)))
    (let ((mbtx (open-input-pipe (string-append "mblist -MX20 -OXYZ -I" filename))))
      (xyz->port mbtx oport #:test-fun test-fun))))

(define* (mb->port filename #:optional (oport (current-output-port)))
  (let ((mbti (mbinfo filename)))
    (open-input-pipe (string-append "mblist -MX20 -OXYZ -I" filename))))

;; Run mbinfo and parse the output to an association-list
;; with keys: "filename" "driver" "origin" "size" "pixel-size" "z-range"
(define (mbinfo filename)
  (let ((mbti (open-input-pipe (string-append "mbinfo -I" filename))))
    (acons "filename" filename (mbinfo->scm mbti #t))))

(define (mb->region filename)
  (let* ((mb-infos (mbinfo filename))
	 (lons (assoc-ref mb-infos "lons"))
	 (lats (assoc-ref mb-infos "lats"))
	 (deps (assoc-ref mb-infos "deps")))
    (list (car lons) (cadr lons) (car lats) (cadr lats))))

(define (mbinfos->region mb-infos)
  (let ((lons (assoc-ref mb-infos "lons"))
	(lats (assoc-ref mb-infos "lats"))
	(deps (assoc-ref mb-infos "deps")))
    (list (car lons) (cadr lons) (car lats) (cadr lats))))
;;; End

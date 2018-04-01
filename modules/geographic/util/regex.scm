;;-*-scheme-*-
;;; regex.scm
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
;; Some regex helper functions.
;;
;;; Code:

(define-module (geographic util regex)
  #:use-module (ice-9 regex)
  #:export
  (match:times match:replace))


;; Returns the number of times a string is matched in a string.
;; (match:times (string-match "t" "test")) ==> 2
;; the default match:count is a bit different:
;; (match:count (string-match "t" "test")) ==> 1
(define* (match:times match #:optional (n 0))
  "Return the number of times a string match occurs."
  (if (not (regexp-match? match)) n
      (match:times 
       (string-match (match:substring match) 
		     (match:suffix match)) (1+ n))))

;; Replace the string matched in string-match and replace it with str.
;; (match:replace (string-match "t" "test") "b") ==> "besb"
;; (define s (string-match "[0-9][0-9][0-9][0-9]" "blah2002foo"))
;; (match:replace s "BAR") ==> "blahBARfoo"
(define* (match:replace match str #:optional (outstr ""))
  "Replace the string matched in string-match with str."
  (if (not (regexp-match? match)) (if (= 0 (string-length outstr)) #f outstr)
      (let ((prefix-str (string-append (match:prefix match) str))
	    (suffix-match (string-match (match:substring match) (match:suffix match))))
	(if (regexp-match? suffix-match)
	    (match:replace suffix-match str (string-append outstr prefix-str))
	    (string-append outstr prefix-str (match:suffix match))))))

;;; End

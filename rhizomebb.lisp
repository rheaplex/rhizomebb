;; rhizomebb.lisp - Functions to generate bbcode aestheticized text for Rhizome.
;; Copyright (c) 2008 Rob Myers <rob@robmyers.org>
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;
;; map into vector is used to make functions composable/chainable.
;; e.g. (random-colour (random-size "hello" 6 18) 100 200)
;; Todo: random caps, misused unicode replace, image replace from flickr ;-)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-range (min max)
  "Generate a number between min & max, assumes max is greater than min."
  (+ min
     (random (- max min))))

(defun random-sign (val)
  "Return the value with its sign reversed 50% of the time."
  (if (< 0.5 (random 1.0)) 
      val
      (* val -1.0)))

(defun next-value (current min-val max-val min-delta max-delta)
  "Generate a value between min and max varying from current by the delta."
  (let* ((delta (random-sign (random-range min-delta max-delta)))
	 (next (+ current delta)))
    ;; Constrain to range min-val..max-val
    (max min-val (min next max-val))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBCode wrapper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-size (text min-val max-val)
  "Print the string with each character wrapped in a bbcode size block."
  (map 'vector
       #'(lambda (char) 
	   (format nil "[size=~d]~a[/size]" 
		   (floor (random-range min-val max-val))
		   char))
       text))

(defun random-colour (text min-val max-val) 
  "Print the string with each character wrapped in a bbcode color block."
  (map 'vector
       #'(lambda (char) 
	   (format nil "[color=#~2,'0x~2,'0x~2,'0x]~a[/color]" 
		   (random-range min-val max-val) 
		   (random-range min-val max-val) 
		   (random-range min-val max-val) 
		   char))
       text))

(defun random-walk-size (text min-val max-val min-delta max-delta) 
  "Print the string with each character wrapped in a bbcode size block."
  (let ((siz (random-range min-val max-val))) 
    (map 'vector
	 #'(lambda (char) 
	     (setf siz (next-value siz min-val max-val min-delta max-delta))
	     (format nil "[size=~d]~a[/size]" (floor siz) char))
	 text)))

(defun random-walk-colour (text min-val max-val min-delta max-delta) 
  "Print the string with each character wrapped in a bbcode color block."
  (let ((r (random-range min-val max-val))
	(g (random-range min-val max-val))
	(b (random-range min-val max-val))) 
    (map 'vector
	 #'(lambda (char) 
	     (setf r (next-value r min-val max-val min-delta max-delta))
	     (setf g (next-value g min-val max-val min-delta max-delta))
	     (setf b (next-value b min-val max-val min-delta max-delta))
	     (format nil "[color=#~2,'0x~2,'0x~2,'0x]~a[/color]" 
		     (floor r) (floor g) (floor b) char))
	 text)))

(defun random-walk-brightness (text min-val max-val min-delta max-delta) 
  "Print the string with each character wrapped in a bbcode color block."
  ;; Needs improving
  (let ((r (random-range min-val max-val))
	(g (random-range min-val max-val))
	(b (random-range min-val max-val))
	(level (random-range min-val max-val))) 
    (map 'vector
	 #'(lambda (char) 
	     (setf level (next-value level min-val max-val min-delta max-delta))
	     (format nil "[color=#~2,'0x~2,'0x~2,'0x]~a[/color]" 
		     (floor (* r level)) 
		     (floor (* g level)) 
		     (floor (* b level))
		     char))
	 text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo ()
  "Print the string with each character wrapped in a bbcode size & color block."
  (format t "Enter text to encode and press return:~%")
  (map nil
       #'(lambda (char) (princ char))
       (random-colour (random-size (read-line) 8 18) 0 200))
  (format t "~%"))

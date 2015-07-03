;;; generator.el --- Port of test.check generators -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Matthew O. Smith

;; Author: Matthew O. Smith <Matthew.Smith@imail.org>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(defclass tcel-generator ()
  ((gen :initarg :gen)))

(defun tcel-generator? (x)
  "Test is `x` is a generator. Generators should be treated as opaque values."
  (tcel-generator-p x))

(defun tcel-make-gen  (generator-fn)
  (tcel-generator "make-gen" :gen generator-fn))

(defun tcel-call-gen (g rnd size)
  (let ((generator-fn (oref g gen)))
    (funcall generator-fn rnd size)))

(defun tcel-gen-pure  (value)
  (tcel-make-gen
    (lambda (rnd size) value)))

(defun gen-fmap  (k g)
  (let ((h (oref g gen))) 
    (tcel-make-gen
     (lambda (rnd size)
       (funcall k (funcall h rnd size))))))


(defun tcel-gen-bind   [g k]
  (let ((h (oref g gen))) 
    (tcel-make-gen
     (lambda (rnd size)
       (let* ((inner (funcall h rnd size))
	      (result-gen (funcall k inner))
	      (result (oref result-gen gen)))
	 (funcall result rnd size))))))

(defun gen-seq->seq-gen (gens)
  "Takes a sequence of generators and returns a generator of
sequences (er, lists)."
  (tcel-make-gen
   (lambda (rnd size)
     (mapcar (lambda (a) (tcel-call-gen a rnd size) gens)))))

(provide 'generator)
;;; generator.el ends here

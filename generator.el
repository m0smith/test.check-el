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

(defun tcel-gen-fmap  (k g)
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


;; Exported generator functions
;; ---------------------------------------------------------------------------

(defun tcel-fmap  (f gen)
  (assert (tcel-generator? gen) t "Second arg to fmap must be a generator")
  (tcel-gen-fmap (lambda (a) (qc-rt-fmap f a)) gen))

(defun tcel-return (value)
  "Create a generator that always returns `value`,
  and never shrinks. You can think of this as
  the `constantly` of generators."
  (tcel-gen-pure (qc-rt-pure value)))

(defun tcel-bind-helper  (k)
  (lambda (rose)
    (tcel-gen-fmap qc-rt-join
		   (tcel-make-gen
		    (lambda (rnd size)
		      (qc-rt-fmap (lambda (a) (tcel-call-gen a rnd size)
				    (qc-rt-fmap k rose))))))))

(defun tcel-bind (generator k)
  "Create a new generator that passes the result of `gen` into function
  `k`. `k` should return a new generator. This allows you to create new
  generators that depend on the value of other generators. For example,
  to create a generator which first generates a vector of integers, and
  then chooses a random element from that vector:
      (gen/bind (gen/such-that not-empty (gen/vector gen/int))
                ;; this function takes a realized vector,
                ;; and then returns a new generator which
                ;; chooses a random element from it
                gen/elements)
  "

  (assert (generator? generator) t "First arg to bind must be a generator") ;
  (tcel-gen-bind generator (tcel-bind-helper k)))

(defun tcel-random (&optional seed)
  (if seed 
      (random)
    (random seed)))

(defun tcel-make-size-range-seq  (max-size)
  (cycle (cljs-el-range 0 max-size)))



(provide 'generator)
;;; generator.el ends here

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

(defclass tcel-random-class ()
  ((seed :initarg :seed
	:initform t)
   (stte :initarg :state :initform nil)))

(defun tcel-random-instance (&optional seed)
  "SEED needs to be an integer"
  (if seed
      (tcel-random-class (format "random %d" seed) :seed seed :state ( cl-make-random-state seed))
    (tcel-random-class "random nil" :state (cl-make-random-state))))
  
(defun tcel-random (&optional seed)
  "SEED must be an integer"
  (tcel-random-instance seed))

(defun tcel-random-float (&optional rnd)
  "Return a number between 0.0 and 1.0 exclusive. RND is an instance of `tcel-random-class'"
  (if rnd
      (cl-random 1.0 (oref rnd stte))
    (cl-random 1.0)))


(defun tcel-make-size-range-seq  (max-size)
  (cljs-el-cycle (cljs-el-range 0 max-size)))

(defun tcel-sample-seq (generator &optional max-size)
  "Return a sequence of realized values from `generator`."
  (let* ((max-size (or max-size 100))
	 (r (tcel-random))
	 (size-seq (tcel-make-size-range-seq max-size)))
    (cljs-el-map (lambda (a) (qc-rt-root (tcel-call-gen generator r a))) size-seq)))

(defun tcel-sample (generator &optional num-samples)
  "Return a sequence of `num-samples` (default 10)
  realized values from `generator`."
  (assert (tcel-generator? generator) "First arg to sample must be a generator")
  (let ((num-samples (or num-samples 10)))
    (cljs-el-take num-samples (tcel-sample-seq generator))))

;; Internal Helpers
;; ---------------------------------------------------------------------------

(defun tcel-halfs (n)
  (cljs-el-take-while (lambda (a) (not(equal 0 a))) (cljs-el-iterate (lambda (a) (/ a  2)) n)))

(defun tcel-shrink-int  (n)
  (cljs-el-map (lambda (a) (- n a)) (tcel-halfs n)))

(defun tcel-int-rose-tree  (value)
  (vconcat (vector value) (cljs-el-vec (cljs-el-map 'tcel-int-rose-tree (tcel-shrink-int value)))))

(defun tcel-rand-range (rnd lower upper)
  (assert (<= lower upper) t "Lower must be <= upper")
  (let ((factor (tcel-random-float rnd)))
    (floor (+ lower (- (* factor (+ 1 upper))
		       (* factor lower))))))


(defun tcel-sized (sized-gen)
  "Create a generator that depends on the size parameter.
  `SIZED-GEN` is a function that takes an integer and returns
  a generator."
  (tcel-make-gen
    (lambda (rnd size)
      (let ((sized-gen (funcall sized-gen size)))
        (tcel-call-gen sized-gen rnd size)))))

;; Combinators and helpers
;; ---------------------------------------------------------------------------

(defun tcel-resize   (n generator)
  "Create a new generator with `size` always bound to `n`."
  (assert (tcel-generator? generator) y "Second arg to resize must be a generator")
  (let ((gen (oref generator gen)))
    (tcel-make-gen
     (lambda (rnd _size)
       (funcall gen rnd n)))))

(defun tcel-choose   (lower upper)
  "Create a generator that returns numbers in the range
  `min-range` to `max-range`, inclusive."
  (tcel-make-gen
    (lambda (rnd _size)
      (let ((value (tcel-rand-range rnd lower upper)))
        (qc-rt-filter
          (lambda (a) (and (>= a lower) (<= a upper)))
          (tcel-int-rose-tree value))))))




(provide 'generator)
;;; generator.el ends here

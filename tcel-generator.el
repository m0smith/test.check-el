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

(require 'tcel-rosetree)
(require 'cljs-el)

(defclass tcel-generator-generator ()
  ((gen :initarg :gen)))

(defun tcel-generator-generator? (x)
  "Test is `x` is a generator. Generators should be treated as opaque values."
  (tcel-generator-generator-p x))

(defun tcel-generator-make-gen  (name generator-fn)
  (tcel-generator-generator name :gen generator-fn))

(defun tcel-generator-call-gen (g rnd size)
  (let ((generator-fn (oref g gen)))
    (funcall generator-fn rnd size)))

(defun tcel-generator-gen-pure  (value)
  (tcel-generator-make-gen "gen-pure"
   (lambda (rnd size) value)))

(defun tcel-generator-gen-fmap (k g)
  (let ((h (oref g gen))) 
    (tcel-generator-make-gen "gen-fmap"
     (lambda (rnd size)
       (funcall k (funcall h rnd size))))))


(defun tcel-generator-gen-bind   (g k)
  (let ((h (oref g gen))) 
    (tcel-generator-make-gen "gen-bind"
     (lambda (rnd size)
       (let* ((inner (funcall h rnd size))
	      (result-gen (funcall k inner))
	      (result (oref result-gen gen)))
	 (funcall result rnd size))))))

(defun tcel-generator-gen-seq->seq-gen (gens)
  "Takes a sequence of generators and returns a generator of
sequences (er, lists)."
  (tcel-generator-make-gen "gen-seq->seq-gen"
   (lambda (rnd size)
     (map 'vector (lambda (a) (tcel-generator-call-gen a rnd size)) gens))))


;; Exported generator functions
;; ---------------------------------------------------------------------------

(defun tcel-generator-fmap  (f gen)
  (assert (tcel-generator-generator? gen) t "Second arg to fmap must be a generator")
  (tcel-generator-gen-fmap (lambda (a) (qc-rt-fmap f a)) gen))

(defun tcel-generator-return (value)
  "Create a generator that always returns `value`,
  and never shrinks. You can think of this as
  the `constantly` of generators."
  (tcel-generator-gen-pure (qc-rt-pure value)))

(defun tcel-generator-bind-helper  (k)
  (lambda (rose)
    (tcel-generator-gen-fmap 'qc-rt-join
			     (tcel-generator-make-gen "bind-helper"
			      (lambda (rnd size)
				(qc-rt-fmap (lambda (a) (tcel-generator-call-gen a rnd size))
					    (qc-rt-fmap k rose)))))))

(defun tcel-generator-bind (generator k)
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

  (assert (tcel-generator-generator? generator) t "First arg to bind must be a generator") ;
  (tcel-generator-gen-bind generator (tcel-generator-bind-helper k)))

(defclass tcel-generator-random-class ()
  ((seed :initarg :seed
	 :initform t)
   (stte :initarg :state :initform nil)))

(defun tcel-generator-random-instance (&optional seed)
  "SEED needs to be an integer"
  (if seed
      (tcel-generator-random-class (format "random %d" seed) :seed seed :state ( cl-make-random-state seed))
    (tcel-generator-random-class "random nil" :state (cl-make-random-state))))

(defun tcel-generator-random (&optional seed)
  "SEED must be an integer"
  (tcel-generator-random-instance seed))

(defun tcel-generator-random-float (&optional rnd)
  "Return a number between 0.0 and 1.0 exclusive. RND is an instance of `tcel-generator-random-class'"
  (if rnd
      (cl-random 1.0 (oref rnd stte))
    (cl-random 1.0)))


(defun tcel-generator-make-size-range-seq  (max-size)
  (cljs-el-cycle (cljs-el-range 0 max-size)))

(defun tcel-generator-sample-seq* (generator rnd a)
  (qc-rt-root (tcel-generator-call-gen generator rnd a)))

(defun tcel-generator-sample-seq (generator &optional max-size seed)
  "Return a sequence of realized values from `generator`."
  (let* ((max-size (or max-size 100))
	 (r (if seed (tcel-generator-random seed) (tcel-generator-random)))
	 (size-seq (tcel-generator-make-size-range-seq max-size)))
    (cljs-el-map (lambda (a) (tcel-generator-sample-seq* generator r a)) size-seq)))

(defun tcel-generator-sample (generator &optional num-samples seed)
  "Return a sequence of `num-samples` (default 10)
  realized values from `generator`."
  (assert (tcel-generator-generator? generator) "First arg to sample must be a generator")
  (let ((num-samples (or num-samples 10)))
    (cljs-el-list (cljs-el-take num-samples (tcel-generator-sample-seq generator nil seed)))))

;; Internal Helpers
;; ---------------------------------------------------------------------------
(defvar tcel-generator-int-rose-tree-cache (make-hash-table :test 'equal)
  "Cache for int-rose-tree to speed things up")

(defvar tcel-generator-int-rose-tree-cache-hits 0)

(defvar tcel-generator-int-rose-tree-cache-misses 0)

(defun tcel-generator-int-rose-tree-check-cache (key)
  (if (= (car key) 0)
      nil
    (let ((rtnval (gethash key tcel-generator-int-rose-tree-cache nil)))
      (if rtnval
	  (setq tcel-generator-int-rose-tree-cache-hits (1+ tcel-generator-int-rose-tree-cache-hits))
	(setq tcel-generator-int-rose-tree-cache-misses (1+ tcel-generator-int-rose-tree-cache-misses)))
      rtnval)))
      

(defun  tcel-generator-halfs (n)
  "Return a non-lazy sequence"
      (let ((i n)
	    (accum '()))
	(while (not (= 0 i))
	  (setq accum (cons i accum))
	  (setq i (/ i 2)))
	(let ((rtnval (nreverse accum)))
	  rtnval)))
      
      
  ;;(cljs-el-list (cljs-el-take-while (lambda (a) (not (equal 0 a))) (cljs-el-iterate (lambda (a) (/ a  2)) n))))

(defun tcel-generator-shrink-int  (n &optional pred)
  "Return a non-lazy list"
  (if (and pred (not (= 0 n)))
      (-filter pred (mapcar (lambda (a) (- n a)) (tcel-generator-halfs n)))
    (mapcar (lambda (a) (- n a)) (tcel-generator-halfs n))))

(defun tcel-generator-int-rose-tree  (value &optional pred lower upper)
  (let ((key (list value lower upper)))
    (or  (tcel-generator-int-rose-tree-check-cache key)
	 (let ((children (cljs-el-map (lambda (v) (tcel-generator-int-rose-tree v pred lower upper))
				      (tcel-generator-shrink-int value pred))))
	   (let ((rtnval (qc-rt-make-rose value  children)))
	     (puthash key rtnval tcel-generator-int-rose-tree-cache)
	     rtnval)))))

(defun tcel-generator-rand-range (rnd lower upper)
  (assert (<= lower upper) t "Lower must be <= upper")
  (let ((factor (tcel-generator-random-float rnd)))
    (floor (+ lower (- (* factor (+ 1 upper))
		       (* factor lower))))))


(defun tcel-generator-sized (sized-gen)
  "Create a generator that depends on the size parameter.
  `SIZED-GEN` is a function that takes an integer and returns
  a generator."
  (tcel-generator-make-gen "sized"
   (lambda (rnd size)
     (let ((sized-gen (funcall sized-gen size)))
       (tcel-generator-call-gen sized-gen rnd size)))))

;; Combinators and helpers
;; ---------------------------------------------------------------------------

(defun tcel-generator-resize   (n generator)
  "Create a new generator with `size` always bound to `n`."
  (assert (tcel-generator-generator? generator) y "Second arg to resize must be a generator")
  (let ((gen (oref generator gen)))
    (tcel-generator-make-gen "resize"
     (lambda (rnd _size)
       (funcall gen rnd n)))))


(defun tcel-generator-choose   (lower upper)
  "Create a generator that returns numbers in the range
  `min-range` to `max-range`, inclusive."
  (tcel-generator-make-gen "choose"
   (lambda (rnd _size)
     (let ((value (tcel-generator-rand-range rnd lower upper))
	   (pred (lambda (a) (and (>= a lower) (<= a upper)))))
       (qc-rt-filter pred
	(tcel-generator-int-rose-tree value pred lower upper))))))


(defun tcel-generator-one-of  (generators)
  "Create a generator that randomly chooses a value from the list of
  provided generators. Shrinks toward choosing an earlier generator,
  as well as shrinking the value generated by the chosen generator.
  Examples:
      (one-of (list gen/int gen/boolean (gen/vector gen/int)))
  "

  (assert (every 'tcel-generator-generator? generators) t "Arg to one-of must be a collection of generators")
  (tcel-generator-bind (tcel-generator-choose 0 (1- (length generators)))
		       (lambda (a) (nth a generators))))


(defun tcel-generator-pick (coll n)
  (let ((h (car coll))
	(tail (cdr coll)))
    (destructuring-bind (chance gen) h
      (if (<= n chance)
	  gen
	(tcel-generator-pick tail (- n chance))))))

(defun tcel-generator-frequency (pairs)
  "Create a generator that chooses a generator from `pairs` based on the
  provided likelihoods. The likelihood of a given generator being chosen is
  its likelihood divided by the sum of all likelihoods
  Examples:
      (gen/frequency [[5 gen/int] [3 (gen/vector gen/int)] [2 gen/boolean]])
  "

  (assert (every (lambda (a) (destructuring-bind (x g) a 
			       (and (numberp x) (tcel-generator-generator? g))))
		 pairs)
	  t
	  "Arg to frequency must be a list of (num generator) pairs")
  (let ((total (apply '+ (mapcar 'first pairs))))
    (tcel-generator-gen-bind (tcel-generator-choose 1 total)
			     (lambda (a) (tcel-generator-pick pairs (qc-rt-root a))))))

(defun tcel-generator-elements (coll)
  "Create a generator that randomly chooses an element from `coll`.
  Examples:
      (tcel-generator-elements [:foo :bar :baz])
  "

  (assert (cljs-el-seq coll) t "elements cannot be called with an empty collection")
  (let ((v (lambda(a) (elt coll a))))
    (tcel-generator-gen-bind (tcel-generator-choose 0 (1- (length coll)))
			     (lambda (a) (tcel-generator-gen-pure (qc-rt-fmap v a))))))


(defun tcel-generator-such-that-helper   (max-tries pred gen tries-left rand-seed size)
  (if (zerop tries-left)
      (throw ex-info (str "Couldn't satisfy tcel-generator-such-that predicate after "
			  max-tries " tries.")))
  (let ((value (tcel-generator-call-gen gen rand-seed size)))
    (if (funcall pred (qc-rt-root value))
	(qc-rt-filter pred value)
      (tcel-generator-such-that-helper max-tries pred gen (1- tries-left) rand-seed (1+ size)))))


(defun tcel-generator-such-that   (pred gen &optional max-tries)
  "Create a generator that generates values from `gen` that satisfy predicate
  `pred`. Care is needed to ensure there is a high chance `gen` will satisfy
  `pred`. By default, `such-that` will try 10 times to generate a value that
  satisfies the predicate. If no value passes this predicate after this number
  of iterations, a runtime exception will be throw. You can pass an optional
  third argument to change the number of times tried. Note also that each
  time such-that retries, it will increase the size parameter.
  Examples:
      ;; generate non-empty vectors of integers
      ;; (note, tcel-generator-not-empty does exactly this)
      (tcel-generator-such-that tcel-generator-not-empty (tcel-generator-vector tcel-generator-int))
  "
  (let ((max-tries (or max-tries 10)))
    (assert (tcel-generator-generator? gen) t "Second arg to such-that must be a generator")
    (tcel-generator-make-gen "such-that"
     (lambda (rand-seed size)
       (tcel-generator-such-that-helper max-tries pred gen max-tries rand-seed size)))))


(defun tcel-generator-not-empty (gen)
  "Modifies a generator so that it doesn't generate empty collections.
  Examples:
      ;; generate a vector of booleans, but never the empty vector
      (tcel-generator-not-empty (tcel-generator-vector tcel-generator-boolean))
  "
  (assert (tcel-generator-generator? gen) "Arg to not-empty must be a generator")
  (tcel-generator-such-that (lambda(coll) (< 0 (length coll))) gen))

(defun tcel-generator-no-shrink (gen)
  "Create a new generator that is just like `gen`, except does not shrink
  at all. This can be useful when shrinking is taking a long time or is not
  applicable to the domain."
  (assert (generator? gen) "Arg to no-shrink must be a generator")
  (tcel-generator-gen-bind gen
			   (lambda (coll)
			     (destructuring-bind (root _children) coll
			       (tcel-generator-gen-pure
				(list root '()))))))

(defun shrink-2 (gen)
  "Create a new generator like `gen`, but will consider nodes for shrinking
  even if their parent passes the test (up to one additional level)."

  (assert (tcel-generator-generator? gen) "Arg to shrink-2 must be a generator")
  (tcel-generator-gen-bind gen (lambda (a) (tcel-generator-gen-pure (qc-rt-collapse a)))))

(defun tcel-generator-boolean (&rest args)
  (tcel-generator-elements [t nil]))


(defun tcel-generator-tuple (&rest generators)
  "Create a generator that returns a vector, whose elements are chosen
  from the generators in the same position. The individual elements shrink
  according to their generator, but the value will never shrink in count.
  Examples:
      (setq ts (tcel-generator-tuple (tcel-generator-int) (tcel-generator-boolean)))
      (tcel-generator-sample ts)
      ;; => ([1 true] [2 true] [2 false] [1 false] [0 true] [-2 false] [-6 false]
      ;; =>  [3 true] [-4 false] [9 true]))
  "

  (assert (every 'tcel-generator-generator? generators) t
          "Args to tuple must be generators")
  (tcel-generator-gen-bind (tcel-generator-gen-seq->seq-gen generators)
			   (lambda (roses)
			     (tcel-generator-gen-pure (qc-rt-zip 'list roses)))))

(defun tcel-generator-int (&rest _)
  "Generates a positive or negative integer bounded by the generator's
  `size` parameter.
  Really returns a long"
  (tcel-generator-sized (lambda (size) (tcel-generator-choose (- size) size))))

(defun tcel-generator-nat (&rest _)
  "Generates natural numbers, starting at zero. Shrinks to zero."
  (tcel-generator-fmap (lambda (a) (abs a)) (tcel-generator-int)))

(defun tcel-generator-pos-int (&rest _)
  "Generate positive integers bounded by the generator's `size` parameter."
  (tcel-generator-nat))

(defun tcel-generator-neg-int (&rest _)
  "Generate negative integers bounded by the generator's `size` parameter."
  (tcel-generator-fmap (lambda (a) (* -1 a)) (tcel-generator-nat)))


(defun tcel-generator-s-pos-int (&rest _)
  "Generate strictly positive integers bounded by the generator's `size`
   parameter."
  (tcel-generator-fmap '1+ (tcel-generator-nat)))

(defun tcel-generator-s-neg-int (&rest _)
  "Generate strictly negative integers bounded by the generator's `size`
   parameter."
  (tcel-generator-fmap '1- (tcel-generator-neg-int)))


(defun tcel-generator-vector1 (generator)
  "See `tcel-generator-vector'"
  (assert (tcel-generator-generator? generator) t "Arg to vector must be a generator")
  (tcel-generator-gen-bind
   (tcel-generator-sized (lambda (a) (tcel-generator-choose 0 a)))
   (lambda (num-elements-rose)
     (tcel-generator-gen-bind (tcel-generator-gen-seq->seq-gen
			       (make-list (qc-rt-root num-elements-rose)
					  generator))
			      (lambda (roses)
				(tcel-generator-gen-pure (qc-rt-shrink 'vector roses)))))))

(defun tcel-generator-vector2 (generator num-elements)
  "See `tcel-generator-vector'"
  (assert (tcel-generator-generator? generator) t "First arg to vector must be a generator")
  (apply 'tcel-generator-tuple (make-list num-elements generator)))

(defun tcel-generator-vector3 (generator min-elements max-elements)
  "See `tcel-generator-vector'"
   (assert (tcel-generator-generator? generator) t "First arg to vector must be a generator")
   (tcel-generator-gen-bind
    (tcel-generator-choose min-elements max-elements)
    (lambda (num-elements-rose)
      (tcel-generator-gen-bind (tcel-generator-gen-seq->seq-gen
				(make-list (qc-rt-root num-elements-rose)
					   generator))
			       (lambda (roses)
				 (tcel-generator-gen-bind
				  (tcel-generator-gen-pure (qc-rt-shrink 'vector roses))
				  (lambda (rose)
				    (tcel-generator-gen-pure (qc-rt-filter
							      (lambda (v) (and (>= (length v) min-elements)
									       (<= (length v) max-elements))) rose)))))))))

(defun tcel-generator-vector (generator &optional num-elements max-elements)
  "Create a generator whose elements are chosen from `gen`. The count of the
  vector will be bounded by the `size` generator parameter."
  (if (and num-elements max-elements)
      (tcel-generator-vector3 generator num-elements max-elements)
    (if num-elements
	(tcel-generator-vector2 generator num-elements)
      (tcel-generator-vector1 generator))))

(defun tcel-generator-list (generator)
  "Like `vector`, but generates lists."
  (assert (tcel-generator-generator? generator) t "First arg to list must be a generator")
  (tcel-generator-gen-bind (tcel-generator-sized (lambda (a) (tcel-generator-choose 0 a)))
			   (lambda (num-elements-rose)
			     (tcel-generator-gen-bind (tcel-generator-gen-seq->seq-gen
						       (make-list (qc-rt-root num-elements-rose)
								  generator))
						      (lambda (roses)
							(tcel-generator-gen-pure (qc-rt-shrink 'list roses)))))))



(defun tcel-generator-swap (coll indexes)
  (when indexes
    (let* ((i1 (elt indexes 0))
	   (i2 (elt indexes 1))
	   (v1 (elt coll i1))
	   (v2 (elt coll i2)))
      (-replace-at i1 v2 
		   (-replace-at i2 v1 coll)))))


(defun tcel-generator-shuffle (coll)
  "Create a generator that generates random permutations of `coll`. Shrinks
  toward the original collection: `coll`."
  (let ((index-gen (tcel-generator-choose 0 (1- (length coll)))))
    (tcel-generator-fmap (lambda (a)  (cljs-el-reduce3 'tcel-generator-swap coll a))
          ;; a vector of swap instructions, with count between
          ;; zero and 2 * count. This means that the average number
          ;; of instructions is count, which should provide sufficient
          ;; (though perhaps not 'perfect') shuffling. This still gives us
          ;; nice, relatively quick shrinks.
			 (tcel-generator-vector (tcel-generator-tuple index-gen index-gen) 0 (* 2 (length coll))))))


(defun tcel-generator-alist (key-gen val-gen)
  "Create a generator that generates alists, with keys chosen from
  `key-gen` and values chosen from `val-gen`."

  (let ((input (tcel-generator-vector (tcel-generator-tuple key-gen val-gen))))
    (tcel-generator-fmap (lambda (v) (mapcar (lambda (t) (cons (elt t 0) (elt t 1)))
					     v)) 
			 input)))

(defun tcel-generator-plist (key-gen val-gen)
  "Create a generator that generates plists, with keys chosen from
  `key-gen` and values chosen from `val-gen`."

  (let ((input (tcel-generator-vector (tcel-generator-tuple key-gen val-gen))))
    (tcel-generator-fmap (lambda (v) (apply 'append (mapcar (lambda (t) (list (elt t 0) (elt t 1)))
							    v)))
			 input)))

(defun tcel-generator-hash-table (key-gen val-gen)
  "Create a generator that generates hash-tables, with keys chosen from
  `key-gen` and values chosen from `val-gen`."

  (let ((input (tcel-generator-vector (tcel-generator-tuple key-gen val-gen)))
	(rtnval (make-hash-table)))
    (tcel-generator-fmap (lambda (v) (let ((rtnval (make-hash-table)))
				       (mapcar (lambda (t) (puthash (elt t 0) 
								    (elt t 1)
								    rtnval))
				       
					       v)
				       rtnval))
			 input)))


(defun plist-keys (pl)
  (when pl
    (cl-loop for (key _) on pl by 'cddr
	     collect key)))
  
(defun plist-values (pl)
  (when pl
    (cl-loop for (_ value) on pl by 'cddr
	     collect value)))

(defun tcel-generator-keyed-alist (&rest kvs)
  "
   Returns a generator that makes maps with the supplied keys and
   values generated using the supplied generators.
  Examples:
    (tcel-generator-keyed-alist :a tcel-generator-boolean :b tcel-generator-nat)
  "
  (assert (evenp (length kvs)))
  (let ((ks (plist-keys kvs))
	(vs (plist-values kvs)))
    (assert (every 'tcel-generator-generator? vs)
            "Value args to hash-map must be generators")
    (tcel-generator-fmap (lambda (a) (-zip ks a))
          (apply 'tcel-generator-tuple vs))))


(defun tcel-generator-keyed-plist (&rest kvs)
  "
   Returns a generator that makes a plist with the supplied keys and
   values generated using the supplied generators.
  Examples:
    (tcel-generator-keyed-plist :a tcel-generator-boolean :b tcel-generator-nat)
  "
  (assert (evenp (length kvs)))
  (let ((ks (plist-keys kvs))
	(vs (plist-values kvs)))
    (assert (every 'tcel-generator-generator? vs)
            "Value args to hash-map must be generators")
    (tcel-generator-fmap (lambda (a) (-interleave ks a))
          (apply 'tcel-generator-tuple vs))))


(defun tcel-generator-keyed-hash-table (&rest kvs)
  "
   Returns a generator that makes a plist with the supplied keys and
   values generated using the supplied generators.
  Examples:
    (tcel-generator-keyed-plist :a tcel-generator-boolean :b tcel-generator-nat)
  "
  (assert (evenp (length kvs)))
  (let ((ks (plist-keys kvs))
	(vs (plist-values kvs)))
    (assert (every 'tcel-generator-generator? vs)
            "Value args to hash-map must be generators")
    (tcel-generator-fmap (lambda (a) (let ((rtnval (make-hash-table)))
				       (-zip-with (lambda (k v)
						    (puthash k v rtnval))
						  ks a)
				       rtnval))
			 (apply 'tcel-generator-tuple vs))))


(defun tcel-generator-char ( &rest _)
  "Generates character from 0-255."
  (tcel-generator-choose 0 255))

(defun tcel-generator-char-ascii ( &rest _)
  "Generate only ascii character."
  (tcel-generator-choose 32 126))

(defun tcel-generator-char-alphanumeric  ( &rest _)
  "Generate alphanumeric characters."
  (tcel-generator-one-of (list (tcel-generator-choose 48 57)
			       (tcel-generator-choose 65 90)
			       (tcel-generator-choose 97 122))))

(defalias  'tcel-generator-char-alpha-numeric    'tcel-generator-char-alphanumeric
  "Deprecated - use char-alphanumeric instead.
  Generate alphanumeric characters."
)

(defun tcel-generator-char-alpha ( &rest _ )
  "Generate alpha characters."
  (tcel-generator-one-of (list(tcel-generator-choose 65 90)
			      (tcel-generator-choose 97 122))))

(defun tcel-generator-char-symbol-special  ( &rest _)
  "Generate non-alphanumeric characters that can be in a symbol."
  (tcel-generator-elements [?* ?+ ?! ?- ?_ ??]))

(defun tcel-generator-char-keyword-rest  ( &rest _)
  "Generate characters that can be the char following first of a keyword."
  (tcel-generator-frequency (list (list 2 (tcel-generator-char-alphanumeric))
				  (list 1 (tcel-generator-char-symbol-special)))))

(defun tcel-generator-char-keyword-first  ( &rest _)
  "Generate characters that can be the first char of a keyword."
  (tcel-generator-frequency (list (list 2 (tcel-generator-char-alpha))
				  (list 1 (tcel-generator-char-symbol-special)))))


(defun tcel-generator-string (&rest _)
  "Generate strings. May generate unprintable characters."
  (tcel-generator-fmap 'concat (tcel-generator-vector (tcel-generator-char))))


(defun tcel-generator-string-ascii (&rest _)
  "Generate ascii strings."
  (tcel-generator-fmap 'concat (tcel-generator-vector (tcel-generator-char-ascii))))

(defun tcel-generator-string-alpha (&rest _)
  "Generate ascii strings."
  (tcel-generator-fmap 'concat (tcel-generator-vector (tcel-generator-char-alpha))))

(defun tcel-generator-string-alphanumeric (&rest _)
  "Generate alphanumeric strings."
  (tcel-generator-fmap 'concat (tcel-generator-vector (tcel-generator-char-alphanumeric))))

(defun  tcel-generator-string-alpha-numeric 'tcel-generator-string-alphanumeric
  "Deprecated - use string-alphanumeric instead.
  Generate alphanumeric strings."
  )

(defun tcel-generator-+-or---digit? (c d)
  "Returns true if c is \\+ or \\- and d is non-nil and a digit.
  Symbols that start with +3 or -2 are not readable because they look
  like numbers.0 - 9"
  (and d
       (or (eq ?+ c)
	   (eq ?- c))
       (or 
	(and (<= ?0 d) (>= ?9 d)))))

(defun tcel-generator-to-list (coll)
  "Convert a simple sequence into a list"
  (when coll
    (cond ((listp coll) coll)
	  (t (mapcar 'identity coll)))))

(defun tcel-generator-keyword-segment-rest (&rest _)
  "Generate segments of a keyword (between \\:)"
  (->> (tcel-generator-tuple (tcel-generator-char-keyword-rest) (tcel-generator-vector (tcel-generator-char-keyword-rest)))
       (tcel-generator-fmap (lambda (a) (concat (when (car a) (list (car a)))  (tcel-generator-to-list (cadr a)))))))

(defun tcel-generator-keyword-segment-first (&rest _)
  "Generate segments of a keyword that can be first (between \\:)"
  (->> (tcel-generator-tuple (tcel-generator-char-keyword-first) (tcel-generator-vector (tcel-generator-char-keyword-rest)))
       (tcel-generator-fmap (lambda (a) (concat  (when (car a) (list (car a))) (tcel-generator-to-list (cadr a)))))))

(defun tcel-generator-keyword (&rest _)
  "Generate keywords without namespaces."
  (->> (tcel-generator-tuple (tcel-generator-keyword-segment-first) (tcel-generator-list (tcel-generator-keyword-segment-rest)))
       (tcel-generator-fmap (lambda (a)
			      (intern (apply 'concat ":" (car a) (cadr a)))))))


(defun tcel-generator-symbol (&rest _)
  "Generate keywords without namespaces."
  (->> (tcel-generator-tuple (tcel-generator-keyword-segment-first) 
			     (tcel-generator-list (tcel-generator-keyword-segment-rest)))
       (tcel-generator-fmap (lambda (a)
			      (intern (apply 'concat (car a) (cadr a)))))))

(defun tcel-generator-simple-type ()
  (tcel-generator-one-of (list (tcel-generator-int )
			       (tcel-generator-char )
			       (tcel-generator-string )
			       (tcel-generator-boolean )
			       (tcel-generator-keyword )
			       (tcel-generator-symbol))))

(defun tcel-generator-simple-type-printable ()
  (tcel-generator-one-of (list( tcel-generator-int )
			       (tcel-generator-char-ascii )
			       (tcel-generator-string-ascii )
			       (tcel-generator-boolean )
			       (tcel-generator-keyword )
			       (tcel-generator-symbol))))


(defun tcel-generator-container-type  (inner-type)
  (tcel-generator-one-of (list (tcel-generator-vector inner-type)
			       (tcel-generator-list inner-type)
			       (tcel-generator-alist inner-type inner-type)
			       (tcel-generator-plist inner-type inner-type)
			       (tcel-generator-hash-table inner-type inner-type))))


(defun tcel-generator-recursive-helper (container-gen-fn scalar-gen scalar-size children-size height)
  (if (zerop height)
      (tcel-generator-resize scalar-size scalar-gen)
    (tcel-generator-resize children-size
			   (funcall container-gen-fn
				    (tcel-generator-recursive-helper
				     container-gen-fn scalar-gen
				     scalar-size children-size (1- height))))))


(defun tcel-generator-recursive-gen (container-gen-fn scalar-gen)
  "This is a helper for writing recursive (tree-shaped) generators. The first
  argument should be a function that takes a generator as an argument, and
  produces another generator that 'contains' that generator. The vector function
  in this namespace is a simple example. The second argument is a scalar
  generator, like boolean. For example, to produce a tree of booleans:
    (tcel-generator-recursive-gen 'tcel-generator-vector (tcel-generator-int))
  Vectors or maps either recurring or containing booleans or integers:
    (gen/recursive-gen (fn [inner] (gen/one-of [(gen/vector inner)
                                                (gen/map inner inner)]))
                       (gen/one-of [gen/boolean gen/int]))
  "

  (assert (tcel-generator-generator? scalar-gen)
          "Second arg to recursive-gen must be a generator")
  (tcel-generator-sized (lambda (size)
			  (tcel-generator-bind (tcel-generator-choose 1 5)
					       (lambda (height)
						 (let ((children-size (floor (expt size (/ 1.0 height)))))
						   (tcel-generator-recursive-helper container-gen-fn scalar-gen size
										    children-size height)))))))

(defun tcel-generator-any ()
  "A recursive generator that will generate many different, often nested, values"
  (tcel-generator-recursive-gen (tcel-generator-container-type) (tcel-generator-simple-type)))

(defun tcel-generator-any-printable ()
  "Like any, but avoids characters that the shell will interpret as actions,
  like 7 and 14 (bell and alternate character set command)"
  (tcel-generator-recursive-gen 'tcel-generator-container-type (tcel-generator-simple-type-printable)))


(provide 'tcel-generator)
;;; generator.el ends here

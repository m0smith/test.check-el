;;; rosetree.el --- Rose Tree implementation based on Clojure test.check  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Matthew O. Smith

;; Author: Matthew O. Smith <matt@m0smith.com>
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

;; See
;;  https://github.com/clojure/test.check/blob/master/src/main/clojure/cljs/test/check/rose_tree.cljs
;;  Rose trees are cons cells with the car being the value.  The cdr
;;  is a lazy collection of zero or more rose trees.
;;
;;  Naming Convention:
;;     coll  - anything that is a cljs-el-seq: list,
;;     vector, cljs-el-lazy-cons, cljs-el-lazy-chunk
;;     rose  - a rose tree which is a cons (value . children) where
;;            children is a coll of rose
;;     roses - a coll of rose, same as children
;;; Code:

(require 'dash)
(require 'cljs-el)

(defun qc-rt-exclude-nth (n coll)
  (cljs-el-map 'cdr (cljs-el-filter (lambda (e) (not (= n (car e))))
				    (cljs-el-map-indexed 'cons coll))))

(defun qc-rt-replace-at (n val coll)
  "Repleace the Nth element of COLL with VAL"
  (cljs-el-map  (lambda (e) (if (= n (car e)) val (cdr e)))
		(cljs-el-map-indexed 'cons coll)))

(defun qc-rt-rosep (rose)
  (or (not rose)
      (consp rose)))

(defun qc-rt-map (f coll)
  "Return a new vector with F applied to each element of COLL"
  (cljs-el-map f coll))

(defun qc-rt-root (rose)
  "Returns the root value of a Rose tree."
  (assert (qc-rt-rosep rose) t "Must be a rose")
  (let ((root (car rose)))
    root))

(defun qc-rt-children (rose)
  "Returns the children of the root of the Rose tree."
  (assert (qc-rt-rosep rose) t "Must be a rose")
  (let ((children (cdr rose)))
    children))

(defun qc-rt-make-rose (value children)
  "Return a new Rose Tree with value and children"
  (cons value children))

(defun qc-rt-join (rose)
  "Turn a tree of trees into a single tree. Does this by concatenating
  children of the inner and outer trees.
"
  (assert (qc-rt-rosep rose) t "Must be a rose")
  (let* ((the-root (qc-rt-root rose))
	 (children (qc-rt-children rose))
	 (inner-root (qc-rt-root the-root))
	 (inner-children (qc-rt-children the-root)))
    (qc-rt-make-rose inner-root (-concat (cljs-el-map 'qc-rt-join children)
				   inner-children))))

(defun qc-rt-pure (x)
  "Puts a value `x` into a Rose tree, with no children."
  (qc-rt-make-rose x []))

(defun qc-rt-fmap (f rose)
  "Applies functions `f` to all values in the tree."
  (assert (qc-rt-rosep rose) t "Must be a rose")
  (let ((root (qc-rt-root rose))
	(children (qc-rt-children rose)))
    (qc-rt-make-rose (funcall f root) (cljs-el-map (lambda (a) (qc-rt-fmap f a)) children))))

(defun qc-rt-bind (rose f)
  "Takes a Rose tree (ROSE) and a function (F) from
  values to Rose tree and returns a new Rose tree.
  This is the monadic bind (>>=) for Rose trees."
  (qc-rt-join (qc-rt-fmap f rose)))


(defun qc-rt-make-lazy (coll)
  "Convert coll to a lazy, unless it is already lazy"
  (when (cljs-el-seq coll)
    (if (cljs-el-lazy-p coll)
	coll
      (let ((chunk (cljs-el-list coll)))
	(cljs-el-lazy-cons "qc-rt-make-lazy"
			   :car-fn (lambda () (cljs-el-car coll))
			   :cdr-fn (lambda () (cljs-el-cdr coll)))))))

(defun qc-rt-make-lazy-chunk (coll)
  "Convert coll to a lazy chunk, unless it is already lazy"
  (when (cljs-el-seq coll)
    (if (cljs-el-lazy-p coll)
	coll
      (let ((chunk (cljs-el-list coll)))
	(cljs-el-lazy-chunk "qc-rt-make-lazy-chunk"
			    :car  chunk
			    :cdr-fn (lambda () nil))))))

(defun qc-rt-filter (pred rose)
  "Returns a new Rose tree whose values pass `pred`. Values who
  do not pass `pred` have their children cut out as well."
  (assert (qc-rt-rosep rose) t "Must be a rose")

  (let ((the-root (qc-rt-root rose))
	(children (qc-rt-children rose)))
    (cl-labels ((recur (rose) (qc-rt-filter pred rose))
		(valid-root (child) (funcall pred (qc-rt-root child))))
      (let* ((valid-childern (qc-rt-make-lazy (cljs-el-filter (function valid-root) children))) ;
	    (new-children (cljs-el-map (function recur) valid-childern)))
	(qc-rt-make-rose the-root new-children)))))
			



(defun qc-rt-indexed-coll (coll)
  "For each element in COLL, return (list idx ele)"
  (cljs-el-map-indexed 'list coll))

(defun qc-rt-permutations   (roses)
  "Create a seq of lists, where each rose in turn, has been replaced
  by its children."
  (apply 'vconcat
	 (loop for (index root) in (cljs-el-list (qc-rt-indexed-coll roses))
	       collect (loop  for child in (cljs-el-list (qc-rt-children root))
			      collect (qc-rt-replace-at index child roses)))))



(defun qc-rt-zip   (f roses)
  "Apply `f` to the sequence of Rose trees `roses`."
  (qc-rt-make-rose (apply f (mapcar 'qc-rt-root roses))
		   (qc-rt-map (lambda (a) (qc-rt-zip f a))
			      (qc-rt-make-lazy-chunk (qc-rt-permutations roses)))))

(defun qc-rt-remove (roses)
  (when (< 0 (length roses))
    (vconcat
     (map 'vector (lambda (index) (qc-rt-exclude-nth index roses)) (number-sequence 0 (1- (length roses))))
     (qc-rt-permutations  roses))))


(defun qc-rt-shrink  (f roses)
  (if (and roses (sequencep roses))
      (cl-labels (( recur (a) (qc-rt-shrink f a)))
	(qc-rt-make-rose (apply f (mapcar 'qc-rt-root roses))
		   (qc-rt-map (function recur) (qc-rt-make-lazy-chunk (qc-rt-remove roses)))))
    (qc-rt-make-rose (funcall f) '[] )))
  
(defun qc-rt-collapse  (coll)
  "Return a new rose-tree whose depth-one children
  are the children from depth one _and_ two of the input
  tree."
  (let ((root (elt coll 0))
	(the-children (elt coll 1)))
    (qc-rt-make-rose root (vconcat (map 'vector 'qc-rt-collapse the-children)
			     (map 'vector 'qc-rt-collapse
				  (map 'vector 'qc-rt-children the-children))))))

(defun qc-rt-make-stack  (children stack)
  (if  (and children (sequencep children))
    (cons children stack)
    stack))


(defun qc-rt-puthash (key value table)
  (puthash key value table)
  table)

(defun qc-rt-seq (root)
  "Create a list of all of the (unique) nodes in a shrink-tree.
  This assumes that two nodes with the same value have the same children.
  While it's not common, it's possible to create trees that don't
  fit that description. This function is significantly faster than
  brute-force enumerating all of the nodes in a tree, as there will
  be many duplicates."
  (cl-labels ((helper (coll seen stack)
		      (let ((node     (elt coll 0))
			    (children (elt coll 1)))
		    (if (not (gethash node seen))
                     (cons node
                           (if (and children (sequencep children))
			       (helper (first children) (qc-rt-puthash node node seen)
				       (qc-rt-make-stack (rest children) stack))
                             (-when-let (s (and (sequencep stack) stack))
                               (let ((f (first (first s)))
				     (r (rest (first s))))
                                 ( helper f (qc-rt-puthash node node seen)
					  (qc-rt-make-stack r (rest s)))))))
                     (-when-let (s (and (sequencep stack) stack))
                       (let ((f (first( first s)))
                             (r (rest (first s))))
                         ( helper f seen (qc-rt-make-stack r (rest s)))))))))
    (helper root (make-hash-table :test 'equal) '())))

(provide 'tcel-rosetree)
;;; rosetree.el ends here

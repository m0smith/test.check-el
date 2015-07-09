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

;; See https://github.com/clojure/test.check/blob/master/src/main/clojure/cljs/test/check/rose_tree.cljs
;;  Rose trees are nested vectors for performance reasons
;;; Code:

(require 'dash)

(defun qc-rt-exclude-nth (n coll)
  (qc-rt-replace-at n 'qc-rt-excluded coll))

(defun qc-rt-replace-at (n val coll)
  (when coll
    (aset (copy-sequence coll) n val)))

(defun qc-rt-map (f coll)
  "Return a new vector with F applied to each element of COLL"
  (let ((rtnval (copy-sequence coll)))
    (dotimes (i (length coll) rtnval)
      (aset rtnval i (funcall f (aref coll i))))))
   
(defun qc-rt-vector (coll)
  (when coll
    (if (arrayp coll) 
	coll
      (vector coll))))
	

(defun qc-rt-join (coll)
  "Turn a tree of trees into a single tree. Does this by concatenating
  children of the inner and outer trees.
"
  (let* ((the-root (elt coll 0))
	 (children (elt coll 1))
	 (inner-root (elt the-root 0))
	 (inner-children (elt the-root 1)))
    (vector inner-root (-concat (cljs-el-map 'qc-rt-join children)
				inner-children))))

(defun qc-rt-root (coll)
  "Returns the root of a Rose tree."
  (let ((root (elt coll 0)))
    root))

(defun qc-rt-children (coll)
  "Returns the children of the root of the Rose tree."
  (let ((children (elt coll 1)))
    children))

(defun qc-rt-pure (x)
  "Puts a value `x` into a Rose tree, with no children."
  (vector x []))

(defun qc-rt-fmap (f coll)
  "Applies functions `f` to all values in the tree."
  (let ((root (elt coll 0))
	(children (elt coll 1)))
    (vector (funcall f root) (cljs-el-map (lambda (a) (qc-rt-fmap f a)) children))))

(defun qc-rt-bind (m k)
  "Takes a Rose tree (m) and a function (k) from
  values to Rose tree and returns a new Rose tree.
  This is the monadic bind (>>=) for Rose trees."
  (qc-rt-join (qc-rt-fmap k m)))


(defun qc-rt-filter (pred coll)
  "Returns a new Rose tree whose values pass `pred`. Values who
  do not pass `pred` have their children cut out as well.
  Takes a list of roses, not a rose"
  (let ((the-root (elt coll 0))
	(children (elt coll 1)))
    (vector the-root (cljs-el-map (lambda (a) (qc-rt-filter pred a))
				  (cljs-el-filter (lambda (b) (funcall pred (qc-rt-root b))) children)))))

(defun qc-rt-permutations   (roses)
  "Create a seq of lists, where each rose in turn, has been replaced
  by its children."
  (apply 'vconcat
	 (loop for (root index) in (map 'vector (lambda(index) (list (elt roses index) index)) (number-sequence 0 (1- (length roses))))
	       collect (loop  for child in (qc-rt-children root)
			      collect (qc-rt-replace-at index child roses)))))

(defun qc-rt-zip   (f roses)
  "Apply `f` to the sequence of Rose trees `roses`."
  (vector (apply f (mapcar 'qc-rt-root roses))
	  (qc-rt-map (lambda (a) (qc-rt-zip f a))
		     (qc-rt-permutations roses))))

(defun qc-rt-remove (roses)
  (when (< 0 (length roses))
    (vconcat
     (map 'vector (lambda (index) (qc-rt-exclude-nth index roses)) (number-sequence 0 (1- (length roses))))
     (qc-rt-permutations  roses))))


(defun qc-rt-shrink  (f roses)
  (if (and roses (sequencep roses))
      (vector (apply f (mapcar 'qc-rt-root roses))
	      (qc-rt-map (lambda (a) (qc-rt-shrink f a)) (qc-rt-remove roses)))
    (vector (funcall f) '[] )))
  
(defun qc-rt-collapse  (coll)
  "Return a new rose-tree whose depth-one children
  are the children from depth one _and_ two of the input
  tree."
  (let ((root (elt coll 0))
	(the-children (elt coll 1)))
    (vector root (vconcat (map 'vector 'qc-rt-collapse the-children)
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

(provide 'rosetree)
;;; rosetree.el ends here

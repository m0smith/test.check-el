;;; tcel-check.el --- test.check  -*- lexical-binding: t; -*-

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

;; https://github.com/clojure/test.check/blob/master/src/main/clojure/cljs/test/check.cljs

;;; Code:

(require 'tcel-generator)
(require 'tcel-rosetree)

(defun tcel-check-make-rng (&optional seed)
  (let ((seed (or seed (ffloor (float-time)))))
    (list seed (tcel-generator-random seed))))

(defun tcel-check-complete (progress-reporter property num-trials seed)
  (progress-reporter-done progress-reporter)
  (list :result t :num-tests num-trials :seed seed))

(defun tcel-check-not-falsey-or-exception-p (value)
  "True if the value is not falsy or an exception"
  (let ((rtnval (or (and value (not (consp value)))
		    (and (consp value)
			      (not (get (car value) 'error-conditions))))))
    rtnval))


(defun tcel-check-smallest-shrink  (total-nodes-visited depth smallest)
  (list :total-nodes-visited total-nodes-visited
	:depth depth
	:result (plist-get smallest :result)
	:smallest (plist-get smallest :args)))
  
(defun tcel-check-shrink-loop (rose-tree)
  "Shrinking a value produces a sequence of smaller values of the same type.
  Each of these values can then be shrunk. Think of this as a tree. We do a
  modified depth-first search of the tree:
  Do a non-exhaustive search for a deeper (than the root) failing example.
  Additional rules added to depth-first search:
  * If a node passes the property, you may continue searching at this depth,
  but not backtrack
  * If a node fails the property, search its children
  The value returned is the left-most failing example at the depth where a
  passing example was found."

  (let ((shrinks-this-depth (qc-rt-children rose-tree)))
    (let ((nodes shrinks-this-depth)
	  (current-smallest (qc-rt-root rose-tree))
	  (total-nodes-visited 0)
	  (depth 0)
	  (rtnval nil))
      (while (not rtnval)

	(if (not (cljs-el-seq nodes))
	    (setq rtnval (tcel-check-smallest-shrink total-nodes-visited depth current-smallest))
	  (let* ((head (cljs-el-car nodes))
		 (tail (cljs-el-cdr nodes))
		 (result (plist-get  (qc-rt-root head) :result)))
	    (if (tcel-check-not-falsey-or-exception-p result)
		;; this node passed the test, so now try testing its right-siblings
		(setq nodes tail
		      total-nodes-visited (1+ total-nodes-visited))

	      ;; this node failed the test, so check if it has children,
	      ;; if so, traverse down them. If not, save this as the best example
	      ;; seen now and then look at the right-siblings
	      ;; children
	      (let ((children (qc-rt-children head)))
		(if (not (cljs-el-seq children))
		    (setq nodes tail
			  current-smallest (qc-rt-root head)
			  total-nodes-visited (1+ total-nodes-visited))
		  (setq nodes children
			current-smallest (qc-rt-root head)
			total-nodes-visited (1+ total-nodes-visited)
			depth (1+ depth)))))))))))


(defun tcel-check-failure  (property failing-rose-tree trial-number size seed)
  (let* ((root (qc-rt-root failing-rose-tree))
	 (result (plist-get root :result))
	 (failing-args (plist-get root :args)))
    
    ;(ct/report-failure property result trial-number failing-args)
    
    (ert-fail(list :result result
		   :seed seed
		   :failing-size size
		   :num-tests (1+ trial-number)
		   :fail failing-args
		   :shrunk (tcel-check-shrink-loop failing-rose-tree)))))


(defun tcel-check-quick-check   (num-tests property &optional options)
  "Tests `property` `num-tests` times.
  Takes optional keys `:seed` and `:max-size`. The seed parameter
  can be used to re-run previous tests, as the seed used is returned
  after a test is run. The max-size can be used to control the 'size'
  of generated values. The size will start at 0, and grow up to
  max-size, as the number of tests increases. Generators will use
  the size parameter to bound their growth. This prevents, for example,
  generating a five-thousand element vector on the very first test.
  Examples:
       (tcel-check-quick-check 100
                          (tcel-properties-for-all (a (tcel-generator-int)) 
                                                   (>= (* a a) a)))

  "
  (let* ((seed (plist-get options :seed))
	 (max-size (or (plist-get options :max-size) 200))
	 (progress-reporter (make-progress-reporter "Quick check ... " 0 num-tests 0)))
    (destructuring-bind (created-seed rng) (tcel-check-make-rng seed)
      (let ((size-seq (tcel-generator-make-size-range-seq max-size))
	    (so-far 0)
	    (rtnval nil))
	(while (not rtnval)
	  (if (= so-far num-tests)
	      (setq rtnval (tcel-check-complete progress-reporter property num-tests created-seed))
	    (let* ((size (cljs-el-car size-seq))
		   (rest-size-seq (cljs-el-cdr size-seq))
		   (result-map-rose (tcel-generator-call-gen property rng size))
		   (result-map (qc-rt-root result-map-rose))
		   (result (plist-get result-map :result))
		   (args (plist-get result-map :args)))
	      (if (not (tcel-check-not-falsey-or-exception-p result))
		  (setq rtnval (tcel-check-failure property result-map-rose so-far size created-seed))
		(progn
		  (setq so-far (1+ so-far))
		  (progress-reporter-update progress-reporter so-far)
		  (setq size-seq rest-size-seq))))))
	rtnval))))



  

(provide 'tcel-check)
;;; tcel-check.el ends here

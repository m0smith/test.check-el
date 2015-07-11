;;; tcel-test.el --- test.check running and reporting  -*- lexical-binding: t; -*-

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

;; https://github.com/clojure/test.check/blob/master/src/main/clojure/cljs/test/check/cljs_test.cljs

;; report map keys
;;   :trial is a list with [so-far total]
;;   :property-fun (needs a property :name)
;;   :params 
;;   :progress - a progress-reporter https://www.gnu.org/software/emacs/manual/html_node/elisp/Progress.html#Progress


;;; Code:


(defun tcel-test-error? (m)
  "Return true if the :result key of M (a plist) is an error. 
An error is a cons ( symbol . data ) where symbol will have a property 'error-conditions'.
See https://www.gnu.org/software/emacs/manual/html_node/elisp/Error-Symbols.html "
  (let ((result (plist-get m :result)))
    (and (consp result)
	 (get (car result) 'error-conditions))))

(defun tcel-test-assert-check (m)
  "M is a propery list created by `tcel-properties-apply-gen'"
  (let ((result (plist-get m :result)))
  (message "%s" m)
  (assert (not (tcel-test-error? m)) nil "Caught signal: %s :s" (car result) (get (car result) 'error-message))
  (should result)))

(defcustom tcel-test-default-test-count 100
  "The default number of tests to run.")

(defun tcel-test-process-options  (options)
  "OPTIONS can be nil, a number or a plist.  Return a plist."
  (cond ((null options) (list :num-tests tcel-test-default-test-count))
        ((numberp options) (list :num-tests options))
        ((listp options) (if (plist-get options :num-tests)
			     options
			   (plist-put options  :num-tests tcel-test-default-test-count)))
        (t (signal 'error (list  "Invalid tcel-test-defspec options: " options)))))

(defcustom tcel-test-report-trials nil
  "Controls whether property trials should be reported via clojure.test/report.
  Valid values include:
  * nil - no reporting of trials (default)
  * a function - will be passed a clojure.test/report-style map containing
  :clojure.test.check/property and :clojure.test.check/trial slots
  * t - provides quickcheck-style trial reporting (dots) via
  `trial-report-dots`
  (Note that all reporting requires running `quick-check` within the scope of a
  clojure.test run (via `test-ns`, `test-all-vars`, etc.)
  Reporting functions offered by clojure.test.check include `trial-report-dots` and
  `trial-report-periodic` (which prints more verbose trial progress information
  every `*trial-report-period*` milliseconds."
  )

(defcustom  tcel-test-report-shrinking nil
  "If true, a verbose report of the property being tested, the
  failing return value, and the arguments provoking that failure is emitted
  prior to the start of the shrinking search."
  )

(defcustom tcel-test-trial-report-period 10000
  "Milliseconds between reports emitted by `trial-report-periodic`."
  )

(defvar tcel-test-last-trial-report 0)


(defun tcel-test-get-property-name (report-map)
  ( let ((property-fun (plist-get report-map :property-fun)))
    (or (get property-fun :name) 
	(ct/testing-vars-str report-map))))


(defun tcel-test-trial-report-periodic (m)
  "Intended to be bound as the value of `tcel-test-report-trials`; will emit a verbose
  status every `tcel-test-trial-report-period` milliseconds, like this one:
  Passing trial 3286 / 5000 for (your-test-var-name-here) (:)"
  (let* ((progress-reporter (plist-get m :progress))
	 (trial (plist-get m :trial))
	 (so-far (car trial)))
    (progress-reporter-update progress-reporter so-far)))

(defun trial-report-dots   (report-map)
  "Intended to be bound as the value of `tcel-test-report-trials`; will emit a single
  dot every 1000 trials reported. [{[so-far total] ::trial}]"
  (let* ((trial (plist-get report-map :trial))
	 (so-far (car trial))
	 (total (cadr trial)))
    (when (< 0 so-far)
      (when (< 0  (mod so-far 1000))
	(message "."))
      (when (== so-far total) (message "")))))


(defun tcel-test-ct/report-default-trial (m)
  (-when-let (trial-report-fn (and tcel-test-report-trials
                                  (if tcel-test-report-trials
				      'tcel-test-trial-report-periodic
                                    tcel-test-report-trials)))
    (funcal trial-report-fn m)))

(defun tcel-test-ct/report-default-begin-test-var (m)
    (setq tcel-testlast-trial-report (floor (float-time)))
    (when begin-test-var-method (begin-test-var-method m)))

(defun tcel-test-ct/report-default-shrinking (m)
  (when tcel-test-report-shrinking
    (message "Shrinking %s starting with parameters %s" 
	     (tcel-test-get-property-name m)
	     (plist-get m :params ))))


(defun tcel-test-report-trial  (property-fun so-far num-tests)
  (ct/report (list :type :trial
		   :property property-fun
		   :trial (list so-far num-tests))))

(defun tcel-test-report-failure  (property-fun result trial-number failing-params)
  (ct/report (list :type :shrinking
		   :property property-fun
		   :params  failing-params)))


(defmacro tcel-test-defspec (name &optional options prop)
  "Defines a new `ert' test var that uses `quick-check` to verify
  [property] with the given [args] (should be a sequence of generators),
  [default-times] times by default.  You can call the function defined as [name]
  with no arguments to trigger this test directly (i.e., without starting a
  wider cljs.test run), with a single argument that will override
  [default-times], or with a map containing any of the keys
  [:seed :max-size :num-tests]."

  (let ((property (or prop options))
	(options  (and prop options))
	(property-sym (make-symbol "property-sym")))
    
    
    `(progn
       (fset ',property-sym ,property)
       (put ',property-sym :name ',name)
       (ert-deftest ,name () ;(&optional times quick-check-options)
	 (let* ((options (tcel-test-process-options ,options))
		(times  (plist-get options :num-tests))
					;(seed (plist-get quick-check-options :seed))
		(seed 10)
					;(max-size (plist-get quick-check-options :max-size))
		(max-size 11))
	   
	   (funcall 'tcel-check-quick-check times ,property) ;quick-check-options)
	   ))
       
       (put ',name :defspec t)
       (put ',name :test (lambda () (tcel-test-assert-check (plist-put ',name :test-var ',name))))))) 
	;~(vary-meta name assoc
	;	    ::defspec true
;		    :test `#(cljs.test.check.cljs-test/assert-check
;			     (assoc (~name) :test-var (str '~name))))
;        ([] (let [options# (process-options ~options)]
;              (apply ~name (:num-tests options#) (apply concat options#))))
;        ([~'times & {:keys [~'seed ~'max-size] :as ~'quick-check-opts}]
;         (apply
;           cljs.test.check/quick-check
;           ~'times
;'           (vary-meta ~property assoc :name (str '~property))
;           (apply concat ~'quick-check-opts))))
;	(set ,name 


(provide 'tcel-test)
;;; tcel-test.el ends here

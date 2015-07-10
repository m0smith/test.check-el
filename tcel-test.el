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

;;; Code:


(defun tcel-test-error? (m)
  "Return true if the :result key of M (a plist) is an error. 
An error is a cons ( symbol . data ) where symbol will have a property 'error-conditions'.
See https://www.gnu.org/software/emacs/manual/html_node/elisp/Error-Symbols.html "
  (let ((result (plist-get m :result)))
    (and (consp result)
	 (get (car result) 'error-conditions))))

(defun tcel-tet-assert-check (m)
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


(provide 'tcel-test)
;;; tcel-test.el ends here

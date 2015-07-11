;;; properties.el --- Properties from ClojureScript test.check -*- lexical-binding: t; -*-

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

;; https://github.com/clojure/test.check/blob/master/src/main/clojure/cljs/test/check/properties.cljs

;;; Code:

(require 'tcel-generator)

(defun tcel-properties-apply-gen (fn1)
  (lambda (args)
    (let ((result (condition-case ex (apply fn1 args)
		    (error ex))))
      (list :result   result
	    :function fn1
	    :args     args))))

(defun tcel-properties-for-all*  (args fn2)
  "Creates a property (properties are also generators). A property
  is a generator that generates the result of applying the function
  under test with the realized arguments. Once realized, the arguments
  will be applied to `function` with `apply`.
  Example:
  (for-all* [gen/int gen/int] (fn [a b] (>= (+ a b) a)))
  "
  (tcel-generator-fmap
    (tcel-properties-apply-gen fn2)
    (apply 'tcel-generator-tuple args)))


(defmacro tcel-properties-for-all (bindings &rest body)
  "Macro sugar for `for-all*`. `for-all` lets you name the parameter
  and use them in expression, without wrapping them in a lambda. Like
  `for-all*`, it returns a property.
  Examples
  (tcel-properties-for-all (a tcel-generator-int b tcel-generator-int)
    (>= (+ a b) a))
  "
  
  `(tcel-properties-for-all*
    ,(cons 'list (plist-values bindings)) 
    (lambda ,(plist-keys bindings)
              (should ,@body))))

(provide 'tcel-properties)
;;; properties.el ends here


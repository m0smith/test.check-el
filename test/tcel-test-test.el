;;; tcel-test-test.el --- Tests for tcel-test

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


(require 'tcel-test)


(tcel-test-defspec default-trial-counts
		   (tcel-properties-for-all (a (tcel-generator-int)) 
					    (= a a)))
					      

(tcel-test-defspec default-trial-count-2 '(:num-tests 100)
		   (tcel-properties-for-all (a (tcel-generator-int) b (tcel-generator-int)) 
					     (= (+ b a) (+ a b))))


(provide 'tcel-test-test)
;;; tcel-test-test.el ends here

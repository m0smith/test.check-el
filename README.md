# test.check-el
Port of test.check from ClojureScript to EMACS Lisp

## Using with ERT
Use `tcel-test-defspec` like:
```
(tcel-test-defspec default-trial-counts
		   (tcel-properties-for-all (a (tcel-generator-int)) 
					    (= a a)))
```

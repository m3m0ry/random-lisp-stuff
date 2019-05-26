(in-package :nlp)

(defpackage :nlp-tests
  (:use :cl)
  (:export
   #:run-test-suit))

(in-package :nlp-tests)

(def-suite all-tests)

(in-suite all-tests)

(test nothing
      (is (= t t)))

(defun run-test-suit
    (fiveam:run! all-tests))

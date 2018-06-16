(in-package :calc)

(defpackage :calc-tests
  (:use :cl :fiveam)
  (:export
   #:run-test-suit))

(in-package :calc-tests)

(def-suite all-tests)

(in-suite all-tests)

(test addition
  (is (= 2 (calc:epel "1+1"))))

(test multiplication
  (is (= 4 (calc:epel "2*2"))))

(test factor
  (is (= 8 (calc:epel "2*(2+2)"))))


(defun run-test-suit
  (fiveam:run! all-tests))

(defpackage :poker
  (:use :cl :alexandria)
  (:export #:rank-hand #:print-hand #:mix :*deck* #:split #:draw #:holdem))


(in-package :poker)

(defvar *deck* (alexandria:map-product 'list (alexandria:iota 14 :start 2) '(c s h d)))

;;;UTILS
(defun split (l n)
  (values (subseq l 0 n) (subseq l n)))

;;TODO implement
(defmacro repeat (n f)
  (loop repeat n
        collect (funcall f)))

(defun print-hand (hand)
  (format t "~{~a~^ ~}" hand))

(defun mix ()
  (alexandria:shuffle (copy-list *deck*)))

(defun draw (n)
  (split *deck* n))

(defun holdem (n)
  (let ((*deck* (mix)))
    (values
     (repeat n (draw 5))
     (draw 3)
     (draw 1)
     (draw 1))))

(defun rank-hand ())

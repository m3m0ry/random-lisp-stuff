(in-package #:neuwerk)

(defun mat-vec-mul (A v)
  (β #'+
     (α #'* (reshape A (τ (i j) (j i)))
        v)))

(defun vec-add (v1 v2)
  (α #'+ v1 v2))

;;(defun mat-vec-mul2 (A v)
;;  (β #'+
;;     (reshape
;;      (α #'* A
;;         (reshape v (τ (i) (0 i))))
;;      (τ (i j) (j i)))))

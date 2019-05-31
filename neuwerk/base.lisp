(in-package #:neuwerk)

;; Non petalisp functions
(defun logistic-function (x)
  (/ 1 (1+ (exp (- x)))))


(defun logistic-function-deriv (x)
  (* (logistic-function x) (- 1 (logistic-function x))))






;; Petalisp functions
(defun delta-last (a z target &optional (func-deriv 'logistic-function-deriv))
  (α #'*
     (α #'- a target)
     (α func-deriv z)))

(defun delta (w-prev z delta-prev &optional (func-deriv 'logistic-function-deriv))
  (α #'*
     (α #'*
        (reshape w-prev (τ (i j) (j i)))
        delta-prev)
     (α func-deriv z)))

(defun delta-b (d)
  d)

(defun delta-weight (a-prev d)
  (α #'*
     (reshape a-prev (τ (i) (i 0)))
     (reshape d (τ (i) (0 i)))))

(defun mat-vec-mul (A v)
  (β #'+
     (α #'*
        (reshape A (τ (i j) (j i)))
        v)))

(defun vec-add (v1 v2)
  (α #'+ v1 v2))

(defun neuron-input (W a b)
  (vec-add
   (mat-vec-mul W a)
   b))

(defun neuron-output (z &optional (func 'logistic-function))
  (α func z))

(defun cost (a target)
  (α #'/
     (β #'+
        (α 'expt (α #'- target a) 2))
     2))

;;(defun mat-vec-mul2 (A v)
;;  (β #'+
;;     (reshape
;;      (α #'* A
;;         (reshape v (τ (i) (0 i))))
;;      (τ (i j) (j i)))))

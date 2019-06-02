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

(defun delta (w-next z delta-next &optional (func-deriv 'logistic-function-deriv))
  (α #'*
     (α #'*
        (reshape w-next (τ (i j) (j i)))
        delta-next)
     (α func-deriv z)))

(defun delta-b (d)
  d)

(defun delta-weight (a-prev d) ;; TODO check this
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

(defun neuron-input (W b a)
  (vec-add
   (mat-vec-mul W a)
   b))

(defun neuron-output (z &optional (func 'logistic-function))
  (α func z))

(defun feedforward-step (W b a &optional (func 'logistic-function))
  (let ((z (neuron-input W b a)))
    (values (neuron-output z func) z)))

(defun feedforward (weights biases input &optional (func 'logistic-function))
  (let ((as '()) (zs '()))
    (loop for W in weights
          for b in biases
          do (multiple-value-bind (input z) (feedforward-step W b input func)
               (push input as)
               (push z zs)))
    (values as zs)))

(defun backpropagation (weights biases x y)
  (multiple-value-bind (as zs) (feedforward weights biases x)
    (let ((ds (list (delta-last (last as) (last zs) y)))
          (dw '()) (db '()))
      (loop for W in (reverse weights)
            for b in (reverse biases)
            for a in (reverse (cons x as)) ;;TODO drop last?
            for z in (cdr (reverse zs)) ;;TODO with droping i might have one too little iterations
            do (push (delta W z (first ds)) ds))
      (format t "~a~%" ds)
      (format t "~a~%" as)
      (loop for a in (cons x as)
            for d in ds
            collect (delta-weight a d) into dw
            collect (delta-b d) into db)
      (values dw db))))

(defun gradient-descent (values grad eta)
  (α #'- values (α #'* eta grad)))

(defun learn (weights biases func eta)
  (loop for r to 1000
        for x = (random 1.0)
        do (multiple-value-bind
                 (dws dbs)
               (backpropagation weights biases (list x x) (list (funcall func x) (funcall func x)))
             (loop for dw in dws
                   for W in weights
                   for db in dbs
                   for b in biases
                   collect (compute (gradient-descent W dw eta)) into new-weights
                   collect (compute (gradient-descent b db eta)) into new-biases
                   finally (setf weights new-weights)
                   finally (setf biases new-biases)))
        do (format t "Step ~a~%" r))
  (values weights biases))

(defun cost (a target)
  (α #'/
     (β #'+
        (α 'expt (α #'- target a) 2))
     2))

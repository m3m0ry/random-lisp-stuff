;; Problem 1
(defun multiples-of-3-and-5 (below-n)
  (+ (sum-divisible-by-n below-n 3)
     (sum-divisible-by-n below-n 5)
     (- (sum-divisible-by-n below-n 15))))

(defun sum-divisible-by-n (target n)
  (let ((p (floor target n)))
    (floor (* n (* p (+ p 1))) 2)))


;; Problem 2
(defun even-fibonacci-numbers (upper-bound)
  (labels ((even-fibonacci (n)
             (cond
               ((= n 1) 0)
               ((= n 2) 2)
               ((> n 2)
                (+ (* 4 (even-fibonacci (- n 1)))
                   (even-fibonacci (- n 2))))
               (t 0)))
           (fibonacci-list (x)
             (let ((fib (even-fibonacci x)))
               (cond
                 ((<= fib upper-bound) (cons fib (fibonacci-list (+ x 1))))
                 (t nil)))))
    (apply '+ (fibonacci-list 1))))


;; Problem 3
(defun largest-prime-factor (x)
  (let ((max-factor -1)
        (primes '(2)))
    (loop while (/= x 1)
          do (multiple-value-bind (new-x success) (divide-n x (car primes))
               (setf x new-x) ; TODO better then this setf thing?
               (when success
                 (setf max-factor (car primes))))
             (push (next-prime primes) primes)
          finally (return max-factor))))

(defun divide-n (x k)
  (if (= (mod x k) 0)
      (values (divide-n (/ x k) k) t)
      (values x nil)))

(defun next-prime (primes)
  (labels ((prime-p (x primes)
             (cond ((null primes) x)
                   ((= (mod x (car primes)) 0) nil)
                   (t (prime-p x (cdr primes))))))
    (do ((next (1+ (reduce #'max primes)) (1+ next)))
        ((prime-p next primes) next))))

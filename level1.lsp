;; Problem 1
;; TODO tail-recursion
(defun multiples-of-3-and-5 (below-n)
  (labels ((get-multiples (n)
             (cond
               ((= n 0) nil)
               ((= (mod n 3) 0) (cons n (get-multiples (- n 1))))
               ((= (mod n 5) 0) (cons n (get-multiples (- n 1))))
               (t (get-multiples (- n 1))))))
    (reduce #'+ (get-multiples (- below-n 1)))))

;; Problem 2
;; TODO tail-recursion
(defun even-fibonacci-numbers (upper-bound)
  (labels ((fibonacci (n)
             (cond
               ((= n 1) 1)
               ((= n 2) 2)
               ((> n 2) (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
               (t 0)))
           (fibonacci-list (x)
             (let ((fib (fibonacci x)))
               (cond
                 ((<= fib upper-bound) (cons fib (fibonacci-list (+ x 1))))
                 (t nil)
                 ))))
    (apply '+ (remove-if-not 'evenp (fibonacci-list 1)))))

;; Problem 3
(defun largest-prime-factor (x)
  (let ((primes '(2)))
    (labels ((next-prime (primes)
	       (loop for candidate from (+ (car primes) 1)
		  do (dolist (prime primes)
		       when (not (= (mod candidate prime) 0))(return-from next-prime candidate)))))))
  (loop while (not(= x 1))
     do ((setq x (/ x 2))
     (print x)))
  )

(defun sieve (x)
  (let ((primes '(3 2)))
    (labels ((get-prime (i)
               (dolist (prime primes)
                 (when (= (mod i prime) 0) (return-from get-prime nil))
                     )
               i)
             (get-primes (y)
               (loop for test from 4 to y
                     do (let ((p (get-prime test))) 
                          (when p (push p primes)))
                     )
               )
             )
      (get-primes x)
      (return-from sieve primes)
      )))

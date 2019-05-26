(defpackage :calc
  (:use :cl :esrap)
  (:export #:lexer #:epel #:expression #:repl #:print-grammar))

(in-package :calc)

(defun interactive-interpreter (prompt transformer &optional (form "~a"))
  "Read an expression, transform it, and print the result."
  (loop
    (handler-case
        (progn
          (format t (if (stringp prompt) prompt (funcall prompt)))
          (format t (if (stringp form) form (funcall form))
                  (funcall transformer (read-line))))
      ;;In case of error, do this :
      (error (condition)
        (format t "~&;; Error~& ~a ~&"
                condition)))))

(defun form-generator (&optional (ctl-string "[~d]") (num 0))
  "Return a function that prints prompts like [1] . [2]. etc."
  #'(lambda () (format nil ctl-string (incf num))))

(defun repl ()
  (interactive-interpreter (form-generator "~~&In [~d]: ") #'epel
                           (form-generator "Out [~d]: ~~a")))

(defun epel (s)
  (unless (= (length (lexer s)) 0)
  (eval (parse 'expression (lexer s)))))

(defun lexer (s)
  (remove-if-not (lambda (c) (or (alphanumericp c) (find c ".*/+-()^"))) s))

(defrule expression (or add-sub term))
(defrule add-sub (and expression (or "+" "-") term)
  (:destructure (a op b)
    (list (intern op) a b)))

(defrule term (or mul-div factor))
(defrule mul-div (and term (or "*" "/") factor)
  (:destructure (a op b)
    (list (intern op) a b)))

(defrule factor (or pow mini))
(defrule pow (and factor "^" mini)
  (:destructure (a op b)
    (declare (ignore op))
    (list 'expt a b)))

(defrule mini (or brackets negative number))
(defrule brackets (and "(" expression ")")
  (:destructure (b1 e b2)
    (declare (ignore b1 b2))
    e))
(defrule negative (and "-" factor)
  (:destructure (minus f)
    (declare (ignore minus))
    (- f)))

(defrule number (or float integer))

(defrule nums (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defrule float (and (+ nums) "." (* nums))
  (:lambda (list)
   (with-input-from-string (f (text list))
     (read f))))

(defrule integer (and (+ nums))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defun print-grammar ()
 (describe-grammar 'expression))

;(defrule function )

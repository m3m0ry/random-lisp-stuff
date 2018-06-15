(require :esrap)

(defpackage :calc-grammar
  (:use :cl :esrap)
  (:export #:lexer #:epel #:expression))

(in-package :calc-grammar)

(defun epel (s)
  (eval (parse 'expression (lexer s))))

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

(defrule factor (or brackets negative number))
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

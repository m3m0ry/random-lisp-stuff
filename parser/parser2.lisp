(require :esrap)

(defpackage :calc-grammar
  (:use :cl :esrap)
  (:export #:calc #:lexer #:cepl #:float #:expression)
  )

(in-package :calc-grammar)

(defun cepl (s); todo better name
  (eval (parse 'calc (lexer s))))

(defun lexer (s)
  (remove-if-not (lambda (c) (or (alphanumericp c) (find c ".*/+-()^"))) s))

;;; Utility rules.

(defrule expression (or (and expression (or "+" "-") term) term))
(defrule term (or (and term (or "*" "/") factor) factor))
(defrule factor (or (and "(" expression ")") (and "-" factor) number))

;(defrule expression (or (and expression operator number) (and number operator number))
;  (:lambda (e)
;   (list (second e) (first e) (third e))))

;(defrule operator (or "*" "/" "+" "-" "^")
;  (:lambda (op)
;   (if (string-equal op "^") 'expt
;       (intern op))))

(defrule alphanumeric (alphanumericp character))

(defrule number (or float integer))

(defrule nums (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defrule float (and (+ nums) "." (* nums))
  (:lambda (list)
   (with-input-from-string (f (text list))
     (read f))))

(defrule integer (and (+ nums))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule symbol (not-integer (+ alphanumeric))
  (:lambda (list)
    (intern (text list))))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

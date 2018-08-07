(defpackage :poker
  (:use :cl :alexandria)
  (:export #:rank-holdem-hand #:rank-hand #:print-hand #:print-holdem #:mix
           :*deck* #:split #:draw #:holdem))


(in-package :poker)

(defvar *deck* (alexandria:map-product 'list (alexandria:iota 14 :start 2) '(c s h d)))

(defvar *poker-encoding* )

;;;UTILS
(defun split (l n)
  (values (subseq l 0 n) (subseq l n)))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun print-hand (hand)
  (format t "~{~{~d~a~}~^ ~}~%" hand))

(defun print-holdem (game)
  (format t "Hands:~%")
  (loop for hand in (first game)
        do (print-hand hand))
  (format t "Flop:~%")
  (print-hand (second game))
  (format t "Turn:~%")
  (print-hand (third game))
  (format t "River:~%")
  (print-hand (fourth game)))

(defun mix ()
  (alexandria:shuffle (copy-list *deck*)))

(defun draw (n)
  (multiple-value-bind (drawn new-deck) (split *deck* n)
    (setf *deck* new-deck)
    drawn))

(defun holdem (n)
  (let ((*deck* (mix)))
    (list
     (loop repeat n collect (draw 5))
     (draw 3)
     (draw 1)
     (draw 1))))

;;Straight flush + highcard
;;Four + fourvalue + lastcard
;;Full House + high + low
;;Flush + highcardS
;;Straight + highcard
;;Three + threevalue + highcards
;;Two pair + high + low + kicker
;;Pair + pairvalue + highcard
;;Highcard = Highcards

(defun rank-hand (hand)
  (apply '+ hand))

(defun rank-holdem-hand (hand board)
  (let ((possible-hands nil))
    (map-combinations (lambda (x) (push x possible-hands)) (append hand board) :length 5)
    (most #'rank-hand possible-hands)))


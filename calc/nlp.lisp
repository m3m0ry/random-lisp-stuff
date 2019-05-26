(defpackage :nlp
  (:use :cl :str)
  (:export #:min-edit-distance))

(in-package :nlp)

(defun sub-cost (a b)
  (if (char= a b) 0 2))

(defun min-edit-distance (source target)
  (aref (distance-matrix source target) (length source) (length target)))

(defun distance-matrix (source target &optional (del-cost 1) (ins-cost 1))
  (let* ((n (length source))
         (m (length target))
         (D (make-array (list (1+ n) (1+ m)))))

    (setf (aref D 0 0) 0)
    (loop for i from 1 to n do
      (setf (aref D i 0) (+ (aref D (1- i) 0) del-cost)))
    (loop for i from 1 to m do
      (setf (aref D 0 i) (+ (aref D 0 (1- i)) ins-cost)))

    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (setf (aref D (1+ i) (1+ j))
              (min (+ (aref D i (1+ j)) del-cost)
                   (+ (aref D (1+ i) j) ins-cost)
                   (+ (aref D i j) (sub-cost (char source i) (char target j)))))))
    D))

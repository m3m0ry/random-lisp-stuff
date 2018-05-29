(defpackage #:bl
  (:use #:cl #:alexandria)
  (:export #:test #:pattern-matching #:dna-complement #:pattern-count #:frequent-words
           #:clump-finding #:pattern-to-number #:number-to-pattern
           #:computing-frequencies #:skew #:minimum-skew #:hamming-distance
           #:approximate-pattern-matching #:approximate-pattern-count
           #:neighbors #:all-positions #:frequent-words-with-mismatch
           #:minimum #:occurences #:motif-enumeration
           ))

(in-package #:bl)

;; Lecture 3

(defun motif-enumeration (dnas k d)
  (let ((patterns '())
        (final-patterns '()))
    (loop for dna in dnas
          do (loop for i from 0 to (- (length dna) k)
                   do (setf patterns (union  (neighbors (subseq dna i (+ i k)) d) patterns))))
    (loop for pattern in patterns
          when (every 'plusp (mapcar (lambda (dna) (approximate-pattern-count pattern dna d)) dnas))
             do (setf final-patterns (adjoin pattern final-patterns)))
    final-patterns))


(defun occurences (k n l)
  "How often does a k-mer occure in n random DNAs of length l"
  (* (/ 1 (expt 4 k)) (1+ (- l k)) n))


;; Lectures 1&2

(defun frequent-words-with-mismatch (dna k d &optional (times 1) (rev nil))
  (let ((neighborhoods nil)
        (table (make-hash-table :test #'equal)))
    (loop for i from 0 to (- (length dna) k)
          do (push (append (neighbors (subseq dna i (+ i k)) d) (when rev (neighbors (dna-complement (subseq dna i (+ i k))) d))) neighborhoods))
    (loop for hood in neighborhoods
          do (loop for neighbor in hood 
                   do (incf (gethash neighbor table 0))))
    (sort (remove-if (lambda (a) (> times (cdr a))) (alexandria:hash-table-alist table)) (lambda (a b) (> (cdr a) (cdr b))))))

(defun approximate-pattern-count (pattern dna d)
  (loop for x from 0 to (- (length dna) (length pattern))
        count (<= (hamming-distance pattern (subseq dna x (+ x (length pattern)))) d)))

(defun approximate-pattern-matching (pattern dna d)
  (loop for x from 0 to (- (length dna) (length pattern))
        when (<= (hamming-distance pattern (subseq dna x (+ x (length pattern)))) d) collect x))

(defun hamming-distance (p q)
  (loop for i across p
        for j across q
        with x = 0
        unless (equal i j) do (incf x)
        finally (return x)))

(defun minimum (l)
  (loop for x in l with m = (car l)
        when (< x m) do (setf m x)
        finally (return m)))

(defun minimum-skew (dna) 
  (let ((sk (skew dna)))
    (all-positions (minimum sk) sk)))

(defun all-positions (needle haystack)
  (loop
    for element in haystack 
    and position from 0
     when (eql element needle)
      collect position))

(defun skew (dna)
  (loop for n across dna
        with x = 0
        do
        (case n
          (#\C (decf x))
          (#\G (incf x)))
        collect x into sk 
        finally (return (cons 0 sk))))

(defun bases ()
  '("A" "C" "G" "T"))

(defun neighbors (pattern d)
  (when (= d 0) (return-from neighbors (list pattern)))
  (when (= (length pattern) 1) (return-from neighbors (bases)))
  (let ((neighborhood '())
        (suffix-neighborhood (neighbors (subseq pattern 1) d)))
    (loop for p in suffix-neighborhood
          if (< (hamming-distance (subseq pattern 1) p) d)
            do (loop for x in (bases)
                   do (push (concatenate 'string x p) neighborhood))
          else
            do (push (concatenate 'string (subseq pattern 0 1) p) neighborhood))
    (return-from neighbors neighborhood)))


(defun computing-frequencies (dna k)
  (let ((freq (make-array (expt 4 k) :initial-element 0)))
    (loop for x from 0 to (- (length dna) k)
          do
          (incf (aref freq (pattern-to-number (subseq dna x (+ x k)))))
          finally (return freq))))

(defun number-to-pattern (n width)
  (flet ((convert (c)
           (case c
             (#\0 #\A)
             (#\1 #\C)
             (#\2 #\G)
             (#\3 #\T))))
    (format nil "~v,,,'A@A" width (coerce (mapcar #'convert (coerce (write-to-string n :base 4) 'list)) 'string))))

(defun pattern-to-number (pattern)
  (flet ((convert (c)
           (case c
             (#\A #\0)
             (#\C #\1)
             (#\G #\2)
             (#\T #\3))))
    (parse-integer (coerce (mapcar #'convert (coerce pattern 'list)) 'string) :radix 4)))

(defun clump-finding (dna k L times)
  (loop for x from 0 to (- (length dna) L)
        with words = '()
        do
        (setf words (union (mapcar #'car (frequent-words (subseq dna x 
                                                                 (if (> (length dna)  (+ x L)) 
                                                                     (+ x L)
                                                                     (length dna)))
                                                         k times))
                           words :test #'equal)) 
        finally (return words)))

(defun test (input)
  (with-open-file (stream-in input );:direction :output)
    (format t "~A" (frequent-words-with-mismatch (subseq  (let ((data (make-string (file-length stream-in))))
                                                            (read-sequence data stream-in)
                                                            data) (-  3764857 500) (+  3764857 500))
                                                          9 1 6 t))))

(defun pattern-matching (pattern dna)
  (loop for x = (search pattern dna) then (search pattern dna :start2 x)
        when (or (not x) (< (- (length dna) (length pattern)) x)) return match
        collect x into match
        do (incf x)))

(defun nucleotide-complement (nucleotide)
  (cond
    ((char= nucleotide #\G) #\C)
    ((char= nucleotide #\C) #\G)
    ((char= nucleotide #\T) #\A)
    ((char= nucleotide #\A) #\T)
    (t (error "~A not a valide nucldeotide" nucleotide))))

(defun dna-complement (dna)
  (reverse (map 'string #'nucleotide-complement dna)))

(defun pattern-count (text pattern)
  (loop for i from 0 to (- (length text) (length pattern))
        counting (string= (subseq text i (+ i (length pattern))) pattern)))

(defun frequent-words (text k times)
  (let ((alist (loop for i from 0 to (- (length text) k)
                     with table = (make-hash-table :test #'equal)
                     do (incf (gethash (subseq text i (+ i k)) table 0))
                     finally (return (sort (alexandria:hash-table-alist table) (lambda (a b) (> (cdr a) (cdr b))))))))
    (remove-if (lambda (a) (> times (cdr a))) alist)))

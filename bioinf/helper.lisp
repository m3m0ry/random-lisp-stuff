(defpackage #:bl
  (:use :cl :alexandria)
  (:export #:test #:pattern-matching #:dna-complement #:pattern-count #:frequent-words
           #:clump-finding #:pattern-to-number #:number-to-pattern
           #:computing-frequencies #:skew #:minimum-skew #:hamming-distance
           #:approximate-pattern-matching #:approximate-pattern-count
           #:neighbors #:all-positions #:frequent-words-with-mismatch
           #:minimum #:occurences #:motif-enumeration #:distance-pattern-strings
           #:median-string #:pr #:profile-most-portable #:profile-matrix
           #:greedy-motif-search #:find-consensus #:score
           #:transponate #:random-motif-search #:random-motif
           #:repeated-random-motif-search #:probability-lecture-4
           #:wierd-random #:gibbs-sampler #:repeated-random-gibbs-search
           ))

(in-package #:bl)
(setf *print-case* :downcase)


;; Lecture 4

(defun repeated-random-gibbs-search (dnas k N)
  (loop for i to 200
        for candidate = (gibbs-sampler dnas k N)
        with best = nil
        when (< (score candidate) (score best)) do (setf best candidate)
        do (format t "Iteration:~d~tBest score:~d~&" i (score best))
        finally (return best)))

(defun gibbs-sampler (dnas k N)
  (loop repeat N
    for i = (random (length dnas))
    for dna = (elt dnas i)
    with motifs = (repeated-random-motif-search dnas k)
    with best-motifs = motifs
    for r = (profile-randomly dna k (profile-matrix (remove (elt motifs i) motifs :test 'equal)))
    do (setf (elt motifs i) (subseq dna r (+ r k))) 
    when (< (score motifs) (score best-motifs)) do (setf best-motifs motifs)
    finally (return best-motifs)))

(defun profile-randomly (dna k profile)
  (wierd-random (loop for i from 0 to (- (length dna) k)
                      for new-consensus = (subseq dna i (+ i k))
                      for new-score = (pr new-consensus profile)
                      collect new-score
                      )))

(defun wierd-random (probabilities)
  (loop for p in (mapcar (lambda (x) (/ x (apply '+ probabilities))) probabilities)
        for i = 0 then (1+ i) and sum = p then (+ sum p)
        with r = (random 1.0)
        when (> sum r) do (return i)))

(defun probability-lecture-4 (k l times)
  (- 1 (expt (/ (- l k) (1+ (- l k))) times)))

(defun repeated-random-motif-search (dnas k)
  (loop repeat 10
        for candidate = (random-motif-search dnas k)
        with best = nil
        when (< (score candidate) (score best)) do (setf best candidate)
        finally (return best)))

(defun random-motif-search (dnas k)
  (loop 
        with motifs = (random-motif dnas k)
        with best-motifs = motifs
        do (setf motifs (loop for dna in dnas
                            collect (profile-most-portable dna k (profile-matrix motifs))))
        if (< (score motifs) (score best-motifs)) do (setf best-motifs motifs)
        else do (return best-motifs)))

(defun random-motif (dnas k)
  (loop for dna in dnas
        for r = (random (- (length dna) k))
        collect (subseq dna r (+ r k))))

;; Lecture 3

(defun greedy-motif-search (dnas k)
  (loop for i from 0 to (- (length (car dnas)) k)
        for motifs = (list (subseq (car dnas) i (+ i k)))
        with best-motifs = (mapcar (lambda (x) (subseq x 0 k)) dnas)
        do (loop for dna in (cdr dnas)
                 do (push (profile-most-portable dna k (profile-matrix motifs)) motifs))
        when (< (score motifs) (score best-motifs)) do (setf best-motifs motifs)
        finally (return best-motifs)))

(defun score (motifs)
  (if (not motifs) 999999999999999999
      (loop for motif in motifs
            with consensus = (find-consensus motifs) and score = 0
            do (incf score (hamming-distance consensus motif))
            finally (return score) )))

(defun find-consensus (motifs)
  (coerce (loop for column in (transponate (profile-matrix motifs))
                                 for m = (apply 'max column)
                                 collect (cond
                                           ((= m (first column)) #\A)
                                           (( = m ( second column)) #\C)
                                           (( = m ( third column)) #\G)
                                           (( = m ( fourth column)) #\T)
                                           )) 'string) )

(defun transponate (motifs)
  (loop for motif in motifs
        collect (car motif) into next
        collect (cdr motif) into m
        finally (if (car m) (return (cons next (transponate m)))
                    (return (list next))
                    )))


(defun profile-matrix (motifs)
  (labels ((profile-it (base profile motifs)
             (if (= 0 (length (car motifs))) (return-from profile-it profile)
                 (profile-it base (cons (/ (reduce (lambda (x y) (+ x (if (equal base (char y 0)) 1 0)))
                                                   motifs :initial-value 1) (1+ (length motifs))) profile)
                                                   ;initial value 1 and 1+ on length determines pseudocount
                             (mapcar (lambda (x) (subseq x 1)) motifs)))))
    (list (reverse (profile-it #\A '() motifs))
          (reverse (profile-it #\C '() motifs))
          (reverse (profile-it #\G '() motifs))
          (reverse (profile-it #\T '() motifs))
          )))

(defun profile-most-portable (dna k profile)
  (loop for i from 0 to (- (length dna) k)
        for new-consensus = (subseq dna i (+ i k))
        for new-score = (pr new-consensus profile)
        with best-consensus = (subseq dna 0 k) and best-score = 0
        when (> new-score best-score) do (setf best-score new-score) (setf best-consensus new-consensus)
        finally (return best-consensus)))

(defun pr (consensus profile)
  (loop for x across consensus
        for i from 0 to (length consensus)
        with prod = 1
        do (case x
             (#\A (setf prod (* prod (elt (first profile) i))))
             (#\C (setf prod (* prod (elt (second profile) i))))
             (#\G (setf prod (* prod (elt (third profile) i))))
             (#\T (setf prod (* prod (elt (fourth profile) i)))))
        finally (return prod)))


(defun median-string (dnas k)
    (loop for i from 0 to (1- (expt 4 k))
          for new-pattern = (number-to-pattern i k)
          for new-distance = (distance-pattern-strings new-pattern dnas)
          with distance = (* k (length dnas)) and pattern = '()
          when (< new-distance distance) do (setf pattern new-pattern) (setf distance new-distance)
          finally (return pattern)))

(defun distance-pattern-string (pattern dna)
  (loop for i from 0 to (- (length dna) (length pattern))
        minimizing (hamming-distance pattern (subseq dna i (+ i (length pattern))))))

(defun distance-pattern-strings (pattern dnas)
  (loop for dna in dnas
        summing (distance-pattern-string pattern dna)))

(defun motif-enumeration (dnas k d)
  (let ((patterns '())
        (final-patterns '()))
    (loop for dna in dnas
          do (loop for i from 0 to (- (length dna) k)
                   do (setf patterns (union (neighbors (subseq dna i (+ i k)) d) patterns :test 'equal))))
    (loop for pattern in patterns
          when (every 'plusp (mapcar (lambda (dna) (approximate-pattern-count pattern dna d)) dnas))
             do (setf final-patterns (adjoin pattern final-patterns :test 'equal)))
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

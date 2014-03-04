(ql:quickload "alexandria")

(defvar trig-number 
  (alexandria:alist-hash-table 
    '(
      (1 . #( 75 ))                                                   
      (2 . #(95 64))
      (3 . #(17 47 82))
      (4 . #(18 35 87 10))
      (5 . #(20 04 82 47 65))
      (6 . #(19 01 23 75 03 34))
      (7 . #(88 02 77 73 07 63 67))
      (8 . #(99 65 04 28 06 16 70 92))
      (9 . #(41 41 26 56 83 40 80 70 33))
      (10 . #(41 48 72 33 47 32 37 16 94 29))
      (11 . #(53 71 44 65 25 43 91 52 97 51 14))
      (12 . #(70 11 33 28 77 73 17 78 39 68 17 57))
      (13 . #(91 71 52 38 17 14 91 43 58 50 27 29 48))
      (14 . #(63 66 04 68 89 53 67 30 73 16 69 87 40 31))
      (15 . #(04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))
      )))

(defvar trig-max-path (make-hash-table))
(defun init-trig-max-path (m)
 (loop
   for i from 1 to m
   for x = (make-hash-table)
   do
   (setf (gethash 'len x) 0)
   (setf (gethash 'path x) '())
   (setf (gethash i trig-max-path) (make-array i :initial-element x))
  )   )

(defun get-trig-number (i j) (aref (gethash i trig-number) j))
(defun get-max-path-len (i j) (gethash 'len (aref (gethash i trig-max-path) j) ) )
(defun get-max-path (i j) (gethash 'path  (aref (gethash i trig-max-path) j) ) )
(defun find-max-path (i j)
  (let ((tj 0) (ti (- i 1)) (pv (make-hash-table)) (p '()) (n (get-trig-number i j)) )

    (if (> (get-max-path-len i j) 0) (return (get-max-path i j)))
    (if (or (= i 1) (= j 0)) (setf tj 0)
      (if (= j ti) (setf tj (- j 1))
        (setf tj (if (> 
                       (get-max-path-len ti (- j 1)) 
                       (get-max-path-len ti j)) (- j 1) j)) 
        ))
    (if (= i 1) (setf ti 1))
    
    
    (setf (gethash 'len pv) (+ (get-max-path-len ti tj) n))
    (setf p (get-max-path ti tj))
    (push n p)
    (setf (gethash 'path pv) p)
    (setf (aref (gethash i trig-max-path) j) pv) 

    (block nil (return p)) 
    ))

(defun main-max-path (m)
  (let ((p '()) (len 0))
    (init-trig-max-path m)
    (loop
      for i from 1 to m
      do
      (loop
        for j from 0 to (- i 1)
        for plen = 0
        for tp = '()
        do
        (setf tp (find-max-path i j)) 
        (setf plen (get-max-path-len i j))
        (if (> plen len) (progn (setf len plen) (setf p tp)) )
        )
      ) 
    (print p)
    (block nil (return (reduce '+ p)))
    ))

(print  (main-max-path 15))

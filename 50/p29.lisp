(ql:quickload "alexandria")

(defun calc-expt-count (visited a n m)
  (let ((h (make-hash-table)) (i 1))
    (loop 
      for x = (expt a i)
      while (<= x n)
      do
      (loop
        for j from 2 to m
        for k = (* i j)
        do
        (setf (gethash k h) 1)
        )
      (incf i)
      (setf (gethash x visited) 1))
    (block nil (return (length (alexandria:hash-table-keys h)))) 
    ))


(defun sum-expt-count (n m)
  (let ( (visited (make-hash-table)) (sum 0))
  (loop
    for a from 2 to n
    do
    (if (null (gethash a visited) )
      (setf sum (+ sum (calc-expt-count visited a n m)))
      )
    )
  (print sum)
  (block nil (return sum))
   )
  )

(sum-expt-count 100 100)

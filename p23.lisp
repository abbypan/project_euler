(ql:quickload "alexandria")

(defun divisor-seq-n (n)
  (let (( h (make-hash-table)))
    (loop 
      for i from 1 to n
      do 
      (setf (gethash i h) '())
      )
    (loop
      for i from 1 to n
      do
      (loop 
        for j from (+ i i) to n by i
        do
        (if (<= j n) (push i (gethash j h) )
          )))
    (remhash 1 h)
    (maphash (lambda (k v) (setf (gethash k h) (reduce '+ v))) h)
    (block nil (return h))))

(defun abundant-seq-n (n)
  (let ((h (divisor-seq-n n)) (s '()))
    (maphash (lambda (k v) (if (< k v) (push k s))) h)
    (block nil (return (sort s '<)))
    ))

(defun no-abundant-number-sum (n)
  (let ((s (abundant-seq-n n)) (sum (/ (* n (+ n 1)) 2)) (v 0) (ts (make-hash-table)))
    (loop
      while (> (length s) 1)
      do
      (setf v (car s))
      (mapcar 
        (lambda (x) (if (<= (+ v x) n) (setf (gethash (+ v x) ts) 1))) s)
      (pop s)
      )
    (setf v (reduce '+ (alexandria:hash-table-keys ts)))
    (setf sum (- sum v))
    (block nil (return sum))))

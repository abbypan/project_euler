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

(defun get-amicable (n)
 (let ((s (divisor-seq-n n)) (m '()))
  (loop 
   for i from 2 to n
   for v = (gethash i s)
   for p = (if (and (> v 1) (<= v n)) (gethash v s) 0)
   do
   (if (and (= i p) (not (= v p))) (push i m))
  )
  (block nil (return (reduce '+ m)))))

(get-amicable 10000)

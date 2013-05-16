
(defun canceling-fractions ()
  (let ((u 1) (d 1))
    (loop for a from 1 upto 9
	do
	(loop for b from (+ a 1) upto 9
	do
	(loop 
	 for n from 1 upto 9
	 for k = (* b (+ n (* 10 a)))
	 for v = (* a (+ b (* 10 n)))
	do
	 (if (= k v) (progn
(setf u (* u a))
(setf d (* d b))

)))))
(print u)
(print d)
(block nil (return (/ d (gcd d u))))))


(ql:quickload "alexandria")

(defun diagonal-num (n) (if (= n 0) 0 (expt (- (* 2 n) 1) 2)))

(defun diagonal-sum (m)
   (let ((s 1) (n (/ (+ m 1) 2)))
     (loop 
	for i from 2 upto n
	for e = (diagonal-num i)
	for j = (- i 1)
	for b = (* 3 (diagonal-num j))
	for u = (* 12 j)
	do 
	(setf s (+ s e b u)))
	(block nil (return s))))

(diagonal-sum 1001)

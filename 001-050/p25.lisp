(defun fib-n-digit (n)
   (let ((s 1) (r 1) (m (expt 10 (- n 1))) (v 0) (i 2))
     (loop 
      while (< r m)
      do 
      (setf v (+ s r))
      (setf s r)
      (setf r v)
      (incf i)
	)
(block nil (return i))))

(fib-n-digit 1000)

(defun get-champernowne-number (m)
  (let ((i 1))
    (loop
      for j = (floor (log i 10)) 
      for x = (+ 1 j)
      while (> m x)
      do
      (setf m (- m x))
      (incf i)
      )

    (loop 
      for j = (expt 10 (floor (log i 10))) 
      for v = (floor (/ i j))
      do
      (if (= m 1) (return-from get-champernowne-number v))
      (decf m)
      (setf i (- i j))
      )

    (block nil (return 0))))

(print 
(reduce '* (mapcar 'get-champernowne-number 
                   '(1 10 100 1000 10000 100000 1000000)))
)
      

(defun calc-next-convergents (x y)
  (let ((ny (+ x y))
        (nx (+ x y y)))
    (setf  g (gcd nx ny))
    (setf nx (/ nx g))
    (setf ny (/ ny g))
    (block nil (return (values nx ny)))))


(defun main-root-convergents (n)
  (let ((m 0) (x 1) (y 1))
    (loop for i from 1 to n
          do
          (setf (values x y) (calc-next-convergents x y))
          (if (> x 
                 (expt 10 (+ 1 (floor (log y 10))) )) 
            (incf m)))
    (block nil (return m))))

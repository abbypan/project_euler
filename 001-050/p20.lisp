(ql:quickload "alexandria")

(defun fac (n) (if (= n 1) 1 (* n (fac (- n 1)))))

(defun destruct-n (n)
   (let ((s 0))
 (loop 
while (> n 0)
   do
   (setf s (+ s (mod n 10)))
   (setf n (floor (/ n 10)))
   )
   (block nil (return s))
)) 

(destruct-n (fac 100))
   

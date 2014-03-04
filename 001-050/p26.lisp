(defun calc-cycle-length (n)
  (let ((v 10) (h (make-hash-table)) (i 1))
    (loop while (< v n)
          do
          (setf v (* 10 v)))
    (setf v (mod v n))

    (loop for j = (gethash v h)
          while (> (mod v n) 0)
          do
          (if (null j) (setf (gethash v h) i) 
            (return-from calc-cycle-length (- i j)))
          (incf i)
          (setf v (mod (* 10 v) n)))
    (block nil	(return 0))
    ))

(defun max-cycle-length (n)
  (let ((s 0) (m 0))
    (loop 
      for i from 2 to n 
      for j = (calc-cycle-length i)
      do 
      (if (> j m) (setf m j s i)))
    (block nil (return s))))

(max-cycle-length 1000)

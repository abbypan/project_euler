(defun sumsqu (n)
  (let ((s 0))
    (loop
      for i from 1 to n
      do
      (setf s (+ s (* i i)))
      )
    (block nil (return s))
    ))

(defun squsum (n)
  (let ((s 0))
    (loop
      for i from 1 to n
      do
      (setf s (+ s i))
      )
    (block nil (return (* s s)))
    ))

(defun squdelta (n)
  (let ((s (- (squsum n) (sumsqu n))))
    (block nil (return s))
    ))

(squdelta 100) 


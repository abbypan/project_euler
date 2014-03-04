(defun main-calc-ms (d)
  (let ((s 28433) (m 7830457) (n (expt 10 d)))
    (loop
      for i from 1 to m
      do
      (setf s (mod (* s 2) n))
      )
    (setf s (mod (+ s 1) n))
    (block nil (return s))))

(defun destruct-sum (n)
  (let ((s (list 0)))
    (loop
      while (> n 0)
      do
      (push (mod n 10) (cdr (last s)))
      (setf n (floor (/ n 10))))
    (reduce '+ s)
    )
  )

(destruct-sum (expt 2 1000))


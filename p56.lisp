(defun destruct-n (n)
  (let ((nlist nil)) 
    (loop for x = (mod n 10)
      while (> n 0) 
      do
      (push x nlist)
      (setf n (floor (/ n 10))))
    (block nil (return nlist))))

(defun calc-digit-sum (n) (reduce '+ (destruct-n n)) )

(defun powerful-digit-sum (n)
  (let ((m 0))
    (loop
      for a from 1 to n
      do
      (loop
      for b from 1 to n
      for c = (expt a b)
      for s = (calc-digit-sum c)
      do
      (if (> s m) (setf m s))
        )
      )
    (block nil (return m))
    )
  )

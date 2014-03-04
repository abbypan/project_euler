(defun isprimeto (m s)
 (notany #'(lambda (n) (= (mod m n) 0)) s)
 )

(defun nprime (s)
  (let ((m (car (last s))))
    (loop
      do
      (when (isprimeto m s) (return m))
      (incf m))))

(defun nthprime (n)
  (let ((s (list 2)) (m (- n 1)))
    (loop
      for i from 1 to m
      for p = (nprime s)
      do
       (push p (cdr (last s)))
        )
    (block nil (return (car (last s))))
    ))

(nthprime 10001)


(defun maxgy (m n)
  (let ((x (max m n)) (y (min m n)))
    (loop
      (when (or (= y 1) (= (mod x y) 0)) (return y))
      (progn (let ((k (- x y)))
               (setf x (max y k))
               (setf y (min y k))
               )))))

(defun mingb (m n)
  (let ((x (/ (* m n) (maxgy m n))))
    (block nil (return x))
    ))

(defun smult (n)
  (let ((s 1))
    (loop
      for i from 2 to n
      do
      (setf s (mingb s i))
      )
    (block nil (return s))
    ))

(smult 20)


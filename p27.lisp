(defun is-prime (n) 
  (progn
    (if (< n 2) (return-from is-prime 0))
    (loop
      for i from 2 to (floor (sqrt n) )
      do 
      (if (= (mod n i) 0) (return-from is-prime 0))
      )
    (block nil (return 1))
    ))

(defun check-prime-length (a b)
  (let ((n 0))
    (loop
      for v = (+ (expt n 2) (* a n) b)
      while (= (is-prime v) 1)
      do
      (incf n)
      )
    (block nil (return n))))

(defun quadratic-prime (n)
  (let ((max-a 0) (max-b 0) (max-len 0))
    (loop
      for bi from 2 upto n
      do
      (if (= (is-prime bi) 1) 
        (loop
          for ai from (* -1 n) upto n
          for len = (check-prime-length ai bi)
          do
          (if (> len max-len) 
            (progn (setf max-a ai) (setf max-b bi) (setf max-len len)))
          )))
    (print max-a)
    (print max-b)
    (print max-len)
    (block nil (return (* max-a max-b)))))

(quadratic-prime 1000)

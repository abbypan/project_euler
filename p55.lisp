(ql:quickload "alexandria")
(defun reverse-n (n)
  (let ((m 0))
    (loop
    for x = (mod n 10)
    while (> n 0) 
    do
    (setf m (+ x (* 10 m)))
    (setf n (floor (/ n 10)))
     )
    (block nil (return m)) 
    )
  )

(defun is-palindromic (n) (= n (reverse-n n)) )

(defun is-lychrel (n)
  (let ((x n))
    (loop
      for i from 1 to 50
      do
      (setf x (+ x (reverse-n x)))
      (if (is-palindromic x) (return-from is-lychrel))
      )
    (block nil (return t))
   )
  )

(print (count-if #'is-lychrel (alexandria:iota 10000 :start 1))) 

(defun is-prime (n) 
  (progn
    (if (< n 2) (return-from is-prime 0))
    (loop
      for i from 2 to (floor (sqrt n) )
      do 
      (if (= (mod n i) 0) (return-from is-prime )))
    (block nil (return t))))

(defun next-spiral (x n)
  (let ((s (- n 1)) (prime-n 0))
    (loop for i from 1 to 4
          for j = (+ x (* i s))
          do
          (if (is-prime j) (incf prime-n)))
    (block nil (return prime-n))))

(defun main-spiral-prime (rate)
  (let ((prime-number 0) (sprial-number 1) (sprial 1) (n 3))
    (loop for prime-n = (next-spiral sprial n) 
          do
          (setf prime-number (+ prime-number prime-n))
          (setf sprial-number (+ 4 sprial-number))
          (setf r (/ prime-number sprial-number))
          (setf sprial (+ sprial (* 4 (- n 1))))
          (if (< r rate) (return))
          (setf n (+ 2 n)))
    (block nil (return n))))

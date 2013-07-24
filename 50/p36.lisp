(defun reverse-m (n m)
  (let ((i 0))
    (loop
      (when (= n 0) (return i))
      (progn (setf i (+ (mod n m) (* m i)))
             (setf n (floor (/ n m))))
      )))

(defun is-m-palindromic (n m)
  (let ((s (reverse-m n m)))
    (if (= s n) t)
    ))

(defun main-double-base-palindromes (n)
  (let ((s 0))
    (loop
      for i from 1 to n
      do
      (if (and (is-m-palindromic i 10) (is-m-palindromic i 2)) (setf s (+ s i))))
     (block nil (return s))))

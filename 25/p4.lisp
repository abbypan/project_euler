(defun rev10 (n)
  (let ((i 0))
    (loop
      (when (= n 0) (return i))
      (progn (setf i (+ (mod n 10) (* 10 i)))
             (setf n (floor (/ n 10))))
      )))

(defun palindromic (n)
  (let ((m (rev10 n)))
    (if (= m n) t)
    ))

(defun maxpal (k)
  (let ((m 1))
    (loop
      for x downfrom k to 1
      do
      (loop
        for y downfrom x to 1
        for n = (* x y)
        do
        (if (and (> n m) (palindromic n))
          (setf m n))
        ))
    (block nil (return m))
    ))

(maxpal 999) 


(defun mfac (n i j)
    (loop
      (when (or (< n i) (< n j))
      (return i))
      (if (= (mod n j) 0)
        (progn (setf i j) (setf n (/ n j)))
        (incf j)
      )
      )
    )

(mfac 600851475143 1 2)


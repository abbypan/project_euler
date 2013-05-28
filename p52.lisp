(defun destruct-n (n)
  (let ((v (list)))
    (loop while (> n 0)
          do
          (setf d (mod n 10))
          (push d v)
          (setf n (floor (/ n 10))))
    (block nil (return (sort v #'<)))))

(defun check-permute-n (n x)
  (let ((nlist (destruct-n n)))
    (loop
      for i from 2 to x
      for ilist = (destruct-n (* i n))
      do
      (if (not (equal ilist nlist)) 
        (return-from check-permute-n)))
    (block nil (return t))))

(defun main-permute-n (x)
  (let ((i 1))
    (loop
      while (> i 0)
      do
      (if (check-permute-n i x) 
        (return-from main-permute-n i))
      (incf i))
    (block nil (return 0))))

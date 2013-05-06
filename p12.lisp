(defun destruct (n)
  (let ((s (make-hash-table)))
    (loop
      for i from 2 to n
      for j = 0
      do
      (progn
      (loop while (= (mod n i) 0)
          do
          (setf n (/ n i))
          (incf j)
          )
      (if (> j 0) (setf (gethash i s) j))
      (when (= n 1) (return s))
    ))
    ))

(defun destruct-count (n)
  (let ((s (destruct n)) (c 1))
    (maphash (lambda (k v) (setf c (* c (+ v 1)))) s)
    c
    ))

(defun destruct-n (m)
  (let ((i 2))
    (loop for n = (/ (* i (+ i 1)) 2)
          for c = (destruct-count n)
     while t
    do
    (if (> c m) (return n) (incf i))
    )))

(destruct-n 500) 


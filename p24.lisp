(ql:quickload "alexandria")

(defun number-n (n) 
  (let ((s (make-hash-table)))
    (loop
      for i from 0 to (- n 1) 
      do 
      (setf (gethash i s) 1)
      )
    (block nil (return s))))

(defun select-m (s m)
  (let ((i 0) (r -1))
    (loop 
      for j from 0 to 9
      for v =(gethash j s)
      do
      (if (not (null v)) (incf i))
      (if (= i m) (progn (remhash j s) (setf r j) (return))) 
      )
    (block nil (return r))))

(defun get-lexicographic-permutation (n)
  (let ((nhash (number-n 10)) (v 0))
    (decf n)
    (loop
      for i from 1 to 10
      for j = (- 10 i)
      for f = (alexandria:factorial j)
      for m = (floor (/ n f))
      for r = (select-m nhash (+ 1 m))
      do
      (setf v (+ r (* 10 v)))
      (setf n (mod n f)))
    (block nil (return  v))))

(get-lexicographic-permutation 1000000)

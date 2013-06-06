

(ql:quickload "alexandria")

(defun destruct-n (n)
  (let ((nlist nil)) 
    (loop for x = (mod n 10)
          while (> n 0) 
          do
          (push x nlist)
          (setf n (floor (/ n 10))))
    (block nil (return nlist))))

(defun calc-max-nlist (nlist)
  (let ((v (sort nlist #'>)) (s 0))
    (loop while (not (null v))
          do
          (setf w (pop v))
          (setf s (+ (* 10 s) w))
          )
    (block nil (return s)) 
    )
  )


(defun main-cubic-permutations (n)
  (let ((h (make-hash-table)))
    (loop
      for i from 1
      for j = (expt i 3)
      for k = (calc-max-nlist (destruct-n j))
      for m = (gethash k h)
      do
      (if (null m) (setf m nil))
      (push j m)
      (setf mlen (length m))

      (loop for vk in (alexandria:hash-table-keys h)
            do
            (if (and (>= k vk) (= mlen n))
              (return-from main-cubic-permutations 
                           (car (last (gethash k h))))))
      (setf (gethash k h) m)
      (incf i))))

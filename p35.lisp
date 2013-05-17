(ql:quickload "alexandria")

(defun get-prime-hash (m)
  (let ((s (make-array (+ m 1) :initial-element 1)) (h (make-hash-table)))
    (loop
      for i from 2 to m
      for v = (aref s i)
      do
      (if (> v 0) (progn
                    (loop
                      for j from (+ i i) to m by i
                      do
                      (setf (aref s j) 0))
                    (setf (gethash i h) i)
                    )))
    (block nil (return h))
    ))

(defun get-cycle-number (n)
  (let ((d (mod n 10)) (s (floor (/ n 10))) (g (floor (log n 10))))
    (block nil (return (+ s (* d (expt 10 g)))))))

(defun get-cycle-number-list (n)
  (let ((v (list n)) (c (floor (log n 10)) ) (j n))
    (loop 
      for i from 1 to c
      for k = (get-cycle-number j)
      do
      (push k v)
      (setf j k)
      )
    (block nil (return v))))

(defun check-cycle-number (h n)
  (let ((s (get-cycle-number-list n)))
    (every (lambda (x) (gethash x h)) s)))

(defun main-cycle-number (n)
  (let ((h (get-prime-hash n)) (s 0))
    (alexandria:maphash-keys (lambda (x) (if (check-cycle-number h x) (incf s))) h)
    (block nil (return s))))

(main-cycle-number 1000000)

(defun sprime (m)
  (let ((s (make-array (+ m 1) :initial-element 1)))
    (loop
      for i from 2 to m
      for v = (aref s i)
      do
      (if (> v 0) (progn
        (loop
         for j from (+ i i) to m by i
         do
         (setf (aref s j) 0))
        (setf (aref s i) i)
        )))
    (setf (aref s 0) 0)
    (setf (aref s 1) 0)
    (block nil (return (reduce '+ s)))
    ))

(sprime 2000000)


(defun is-n-digit (d n) (= n (+ 1 (floor (log d 10)))))

(defun main-powerful-digit ()
  (let ((n 0) (h (make-hash-table)))
    (loop for j from 1
          do
          (setf temp 0)
          (loop for i from 1 to 9
                for d = (expt i j)
                do
                (if (and (null (gethash d h))
                         (is-n-digit d j))
                  (progn
                    (format t "~D ~D ~D ~%" i j d)
                    (setf (gethash d h) 1)
                    (incf temp))))
          (if (= temp 0) (return-from main-powerful-digit n)
            (setf n (+ n temp))))
    (block nil (return n))))

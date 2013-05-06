(defun fac (n)
  (let ((f 1))
    (loop for i from 2 to n
          do
          (setf f (* f i))
          )
    f))

(defun select-nk (n k)
  (/ (fac n) (* (fac k) (fac (- n k)))))

(select-nk 40 20) 


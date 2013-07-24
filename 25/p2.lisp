(defun fibsum (s n i j)
  (let ((x (+ i j)))
    (cond ((> x n) s)
          ((= (mod x 2) 0)  (fibsum (+ s x) n j x))
          (t (fibsum s n j x))
    )))

(fibsum  0 4000000 1 1) 


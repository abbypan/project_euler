(defstruct frac u d)

(defun get-left-frac (up down cnt)
  (let ( (x 0) (y 0)
               (i 0) (j 0)
               (max-d (* up cnt))
               (min-up 1) (min-down down))

    (loop while(<= y max-d)
          do
          (setf x (+ x down))
          (incf i)
          (loop while (>= x y)
                do
                (setf y (+ y up))
                (incf j))

          (if (and (= (gcd i j) 1) (> (* i min-down) (* min-up j)))
            (progn 
              (setf min-up i)
              (setf min-down j))))

    (block nil (return min-up))))

(time (print (get-left-frac 3 7 1000000)))

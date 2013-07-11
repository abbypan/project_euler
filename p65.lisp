

(defun new-e-x (m x y) (+ y (* (* 2 m) (+ x y))))
(defun new-e-y (m x y) (+ x y (new-e-x m x y)))

(defun get-e-convergent_fraction (m)
  (let* ((n (/ (- m 1) 3))
	(x (+ 1 (* 2 n)))
	(y (+ 1 x))
	)
	(loop for i from (- n 1) downto 1
	for nx = (new-e-x i x y)
	for ny = (new-e-y i x y)
	for nxy = (gcd nx ny)
	do
	(setf x (/ nx nxy))
	(setf y (/ ny nxy))
	)

	(setf x (+ x (* 2 y)))

	(format t "m : ~A~%x : ~A~%y : ~A~%" m x y)

	(block nil (return x))
))

(defun sum-of-digits (d)
  (labels ((s-recur (n m)
	(if (= n 0) m
	   (s-recur (floor (/ n 10)) (+ m (mod n 10))))))
   (s-recur d 0)
)
)

(print (sum-of-digits (get-e-convergent_fraction 100)))

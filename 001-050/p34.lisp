(ql:quickload "alexandria")

(defun get-max-num ()
  (let ((v 10) (f (alexandria:factorial 9)))
    (loop
      while (< v f)
      do
      (setf v (* 10 v)))
    (block nil (return v))))


(defun digit-factorials ()
   (let ((m (get-max-num)) (s 0))
	(loop
	 for i from 3 to m
	 do
	 (if (is-digit-fac i) (setf s (+ s i))))
	(block nil (return s))))


(defun is-digit-fac (n)
   (let ((s 0) (m n))
    (loop
	while (> m 0)
	for i = (mod m 10)
     do
	(setf m (floor (/ m 10)))
	(setf s (+ s (alexandria:factorial i))))
     (block nil (return (= n s)))))

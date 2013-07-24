	(defun check-powers-sum (n m)
	 (let ( (s 0) (x n))
	  (loop
	   for i = (mod x 10)
	   while (> x 0)
	   do
	   (setf s (+ s (expt i m)))
	   (setf x (floor (/ x 10))
	   ))
	  (block nil (return (= n s)))))

(defun get-high-number (m)
    (let ((i 1) (j (expt 9 m)))
     (loop
	for x = (expt 10 i)
	for y = (* i j)
	while (< x y)
	do
	(incf i)
	)
	(block nil (return (expt 10 i )))))

	

(defun main-powers-sum (m)
    (let ( (s 0) (high (get-high-number m)))
	(loop 
	for i from 2 upto high
	do 
	(if (check-powers-sum i m) (setf s (+ s i)))
	)
	(block nil (return s))))

(defun destruct-n (n)
 (let ((nlist nil)) 
  (loop for x = (mod n 10)
   while (> n 0) 
   do
   (push x nlist)
   (setf n (floor (/ n 10))))
  (block nil (return nlist))))

(defun is-permutation (n m)
 (equal (sort (destruct-n n) #'<) (sort (destruct-n m) #'<)))

	(defun calc-euler-totient (value-h n max-n)
	 (let ((c n) (max-p (- max-n n)))
	  (if (null (gethash n value-h)) 
	   (progn
	    (if (<= n max-p) (push n (gethash (+ n n) value-h)))
	    (return-from calc-euler-totient (- c 1))
	   ))
	  (loop
	   for p in (gethash n value-h)
	   do
	   (setf c (/ (* c (- p 1)) p))
	   (if (<= p max-p) (push p (gethash (+ n p) value-h)))
	  )
	  (block nil (return c))))

	(defun main-totient-permutation (n)
	 (let ((result-i 1) (result-div n) (value-h (make-hash-table)))
	  (loop for i from 2 to n
	   for j =     (calc-euler-totient value-h i n)
	   for k = (/ i j)
	   do
	   (if (and (< k result-div) (is-permutation i j))
	    (progn
	     (setf result-div k)
	     (setf  result-i i)))

	   (remhash i value-h)
	  )

	  (block nil (return result-i))
	 ))

(ql:quickload "alexandria")

(defun is-pandigital (n)
  (let ((h (make-hash-table)))
    (loop
      for i = (mod n 10)	
      while (> n 0)
      do
      (if (or (gethash i h) (= i 0)) 
        (return-from is-pandigital ))
      (setf n (floor (/ n 10)))
      (setf (gethash i h) 1))

    (loop for x from 1 to 9
          do
          (if (null (gethash x h)) 
            (return-from is-pandigital)))

    (block nil (return t))))


(defun join-number (n m)
  (let ((c (expt 10 (+ 1 (floor (log m 10))))))
    (block nil (return (+ m (* c n))))))

(defun join-nlist (nlist)
  (reduce 'join-number nlist))

(defun concatenated-product (n m)
  (let* ((s (alexandria:iota m :start 1))
         (r (mapcar (lambda (x) (* x n)) s))
         )
    (block nil (return (join-nlist r)))))
   
(defun main-pandigital-product ()
  (let ((m 0) (i 1) (e (expt 10 10)) (max-j 9))
    (loop 
      while (> i 0)
      do
      (if (>= (concatenated-product i 2) e) (return))
      (loop
        for j from max-j downto 2
        for k = (concatenated-product i j)
        do
        (if (<= k m) (return))
        (if (>= k e) (progn (setf max-j (- j 1)) (return)))
        (if (is-pandigital k)
          (progn (setf m k) (print i) (print j) (print m)))
       )
      (incf i)
      )
    (block nil (return m))))

(main-pandigital-product)

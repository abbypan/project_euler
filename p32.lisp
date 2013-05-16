(ql:quickload "alexandria")

(defun is-pandigital (nlist)
  (let ((h (make-hash-table)) (n 0))
    (loop
      for x from 0 to (- (list-length nlist) 1)
      do
      (setf n (nth x nlist))
      (loop
        for i = (mod n 10)	
        while (> n 0)
        do
        (if (or (gethash i h) (= i 0)) 
          (return-from is-pandigital ))
        (setf n (floor (/ n 10)))
        (setf (gethash i h) 1)))

    (loop for x from 1 to 9
          do
          (if (null (gethash x h)) 
            (return-from is-pandigital)))

    (block nil (return t))))


(defun main-pandigital-sum ()
  (let ((h (make-hash-table)) )
    (loop
      for i from 1 to 99
      do
      (loop
        for j from i to 9999
        for k = (* i j)
        do
        (if (is-pandigital (list i j k)) 
          (setf (gethash k h) 1)
          )))
    (block nil (return (reduce '+ (alexandria:hash-table-keys h))))))


(main-pandigital-sum)

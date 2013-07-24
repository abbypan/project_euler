
(defun is-prime-to (m s)
  (if (null s) t
   (every (lambda (n) (/= (mod m n) 0)) s)
    ))
(defun calc-prime-and-squre (p x)  (+ p (* 2 x x)))

(defun update-prime-value (h p)
  (loop
    for i from 1 to (- p 1) 
    for j = (calc-prime-and-squre p i)
    do
    (setf (gethash j h) 1)
    ))

(defun update-newnum-value (h plist n)
  (loop
    for p in plist
    for j = (calc-prime-and-squre p n)
    do
    (setf (gethash j h) 1)
    ))

(defun main-odd-np ()
  (let ((prime-list '()) (h (make-hash-table)) (i 2))
    (loop
      while (> i 1)
      do
      (if (is-prime-to i prime-list) 
        (progn (update-prime-value h i)
            (push i prime-list)) 
        (if (and (/= (mod i 2) 0) 
                 (null (gethash i h))) 
                 (return-from main-odd-np i)))
      (update-newnum-value h prime-list i)
      (incf i)
      )
    (block nil (return 0))))

                                       





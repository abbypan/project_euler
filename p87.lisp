(ql:quickload "alexandria")

(defun is-prime-to (s m)
  (if (null s) t
    (every (lambda (n) (/= (mod m n) 0)) s)
    ))

(defun next-prime (s)
  (let ((x (car  s)))
    (loop
      while t
      do
      (if (is-prime-to s x) (return)) 
      (incf x)
      )
    (block nil (return x))
    ))

(defun calc-prime-to-n (n)
  (let ((s '(2)))
    (loop
      for p = (next-prime s)
      while (<= p n)
      do
      (push p s)
      )
    (block nil (return (reverse s)))
    ))


(defun calc-max-num (n) (floor (sqrt (- n (expt 2 4) (expt 2 3)))))

(defun main-prime-sum-cnt (m)
  (let* ((n (calc-max-num m)) 
         (plist (calc-prime-to-n n)) 
         (h (make-hash-table))
         )
    (loop for x in plist 
          for s2 = (expt x 2)
          do
          (if (> s2 m) (return))
          (loop for y in plist 
                for s3 = (+ s2 (expt y 3))
                do
                (if (> s3 m) (return))
                (loop for z in plist 
                      for s4 = (+ s3 (expt z 4))
                      do
                      (if (> s4 m) (return) (setf (gethash s4 h) 1))
                      )))
    (block nil (return (length (alexandria:hash-table-keys h))))))

;1097343

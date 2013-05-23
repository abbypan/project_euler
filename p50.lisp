(ql:quickload "alexandria")

(defun prime-list (m)
  (let ((s (make-array (+ m 1) :initial-element 1)) (plist '()))
    (loop for i from 2 to m
          for v = (aref s i)
          do
          (if (> v 0) 
            (progn
              (loop for j from (+ i i) to m by i
                    do (setf (aref s j) 0))
              (push i plist))))
    (block nil (return (reverse  plist)))))

(defun calc-prime-maxlen (plist i old-mlen)
  (let* ((x 0) (max-len 0) (max-num 0) 
         (n (length plist)) (max-p (nth (- n 1) plist)))
    (loop
      for j from i upto (- n 1) 
      for pj = (nth j plist)
      for new-len = (+ 1 (- j i))
      do
      (setf x (+ x pj)) 
      (if (> x max-p) (return))
      (if (and (> new-len old-mlen)  (member x plist) )
        (progn (setf max-len new-len) (setf max-num x))))
    (block nil (return (list max-len max-num)))
    ))

(defun main-maxlen-prime (n)
  (let ((plist (prime-list n)) 
        (max-len 1) (max-prime 0))

    (setf ilast (- (length plist) 1) )
    (setf maxp (nth ilast plist))
    (setf y maxp)

    (loop for i from 0 upto ilast
          for x = (nth i plist)
          while (<= x y)
          do
          (setf mlen-mnum  (calc-prime-maxlen plist i max-len)) 
          (setf mlen (car mlen-mnum))
          (if (> mlen max-len) 
            (progn (setf max-len mlen)
                   (setf max-prime (cadr mlen-mnum))
                   (setf y  (floor (/ maxp max-len)))
                   )))
    (block nil (return max-prime))))

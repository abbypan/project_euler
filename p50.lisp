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
  (let* ((max-len 0) (max-num 0) 
         (max-index (- (length plist) old-mlen) ) 
         (p-sum 0) (max-p (car (last plist)) ))
    (loop
      for j from i upto max-index
      for pj = (nth j plist)
      for len = (+ (- j i) 1)
      do
      (setf p-sum (+ p-sum pj))
      (if (> p-sum max-p) (return))
      (if (and (> len old-mlen)  (member p-sum plist) )
        (progn (setf max-len len) (setf max-num p-sum))))
    (block nil (return (list max-len max-num)))
    ))

(defun main-maxlen-prime (n)
  (let* ((max-len 1) 
         (max-prime 0)
         (plist (prime-list n)) 

         (last-p (car (last plist)) )
         (max-p last-p)

         (last-i (- (length plist) 1) )
         (max-i last-i))

    (loop for i from 0 upto max-i
          for x = (nth i plist)
          while (<= x max-p)
          do
          (setf mlen-mnum  (calc-prime-maxlen plist i max-len)) 
          (setf mlen (car mlen-mnum))
          (if (> mlen max-len) 
            (progn (setf max-len mlen)
                   (setf max-prime (cadr mlen-mnum))
                   (setf max-p  (floor (/ last-p max-len)))
                   (setf last-i (+ 1 (- last-i max-len)))
                   )))
    (block nil (return max-prime))))

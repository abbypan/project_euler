(ql:quickload "alexandria")
(defun prime-list (m)
  (let ((s (make-array (+ m 1) :initial-element 1)) (plist '()))
    (loop for i from 2 to m
          for v = (aref s i)
          do
          (if (> v 0) (progn
                        (loop for j from (+ i i) to m by i
                              do (setf (aref s j) 0))
                        (push i plist)
                        )))
    (block nil (return plist))
    ))

(defun map-to-nlist (x)
  (let ((v '()))
    (loop for i = (mod x 10)
      while (> x 0)
      do
      (push i v)
      (setf x (floor (/ x 10)))
      )
    (block nil (return v))
    ))

(defun calc-min-nlist (nlist)
  (let ((v (sort nlist #'<)) (s 0))
    (loop while (not (null v))
      do
      (setf s (+ (* 10 s) (pop v)))
      )
    (block nil (return s)) 
    )
  )

(defun calc-three-inc-prime (nlist m)
  (let ((n (length nlist)) (v '()))
    (setf base (expt 10 (+ m 1)))
    (if (< n m) (return-from calc-three-inc-prime 0))
    (loop for i from 0 to (- n m)
      for x = (nth i nlist)
      do
      (loop for j from (+ i 1) to (- n (- m 1))
        for y = (nth j nlist)
        for d = (- (* 2 y) x)
        for s = (+ d (* base (+ y (* base x))))
        do
        (if (member d nlist) 
          (return-from calc-three-inc-prime s)
          )))
    (block nil (return 0)) 
    ))

(defun main-alldiff-prime-seq ()
  (let ((plist (prime-list 9999)) (h (make-hash-table)))

    (loop for p in plist
      for pn = (map-to-nlist p)
      do
      (if (> p 1000)
        (push p (gethash (calc-min-nlist pn) h))
        ))

    (loop for pl in (alexandria:hash-table-values h)
          for pl-three = (calc-three-inc-prime pl 3)
          do
          (if (and (> pl-three 0) (/= pl-three 148748178147) ) 
            (return-from main-alldiff-prime-seq pl-three))
          )
    (block nil (return 0))))

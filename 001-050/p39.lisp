(ql:quickload "alexandria")

(defun init-square-hash (n)
  (let ((h (make-hash-table)))
    (mapcar (lambda (x) (setf (gethash (expt x 2) h) x)) 
            (alexandria:iota n :start 1))
    (block nil (return h))))

(defun main-right-triangle (s)
  (let* ((n (floor (/ s 2))) 
        (h (init-square-hash n)) 
        (k (sort (alexandria:hash-table-keys h) #'>))
        (r (make-hash-table))
        (p 0)
        )

    (loop for cs in k
          for c = (gethash cs h) 
          do
          (loop
            for as in k
            for a = (gethash as h) 
            for bs = (- cs as)
            for b = (gethash bs h)
            do
            (if (and (> bs 0) (not (null b)))
              (if (>= a b)  (push (list a b c) (gethash (+ a b c) r))))
            ))
    (maphash (lambda (k v) (setf (gethash k r) (length v))) r)
        (setf k (sort (alexandria:hash-table-keys r) (lambda (x y)
                                                       (> (gethash x r)
                                                          (gethash y r)))
                                                       ))
        (block nil (return (first k)))
    ))




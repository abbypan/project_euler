(defun next-collatz (n)
  (if (= (mod n 2) 0) (/ n 2)
    (+ (* 3 n) 1)))

(defun hash-keys (h)
  (loop for k being each hash-key in h collecting k)
  )

(defun hash-values (h)
  (loop for v being each hash-value in h collecting v)
  )

(defun longest-collatz (m)
  (let ((s (make-hash-table)))
    (loop
      for i from 2 to m
      do
      (setf (gethash i s) (next-collatz i))
      )
    (loop
      for ks = (hash-keys s)
      for vs = (hash-values s)
      do
      (if (every #'(lambda (x) (= x 1)) vs) (return ks))
      (if (= (list-length ks) 1) (return ks))
      (loop
        for k in ks
        for v = (gethash k s)
        do
        (if (not (null v)) (progn
            (if (gethash v s) (remhash v s))
            (if (= v 1) (remhash k s))
            (if (> v 1) (progn
                (setf (gethash k s) (next-collatz v))
                (setf vzero 1)
            ))))))))

(longest-collatz 1000000)


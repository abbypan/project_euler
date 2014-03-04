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

(defun generate-prime-to-n (n)
  (let ((s '(2)))
    (loop
      for p = (next-prime s)
      while (<= p n)
      do
      (push p s)
      )
    (block nil (return (reverse s)))
    ))

(defun set-hamming (n p i)
  (let ((h (make-hash-table)))
    (if (>= i (length p)) (return-from set-hamming nil))
    (setf (gethash 'number h) n)
    (setf (gethash 'primes h) p)
    (setf (gethash 'index h) i)
    (block nil (return h))
    ))

(defun init-select-hamming (p)
  (let ((hlist '()) (h (set-hamming 1 p 0))
                    (init (make-hash-table))
                    )
    (push h hlist)
    (setf (gethash 'vlist init) '())
    (setf (gethash 'hlist init) hlist)
    (block nil (return init))
    )
  )

(defun step-hamming (hv m)
  (let* ((hlist (gethash 'hlist hv))
         (vlist (gethash 'vlist hv))
         (h (pop hlist))
         (plist (gethash 'primes h))
         (i (gethash 'index h))
         (n (gethash 'number h))
         (x (nth i plist))
         )
    (incf i)

    (loop 
      for y = (set-hamming n plist i)
      for z = (nth i plist)
      while(<= n m) 
      do
      (if (or (null y) (and z (> (* z n) m))) (push n vlist) 
        (push y hlist))
      (setf n (* n x))
      )
    (setf (gethash 'vlist hv) vlist)
    (setf (gethash 'hlist hv) hlist)
    (block nil (return hv))
    )
  )

(defun main-hamming (pmax nmax)
  (let* ((plist (generate-prime-to-n pmax))
         (hamming (init-select-hamming plist))
         )
    (loop
      for hlist = (gethash 'hlist hamming) 
      while hlist
      do
      (step-hamming hamming nmax)
      )
    (setf vlist (sort (gethash 'vlist hamming) '<))
    (block nil (return (length vlist)))
    ))

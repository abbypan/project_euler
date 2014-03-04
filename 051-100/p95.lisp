(ql:quickload "alexandria")

(defun longest-amicable-chain (max-n)
  (let ((chain-h (get-amicable-chain max-n))
        (len 0)
        (chain '())
        (min-v max-n)
        )
    (loop 
      for i being the hash-keys in chain-h using (hash-value j)
      for s = i
      for k = 0
      for c = (list i)
      do
      (loop 
        while (and j (/= j s))
        do
        (remhash i chain-h)
        (push j c)
        (setf i j)
        (setf j (gethash i chain-h))
        (incf k)
        )

      (setf temp-min-v (reduce 'min c))
      (if (or (> k len) (<= temp-min-v min-v))
        (progn 
          (setf chain c) 
          (setf len k)
          (setf min-v temp-min-v)
          ))

      )
    (print chain)
    (block nil (return (reduce 'min chain)))
    ))

(defun get-amicable-chain (max-n)
  (let ((ami-h (get-amicable-hash max-n)))
    (loop 
      while t
      do
      (setf flag 0)
      (loop 
        for k being the hash-keys in ami-h using (hash-value v)
        do
        (if (null (gethash v ami-h)) (progn 
                                       (setf flag 1)
                                       (remhash k ami-h)                                     )        ))
      (if (= flag 0) (return-from get-amicable-chain ami-h))
      )))

(defun get-amicable-hash (max-n)
  (let ((primes '())
        (ami-h (make-hash-table))
        )
    (loop for i from 2 to max-n
          for j = (gethash i ami-h)
          do
          (if (and j
                   (or 
                     (and (< j i) (null (gethash j ami-h)))
                     (= j i)
                     (> j max-n)
                     )
                   )
            (remhash i ami-h)
            )

          (setf k (+ i i))
          (loop
            for z = (gethash k ami-h)
            while(<= k max-n)
            do
            (setf (gethash k ami-h)
                  (if (null z) (+ i 1) (+ z i))
                  )
            (setf k (+ k i))
            )

          )
    (block nil (return ami-h))
    ))

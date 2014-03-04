(ql:quickload "alexandria")

(defun init-select-coins (m)
  (let ((v '()) (h ( make-hash-table)))
    (setf (gethash 'money h) m)
    (setf (gethash 'coins h) '())
    (push h v)
    (block nil (return v))
    )
  )	

(defun calc-max-coin-cnt (h maxcoin)
  (block nil (return (floor (/ (gethash 'money h) maxcoin)))))

(defun select-max-coin (source-h maxcoin cnt)
  (let ((h (alexandria:copy-hash-table source-h)) 
        (v (* maxcoin cnt)) 
        (s '()) (m 0)) 

    (setf m (- (gethash 'money h) v))
    (setf (gethash 'money h) m)

    (setf s (gethash 'coins h))
    (if (> cnt 0) (push (list maxcoin cnt) s))
    (setf (gethash 'coins h) s)

    (block nil (return h))))


(defun select-coins (money coins)
  (let ((v (init-select-coins money)) 
        (temp-v '()) 
        (result '())

        (coin 0) 
        (max-cnt 0) 
        (min-cnt 0) 
        (old-hash (make-hash-table)) 
        (new-hash (make-hash-table)) 
        ) 
    (loop
      while (> (length coins) 0) 
      do
      (setf coin (pop coins))
      (loop
        while (> (length v) 0) 
        do
        (setf old-hash (pop v))
        (setf max-cnt (calc-max-coin-cnt old-hash coin))
        (setf min-cnt (if (= (length coins) 0) max-cnt 0)) 
        (loop 
          for i from min-cnt upto max-cnt
          do
          (setf new-hash (select-max-coin old-hash coin i))
          (push new-hash (if (= (gethash 'money new-hash) 0) result temp-v))
          )
        )
      (setf v temp-v)
      (setf temp-v '()))
    (print result)
    (print (length result))
    (block nil (return (length result)))
    ))

(select-coins 200 '(200 100 50 20 10 5 2 1))

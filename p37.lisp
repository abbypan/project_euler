(ql:quickload "alexandria")

(defun left-trim (m)
  (let ((v '()) (c (floor (log m 10))))
    (loop
      for i from 1 upto c
      for j = (expt 10 i)
      for x = (mod m j)
      do
      (push x v)
      )
    (block nil (return v))
    ))

(defun right-trim (m)
  (let ((v '()) (c (floor (log m 10))))
    (loop
      for i from 1 upto c
      for j = (expt 10 i)
      for y = (floor  (/ m  j))
      do
      (if (> y 0) (push y v))
      )
    (block nil (return v))
    ))

(defun is-prime (n)
  (let ((x (floor (sqrt n))))
    (if (<  n 2) (return-from is-prime))
    (loop for i from 2 upto x
          do
          (if (= (mod n i) 0) (return-from is-prime))
          )
    (block nil (return t))
    ))

(defun is-left-prime (n)
  (every #'is-prime (left-trim n)))

(defun is-right-prime (n)
  (every #'is-prime (right-trim n)))

(defun make-double-trim (x y)
  ; x is right-prime
  ; y is left-prime
  (let ((m (floor (/ y 10))) (i (expt 10 (floor (log x 10))) ) (f 0))
    (if (/= (mod x i) m) (return-from make-double-trim 0)) 
    (setf f (+ y (* 10 (- x m))))
    (if (is-prime f) (return-from make-double-trim f))
    (block nil (return 0))
    )
  )


(defun next-left-prime (n)
  (let ((v '()) (c (expt 10 (+ 1 (floor (log n 10))))))
    (loop
      for i from 1 upto 9
      for j = (+ n (* c i)) 
      do
      (if (is-prime j) (push j v))
      )
    (block nil (return v))
    )
  )

(defun next-right-prime (n)
  (let ((v '()))
    (loop
      for i from 1 upto 9
      for j = (+ i (* n 10)) 
      do
      (if (is-prime j) (push j v))
      )
    (block nil (return v))
    )
  )

(defun extract-double-prime-list (right-prime left-prime)
  (let ((v '()))
    (loop
      for x in right-prime
      do
      (loop
        for y in left-prime
        for z = (make-double-trim x y)
        do
        (if (> z 0) (push z v))
        )
      )
    (block nil (return v)) 
    )
  )


(defvar src-prime-list '(2 3 5 7))

(defun main-trim-prime ()
  (let ( (left-prime (copy-list src-prime-list))
        (right-prime (copy-list src-prime-list))
        (double-trim-prime '())
        )
    (loop
      for left = '()
      for right = '()
      while (not (null right-prime))
      do
      (alexandria:appendf double-trim-prime (extract-double-prime-list right-prime left-prime))
      (mapcar (lambda (x) (alexandria:appendf left (next-left-prime x))) left-prime)
      (setf left-prime left)
      (mapcar (lambda (x) (alexandria:appendf right (next-right-prime x))) right-prime)
      (setf right-prime right)
      )
    (print double-trim-prime)
    (block nil (return (reduce '+ double-trim-prime) )) 
    ))

(main-trim-prime)

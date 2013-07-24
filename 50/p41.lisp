(ql:quickload "alexandria")

(defun select-n (nlist)
  (let ((v '()) (s (pop nlist)))
    (loop
      for n in nlist
      for x = (+ n (* s 10))
      for nv = (remove n (copy-list nlist))
      do
      (push (cons x nv) v)
      )
    (block nil (return v))))

(defun is-prime (n)
  (let ((x (floor (sqrt n))))
    (if (<  n 2) (return-from is-prime))
    (loop for i from 2 upto x
          do
          (if (= (mod n i) 0) (return-from is-prime))
          )
    (block nil (return t))
    ))


(defun pandigital-n-prime (n)
  (let ((v (list (alexandria:iota (+ 1 n)))) (num 0))
    (loop 
      while (> (length v) 0)
      do
      (setf x (select-n (pop v)))

      (if (= (length (car x)) 1) 
        (mapcar (lambda (k) 
                  (if (is-prime (car k)) 
                    (return-from pandigital-n-prime (car k)))) x)
        (progn (alexandria:appendf x v)
               (setf v x)))
      )
    (block nil (return 0))))

(defun max-pandigital-prime ()
  (let ((p 0))
    (loop
      for x from 9 downto 2
      do
      (setf p (pandigital-n-prime x))
      (print x)
      (print p)
      (if (> p 0) (return-from max-pandigital-prime p)))
    (block nil (return p))))


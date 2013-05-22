(ql:quickload "alexandria")

(defun get-subseq-n (nlist i n)
  (reduce (lambda (x y) (+ y (* 10 x))) 
          (subseq nlist i (+ i n))))

(defvar plist '(2 3 5 7 11 13 17))
(defun is-prime-divisible (nlist)
  (let ((m (- (length nlist) 3)))
    (setf p (nth (- m 1) plist))
    (setf x (get-subseq-n nlist m 3))
    (block nil (return (= (mod x p) 0) 
                       ))))

(defun map-nlist-to-number (nlist)
  (let ((s 0))
    (loop for n in nlist
          do
          (setf s (+ n (* 10 s))))
    (block nil (return s))
    ))

(defun select-n (nlist)
  (let ((v '()) (s (pop nlist)))
    (loop
      for n in nlist
      for x = (copy-list s)
      do
      (if (not (and (null s) (= n 0)))
        (progn
          (push n x)
          (setf nv  (remove n (copy-list nlist)))
          (push (cons x nv) v)))
      )
    (block nil (return v))))

(defun pandigital-n-prime (n)
  (let* ((init (alexandria:iota (+ 1 n)))
         (v '()) 
         (result '())
         )
    (push '() init)
    (push init v)

    (loop 
      while (> (length v) 0)
      do
      (setf x (select-n (pop v)))

      (if (< (length (car (car x))) 4) 
        (progn (alexandria:appendf x v)
               (setf v x))
        (loop
          for kn in x
          for ks = (reverse (car kn))
          do
          (if (is-prime-divisible ks) 
            (if (= (length kn) 1) 
              (push ks result)
              (push kn v)
              )))
        ))
    (print result)
    (block nil (return 
                 (reduce #'+ 
                         (mapcar #'map-nlist-to-number result))))))

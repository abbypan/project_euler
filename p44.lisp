
(defvar kv (make-hash-table))
(defvar v (make-hash-table))
(defvar max-k 0)
(defvar max-v 0)

(defun get-n-pairs (n)
  (let ((v '()))
    (loop for i from 1 upto (- n 1)
      do
      (push (list i n) v))
    (block nil (return v))))

(defun calc-pentagonal (n) (/ (* n (- (* 3 n) 1)) 2))

(defun update-pentagonal-kv (n)
  (let ((max-nv (+ (calc-pentagonal n) (calc-pentagonal (- n 1)))))
    (loop
        while (< max-v max-nv)
        do
        (incf max-k)
        (setf max-v (calc-pentagonal max-k))
        (setf (gethash max-k kv) max-v)
        (setf (gethash max-v v) 1)
        )
    (block nil (return kv)))
  )

(defun check-pentagonal-pair (pair)
  (let ((mv (gethash (car pair) kv)) (nv  (gethash (cadr pair) kv)))
    (if (and (gethash (- nv mv) v) (gethash (+ nv mv) v))
      (return-from check-pentagonal-pair (- nv mv)))
    (block nil (return 0))))

(defun main-pentagonal-pair ()
  (let ((i 2))
  (loop
    while (> i 0)
    do
    (update-pentagonal-kv i)
    (loop
      for x in (get-n-pairs i)
      for y = (check-pentagonal-pair x)
      do
      (if (> y 0) 
        (progn (print x)
        (return-from main-pentagonal-pair y)))
      )
    (incf i)
    )
  (block nil (return 0))
  ))


(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")

(defvar cyclical-type-list 
  (list 'triangle 'square 'pentagonal 'hexagonal 'heptagonal 'octagonal))
(defstruct cyc-elem head tail num ctype)
(defstruct cyc-chain elems next-head start-head)

(defun main-cyclical (max_n)
  (let* ((cy-hash (cyc-num-hash max_n))
         (cy-list (init-cyc-chain cy-hash))
         (res '())
  )
    (loop
      while cy-list
      do
      (setf x (pop cy-list))
      (setf xlist (get-next-cyc cy-hash x 'check-cyc-chain))
      (loop for y in xlist
            for len = (length (cyc-chain-elems y))
            do
            (if (= len 6) (if (is-stop-cyc-chain y) (push y res))
            (push y cy-list))))
    (block nil (return res))
    ))

(defun cyc-num-hash (max_n)
  (let ((s (maybe-cyc-chain max_n))
        (tail-hash (make-hash-table))
        (cyclical-hash (make-hash-table)))

    (loop for x in s
      do
      (loop for y in x
            for tail = (cyc-elem-tail y)
            do
            (setf (gethash tail tail-hash) 1)))

    (loop for x in s
      do
      (loop for y in x
            for z = (cyc-elem-head y)
            for w = (gethash z cyclical-hash)
            do
            (if (not (null (gethash z tail-hash)))
              (progn
            (if (null w) (setf w '()))
            (push y w)
            (setf (gethash z cyclical-hash) w)))))
    (block nil (return cyclical-hash))))

(defun init-cyc-chain (cy-hash)
  (let ((s '()))
    (loop 
      for n in (sort (alexandria:hash-table-keys cy-hash) '<)
      for x = (make-cyc-chain :elems '() :next-head n :start-head n)
      do
      (push x s))

    (block nil (return s))))

(defun get-next-cyc (cy-hash x check_func)  
  (let* ((pairs (cyc-chain-elems x))
         (next-head (cyc-chain-next-head x))
         (start-head (cyc-chain-start-head x))
         (res '()))

    (loop for y in (gethash next-head cy-hash)
          for z = (make-cyc-chain :elems  (append pairs (list y))
                                    :next-head  (cyc-elem-tail y)
                                    :start-head start-head)
          do
          (if (funcall check_func z) (push z res)))
    (block nil (return res))))

(defun check-cyc-chain(z)
  (let* ((s (cyc-chain-elems z))
        (len (list-length s))
        (h (make-hash-table))
        (flag nil))

    (if (> (cyc-chain-start-head z)
           (cyc-chain-next-head z)) (return-from check-cyc-chain))
    (loop for x in s
          for ct = (cyc-elem-ctype x)
          for z = (gethash ct h)
          do
          (if (not (null z)) (return-from check-cyc-chain))
          (setf (gethash ct h) 1))

    (if (<= len 6) (return-from check-cyc-chain t))))

(defun  maybe-cyc-chain (max_n)
  (reverse (mapcar (lambda (f) (cyc-num-list f max_n)) cyclical-type-list)))

(defun init-cyc-elem (n ct)
  (make-cyc-elem
    :head (floor (/ n 100))
    :tail (mod n 100)
    :num n
    :ctype ct))

(defun maybe-cyc (x)
  (and (> (cyc-elem-head x) 9) (> (cyc-elem-tail x) 9))) 

(defun triangle (i) (/ (* i (+ i 1)) 2))
(defun square (i) (* i i))
(defun pentagonal (i) (/ (* i (- (* 3 i) 1)) 2))
(defun hexagonal (i) (* i (- (* 2 i) 1)))
(defun heptagonal (i) (/ (* i (- (* 5 i) 3)) 2))
(defun octagonal (i) (* i (- (* 3 i) 2)))

(defun cyc-num-list (func max_n)
  (let ((s '()) (i 0))
      (loop
        for n = (funcall func i)
        for x = (init-cyc-elem n func)
        while (< n max_n) 
        do
        (if (maybe-cyc x) (push x s))
        (incf i))
      (block nil (return s))))

(defun is-stop-cyc-chain (y)
  (= (cyc-chain-start-head y) (cyc-chain-next-head y)))

(defun calc-cyc-chain-sum (y)
  (reduce '+ (mapcar (lambda (x) (cyc-elem-num x)) 
                     (cyc-chain-elems y))))

(calc-cyc-chain-sum (first (main-cyclical 10000)))

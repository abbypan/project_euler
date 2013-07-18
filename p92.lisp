(ql:quickload "alexandria")

; 这边 get-cell-list 默认 n = 10^i

(defun  next-iter-cell (i x cell)
  ; sum is the square digit sum
  ; cnt is the count of number, calc-squre-digit(n) = sum
  (let* ((digit-cnt (car cell))
         (sum (+ (cadr cell) (* i (* x x))))
         (cnt (* (caddr cell) (alexandria:binomial-coefficient digit-cnt i))))
    (setf next-cell (list (- digit-cnt i) sum cnt))
    (block nil (return next-cell))))

(defun get-cell-list (n)
  (let* ((cnt (floor (log n 10)))
         (cell (list cnt 0 1))
         (s (list cell))
         (result nil))

    (loop for d in (alexandria:iota 10 :start 0)
          do
          (setf temp-s nil)
          (loop for c in s
                for c-cnt = (car c)
                do
                (if (= c-cnt 0) (push c result)
                  (loop for i from 0 to c-cnt
                        do
                        (push (next-iter-cell i d c) temp-s))))
          (setf s temp-s))

    (alexandria:appendf result (remove-if-not (lambda (x) (= 0 (car x))) s))
    (setf result (mapcar (lambda (x) (cdr x)) result))
    (block nil (return result))))


(defun get-chain-hash (nlist calc stop)
  (let ((h (make-hash-table)))
    (loop
      for i in nlist 
      for s = (iter-chain h i calc stop)
      for v = (car s)
      for ks = (remove-if-not (lambda (x) (member x nlist)) (cdr s))
      do
      (mapcar (lambda (x) (setf (gethash x h) v)) ks)
      )
    (block nil (return h))))

(defun iter-chain (h i calc stop)
  (let ((s (list i)) (n i))
    (loop for j = (funcall calc n)
          for k = (gethash j h)
          do
          (push j s)
          (if k (progn (rplaca s k) (return)))
          (if (funcall stop j) (return))
          (setf n j))
    (block nil (return s))))

(defun calc-squre-digit (n)
  (reduce #'+ 
          (mapcar 
            (lambda (x) (* x x))
            (split-n-digits n))))

(defun split-n-digits (n)
  (labels ((sp-n (s n)
                 (if (= n 0) s (sp-n (cons (mod n 10) s) (floor (/ n 10))))))
    (sp-n nil n)))

(defun is-stop-number (n)
  (or (= n 89) (= n 1) (= n 0)))

(defun main-square-digits (n)
  (let* ((cell-list (get-cell-list n))
         (chain-h (get-chain-hash 
                    (mapcar (lambda (x) (car x)) cell-list) 'calc-squre-digit 'is-stop-number))
         (cnt nil))
    (mapcar (lambda (x) (if (= (gethash (car x) chain-h) 89) (push (cadr x) cnt))) cell-list)
    (block nil (return (reduce #'+ cnt)))))

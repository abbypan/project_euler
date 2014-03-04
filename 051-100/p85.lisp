(defun calc-surface (cell) (* (car cell) (cadr cell)))

(defun calc-cell-distance (cell cnt) (abs (- cnt (caddr cell))))
(defun is-stop-cell (cell cnt) (> (caddr cell) cnt))

(defun incf-n-cnt (cell)
  (let* ((m (car cell))
         (n (cadr cell))
         (new-n (+ n 1))

         (x (/ (* m (+ m 1)) 2)) 
         (cnt (+ (caddr cell) (* x new-n)))
         )
    (block nil (return (list m new-n cnt)))
    ))

(defun incf-m-cnt (cell)
  (let* ((m (car cell))
         (n (cadr cell))
         (new-m (+ m 1))

         (x (/ (* n (+ n 1)) 2)) 
         (cnt (+ (caddr cell) (* x new-m)))
         )
    (block nil (return (list new-m n cnt)))
    ))


(defun step-cell-list (s) 
  (let ((ns nil) (h (car s)))
    (setf ns (mapcar 'incf-n-cnt s))
    (if (= 1 (cadr h))
      (push (incf-m-cnt h) ns)
      )
    (block nil (return ns))
    ))


(defun closer-cell (x y n)
  (if (< (calc-cell-distance x n) (calc-cell-distance y n))
    x y))

(defun closest-cell (s init-cell cnt)
  (reduce (lambda (x y) (closer-cell x y cnt)) s
          :initial-value init-cell
          ))



(defun main-cell (cnt)
  (let* ((closest-c (list 1  1 1)) 
         (s (list closest-c)) 
         )

    (loop
      do
      (setf closest-c (closest-cell s closest-c cnt))

      (setf ns (remove-if (lambda (x) (is-stop-cell x cnt)) s)) 
      (if (null ns) (return))

      (setf s (step-cell-list ns))
      )

    (print closest-c)
    (block nil (return (calc-surface closest-c)))
    ))

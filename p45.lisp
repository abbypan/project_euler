
(defun calc-triangle (n) (/ (* n (+ n 1)) 2))
(defun calc-pentagonal (n) (/ (* n (- (* 3 n) 1)) 2))
(defun calc-hexagonal (n) (* n (- (* 2 n) 1)))

(defun main-three-gonal (m)
  (let ((i 0) (j 0) (k 0) (s 0))
     (loop
       for iv = (calc-triangle i)
       for jv = (calc-pentagonal j)
       for kv = (calc-hexagonal k)
       for mv = (max iv jv kv)
       while (<= s m)
       do
       (if (<= mv m) (progn (incf i) (incf j) (incf k))
         (progn
           (if (< iv mv) (incf i))
           (if (< jv mv) (incf j))
           (if (< kv mv) (incf k))
         ))
       (if (= iv jv kv) (setf s iv))
       )
     (print (list i j k s))
     (block nil (return (calc-triangle i)))))

(main-three-gonal 40755)


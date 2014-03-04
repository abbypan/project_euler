(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")


(defun read-words (f)
  (let ((in (open f)) (s '()))
    (setf s (cl-ppcre:all-matches-as-strings "\"(\\w+)\"" (read-line in)))
    (setf s (sort s #'string<))
    (close in)
    (block nil (return s))
    ))

(defun calc-string-value (r)
  (let ((sc (map 'list #'char-code r)) (s 0))
    (loop
      for i in sc
      for j = (if (or (< i 65) (> i 90)) 0 (- i 64))
      do 
      (setf s (+ s j))
      )
    (block nil (return s))))

(defun main-calc-words (f)
  (let ((word-values (sort (mapcar 'calc-string-value (read-words f)) #'<)) (i 1) (trig 1) (cnt 0))
    (loop
      for v in word-values
      do
      (loop
        while (> v trig)
        do
        (incf i)
        (setf trig (/ (* i (+ i 1)) 2))
        )
      (if (= v trig) (incf cnt))
      )
    (block nil (return cnt))
    ))

(print (main-calc-words "p42-words.txt"))

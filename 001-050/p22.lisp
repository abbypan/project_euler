(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")


(defun read-names (f)
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

	(defun main-calc-names (f)
	 (let ((s (read-names f)) (i 0) (sum 0))
	  (loop
	   while s
	   do 
	   (incf i)
	   (setf sum (+ sum (* i (calc-string-value (pop s))))))
	  (block nil (return sum))
	 ))

	(print (main-calc-names "p22-names.txt"))

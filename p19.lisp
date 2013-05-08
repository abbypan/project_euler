(ql:quickload "alexandria")

(defun is-leap-year (y)
  (let ((leap (if (= (mod y 400) 0) 1
    (if (= (mod y 100) 0) 0
      (if (= (mod y 4) 0) 1
        0 )))))
   (block nil (return leap))
  ))

(defun get-month-days (y m)
   (let ((d (case m
(1 31)
(2 28)
(3 31)
(4 30)
(5 31)
(6 30)
(7 31)
(8 31)
(9 30)
(10 31)
(11 30)
(12 31)
)))
(if (and (= m 2) (= (is-leap-year y) 1)) (incf d))
(block nil (return d))
))

(defun get-year-days (y)
(reduce '+ (loop for i from 1 upto 12 collect (get-month-days y i)))
)

(defun get-years-days (begin-y end-y)
(reduce '+ (loop for i from begin-y upto end-y collect (get-year-days i)))
)
  
(defun is-sunday (y m d)
  (let ((s (get-years-days 1900 (- y 1)))
	)
   (loop 
	for i from 1 upto (- m 1)
	for j = (get-month-days y i)
    do 
    (setf s (+ s j))
)
    (setf s (+ s d))
    (block nil (return (if (= (mod s 7) 0) 1 0)))	
))

(defun calc-sunday-of-month-first-day (begin end)
   (let ((s 0))
    (loop for y from begin upto end
do 
(loop for m from 1 upto 12
do
(if (= (is-sunday y m 1) 1) (incf s))
))
(block nil (return s))))

(calc-sunday-of-month-first-day 1901 2000)

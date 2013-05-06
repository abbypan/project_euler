(ql:quickload "alexandria")
(defvar num-string (alexandria:alist-hash-table '((1 . "one") (2 . "two") (3 . "three") (4 . "four") (5 . "five") (6 . "six") (7 . "seven") (8 . "eight") (9 . "nine") (10 . "ten") (11 . "eleven") (12 . "twelve") (13 . "thirteen") (14 . "fourteen") (15 . "fifteen") (16 . "sixteen") (17 . "seventeen") (18 . "eighteen") (19 . "nineteen") (20 . "twenty") (30 . "thirty") (40 . "forty") (50 . "fifty") (60 . "sixty") (70 . "seventy") (80 . "eighty") (90 . "ninety") (100 . "hundred") (1000 . "thousand"))))

(defun number-to-string (n)
  (let* ((s (list))
         (q (floor (/ n 1000)))
         (h (mod (floor (/ n 100)) 10))
         (x (mod n 100))
         (d (mod n 10))
         (c (- x d))
         )
    (if (> q 0) (progn
                  (push (gethash q num-string) s)
                  (push (gethash 1000 num-string) s)
                  (if (or (> x 0) (> h 0)) (push "and" s))
                  ))
    (if (> h 0) (progn
                  (push (gethash h num-string) s)
                  (push (gethash 100 num-string) s)
                  (if (> x 0) (push "and" s))
                  ))
    (if (> x 20) (progn
                   (push (gethash d num-string) s)
                   (push (gethash c num-string) s)
                   )
      (push (gethash x num-string) s))
    (block nil (return s))
    ))

(defun calc-one-num (n)
  (let ((s (number-to-string n)))
    (block nil (return (reduce '+ (mapcar 'length s))))
    ))

(defun calc-all-num (m)
  (let ((s 0))
    (loop
      for i from 1 to m
      do
      (setf s (+ s (calc-one-num i)))
      )
    (block nil (return s))))

(ql:quickload "alexandria")

(defun main-combinatoric (n x)
  (let ((s 0))
    (loop for i from 1 to n
          do
      (loop for j from 0 to i
        for k = (alexandria:binomial-coefficient i j)
        do
        (if (> k x) (incf s))))
    (block nil (return s))))

(ql:quickload "cl-ppcre")

(defun read-base-exp-file (f)
  (let ((i 1) (h nil))
    (setf in (open f))
    (when in
      (loop for line = (read-line in nil)
            for num-list = (read-line-integer line)
            while num-list
            do 
            (push i num-list)
            (push num-list h)
            (incf i)
            )
      (close in))
    (block nil (return h)) 
    ))

(defun read-line-integer (line)
  (mapcar #'parse-integer 
          (cl-ppcre:all-matches-as-strings "(\\d+)" line))
  )

(defun calc-base-exp (nbase nexp) (* nexp (log nbase 2)))

(defun main-base-exp (f)
  (let ((h (read-base-exp-file f)))
    (setf s (mapcar (lambda (x) 
                      (list (car x) 
                            (calc-base-exp (cadr x) (caddr x)))) h))
    (setf s (sort s #'(lambda (x y) (> (cadr x) (cadr y)))))
    (block nil (return (car (car s))))))

(print (main-base-exp "d:\\p99-base_exp.txt"))

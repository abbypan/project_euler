(ql:quickload "cl-ppcre")

(defun read-line-integer (line)
  (mapcar #'parse-integer 
          (cl-ppcre:all-matches-as-strings "(\\d+)" line))
  )

(defun read-mat-file (f)
  (let ((i 0) (h nil))
    (setf in (open f))
    (when in
      (loop for line = (read-line in nil)
            for num-list = (read-line-integer line)
            while num-list
            do 
            (push num-list h)
            (incf i)
            )
      (close in))
    (setf n (length h))
    (block nil (return (make-array (list n n) :initial-contents (reverse h))))
    ))

(defun get-mat-cell (m i j) (aref m i j))
(defun set-mat-cell (m i j v) (setf (aref m i j) v))

(defun calc-cell-min-value (val-mat min-mat i j)
  (cond
    ((and (= i 0) (= j 0))  (get-mat-cell val-mat 0 0))
    ((= i 0) (+ (get-mat-cell min-mat 0 (- j 1)) (get-mat-cell val-mat 0 j)))
    ((= j 0) (+ (get-mat-cell min-mat (- i 1) 0) (get-mat-cell val-mat i 0)))
    (t  (min
          (+ (get-mat-cell min-mat (- i 1) j) (get-mat-cell val-mat i j))
          (+ (get-mat-cell min-mat i (- j 1)) (get-mat-cell val-mat i j))
          ))))

(defun main-mat-min (f)
  (let* ((val-mat (read-mat-file f))
         (n (array-dimension val-mat 0))
         (min-mat (make-array (list n n))))
    (decf n)
    (loop for m from 0 upto (* 2 n)
          for k = (min m n)
          for p = (max 0 (- m n))
          do
          (loop for i from p upto k
                for j = (- m i)
                for min-cell-v = (calc-cell-min-value val-mat min-mat i j)
                do
                (format t "m ~A, i ~A, j ~A,min ~A~%" m i j min-cell-v)
                (set-mat-cell min-mat i j min-cell-v)
                ))
    (setf min-d (get-mat-cell min-mat n n))
    (block nil (return min-d))))


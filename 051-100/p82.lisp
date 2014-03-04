(ql:quickload "cl-ppcre")

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

(defun read-line-integer (line)
  (mapcar #'parse-integer 
          (cl-ppcre:all-matches-as-strings "(\\d+)" line))
  )

(defun get-mat-cell (m i j) (aref m i j))
(defun set-mat-cell (m i j v) (setf (aref m i j) v))

(defun main-mat-min (f)
  (let* ((val-mat (read-mat-file f))
         (n (array-dimension val-mat 0))
         (min-mat (make-array (list n n))))
    (decf n)

    (loop for col from 0 upto n
          do
          (init-col-cell val-mat min-mat col n)
          (loop for row from 0 upto n
                do
                (update-up-cell-list val-mat min-mat row col n)
                (update-down-cell-list val-mat min-mat row col n)
                ))

    (block nil (return (get-min-path-sum min-mat)))
    ))

(defun init-col-cell (val-mat min-mat col n)
  (loop for row from 0 upto n
        for min-cell-v = (calc-right-cell-value val-mat min-mat row col)
        do
        (set-mat-cell min-mat row col min-cell-v)))

(defun calc-right-cell-value (val-mat min-mat i j)
  (cond
    ((= j 0)  (get-mat-cell val-mat i 0))
    (t (+ (get-mat-cell min-mat i (- j 1)) (get-mat-cell val-mat i j)))
    ))

(defun update-down-cell-list (val-mat min-mat row col n) 
  (loop for r from row upto n
        for flag = (update-down-cell val-mat min-mat r col n)
        do
        (if (null flag) (return))
        )
  )

(defun update-up-cell-list (val-mat min-mat row col n)
  (loop for r from row downto 0
        for flag = (update-up-cell val-mat min-mat r col n)
        do
        (if (null flag) (return))
        ))

(defun update-up-cell (v-mat m-mat i j n)
  (let ((ni (- i 1)))
    (if (< ni 0) (return-from update-up-cell))
    (block nil (return (update-neighbor-cell v-mat m-mat i j n ni))) 
    ))

(defun update-down-cell (v-mat m-mat i j n)
  (let ((ni (+ i 1)))
    (if (> ni n) (return-from update-down-cell))

    (block nil (return (update-neighbor-cell v-mat m-mat i j n ni))) 
    ))

(defun update-neighbor-cell (v-mat m-mat i j n ni)
  (let ((min-i-j (get-mat-cell m-mat i j)))
    (setf k (get-mat-cell m-mat ni j))
    (setf nk (+ min-i-j (get-mat-cell v-mat ni j)))

    (if (>= nk k) (return-from update-neighbor-cell))
    (set-mat-cell m-mat ni j nk)
    (block nil (return t)) 
    ))

(defun get-min-path-sum (min-mat)
  (let ((result nil) (n (- (array-dimension min-mat 0) 1)))
    (loop for i from 0 upto n
          for j = (aref min-mat i n)
          do
          (push  j result))
    (block nil (return (reduce 'min result)))))

(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")

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


(defun update-up-cell (v-mat m-mat i j)
  (update-neighbor-cell v-mat m-mat i j i (- j 1)))
(defun update-down-cell (v-mat m-mat i j)
  (update-neighbor-cell v-mat m-mat i j i (+ j 1)))
(defun update-left-cell (v-mat m-mat i j)
  (update-neighbor-cell v-mat m-mat i j (- i 1) j))
(defun update-right-cell (v-mat m-mat i j)
  (update-neighbor-cell v-mat m-mat i j (+ i 1) j))

(defun update-cell (v-mat m-mat i j)
  (let ((s nil))
    (push (update-up-cell v-mat m-mat i j) s)
    (push (update-down-cell v-mat m-mat i j) s)
    (push (update-left-cell v-mat m-mat i j) s)
    (push (update-right-cell v-mat m-mat i j) s)
    (block nil (return (remove-if #'null s)))
    ))

(defun get-matrix-colnum (mat)
  (array-dimension mat 0))

(defun check-valid-cell (n i j)
  (and (>= i 0) (>= j 0) (< i n) (< j n)))

(defun update-neighbor-cell (v-mat m-mat src-i src-j dst-i dst-j)
  (if (check-valid-cell (get-matrix-colnum v-mat) dst-i dst-j)
    (let (
          (src-min (get-mat-cell m-mat src-i src-j))
          (dst-min (get-mat-cell m-mat dst-i dst-j))
          (dst-v (get-mat-cell v-mat dst-i dst-j))
          )

      (setf new-min (+ src-min dst-v))
      (if (or (null dst-min) (< new-min dst-min))
        (progn
          (set-mat-cell m-mat dst-i dst-j new-min)
          (block nil (return (list dst-i dst-j)))
          )
        ))))

(defun calc-row-col-sum (c)
  (+ (second c) (first c)))

(defun get-min-path-sum (min-mat)
  (let ((n (- (get-matrix-colnum min-mat) 1)))
    (block nil (return (get-mat-cell min-mat n n)))
    ))

(defun is-different-cell (x y)
  (or (/= (first x) (first y)) (/= (second x) (second y))))

(defun main-mat-min (f)
  (let* ((val-mat (read-mat-file f))
         (n (get-matrix-colnum val-mat))
         (min-mat (make-array (list n n)))
         (max-index (- n 1))
         (c (list 0 0))
         (cell-list (list c))
         (last-cell (list -1 -1))
         )

    (set-mat-cell min-mat 0 0 (get-mat-cell val-mat 0 0))

    (loop 
      while cell-list
      do
      (setf c (pop cell-list))
      (if (is-different-cell c last-cell)
        (progn
          (setf update-cells (update-cell val-mat min-mat (first c) (second c)))
          (if update-cells (alexandria:appendf cell-list update-cells))

          (setf cell-list 
                (sort cell-list 
                      (lambda (x y)
                        (< (calc-row-col-sum x) (calc-row-col-sum y)))))
          (setf last-cell c)
          ))
      )
    ;(print min-mat)
    (block nil (return (get-min-path-sum min-mat)))
    ))

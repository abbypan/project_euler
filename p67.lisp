(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")

(defvar *trig-number* (make-hash-table))

(defvar trig-max-path (make-hash-table))

(defun init-trig-max-path (i)
  (let ((x (make-hash-table)))
    (setf (gethash 'len x) 0)
    (setf (gethash 'path x) '())
    (setf (gethash i trig-max-path) 
          (make-array i :initial-element x))))

(defun get-trig-number (i j) (nth j (gethash i *trig-number*) ))
(defun get-max-path-len (i j) (gethash 'len (aref (gethash i trig-max-path) j) ) )
(defun get-max-path (i j) (gethash 'path  (aref (gethash i trig-max-path) j) ) )
(defun find-max-path (i j)
  (let ((tj 0) (ti (- i 1)) (pv (make-hash-table)) (p '()) (n (get-trig-number i j)) )
    (if (> (get-max-path-len i j) 0) (return (get-max-path i j)))
    (if (or (= i 1) (= j 0)) (setf tj 0)
      (if (= j ti) (setf tj (- j 1))
        (setf tj (if (> 
                       (get-max-path-len ti (- j 1)) 
                       (get-max-path-len ti j)) (- j 1) j)) 
        ))
    (if (= i 1) (setf ti 1))

    (setf (gethash 'len pv) (+ (get-max-path-len ti tj) n))
    (setf p (get-max-path ti tj))
    (push n p)
    (setf (gethash 'path pv) p)
    (setf (aref (gethash i trig-max-path) j) pv) 
    (block nil (return p))))

(defun main-max-path (f)
  (let ((p '()) (len 0) (in (open f)) (i 0))
    (when in
      (loop for line = (read-line in nil)
            while line do 
            (incf i)
            (init-trig-max-path i)
            (setf temp-s  (mapcar #'parse-integer 
                                  (cl-ppcre:all-matches-as-strings "(\\d+)" line)))
            (setf (gethash i *trig-number*) temp-s)
            (loop
              for j from 0 to (- i 1)
              for plen = 0
              for tp = '()
              do
              (setf tp (find-max-path i j)) 
              (setf plen (get-max-path-len i j))
              (if (> plen len) (progn (setf len plen) (setf p tp)))))
      (close in))
    (print p)
    (block nil (return (reduce '+ p)))
    ))

(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(defun is35n (n)
(cond  ((= (mod n 3) 0)  n)
        ((= (mod n 5) 0)  n)
        (t 0)))

(reduce  '+  (mapcar #'is35n (upto 10)))


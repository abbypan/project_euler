(defun is-prime (n) 
  (progn
    (if (< n 2) (return-from is-prime))
    (loop for i from 2 to (floor (sqrt n))
          do 
          (if (= (mod n i) 0) (return-from is-prime)))
    (block nil (return t))))

(defun merge-number (a b)
  (let ((n (floor (log b 10))))
    (block nil 
           (return (+ b (* a (expt 10 (+ n 1))))))))

(defvar memoize-prime (make-hash-table))
(defun is-prime-memoize (n)
  (progn
    (if (null (gethash n memoize-prime)) 
      (setf (gethash n memoize-prime)
            (if (is-prime n) 1 0))
      )
    (block nil 
           (return 
             (if (> (gethash n memoize-prime) 0) t nil)))))

(defun is-prime-pair (a b)
  (and (is-prime-memoize (merge-number a b)) (is-prime-memoize (merge-number b a))) 
  )

(defun is-prime-set (plist b)
  (every (lambda (a) (is-prime-pair a b)) plist))

(defun update-prime-set (pset p)
  (let ((prime-set nil))
    (loop for plist in pset
          do
          (if (is-prime-set plist p) 
            (push (push p plist) prime-set)))
    (push (list p) prime-set)
    (block nil (return prime-set))))

(defun main-prime-set (n)
  (let ((prime-set nil) 
        (i-prime-set nil) (i 2))
    (loop while (> i 0)
          do
          (if (is-prime-memoize i) 
            (progn
              (setf i-prime-set (update-prime-set prime-set i))
              (loop for i-p-set in i-prime-set
                    for j = (length i-p-set)
                    do
                    (if (= j n) 
                      (block nil (return-from main-prime-set (reduce '+ i-p-set)))))
              (alexandria:appendf prime-set i-prime-set)))
          (incf i))))

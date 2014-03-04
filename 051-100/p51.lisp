(ql:quickload "alexandria")

(defun main-pass-number (replace-prime-cnt)
  (let ((i 2)  
        (h (make-hash-table)))
    (loop
      for j = (pass-number-factor h i)
      do
      (if (= j 1) 
        (progn 
          (setf dh (destruct-digit i))
          (loop for k in (alexandria:hash-table-keys dh)
                for v  = (gethash k dh)
                do
                (if (>= (- 9 k) replace-prime-cnt)
                  (progn 
                    (setf kvd (replace-digit i k v))
                    (loop for kvlist in kvd
                          do
                          (if (is-m-prime replace-prime-cnt kvlist)
                            (return-from main-pass-number i)
                            )))))))
      (incf i))
    (block nil (return 0))))

(defun pass-number-factor (h n)
  (let ((nlist (get-number-hash h n)))
    (loop for x in nlist
          for y = (+ n x)
          do
          (update-number-factor h y x))
    (block nil (return (if (= n (car nlist)) 1 0) ))))


(defun get-number-hash (h n)
  (let ((nlist (gethash n h)))
    (if (null nlist) 
      (setf nlist (update-number-factor h n n)))
    (block nil (return nlist))))

(defun update-number-factor (h n f)
  (progn
    (if (null (gethash n h)) (setf (gethash n h) (list)))
    (push f (gethash n h))
    (block nil (return (gethash n h)))))

(defun destruct-digit (n)
  (let ((i 0) (h (make-hash-table)))
    (loop while (> n 0)
          do
          (setf d (mod n 10))
          (push i (gethash d h))
          (setf n (floor (/ n 10)))
          (incf i))
    (block nil (return h))))

(defun replace-digit (n d dlist)
  (let ((all-dlist (all-subset dlist))
        (res '()))
    (loop for nlist in all-dlist
          for nbase = (calc-base nlist)
          for v = '()
          for x = n
          do
          (loop for m from (+ d 1) to 9
                do
                (setf x (+ x nbase))
                (push x v))
          (push (sort v #'<) res))
    (block nil (return res))))

(defun all-subset (nlist)
  (let ((v '()) (res '()))
    (push (list '() nlist) v)
    (loop while (not (null v))
          do
          (setf this (pop v))
          (setf next-list (next-subset this))
          (loop for nl in next-list
                for nt = (cadr nl)
                for head = (car nl)
                do
                (if (null nt) 
                  (if (not (null head)) (push head res))  
                  (push nl v))))
    (block nil (return res))))

(defun next-subset (this-list)
  (let* ((head (car this-list))
         (tail (cadr this-list))
         (n (car tail))
         (v '()))
    (pop tail)
    (push (list (copy-list head) (copy-list tail)) v)
    (push (list (append (copy-list head) (list n))  (copy-list tail)) v)
    (block nil (return v))))

(defun calc-base (nlist) (reduce '+ (mapcar (lambda (x) (expt 10 x)) nlist )))

(defun is-m-prime (m nlist)
  (let ((s (length nlist)))
    (loop for n in nlist
          while (>= s m)
          do
          (if (is-prime n) (decf m))
          (decf s))
    (block nil (return (= m 0)))))

(defun is-prime (n)
  (let ((x (floor (sqrt n))))
    (if (<  n 2) (return-from is-prime))
    (loop for i from 2 upto x
          do
          (if (= (mod n i) 0) 
            (return-from is-prime)))
    (block nil (return t))))

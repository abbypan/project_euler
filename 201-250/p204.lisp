;http://projecteuler.net/thread=204;page=4 
;ilyushkin 的 haskell 解法比较漂亮

(defun set-proc (n i)
  (let ((h (make-hash-table)))
    (setf (gethash 'n h) n)
    (setf (gethash 'i h) i)
    (block nil (return h))
    ))

(defun get-result (ph) (gethash 'n ph))

(defun next-proc-module (ph p max_n)
  (let* (
         (n (gethash 'n ph))
         (i (gethash 'i ph))
         (new_n (* (nth i p) n))
         )
    (block nil (return 
                 (if (<= new_n max_n) (set-proc new_n i) nil)
                 ))
    ))

(defun is-stop-proc-module (ph p max_n mem)
  (let* (
         (n (gethash 'n ph))
         (i (gethash 'i ph))
         ;(m (gethash i mem))
         )

    ;(if (and m (>= n m)) (return-from is-stop-proc-module t))

    (setf flag (or (null p) 
                   (>= i (- (length p) 1))
                   (> n max_n)
                   (> (* (nth i p) n) max_n)
                   (> (* (nth (+ i 1) p) n) max_n)
                   ))
    ;(if (and flag (or (null m) (< n m))) (setf (gethash i mem) n))
    (block nil (return flag))
    ))

(defun prepare-step-proc (ph)
  (incf (gethash 'i ph))
  )

(defun main-hamming (pmax nmax)
  (let* ((plist (generate-prime-to-n pmax))
         (start (set-proc 1 -1))
         (stop-mem (make-hash-table))
         (next-proc (lambda (ph) (next-proc-module ph plist nmax)))
         (stop-proc (lambda (ph) (is-stop-proc-module ph plist nmax stop-mem)))
         )
    (setf result (main-proc start stop-proc next-proc))
    ;(setf res (sort result '<))
    ;(print res)
    (block nil (return (length result)))
    ))
;---------

(defun init-proc (ph)
  (let ((proc '()) 
        (result '())
        (init (make-hash-table)))
    (push ph proc)
    (setf (gethash 'proc init) proc)
    (setf (gethash 'result init) result)
    (block nil (return init))
    ))


(defun step-proc (hv is-stop-proc next-proc)
  (let* ((proc (gethash 'proc hv))
         (result (gethash 'result hv))
         (h (pop proc))
         )
    (prepare-step-proc h)

    (setf stop-flag nil)
    (loop 
      while h
      do
      ;(print h)
      ;(print proc)
      ;(print result)
      ;(print "----------")
      (if (or stop-flag (funcall is-stop-proc h))
        (progn 
          (push (get-result h) result)
          (setf stop-flag t)
          )
        (push h proc)
        )
      (setf h (funcall next-proc h))
      )

    (setf (gethash 'proc hv) proc)
    (setf (gethash 'result hv) result)


    (block nil (return hv))
    ))

(defun main-proc (ph stop-proc next-proc)
  (let* (
         (main (init-proc ph))
         )
    (loop
      for proc = (gethash 'proc main) 
      while proc
      do
      (step-proc main stop-proc next-proc)
      )
    (setf result (gethash 'result main))
    (block nil (return result))
    ))

;------------

(defun is-prime-to (s m)
  (if (null s) t
    (every (lambda (n) (/= (mod m n) 0)) s)
    ))

(defun next-prime (s)
  (let ((x (car  s)))
    (loop
      while t
      do
      (if (is-prime-to s x) (return)) 
      (incf x)
      )
    (block nil (return x))
    ))

(defun generate-prime-to-n (n)
  (let ((s '(2)))
    (loop
      for p = (next-prime s)
      while (<= p n)
      do
      (push p s)
      )
    (block nil (return (reverse s)))
    ))


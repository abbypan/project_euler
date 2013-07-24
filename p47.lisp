; 这道题花了3天才想出来，汗
; 算法如下：
; 从 i = 2 开始，往后遍历自然数 (step = 1)
;    假设当前自然数 i 的质因子集合 (P1 .. Pm)，遍历该集合，依次标记自然数 (i + Px) 存在一个质因子 Px
;      1) 如果以前没标记过 i，那么 i 是个质数，其质因子集合为 (i)
;      2) 如果以前已标记过 i，那么 i 是个合数，其质因子集合在遍历 ( i - Px ) 时已被更新
;    由于每次对 i 做标记时，都能更新一个 i 的质因子 Px，因此最终必然能够筛出所有的质因子集合，而不用做乘法or除法

(ql:quickload "alexandria")
(defvar number-factor-hash (make-hash-table))

(defun update-number-factor (h n f)
  (progn
    (if (null (gethash n h)) (setf (gethash n h) (list)))
    (push f (gethash n h))
    (block nil (return (gethash n h))) 
    ))

(defun get-number-hash (h n)
  (let ((nlist (gethash n h)))
    (if (null nlist) 
      (setf nlist (update-number-factor h n n)))
    (block nil (return nlist))
    )
  )

(defun pass-number-factor (h n)
  (let ((nlist (get-number-hash h n)))
    (loop
      for x in nlist
      for y = (+ n x)
      do
      (update-number-factor h y x)
      )
    (block nil (return (length nlist)))
    )
  )

(defun main-pass-number (factor-num factor-cnt)
  (let ((s 0) (i 2) (h (make-hash-table)))
    (loop
      for j = (pass-number-factor h i)
      while (< s factor-cnt)
      do
      (if (= j factor-num) (incf s) (setf s 0))
      (incf i)
      )
    (block nil (return (- i factor-cnt))) 
    )
  )

(print (main-pass-number 4 4)) 

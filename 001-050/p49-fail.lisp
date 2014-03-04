(defun select-n (nlist)
  (let ((v '()) (head (car nlist)) (tail (cadr nlist)))
    (setf n (pop tail))
    (push (list head tail) v)
    (setf  newsrc (copy-list head))
    (push  n newsrc)
    (setf newlist (copy-list tail))
    (push (list newsrc newlist) v)
    (block nil (return v))
    ))

(defun merge-seq (nlist) (append (car nlist) (cadr nlist)))

(defun select-nlist (nlist n)
  (let ((s (list (list '() nlist)) ) 
        (r '()))
    (loop
      while (> (length s) 0)
      do
      (setf x (pop s))
      (loop
        for y in (select-n x)
        for hlen = (length (car y))
        for tlen = (length (cadr y))
        for len = (+ hlen tlen)
        do
        (if (= hlen n) (push (car y) r) 
          (if (= len n) (push (merge-seq y) r)
            (if (> len n) (push y s))
            ))))
    (block nil (return r))))

(defun check-seq-prime (nlist)
  
  )

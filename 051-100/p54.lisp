(ql:quickload "alexandria")
(ql:quickload "cl-ppcre")

(defstruct poker value color)
(defstruct pokergroup group-value value color poker-list)

(defun is-royal-flush (sorted-poker-list)
  (let ( (r (is-five-flush sorted-poker-list)))
    (if (and r (= (pokergroup-value r) 14))
      (progn
        (setf (pokergroup-group-value r) 10)
        (return-from is-royal-flush r)))))

(defun is-straight-flush (sorted-poker-list)
  (let ( (r (is-five-flush sorted-poker-list)))
    (if (and r (< (pokergroup-value r) 14))
      (progn
        (setf (pokergroup-group-value r) 9)
        (return-from is-straight-flush r)))))

(defun is-four-kind (sorted-poker-list)
  (let ((r (is-duplicate-kind sorted-poker-list 4)))
    (if r (setf (pokergroup-group-value r) 8))
    (block nil (return r))))

(defun is-full-house (sorted-poker-list)
  (let ((k3 (is-three-kind sorted-poker-list))
        (k2 (is-one-pair sorted-poker-list)))
    (if (and k3 k2)
      (progn
        (setf (pokergroup-group-value k3) 7)
        (setf (pokergroup-group-value k2) 7)
        (block nil (return (list k3 k2)))))))

(defun is-full-poker (sorted-poker-list)
  (/= 5 (length sorted-poker-list)) 
  )

(defun is-flush (sorted-poker-list)
  (let* (
         (head (car sorted-poker-list)) 
         (color (poker-color head)))
    (if (is-full-poker sorted-poker-list) (return-from is-flush))
    (loop
      for p in sorted-poker-list
      for pc = (poker-color p)
      do
      (if (not (equal color pc))
        (return-from is-flush)))
    (block nil 
           (return (make-pokergroup 
                     :group-value 6
                     :value 0
                     :color color 
                     :poker-list sorted-poker-list)))))

(defun is-straight (sorted-poker-list)
  (let* ((head (car sorted-poker-list)) 
         (value (poker-value head)))

    (if (is-full-poker sorted-poker-list) (return-from is-straight))
    (loop
      for v from value downto (- value 4)
      for p in sorted-poker-list
      for pv = (poker-value p)
      do
      (if (/= pv v)
        (return-from is-straight)))
    (block nil (return (make-pokergroup 
                         :group-value 5
                         :value value 
                         :poker-list sorted-poker-list)))))

(defun is-five-flush (sorted-poker-list)
  (let* ((s (is-straight sorted-poker-list))
         (f (is-flush sorted-poker-list)))
    (if (and s f) (progn
                    (setf (pokergroup-color s) (pokergroup-color f))
                    (return-from is-five-flush s)))))

(defun is-three-kind (sorted-poker-list)
  (let ((r (is-duplicate-kind sorted-poker-list 3)))
    (if r (setf (pokergroup-group-value r) 4))
    (block nil (return r))))

(defun is-two-pairs (sorted-poker-list)
  (let ((r (is-one-pair sorted-poker-list)))
    (if (null r) (return-from is-two-pairs))
    (setf second-s 
          (set-difference
            sorted-poker-list 
            (pokergroup-poker-list r)))
    (setf sr (is-one-pair second-s))
    (if (null sr) (return-from is-two-pairs))
    (setf (pokergroup-group-value r) 3)
    (setf (pokergroup-group-value sr) 3)
    (block nil (return (list r sr)))))

(defun is-one-pair (sorted-poker-list)
  (let ((r (is-duplicate-kind sorted-poker-list 2)))
    (if r (setf (pokergroup-group-value r) 2))
    (block nil (return r))))

(defun high-card (sorted-poker-list)
  (let* (
         (head (car sorted-poker-list))
         (high-value (poker-value head))
         )
    (block nil (return (make-pokergroup 
                         :group-value 1
                         :value high-value
                         :poker-list (list head))))))

(defun is-duplicate-kind (sorted-poker-list dup)
  (let ((h (make-hash-table)))
    (loop for p in sorted-poker-list
          for v = (poker-value p)
          do
          (push p (gethash v h)))
    (loop
      for k being the hash-keys of h
      for s being the hash-values of h
      do
      (if (= (length s) dup) 
        (return-from is-duplicate-kind
                     (make-pokergroup :value k :poker-list (reverse s))
                     )))))

(defun compare-poker-group (x y)
  (if (> (pokergroup-group-value x) (pokergroup-group-value y))
    (return-from compare-poker-group 1)
    (if (< (pokergroup-group-value x) (pokergroup-group-value y))
      (return-from compare-poker-group -1)
      (if (> (pokergroup-value x) (pokergroup-value y))
        (return-from compare-poker-group 1)
        (if (< (pokergroup-value x) (pokergroup-value y))
          (return-from compare-poker-group -1)
          (return-from compare-poker-group 0)
          )
        )
      )))

(defun compare-poker-list (pa pb)
  (let ((ga (parse-poker-group pa)) (gb (parse-poker-group pb)))
    (mapcar 
      (lambda (x y) 
        (let ((r (compare-poker-group x y)))
          (if (equal r 1) (return-from compare-poker-list t)
            (if (equal r -1) (return-from compare-poker-list nil))
            )
          ))
      ga gb)
    (block nil (return nil))
    ))


(defun parse-poker-group (poker-list)
  (let (
        (s (sort-poker-list poker-list))
        (s (copy-list poker-list))
        (result nil)
        (funcs '(is-royal-flush is-straight-flush is-four-kind is-full-house is-flush is-straight is-three-kind is-two-pairs is-one-pair high-card)
               ))

    (loop
      while s
      do
      (loop for func in funcs
            for r = (funcall func s)
            do
            (if r (progn
                    (setf ds (if (equal 'pokergroup (type-of r)) (list r) r))
                    (mapcar (lambda (x) (push x result)) ds)
                    (loop for d in ds
                          do
                          (setf s (remove-poker-list s 
                                                     (pokergroup-poker-list d))))))
            (if (null s) (return))))
    (block nil (return (sort result #'compare-poker-group)))))


(defun sort-poker-list (s)
  (sort s (lambda (x y) (>= (poker-value x) (poker-value y)))))

(defun same-poker (x y)
  (and
    (= (poker-value x) (poker-value y))
    (equal (poker-color x) (poker-color y))
    ))

(defun remove-poker-list (s ds)
  (let ((rs nil) (i 0))
    (loop 
      for x in s
      for d = (nth i ds)
      do
      (if (and d (same-poker x d)) (incf i)
        (push x rs))
      )
    (block nil (return (reverse rs)))
    ))


(defun convert-poker-value (x)
  (cond ((equal x "J") 11)
        ((equal x "Q") 12)
        ((equal x "K") 13)
        ((equal x "A") 14)
        ((equal x "T") 10)
        (t (parse-integer x))))

(defun read-poker (str)
  (let ((s (cl-ppcre:all-matches-as-strings "." str)))
    (block nil (return
                 (make-poker
                   :value (convert-poker-value (first s))
                   :color (second s)
                   )))))

(defun compare-poker-str (str)
  (let ((s (mapcar #'read-poker 
                   (cl-ppcre:all-matches-as-strings "\\S+" str))))
    (setf vx (mapcar (lambda (i) (nth i s)) (list 0 1 2 3 4)))
    (setf vy (mapcar (lambda (i) (nth i s)) (list 5 6 7 8 9)))
    (block nil (return (compare-poker-list vx vy)))
    ))


(defun count-player-win (f)
  (let ((in (open f)) (i 0))
    (when in
      (loop for x = (read-line in nil)
            while x do 
            (if (compare-poker-str x) (incf i))
            )
      (close in)
      )
    (block nil (return i))
    ))

(print (count-player-win "p54-poker.txt"))

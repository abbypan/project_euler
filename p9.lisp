(defun pythagorean (k)
    (loop
      for x from 1 to k
      do
      (loop
        for y from x to k
        for n = (+ (* x x) (* y y))
        for z = (- 1000 (+ x y))
        for m = (* z z)
        do
        (if (and (> z y) (= m n))
          (return-from pythagorean (* x y z)))
        ))
    )

(pythagorean 1000)


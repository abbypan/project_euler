(ql:quickload "alexandria")

(defvar base (expt 10 11))

(print (mod (reduce '+ 
                    (mapcar (lambda (x) (mod (expt x x) base)) 
                            (alexandria:iota 1000 :start 1))) base))

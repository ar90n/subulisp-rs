(
(def fibo (n) (if (<= (n) 1.0) 1.0 (+ (fibo (- (n) 1.0)) (fibo (- (n) 2.0)))))
(fibo 10.0)
)
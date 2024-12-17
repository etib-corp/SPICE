(define (abc a b n) (if (< n 1) a (abc b (+ a b) (- n 1))))
(define (fib n) (abc 0 1 n))
(fib 10)

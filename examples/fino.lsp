(define (fibit a b n) (if (< n 1) a (fibit b (+ a b) (- n 1))))
(define (fib x) (fibit 0 1 x))
(fib 10)

(define (fibit a b n) (if (< n 1) a (fibit b (+ a b) (- n 1))))
(fibit 1 2 10)
#lang sicp

(define (sqr x)
    (* x x))

(define (min a b)
    (if (< a b) a b))

(define (sum-of-sqr-of-largest-2 a b c)
    (- (+ (sqr a) (sqr b) (sqr c))
        (sqr (min (min a b) c))))

(sum-of-sqr-of-largest-2 1 2 3)         ; should output 13
(sum-of-sqr-of-largest-2 1 1 -1)        ; should output 2

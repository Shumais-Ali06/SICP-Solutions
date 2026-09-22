#lang sicp

; Exercise 1.3:
; Define a procedure that takes three numbers as arguments and returns the sum
; of the squares of the two larger numbers.

(define (sqr x)
    (* x x))

(define (min a b)
    (if (< a b) a b))

; Implementation: Add the squares of all 3 numbers and subtract the square of
; the smallest number from it
(define (sum-of-sqr-of-largest-2 a b c)
    (- (+ (sqr a) (sqr b) (sqr c))
        (sqr (min (min a b) c))))

(sum-of-sqr-of-largest-2 1 2 3)         ; should output 13
(sum-of-sqr-of-largest-2 1 1 -1)        ; should output 2

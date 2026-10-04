#lang sicp

; Exercise 2.7:
; Alyssa’s program is incomplete because she has not specified the implementation
; of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

; Define selectors `upper-bound` and `lower-bound` to complete the
; implementation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; By using the min-max procedures, we don't have to care about the order

(define (upper-bound x)
    (max (car x)
         (cdr x)))

(define (lower-bound x)
    (min (car x)
         (cdr x)))

; Tests
(upper-bound (make-interval -1 1))
(lower-bound (make-interval 9 0))

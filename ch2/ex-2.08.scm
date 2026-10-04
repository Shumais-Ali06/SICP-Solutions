#lang sicp

; Exercise 2.8:
; Using reasoning analogous to Alyssa’s, describe how the difference of two
; intervals may be computed. Define a corresponding subtraction procedure,
; called `sub-interval`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-interval a b)
    (cons a b))

(define (upper-bound x)
    (max (car x)
         (cdr x)))

(define (lower-bound x)
    (min (car x)
         (cdr x)))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

; Newly defined procedures

(define (negate-interval x)
    (make-interval (- (lower-bound x))
                   (- (upper-bound x))))

(define (sub-interval x y)
    (add-interval x
                  (negate-interval y)))

(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display ",")
    (display (upper-bound x))
    (display ")"))

; Tests
(print-interval (sub-interval (make-interval 0 1)
                              (make-interval 0 1)))
(newline)
(print-interval (sub-interval (make-interval 2 3)
                              (make-interval 1 1)))

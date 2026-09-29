#lang sicp

; Exercise 1.32 (a):
; Show that `sum` and `product` (Exercise 1.31) are both special cases of a
; still more general notion called `accumulate` that combines a collection of
; terms, using some general accumulation function:
;
; (accumulate combiner null-value term a next b)
;
; `accumulate` takes as arguments the same term and range specifications as
; `sum` and `product`, together with a `combiner` procedure (of two arguments)
; that specifies how the current term is to be combined with the accumulation of
; the preceding terms and a `null-value` that specifies what base value to use
; when the terms run out. Write `accumulate` and show how `sum` and `product`
; can both be defined as simple calls to `accumulate`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recursive implementation of `accumulate`
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner
                              null-value
                              term
                              (next a)
                              next
                              b))))

; `sum` defined in terms of `accumulate` with `+` as the `combiner` and 0 as
; the `null-value`
(define (sum term a next b)
    (accumulate + 0 term a next b))

; `product` defined in terms of `accumulate` with `*` as the `combiner` and 1
; as the `null-value`
(define (product term a next b)
    (accumulate * 1 term a next b))

; Procedure for `term`
(define (identity x)
    x)

; Procedure for `next`
(define (inc x)
    (+ x 1))

; Tests
(display "Sum of integers from 1 to 10: ")
(sum identity 1 inc 10)

(display "Product of integers from 1 to 10: ")
(product identity 1 inc 10)

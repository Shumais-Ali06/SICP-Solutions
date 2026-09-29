#lang sicp

; Exercise 1.32 (b):
; If your `accumulate` procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Iterative implementation of `accumulate`
(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))

    (iter a null-value))

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

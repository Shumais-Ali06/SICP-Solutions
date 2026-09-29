#lang sicp

; Exercise 1.31 (b):
; If your `product` procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `product` written as an iterative procedure
(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))

    (iter a 1))

(define (inc n)
    (+ n 1))

(define (identity n)
    n)

(define (factorial n)
    (product identity 1 inc n))

; Tests
(display "Computing factorials from n = 0 to 5")
(newline)
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

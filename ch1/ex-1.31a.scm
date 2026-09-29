#lang sicp

; Exercise 1.31 (a):
; The `sum` procedure is only the simplest of a vast number of similar
; abstractions that can be captured as higher-order procedures. Write an
; analogous procedure called `product` that returns the product of the values of
; a function at points over a given range. Show how to define `factorial` in
; terms of product. Also use `product` to compute approximations to π using the
; formula
;
;  π       2 * 4 * 4 * 6 * 6 * 8 ...
; --- = -------------------------------
;  4       3 * 3 * 5 * 5 * 7 * 7 ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `product` written as a recursive procedure
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

(define (inc n)
    (+ n 1))

(define (identity n)
    n)

(define (factorial n)
    (product identity 1 inc n))

(display "Computing factorials from n = 0 to 5")
(newline)
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(newline)

; Now let's write the `product` function to compute pi using Wallis formula

; Multiplying both sides of the provided equation and rearranging, we get:
;
;  π    ( 2     2 )   ( 4     4 )   ( 6     6 )
; --- = (--- * ---) * (--- * ---) * (--- * ---) * ...
;  2    ( 1     3 )   ( 3     5 )   ( 5     7 )
;
; Thus, π = prod_{k=1}^{n} ((2k)^2 / ((k - 1)(k + 1)))
;         = prod_{k=1}^{n} (4k^2 / (4k^2 - 1))
;
; as n -> infinity

(define (square x)
    (* x x))

(define (compute-pi-wallis n)
    (define (term k)
        (/ (* 4 (square k))
           (- (* 4 (square k)) 1)))

    (convert-to-float
        (* 2 (product term 1 inc n))))

; The `compute-pi-wallis` procedure produces a rational number with extremely
; large numerators and denominators for large values of `n`. This procedure
; converts the final result to a floating point number for display.
(define (convert-to-float x)
    (* 1.0 x))

(display "Computing pi using Wallis formula with n = 1, 10, 100, 1000, 10000")
(newline)
(compute-pi-wallis 1)
(compute-pi-wallis 10)
(compute-pi-wallis 100)
(compute-pi-wallis 1000)
(compute-pi-wallis 10000)

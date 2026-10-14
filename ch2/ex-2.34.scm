#lang sicp

; Exercise 2.34:
; Evaluating a polynomial in `x` at a given value of `x` can be formulated as an
; accumulation. We evaluate the polynomial
;
;       a_{n}x^n + a_{n-1}x^(n-1) + ... + a_{1}x + a_{0}
;
; using a well-known algorithm called Horner’s rule, which structures the
; computation as
;
;       (...(a_{n}x + a_{n-1})x + ... + a_{1})x + a_{0}
;
; In other words, we start with `a_{n}` multiply by `x`, add `a_{n-1}`, multiply
; by `x`, and so on, until we reach `a_{0}`.
;
; Fill in the following template to produce a procedure that evaluates a
; polynomial using Horner’s rule. Assume that the coefficients of the polynomial
; are arranged in a sequence, from `a_{0}` through `a_{n}`.

; (define (horner-eval x coefficient-sequence)
;     (accumulate (lambda (this-coeff higher-terms) ⟨??⟩)
;                 0
;                 coefficient-sequence))
;
; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate
;
; (horner-eval 2 (list 1 3 0 5 0 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `accumulate` as defined in Section 2.2.3
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                    (+ this-coeff
                       (* higher-terms x)))
                0
                coefficient-sequence))

; Tests

(horner-eval 2 (list 1 3 0 5 0 1))      ; f(x) = 1 + 3x + 5x^3 + x^5
(horner-eval 1 (list 1 1 1 1))          ; f(x) = 1 + x + x^2 + x^3
(horner-eval 3 (list 4 -4 1))           ; f(x) = 4 - 4x + x^2

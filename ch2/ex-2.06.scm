#lang sicp

; Exercise 2.6:
; In case representing pairs as procedures wasn’t mind-boggling enough, consider
; that, in a language that can manipulate procedures, we can get by without
; numbers (at least insofar as nonnegative integers are concerned) by
; implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

; This representation is known as Church numerals, after its inventor, Alonzo
; Church, the logician who invented the λ-calculus.
;
; Define `one` and `two` directly (not in terms of `zero` and `add-1`).
; (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of
; the addition procedure `+` (not in terms of repeated application of `add-1`).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Church numeral 1: applies f once
(define one
    (lambda (f)
        (lambda (x)
            (f x))))

; Church numeral 2: applies f twice
(define two
    (lambda (f)
        (lambda (x)
            (f (f x)))))

; Adds two Church numerals by combining their applications
(define +
    (lambda (m)
        (lambda (n)
            (lambda (f)
                (lambda (x)
                    ((m f) ((n f) x)))))))

; Test it with a known procedure
(define (double x)
    (* 2 x))

((zero double) 5)
((one double) 3)
((two double) 2)
((((+ one) two) double) 4)

#lang sicp

; Exercise 2.1:
; Define a better version of `make-rat` that handles both positive and negative
; arguments. `make-rat` should normalize the sign so that if the rational number
; is positive, both the numerator and denominator are positive, and if the
; rational number is negative, only the numerator is negative.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (same-sign? a b)
    (or (and (> a 0)
             (> b 0))
        (and (< a 0)
             (< b 0))))

(define (make-rat n d)
    (let ((g (gcd (abs n)
                  (abs d))))
        (cons ((if (same-sign? n d) + -)
               (/ (abs n) g))
              (/ (abs d) g))))

; Tests
(print-rat (make-rat -3 6))
(print-rat (make-rat 0 12))
(print-rat (make-rat -2 -5))
(print-rat (make-rat 4 -4))

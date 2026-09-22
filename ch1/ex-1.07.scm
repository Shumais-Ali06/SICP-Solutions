#lang sicp

; Exercise 1.7:
; The 'good-enough?' test used in computing square roots will not be very
; effective for finding the square roots of very small numbers. Also, in real
; computers, arithmetic operations are almost always performed with limited
; precision. This makes our test inadequate for very large numbers. Explain
; these statements, with examples showing how the test fails for small and large
; numbers. An alternative strategy for implementing 'good-enough?' is to watch
; how 'guess' changes from one iteration to the next and to stop when the change
; is a small fraction of the guess. Design a square-root procedure that uses
; this kind of test. Does this work better for small and large numbers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We reuse the earlier provided definitions from the book

(define (square x)
    (* x x))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Numbers for testing

(define million 1000000)
(define millionth 0.000001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Tests using original 'good-enough?':")
(newline)

(sqrt millionth)
(sqrt (sqrt millionth))
(sqrt (sqrt (sqrt millionth)))

(sqrt million)
(sqrt (square million))
(sqrt (* 16 (square (square million))))

(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Define the new versions of the required functions.
; These are appended with '-v1' to indicate changes

(define (good-enough?-v1 last-guess guess x)
    (< (abs (/ (- guess last-guess) last-guess)) 0.0000000001))

(define (sqrt-iter-v1 last-guess guess x)
    (if (good-enough?-v1 last-guess guess x)
        last-guess
        (sqrt-iter-v1 guess (improve guess x) x)))

; -1.0 is a good value for last-guess since square roots are never negative
(define (sqrt-v1 x)
    (sqrt-iter-v1 -1.0 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Tests using 'good-enough?-v1':")
(newline)

(sqrt-v1 millionth)
(sqrt-v1 (sqrt-v1 millionth))
(sqrt-v1 (sqrt-v1 (sqrt-v1 millionth)))

(sqrt-v1 million)
(sqrt-v1 (square million))
(sqrt-v1 (* 16 (square (square million))))

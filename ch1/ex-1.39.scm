#lang sicp

; Exercise 1.39:
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:
;
;                     x
;  tan x = ------------------------
;                      x^2
;           1 - -----------------
;                        x^3
;                3 - -----------
;                      5 - ...
;
; where `x` is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert’s formula. `k`
; specifies the number of terms to compute, as in Exercise 1.37.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Iterative implementation of the `cont-frac` procedure
(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1)
                  (/ (n i)
                     (+ (d i)
                        result)))))

    (iter k 0))

(define (square x)
    (* x x))

(define (tan-cf x k)
    (cont-frac (lambda (i)
                   (if (= i 1)
                       x
                       (- (square x))))
               (lambda (i)
                   (- (* 2 i)
                      1))
               k))

; Global variable definitions
(define pi 3.1415926535897932384626433)
(define k-iters 100)

; Tests
(display "Computing tanx for x = 0, pi/4, pi/2, 3pi/4 and pi")
(newline)
(tan-cf 0 k-iters)
(tan-cf (/ pi 4) k-iters)
(tan-cf (/ pi 2) k-iters)
(tan-cf (* 3 (/ pi 4)) k-iters)
(tan-cf pi k-iters)

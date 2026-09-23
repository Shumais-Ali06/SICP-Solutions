#lang sicp

; Exercise 1.8:
; Newton's method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by
; the value
;
;   x/y^2 + 2y
; --------------
;        3
;
; Use this formula to implement a cube-root procedure analogous to the square-
; root procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define epsilon 0.00000001)

(define (square x)
    (* x x))

(define (cube x)
    (* x x x))

(define (good-enough? guess x)
    (< (abs (- (cube guess) x)) epsilon))

(define (improve guess x)
    (/ (+ (/ x (square guess))
        (* 2 guess))
        3))

(define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))

(define (cube-root x)
    (cube-root-iter 1.0 x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now it's time to test our function

(define million 1000000)
(define millionth 0.000001)

(cube-root millionth)
(cube-root (cube-root millionth))
(cube-root (cube-root (cube-root millionth)))
(newline)

(cube-root million)
(cube-root (cube million))
(cube-root (* 64 (cube (cube million))))
(newline)

(cube-root 2)
(cube-root 100)
(cube-root 125)

#lang sicp

; Exercise 2.46:
; A two-dimensional vector `\textbf{v}` running from the origin to a point can
; be represented as a pair consisting of an x-coordinate and a y-coordinate.
; Implement a data abstraction for vectors by giving a constructor `make-vect`
; and corresponding selectors `xcor-vect` and `ycor-vect`. In terms of your
; selectors and constructor, implement procedures `add-vect`, `sub-vect`, and
; `scale-vect` that perform the operations vector addition, vector subtraction,
; and multiplying a vector by a scalar:
;
;                  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2),
;                  (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2),
;                          s · (x , y) = (sx, sy).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-vect x y)
    (cons x y))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cdr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))

; Tests

(define (print-vect v)
    (display "(")
    (display (xcor-vect v))
    (display ", ")
    (display (ycor-vect v))
    (display ")")
    (newline))

(define v1 (make-vect 2 3))
(define v2 (make-vect 7 4))

(print-vect (add-vect v1 v2))
(print-vect (sub-vect v1 v2))
(print-vect (scale-vect 2.5 v1))
(print-vect (scale-vect -3 v2))

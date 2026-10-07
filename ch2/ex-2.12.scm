#lang sicp

; Exercise 2.12:
; Define a constructor `make-center-percent` that takes a center and a
; percentage tolerance and produces the desired interval. You must also define a
; selector percent that produces the percentage tolerance for a given interval.
; The `center` selector is the same as the one shown above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Predefined procedures

(define (make-interval a b)
    (cons a b))

(define (upper-bound x)
    (max (car x)
         (cdr x)))

(define (lower-bound x)
    (min (car x)
         (cdr x)))

(define (center x)
    (average (lower-bound x)
             (upper-bound x)))

(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display ",")
    (display (upper-bound x))
    (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average x y)
    (/ (+ x y) 2))

(define (width x)
    (/ (- (upper-bound x)
          (lower-bound x))
       2))

(define (make-center-percent c %-tol)
    (let ((e (abs (* c (/ %-tol 100)))))
        (make-interval (- c e)
                       (+ c e))))

(define (%-tolerance x)
    (abs (/ (width x)
            (center x))))

(define (print-conversion c %-tol)
    (let ((x (make-center-percent c %-tol)))
        (display (center x))
        (display " ± ")
        (display (%-tolerance x))
        (display "% = ")
        (print-interval x)
        (newline)))

; Tests

(print-conversion 2 0.3)
(print-conversion 100 25)
(print-conversion -3.14 0.05)

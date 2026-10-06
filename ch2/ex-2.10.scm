#lang sicp

; Exercise 2.10:
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and
; comments that it is not clear what it means to divide by an interval that
; spans zero. Modify Alyssa’s code to check for this condition and to signal an
; error if it occurs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-interval a b)
    (cons a b))

(define (upper-bound x)
    (max (car x)
         (cdr x)))

(define (lower-bound x)
    (min (car x)
         (cdr x)))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x)
                 (lower-bound y)))
          (p2 (* (lower-bound x)
                 (upper-bound y)))
          (p3 (* (upper-bound x)
                 (lower-bound y)))
          (p4 (* (upper-bound x)
                 (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

; Newly defined procedures

(define (spans-zero? x)
  (and (<= (lower-bound x) 0)
       (>= (upper-bound x) 0)))

(define (div-interval x y)
    (if (spans-zero? y)
        ; TODO: Improve error message
        ; Something like "div-interval: denominator interval (-1,1) spans zero"
        (error "div-interval: denominator interval spans zero" y)
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display ",")
    (display (upper-bound x))
    (display ")"))

; Tests

(print-interval (div-interval (make-interval -2 4)
                              (make-interval  1 2)))

(print-interval (div-interval (make-interval 2 5)
                              (make-interval -1 1)))

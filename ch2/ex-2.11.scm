#lang sicp

; Exercise 2.11:
; In passing, Ben also cryptically comments: “By testing the signs of the
; endpoints of the intervals, it is possible to break `mul-interval` into nine
; cases, only one of which requires more than two multiplications.” Rewrite this
; procedure using Ben’s suggestion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Already defined procedures

(define (make-interval a b)
    (cons a b))

(define (upper-bound x)
    (max (car x)
         (cdr x)))

(define (lower-bound x)
    (min (car x)
         (cdr x)))

; Newly defined procedures

(define (only-positive? x)
    (> (lower-bound x)
       0))

(define (only-negative? x)
    (< (upper-bound x)
       0))

; Each interval may only span all negatives, all postives or include zero.
; Based on this observation, the multiplication can be broken down into 3x3=9
; cases.
(define (mul-interval x y)
    (cond ((only-positive? x)
           (cond ((only-positive? y)
                  (make-interval (* (lower-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y))))
                 ((only-negative? y)
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (lower-bound x) (upper-bound y))))
                 (else
                   ; TODO
                  (make-interval <??>))))
          ((only-negative? x)
           (cond ((only-positive? y)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (lower-bound y))))
                 ((only-negative? y)
                  (make-interval (* (upper-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y))))
                 (else
                   ; TODO
                  (make-interval <??>))))
          (else
            ; TODO
           (cond ((only-positive? y)
                  (make-interval <??>
                                 <??>)
                 ((only-negative? y)
                  (make-interval <??>
                                 <??>)
                 (else
                   ; TODO
                  (make-interval <??>))))

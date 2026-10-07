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

(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display ",")
    (display (upper-bound x))
    (display ")"))

; Newly defined procedures

(define (only-positive? x)
    (> (lower-bound x)
       0))

(define (only-negative? x)
    (< (upper-bound x)
       0))

; Each interval may only span all negatives, all postives or include zero.
; Thus, the multiplication can be broken down into 3x3=9 cases.
(define (mul-interval x y)
    (cond ((only-positive? x)
           (cond ((only-positive? y)
                  (make-interval (* (lower-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y))))
                 ((only-negative? y)
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (lower-bound x) (upper-bound y))))
                 (else
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y))))))
          ((only-negative? x)
           (cond ((only-positive? y)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (lower-bound y))))
                 ((only-negative? y)
                  (make-interval (* (upper-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y))))
                 (else
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y))))))
          (else
           (cond ((only-positive? y)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (upper-bound y))))
                 ((only-negative? y)
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (lower-bound x) (lower-bound y))))
                 (else
                  ; This is the case where we must perform more than 2 multiplications
                  (make-interval (min (* (lower-bound x) (upper-bound y))
                                      (* (upper-bound x) (lower-bound y)))
                                 (max (* (lower-bound x) (lower-bound y))
                                      (* (upper-bound x) (upper-bound y)))))))))

; Tests

(define (print-product x y)
    (print-interval x)
    (display " x ")
    (print-interval y)
    (display " = ")
    (print-interval (mul-interval x y))
    (newline))

(print-product (make-interval 3 5)
               (make-interval 1 2))

(print-product (make-interval 3 5)
               (make-interval -2 -1))

(print-product (make-interval 3 5)
               (make-interval -2 1))

(print-product (make-interval -5 -3)
               (make-interval 1 2))

(print-product (make-interval -5 -3)
               (make-interval -2 -1))

(print-product (make-interval -5 -3)
               (make-interval -2 1))

(print-product (make-interval -5 3)
               (make-interval 1 2))

(print-product (make-interval -5 3)
               (make-interval -2 -1))

(print-product (make-interval -5 3)
               (make-interval -2 1))

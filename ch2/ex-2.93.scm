#lang sicp

; Exercise 2.93:
; Modify the rational-arithmetic package to use generic operations, but change
; `make-rat` so that it does not attempt to reduce fractions to lowest terms.
; Test your system by calling `make-rational` on two polynomials to produce a
; rational function:
;
; (define p1 (make-polynomial 'x '((2 1) (0 1))))
; (define p2 (make-polynomial 'x '((3 1) (0 1))))
; (define rf (make-rational p2 p1))
;
; Now add `rf` to itself, using `add`. You will observe that this addition
; procedure does not reduce fractions to lowest terms.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rational-package)
    ;; internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (cons n d))
    (define (add-rat x y)
        (make-rat (add (mul (numer x) (denom y))
                       (mul (numer y) (denom x)))
                  (mul (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (sub (mul (numer x) (denom y))
                       (mul (numer y) (denom x)))
                  (mul (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (mul (numer x) (numer y))
                  (mul (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (mul (numer x) (denom y))
                  (mul (denom x) (numer y))))

    ;; interface to rest of the system
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
    'done)

; TODO: Add tests

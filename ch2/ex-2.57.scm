#lang sicp

; Exercise 2.57:
; Extend the differentiation program to handle sums and products of arbitrary
; numbers of (two or more) terms. Then the last example above could be expressed
; as
;
; (deriv '(* x y (+ x 3)) 'x)
;
; Try to do this by changing only the representation for sums and products,
; without changing the `deriv` procedure at all. For example, the addend of a
; sum would be the first term, and the augend would be the sum of the rest of
; the terms.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (=number? exp n)
    (and (number? exp) (= exp n)))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp)
           (make-sum (make-product (multiplier exp)
                                   (deriv (multiplicand exp) var))
                     (make-product (deriv (multiplier exp) var)
                                   (multiplicand exp))))
          ; NOTE: The exponentiation operator as defined here only takes two
          ; arguments. Nested applications must be explicitly specified.
          ((exponentiation? exp)
           (make-product (make-product
                          (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum (exponent exp)
                                                         -1)))
                         (deriv (base exp) var)))
          (else
           (error "unknown expression type: DERIV" exp))))

(define (variable? x)
    (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s)
    (cadr s))

; Changed code begins
(define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (cons '+ (cddr s))))
; Changed code ends

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
    (cadr p))

; Changed code begins
(define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (cons '* (cddr p))))
; Changed code ends

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (base x)
    (cadr x))

(define (exponent x)
    (caddr x))

(define (make-exponentiation b p)
    (cond ((=number? p 0) 1)
          ((=number? p 1) b)
          ((and (number? b) (number? p)) (expt b p))
          (else (list '** b p))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tests

; xy(x + 3) --> xy + y(x + 3)
(deriv '(* x y (+ x 3)) 'x)

; 3x^2y + 2x + 3 + y^3 --> 6xy + 2
(deriv '(+ (* 3 (** x 2) y)
           (+ (* 2 x) 3 (** y 3)))
       'x)

; 5x^(y^3 + 27)2^y --> 5(y^3 + 27)x^(y^3 + 26)2^y
(deriv '(* 5
           (** x
               (+ (** y 3) 27))
           (** 2 y))
       'x)

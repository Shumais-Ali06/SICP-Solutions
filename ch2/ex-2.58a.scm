#lang sicp

; Exercise 2.58:
; Suppose we want to modify the differentiation program so that it works with
; ordinary mathematical notation, in which `+` and `*` are infix rather than
; prefix operators. Since the differentiation program is defined in terms of
; abstract data, we can modify it to work with different representations of
; expressions solely by changing the predicates, selectors, and constructors
; that define the representation of the algebraic expressions on which the
; differentiator is to operate.
;
; a. Show how to do this in order to differentiate algebraic expressions
; presented in infix form, such as `(x + (3 * (x + (y + 2))))`. To simplify the
; task, assume that `+` and `*` always take two arguments and that expressions
; are fully parenthesized.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Summary of changes done (compared to Exercise 2.57)
; 1. Predicates check the operator in the middle and builds infix lists.
; 2. Selectors for the first operand now use `car`.

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

; Changed code begins
(define (sum? x)
    (and (pair? x) (eq? (cadr x) '+)))
; Changed code ends

; Changed code begins
(define (addend s)
    (car s))
; Changed code ends

(define (augend s)
    (caddr s))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          ; Changed code begins
          (else (list a1 '+ a2))))
          ; Changed code ends

; Changed code begins
(define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))
; Changed code ends

; Changed code begins
(define (multiplier p)
    (car p))
; Changed code ends

(define (multiplicand p)
    (caddr p))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          ; Changed code begins
          (else (list m1 '* m2))))
          ; Changed code ends

; Changed code begins
(define (exponentiation? x)
    (and (pair? x) (eq? (cadr x) '**)))
; Changed code ends

; Changed code begins
(define (base x)
    (car x))
; Changed code ends

(define (exponent x)
    (caddr x))

(define (make-exponentiation b p)
    (cond ((=number? p 0) 1)
          ((=number? p 1) b)
          ((and (number? b) (number? p)) (expt b p))
          ; Changed code begins
          (else (list b '** p))))
          ; Changed code ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tests

; x + 3(x + y + 2) --> 4
(deriv '(x + (3 * (x + (y + 2)))) 'x)

; xy + 3(x^2 + y + 2) -> 6x + y
(deriv '((x * y) + (3 * ((x ** 2) + (y + 2)))) 'x)

; 3x^2y + 2x + y^3 --> 6xy + 2
(deriv '(((3 * ((x ** 2) * y))
           + (2 * x)) + (y ** 3)) 'x)

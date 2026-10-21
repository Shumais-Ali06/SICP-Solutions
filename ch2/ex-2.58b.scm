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
; b. The problem becomes substantially harder if we allow standard algebraic
; notation, such as `(x + 3 * (x + y + 2))`, which drops unnecessary parentheses
; and assumes that multiplication is done before addition. Can you design
; appropriate predicates, selectors, and constructors for this notation such
; that our derivative program still works?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Newly added procedures

; As defined in the book
(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

; Returns the element when the argument is a one-item list, and otherwise
; returns the list unchanged.
(define (unwrap-singleton x)
    (if (null? (cdr x))
        (car x)
        x))

; NOTE: The selectors split at the first occurrence of the operator. That makes
; chains of `+` and `*` right-associative in the parsed structure, but that’s
; equivalent for these associative operations.

; Grab everything up to but not including to the first occurence of `op`
(define (left-operand exp op)
    (define (iter w)
        (if (eq? (car w) op)
            nil
            (cons (car w)
                  (iter (cdr w)))))

    (unwrap-singleton (iter exp)))

; Grab everything beyond the first occurence of `op`
(define (right-operand exp op)
    (unwrap-singleton (cdr (memq op exp))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Summary of changes done (compared to Exercise 2.58 (a))
; 1. Predicates detect whether the corresponding operator occurs at the
;    top level of an expression.
; 2. Selectors split the expression at the first occurrence of that
;    operator: the left selector returns the part before it, and the
;    right selector returns the part after it.

(define (=number? exp n)
    (and (number? exp) (= exp n)))

; The checks for each operation proceed from lower to higher precedence.
; Thus, a top-level sum is recognized before products or exponentiations; if
; there is no top-level sum, a product is recognized before an exponentiation.
; Nested lists represent parenthesized subexpressions.

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
          ; NOTE: Remember that exponentiation is right-associative.
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
    (and (pair? x)
         (pair? (memq '+ x))))

(define (addend s)
    (left-operand s '+))

(define (augend s)
    (right-operand s '+))
; Changed code ends

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list a1 '+ a2))))

; Changed code begins
(define (product? x)
    (and (pair? x)
         (pair? (memq '* x))))

(define (multiplier p)
    (left-operand p '*))

(define (multiplicand p)
    (right-operand p '*))
; Changed code ends

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

; Changed code begins
(define (exponentiation? x)
    (and (pair? x)
         (pair? (memq '** x))))

(define (base x)
    (left-operand x '**))

(define (exponent x)
    (right-operand x '**))
; Changed code ends

(define (make-exponentiation b p)
    (cond ((=number? p 0) 1)
          ((=number? p 1) b)
          ((and (number? b) (number? p)) (expt b p))
          (else (list b '** p))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tests

; x + 3(x + y + 2) --> 4
(deriv '(x + 3 * (x + y + 2)) 'x)

; xy + 3(x^2 + y + 2) --> 6x + y
(deriv '(x * y + 3 * (x ** 2 + y + 2)) 'x)

; 3x^2y + 2x + y^3 --> 6xy + 2
(deriv '(3 * x ** 2 * y + 2 * x + y ** 3) 'x)

; x^(y^(2^3)) --> y^(2^3)x^(y^(2^3)-1)
(deriv '(x ** y ** 2 ** 3) 'x)

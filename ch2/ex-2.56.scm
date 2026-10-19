#lang sicp

; Exercise 2.56:
; Show how to extend the basic differentiator to handle more kinds of
; expressions. For instance, implement the differentiation rule
;
;                            d(u^n)             du
;                           -------- = nu^(n-1)----
;                              dx               dx
;
; by adding a new clause to the `deriv` program and defining appropriate
; procedures `exponentiation?`, `base`, `exponent`, and `make-exponentiation`.
; (You may use the symbol `**` to denote exponentiation.) Build in the rules
; that anything raised to the power 0 is 1 and anything raised to the power 1
; is the thing itself.
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
          ; Newly added code begins
          ((exponentiation? exp)
           (make-product (make-product
                          (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum (exponent exp)
                                                         -1)))
                         (deriv (base exp) var)))
          ; Newly added code ends
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

(define (augend s)
    (caddr s))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
    (cadr p))

(define (multiplicand p)
    (caddr p))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; Tests

; NOTE: At this stage, we are only dealing with operations on two arguments
; WARNING: Therefore, do NOT pass multiple arguments to the built-in `+` and
; `*` procedures, unless you want to spend an hour debugging this silly.

; 3x^2y + 2x + 3 + y^3 --> 6xy + 2
(deriv '(+ (* 3
              (* (** x 2) y))
           (+ (* 2 x)
              (+ 3
                 (** y 3))))
       'x)



; 5x^(y^3 + 27)2^y --> 5(y^3 + 27)x^(y^3 + 26)2^y
(deriv '(* 5
           (* (** x
                  (+ (** y 3)
                     27))
              (** 2 y)))
       'x)

; Well, at this stage, our program has never heard of a logarithm.
; So guess what happens when the correct result requires it.

; x^x --> x^x(xln(x) + 1)
(deriv `(** x x) 'x)

; Now where's my Turing award and Fields medal :P

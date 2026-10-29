#lang sicp

; Exercise 2.73:
; Section 2.3.2 described a program that performs symbolic differentiation:
;
; (define (deriv exp var)
;     (cond ((number? exp) 0)
;           ((variable? exp)
;            (if (same-variable? exp var) 1 0))
;           ((sum? exp)
;            (make-sum (deriv (addend exp) var)
;                      (deriv (augend exp) var)))
;           ((product? exp)
;            (make-sum (make-product
;                       (multiplier exp)
;                       (deriv (multiplicand exp) var))
;                      (make-product
;                       (deriv (multiplier exp) var)
;                       (multiplicand exp))))
;           ⟨more rules can be added here⟩
;           (else (error "unknown expression type:
;                        DERIV" exp))))
;
; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the “type tag” of the datum
; is the algebraic operator symbol (such as `+`) and the operation being
; performed is `deriv`. We can transform this program into data-directed style
; by rewriting the basic derivative procedure as

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp))
                 (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;   c. Choose any additional differentiation rule that you like, such as the
;      one for exponents (Exercise 2.56), and install it in this data-directed
;      system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: We need not know how these work right now. These are treated as black-
; boxes for this problem. Implementation only provided for testing.

; `get` and `put` procedures defined ahead in Section 3.3.3

(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable
                   (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                           (assoc key-2 (cdr subtable))))
                        (if record (cdr record) false))
                    false)))

        (define (insert! key-1 key-2 value)
            (let ((subtable
                   (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                           (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1 (cons key-2 value))
                                    (cdr local-table))))))

        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation: TABLE" m))))

        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? x)
    (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (install-deriv-package)
    (define (=number? exp n)
        (and (number? exp) (= exp n)))

    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
              ((=number? a2 0) a1)
              ((and (number? a1) (number? a2)) (+ a1 a2))
              (else (list '+ a1 a2))))

    (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
              ((=number? m1 1) m2)
              ((=number? m2 1) m1)
              ((and (number? m1) (number? m2)) (* m1 m2))
              (else (list '* m1 m2))))

    ; Newly added code begins (same as in Exercise 2.56)
    (define (make-exponentiation b p)
        (cond ((=number? p 0) 1)
              ((=number? p 1) b)
              ((and (number? b) (number? p)) (expt b p))
              (else (list '** b p))))
    ; Newly added code ends

    (put 'deriv
         '+
         (lambda (operands var)
             (let ((addend (car operands))
                   (augend (cadr operands)))
                 (make-sum (deriv addend var)
                           (deriv augend var)))))

    (put 'deriv
         '*
         (lambda (operands var)
             (let ((multiplier (car operands))
                   (multiplicand (cadr operands)))
                 (make-sum (make-product multiplier
                                         (deriv multiplicand var))
                           (make-product multiplicand
                                         (deriv multiplier var))))))

    ; Newly added code begins
    (put 'deriv
         '**
         (lambda (operands var)
             (let ((base (car operands))
                   (exponent (cadr operands)))
                 (make-product
                  (make-product exponent
                                (make-exponentiation base
                                                     (make-sum exponent -1)))
                  (deriv base var))))))
    ; Newly added code ends

(install-deriv-package)

; Tests

; NOTE: Each operator MUST take exactly two arguments.

; x^2 + x^2 --> 2x + 2x
(deriv '(+ (* x x) (* x x)) 'x)

; 3xy + x^2 --> 3y + 2x
(deriv '(+ (* 3 (* x y)) (* x x)) 'x)

; 3x^2y + 2x + 3 + y^3 --> 6xy + 2
(deriv '(+ (* 3
              (* (** x 2) y))
           (+ (* 2 x)
              (+ 3
                 (** y 3))))
       'x)

#lang sicp

; Exercise 2.78:
; The internal procedures in the `scheme-number` package are essentially nothing
; more than calls to the primitive procedures `+`, `-`, etc. It was not possible
; to use the primitives of the language directly because our type-tag system
; requires that each data object have a type attached to it. In fact, however,
; all Lisp implementations do have a type system, which they use internally.
; Primitive predicates such as `symbol?` and `number?` determine whether data
; objects have particular types. Modify the definitions of `type-tag`,
; `contents`, and `attach-tag` from Section 2.4.2 so that our generic system
; takes advantage of Scheme’s internal type system. That is to say, the system
; should work as before except that ordinary numbers should be represented
; simply as Scheme numbers rather than as pairs whose `car` is the symbol
; `scheme-number`.
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

; Procedures for rectangular and polar representations defined in Section 2.4.3

(define (square x)
    (* x x))

(define (install-rectangular-package)
    ;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
        (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (install-polar-package)
    ;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
        (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
         (let ((proc (get op type-tags)))
             (if proc
                 (apply proc (map contents args))
                 (error "No method for these types: APPLY-GENERIC"
                        (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Generic arithemtic procedures from Section 2.5.1

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; NOTE: We no longer have to tag the results of ordinary number arithmetic
(define (install-scheme-number-package)
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (+ x y)))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (- x y)))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (* x y)))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (/ x y)))
    (put 'make 'scheme-number (lambda (x) x))
    'done)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

(define (install-rational-package)
    ;; internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))

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

(define (make-rational n d)
    ((get 'make 'rational) n d))

(define (install-complex-package)
    (install-rectangular-package)
    (install-polar-package)

    ;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;; internal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))

    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-tag datum)
    (cond ((pair? datum) (car datum))
          ((number? datum) 'scheme-number)
          (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
    (cond ((pair? datum) (cdr datum))
          ((number? datum) datum)
          (else (error "Bad tagged datum: CONTENTS" datum))))

(define (attach-tag type-tag contents)
    (if (eq? type-tag 'scheme-number)
        contents
        (cons type-tag contents)))

; Tests

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

; TODO: Improve output format using generic print procedures for each type

; Scheme-number arithmetic
(display (add 8 3))
(newline)
(display (sub 8 3))
(newline)
(display (mul 8 3))
(newline)
(display (div 8 2))
(newline)

; Rational arithmetic
(display (add (make-rational 1 2) (make-rational 1 3)))
(newline)
(display (sub (make-rational 1 2) (make-rational 1 3)))
(newline)
(display (mul (make-rational 1 2) (make-rational 1 3)))
(newline)
(display (div (make-rational 1 2) (make-rational 1 3)))
(newline)

; Complex arithmetic

; Polar–polar
(display (add (make-complex-from-mag-ang 2 0)
              (make-complex-from-mag-ang 3 0)))
(newline)

; Rectangular–rectangular
(display (add (make-complex-from-real-imag 1 2)
              (make-complex-from-real-imag 3 4)))
(newline)

; Mixed
(display (add (make-complex-from-mag-ang 2 0)
              (make-complex-from-real-imag 3 4)))
(newline)
(display (sub (make-complex-from-real-imag 1 2)
              (make-complex-from-mag-ang 3 0)))
(newline)
(display (mul (make-complex-from-mag-ang 2 0)
              (make-complex-from-real-imag 3 4)))
(newline)
(display (div (make-complex-from-real-imag 1 2)
              (make-complex-from-mag-ang 3 0)))
(newline)

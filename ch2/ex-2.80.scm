#lang sicp

; Exercise 2.80:
; Define a generic predicate `=zero?` that tests if its argument is zero, and
; install it in the generic arithmetic package. This operation should work for
; ordinary numbers, rational numbers, and complex numbers.
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

; Updated procedures for tagged data from Exercise 2.78

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

; Procedures for rectangular and polar representations defined in Section 2.4.3

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

(define (square x)
    (* x x))

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

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'make 'scheme-number (lambda (x) (tag x)))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))
    ; Newly added code begins
    (put '=zero? '(scheme-number)
         (lambda (x) (= x 0)))
    ; Newly added code ends
    'done)

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))

    (define (tag x) (attach-tag 'rational x))
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    (put 'equ? '(rational rational)
         (lambda (x y)
             (= (* (numer x) (denom y))
                (* (numer y) (denom x)))))
    ; Newly added code begins
    (put '=zero? '(rational)
         (lambda (x) (= (numer x) 0)))
    ; Newly added code ends
    'done)

(define (install-complex-package)
    (install-rectangular-package)
    (install-polar-package)

    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    (define (tag z) (attach-tag 'complex z))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equ?
         '(complex complex)
         (lambda (x y)
             (and (= (real-part x) (real-part y))
                  (= (imag-part x) (imag-part y)))))
    ; Newly added code begins
    (put '=zero? '(complex)
         (lambda (x)
             (and (= (real-part x) 0)
                  (= (imag-part x) 0))))
    ; Newly added code ends
    'done)

(define (equ? x y)
    (apply-generic 'equ? x y))

; Newly added code begins
(define (=zero? x)
    (apply-generic '=zero? x))
; Newly added code ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tests

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(=zero? 0)
(=zero? 5)

(=zero? ((get 'make 'rational) 0 3))
(=zero? ((get 'make 'rational) 2 3))

(=zero? ((get 'make-from-real-imag 'complex) 0 0))
(=zero? ((get 'make-from-mag-ang 'complex) 0 4))

#lang sicp

; Exercise 2.79:
; Define a generic equality predicate `equ?` that tests the equality of two
; numbers, and install it in the generic arithmetic package. This operation
; should work for ordinary numbers, rational numbers, and complex numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x'))
    (put 'make 'scheme-number (lambda (x) (tag x)))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))

    (define (tag x) (attach-tag 'rational x'))
    (put 'make 'rational
         (lambda (n d) (make-tag (make-rat n d))))
    (put 'equ? '(rational rational)
         (lambda (x y)
             (= (* (numer x) (denom y))
                (* (numer y) (denom x))))))

(define (install-complex-package)
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
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags))))

(define (equ? x y)
  (apply-generic 'equ? x y))

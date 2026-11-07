#lang sicp

; Exercise 2.81:
; Louis Reasoner has noticed that `apply-generic` may try to coerce the
; arguments to each other’s type even if they already have the same type.
; Therefore, he reasons, we need to put procedures in the coercion table to
; coerce arguments of each type to their own type. For example, in addition to
; the `scheme-number->complex coercion` shown above, he would do:
;
; (define (scheme-number->scheme-number n) n)
; (define (complex->complex z) z)
; (put-coercion 'scheme-number
;               'scheme-number
;               scheme-number->scheme-number)
; (put-coercion 'complex 'complex complex->complex)
;
;   c. Modify apply-generic so that it doesn’t try coercion if the two arguments
;      have the same type.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        ; Newly added code begins
                        (if (eq? type1 type2)
                            (error "No method for these types"
                                   (list op type-tags))
                        ; Newly added code ends
                            (let ((t1->t2 (get-coercion type1 type2))
                                  (t2->t1 (get-coercion type2 type1)))
                                (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                      (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                                      (else (error "No method for these types"
                                                   (list op type-tags))))))
                    (error "No method for these types"
                           (list op type-tags))))))))

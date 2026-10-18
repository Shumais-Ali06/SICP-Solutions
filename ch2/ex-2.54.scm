#lang sicp

; Exercise 2.54:
; Two lists are said to be `equal?` if they contain equal elements arranged in
; the same order. For example,
;
; (equal? '(this is a list) '(this is a list))
;
; is true, but
;
; (equal? '(this is a list) '(this (is a) list))
;
; is false. To be more precise, we can define `equal?` recursively in terms of
; the basic `eq?` equality of symbols by saying that `a` and `b` are `equal?`
; if they are both symbols and the symbols are `eq?`, or if they are both lists
; such that `(car a)` is `equal?` to `(car b)` and `(cdr a)` is `equal?` to
; `(cdr b)`. Using this idea, implement `equal?` as a procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (equal? a b)
    (if (and (pair? a) (pair? b))
        (and (equal? (car a) (car b))
             (equal? (cdr a) (cdr b)))
        (eq? a b)))

; Tests

(equal? '(this is a list) '(this is a list))        ; => #t
(equal? '(this is a list) '(this (is a) list))      ; => #f
(equal? 'george 'george)                            ; => #t
(equal? 'george 'martha)                            ; => #f
(equal? '() '())                                    ; => #t
(equal? '(a b) '(a b c))                            ; => #f
(equal? '(a b c) '(a b))                            ; => #f
(equal? '((a b) c) '((a b) c))                      ; => #t
(equal? '((a b) c) '((a d) c))                      ; => #f

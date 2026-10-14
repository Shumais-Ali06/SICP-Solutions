#lang sicp

; Exercise 2.35:
; Redefine `count-leaves` from Section 2.2.2 as an accumulation:
;
; (define (count-leaves t)
;     (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `count-leaves` as defined in Section 2.2.2
;
; (define (count-leaves x)
;     (cond ((null? x) 0)
;           ((not (pair? x)) 1)
;           (else (+ (count-leaves (car x))
;                    (count-leaves (cdr x))))))

; `accumulate` as defined in Section 2.2.3
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
    (accumulate +
                0
                ; Produce a list of leaf counts to be added
                (map (lambda (x)
                         (cond ((null? x) 0)
                               ((pair? x) (count-leaves x))
                               (else 1)))
                     t)))

; Here is a slightly different implementation
; (define (count-leaves t)
;     (accumulate (lambda (x y)
;                     ; Compute each element's leaf count along the way
;                     (+ (cond ((null? x) 0)
;                              ((pair? x) (count-leaves x))
;                              (else 1))
;                        y))
;                 0
;                 (map (lambda (x) x)     ; This maybe simplified to just `t`.
;                      t)))               ; It is retained only to match the
;                                         ; structure in the problem.

; Tests

(define x (cons (list 1 2) (list 3 4)))
(count-leaves nil)
(count-leaves x)
(count-leaves (list x x))

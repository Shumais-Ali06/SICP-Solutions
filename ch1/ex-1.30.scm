#lang sicp

; Exercise 1.30:
; The `sum` procedure above generates a linear recursion. The procedure can be
; rewritten so that the sum is performed iteratively. Show how to do this by
; filling in the missing expressions in the following definition:
;
; (define (sum term a next b)
;     (define (iter a result)
;         (if <??>
;             <??>
;             (iter <??> <??>)))
;
;     (iter <??> <??>))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a)
                  (+ result (term a)))))

    (iter a 0))

; Procedures for `term`
(define (cube x)
    (* x x x))

(define (square x)
    (* x x))

; Procedures for `next`
(define (inc x)
    (+ x 1))

(define (identity x)
    x)

; Tests
(sum cube 1 inc 10)
(sum identity 1 inc 100)

#lang sicp

; Exercise 2.33:
; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:
;
; (define (map p sequence)
;     (accumulate (lambda (x y) ⟨??⟩) nil sequence))
; (define (append seq1 seq2)
;     (accumulate cons ⟨??⟩ ⟨??⟩))
; (define (length sequence)
;     (accumulate ⟨??⟩ 0 sequence))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `accumulate` as defined in Section 2.2.3
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; WARNING: The argument `sequence` must not contain lists as elements.
(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y))
                nil
                sequence))

(define (append seq1 seq2)
    (accumulate cons
                seq2
                seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y))
                0
                sequence))

; Tests

(define (square x)
    (* x x))

(map square (list 1 2 3 4 5))
(append (list 1 2) (list 3 4))
(length (list 1 2 (list 3)))

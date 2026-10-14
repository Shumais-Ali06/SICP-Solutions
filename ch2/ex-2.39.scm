#lang sicp

; Exercise 2.39:
; Complete the following definitions of `reverse` (Exercise 2.18) in terms of
; `fold-right` and `fold-left` from Exercise 2.38:
;
; (define (reverse sequence)
;     (fold-right (lambda (x y) ⟨??⟩) nil sequence))
; (define (reverse sequence)
;     (fold-left (lambda (x y) ⟨??⟩) nil sequence))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `fold-left` as defined in Exercise 2.38
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

; `fold-right` (renamed from `accumulate`) as defined in Section 2.2.3
(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))

; NOTE: Both versions are numbered to prevent conflicts.

; Time complexity: O(N^2) (due to repeated walks)
(define (reverse-v1 sequence)
    (fold-right (lambda (x y)
                    ; Append the current element at the end of the reversed tail
                    (append y (list x)))
                nil
                sequence))

; Time complexity: O(N)
(define (reverse-v2 sequence)
    (fold-left (lambda (x y)
                   ; Put each next element at the front
                   (cons y x))
               nil
               sequence))

; Tests

(reverse-v1 (list 1 2 3 4 5))
(reverse-v2 (list 1 2 3 4 5))

(reverse-v1 nil)
(reverse-v2 nil)

(reverse-v1 (list 1 2 (list 3 (list 4)) 5))
(reverse-v2 (list 1 2 (list 3 (list 4)) 5))

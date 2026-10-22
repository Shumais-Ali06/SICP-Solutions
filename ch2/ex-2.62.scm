#lang sicp

; Exercise 2.62:
; Give a Θ(n) implementation of `union-set` for sets represented as ordered
; lists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Repeatedly take the smaller head and advance that list; if the heads are
; equal, take the value once and advance both lists. When either list is empty,
; return the remaining tail of the other list.

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
               (cond ((= x1 x2)
                      (cons x1
                            (union-set (cdr set1) (cdr set2))))
                     ((< x1 x2)
                      (cons x1
                            (union-set (cdr set1) set2)))
                     (else
                      (cons x2
                            (union-set set1 (cdr set2)))))))))

; Tests

(union-set '() '())                 ; => ()
(union-set '(1 3 5) '(2 4 6))       ; => (1 2 3 4 5 6)
(union-set '(1 3 5) '(1 2 5 6))     ; => (1 2 3 5 6)
(union-set '(1 2 3) '(1 2 3))       ; => (1 2 3)
(union-set '() '(2 4 6))            ; => (2 4 6)
(union-set '(1 3 5) '())            ; => (1 3 5)
(union-set '(4) '(2))               ; => (2 4)

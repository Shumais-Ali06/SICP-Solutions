#lang sicp

; Exercise 2.60:
; We specified that a set would be represented as a list with no duplicates.
; Now suppose we allow duplicates. For instance, the set {1, 2, 3} could be
; represented as the list (2 3 2 1 3 2 2). Design procedures `element-of-set?`,
; `adjoin-set`, `union-set`, and `intersection-set` that operate on this
; representation. How does the efficiency of each compare with the corresponding
; procedure for the non-duplicate representation? Are there applications for
; which you would use this representation in preference to the non-duplicate
; one?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Time complexity: O(n) same as before
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

; Time complexity: O(1) compared to O(n) before
(define (adjoin-set x set)
    (cons x set))

; Time complexity: O(n1) compared to O(n1*n2) before
(define (union-set set1 set2)
    (append set1 set2))

; Time complexity: O(n1*n2) same as before
(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

; Test cases for `union-set`

(union-set '() '(1 2))                  ; => (1 2)
(union-set '(1 2) '())                  ; => (1 2)
(union-set '(1 2) '(2 3))               ; => (1 2 2 3)
(union-set '(1 2 2) '(2 3 3))           ; => (1 2 2 2 3 3)

; Test cases for `intersection-set`

(intersection-set '() '(1 2))           ; => ()
(intersection-set '(1 2) '())           ; => ()
(intersection-set '(1 2 3) '(2 3 4))    ; => (2 3)
(intersection-set '(1 2 2 3) '(2 3 3))  ; => (2 2 3)
(intersection-set '(1 2) '(3 4))        ; => ()

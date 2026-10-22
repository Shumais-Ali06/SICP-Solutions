#lang sicp

; Exercise 2.61:
; Give an implementation of `adjoin-set` using the ordered representation. By
; analogy with `element-of-set?` show how to take advantage of the ordering to
; produce a procedure that requires on the average about half as many steps as
; with the unordered representation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Scan until `x` belongs before the current element or is already present.
; Because the list is ordered, the search stops as soon as the current element
; in the list exceeds `x`.

(define (adjoin-set x set)
    (cond ((or (null? set) (< x (car set)))
           (cons x set))
          ((= x (car set)) set)
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

; Tests

(adjoin-set 0 '())                  ; => (0)
(adjoin-set 2 '(1 3 5 7))           ; => (1 2 3 5 7)
(adjoin-set 1 '(2 3 4 5 6))         ; => (1 2 3 4 5 6)
(adjoin-set 9 '(1 3 5 7))           ; => (1 3 5 7 9)
(adjoin-set 3 '(1 3 5 7))           ; => (1 3 5 7)
(adjoin-set 2 '(2 3 4 5 6))         ; => (2 3 4 5 6)
(adjoin-set 7 '(1 3 5 7))           ; => (1 3 5 7)

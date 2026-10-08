#lang sicp

; Exercise 2.17:
; Define a procedure `last-pair` that returns the list that contains only the
; last element of a given (nonempty) list:
;
; (last-pair (list 23 72 149 34))
; > (34)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-pair items)
    (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))))

; Tests
(last-pair (list 23 72 149 34))
(last-pair (list -1))
(last-pair (list 1 4 9 16))

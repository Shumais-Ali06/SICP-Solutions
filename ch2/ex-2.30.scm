#lang sicp

; Exercise 2.30:
; Define a procedure `square-tree` analogous to the `square-list` procedure of
; Exercise 2.21. That is, `square-tree` should behave as follows:
;
; > (square-tree
;       (list 1
;             (list 2 (list 3 4) 5)
;             (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
;
; Define `square-tree` both directly (i.e., without using any higher-order
; procedures) and also by using `map` and recursion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)
    (* x x))

; Implementation without any higher-order procedures
(define (square-tree-v1 tree)
    (cond ((null? tree) nil)
          ((pair? tree) (cons (square-tree-v1 (car tree))
                              (square-tree-v1 (cdr tree))))
          (else (square tree))))

; Implementation using `map`
(define (square-tree-v2 tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree-v2 sub-tree)
                (square sub-tree)))
         tree))

; Tests

(square-tree-v1 (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7)))

(square-tree-v2 (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7)))

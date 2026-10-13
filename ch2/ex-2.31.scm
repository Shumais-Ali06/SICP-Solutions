#lang sicp

; Exercise 2.31:
; Abstract your answer to Exercise 2.30 to produce a procedure `tree-map` with
; the property that `square-tree` could be defined as

(define (square-tree tree) (tree-map square tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)
    (* x x))

(define (tree-map proc tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map proc sub-tree)
                (proc sub-tree)))
         tree))

; Tests

(tree-map square (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))

(tree-map square nil)

(tree-map square (list 1 3 5 7))

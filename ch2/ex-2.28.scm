#lang sicp

; Exercise 2.28:
; Write a procedure fringe that takes as argument a tree (represented as a list)
; and returns a list whose elements are all the leaves of the tree arranged in
; left-to-right order. For example,
;
; > (define x (list (list 1 2) (list 3 4)))
; > (fringe x)
; (1 2 3 4)
; > (fringe (list x x))
; (1 2 3 4 1 2 3 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fringe tree)
    (define (collect tree tail)
        (cond ((null? tree) tail)           ; For an empty list, return the tail
              ((pair? tree)                 ; For a pair (non-leaf), recurse and
               (collect (car tree)          ; collect all the leaves inside it
                        (collect (cdr tree) ; and the next pair.
                                 tail)))
              (else (cons tree tail))))     ; For a non-pair (leaf), prepend it
                                            ; to the tail

    (collect tree nil))

; Helper function to test and display the results nicely
(define (test items)
    (display "Original: ")
    (display items)
    (newline)
    (display "Fringe: ")
    (display (fringe items))
    (newline)
    (newline))

; Tests
(define x (list (list 1 2) (list 3 4)))
(test x)
(test (list x x))

#lang sicp

; Exercise 2.18:
; Define a procedure `reverse` that takes a list as argument and returns a list
; of the same elements in reverse order:
;
; (reverse (list 1 4 9 16 25))
; > (25 16 9 4 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse items)
    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items)
                  (cons (car items) result))))

  (iter items nil))

; Tests
(reverse (list 1 4 9 16 25))
(reverse (list 1 2 3 4 5 6 7 8 9 10))
(reverse (list -1))


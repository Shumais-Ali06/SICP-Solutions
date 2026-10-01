#lang sicp

; Exercise 1.37 (b):
; If your `cont-frac` procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Iterative implementation of the `cont-frac` procedure
(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1)
                  (/ (n i)
                     (+ (d i)
                        result)))))

    (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

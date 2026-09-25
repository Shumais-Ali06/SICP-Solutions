#lang sicp

; Exercise 1.11:
; A function f is defined by the rule that
;
; f(n) = { n                               , if n < 3
;        { f(n - 1) + 2f(n - 2) + 3f(n - 3), if n >= 3
;
; Write a procedure that computes f by means of an recursive process. Write a
; procedure that computes f by means of an iterative process.

; Helper function to get the next number in the sequence
(define (next a b c)
    (+ (* 3 a) (* 2 b) c))

; Recursive implementation
(define (f-recursive n)
    (if (< n 3)
        n
        (next (f-recursive (- n 3))
              (f-recursive (- n 2))
              (f-recursive (- n 1)))))

; Iterative implementation
(define (f-iterative n)
    (define (iter i a b c)
        (if (= i n)
            (next a b c)
            (iter (+ i 1) b c (next a b c))))

    (if (< n 3)
        n
        (iter 3 0 1 2)))

; Tests
(display "Results for recursive version")
(newline)
(f-recursive 3)
(f-recursive 4)
(f-recursive 11)
(newline)

(display "Results for iterative version")
(newline)
(f-iterative 3)
(f-iterative 4)
(f-iterative 11)

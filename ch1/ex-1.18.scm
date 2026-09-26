#lang sicp

; Exercise 1.18:
; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
; generates an iterative procedure for multiplying two integers in terms of
; adding, doubling, and halving and uses a logarithmic number of steps.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Our implementation for `double` and `halve` uses the multiplication
; operator underneath, but it is allowed since both procedures are assumed
; to be predefined by our language.
(define (double n)
    (* n 2))

(define (halve n)
    (/ n 2))

; No. of calls to `iter` = No. of right-shifts performed in the even-step +
;                          No. of 1-bits that are removed in each odd-step
;                        = floor(log2(b)) + popcnt(b), for a, b > 0
;
; The overall time complexity is O(logb)
(define (fast-* a b)
    (define (iter x a b)
        (cond ((= b 0) x)
              ((odd? b) (iter (+ x a) a (- b 1)))
              (else (iter x (double a) (halve b)))))

    (if (or (= a 0) (= b 0))
        0
        (iter 0 a b)))

; Tests
(fast-* 2 5)
(fast-* 3 3)
(fast-* 83 3)
(fast-* 2 1000)
(fast-* 0 0)
(fast-* 0 32)
(fast-* 1 0)
(fast-* 1 999)

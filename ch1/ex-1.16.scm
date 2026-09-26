#lang sicp

; Exercise 1.16:
; Design a procedure that evolves an iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps as does
; `fast-expt`. (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2),
; keep, along with the exponent `n` and the base `b`, an additional state
; variable `a` and define the state transformation such that the product ab^n is
; unchanged from state to state. At the beginning of the process `a` is taken to
; be 1, and the answer is given by the value of `a` at the end of the process.
; In general, the technique of defining an invariant quantity that remains
; unchanged from state to state is a powerful way to think about the design of
; iterative algorithms.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; No. of calls to `iter` = No. of right-shifts performed in the even-step +
;                          No. of 1-bits that are removed in each odd-step
;                        = floor(log2(n)) + popcnt(n), for non-trivial b, n
;
; The overall time complexity is O(logn)
(define (fast-expt b n)
    (define (iter a b n)
        (cond ((= n 0) a)
              ((odd? n) (iter (* a b) b (- n 1)))
              (else (iter a (* b b) (/ n 2)))))

    (cond ((= n 0) 1)
          ((= b 0) 0)
          ((= b 1) 1)
          (else (iter 1 b n))))

; Tests
(fast-expt 2 5)
(fast-expt 3 3)
(fast-expt 83 3)
(fast-expt 2 1000)
(fast-expt 0 0)
(fast-expt 0 32)
(fast-expt 1 0)
(fast-expt 1 999)

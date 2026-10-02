#lang sicp

; Exercise 1.43:
; If `f` is a numerical function and `n` is a positive integer, then we can form
; the n'th repeated application of `f`, which is defined to be the function
; whose value at `x` is f(f( ... (f(x)) ... )). For example, if `f` is the
; function x → x + 1, then the n'th repeated application of `f` is the function
; x → x + n. If `f` is the operation of squaring a number, then the n'th
; repeated application of `f` is the function that raises its argument to the
; 2^n'th power. Write a procedure that takes as inputs a procedure that computes
; `f` and a positive integer `n` and returns the procedure that computes the
; n'th repeated application of `f`. Your procedure should be able to be used as
; follows:
;
; ((repeated square 2) 5)
; > 625
;
; Hint: You may find it convenient to use `compose` from Exercise 1.42.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compose f g)
    (lambda (x)
        (f (g x))))

; Ok maybe the identity function is not as useless I used to believe in high
; school. Here we will use it for the base case of recursion.
(define (identity x) x)

; Linear recursive implementation
(define (repeated f n)
    (if (= n 0)
        identity
        (compose f
                 (repeated f (- n 1)))))

(define (inc x)
    (+ x 1))

(define (square x)
    (* x x))

; Tests
((repeated square 2) 5)
((repeated (compose inc square) 3) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BONUS:
; We can do better by generalizing the concept behind the `fast-expt` procedure
; from Exercise 1.16. In fact, simply changing the `*` to `compose` works. We
; just go from combining (multiplying) values repeatedly to composing functions.

; This iterative implementation achieves the same result in O(logn) time
; NOTE: Any call to `fast-repeated` will still apply `f` n times.
(define (fast-repeated f n)
    (define (iter h f n)
        (cond ((= n 0) h)
              ((odd? n) (iter (compose h f) f (- n 1)))
              (else (iter h (compose f f) (/ n 2)))))

    ; Begin with `identity` as the accumulated function
    (iter identity f n))

#lang sicp

; Exercise 1.33:
; You can obtain an even more general version of `accumulate` (Exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting `filtered-accumulate` abstraction takes the
; same arguments as `accumulate`, together with an additional predicate of one
; argument that specifies the filter. Write `filtered-accumulate` as a procedure.
; Show how to express the following using `filtered- accumulate`:
;
; (a) the sum of the squares of the prime numbers in the interval `a` to `b`
; (assuming that you have a `prime?` predicate already written)
;
; (b) the product of all the positive integers less than `n` that are relatively
; prime to `n` (i.e., all positive integers i < n such that GCD(i, n) = 1).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recursive implementation of `filtered-accumulate`
; It accumulates the current term value using `combiner` if satisfies the
; predicate given by `filter?`, else it accumlate the `null-value` which by
; definition will no effect in the result of the function evaluation.
(define (filtered-accumulate filter? combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (if (filter? (term a))
                      (term a)
                      null-value)
                  (filtered-accumulate filter?
                                       combiner
                                       null-value
                                       term
                                       (next a)
                                       next
                                       b))))

; For part (a) of the problem
(define (f a b)
        (filtered-accumulate prime? + 0 square a inc b))

; For part (b) of the problem
(define (g n)
        (define (filter? a)
                (coprime? a n))

        (filtered-accumulate filter? * 1 identity 1 inc n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedures required by `f` and `g`

(define (square x)
    (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
    (= (smallest-divisor n) n))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (coprime? a b)
    (= (gcd a b) 1))

(define (inc x)
    (+ x 1))

(define (identity x)
    x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests

(display "f(a=3, b=28) = ")
(f 3 28)

(display "g(n=20) = ")
(g 20)

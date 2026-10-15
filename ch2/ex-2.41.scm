#lang sicp

; Exercise 2.41:
; Write a procedure to find all ordered triples of distinct positive integers
; `i`, `j`, and `k` less than or equal to a given integer `n` that sum to a
; given integer `s`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Sequence and nested-mapping operations from Section 2.2.3

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE : We use `flatmap` at the outer levels to combine results into one list.
; The innermost `map` is for building a triplet for each k.

; Returns the list of all triplets (i j k) where 1 <= k < j < i <= n
(define (unique-triplets n)
    (flatmap (lambda (i)
                 (flatmap (lambda (j)
                              (map (lambda (k)
                                       (list i j k))
                                   (enumerate-interval 1 (- j 1))))
                          (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (three-sum-triplets n s)
    (filter (lambda (x)
                (= (+ (car x) (cadr x) (caddr x))
                   s))
            (unique-triplets n)))

; Tests

(three-sum-triplets 6 10)
(three-sum-triplets 10 23)

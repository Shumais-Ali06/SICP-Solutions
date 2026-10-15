#lang sicp

; Exercise 2.40:
; Define a procedure `unique-pairs` that, given an integer `n`, generates the
; sequence of pairs (i, j) with 1 ≤ j < i ≤ n. Use unique-pairs to simplify the
; definition of `prime-sum-pairs` given above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Primality test procedures from Section 1.2.6

(define (square x)
    (* x x))

(define (smallest-divisor n) (find-divisor n 2))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

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

(define (unique-pairs n)
    (flatmap (lambda (i)
                 (map (lambda (j)
                          (list i j))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (make-pair-sum pair)
    (list (car pair)
          (cadr pair)
          (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

; `prime-sum` rewritten in terms of the new `unique-pairs` procedure
(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (unique-pairs n))))

; Tests

(prime-sum-pairs 1)
(prime-sum-pairs 2)
(prime-sum-pairs 3)
(prime-sum-pairs 4)
(prime-sum-pairs 5)
(prime-sum-pairs 6)
(prime-sum-pairs 7)
(prime-sum-pairs 8)
(prime-sum-pairs 9)
(prime-sum-pairs 10)

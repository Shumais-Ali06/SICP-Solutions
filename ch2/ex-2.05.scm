#lang sicp

; Exercise 2.5:
; Show that we can represent pairs of non-negative integers using only numbers
; and arithmetic operations if we represent the pair `a` and `b` as the integer
; that is the product 2^a3^b. Give the corresponding definitions of the
; procedures `cons`, `car`, and `cdr`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Computes floor(log_b(x)) using only arithmetic operations
(define (floor-log x b)
    (if (< x b)
        0
        (+ 1
           (floor-log (/ x b) b))))

; Computes b^n in log(n) time. We defined it in Exercise 1.16
(define (fast-expt b n)
    (define (iter a b n)
        (cond ((= n 0) a)
              ((odd? n) (iter (* a b) b (- n 1)))
              (else (iter a (* b b) (/ n 2)))))

    (cond ((= n 0) 1)
          ((= b 0) 0)
          ((= b 1) 1)
          (else (iter 1 b n))))

(define (divides? a b)
    (= (remainder a b) 0))

(define (cons a b)
    (* (fast-expt 2 a)
       (fast-expt 3 b)))

; First remove all multiples of 3, then get the exponent of 2
(define (car z)
    (if (divides? z 3)
        (car (/ z 3))
        (floor-log z 2)))

; First remove all multiples of 2, then get the exponent of 3
(define (cdr z)
    (if (divides? z 2)
        (cdr (/ z 2))
        (floor-log z 3)))

; Tests
(car (cons 1 3))
(cdr (cons 6 7))

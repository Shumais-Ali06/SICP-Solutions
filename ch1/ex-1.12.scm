#lang sicp

; Exercise 1.12:
; The following pattern of numbers is called Pascals's triangle.
;
;           1
;         1   1
;       1   2   1
;     1   3   3   1
;   1   4   6   4   1
;
; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes elements of Pascal's triangle by means of a recursive process.

; NOTE: In the functions defined below, both 'row' and 'col' are both 0-indexed

; Computes a particular element
(define (elem row col)
    (if (or (= col 0)
            (= col row))
        1
        (+ (elem (- row 1) col)
           (elem (- row 1) (- col 1)))))

(define (sum-pascal num-rows)
    ; Computes the sum of the nth row
    (define (sum-nth-row row)
        (define (iter sum col)
            (if (> col row)
                sum
                (iter (+ sum (elem row col))
                      (+ col 1))))

        (iter 0 0))

    (define (iter sum row)
        (if (= row num-rows)
            sum
            (iter (+ sum (sum-nth-row row))
                  (+ row 1))))

    (if (< num-rows 1)
        0
        (iter 0 0)))

; Tests
(sum-pascal 1)
(sum-pascal 2)
(sum-pascal 3)
(sum-pascal 4)
(sum-pascal 5)

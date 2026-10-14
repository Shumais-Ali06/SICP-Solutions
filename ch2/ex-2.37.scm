#lang sicp

; Exercise 2.37:
; Suppose we represent vectors `\textbf{v} = (v_{i})` as sequences of numbers,
; and matrices `\textbf{m} = (m_{i,j})` as sequences of vectors (the rows of the
; matrix). For example, the matrix
; 
;                                ( 1  2  3  4 )
;                                ( 4  5  6  6 )
;                                ( 6  7  8  9 )
;
; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))`. With this
; representation, we can use sequence operations to concisely express the basic
; matrix and vector operations. These operations (which are described in any
; book on matrix algebra) are the following:
;
;               `(dot-product v w)`   returns the sum `\sum_{i}v_{i}w_{i}`;
;           `(matrix-*-vector m v)`   returns the vector `\textbf{t}`,
;                                     where `t_{i} = \sum_{j}m_{i,j}v_{j}`;
;           `(matrix-*-matrix m n)`   returns the vector `\textbf{p}`,
;                                     where `p_{i,j} = \sum_{k}m_{i,k}n_{k,j}`;
;                   `(transpose m)`   returns the matrix `\textbf{n}`,
;                                     where `n_{i,j} = m_{j,i}`;
;
; We can define the dot product as

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure `accumulate-n` is defined in Exercise
; 2.36.)
;
; (define (matrix-*-vector m v)
;     (map ⟨??⟩ m))
; (define (transpose mat)
;     (accumulate-n ⟨??⟩ ⟨??⟩ mat))
; (define (matrix-*-matrix m n)
;     (let ((cols (transpose n)))
;         (map ⟨??⟩ m)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `accumulate` as defined in Section 2.2.3
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

; `accumulate-n` as defined in Exercise 2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op
                          init
                          (map (lambda (x) (car x)) seqs))
              (accumulate-n op
                            init
                            (map (lambda (x) (cdr x)) seqs)))))

(define (matrix-*-vector m v)
    (map (lambda (row)
             (dot-product row v))
         m))

(define (transpose mat)
    (accumulate-n cons
                  nil
                  mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row)
                 (matrix-*-vector cols row))
             m)))

; Tests

; TODO: Improve the output format
(define (print-matrix mat)
    (if (pair? (car mat))
        ; For a non-vector, print each row, one-per-line
        (for-each (lambda (row)
                    (display row)
                    (newline))
                  mat)
        ; For a vector, print the elements directly
        (begin
            (display mat)
            (newline)))
    (newline))

(define A (list (list 1 2 3 4)
                (list 5 6 7 8)
                (list 9 10 11 12)))

; Eexpected: (30 70 110)
(print-matrix (matrix-*-vector A
                               (list 1 2 3 4)))

; Expected: ((1 5 9) (2 6 10) (3 7 11) (4 8 12))
(print-matrix (transpose A))

; Expected: ((50 60) (114 140) (178 220))
(print-matrix (matrix-*-matrix
               A
               (list (list 1 2)
                     (list 3 4)
                     (list 5 6)
                     (list 7 8))))

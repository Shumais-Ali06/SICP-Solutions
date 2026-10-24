#lang sicp

; Exercise 2.66:
; Implement the `lookup` procedure for the case where the set of records is
; structured as a binary tree, ordered by the numerical values of the keys.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup given-key set-of-records)
    (if (null? set-of-records)
        #f
        (let ((x (car set-of-records)))
            (cond ((< given-key x)
                   (lookup given-key (left-branch set-of-records)))
                  ((> given-key x)
                   (lookup given-key (right-branch set-of-records)))
                  (else (entry set-of-records))))))

; Binary tree procedures as defined in Section 2.3.3

(define (entry tree)
    (car tree))

(define (left-branch tree)
    (cadr tree))

(define (right-branch tree)
    (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

; NOTE: For simplicity, each record only consists of the key since the contents
; of the record itself do not affect the algorithm.

; One of the binary trees representing the set {1,3,5,7,9,11} in Figure 2.16.

(define tr
    (make-tree 7
               (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 5 '() '()))
               (make-tree 9
                          '()
                          (make-tree 11 '() '()))))

; Tests

(lookup 0 tr)
(lookup 7 tr)
(lookup 4 tr)
(lookup 9 tr)

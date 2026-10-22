#lang sicp

; Exercise 2.59:
; Implement the `union-set` operation for the unordered-list representation of
; sets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Set operations as defined in Section 2.3.3

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recursively consume elements from `set1` and put them in `set2` if not already
; present. Return `set2` when `set1` becomes empty.
(define (union-set set1 set2)
    (if (null? set1)
        set2
        (union-set (cdr set1)
                   (adjoin-set (car set1) set2))))

; Tests

(union-set '() '())             ; => ()
(union-set '() '(a b))          ; => (a b)
(union-set '(a b) '())          ; => (b a)
(union-set '(a b c) '(c d))     ; => (b a c d)
(union-set '(a b) '(b c a))     ; => (b c a)
(union-set '(1 2 3) '(4 5))     ; => (3 2 1 4 5)

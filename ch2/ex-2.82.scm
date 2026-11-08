#lang sicp

; Exercise 2.82:
; Show how to generalize `apply-generic` to handle coercion in the general case
; of multiple arguments. One strategy is to attempt to coerce all the arguments
; to the type of the first argument, then to the type of the second argument,
; and so on. Give an example of a situation where this strategy (and likewise
; the two-argument version given above) is not sufficiently general. (Hint:
; Consider the case where there are some suitable mixed-type operations present
; in the table that will not be tried.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Convert datum to type if a direct coercion is available
(define (coerce-to-type datum type)
    (if (eq? (type-tag datum) type)
        datum
        (let ((coercion (get-coercion (type-tag datum) type)))
            (and coercion (coercion datum)))))

; Try coercing every argument to each candidate type in turn
(define (try-coercion op args targets)
    (if (null? targets)
        (error "No method for these types" (map type-tag args))
        (let ((coerced
              (map (lambda (arg)
                       (coerce-to-type arg (car targets)))
                   args)))
            (if (memq #f coerced)
                (try-coercion op args (cdr targets))
                (let ((proc (get op (map type-tag coerced))))
                    (if proc
                        (apply proc (map contents coerced))
                        (try-coercion op args (cdr targets))))))))

; Try the exact method first; otherwise try all common-type coercions
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (> (length args) 1)
                    (try-coercion op args type-tags)
                    (error "No method for these types"
                           (list op type-tags)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FOLLOW-UP:
; To see why this "general" implementation is not sufficient for all possible
; cases, consider a scenario where we have types A, B, C. We want to perform the
; operation `(op A C)`, but we only have the procedure `(op B C)` and coercion
; `A->B` in the tables. From this, our implementation only checks for `(op A A)`
; and `(op C C)` and fails.

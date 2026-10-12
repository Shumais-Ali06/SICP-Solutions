#lang sicp

; Exercise 2.23:
; The procedure `for-each` is similar to map. It takes as arguments a procedure
; and a list of elements. However, rather than forming a list of the results,
; `for-each` just applies the procedure to each of the elements in turn, from
; left to right. The values returned by applying the procedure to the elements
; are not used at all—`for-each` is used with procedures that perform an action,
; such as printing. For example,
;
; > (for-each (lambda (x)
;                 (newline)
;                 (display x))
;             (list 57 321 88))
; 57
; 321
; 88
;
; The value returned by the call to `for-each` (not illustrated above) can be
; something arbitrary, such as true. Give an implementation of `for-each`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I used the `let` syntax to ignore the value of each procedure call.
; Alternatively, one can use the `cond` syntax where the `else` clause supports
; multiple expressions.

; For the return value, one good choice is to return the number of times `proc`
; was caled.
; It does not seem much useful here, but consider a more abstract version of
; `for-each` that takes a third parameter `filter` that conditionally applies
; `proc`. The return value simulataneously reports back the number of elements
; satisfying `filter`.
; However, for the current implementation, I'm simply returning true (`#t`).

(define (for-each proc items)
    (if (null? items)
        #t
        (let ((_ (proc (car items))))
            (for-each proc (cdr items)))))

(for-each (lambda (x)
              (newline)
              (display x))
          (list 57 321 88))

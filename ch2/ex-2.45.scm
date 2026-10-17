#lang sicp
(#%require sicp-pict)

; Exercise 2.45:
; `right-split` and `up-split` can be expressed as instances of a general
; spliting operation. Define a procedure `split` with the property that
; evaluating
;
; (define right-split (split beside below))
; (define up-split (split below beside))
;
; produces procedures `right-split` and `up-split` with the same behaviors as
; the ones already defined.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (split type1 type2)
    (define (f painter n)
        (if (= n 0)
            painter
            (let ((smaller (f painter (- n 1))))
                (type1 painter (type2 smaller smaller)))))

    f)

(define right-split (split beside below))
(define up-split (split below beside))

; Tests using the built-in painter `einstein` from the 'sicp-pict' package

(paint (up-split einstein 3))
(paint (right-split einstein 3))

#lang sicp
(#%require sicp-pict)

; Exercise 2.50:
; Define the transformation `flip-horiz`, which flips painters horizontally, and
; transformations that rotate painters counterclockwise by 180 degrees and 270
; degrees.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(define (rotate180 painter)
    (transform-painter painter
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

(define (rotate270 painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

; Tests using the built-in painter `einstein` from the 'sicp-pict' package
(paint einstein)
(paint (flip-horiz einstein))
(paint (rotate180 einstein))
(paint (rotate270 einstein))

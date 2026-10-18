#lang sicp
(#%require sicp-pict)

; Exercise 2.51:
; Define the `below` operation for painters. `below` takes two painters as
; arguments. The resulting painter, given a frame, draws with the first painter
; in the bottom of the frame and with the second painter in the top. Define
; `below` in two different ways—first by writing a procedure that is analogous
; to the `beside` procedure given above, and again in terms of `beside` and
; suitable rotation operations (from Exercise 2.50).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `below` in terms of `transform-painter`
(define (below-v1 painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-bottom (transform-painter painter1
                                               (make-vect 0.0 0.0)
                                               (make-vect 1.0 0.0)
                                               split-point))
              (paint-top (transform-painter painter2
                                            split-point
                                            (make-vect 1.0 0.5)
                                            (make-vect 0.0 1.0))))
            (lambda (frame)
                (paint-bottom frame)
                (paint-top frame)))))

; `below` in terms of `beside`, `rotate90` and `rotate270`
(define (below-v2 painter1 painter2)
    (rotate90 (beside (rotate270 painter1)
                      (rotate270 painter2))))

; Tests using the built-in painters `einstein` and `diagonal-shading` from the
; 'sicp-pict' package
(paint einstein)
(paint (below-v1 einstein diagonal-shading))
(paint (below-v2 einstein diagonal-shading))

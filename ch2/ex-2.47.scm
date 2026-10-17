#lang sicp

; Exercise 2.47:
; Here are two possible constructors for frames:
;
; (define (make-frame origin edge1 edge2)
;     (list origin edge1 edge2))
;
; (define (make-frame origin edge1 edge2)
;     (cons origin (cons edge1 edge2)))
;
; For each constructor supply the appropriate selectors to produce an
; implementation for frames.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First implementation

(define (make-frame-v1 origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame-v1 fr)
    (car fr))

(define (edge1-frame-v1 fr)
    (cadr fr))

(define (edge2-frame-v1 fr)
    (caddr fr))

; Second implementation

(define (make-frame-v2 origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame-v2 fr)
    (car fr))

(define (edge1-frame-v2 fr)
    (cadr fr))

(define (edge2-frame-v2 fr)
    (cddr fr))

; NOTE: No tests because it feels mundane for this kind of problems

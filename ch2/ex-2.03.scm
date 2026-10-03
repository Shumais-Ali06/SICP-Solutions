#lang sicp

; Exercise 2.3:
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of Exercise 2.2.) In terms of your constructors and selectors, create
; procedures that compute the perimeter and the area of a given rectangle. Now
; implement a different representation for rectangles. Can you design your
; system with suitable abstraction barriers, so that the same perimeter and area
; procedures will work using either representation?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definitions for point from Exercise 2.2

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (square x)
    (* x x))

(define (dist p1 p2)
    (sqrt (+ (square (- (x-point p1)
                        (x-point p2)))
             (square (- (y-point p1)
                        (y-point p2))))))
(define (print-point p)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Representation 1:
; Directly store the side lengths

(define (make-rect-1 length breadth)
    (cons length breadth))

(define (length-rect-1 r)
    (car r))

(define (breadth-rect-1 r)
    (cdr r))

(define (perimeter-rect-1 r)
    (* 2
       (+ (length-rect-1 r)
          (breadth-rect-1 r))))

(define (area-rect-1 r)
    (* (length-rect-1 r)
       (breadth-rect-1 r)))

(define (show-details-rect-1 r)
    (display "Rectangle with length = ")
    (display (length-rect-1 r))
    (display ", breadth = ")
    (display (breadth-rect-1 r))
    (display " has perimeter = ")
    (display (perimeter-rect-1 r))
    (display " and area = ")
    (display (area-rect-1 r))
    (newline))

(show-details-rect-1 (make-rect-1 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Representation 2:
; Store any three corner points (with p1 being adjacent to both p2 and p3)

(define (make-rect-2 p1 p2 p3)
    (cons p1
          (cons p2 p3)))

(define (p1-rect-2 r)
    (car r))

(define (p2-rect-2 r)
    (car (cdr r)))

(define (p3-rect-2 r)
    (cdr (cdr r)))

(define (length-rect-2 r)
    (dist (p1-rect-2 r)
          (p2-rect-2 r)))

(define (breadth-rect-2 r)
    (dist (p1-rect-2 r)
          (p3-rect-2 r)))

(define (perimeter-rect-2 r)
    (* 2
       (+ (length-rect-2 r)
          (breadth-rect-2 r))))

(define (area-rect-2 r)
    (* (length-rect-2 r)
       (breadth-rect-2 r)))

; Tests
(define (show-details-rect-2 r)
    (display "Rectangle with vertices p1 = ")
    (print-point (p1-rect-2 r))
    (display ", p2 = ")
    (print-point (p2-rect-2 r))
    (display ", p3 = ")
    (print-point (p3-rect-2 r))
    (display " has perimeter = ")
    (display (perimeter-rect-2 r))
    (display " and area = ")
    (display (area-rect-2 r))
    (newline))

(show-details-rect-2 (make-rect-2 (make-point 0 0)
                                  (make-point 0 1)
                                  (make-point 2 0)))

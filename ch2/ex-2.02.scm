#lang sicp

; Exercise 2.2:
; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and an ending point. Define
; a constructor `make-segment` and selectors `start-segment` and `end-segment`
; that define the representation of segments in terms of points. Furthermore, a
; point can be represented as a pair of numbers: the `x` coordinate and the `y`
; coordinate. Accordingly, specify a constructor `make-point` and selectors
; `x-point` and `y-point` that define this representation. Finally, using your
; selectors and constructors, define a procedure `midpoint-segment` that takes a
; line segment as argument and returns its midpoint (the point whose coordinates
; are the average of the coordinates of the endpoints). To try your procedures,
; you’ll need a way to print points:

; (define (print-point p)
;     (newline)
;     (display "(")
;     (display (x-point p))
;     (display ",")
;     (display (y-point p))
;     (display ")"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-point p)
    ; (newline)             ; Removed the mandatory newline at the beginning to
                            ; get greater flexibility in formatting the output
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

; Definitions for segment

(define (make-segment start end)
    (cons start end))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

; Definitions for point

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (average x y)
    (/ (+ x y) 2))

(define (midpoint-segment s)
    (let ((p1 (start-segment s))
          (p2 (end-segment s)))
        (cons (average (x-point p1)
                       (x-point p2))
              (average (y-point p1)
                       (y-point p2)))))

; Helper function to print our tests and results nicely, one per line
(define (display-midpoint p1 p2)
    (display "Midpoint of ")
    (print-point p1)
    (display " and ")
    (print-point p2)
    (display " is: ")
    (print-point (midpoint-segment (make-segment p1 p2)))
    (newline))

; Tests
(display-midpoint (make-point 1 2)
                  (make-point 3 4))

(display-midpoint (make-point 2 2)
                  (make-point 3 3))

(display-midpoint (make-point -1 -1)
                  (make-point 1 1))

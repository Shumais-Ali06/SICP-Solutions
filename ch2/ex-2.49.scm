#lang sicp
(#%require sicp-pict)

; Exercise 2.49:
; Use `segments->painter` to define the following primitive painters:
;
;   a. The painter that draws the outline of the designated frame.
;   b. The painter that draws an “X” by connecting opposite corners of the
;      frame.
;   c. The painter that draws a diamond shape by connecting the midpoints of
;      the sides of the frame.
;   d. The `wave` painter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: The `segments->painter` procedure used below comes from the imported
; 'sicp-pict' package.

; NOTE: The procedures below use the `vector-xcor` and `vector-ycor` selectors
; defined in the `sicp-pict` package instead of `xcor-vect` and `ycor-vect`
; defined in the book.

; Parts `a` thorugh `c` can be easily done using knowledge of only straight
; lines.

(define painter-a
    (segments->painter
     (list (make-segment (make-vect 0 0) (make-vect 0 1))
           (make-segment (make-vect 0 1) (make-vect 1 1))
           (make-segment (make-vect 1 1) (make-vect 1 0))
           (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define painter-b
    (segments->painter
     (list (make-segment (make-vect 0 0) (make-vect 1 1))
           (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define painter-c
    (segments->painter
     (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
           (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
           (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))

(paint painter-a)
(paint painter-b)
(paint painter-c)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: This procedure was entirely written by an LLM viz. GPT-6 Luna.

; TODO: Rewrite this procedure by-hand in fewer lines of code and without using
; language constructs like `let*`, `list-ref` that have not been introduced yet
; in the book.

(define (curve-through-points points)
    (let* ((n (length points))
           (segment-count (- n 1)))

        (define (point i)
            (list-ref points i))

        (define (coordinate p axis)
            (if (= axis 0)
                (vector-xcor p)
                (vector-ycor p)))

        ; Tangent at point i, for the x or y coordinate.
        (define (tangent i axis)
            (cond ((= i 0)
                   (- (coordinate (point 1) axis)
                      (coordinate (point 0) axis)))
                  ((= i (- n 1))
                   (- (coordinate (point i) axis)
                      (coordinate (point (- i 1)) axis)))
                  (else
                   (/ (- (coordinate (point (+ i 1)) axis)
                         (coordinate (point (- i 1)) axis))
                      2))))

        (define (hermite p0 p1 m0 m1 u)
            (let ((u2 (* u u))
                  (u3 (* u u u)))
                (+ (* (+ (* 2 u3) (* -3 u2) 1) p0)
                   (* (+ u3 (* -2 u2) u) m0)
                   (* (+ (* -2 u3) (* 3 u2)) p1)
                   (* (+ u3 (* -1 u2)) m1))))

        (define (coordinate-at axis i u)
            (hermite (coordinate (point i) axis)
                     (coordinate (point (+ i 1)) axis)
                     (tangent i axis)
                     (tangent (+ i 1) axis)
                     u))

        (lambda (t)
            (let* ((i (if (= t 1)
                          (- segment-count 1)
                          (floor (* t segment-count))))
                   (u (- (* t segment-count) i)))
                (make-vect (coordinate-at 0 i u)
                           (coordinate-at 1 i u))))))

(define (sample-curve curve n)
    (define (loop i)
        (if (> i n)
            nil
            (cons (curve (/ i n))
                  (loop (+ i 1)))))
    (loop 0))

; Generates segments by pairing each point with its successor. Direct recursion
; is used instead of `map` or `accumulate` to allow access to the next element.
(define (generate-segments points)
    (if (null? (cdr points))
        nil
        (cons (make-segment (car points)
                            (cadr points))
              (generate-segments (cdr points)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Sample points (x, y) for each curve of `wave` painter with 0 <= x, y <= 1

; These are all technically rational numbers but I instead chose to use floating
; point numbers rounded off to 5 decimal places for readabilty

; Steps to reproduce (final result may differ slightly):
; 1. Take a screenshot of `wave` painter from one of the figures in the book
; 2. Adjust the aspect-ratio to 1:1, if necessary.
; 3. Superimpose a NxN grid on the image. (I took a 6x6 grid.)
; 4. Calculate the one coordinate of each point that intersect the grid lines.
; 5. Appromixate the other coordinate by sight in terms of simple fractions,
;    e.g: 1/2, 2/3, 3/4, 4/5, 1/6.

(define top-left-curve
    (curve-through-points
     (list (make-vect 0       0.72222)
           (make-vect 0.16667 0.58333)
           (make-vect 0.33333 0.66667)
           (make-vect 0.41667 0.66667)
           (make-vect 0.33333 0.83333)
           (make-vect 0.41667 1))))

(define bottom-left-curve
    (curve-through-points
     (list (make-vect 0.00000 0.63333)
           (make-vect 0.16667 0.46667)
           (make-vect 0.33333 0.55555)
           (make-vect 0.36667 0.5)
           (make-vect 0.33333 0.33333)
           (make-vect 0.25    0))))

(define top-right-curve
    (curve-through-points
     (list (make-vect 0.53333 1)
           (make-vect 0.625   0.83333)
           (make-vect 0.54167 0.66667)
           (make-vect 0.66667 0.66667)
           (make-vect 0.83333 0.55555)
           (make-vect 1       0.36667))))

(define bottom-curve
    (curve-through-points
     (list (make-vect 0.375   0)
           (make-vect 0.41667 0.16667)
           (make-vect 0.5     0.3)
           (make-vect 0.58333 0.16667)
           (make-vect 0.63333 0))))

(define bottom-right-curve
    (curve-through-points
     (list (make-vect 0.75    0)
           (make-vect 0.7     0.16667)
           (make-vect 0.66667 0.25)
           (make-vect 0.63333 0.33333)
           (make-vect 0.61296 0.5)
           (make-vect 0.66667 0.55555)
           (make-vect 0.83333 0.44444)
           (make-vect 1       0.275))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define the painter for `wave` by considering segments of all its constituent
; curves.

(define painter-d
    (segments->painter
     (append
      (generate-segments (sample-curve top-left-curve 100))
      (generate-segments (sample-curve top-right-curve 100))
      (generate-segments (sample-curve bottom-left-curve 100))
      (generate-segments (sample-curve bottom-curve 100))
      (generate-segments (sample-curve bottom-right-curve 100)))))

(paint painter-d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BONUS: IT'S FUN TIME!

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                  (bottom-right (below right right))
                  (corner (corner-split painter (- n 1))))
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(paint (up-split painter-d 3))
(paint (right-split painter-d 3))
(paint (corner-split painter-d 3))
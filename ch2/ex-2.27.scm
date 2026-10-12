#lang sicp

; Exercise 2.27:
; Modify your reverse procedure of Exercise 2.18 to produce a `deep-reverse`
; procedure that takes a list as argument and returns as its value the list with
; its elements reversed and with all sublists deep-reversed as well. For example,
;
; (define x (list (list 1 2) (list 3 4)))
; x
; > ((1 2) (3 4))
; (reverse x)
; > ((3 4) (1 2))
; (deep-reverse x)
; > ((4 3) (2 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse items)
    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items)
                  (cons (car items) result))))

    (iter items nil))

(define (deep-reverse items)
    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items)
                  ; Recurse each pair element instead of simply iterating over
                  (cons (if (pair? (car items))
                            (deep-reverse (car items))
                            (car items))
                        result))))

    (iter items nil))

; Helper function to print the test results nicely
(define (test items)
    (display "Original: ")
    (display items)
    (newline)
    (display "Using reverse: ")
    (display (reverse items))
    (newline)
    (display "Using deep-reverse: ")
    (display (deep-reverse items))
    (newline)
    (newline))

; Tests (including the lists provided in Exercise 2.25)
(test (list (list 1 2) (list 3 4)))
(test (list 1 3 (list 5 7) 9))
(test (list (list 7)))
(test (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

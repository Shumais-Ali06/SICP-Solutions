#lang sicp

; Exercise 2.29:
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or
; another binary mobile. We can represent a binary mobile using compound data by
; constructing it from two branches (for example, using `list`):

(define (make-mobile left right)
    (list left right))

; A branch is constructed from a `length` (which must be a number) together with
; a structure, which may be either a number (representing a simple weight) or
; another mobile:

(define (make-branch length structure)
    (list length structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a. Write the corresponding selectors `left-branch` and `right-branch`, which
;    return the branches of a mobile, and `branch-length` and `branch-structure`,
;    which return the components of a branch.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; b. Using your selectors, define a procedure `total-weight` that returns the
;    total weight of a mobile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (structure-weight struct)
    (if (pair? struct)
        (total-weight struct)
        struct))

(define (total-weight mobile)
    (+ (structure-weight (branch-structure (left-branch mobile)))
       (structure-weight (branch-structure (right-branch mobile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c. A mobile is said to be balanced if the torque applied by its top-left
;    branch is equal to that applied by its top-right branch (that is, if the
;    length of the left rod multiplied by the weight hanging from that rod is
;    equal to the corresponding product for the right side) and if each of the
;    submobiles hanging off its branches is balanced. Design a predicate that
;    tests whether a binary mobile is balanced.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (balanced-mobile? mobile)
    (let ((left-struct (branch-structure (left-branch mobile)))
          (right-struct (branch-structure (right-branch mobile))))
        ; Check for torque balance for the current mobile and any submobiles,
        ; if they exist
        (and (= (* (branch-length (left-branch mobile))
                   (structure-weight left-struct))
                (* (branch-length (right-branch mobile))
                   (structure-weight right-struct)))
             (or (not (pair? left-struct))
                 (balanced-mobile? left-struct))
             (or (not (pair? right-struct))
                 (balanced-mobile? right-struct)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; d. Suppose we change the representation of mobiles so that the constructors
;    are
;
;    (define (make-mobile left right) (cons left right))
;    (define (make-branch length structure)
;        (cons length structure))
;
; How much do you need to change your programs to convert to the new
; representation?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Only the following two changes are required for the selectors
; Toggle comments with the earlier definitions to test

; (define (right-branch mobile)
;     (cdr mobile))
;
; (define (branch-structure branch)
;     (cdr branch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test cases

(define simple-balanced
  (make-mobile
    (make-branch 2 3)
    (make-branch 3 2)))
; Expected: #t   ; 2×3 = 3×2

(define simple-unbalanced
  (make-mobile
    (make-branch 2 3)
    (make-branch 3 3)))
; Expected: #f   ; 2×3 ≠ 3×3

(define inner-balanced
  (make-mobile
    (make-branch 2 3)
    (make-branch 3 2)))
; Total weight: 5

(define nested-balanced
  (make-mobile
    (make-branch 2 inner-balanced)
    (make-branch 5 2)))
; Expected: #t   ; Top torques are 2×5 and 5×2; inner mobile is balanced

(define inner-unbalanced
  (make-mobile
    (make-branch 1 1)
    (make-branch 1 2)))
; Total weight: 3, but its torques are unequal

(define nested-with-unbalanced-submobile
  (make-mobile
    (make-branch 2 inner-unbalanced)
    (make-branch 3 2)))
; Expected: #f   ; Top torques balance (2×3=3×2), but the inner mobile does not

(balanced-mobile? simple-balanced)                    ; #t
(balanced-mobile? simple-unbalanced)                  ; #f
(balanced-mobile? nested-balanced)                    ; #t
(balanced-mobile? nested-with-unbalanced-submobile)   ; #f

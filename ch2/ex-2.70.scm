#lang sicp

; Exercise 2.70:
; The following eight-symbol alphabet with associated relative frequencies was
; designed to efficiently encode the lyrics of 1950s rock songs. (Note that the
; “symbols” of an “alphabet” need not be individual letters.)
;
; A    2   GET 2   SHA 3   WAH 1
; BOOM 1   JOB 2   NA  16  YIP 9
;
; Use `generate-huffman-tree` (Exercise 2.69) to generate a corresponding
; Huffman tree, and use encode (Exercise 2.68) to encode the following message:
;
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom
;
; How many bits are required for the encoding? What is the smallest number of
; bits that would be needed to encode this song if we used a fixed-length code
; for the eight-symbol alphabet?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Huffman trees procedures as defined in Section 2.3.4

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (eq? (car object) 'leaf))

(define (symbol-leaf x)
    (cadr x))

(define (weight-leaf x)
    (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree)
    (car tree))

(define (right-branch tree)
    (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

; Ordered-set procedures as defined in Section 2.3.4

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)       ; symbol
                                   (cadr pair))     ; frequency
                        (make-leaf-set (cdr pairs))))))

; `encode` as defined in Exercise 2.68

(define (encode-symbol sym tree)
    (define (search tree path)
        (cond ((null? tree) #f)
              ((leaf? tree)
               (if (eq? (symbol-leaf tree) sym)
                   path
                   #f))
              (else (or (search (left-branch tree)
                                (append path '(0)))
                        (search (right-branch tree)
                                (append path '(1)))))))

    (let ((result (search tree '())))
        (if (eq? result #f)
            (error "unrecognized symbol: ENCODE-SYMBOL" sym)
            result)))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

; `generate-huffman-tree` as defined in Exercise 2.69

(define (successive-merge elements)
  (if (null? (cdr elements))
      (car elements)
      (successive-merge
       (adjoin-set (make-code-tree (car elements)
                                   (cadr elements))
                   (cddr elements)))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rock-tree
    (generate-huffman-tree
     '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (NA 6) (WAH 1) (YIP 9))))

(define messages
    (list '(GET A JOB)
          '(SHA NA NA NA NA NA NA NA NA)
          '(GET A JOB)
          '(SHA NA NA NA NA NA NA NA NA)
          '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
          '(SHA BOOM)))

(for-each (lambda (x)
              (display (encode x rock-tree))
              (newline))
          messages)

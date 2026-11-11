#lang sicp

; Exercise 2.89:
; Define procedures that implement the term-list representation described above
; as appropriate for dense polynomials.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
    ;; internal procedures
    ;; representation of poly
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    ; ⟨procedures same-variable? and variable? from section 2.3.2⟩
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

    ; Modified code begins
    (define (=zero-poly? p)
        (zero-terms? (term-list p)))
    ; Modified code ends
    ; Newly added begins
    (define (make-zero-list n)
        (if (= n 0)
            (the-empty-termlist)
            (cons 0 (make-zero-list (- n 1)))))
    (define (trim-leading-zeros L)
        (cond ((empty-termlist? L) (the-empty-termlist))
              ((=zero? (first-term L)) (trim-leading-zeros (rest-terms L)))
              (else L)))
    (define (zero-terms? L)
        (or (empty-termlist? L)
            (and (=zero? (first-term L))
                 (zero-terms? (rest-terms L)))))
    ; Newly added ends

    ;; representation of terms and term lists
    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))))
    ; Modified code begins
    (define (add-terms L1 L2)
        (define (add-coefficients L1 L2)
            (if (empty-termlist? L1)
                '()
                (cons (add (first-term L1) (first-term L2))
                      (add-coefficients (rest-terms L1) (rest-terms L2)))))

        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else
               (let ((n1 (length L1))
                     (n2 (length L2)))
                   (let ((L1*
                          (append (make-zero-list (- (max n1 n2) n1))
                                  L1))
                         (L2*
                          (append (make-zero-list (- (max n1 n2) n2))
                                  L2)))
                       (trim-leading-zeros (add-coefficients L1* L2*)))))))
    ; Modified code ends

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: MUL-POLY" (list p1 p2))))
    ; Modified code begins
    (define (mul-terms L1 L2)
        (if (or (empty-termlist? L1)
                (empty-termlist? L2))
            '()
            (add-terms (append (map (lambda (c) (mul (first-term L1) c))
                                    L2)
                               (make-zero-list (- (length L1) 1)))
                       (mul-terms (rest-terms L1) L2))))
    ; Modified code ends

    (define (negate-poly p)
        (make-poly (variable p)
                   (negate-terms (term-list p))))
    ; Modified code begins
    (define (negate-terms L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (cons (negate (first-term L))
                  (negate-terms (rest-terms L)))))
    ; Modified code ends
    (define (sub-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (add-poly p1 (negate-poly p2))
            (error "Polys not in same var: SUB-POLY" (list p1 p2))))

    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
         (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
         (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
         (lambda (var terms) (tag (make-poly var terms))))
    (put '=zero? '(polynomial)
         (lambda (p) (=zero-poly? p)))
    (put 'negate '(polynomial)
         (lambda (p) (tag (negate-poly p))))
    (put 'sub '(polynomial polynomial)
         (lambda (p1 p2) (tag (sub-poly p1 p2))))
    'done)

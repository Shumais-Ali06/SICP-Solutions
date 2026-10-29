#lang sicp

; Exercise 2.74:
; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company
; consisting of a large number of independent divisions located all over the
; world. The company’s computer facilities have just been interconnected by
; means of a clever network-interfacing scheme that makes the entire network
; appear to any user to be a single computer. Insatiable’s president, in her
; first attempt to exploit the ability of the network to extract administrative
; information from division files, is dismayed to discover that, although all
; the division files have been implemented as data structures in Scheme, the
; particular data structure used varies from division to division. A meeting of
; division managers is hastily called to search for a strategy to integrate the
; files that will satisfy headquarters’ needs while preserving the existing
; autonomy of the divisions. Show how such a strategy can be implemented with
; data-directed programming. As an example, suppose that each division’s
; personnel records consist of a single file, which contains a set of records
; keyed on employees’ names. The structure of the set varies from division to
; division. Furthermore, each employee’s record is itself a set (structured
; differently from division to division) that contains information keyed under
; identifiers such as address and salary. In particular:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a. Implement for headquarters a `get-record` procedure that retrieves a
;    specified employee’s record from a specified personnel file. The procedure
;    should be applicable to any division’s file. Explain how the individual
;    divisions’ files should be structured. In particular, what type information
;    must be supplied?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Each division should tag its personnel file with a type identifying its file
; representation. The corresponding `get-record` method handles that
; representation and returns the employee record with a type tag. The tag lets
; headquarters dispatch operations without knowing how the division stores its
; data.

(define (get-record emp-name division-file)
    ((get 'get-record (type-tag division-file))
     (contents division-file)
     emp-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; b. Implement for headquarters a `get-salary` procedure that returns the salary
;    information from a given employee’s record from any division’s personnel
;    file. How should the record be structured in order to make this operation
;    work?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-salary emp-name division-file)
    (let ((emp-record (get-record emp-name division-file)))
        (if emp-record
            ((get 'get-salary (type-tag emp-record))
             (contents emp-record))
            #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c. Implement for headquarters a `find-employee-record` procedure. This should
;    search all the divisions’ files for the record of a given employee and
;    return the record. Assume that this procedure takes as arguments an
;    employee’s name and a list of all the divisions’ files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-employee-record emp-name list-of-division-files)
    (if (null? list-of-division-files)
        #f
        (or (get-record (car list-of-division-files) emp-name)
            (find-employee-record emp-name
                                  (cdr list-of-division-files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; d. When Insatiable takes over a new company, what changes must be made in
;    order to incorporate the new personnel information into the central system?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; When a new company joins, add code that tags its files and installs the
; appropriate `get-record` method; also install a `get-salary` method for its
; employee-record representation if that representation is new. The headquarters
; procedures need not change.


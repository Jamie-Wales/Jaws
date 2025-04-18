;; utilities.scm
(define-library (utilities)
  (export filter fold-left fold-right sort ; Existing
          any? every? find find-tail remove partition ; List Predicates/Search/Manipulation HOFs
          compose identity constantly) ; Function Manipulation HOFs

  (import (base)      
          (list)      
          )   

  (begin
    (define (filter pred lst)
      (cond
        ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

    ;; (fold-left func init lst) -> value
    (define (fold-left func init lst)
      (let loop ((l lst) (acc init))
        (if (null? l)
            acc
            (loop (cdr l) (func (car l) acc)))))

(define (sort lst compare-proc)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append (sort (filter (lambda (x) (compare-proc x pivot)) rest) compare-proc)
                (list pivot)
                (sort (filter (lambda (x) (not (compare-proc x pivot))) rest) compare-proc)))))

    (define (fold-right func init lst)
      (if (null? lst)
          init
          (func (car lst) (fold-right func init (cdr lst)))))

    ;; (reverse lst) -> list
    (define (reverse lst)
      (let loop ((original lst) (result '()))
        (if (null? original)
            result
            (loop (cdr original) (cons (car original) result)))))

    ;; --- New Higher-Order Functions ---

    ;; (any? pred lst) -> boolean
    ;; Checks if predicate pred holds for at least one element in lst.
    (define (any? pred lst)
      (let loop ((current-list lst))
        (cond
          ((null? current-list) #f)          ; Reached end, none found
          ((pred (car current-list)) #t)     ; Found one, return true
          (else (loop (cdr current-list)))))) ; Keep looking

    ;; (every? pred lst) -> boolean
    ;; Checks if predicate pred holds for all elements in lst.
    (define (every? pred lst)
      (let loop ((current-list lst))
        (cond
          ((null? current-list) #t)          ; Reached end, all passed
          ((not (pred (car current-list))) #f) ; Found one failing, return false
          (else (loop (cdr current-list)))))) ; Keep checking

    ;; (find pred lst) -> element | #f
    ;; Returns the first element in lst that satisfies predicate pred,
    ;; or #f if no such element exists.
    (define (find pred lst)
      (cond
        ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find pred (cdr lst)))))

    ;; (find-tail pred lst) -> sublist | #f
    ;; Returns the first tail (sublist) of lst whose car satisfies
    ;; predicate pred, or #f if no such tail exists.
    (define (find-tail pred lst)
       (cond
         ((null? lst) #f)
         ((pred (car lst)) lst) ; Return the list starting here
         (else (find-tail pred (cdr lst)))))

    ;; (remove pred lst) -> list
    ;; Returns a list containing elements from lst that *do not*
    ;; satisfy predicate pred. (Equivalent to (filter (lambda (x) (not (pred x))) lst))
    (define (remove pred lst)
       (filter (lambda (x) (not (pred x))) lst)) ; Use existing filter

    ;; (partition pred lst) -> list list
    ;; Returns two lists (as multiple values): one with elements satisfying pred,
    ;; and one with elements not satisfying pred.
    (define (partition pred lst)
      (let loop ((original lst) (true-list '()) (false-list '()))
        (if (null? original)
            ;; Return the reversed lists to maintain original relative order
            (values (reverse true-list) (reverse false-list))
            (let ((element (car original)))
              (if (pred element)
                  (loop (cdr original) (cons element true-list) false-list)
                  (loop (cdr original) true-list (cons element false-list)))))))

    ;; (compose f g ...) -> procedure
    ;; Returns a procedure that is the composition of the input procedures.
    ;; (compose f g h) => (lambda (x) (f (g (h x))))
    (define (compose . procs)
       (if (null? procs)
           identity ; Composition of no functions is identity
           (let ((f (car procs))
                 (rest-procs (cdr procs)))
              (if (null? rest-procs)
                  f ; Composition of one function is the function itself
                  ;; Recursively compose the rest, then compose f with that result
                  (let ((g (apply compose rest-procs)))
                    (lambda args ; Handle multiple arguments for the innermost function
                       (f (apply g args))))))))

    ;; (identity x) -> x
    ;; The identity function.
    (define (identity x) x)

    ;; (constantly val) -> procedure
    ;; Returns a procedure that ignores its arguments and always returns val.
    (define (constantly val)
      (lambda args ; Use args to accept any number of arguments
        val))

  ) ; end begin
) ; end define-library

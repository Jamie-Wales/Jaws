(define-library
  (loops)
  (export while
          for
          repeat
          until
          do-while
          for-each-with-index
          for-range
          iterate
          fold-loop
          select-case
          nested-loop
          for-each)
  (import (base))

  (import (list-utils))

  (begin
    (define-syntax while
      (syntax-rules ()
        ((while condition body ...)
         (let loop ()
           (if condition
               (begin
                 body
                 ...
                 (loop))
               #f)))))

    (define (for-each proc . lists)
      (define (loop current-lists)
        (define (any-null? lists-to-check)
          (cond
            ((null? lists-to-check) #f) ; Base case: No lists left to check, so none were null.
            ((null? (car lists-to-check)) #t) ; Found an empty list.
            (else (any-null? (cdr lists-to-check))))) ; Recursively check the rest.
        (if (not (any-null? current-lists))
            (begin
              (apply proc (map car current-lists))

              (loop (map cdr current-lists)))))
      )
    (define-syntax do-while
      (syntax-rules ()
        ((do-while condition
                   body
                   ...)
         (let loop ()
           (begin
             body
             ...
             (if condition
                 (loop)
                 #f))))))

    (define-syntax fold-loop
      (syntax-rules (init in)
        ((fold-loop acc init initial-value val in list body ...)
         (let loop ((remaining-list list)
                    (current-acc initial-value))
           (if (null? remaining-list)
               current-acc
               (let ((val (car remaining-list)))
                 (let ((next-acc (let ((acc current-acc))
                                   (begin
                                     body
                                     ...))))
                   (loop (cdr remaining-list) next-acc))))))))

    (define-syntax for
      (syntax-rules (in as)
        ((for element in
           list
           body
           ...)
         (map (lambda (element)
                body
                ...)
              list))
        ((for list as
           element
           body
           ...)
         ; Alternative syntax
         (for element in
           list
           body
           ...))))

    (define-syntax for-each
      (syntax-rules (in)
        ;; 'in' is a literal keyword
        ((for-each element in list body ...)
         (let loop ((current-list list))
           (if (not (null? current-list))
               (begin
                 (let ((element (car current-list)))
                   body
                   ...)
                 (loop (cdr current-list)))
               #f)))))

    (define-syntax for-each-with-index
      (syntax-rules (in)
        ((for-each-with-index (element index) in list body ...)
         (let loop ((lst list)
                    (i 0))
           (if (not (null? lst))
               (begin
                 (let ((element (car lst))
                       (index i))
                   body
                   ...)
                 (loop (cdr lst) (+ i 1)))
               #f)))))

    (define-syntax for-range
      (syntax-rules ()
        ((for-range var from to body ...)
         (let loop ((var from))
           (if (<= var to)
               (begin
                 body
                 ...
                 (loop (+ var 1)))
               #f)))))

    (define-syntax iterate
      (syntax-rules (from to by)
        ((iterate var from start to end by step body ...)
         (let loop ((var start))
           (if (<= var end)
               (begin
                 body
                 ...
                 (loop (+ var step)))
               #f)))))

    (define-syntax nested-loop
      (syntax-rules (outer inner)
        ((nested-loop (outer outer-var outer-from outer-to)
                      (inner inner-var inner-from inner-to)
                      body
                      ...)
         (for-range outer-var
                    outer-from
                    outer-to
                    (for-range inner-var inner-from inner-to body ...)))))

    (define-syntax repeat
      (syntax-rules ()
        ((repeat n body ...)
         (let loop ((i 0))
           (if (< i n)
               (begin
                 body
                 ...
                 (loop (+ i 1)))
               #f)))))

    (define-syntax select-case
      (syntax-rules (else)
        ((select-case key ((v1) e1 ...) rest ... (else ee ...))
         (if (memq key '(v1))
             (begin
               e1
               ...)
             (select-case key rest ... (else ee ...))))
        ((select-case key ((v1 v2 ...) e1 ...) rest ... (else ee ...))
         (if (memq key '(... v1 v2))
             (begin
               e1
               ...)
             (select-case key rest ... (else ee ...))))
        ((select-case key (else ee ...))
         (begin
           ee
           ...))
        ((select-case key ((v1) e1 ...) rest ...)
         (if (memq key '(v1))
             (begin
               e1
               ...)
             (select-case key rest ...)))
        ((select-case key ((v1 v2 ...) e1 ...) rest ...)
         (if (memq key '(... v1 v2))
             (begin
               e1
               ...)
             (select-case key rest ...)))
        ((select-case key)
         #f)))

    (define-syntax until
      (syntax-rules ()
        ((until condition body ...)
         (let loop ()
           (if (not condition)
               (begin
                 body
                 ...
                 (loop))
               #f)))))))

(define-library (loops)

  (export while for repeat until do-while for-each-with-index
          for-range iterate fold-loop select-case nested-loop)

  (import 
          (list-utils))

  (begin

    (define-syntax while
      (syntax-rules ()
        ((while condition body ...)
         (let loop ()
           (if condition
               (begin
                 body ...
                 (loop))
               #f))))) ; R7RS requires an alternative for if

    (define-syntax for ; Note: 'map' needs to be available from (scheme base) or similar
      (syntax-rules (in as)
        ((for element in list body ...)
         (map (lambda (element) body ...) list))
        ((for list as element body ...) ; Alternative syntax
         (for element in list body ...))))

    (define-syntax repeat
      (syntax-rules ()
        ((repeat n body ...)
         (let loop ((i 0))
           (when (< i n)
             (begin body ...)
             (loop (+ i 1)))))))

    (define-syntax until
      (syntax-rules ()
        ((until condition body ...)
         (let loop ()
           (if (not condition)
               (begin
                 body ...
                 (loop))
               #f))))) ; R7RS requires an alternative for if

    (define-syntax do-while
      (syntax-rules ()
        ((do-while condition body ...)
         (let loop ()
           (begin
             body ...
             (if condition
                 (loop)
                 #f)))))) ; R7RS requires an alternative for if

    (define-syntax for-each-with-index
      (syntax-rules ()
        ((for-each-with-index (element index) in list body ...)
         (let loop ((lst list) (i 0))
           (unless (null? lst)
             (let ((element (car lst))
                   (index i)) ; Bind index locally
               body ...)
             (loop (cdr lst) (+ i 1)))))))

    (define-syntax for-range
      (syntax-rules ()
        ((for-range var from to body ...)
         (let loop ((var from))
           (when (<= var to) ; Assumes numerical comparison
             body ...
             (loop (+ var 1)))))))

    (define-syntax iterate
      (syntax-rules (from to by) ; Define keywords if needed for clarity
        ((iterate var from start to end by step body ...)
         (let loop ((var start))
           (if (<= var end) ; Assumes numerical comparison
               (begin
                 body ...
                 (loop (+ var step)))
               #f))))) ; R7RS requires an alternative for if

    (define-syntax fold-loop
      (syntax-rules (init) ; Define keywords if needed
        ((fold-loop acc init initial-value val in list body ...) ; 'val' for loop var
         (let loop ((remaining list) (acc initial-value))
           (if (null? remaining)
               acc
               (loop (cdr remaining)
                     (let ((val (car remaining))) ; Bind current element
                       body ...))))))) ; Body uses 'val' and 'acc'

    (define-syntax select-case
      (syntax-rules (else)
        ((select-case key ((v1) e1 ...) rest ... (else ee ...))
         (if (memq key '(v1)) (begin e1 ...) (select-case key rest ... (else ee ...))))
        ((select-case key ((v1 v2 ...) e1 ...) rest ... (else ee ...))
         (if (memq key '(v1 v2 ...)) (begin e1 ...) (select-case key rest ... (else ee ...))))
        ((select-case key (else ee ...)) (begin ee ...))
        ((select-case key ((v1) e1 ...) rest ...)
         (if (memq key '(v1)) (begin e1 ...) (select-case key rest ...)))
        ((select-case key ((v1 v2 ...) e1 ...) rest ...)
         (if (memq key '(v1 v2 ...)) (begin e1 ...) (select-case key rest ...)))
        ((select-case key) #f))) ; Return #f or undefined if no match and no else

    (define-syntax nested-loop
      (syntax-rules (outer inner) ; Define keywords if needed
        ((nested-loop (outer outer-var outer-from outer-to)
                      (inner inner-var inner-from inner-to)
                      body ...)
         (for-range outer-var outer-from outer-to
           (for-range inner-var inner-from inner-to
             body ...)))))))

   ;; end begin
 ;; end define-library

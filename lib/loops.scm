(import base list)

(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (if condition
           (begin
             body ...
             (loop))
           #f)))))


(define-syntax for
  (syntax-rules (in as)
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list))
    ((for list as element body ...)
     (for element in list body ...))))


(define-syntax repeat
  (syntax-rules ()
    ((repeat n body ...)
     (let loop ((i 0))
       (when (< i n)
         (begin body ...)
         (loop (+ i 1)))))))
;;
(define-syntax until
  (syntax-rules ()
    ((until condition body ...)
     (let loop ()
       (if (not condition)
           (begin
             body ...
             (loop))
           #f)))))

;; Do-while - always executes body at least once
(define-syntax do-while
  (syntax-rules ()
    ((do-while condition body ...)
     (let loop ()
       (begin
         body ...
         (if condition
             (loop)
             #f))))))

;; For-each with index
(define-syntax for-each-with-index
  (syntax-rules ()
    ((for-each-with-index (element index) in list body ...)
     (let loop ((lst list) (i 0))
       (unless (null? lst)
         (let ((element (car lst))
               (index i))
           body ...
           (loop (cdr lst) (+ i 1))))))))

;; Range-based for loop
(define-syntax for-range
  (syntax-rules ()
    ((for-range var from to body ...)
     (let loop ((var from))
       (when (<= var to)
         body ...
         (loop (+ var 1)))))))

;; Iterate with step
(define-syntax iterate
  (syntax-rules (from to by)
    ((iterate var from start to end by step body ...)
     (let loop ((var start))
       (if (<= var end)
           (begin
             body ...
             (loop (+ var step)))
           #f)))))

;; Fold-like loop
(define-syntax fold-loop
  (syntax-rules (init)
    ((fold-loop acc init val in list body ...)
     (let loop ((remaining list) (acc val))
       (if (null? remaining)
           acc
           (loop (cdr remaining) 
                 (let ((element (car remaining)))
                   body ...)))))))

(define-syntax select-case
  (syntax-rules (else)
    ((select-case key ((v1) e1 ...) rest ... (else ee ...))
     (if (memq key '(v1))
         (begin e1 ...)
         (select-case key rest ... (else ee ...))))
    ((select-case key ((v1 v2 ...) e1 ...) rest ... (else ee ...))
     (if (memq key '(v1 v2 ...))
         (begin e1 ...)
         (select-case key rest ... (else ee ...))))
    ((select-case key (else ee ...))
     (begin ee ...))
    
    ((select-case key ((v1) e1 ...) rest ...)
     (if (memq key '(v1))
         (begin e1 ...)
         (select-case key rest ...)))
    ((select-case key ((v1 v2 ...) e1 ...) rest ...)
     (if (memq key '(v1 v2 ...))
         (begin e1 ...)
         (select-case key rest ...)))
    ((select-case key)
     '())))

(define-syntax nested-loop
  (syntax-rules (outer inner)
    ((nested-loop (outer outer-var outer-from outer-to)
                 (inner inner-var inner-from inner-to)
                 body ...)
     (for-range outer-var outer-from outer-to
       (for-range inner-var inner-from inner-to
         body ...)))))


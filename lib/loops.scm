(import base)
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

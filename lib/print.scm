
(define-syntax print
  (syntax-rules ()
    ((print expr)
     (display expr))))

(define-syntax println
  (syntax-rules ()
    ((println expr)
     (begin (display expr) (newline)))))


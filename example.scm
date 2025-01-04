(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
     (if condition
         (begin body ...)
         #f))))

(define-syntax match
  (syntax-rules (list quote)
    ;; Empty list pattern
    ((match expr
       ((list) body ...)
       rest ...)
     (let ((value expr))
       (if (null? value)
           (begin body ...)
           (match value rest ...))))

    ;; Quoted symbol pattern
    ((match expr
       ((quote sym) body ...)
       rest ...)
     (let ((value expr))
       (if (eq? value (quote sym))
           (begin body ...)
           (match value rest ...))))

    ;; Default case
    ((match expr
       (default body ...))
     (begin body ...))

    ;; No matches
    ((match expr)
     #f)))
(when (> 5 3)
  (display "5 is greater than 3")
  (newline))
(match '()
  ((list) (display "Found empty list"))
  ((quote x) (display "Found x"))
  (default (display "No match")))

(match 'x
  ((list) (display "Found empty list"))
  ((quote x) (display "Found x"))
  (default (display "No match")))

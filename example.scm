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
       (if (eq? value sym)  ;; Changed this line - compare directly with sym
           (begin body ...)
           (match value rest ...))))

    ;; Default case
    ((match expr
       (default body ...))
     (begin body ...))

    ;; No matches
    ((match expr)
     #f)))

(match '()
  ((list) (display "**** FOUND empty list ****"))
  ((quote x) (display "**** FOUND x *****"))
  (default (display "No match")))

(match 'x
  ((list) (display "**** FOUND empty list ****"))
  ((quote x) (display "**** FOUND x *****"))
  (default (display "No match")))

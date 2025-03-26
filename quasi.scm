(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((quasiquote (unquote expr))
     expr)
    ((quasiquote ((unquote-splicing expr) . rest))
     (append expr (quasiquote rest)))
    ((quasiquote (head . ((unquote-splicing expr) . rest)))
     (cons (quasiquote head)
           (append expr (quasiquote rest))))
    ((quasiquote (head . tail))
     (cons (quasiquote head)
           (quasiquote tail)))
    ((quasiquote expr)
     quote expr)))


(quasiquote 
  (a 
    (b (unquote (+ 1 2))) 
    c))

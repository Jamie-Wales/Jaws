(define-syntax let*
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((name value)) body ...)
     (let ((name value))
       body ...))
    ((_ ((name1 value1) (name2 value2) rest ...) body ...)
     (let ((name1 value1))
       (let* ((name2 value2) rest ...)
         body ...)))))

(let* ((x 1) (y (+ x 1)) (z (+ x 2)))
  (* x y))



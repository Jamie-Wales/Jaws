(define-syntax when
 (syntax-rules ()
  ((when test
    body)
   =>
   (if test
    body))))

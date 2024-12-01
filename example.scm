(define-syntax myLet
  (syntax-rules ()
    ((myLet ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

(myLet ((x 10)) (display x))

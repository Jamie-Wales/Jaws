(define-syntax when
  (syntax-rule (when test body) =>
               (if test body)))

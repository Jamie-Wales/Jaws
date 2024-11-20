(define-syntax when
  (syntax-rule (when test body) =>
               (if test body)))
(define-syntax my-first
  (syntax-rule (my-first x _) =>
               x))

(define-syntax my-list
  (syntax-rule (my-list x ...) =>
               (list x ...)))

(define-syntax my-or
  (syntax-rule (my-or x y ...) =>
               (let ((temp x))
                 (if temp temp (my-or y ...)))))

(when (< 1 10) (display "hello friendo"))


(my-or 1 2 3)
(my-list 1 2 3)
(my-first '(1 2 3 4))

(define-syntax push!
  (syntax-rules () ((_ item lst-var) (set! lst-var (cons item lst-var)))))

(define my-list '(b c))
(let ((cons +)) 
  (define temp 99) ; User variable, potentially relevant if 'set!' was also captured

  (push! 'a my-list)) ; Tries to do (set! my-list (cons 'a my-list))

(display my-list) ; What is my-list now?
(newline)
(display temp) ; Was temp affected? (Checks if set! was captured)

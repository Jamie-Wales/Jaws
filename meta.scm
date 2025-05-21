(define *math-env* '())

(define (lookup-var var env)
  (let ((pair (assq var env)))
    (if pair
        (cadr pair)
        (error "Unbound variable:" var))))

(define (define-var! var val)
  (let ((pair (assq var *math-env*)))
    (if pair
        (set-cdr! pair val)
        (set! *math-env* (cons (cons var val) *math-env*))))
  var)

(define (math-eval expr)
  (cond
    ((number? expr) expr)
    ((symbol? expr) (lookup-var expr *math-env*))
    ((list? expr)
     (if (null? expr)
         (error "Cannot evaluate empty list")
         (let ((op (car expr))
               (args (cdr expr)))
           (cond
             ((eq? op 'define)
              (if (and (= (length args) 2) (symbol? (car args)))
                  (define-var! (car args) (math-eval (cadr args)))
                  (error "Invalid define syntax. Use (define symbol value)")))
             (else
              (let ((proc (math-eval op))
                    (evaled-args (map math-eval args)))
                (if (procedure? proc)
                    (apply proc evaled-args)
                    (error "Not a procedure:" op))))))))
    (else (error "Cannot evaluate expression type:" expr))))

(define-var! '+ +)
(define-var! '- -)
(define-var! '* *)
(define-var! '/ /)
(define-var! '= =)
(define-var! '< <)
(define-var! '> >)
(define-var! 'display display)

(newline)
(display "--- Direct Evaluation Examples (using original math-lib logic) ---") (newline)
(math-eval '(define a 20))
(math-eval '(define b 5))
(display "(+ a b) => ") (math-eval '(display (+ a b))) (newline)
(display "(* a (- b 1)) => ") (math-eval '(display (* a (- b 1)))) (newline)
(display "(/ a b) => ") (math-eval '(display (/ a b))) (newline)
(display "Current Env: ") (display *math-env*) (newline)

(define macro-env (list))

(define get-macro
  (lambda (name)
    ((lambda (pair)
       (if pair (cdr pair) #f))
     (assq name macro-env))))

(define set-macro!
  (lambda (name transformer)
    (set! macro-env
          (cons (cons name transformer) macro-env))))

(define pattern-match
  (lambda (pattern expr literals bindings)
    ((lambda (try-match)
       (cond
         ((symbol? pattern)
          (if ((lambda (is-literal)
                 (if is-literal
                     (if (symbol? expr)
                         (if (eq? expr pattern)
                             bindings
                             #f)
                         #f)
                     (cons (cons pattern expr) bindings)))
               (member pattern literals))
              bindings
              (cons (cons pattern expr) bindings)))

         ((pair? pattern)
          (if (pair? expr)
              ((lambda (first-match)
                 (if first-match
                     (pattern-match (cdr pattern)
                                    (cdr expr)
                                    literals
                                    first-match)
                     #f))
               (pattern-match (car pattern)
                              (car expr)
                              literals
                              bindings))
              #f))

         (else (if (equal? pattern expr)
                   bindings
                   #f))))
     #f)))

(define find-match
  (lambda (expr patterns literals)
    (if (null? patterns)
        #f
        ((lambda (match)
           (if match
               (cons ((lambda (template rest) template)
                      (car (cdr patterns))
                      (cdr (cdr patterns)))
                     match)
               (find-match expr
                           (cdr (cdr patterns))
                           literals)))
         (pattern-match (car patterns)
                        expr
                        literals
                        (list))))))

(define expand-template
  (lambda (template bindings)
    (cond
      ((symbol? template)
       ((lambda (binding)
          (if binding
              (cdr binding)
              template))
        (assq template bindings)))

      ((pair? template)
       (cons (expand-template (car template) bindings)
             (expand-template (cdr template) bindings)))

      (else template))))

(define syntax-rules-proc
  (lambda (literals patterns)
    (lambda (expr)
      ((lambda (matches)
         (if matches
             (expand-template (car matches)
                              (cdr matches))
             (error "No matching pattern")))
       (find-match expr (cdr patterns) literals)))))

(define expand-macro
  (lambda (expr)
    (if (pair? expr)
        (if (symbol? (car expr))
            ((lambda (macro)
               (if macro
                   (expand-macro (macro expr))
                   expr))
             (get-macro (car expr)))
            expr)
        expr)))

(define define-syntax-proc
  (lambda (name transformer)
    (set-macro! name transformer)))


(define-syntax-proc when
  (syntax-rules-proc (list)
                     (list
                      (list 'when 'test 'expr)
                      (list 'if 'test 'expr #f))))

; Then let's just expand and print a test expression
(display (expand-macro '(when (> 1 0) (display "True!"))))

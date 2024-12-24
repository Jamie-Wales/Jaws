(define (and x y)
  (if x y #f))

(define (or x y)
  (if x #t y))

(define (not x)
  (if x #f #t))

(define (match value pattern)
  (cond
    [(symbol? pattern)
     (if (eq? pattern '_)
         '()
         (list (cons pattern value)))]
    [(null? pattern)
     (if (null? value) '() #f)]
    [(pair? pattern)
     (if (pair? value)
         (let ([m1 (match (car value) (car pattern))])
           (if m1
               (let ([m2 (match (cdr value) (cdr pattern))])
                 (if m2 (append m1 m2) #f))
               #f))
         #f)]
    [else (if (equal? pattern value) '() #f)]))

(define (try-match value patterns)
  (if (null? patterns)
      #f
      (let ([m (match value (caar patterns))])
        (if m
            (cons m (cadar patterns))
            (try-match value (cdr patterns))))))

(define (flatten x)
  (cond
    [(null? x) '()]
    [(pair? x) 
     (append (flatten (car x))
             (flatten (cdr x)))]
    [else (list x)]))

(define (transform bindings template)
    (cond
      [(symbol? template)
       (let ([binding (assq template bindings)])
         (if binding 
             (cdr binding)    
             template))]
      [(pair? template)
       (cons (transform bindings (car template))
             (transform bindings (cdr template)))]
      [else template]))

(define (expand-macro form rules)
  (let ([result (try-match form rules)])
    (if result
        (transform (car result) (cdr result))
        (error "No matching pattern"))))
(define test-value '(2 + 3))

(define test-pattern '(?x + ?y))
(define test-template '(+ ?x ?y))

; Let's try the match first
(display "Testing match: ")
(define match-result (match test-value test-pattern))
(display match-result)
(newline)

; Now let's transform using the bindings from the match
(display "Testing transform: ")
(define transform-result (transform match-result test-template))
(display transform-result)
(newline)


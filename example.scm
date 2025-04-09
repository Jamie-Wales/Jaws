(define-syntax select
  (syntax-rules (from where)
    ((select fields from table where condition)
     (map fields (filter condition table)))
    ((select fields from table)
     (map fields table))))

; Sample data
(define people
  '((name "Alice" age 30 city "New York")
    (name "Bob" age 25 city "Boston")
    (name "Charlie" age 35 city "New York")
    (name "Diana" age 28 city "Boston")))

; Use our DSL
(select (lambda (p) (list (cadr p) (cadddr p)))
        from people
        where (lambda (p) (> (cadddr p) 27)))

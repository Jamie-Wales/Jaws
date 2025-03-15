(import base)

(define (cadr x) (car (cdr x)))     ; Second element
(define (cddr x) (cdr (cdr x)))     ; Rest after the second element
(define (caddr x) (car (cddr x)))   ; Third element
(define (cdddr x) (cdr (cddr x)))   ; Rest after the third element
(define (cadddr x) (car (cdddr x))) ; Fourth element
(define (caar x) (car (car x)))
(define (cdar x) (cdr (car x)))
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddadr x) (cdr (cadr x)))
(define (cdadr x) (cdr (cadr x)))
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (cadaar x) (car (cdaar x)))
(define (caddar x) (car (cddar x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cdddar x) (cdr (cddar x)))
(define (caaaaa x) (car (caaaar x)))
(define (caaaad x) (car (caaadr x)))
(define (caaada x) (car (caadar x)))
(define (caadaa x) (car (cadaar x)))
(define (caadda x) (car (caddar x)))
(define (cadaaa x) (car (cdaaar x)))
(define (cadaad x) (car (cdaadr x)))
(define (cadada x) (car (cdadar x)))
(define (caddaa x) (car (cddaar x)))
(define (caddda x) (car (cdddar x)))
(define (cdaaaa x) (cdr (caaaar x)))
(define (cdaaad x) (cdr (caaadr x)))
(define (cdaada x) (cdr (caadar x)))
(define (cdadaa x) (cdr (cadaar x)))
(define (cdadda x) (cdr (caddar x)))
(define (cddaaa x) (cdr (cdaaar x)))
(define (cddaad x) (cdr (cdaadr x)))
(define (cddada x) (cdr (cdadar x)))
(define (cdddaa x) (cdr (cddaar x)))
(define (cdddda x) (cdr (cdddar x)))


(define (reverse lst)
  (let loop ([lst lst] [lst-reversed '()])
    (if (null? lst)
        lst-reversed
        (loop (cdr lst) (cons (car lst) lst-reversed)))))


(define (list-tail lst k)
  (if (zero? k)  
      lst
      (list-tail (cdr lst) (- k 1))))


(define (last lst)
  (let loop ([lst lst])
    (if (null? (cdr lst))
        (car lst)
        (loop (cdr lst)))))

(define (last-pair lst)
  (let loop ([lst lst])
    (if (null? (cdr lst))
        lst
        (loop (cdr lst)))))

(define (memq obj lst)
  (cond ((null? lst) #f)
        ((eq? obj (car lst)) lst)
        (else (memq obj (cdr lst)))))

(define (memv obj lst)
  (cond ((null? lst) #f)
        ((eqv? obj (car lst)) lst)
        (else (memv obj (cdr lst)))))

(define (assoc obj alist)
  (cond ((null? alist) #f)
        ((not (pair? (car alist)))
         (error "assoc: elements must be pairs"))
        ((equal? obj (caar alist)) (car alist))
        (else (assoc obj (cdr alist)))))

(define (assv obj alist)
  (cond ((null? alist) #f)
        ((not (pair? (car alist)))
         (error "assv: elements must be pairs"))
        ((eqv? obj (caar alist)) (car alist))
        (else (assv obj (cdr alist)))))

(define (member obj lst)
  (cond ((null? lst) #f)
        ((equal? obj (car lst)) lst)
        (else (member obj (cdr lst)))))

(define (assq obj alist)
  (cond ((null? alist) #f)
        ((not (pair? (car alist)))
         (error "assq: elements must be pairs"))
        ((eq? obj (caar alist)) (car alist))
        (else (assq obj (cdr alist)))))


(define (length lst)
  (let loop ((lst lst) (count 0))
    (if (null? lst)
        count
        (loop (cdr lst) (+ count 1)))))



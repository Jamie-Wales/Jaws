(define-library
  (list-utils)
  (export cadr
          cddr
          caddr
          cdddr
          cadddr
          caar
          cdar
          caaar
          caadr
          cadar
          cddar
          cdddar
          cddadr
          cdadr
          caaaar
          caaadr
          caadar
          cadaar
          caddar
          cdaaar
          cdaadr
          cdadar
          cddaar
          cdddar
          caaaaa
          caaaad
          caaada
          caadaa
          caadda
          cadaaa
          cadaad
          cadada
          caddaa
          caddda
          cdaaaa
          cdaaad
          cdaada
          cdadaa
          cdadda
          cddaaa
          cddaad
          cddada
          cdddaa
          cdddda
          reverse
          list-tail
          last
          last-pair
          length
          memq
          memv
          member
          assoc
          assv
          assq)
  (import (base))

  (begin
    ;; CAR/CDR combinations (up to 4 levels)
    (define (caaar x)
      (car (caar x)))

    (define (caadr x)
      (car (cadr x)))

    (define (caar x)
      (car (car x)))

    (define (cadar x)
      (car (cdar x)))

    (define (cadddr x)
      (car (cdddr x)))

    (define (caddr x)
      (car (cddr x)))

    (define (cadr x)
      (car (cdr x)))

    (define (cdar x)
      (cdr (car x)))

    (define (cddar x)
      (cdr (cdar x)))

    (define (cdddr x)
      (cdr (cddr x)))

    (define (cddr x)
      (cdr (cdr x)))

    (define (cddadr x)
      (cdr (cadr x)))

    (define (cdddar x)
      (cdr (cddar x)))

    (define (caaaar x)
      (car (caaar x)))

    (define (caaadr x)
      (car (caadr x)))

    (define (caadar x)
      (car (cadar x)))

    (define (cadaar x)
      (car (cddar x)))

    (define (cdadr x)
      (cdr (cadr x)))

    (define (caddar x)
      (car (cddar x)))

    (define (cdaaar x)
      (cdr (caaar x)))

    (define (cdaadr x)
      (cdr (caadr x)))

    (define (cdadar x)
      (cdr (cadar x)))

    (define (cddaar x)
      (cdr (cadaar x)))

    (define (caaaaa x)
      (car (caaaar x)))

    (define (caaaad x)
      (car (caaadr x)))

    (define (caaada x)
      (car (caadar x)))

    (define (caadaa x)
      (car (cadaar x)))

    (define (caadda x)
      (car (caddar x)))

    (define (cadaaa x)
      (car (cdaaar x)))

    (define (cadaad x)
      (car (cdaadr x)))

    (define (cadada x)
      (car (cdadar x)))

    (define (caddaa x)
      (car (cddaar x)))

    (define (caddda x)
      (car (cdddar x)))

    (define (cdaaaa x)
      (cdr (caaaar x)))

    (define (cdaaad x)
      (cdr (caaadr x)))

    (define (cdaada x)
      (cdr (caadar x)))

    (define (cdadaa x)
      (cdr (cadaar x)))

    (define (cdadda x)
      (cdr (caddar x)))

    (define (cddaaa x)
      (cdr (cdaaar x)))

    (define (cddaad x)
      (cdr (cdaadr x)))

    (define (cddada x)
      (cdr (cdadar x)))

    (define (cdddaa x)
      (cdr (cddaar x)))

    (define (cdddda x)
      (cdr (cdddar x)))

    (define (last lst)
      (if (null? lst)
          (error "last: empty list")
          (let loop ((current lst))
            (if (null? (cdr current))
                (car current)
                (loop (cdr current))))))

    (define (last-pair lst)
      (if (null? lst)
          (error "last-pair: empty list")
          (let loop ((current lst))
            (if (null? (cdr current))
                current
                (loop (cdr current))))))

    (define (length lst)
      (let loop ((lst lst)
                 (count 0))
        (if (null? lst)
            count
            (loop (cdr lst) (+ count 1)))))

    (define (list-tail lst k)
      (if (zero? k)
          lst
          (list-tail (cdr lst) (- k 1))))

    (define (memq obj lst)
      (cond
        ((null? lst)
         #f)
        ((eq? obj (car lst))
         lst)
        (else
         (memq obj (cdr lst)))))

    (define (reverse lst)
      (let loop ((lst lst)
                 (lst-reversed '()))
        (if (null? lst)
            lst-reversed
            (loop (cdr lst) (cons (car lst) lst-reversed)))))

    (define (assoc obj alist)
      (cond
        ((null? alist)
         #f)
        ((pair? (car alist))
         (if (equal? obj (caar alist))
             (car alist)
             (assoc obj (cdr alist))))
        (else
         (assoc obj (cdr alist)))))

    (define (member obj lst)
      (cond
        ((null? lst)
         #f)
        ((equal? obj (car lst))
         lst)
        (else
         (member obj (cdr lst)))))

    (define (memv obj lst)
      (cond
        ((null? lst)
         #f)
        ((eqv? obj (car lst))
         lst)
        (else
         (memv obj (cdr lst)))))

    (define (assq obj alist)
      (cond
        ((null? alist)
         #f)
        ((pair? (car alist))
         (if (eq? obj (caar alist))
             (car alist)
             (assq obj (cdr alist))))
        (else
         (assq obj (cdr alist)))))

    (define (assv obj alist)
      (cond
        ((null? alist)
         #f)
        ((pair? (car alist))
         (if (eqv? obj (caar alist))
             (car alist)
             (assv obj (cdr alist))))
        (else
         (assv obj (cdr alist)))))))

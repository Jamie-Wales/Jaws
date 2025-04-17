(define-library (math)

  (export + - * /                   ; Basic Arithmetic (Primitives)
          < <= = >= >                ; Comparisons (Primitives, = is numeric)
          quotient remainder modulo    ; Integer Division / Remainder (modulo defined below)
          abs magnitude max min gcd lcm ; Derived Arithmetic (defined below)
          square                     ; Simple Power (defined below)
          sqrt exp log sin cos atan    ; Transcendental / Trig (Primitives)
          real-part imag-part make-rectangular ; Complex Numbers (Primitives)
          exact->inexact inexact->exact  ; Exactness Conversion (Primitives)
          number? complex? real? rational? integer? ; Type Predicates (MUST BE IMPLEMENTED IN C++)
          exact? inexact?            ; Exactness Predicates (MUST BE IMPLEMENTED IN C++)
          zero? positive? negative? odd? even? ; Sign / Parity Predicates (defined below)
          random                     ; Random Numbers (Primitive)
          pi e                         ; Constants (defined below)
          )

  (import (base))

  (begin

    (define pi 3.14159265358979323846)
    (define e  2.71828182845904523536)

    (define (magnitude z)
      (if (not (number? z))
          (error "magnitude requires a number argument" z)
          (if (complex? z)
              (let ((r (real-part z))
                    (i (imag-part z)))
                (if (and (real? r) (real? i)) 
                    (sqrt (+ (* r r) (* i i)))
                    (error "magnitude requires real components for complex numbers" z))) 
              (if (and (real? z) (< z 0)) 
                  (- z) 
                  z))))

    (define abs magnitude)

    (define (max x . args)
       (if (not (number? x)) (error "max requires number arguments" x))
       (if (null? args)
           x
           (let ((m (apply max args))) 
              (if (not (number? m)) (error "max requires number arguments" m))
              (if (> x m) x m))))

     (define (min x . args)
        (if (not (number? x)) (error "min requires number arguments" x))
        (if (null? args)
            x
            (let ((m (apply min args)))
               (if (not (number? m)) (error "min requires number arguments" m))
               (if (< x m) x m))))

    (define (modulo n d)
        (if (or (not (number? n)) (not (number? d)))
            (error "modulo requires number arguments" n d)
            (if (zero? d)
                (error "modulo: division by zero" d)
                (let ((rem (remainder n d)))
                    (cond
                    ((zero? rem) 0)
                    ((and (negative? d) (positive? rem)) (+ rem d))
                    ((and (positive? d) (negative? rem)) (+ rem d))
                    (else rem))))))

    (define (gcd-two a b) 
      (if (or (not (integer? a)) (not (integer? b))) 
          (error "gcd requires integer arguments" a b)
          (let ((na (abs a)) (nb (abs b)))
            (if (zero? nb)
                na
                (gcd-two nb (remainder na nb))))))

    (define (gcd a . args) ; Variadic GCD
      (if (not (integer? a)) (error "gcd requires integer arguments" a)) ;Requires integer?
      (if (null? args)
          (abs a)
          (let loop ((current-gcd (abs a)) (remaining args))
            (if (null? remaining)
                current-gcd
                (let ((next-arg (car remaining)))
                   (if (not (integer? next-arg)) (error "gcd requires integer arguments" next-arg)) ; Requires integer?
                   (loop (gcd-two current-gcd next-arg) (cdr remaining)))))))

    (define (lcm-two a b) 
       (if (or (not (integer? a)) (not (integer? b))) 
           (error "lcm requires integer arguments" a b)
           (if (or (zero? a) (zero? b))
               0
               (let ((g (gcd-two a b)))
                 (if (zero? g) 0 (abs (/ (* a b) g)))))))

    (define (lcm a . args) 
       (if (not (integer? a)) (error "lcm requires integer arguments" a)) ; Requires integer?
       (if (null? args)
           (abs a)
           (let loop ((current-lcm (abs a)) (remaining args))
              (if (null? remaining)
                  current-lcm
                  (let ((next-arg (car remaining)))
                     (if (not (integer? next-arg)) (error "lcm requires integer arguments" next-arg)) ; Requires integer?
                     (loop (lcm-two current-lcm next-arg) (cdr remaining)))))))

    (define (square x)
       (if (not (number? x)) (error "square requires a number argument" x))
      (* x x))

    (define (zero? x)
      (and (number? x) (= x 0)))

    (define (positive? x)
      (and (real? x) (> x 0)))

    (define (negative? x)
      (and (real? x) (< x 0)))

    (define (odd? n)
      (and (integer? n)
           (not (zero? (remainder n 2)))))

    (define (even? n)
      (and (integer? n)
           (zero? (remainder n 2))))


  ) 
) 

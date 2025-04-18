(define-library
 (vector-utils)
 (export vector-map vector-for-each vector-append)
 (import (base))

 (begin
  (define (vector-append . vecs)
   (let loop ((vlist vecs)
              (total-len 0))
    (if (null? vlist)
     (let ((result-vec (make-vector total-len)))
      (let fill-loop ((vlist vecs)
                      (current-idx 0))
       (if (null? vlist)
        result-vec
        (let ((current-vec (car vlist)))
         (if (not (vector? current-vec))
          (error "vector-append: arguments must be vectors" current-vec)
          (let ((len (vector-length current-vec)))
           (let copy-loop ((k 0)
                           (idx current-idx))
            (if (< k len)
             (begin
              (vector-set! result-vec idx (vector-ref current-vec k))
              (copy-loop (+ k 1) (+ idx 1)))
             ;; Base case for copy-loop: Move to next vector
             (fill-loop (cdr vlist) idx)))))))))
     ;; Outer loop: process next vector to calculate total length
     (let ((current-vec (car vlist)))
      (if (not (vector? current-vec))
       (error "vector-append: arguments must be vectors" current-vec)
       (loop (cdr vlist) (+ total-len (vector-length current-vec))))))))

  (define (vector-for-each proc . vecs)
   (if (null? vecs)
    (error "vector-for-each: requires at least one vector argument" proc)
    #f)
   (if (not (procedure? proc))
    (error "vector-for-each: first argument must be a procedure" proc)
    #f)
   (let loop ((vlist vecs)
              (min-len #f))
    (if (null? vlist)
     (let ((len min-len))
      (if (and len
               (> len 0))
       (let index-loop ((k 0))
        ;; Start index loop
        (if (< k len)
         ;; THEN: Process index k and recurse
         (begin
          (let arg-loop ((vlist vecs)
                         (current-args '()))
           ;; Collect args for k
           (if (null? vlist)
            ;; Base case for arg-loop: Apply proc for side effect
            (apply proc (reverse current-args))
            ;; Recursive case for arg-loop: Collect next arg
            (let ((vec (car vlist)))
             (if (not (vector? vec))
              (error "vector-for-each: arguments must be vectors" vec)
              (arg-loop (cdr vlist) (cons (vector-ref vec k) current-args))))))
          (index-loop (+ k 1)))
         ;; Recurse for next index
         ;; ELSE: Base case for index-loop - return unspecified
         #f))))
     ;; Outer loop: process next vector to find min length
     (let ((current-vec (car vlist)))
      (if (not (vector? current-vec))
       (error "vector-for-each: arguments must be vectors" current-vec)
       (let ((current-len (vector-length current-vec)))
        (loop (cdr vlist)
              (if min-len
               (min min-len current-len)
               current-len))))))))

  (define (vector-map proc . vecs)
   (if (null? vecs)
    (error "vector-map: requires at least one vector argument" proc)
    #f)
   (if (not (procedure? proc))
    (error "vector-map: first argument must be a procedure" proc)
    #f)
   (let loop ((vlist vecs)
              (min-len #f))
    (if (null? vlist)
     (let ((len min-len))
      (if (or (not len)
              (zero? len))
       (vector)
       (let ((result-vec (make-vector len)))
        (let index-loop ((k 0))
         ;; Start index loop
         (if (< k len)
          ;; THEN: Process index k and recurse
          (begin
           (let arg-loop ((vlist vecs)
                          (current-args '()))
            ;; Collect args for k
            (if (null? vlist)
             ;; Base case for arg-loop: Apply proc and set result
             (vector-set! result-vec k (apply proc (reverse current-args)))
             ;; Recursive case for arg-loop: Collect next arg
             (let ((vec (car vlist)))
              (if (not (vector? vec))
               (error "vector-map: arguments must be vectors" vec)
               (arg-loop (cdr vlist) (cons (vector-ref vec k) current-args))))))
           (index-loop (+ k 1)))
          ;; Recurse for next index
          ;; ELSE: Base case for index-loop - return completed vector
          result-vec)))))
     ;; Outer loop: process next vector to find min length
     (let ((current-vec (car vlist)))
      (if (not (vector? current-vec))
       (error "vector-map: arguments must be vectors" current-vec)
       (let ((current-len (vector-length current-vec)))
        (loop (cdr vlist)
              (if min-len
               (min min-len current-len)
               current-len))))))))))
;; end begin
;; end define-library

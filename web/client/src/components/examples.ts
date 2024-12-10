export interface SchemeExample {
    name: string;
    code: string;
    description: string;
    difficulty: 'Beginner' | 'Intermediate' | 'Advanced';
}

// data/schemeExamples.ts
export const SCHEME_EXAMPLES: SchemeExample[] = [
    {
        name: "Hello World",
        code: '(display "Hello, World!")',
        description: "A simple greeting program demonstrating basic output",
        difficulty: "Beginner",
    },
    {
        name: "Factorial Function",
        code: `(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))`,
        description: "Recursive function to calculate factorials",
        difficulty: "Intermediate",
    },
    {
        name: "List Operations",
        code: `(define numbers '(1 2 3 4 5))
(map (lambda (x) (* x x)) numbers)`,
        description: "Working with lists and mapping functions",
        difficulty: "Intermediate",
    },
    {
        name: "Fibonacci Sequence",
        code: `(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))`,
        description: "Classic recursive Fibonacci implementation",
        difficulty: "Advanced",
    },
    {
        name: "Define your own Syntax",
        code: `(define-syntax begin
    (syntax-rules ()
        ((begin expr)
        (expr))
    ((begin expr expr2 ...)
        ((let ((dummy expr))
        (begin expr2 ...))))))

(begin (display "Hello, World!") (newline))`,
        description: "Scheme allows for defining of own syntax",
        difficulty: "Advanced"
    },
    {
        name: "Syntax with syntax within it",
        code: `(define-syntax begin
    (syntax-rules ()
        ((begin expr)
        (expr))
    ((begin expr expr2 ...)
        ((let ((dummy expr))
        (begin expr2 ...))))))

(begin (display "Hello, World!") (newline))

(define-syntax cond
    (syntax-rules(else)
        ((cond)(#f))
        ((cond(else expr ...))
            (begin expr ...))
        ((cond(test expr ... ) clause ...)
        (if test
            (begin expr ...)
            (cond clause ...)))))

(cond ((< 5 3) (display "Impossible!"))
      ((= 5 5) (display "Math works!"))
      (else (display "Default case")))`,
        description: "Create syntax which contains other syntax",
        difficulty: "Advanced"
    },
    {
        name: "Build your own higher order functions",
        code: `(define map1
    (lambda (p ls)
        (if (null? ls)
            ls
        (cons (p (car ls))
            (map1 p 
                (cdr ls))))))

(map1 (lambda (x) (* x x)) '(1 4 8 16))`,
        description: "If inbuilt map is not your thing build your own!",
        difficulty: "Advanced"
    },
    {
        name: "Let Syntax Implementation",
        code: `(define-syntax myLet
    (syntax-rules ()
        ((myLet ((var val) ...) body ...)
            ((lambda(var ...) body ...) val ...))))

(myLet ((x 10) (y 20)) ((display (+ x y))))`,
        description: "Implementing let using syntax rules",
        difficulty: "Advanced"
    },
];

import { useNavigate } from 'react-router-dom';
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { ArrowRight } from 'lucide-react';
import { HighlightedText } from '@/components/highlightedText';
import type { Example } from '@/types/types';

const examples: (Example & { difficulty: 'Beginner' | 'Intermediate' | 'Advanced' })[] = [

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
        code:
            `(define-syntax begin
    (syntax-rules ()
        ((begin expr)
        (expr))
    ((begin expr expr2 ...)
        ((let ((dummy expr))
        (begin expr2 ...))))))

(begin (display "Hello, World!") (newline))
        `,
        description: "Scheme allows for defining of own syntax",
        difficulty: "Advanced"
    },
    {
        name: "Syntax with syntax within it",
        code:
            `(define-syntax begin
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
        description: "Ensure you have loaded begin then you can create syntax which contains other syntax",
        difficulty: "Advanced"
    },

    {
        name: "Build your own higher order functions",
        code:
            `(define map1
  (lambda (p ls)
    (if (null? ls)
        ls
        (cons (p (car ls))
            (map1 p 
                (cdr ls)))
    )
  )
)

(map1 (lambda (x) (* x x)) '(1 4 8 16))`,
        description: "If inbuilt map is not your thing build your own!",
        difficulty: "Advanced"
    },

    {
        name: "Some of the language is actually just syntax-rules",
        code:
            `(define-syntax myLet
    (syntax-rules ()
        (
            (myLet ((var val) ...) body ...)
            (
                (lambda(var ...) body ...) val ...)
        )
    )
)

(myLet ((x 10) (y 20)) ((display (+ x y))))
            `,
        description: "It can be an down to an implementation of scheme to decide what to implement, however most of the language is syntax definition",
        difficulty: "Advanced"
    },
];

export function ExamplesView() {
    const navigate = useNavigate();

    const handleTryExample = (code: string) => {
        localStorage.setItem('editorCode', code);
        navigate('/Jaws/editor');
    };

    const getDifficultyColor = (difficulty: Example['difficulty']) => {
        switch (difficulty) {
            case 'Beginner':
                return 'difficulty-beginner';
            case 'Intermediate':
                return 'difficulty-intermediate';
            case 'Advanced':
                return 'difficulty-advanced';
            default:
                return '';
        }
    };

    return (
        <div className="min-h-screen bg-white text-gray-800">
            <div className="container mx-auto px-4 py-12">
                <div className="space-y-6">
                    <div className="flex flex-col gap-2">
                        <h1 className="text-2xl md:text-3xl font-bold text-gray-900">Example Programs</h1>
                        <p className="text-gray-600">
                            Learn Scheme through practical examples. Click "Try It" to load any example into the editor.
                        </p>
                    </div>

                    <div className="grid gap-6">
                        {examples.map((example, index) => (
                            <Card key={index} className="card-background">
                                <CardHeader className="card-header-border">
                                    <div className="flex items-start justify-between">
                                        <div>
                                            <CardTitle className="card-title">{example.name}</CardTitle>
                                            <CardDescription className="card-description">
                                                {example.description}
                                            </CardDescription>
                                        </div>
                                        <span className={`difficulty - badge ${getDifficultyColor(example.difficulty)}`}>
                                            {example.difficulty}
                                        </span>
                                    </div>
                                </CardHeader>
                                <CardContent className="space-y-4">
                                    <div className="code-block">
                                        <HighlightedText text={example.code} type="input" />
                                    </div>
                                    <Button onClick={() => handleTryExample(example.code)} className="primary-button">
                                        Try It <ArrowRight className="h-4 w-4" />
                                    </Button>
                                </CardContent>
                            </Card>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}

interface Chapter {
    id: string;
    title: string;
    description: string;
    sections: Section[];
}

interface CodeExample {
    description: string;
    code: string;
    explanation: string; // Added explanation field
}

interface Section {
    id: string;
    title: string;
    content: string | string[];
    codeExamples?: CodeExample[];
    difficulty: 'Beginner' | 'Intermediate' | 'Advanced';
}

export const CURRICULUM: Chapter[] = [
    {
        id: 'basics',
        title: 'Scheme Basics',
        description: 'Learn the fundamentals of Scheme programming',
        sections: [
            {
                id: 'intro',
                title: 'Introduction to Scheme',
                content: 'Scheme is a dialect of Lisp that supports multiple paradigms with a focus on functional programming. It is known for its simplicity, elegance, and powerful features such as first-class functions, lexical scoping, and proper tail recursion. Scheme was initially designed for education and research, making it an excellent language for learning programming concepts.',
                codeExamples: [
                    {
                        description: 'Your first Scheme program:',
                        code: '(display "Hello, World!")',
                        explanation: 'This example uses the display procedure to output the string "Hello, World!" to the console. It\'s the traditional first program in any language.'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'data-types',
                title: 'Data Types',
                content: [
                    'Scheme has several basic data types including numbers, strings, symbols, and lists. Numbers include integers, floating-point, rationals, and complex. Strings are sequences of characters.',
                    'Symbols are identifiers that are typically used for variables and function names. They are unique objects in memory - when two symbols have the same name, they reference the same object.',
                    'Lists are the fundamental data structure in Scheme, represented with parentheses. They can contain elements of any type, including other lists.'
                ],
                codeExamples: [
                    {
                        description: 'Working with numbers:',
                        code: '; Integer addition\n(+ 1 2)',
                        explanation: 'In Scheme, operators like + are functions that are placed before their arguments. This expression adds 1 and 2, resulting in 3.'
                    },
                    {
                        description: 'Multiplication:',
                        code: '; Multiplication\n(* 3 4)',
                        explanation: 'The * function multiplies its arguments together. This expression computes 3 × 4, resulting in 12.'
                    },
                    {
                        description: 'Subtraction:',
                        code: '; Subtraction\n(- 10 5)',
                        explanation: 'The - function subtracts the second number from the first. This computes 10 - 5, resulting in 5.'
                    },
                    {
                        description: 'Division:',
                        code: '; Division\n(/ 10 2)',
                        explanation: 'The / function divides the first number by the second. This calculates 10 ÷ 2, resulting in 5.'
                    },
                    {
                        description: 'String concatenation:',
                        code: '; Joining strings\n(string-append "Hello" " " "World")',
                        explanation: 'The string-append function joins multiple strings together. This combines "Hello", a space, and "World" to create "Hello World".'
                    },
                    {
                        description: 'String length:',
                        code: '; Finding string length\n(string-length "Hello")',
                        explanation: 'The string-length function counts the number of characters in a string. "Hello" has 5 characters.'
                    },
                    {
                        description: 'Creating a list:',
                        code: '; Creating a list\n(define my-list (list 1 2 3))\nmy-list',
                        explanation: 'This creates a list of three numbers using the list function and assigns it to the variable my-list. The second line returns the list (1 2 3).'
                    },
                    {
                        description: 'Getting the first element (car):',
                        code: '; First element of a list\n(define my-list (list 1 2 3))\n(car my-list)',
                        explanation: 'The car function returns the first element of a list. For the list (1 2 3), the car is 1.'
                    },
                    {
                        description: 'Getting all but the first element (cdr):',
                        code: '; All but first element\n(define my-list (list 1 2 3))\n(cdr my-list)',
                        explanation: 'The cdr function returns the rest of a list after the first element. For the list (1 2 3), the cdr is (2 3).'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'quoting',
                title: 'Quoting and Evaluation',
                content: [
                    'In Scheme, expressions are normally evaluated to produce values. However, sometimes you want to treat code as data rather than evaluating it.',
                    'Quoting is a mechanism that prevents evaluation, allowing you to use symbols and lists as literal data. The quote special form (or its shorthand, the single quote \') treats its argument as literal data instead of evaluating it.',
                    'This concept is fundamental to metaprogramming in Scheme, especially for macros which transform code before it\'s evaluated.'
                ],
                codeExamples: [
                    {
                        description: 'Expression without quoting (evaluates):',
                        code: '; Evaluates the expression\n(+ 1 2)',
                        explanation: 'Without quoting, this expression is evaluated normally, calling the + function with arguments 1 and 2, resulting in 3.'
                    },
                    {
                        description: 'Same expression with quoting (doesn\'t evaluate):',
                        code: '; Returns the expression as data\n\'(+ 1 2)',
                        explanation: 'With quoting, the expression is treated as data instead of being evaluated. This returns the list (+ 1 2) rather than computing the sum.'
                    },
                    {
                        description: 'Explicit quote form:',
                        code: '; Using the quote special form\n(quote (+ 1 2))',
                        explanation: 'This is the full form of quoting. It\'s equivalent to \'(+ 1 2) and returns the list (+ 1 2) without evaluation.'
                    },
                    {
                        description: 'Quoting a symbol:',
                        code: '; Symbol literal\n\'abc',
                        explanation: 'Quoting a symbol name creates a symbol. Without the quote, abc would be treated as a variable name.'
                    },
                    {
                        description: 'Quoting a list:',
                        code: '; List literal\n\'(a b c)',
                        explanation: 'This creates a list containing three symbols: a, b, and c. Without quoting, Scheme would try to call function a with arguments b and c.'
                    },
                    {
                        description: 'Quoting a list of numbers:',
                        code: '; List of numbers\n\'(1 2 3)',
                        explanation: 'This creates a list containing the numbers 1, 2, and 3 as data. Numbers are self-evaluating, but quoting still prevents the list from being treated as a function call.'
                    },
                    {
                        description: 'Setting up for quasiquoting:',
                        code: '; Define variables for the next examples\n(define x 5)\n(define y 10)\nx',
                        explanation: 'This defines two variables, x with value 5 and y with value 10, for use in the next examples. The last line returns the value of x.'
                    },
                    {
                        description: 'Quasiquoting with unquoting:',
                        code: '; Quasiquote (backtick) with unquote (comma)\n`(x is ,x and y is ,y)',
                        explanation: 'Quasiquote (backtick) works like quote, but allows selective evaluation using unquote (comma). Here, the symbols x and y remain as symbols, but their values (5 and 10) are inserted at the comma positions.'
                    },
                    {
                        description: 'Setting up for unquote-splicing:',
                        code: '; Define a list for splicing\n(define nums \'(1 2 3))\nnums',
                        explanation: 'This defines a variable nums containing the list (1 2 3) for use in the next example.'
                    },
                    {
                        description: 'Unquote-splicing example:',
                        code: '; Using unquote-splicing (,@) for lists\n`(The numbers are: ,@nums)',
                        explanation: 'Unquote-splicing (,@) inserts each element of a list into the surrounding list. This results in (The numbers are: 1 2 3) rather than (The numbers are: (1 2 3)).'
                    }
                ],
                difficulty: 'Beginner'
            }
        ]
    },
    {
        id: 'functions',
        title: 'Functions & Procedures',
        description: 'Master function definition and usage in Scheme',
        sections: [
            {
                id: 'defining-functions',
                title: 'Defining Functions',
                content: 'Functions in Scheme are defined using the define keyword. Scheme functions (also called procedures) can take any number of parameters and return a value. In Scheme, functions are first-class objects, meaning they can be passed as arguments, returned from other functions, and assigned to variables.',
                codeExamples: [
                    {
                        description: 'Defining a simple function:',
                        code: '; Define a function to square a number\n(define (square x)\n  (* x x))',
                        explanation: 'This defines a function named square that takes one parameter x and returns the result of multiplying x by itself. The (define (square x) ...) syntax is shorthand for (define square (lambda (x) ...)).'
                    },
                    {
                        description: 'Using the function:',
                        code: '; Call the square function\n(square 5)',
                        explanation: 'This calls the square function with the argument 5. The function returns 5 × 5, which is 25.'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'lambda',
                title: 'Lambda Expressions',
                content: 'Lambda expressions allow you to create anonymous functions in Scheme. These are useful for creating functions that are used only once or as arguments to higher-order functions. The syntax is (lambda (parameters) body).',
                codeExamples: [
                    {
                        description: 'Creating an anonymous function:',
                        code: '; Anonymous function expression\n(lambda (x) (* x x))',
                        explanation: 'This creates an anonymous function that squares its argument. The function isn\'t called or bound to a name; it\'s just a value.'
                    },
                    {
                        description: 'Using a lambda expression directly:',
                        code: '; Applying a lambda directly\n((lambda (x) (* x x)) 5)',
                        explanation: 'This creates an anonymous squaring function and immediately applies it to the argument 5. The extra parentheses cause the lambda to be evaluated and then called with 5.'
                    },
                    {
                        description: 'Assigning a lambda to a variable:',
                        code: '; Defining a function with lambda\n(define square (lambda (x) (* x x)))',
                        explanation: 'This creates a lambda expression and binds it to the name square. This is equivalent to the square function definition we saw earlier, but written with the explicit lambda form.'
                    },
                    {
                        description: 'Using the function:',
                        code: '; Using the lambda-defined function\n(square 5)',
                        explanation: 'This calls the square function created with lambda, passing the argument 5. The function returns 25, just like the previous square function.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'higher-order',
                title: 'Higher-Order Functions',
                content: [
                    'Higher-order functions are functions that operate on other functions, either by taking functions as arguments or returning functions as results.',
                    'This concept is central to functional programming and enables powerful abstractions. Common higher-order functions in Scheme include map, filter, and fold, which apply operations to elements in lists.'
                ],
                codeExamples: [
                    {
                        description: 'Defining a list for our examples:',
                        code: '; Create a list of numbers\n(define numbers \'(1 2 3 4 5))\nnumbers',
                        explanation: 'This creates a list of numbers from 1 to 5 and assigns it to the variable numbers. We\'ll use this list in the following examples.'
                    },
                    {
                        description: 'Using map:',
                        code: '; Map doubles each element\n(map (lambda (x) (* x 2)) numbers)',
                        explanation: 'The map function applies a given function to each element in a list. Here, we\'re applying an anonymous function that doubles its input to each number in our list, resulting in (2 4 6 8 10).'
                    },
                    {
                        description: 'Defining a predicate function:',
                        code: '; Define a predicate for even numbers\n(define (even? x) (= (remainder x 2) 0))',
                        explanation: 'This defines a predicate function named even? that returns #t if its argument is even and #f otherwise. It works by checking if the remainder when dividing by 2 is zero.'
                    },
                    {
                        description: 'Using filter:',
                        code: '; Filter keeps only even numbers\n(filter even? \'(1 2 3 4 5 6))',
                        explanation: 'The filter function creates a new list containing only the elements that satisfy a predicate. Here, we\'re keeping only the even numbers from the list, resulting in (2 4 6).'
                    },
                    {
                        description: 'Using fold to sum elements:',
                        code: '; Fold/reduce to sum all elements\n(fold-left + 0 \'(1 2 3 4 5))',
                        explanation: 'The fold-left function combines list elements using a binary function. Starting with an initial value (0), it applies the function (+) between the accumulator and each element. This computes 0+1+2+3+4+5=15.'
                    },
                    {
                        description: 'Using fold to reverse a list:',
                        code: '; Using fold to reverse a list\n(fold-left (lambda (x acc) (cons x acc)) \'() \'(1 2 3 4))',
                        explanation: 'This uses fold-left to build a reversed list. The lambda takes the current element `x` and the accumulated list `acc`. Starting with an empty list `\'()`, for each element `x` from the input list, we `cons` `x` onto the front of our accumulated list `acc`. This effectively reverses the list, resulting in `(4 3 2 1)`.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'variable-bindings',
                title: 'Variable Binding Forms',
                content: [
                    'Scheme provides several special forms for creating local variables with different scoping rules. The basic form is let, which binds variables in parallel. Two variations are let* (sequential binding) and letrec (recursive binding).',
                    'Understanding these binding forms is essential for managing variable scope and creating complex algorithms in Scheme. Each has specific use cases where it is most appropriate.'
                ],
                codeExamples: [
                    {
                        description: 'Basic let binding:',
                        code: '; Simple parallel binding\n(let ((x 5)\n      (y 10))\n  (+ x y))',
                        explanation: 'The let form creates local variables with specific values. Here, x is bound to 5 and y to 10, and then we compute x + y, which is 15.'
                    },
                    {
                        description: 'Limitation of let (parallel evaluation):',
                        code: '; This will cause an error\n; (let ((x 5)\n;       (y (* x 2)))\n;   (+ x y))\n\n; Instead, we can do:\n(define x 5)\n(let ((y (* x 2)))\n  (+ x y))',
                        explanation: 'In let, all bindings are evaluated before any of them are available. We can\'t reference earlier bindings within the binding list. The commented code would fail because x isn\'t defined when computing y. The alternative defines x globally first.'
                    },
                    {
                        description: 'Using let* for sequential binding:',
                        code: '; Sequential binding with let*\n(let* ((x 5)\n       (y (* x 2))\n       (z (+ x y)))\n  z)',
                        explanation: 'The let* form evaluates bindings in sequence, making earlier bindings available to later ones. Here, x is bound to 5, then y to 10 (5×2), and z to 15 (5+10). The result is 15.'
                    },
                    {
                        description: 'Equivalent nested lets:',
                        code: '; Equivalent to the previous let*\n(let ((x 5))\n  (let ((y (* x 2)))\n    (let ((z (+ x y)))\n      z)))',
                        explanation: 'This shows how let* works internally. It\'s equivalent to nested let forms, where each new binding is made inside the scope of the previous ones.'
                    },
                    {
                        description: 'Using letrec for recursive definitions:',
                        code: '; Creating mutually recursive functions\n(letrec ((even? (lambda (n)\n                  (if (zero? n)\n                      #t\n                      (odd? (- n 1)))))\n         (odd? (lambda (n)\n                 (if (zero? n)\n                     #f\n                     (even? (- n 1))))))\n  (even? 4))',
                        explanation: 'The letrec form allows bindings to reference each other, which is essential for mutually recursive functions. Here, even? calls odd? and vice versa. This determines that 4 is even by recursively checking if 0 is even (which it is by definition).'
                    }
                ],
                difficulty: 'Intermediate'
            }
        ]
    },
    {
        id: 'advanced',
        title: 'Advanced Concepts',
        description: 'Explore powerful features of the Scheme language',
        sections: [
            {
                id: 'macros',
                title: 'Macros and Syntax Rules',
                content: [
                    'Macros are one of Scheme\'s most powerful features, allowing you to extend the language by creating new syntactic forms. Unlike functions, macros operate on code before it\'s evaluated, enabling transformations that would be impossible with functions alone.',
                    'The define-syntax and syntax-rules forms provide a pattern-based system for defining macros. Patterns specify what the macro matches, and templates specify how to transform the code.'
                ],
                codeExamples: [
                    {
                        description: 'Defining a simple when macro:',
                        code: '; Define the when macro\n(define-syntax when\n  (syntax-rules ()\n    ((when condition body ...)\n     (if condition\n         (begin body ...)))))',
                        explanation: 'This defines a when macro that executes a series of expressions only if a condition is true. The syntax-rules specify that when takes a condition followed by any number of body expressions, and transforms this into an if with a begin for the body.'
                    },
                    {
                        description: 'Using the when macro:',
                        code: '; Using our when macro\n(when (> 5 3)\n  (display "5 is greater than 3")\n  (newline))',
                        explanation: 'This uses our when macro with the condition (> 5 3) and two body expressions. Since 5 is greater than 3, both expressions will execute, displaying the text and a newline.'
                    },
                    {
                        description: 'Defining a cond macro:',
                        code: '; Define a simplified cond macro\n(define-syntax cond\n  (syntax-rules (else)\n    ((cond) #f)  ; No clauses\n    ((cond (else result1 result2 ...))\n     (begin result1 result2 ...))\n    ((cond (test result1 result2 ...) clause ...)\n     (if test\n         (begin result1 result2 ...)\n         (cond clause ...)))))',
                        explanation: 'This defines a cond macro for multi-way conditionals. It handles three cases: empty cond (returns #f), an else clause (executes its body), and a test clause (converts to an if-then-else where the "else" recursively processes remaining clauses).'
                    },
                    {
                        description: 'Using the cond macro:',
                        code: '; Using our cond macro\n(define x 5)\n(cond\n  ((< x 0) "negative")\n  ((> x 0) "positive")\n  (else "zero"))',
                        explanation: 'This uses our cond macro to check if x is negative, positive, or zero. Since x is 5, the second clause matches, so it returns "positive".'
                    },
                    {
                        description: 'Implementing let* with macros:',
                        code: '; Implementing let* as a macro\n(define-syntax let*\n  (syntax-rules ()\n    ((let* () body ...)\n     (begin body ...))\n    ((let* ((name value)) body ...)\n     (let ((name value))\n       body ...))\n    ((let* ((name1 value1) (name2 value2) rest ...) body ...)\n     (let ((name1 value1))\n       (let* ((name2 value2) rest ...)\n         body ...)))))',
                        explanation: 'This implements let* as a macro that transforms nested bindings into nested let expressions. It handles three cases: empty bindings (just execute the body), one binding (use a simple let), and multiple bindings (use a let for the first binding and recursively process the rest).'
                    },
                    {
                        description: 'Using our let* implementation:',
                        code: '; Try our let* implementation\n(let* ((a 5)\n       (b (+ a 2))\n       (c (* a b)))\n  (+ a b c))',
                        explanation: 'This uses our let* implementation to create three variables: a=5, b=7 (5+2), and c=35 (5×7). Then it computes a+b+c, which is 5+7+35=47.'
                    },
                    {
                        description: 'Implementing letrec with macros:',
                        code: '; Implementing letrec as a macro\n(define-syntax letrec\n  (syntax-rules ()\n    ((letrec ((var val) ...) body ...)\n     ((lambda ()\n        ; First create uninitialized variables\n        (define var #f) ...\n        ; Then assign them their values\n        (set! var val) ...\n        ; Finally, execute the body\n        body ...)))))',
                        explanation: 'This implements letrec as a macro using a lambda and define/set! combination. It first creates all variables with dummy values (#f), then sets their actual values, allowing recursive references.'
                    },
                    {
                        description: 'Using our letrec implementation:',
                        code: '; Using our letrec implementation\n(letrec ((is-even? (lambda (n)\n                    (if (zero? n)\n                        #t\n                        (is-odd? (- n 1)))))\n         (is-odd? (lambda (n)\n                   (if (zero? n)\n                       #f\n                       (is-even? (- n 1))))))\n  (is-even? 6))',
                        explanation: 'This uses our letrec implementation to define mutually recursive functions is-even? and is-odd?. Then it calls is-even? with 6, which recursively checks if 5 is odd, 4 is even, etc., eventually returning #t.'
                    }
                ],
                difficulty: 'Advanced'
            },
            {
                id: 'recursion',
                title: 'Recursion Patterns',
                content: 'Scheme excels at recursive solutions due to its proper tail call optimization. This section covers common recursion patterns and techniques for efficient recursive algorithms.',
                codeExamples: [
                    {
                        description: 'Defining a recursive factorial function:',
                        code: '; Recursive factorial function\n(define factorial\n  (lambda (n)\n    (if (<= n 1)\n        1\n        (* n (factorial (- n 1))))))',
                        explanation: 'This defines a recursive function to compute the factorial of a number. If n is 0 or 1, it returns 1; otherwise, it multiplies n by the factorial of (n-1).'
                    },
                    {
                        description: 'Computing factorial of 5:',
                        code: '; Calculate 5!\n(factorial 5)',
                        explanation: 'This calls our factorial function with the argument 5. It computes 5×4×3×2×1=120.'
                    },
                    {
                        description: 'Recursive list length:',
                        code: '; Recursive function to find list length\n(define (my-length lst)\n  (if (null? lst)\n      0\n      (+ 1 (my-length (cdr lst)))))',
                        explanation: 'This defines a recursive function to compute the length of a list. If the list is empty, the length is 0; otherwise, it\'s 1 plus the length of the list\'s cdr (rest).'
                    },
                    {
                        description: 'Using the list length function:',
                        code: '; Find length of a list\n(my-length \'(a b c d))',
                        explanation: 'This calls our my-length function on a list with 4 elements. The function returns 4.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'tail-recursion',
                title: 'Tail Recursion Optimization',
                content: [
                    'Tail recursion is a special case of recursion where the recursive call is the last operation in a function. Scheme implementations are required to optimize tail-recursive calls, turning them into iterations that don\'t grow the call stack.',
                    'This optimization allows writing elegant recursive algorithms that are also efficient, even for large inputs or deep recursion levels. Understanding and applying tail recursion is crucial for writing efficient recursive code in Scheme.'
                ],
                codeExamples: [
                    {
                        description: 'Standard recursive factorial (not tail-recursive):',
                        code: '; Standard recursive factorial\n(define (factorial n)\n  (if (<= n 1)\n      1\n      (* n (factorial (- n 1)))))',
                        explanation: 'This is a standard recursive factorial function. The recursive call is not in tail position because after (factorial (- n 1)) returns, there\'s still a multiplication to perform. This means the call stack grows with n.'
                    },
                    {
                        description: 'Tail-recursive factorial implementation:',
                        code: '; Tail-recursive factorial\n(define (factorial-tail n)\n  (define (fact-iter n acc)\n    (if (<= n 1)\n        acc\n        (fact-iter (- n 1) (* n acc))))\n  (fact-iter n 1))',
                        explanation: 'This is a tail-recursive factorial. It uses a helper function that carries an accumulator. Since the recursive call is the last operation, Scheme optimizes it to use constant stack space regardless of n.'
                    },
                    {
                        description: 'Using the tail-recursive factorial:',
                        code: '; Calculate 5! using tail recursion\n(factorial-tail 5)',
                        explanation: 'This calls our tail-recursive factorial function with argument 5. It computes 5! = 120, just like the standard version, but more efficiently for large inputs.'
                    },
                    {
                        description: 'Standard recursive sum (not tail-recursive):',
                        code: '; Recursive sum (not tail-recursive)\n(define (sum-list lst)\n  (if (null? lst)\n      0\n      (+ (car lst) (sum-list (cdr lst)))))',
                        explanation: 'This is a standard recursive function to sum a list. Like the standard factorial, it\'s not tail-recursive because there\'s an addition to perform after the recursive call returns.'
                    },
                    {
                        description: 'Testing the recursive sum:',
                        code: '; Test the recursive sum\n(sum-list \'(1 2 3 4 5))',
                        explanation: 'This calls our sum-list function on a list of numbers. It computes 1+2+3+4+5=15.'
                    },
                    {
                        description: 'Tail-recursive sum implementation:',
                        code: '; Tail-recursive sum\n(define (sum-list-tail lst)\n  (define (sum-iter lst acc)\n    (if (null? lst)\n        acc\n        (sum-iter (cdr lst) (+ (car lst) acc))))\n  (sum-iter lst 0))',
                        explanation: 'This is a tail-recursive sum function. Like the tail-recursive factorial, it uses an accumulator to carry the partial sum, making the recursive call the last operation.'
                    },
                    {
                        description: 'Testing the tail-recursive sum:',
                        code: '; Test the tail-recursive sum\n(sum-list-tail \'(1 2 3 4 5))',
                        explanation: 'This calls our tail-recursive sum function. It computes the same result (15) as the standard version, but can handle very large lists without stack overflow.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'continuations',
                title: 'Continuations',
                content: [
                    'Continuations represent "the rest of the computation" at a given point in a program. Scheme provides first-class continuations through the call-with-current-continuation (call/cc) procedure.',
                    'This powerful feature enables advanced control flow patterns like non-local exits, exception handling, coroutines, and even backtracking algorithms. Continuations are one of the most powerful and unique features of Scheme.'
                ],
                codeExamples: [
                    {
                        description: 'Early return with call/cc:',
                        code: '; Find first even number in a list\n(define (find-first pred lst)\n  (call/cc\n   (lambda (return)\n     (for-each\n      (lambda (x)\n        (if (pred x)\n            (return x)))\n      lst)\n     #f)))',
                        explanation: 'This function uses call/cc to create an early-return mechanism. The continuation "return" captures the current execution state. When an element satisfying the predicate is found, the continuation is called with that element, immediately jumping out of the for-each loop and returning the value. If no element matches, #f is returned as a default value.'
                    },
                    {
                        description: 'Using the find-first function:',
                        code: '; Find first even number\n(find-first even? \'(1 3 5 6 7 8 9))',
                        explanation: 'This calls our find-first function with the even? predicate and a list of numbers. It searches through the list (1 3 5 6 7 8 9) and returns 6, the first even number it encounters. The early-return mechanism avoids unnecessary iteration through the rest of the list.'
                    },
                    {
                        description: 'Implementing division with error handling:',
                        code: '; Safe division with continuations\n(define (divide a b)\n  (call/cc\n   (lambda (k)\n     (if (= b 0)\n         (k \'(error "Division by zero"))\n         (k `(ok ,(/ a b)))))))',
                        explanation: 'This function implements safe division using continuations for error handling. The continuation k is used to return early with an error message if b is zero. Otherwise, it returns a list containing the symbol "ok" and the result of the division. This pattern allows for structured error returns without using exceptions.'
                    },
                    {
                        description: 'Creating a function to handle the result:',
                        code: '; Function to process the result\n(define (handle-result result)\n  (if (eq? (car result) \'error)\n      (cadr result)\n      (cadr result)))',
                        explanation: 'This function processes the result returned by the divide function. It checks if the first element of the result list is the symbol "error". In either case, it returns the second element of the list, which is either the error message or the division result. Note that this implementation would typically have different behavior for error vs. success cases.'
                    },
                    {
                        description: 'Testing successful division:',
                        code: '; Successful division\n(handle-result (divide 10 2))',
                        explanation: 'This tests the divide function with valid inputs 10 and 2. The divide function returns (ok 5), and handle-result extracts and returns 5.'
                    },
                    {
                        description: 'Testing division by zero:',
                        code: '; Division by zero\n(handle-result (divide 10 0))',
                        explanation: 'This tests the divide function with inputs that would cause division by zero. The divide function returns (error "Division by zero"), and handle-result extracts and returns the error message "Division by zero" instead of causing a runtime error.'
                    }
                ],
                difficulty: 'Advanced'
            },
            {
                id: 'vectors',
                title: 'Vectors',
                content: [
                    'Vectors are another important data structure in Scheme. Unlike lists, vectors allow constant-time access to any element by index. They are useful when random access is needed or when dealing with large amounts of data.',
                    'Vectors are written with the #(...) syntax and provide operations for creation, access, and modification.'
                ],
                codeExamples: [
                    {
                        description: 'Creating a vector:',
                        code: '; Create a vector\n(define v #(1 2 3 4 5))\nv',
                        explanation: 'This creates a vector with five elements and assigns it to the variable v. Vectors in Scheme use the #(...) literal syntax, which is different from the list syntax. When v is evaluated on the third line, it displays the vector as #(1 2 3 4 5).'
                    },
                    {
                        description: 'Finding vector length:',
                        code: '; Get the number of elements\n(vector-length v)',
                        explanation: 'The vector-length function returns the number of elements in a vector. For our vector v with elements (1 2 3 4 5), this returns 5. Unlike lists where finding the length requires traversing the entire list, vector length is stored directly and can be accessed in constant time.'
                    },
                    {
                        description: 'Accessing a vector element:',
                        code: '; Access element at index 2\n(vector-ref v 2)',
                        explanation: 'The vector-ref function retrieves an element from a vector at a specified index. Indices in Scheme start at 0, so vector-ref v 2 returns the third element of v, which is 3. This is a constant-time operation, unlike list access which may require traversing the list.'
                    },
                    {
                        description: 'Creating a vector programmatically:',
                        code: '; Create a vector with initial value 0\n(define v2 (make-vector 5 0))\nv2',
                        explanation: 'The make-vector function creates a new vector of a specified length with all elements initialized to a given value. This creates a vector of length 5 with each element set to 0, so v2 is #(0 0 0 0 0). This is useful when you need a vector of a specific size but don\'t know the values yet.'
                    },
                    {
                        description: 'Modifying a vector element:',
                        code: '; Change element at index 2 to 42\n(vector-set! v2 2 42)\nv2',
                        explanation: 'The vector-set! function changes an element in a vector at a specified index. The ! in the name indicates that this function modifies its argument. After setting the element at index 2 to 42, v2 becomes #(0 0 42 0 0). Unlike lists, vectors can be efficiently modified in-place without creating a new copy.'
                    },
                    {
                        description: 'Converting list to vector:',
                        code: '; Convert a list to a vector\n(list->vector \'(a b c d))',
                        explanation: 'The list->vector function creates a new vector containing the elements of a list. This converts the list (a b c d) to a vector #(a b c d). This conversion is useful when you need faster random access to elements or when using functions that require vectors as input.'
                    },
                    {
                        description: 'Converting vector to list:',
                        code: '; Convert a vector to a list\n(vector->list #(a b c d))',
                        explanation: 'The vector->list function creates a new list containing the elements of a vector. This converts the vector #(a b c d) to a list (a b c d). This conversion is useful when you need to use list-specific functions or when working with code that expects lists as input.'
                    }
                ],
                difficulty: 'Intermediate'
            }
        ]
    },
    {
        id: 'io-operations',
        title: 'Input/Output Operations',
        description: 'Learn how to handle input and output in Scheme programs',
        sections: [
            {
                id: 'basic-io',
                title: 'Basic Input and Output',
                content: [
                    'Scheme provides a set of procedures for basic input and output operations. These procedures allow you to read from the keyboard, write to the screen, and interact with files.',
                    'The most common I/O procedures include display, write, newline for output and read for input. Understanding these basic operations is essential for creating interactive Scheme programs.'
                ],
                codeExamples: [
                    {
                        description: 'Writing to the console:',
                        code: '; Display a string\n(display "Hello, world!")\n(newline)',
                        explanation: 'The display procedure outputs its argument to the console. When displaying strings, the quotes aren\'t shown in the output. The newline procedure starts a new line, similar to pressing Enter.'
                    },
                    {
                        description: 'Writing with write:',
                        code: '; Using write instead of display\n(write "Hello, world!")\n(newline)',
                        explanation: 'The write procedure is similar to display, but it outputs values in a form that can be read back by the Scheme reader. For strings, this means including the quotation marks in the output.'
                    },
                    {
                        description: 'Reading from the console:',
                        code: '; Read a Scheme expression\n(define user-input (read))\n; After entering (+ 2 3), user-input contains 5',
                        explanation: 'The read procedure reads a complete Scheme expression from the input and returns it. If the user enters (+ 2 3), Scheme evaluates this to 5 before assigning it to user-input.'
                    },
                    {
                        description: 'Reading a line of text:',
                        code: '; Read a full line of text\n(define line (read-line))',
                        explanation: 'The read-line procedure reads characters until it reaches a newline. Unlike read, it doesn\'t evaluate what the user enters, but returns it as a string.'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'file-io',
                title: 'File Input and Output',
                content: [
                    'Scheme allows you to read from and write to files using ports. A port is an object that represents a connection to a file or another device.',
                    'You can open a file for reading or writing, perform operations on the port, and then close it when you\'re done. Proper file handling is important for data persistence and processing larger datasets.'
                ],
                codeExamples: [
                    {
                        description: 'Opening a file for output:',
                        code: '; Open a file for writing\n(define out-port (open-output-file "example.txt"))',
                        explanation: 'The open-output-file procedure opens a file for writing and returns an output port. If the file already exists, it will typically be overwritten (though the exact behavior may depend on the Scheme implementation).'
                    },
                    {
                        description: 'Writing to a file:',
                        code: '; Write data to the file\n(display "Hello, file!" out-port)\n(newline out-port)',
                        explanation: 'The display procedure can take an optional second argument specifying the output port. Here, we\'re writing to our file port instead of the default console. The newline works similarly.'
                    },
                    {
                        description: 'Closing an output file:',
                        code: '; Close the output port\n(close-output-port out-port)',
                        explanation: 'After writing to a file, it\'s important to close the port with close-output-port. This ensures all data is properly flushed to the file and system resources are released.'
                    },
                    {
                        description: 'Opening a file for input:',
                        code: '; Open a file for reading\n(define in-port (open-input-file "example.txt"))',
                        explanation: 'The open-input-file procedure opens a file for reading and returns an input port. The file must exist, or an error will be raised.'
                    },
                    {
                        description: 'Reading from a file:',
                        code: '; Read a line from the file\n(define file-line (read-line in-port))',
                        explanation: 'Read operations like read-line can also take an optional port argument. Here, we\'re reading a line of text from the file instead of from the console.'
                    },
                    {
                        description: 'Closing an input file:',
                        code: '; Close the input port\n(close-input-port in-port)',
                        explanation: 'After reading from a file, it\'s important to close the port with close-input-port to release system resources.'
                    },
                    {
                        description: 'Reading an entire file:',
                        code: '; Function to read entire file contents\n(define (read-file filename)\n  (let ((p (open-input-file filename)))\n    (let ((content (read-all p)))\n      (close-input-port p)\n      content)))\n\n; Helper function to read all expressions\n(define (read-all port)\n  (let ((data (read port)))\n    (if (eof-object? data)\n        \'()\n        (cons data (read-all port)))))',
                        explanation: 'This example defines a function to read the entire contents of a file. It opens the file, reads all expressions until reaching the end-of-file (EOF), closes the port, and returns the content as a list.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'string-manipulation',
                title: 'String Manipulation',
                content: [
                    'String manipulation is essential for many programming tasks, especially when dealing with text processing or user input. Scheme provides a set of procedures for creating, combining, and extracting parts of strings.',
                    'These operations allow you to build complex string processing functions for tasks like parsing, formatting, or generating text output.'
                ],
                codeExamples: [
                    {
                        description: 'Creating a string:',
                        code: '; Creating strings\n(define greeting "Hello")\n(define name "World")',
                        explanation: 'Strings in Scheme are enclosed in double quotes. Here, we define two string variables for use in our examples.'
                    },
                    {
                        description: 'Getting string length:',
                        code: '; Find the length of a string\n(string-length greeting)',
                        explanation: 'The string-length procedure returns the number of characters in a string. In this case, "Hello" has 5 characters.'
                    },
                    {
                        description: 'Concatenating strings:',
                        code: '; Combining strings\n(string-append greeting ", " name "!")',
                        explanation: 'The string-append procedure joins multiple strings together. This example combines "Hello", ", ", "World", and "!" to create "Hello, World!".'
                    },
                    {
                        description: 'Accessing characters in a string:',
                        code: '; Get a character from a string\n(string-ref greeting 1)',
                        explanation: 'The string-ref procedure accesses a specific character in a string by index (starting from 0). string-ref greeting 1 returns the character at index 1 in "Hello", which is #\\e.'
                    },
                    {
                        description: 'Creating a string from characters:',
                        code: '; Create a string from individual characters\n(string #\\S #\\c #\\h #\\e #\\m #\\e)',
                        explanation: 'The string procedure creates a new string from individual characters. Characters in Scheme are written with the #\\ prefix. This example creates the string "Scheme".'
                    },
                    {
                        description: 'Extracting a substring:',
                        code: '; Get part of a string\n(substring "Scheme Programming" 0 6)',
                        explanation: 'The substring procedure extracts a portion of a string. It takes three arguments: the string, the start index (inclusive), and the end index (exclusive). This example returns "Scheme".'
                    },
                    {
                        description: 'Converting case:',
                        code: '; Convert to uppercase and lowercase\n(string-upcase "Hello")\n(string-downcase "WORLD")',
                        explanation: 'The string-upcase and string-downcase procedures convert a string to uppercase or lowercase. They return "HELLO" and "world" respectively.'
                    },
                    {
                        description: 'Checking string equality:',
                        code: '; Compare strings\n(string=? "Scheme" "scheme")\n(string-ci=? "Scheme" "scheme")',
                        explanation: 'The string=? procedure checks if two strings are exactly equal (case-sensitive). string-ci=? performs a case-insensitive comparison. The first returns #f (false) while the second returns #t (true).'
                    }
                ],
                difficulty: 'Beginner'
            }
        ]
    },
    {
        id: 'practical-examples',
        title: 'Practical Scheme Examples',
        description: 'Apply Scheme concepts to solve real-world problems',
        sections: [
            {
                id: 'data-processing',
                title: 'Data Processing',
                content: [
                    'Data processing is a common task in programming. Scheme\'s powerful list processing capabilities make it well-suited for manipulating and analyzing structured data.',
                    'In this section, we\'ll see how to use Scheme to process collections of data, perform aggregations, and transform data from one form to another.'
                ],
                codeExamples: [
                    {
                        description: 'Defining some sample data:',
                        code: '; Student records: (name score major)\n(define students\n  \'((Alice 95 Computer-Science)\n    (Bob 87 Mathematics)\n    (Charlie 92 Physics)\n    (Diana 97 Computer-Science)\n    (Eva 85 Mathematics)))',
                        explanation: 'We define a list of student records, where each record is a list containing a name, score, and major. This represents a typical dataset we might want to process.'
                    },
                    {
                        description: 'Extracting specific data:',
                        code: '; Get all student names\n(define (get-names students)\n  (map car students))\n\n(get-names students)',
                        explanation: 'This function uses map with car to extract the first element (name) from each student record. When applied to our dataset, it returns (Alice Bob Charlie Diana Eva).'
                    },
                    {
                        description: 'Filtering data:',
                        code: '; Filter Computer Science students\n(define (cs-students students)\n  (filter (lambda (student)\n            (eq? (caddr student) \'Computer-Science))\n          students))\n\n(cs-students students)',
                        explanation: 'This function uses filter with a predicate that checks if the third element (major) of each student record is Computer-Science. It returns just the records for Alice and Diana.'
                    },
                    {
                        description: 'Calculating averages:',
                        code: '; Calculate average score\n(define (average-score students)\n  (let ((scores (map cadr students)))\n    (/ (apply + scores) (length scores))))\n\n(average-score students)',
                        explanation: 'This function calculates the average score of all students. It extracts the scores using map with cadr, then divides the sum (computed with apply +) by the number of scores. For our dataset, it returns 91.2.'
                    },
                    {
                        description: 'Grouping data:',
                        code: '; Group students by major\n(define (group-by-major students)\n  (let ((majors (remove-duplicates (map caddr students))))\n    (map (lambda (major)\n           (cons major\n                 (filter (lambda (student)\n                           (eq? (caddr student) major))\n                         students)))\n         majors)))\n\n; Helper function to remove duplicates\n(define (remove-duplicates lst)\n  (cond ((null? lst) \'())\n        ((member (car lst) (cdr lst))\n         (remove-duplicates (cdr lst)))\n        (else (cons (car lst)\n                    (remove-duplicates (cdr lst))))))',
                        explanation: 'This function groups students by their major. It first extracts all unique majors, then for each major, creates a pair with the major and a list of all students in that major. The result is a list of major-students pairs.'
                    },
                    {
                        description: 'Sorting data:',
                        code: '; Sort students by score (descending)\n(define (sort-by-score students)\n  (sort students\n        (lambda (a b) (> (cadr a) (cadr b)))))\n\n(sort-by-score students)',
                        explanation: 'This function sorts the students by their scores in descending order. It uses the sort procedure with a comparison function that compares the second element (score) of each student record.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'functional-algorithms',
                title: 'Functional Algorithms',
                content: [
                    'Functional programming excels at expressing algorithms in a clear, concise manner. Scheme\'s functional nature makes it an excellent language for implementing classic algorithms.',
                    'In this section, we\'ll implement several common algorithms using Scheme\'s functional programming features, demonstrating how complex operations can be expressed elegantly.'
                ],
                codeExamples: [
                    {
                        description: 'Fibonacci sequence (recursive):',
                        code: '; Recursive Fibonacci implementation\n(define (fib n)\n  (cond ((= n 0) 0)\n        ((= n 1) 1)\n        (else (+ (fib (- n 1))\n                 (fib (- n 2))))))\n\n(fib 10)',
                        explanation: 'This is a classic recursive implementation of the Fibonacci sequence. It directly follows the mathematical definition: fib(n) = fib(n-1) + fib(n-2) with base cases fib(0) = 0 and fib(1) = 1. While elegant, this implementation is inefficient for large n due to repeated calculations.'
                    },
                    {
                        description: 'Fibonacci sequence (tail-recursive):',
                        code: '; Tail-recursive Fibonacci\n(define (fib-tail n)\n  (define (fib-iter a b count)\n    (if (= count 0)\n        a\n        (fib-iter b (+ a b) (- count 1))))\n  (fib-iter 0 1 n))\n\n(fib-tail 10)',
                        explanation: 'This is an efficient tail-recursive implementation of Fibonacci. It uses an iterative approach with an accumulator pattern, tracking two consecutive Fibonacci numbers at each step. This version uses constant stack space and is much more efficient.'
                    },
                    {
                        description: 'QuickSort algorithm:',
                        code: '; QuickSort implementation\n(define (quicksort lst)\n  (if (or (null? lst) (null? (cdr lst)))\n      lst\n      (let ((pivot (car lst))\n            (rest (cdr lst)))\n        (append (quicksort (filter (lambda (x) (< x pivot)) rest))\n                (list pivot)\n                (quicksort (filter (lambda (x) (>= x pivot)) rest))))))\n\n(quicksort \'(3 1 4 1 5 9 2 6 5))',
                        explanation: 'This implements the QuickSort algorithm in a functional style. It picks the first element as a pivot, partitions the remaining elements into those less than and greater than or equal to the pivot, recursively sorts each partition, and combines the results. The result for our input is (1 1 2 3 4 5 5 6 9).'
                    },
                    {
                        description: 'Binary search:',
                        code: '; Binary search in a sorted list\n(define (binary-search lst item)\n  (define (search low high)\n    (if (> low high)\n        #f  ; Not found\n        (let* ((mid (quotient (+ low high) 2))\n               (mid-val (list-ref lst mid)))\n          (cond ((= mid-val item) mid)  ; Found, return index\n                ((< mid-val item) (search (+ mid 1) high))\n                (else (search low (- mid 1)))))))\n  (search 0 (- (length lst) 1)))\n\n(binary-search \'(1 3 5 7 9 11 13) 7)',
                        explanation: 'This implements binary search, an efficient algorithm for finding an item in a sorted list. It repeatedly divides the search range in half, eliminating half of the remaining elements at each step. If the item is found, it returns its index; otherwise, it returns #f. For our example, it returns 3 (the index of 7).'
                    },
                    {
                        description: 'Merge sort:',
                        code: '; Merge sort implementation\n(define (merge-sort lst)\n  (define (merge lst1 lst2)\n    (cond ((null? lst1) lst2)\n          ((null? lst2) lst1)\n          ((< (car lst1) (car lst2))\n           (cons (car lst1) (merge (cdr lst1) lst2)))\n          (else\n           (cons (car lst2) (merge lst1 (cdr lst2))))))\n  \n  (define (split lst)\n    (if (or (null? lst) (null? (cdr lst)))\n        (values lst \'())\n        (let-values (((first-half second-half)\n                      (split (cddr lst))))\n          (values (cons (car lst) first-half)\n                  (cons (cadr lst) second-half)))))\n  \n  (if (or (null? lst) (null? (cdr lst)))\n      lst\n      (let-values (((first-half second-half) (split lst)))\n        (merge (merge-sort first-half)\n               (merge-sort second-half)))))\n\n(merge-sort \'(3 1 4 1 5 9 2 6 5))',
                        explanation: 'This implements the merge sort algorithm. It recursively splits the list into two halves, sorts each half, and then merges the sorted halves. The merge operation takes two sorted lists and combines them into a single sorted list. Merge sort has better worst-case performance than quicksort and is stable, meaning it preserves the relative order of equal elements.'
                    }
                ],
                difficulty: 'Advanced'
            },
            {
                id: 'building-dsl',
                title: 'Building Domain-Specific Languages',
                content: [
                    'One of Scheme\'s strengths is its ability to extend the language through macros, making it ideal for creating domain-specific languages (DSLs). DSLs are specialized languages designed for a particular application domain.',
                    'By building a DSL, you can create abstractions that match the concepts in your problem domain, making your code more readable and maintainable. Scheme\'s macro system enables powerful DSL construction.'
                ],
                codeExamples: [
                    {
                        description: 'A simple query language:',
                        code: '; Define a simple SQL-like query language\n(define-syntax select\n  (syntax-rules (from where)\n    ((select fields from table where condition)\n     (map fields (filter condition table)))\n    ((select fields from table)\n     (map fields table))))\n\n; Sample data\n(define people\n  \'((name "Alice" age 30 city "New York")\n    (name "Bob" age 25 city "Boston")\n    (name "Charlie" age 35 city "New York")\n    (name "Diana" age 28 city "Boston")))\n\n; Use our DSL\n(select (lambda (p) (list (cadr p) (cadddr p)))\n        from people\n        where (lambda (p) (> (cadddr p) 27)))',
                        explanation: 'This example creates a simple SQL-like DSL for querying data. The select macro takes field selectors, a data source, and an optional condition. It transforms these into appropriate map and filter operations. In this example, we select the name and age of people older than 27, resulting in a list of (name age) pairs.'
                    },
                    {
                        description: 'A DSL for mathematical expressions:',
                        code: '; Define a mathematics DSL\n(define-syntax sum\n  (syntax-rules (to)\n    ((sum var from start to end expr)\n     (let loop ((var start) (result 0))\n       (if (> var end)\n           result\n           (loop (+ var 1) (+ result expr)))))))\n\n; Calculate the sum of squares from 1 to 5\n(sum i from 1 to 5 (* i i))',
                        explanation: 'This example creates a DSL for mathematical summations. The sum macro takes a variable, a range, and an expression to evaluate for each value in the range. It transforms this high-level notation into an efficient iterative computation. In this example, we calculate the sum of squares from 1 to 5, which is 1²+2²+3²+4²+5² = 55.'
                    },
                    {
                        "description": "A DSL for pattern matching:",
                        "code": "; Define a pattern matching DSL\n(define-syntax match\n  (syntax-rules (else)\n    ((match value\n       (pattern1 expr1)\n       (pattern2 expr2) ...)\n     (let ((val value))\n       (cond\n        ((equal? val pattern1) expr1)\n        ((equal? val pattern2) expr2) ...)))\n    \n    ((match value\n       (pattern1 expr1)\n       (pattern2 expr2) ...\n       (else default-expr))\n     (let ((val value))\n       (cond\n        ((equal? val pattern1) expr1)\n        ((equal? val pattern2) expr2) ...\n        (else default-expr))))))\n\n; Use our pattern matching DSL\n(define (factorial n)\n  (match n\n    (0 1)\n    (1 1)\n    (else (* n (factorial (- n 1))))))\n\n; Call the factorial function\n(factorial 5)",
                        "explanation": "This example creates a DSL for pattern matching, a common feature in functional languages. The match macro takes a value and a series of pattern-expression pairs, matching the value against each pattern in turn. This allows for clear, declarative code. We use it to implement factorial with explicit base cases, then call it with the value 5 to calculate 5! = 120."
                    },
                    {
                        description: 'A DSL for data validation:',
                        code: '; Define a data validation DSL\n(define-syntax validate\n  (syntax-rules (is length > <)\n    ((validate value is number)\n     (number? value))\n    ((validate value is string)\n     (string? value))\n    ((validate value length > min)\n     (and (or (string? value) (list? value) (vector? value))\n          (> (length value) min)))\n    ((validate value length < max)\n     (and (or (string? value) (list? value) (vector? value))\n          (< (length value) max)))))\n\n; Test our validation DSL\n(define data "test")\n(and (validate data is string)\n     (validate data length > 2)\n     (validate data length < 10))',
                        explanation: 'This example creates a DSL for data validation. The validate macro provides a readable syntax for common validation tasks. It transforms these high-level descriptions into the appropriate Scheme predicates. In this example, we check if data is a string between 3 and 9 characters long.'
                    }
                ],
                difficulty: 'Advanced'
            }
        ]
    },
    {
        id: 'loop-constructs',
        title: 'Loop Constructs',
        description: 'Explore various looping mechanisms and iteration patterns in Scheme',
        sections: [
            {
                id: 'basic-loops',
                title: 'Basic Looping Constructs',
                content: [
                    'While Scheme is primarily a functional language that encourages recursion, looping constructs can make certain algorithmic patterns more readable and intuitive. Scheme\'s macro system allows us to define custom loop constructs that provide convenient syntactic forms for common iteration patterns.',
                    'In this section, we\'ll explore basic looping constructs like while, until, and do-while, which mirror similar constructs in other programming languages but with Scheme\'s distinctive syntax and semantics.'
                ],
                codeExamples: [
                    {
                        description: 'The while loop:',
                        code: '; Define a counter\n(define counter 1)\n\n; Count from 1 to 5 using while\n(while (<= counter 5)\n  (display counter)\n  (display " ")\n  (set! counter (+ counter 1)))',
                        explanation: 'The while macro repeatedly executes its body as long as the condition remains true. Here we count from 1 to 5, displaying each number. Behind the scenes, this is implemented as a named let loop that conditionally recurses.'
                    },
                    {
                        description: 'The until loop:',
                        code: '; Define a counter\n(define counter 1)\n\n; Count from 1 to 5 using until\n(until (> counter 5)\n  (display counter)\n  (display " ")\n  (set! counter (+ counter 1)))',
                        explanation: 'The until macro is the logical complement of while - it continues looping as long as the condition is false. This example produces the same output as the while example, but the termination condition is inverted.'
                    },
                    {
                        description: 'The do-while loop:',
                        code: '; Define a counter\n(define counter 6)\n\n; Try to count from 6 to 5 (impossible, but body executes once)\n(do-while (< counter 5)\n  (display counter)\n  (display " ")\n  (set! counter (+ counter 1)))',
                        explanation: 'The do-while macro guarantees that its body executes at least once, even if the condition is false initially. In this example, even though counter is already 6 (which exceeds our limit of 5), the body still executes once, displaying "6 ".'
                    },
                    {
                        description: 'Implementing a simple loop:',
                        code: '; Compute factorial using a while loop\n(define (factorial n)\n  (let ((result 1)\n        (i 1))\n    (while (<= i n)\n      (set! result (* result i))\n      (set! i (+ i 1)))\n    result))\n\n(factorial 5)',
                        explanation: 'This example implements the factorial function using a while loop instead of recursion. We initialize result to 1 and i to 1, then multiply result by i for each value from 1 to n. The factorial of 5 is 120.'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'list-iteration',
                title: 'Iterating Over Lists',
                content: [
                    'Scheme excels at list processing, and its functional nature provides elegant ways to operate on lists. However, sometimes a more imperative style of iteration can be clearer, especially for those coming from other programming paradigms.',
                    'In this section, we\'ll explore macros that provide convenient syntax for iterating over lists, including for loops with various configurations and specialized iterators that expose element indices.'
                ],
                codeExamples: [
                    {
                        description: 'The for loop:',
                        code: 'Simple list of numbers\n(define numbers \'(1 2 3 4 5))\n\n; Square each number using for\n(for x in numbers\n  (* x x))',
                        explanation: 'The for macro provides a simple way to iterate over a list. Here we square each number in the list. Note that for is implemented using map, so it returns a new list containing the results of applying the body expression to each element. The result is (1 4 9 16 25).'
                    },
                    {
                        description: 'Alternative for syntax:',
                        code: '\n; Using the alternative "as" syntax\n(for numbers as x\n  (* x x))',
                        explanation: 'The for macro supports an alternative syntax where the list comes first, followed by "as" and the element variable. This can be more readable in some cases, especially with complex list expressions. The result is the same: (1 4 9 16 25).'
                    },
                    {
                        description: 'The for-each-with-index macro:',
                        code: '\n Print each element with its index\n(for-each-with-index (element index) in \'(a b c d e)\n  (display "Element at index ")\n  (display index)\n  (display ": ")\n  (display element)\n  (newline))',
                        explanation: 'The for-each-with-index macro iterates over a list, providing both the current element and its index for each iteration. Unlike for, it does not collect return values, making it suitable for side effects like displaying output.'
                    },
                    {
                        description: 'Processing list elements:',
                        code: '\nFind the sum and product of a list\n(define (sum-and-product numbers)\n  (let ((sum 0)\n        (product 1))\n    (for-each-with-index (num _) in numbers\n      (set! sum (+ sum num))\n      (set! product (* product num)))\n    (list sum product)))\n\n(sum-and-product \'(1 2 3 4 5))',
                        explanation: 'This example uses for-each-with-index to calculate both the sum and product of a list of numbers. We use _ as a placeholder for the index since we don\'t need it. The result is (15 120), representing the sum and product of 1 through 5.'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'numeric-iteration',
                title: 'Numeric Iteration Patterns',
                content: [
                    'Many algorithms involve iterating over ranges of numbers. While this can be done with basic loops and manual counters, dedicated numeric iteration constructs make the code more concise and readable.',
                    'This section covers various forms of numeric iteration, including range-based loops, repeat loops for fixed-count iteration, and loops with custom step sizes.'
                ],
                codeExamples: [
                    {
                        description: 'The repeat macro:',
                        code: ';Display "Hello" 3 times\n(repeat 3\n  (display "Hello")\n  (newline))',
                        explanation: 'The repeat macro executes its body a fixed number of times. It\'s useful when you need to perform an operation repeatedly without needing a loop variable. Here, it displays "Hello" three times.'
                    },
                    {
                        description: 'The for-range macro:',
                        code: '\nDisplay numbers from 1 to 5\n(for-range i 1 5\n  (display i)\n  (display " "))',
                        explanation: 'The for-range macro provides a convenient way to iterate over a range of numbers. It takes a variable name, starting value, and ending value (inclusive). This example displays the numbers 1 through 5.'
                    },
                    {
                        description: 'The iterate macro with custom step:',
                        code: ';Display even numbers from 2 to 10\n(iterate i from 2 to 10 by 2\n  (display i)\n  (display " "))',
                        explanation: 'The iterate macro extends for-range by adding the ability to specify a custom step size. This example counts by 2s, displaying only even numbers from 2 to 10.'
                    },
                    {
                        description: 'Computing a sum using for-range:',
                        code: '; Calculate the sum of numbers from 1 to 10\n(define (sum-range from to)\n  (let ((result 0))\n    (for-range i from to\n      (set! result (+ result i)))\n    result))\n\n(sum-range 1 10)',
                        explanation: 'This example uses for-range to calculate the sum of integers from 1 to 10. We initialize result to 0, then add each number in the range to it. The sum of 1 to 10 is 55.'
                    }
                ],
                difficulty: 'Beginner'
            },
            {
                id: 'nested-loops',
                title: 'Nested and Advanced Loop Patterns',
                content: [
                    'Many algorithms require nested loops or more complex iteration patterns. Scheme\'s macro system allows us to create specialized constructs that express these patterns clearly and concisely.',
                    'This section explores nested loops, fold-like accumulation patterns, and pattern-matching based control flow that combines selection and iteration.'
                ],
                codeExamples: [
                    {
                        description: 'The nested-loop macro:',
                        code: '; Print a multiplication table from 1×1 to 3×3\n(nested-loop (outer i 1 3)\n             (inner j 1 3)\n  (display i)\n  (display "×")\n  (display j)\n  (display "=")\n  (display (* i j))\n  (newline))',
                        explanation: 'The nested-loop macro provides a clean syntax for nested iterations, which are common in many algorithms. This example generates a small multiplication table, with the outer loop iterating over values for i and the inner loop iterating over values for j.'
                    },
                    {
                        description: 'The fold-loop pattern:',
                        code: '; Calculate the sum of a list using fold-loop\n(define numbers \'(1 2 3 4 5))\n\n(fold-loop sum init 0 in numbers\n  (+ sum element))',
                        explanation: 'The fold-loop macro combines iteration with accumulation, similar to a fold/reduce operation. It maintains an accumulator that is updated at each step based on the current element. This example calculates the sum of a list of numbers, starting with an initial value of 0.'
                    },
                    {
                        "description": "Implementing a custom loop macro:",
                        "code": "; Define a countdown macro\n(define-syntax countdown\n  (syntax-rules (from to)\n    ((countdown var from start to end body ...)\n     (let loop ((var start))\n       (when (>= var end)\n         body ...\n         (loop (- var 1)))))))\n\n; Use the countdown macro\n(countdown i from 5 to 1\n  (display i)\n  (display \" \"))",
                        "explanation": "This example shows how to implement a custom countdown macro that counts downward instead of upward. Like for-range, it uses a named let, but it decreases the loop variable and continues as long as it remains greater than or equal to the end value. This displays \"5 4 3 2 1 \"."
                    },
                    {
                        description: 'Building a matrix with nested loops:',
                        code: '; Create a 3×3 identity matrix\n(define (make-identity-matrix size)\n  (let ((matrix (make-vector size)))\n    (for-range i 0 (- size 1)\n      (vector-set! matrix i (make-vector size 0)))\n    (for-range i 0 (- size 1)\n      (vector-set! (vector-ref matrix i) i 1))\n    matrix))\n\n; Display a matrix\n(define (display-matrix matrix)\n  (let ((size (vector-length matrix)))\n    (for-range i 0 (- size 1)\n      (for-range j 0 (- size 1)\n        (display (vector-ref (vector-ref matrix i) j))\n        (display " "))\n      (newline))))\n\n(define identity-3 (make-identity-matrix 3))\n(display-matrix identity-3)',
                        explanation: 'This example uses nested for-range loops to create and display a 3×3 identity matrix (a matrix with 1s on the diagonal and 0s elsewhere). The make-identity-matrix function first creates a vector of vectors filled with zeros, then sets the diagonal elements to 1. The display-matrix function uses nested loops to print the matrix rows.'
                    }
                ],
                difficulty: 'Intermediate'
            },
            {
                id: 'loop-implementation',
                title: 'Implementing Loop Constructs',
                content: [
                    'Understanding how loop constructs are implemented as macros provides insight into Scheme\'s macro system and helps you create your own control structures. Most loops in Scheme are built on top of recursion, leveraging Scheme\'s tail-call optimization.',
                    'This section examines the implementation of various loop macros, showing how they transform high-level loop syntax into recursive procedures.'
                ],
                codeExamples: [
                    {
                        description: 'Implementation of the while macro:',
                        code: '; Definition of the while macro\n(define-syntax while\n  (syntax-rules ()\n    ((while condition body ...)\n     (let loop ()\n       (if condition\n           (begin\n             body ...\n             (loop))\n           #f)))))',
                        explanation: 'The while macro transforms its high-level loop syntax into a named let expression that implements a tail-recursive loop. If the condition is true, it executes the body expressions and then recurses by calling loop again. If the condition is false, it returns #f (though the return value is typically ignored).'
                    },
                    {
                        description: 'Implementation of the for macro:',
                        code: '; Definition of the for macro\n(define-syntax for\n  (syntax-rules (in as)\n    ((for element in list body ...)\n     (map (lambda (element)\n            body ...)\n          list))\n    ((for list as element body ...)\n     (for element in list body ...))))',
                        explanation: 'The for macro uses map to apply a lambda function to each element in a list. The lambda takes the element variable as its parameter and executes the body expressions. The macro supports two syntax forms: "for element in list" and "for list as element". Unlike imperative for loops, this is a functional construct that returns a new list of results.'
                    },
                    {
                        description: 'Implementation of the for-range macro:',
                        code: '; Definition of the for-range macro\n(define-syntax for-range\n  (syntax-rules ()\n    ((for-range var from to body ...)\n     (let loop ((var from))\n       (when (<= var to)\n         body ...\n         (loop (+ var 1)))))))',
                        explanation: 'The for-range macro also uses a named let to create a recursive loop, but it manages the loop variable explicitly. It initializes var to the starting value, executes the body as long as var is less than or equal to the ending value, and increments var before each recursive call.'
                    },
                    {
                        description: 'Implementing a custom loop macro:',
                        code: '; Define a countdown macro\n(define-syntax countdown\n  (syntax-rules (from to)\n    ((countdown var from start to end body ...)\n     (let loop ((var start))\n       (when (>= var end)\n         body ...\n         (loop (- var 1)))))))\n\n; Use the countdown macro\n(countdown i from 5 to 1\n  (display i)\n  (display " "))',
                        explanation: 'This example shows how to implement a custom countdown macro that counts downward instead of upward. Like for-range, it uses a named let, but it decreases the loop variable and continues as long as it remains greater than or equal to the end value. This displays "5 4 3 2 1 ".'
                    },
                    {
                        description: 'A more complex loop implementation:',
                        code: '; Define a "times" macro that returns a list of results\n(define-syntax times\n  (syntax-rules (collect)\n    ((times n collect expr)\n     (let loop ((i 0) (results \'()))\n       (if (= i n)\n           (reverse results)\n           (loop (+ i 1) (cons expr results)))))))\n\n; Use the times macro to create a list of squares\n(times 5 collect (* i i))',
                        explanation: 'This example implements a "times" macro that not only repeats an operation n times but also collects the results into a list. It maintains an accumulator called results which it builds in reverse order (for efficiency), then reverses at the end. This returns (0 1 4 9 16), the squares of 0 through 4.'
                    }
                ],
                difficulty: 'Advanced'
            }
        ]
    }
];

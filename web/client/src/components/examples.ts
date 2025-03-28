export interface SchemeDoc {
    name: string;
    signature: string;
    description: string;
    example: string;
    output: string;
    category: 'Math' | 'List' | 'String' | 'IO' | 'Control' | 'Vector' | 'Predicate' | 'Syntax';
}

export const SCHEME_DOCS: SchemeDoc[] = [
    {
        name: "+",
        signature: "(+ number ...)",
        description: "Adds numbers together. With no arguments, returns 0.",
        example: "(+ 1 2 3 4)",
        output: "10",
        category: "Math"
    },
    {
        name: "-",
        signature: "(- number ...)",
        description: "Subtracts numbers. With one argument, negates it.",
        example: "(- 10 2 3)",
        output: "5",
        category: "Math"
    },
    {
        name: "*",
        signature: "(* number ...)",
        description: "Multiplies numbers together. With no arguments, returns 1.",
        example: "(* 2 3 4)",
        output: "24",
        category: "Math"
    },
    {
        name: "/",
        signature: "(/ number ...)",
        description: "Divides numbers. With one argument, returns its reciprocal.",
        example: "(/ 24 2 3)",
        output: "4",
        category: "Math"
    },
    {
        name: "zero?",
        signature: "(zero? number)",
        description: "Returns true if the number is equal to zero.",
        example: "(zero? 0)",
        output: "#t",
        category: "Predicate"
    },

    // Comparison Operators
    {
        name: "<",
        signature: "(< number1 number2 ...)",
        description: "Returns true if each number is strictly less than the next.",
        example: "(< 1 2 3)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: ">",
        signature: "(> number1 number2 ...)",
        description: "Returns true if each number is strictly greater than the next.",
        example: "(> 3 2 1)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "<=",
        signature: "(<= number1 number2 ...)",
        description: "Returns true if each number is less than or equal to the next.",
        example: "(<= 1 1 2)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: ">=",
        signature: "(>= number1 number2 ...)",
        description: "Returns true if each number is greater than or equal to the next.",
        example: "(>= 3 3 2)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "=",
        signature: "(= number1 number2 ...)",
        description: "Returns true if all numbers are equal.",
        example: "(= 42 42 42)",
        output: "#t",
        category: "Predicate"
    },

    // List Operations
    {
        name: "list",
        signature: "(list obj ...)",
        description: "Creates a new list containing the given objects.",
        example: "(list 1 2 3 4)",
        output: "(1 2 3 4)",
        category: "List"
    },
    {
        name: "car",
        signature: "(car pair)",
        description: "Returns the first element of a pair or list.",
        example: "(car '(1 2 3))",
        output: "1",
        category: "List"
    },
    {
        name: "cdr",
        signature: "(cdr pair)",
        description: "Returns all but the first element of a list.",
        example: "(cdr '(1 2 3))",
        output: "(2 3)",
        category: "List"
    },
    {
        name: "cons",
        signature: "(cons obj1 obj2)",
        description: "Constructs a pair whose car is obj1 and cdr is obj2.",
        example: "(cons 1 '(2 3))",
        output: "(1 2 3)",
        category: "List"
    },
    {
        name: "append",
        signature: "(append list ...)",
        description: "Joins lists together into a single list.",
        example: "(append '(1 2) '(3 4))",
        output: "(1 2 3 4)",
        category: "List"
    },
    {
        name: "length",
        signature: "(length list)",
        description: "Returns the number of elements in the list.",
        example: "(length '(1 2 3 4))",
        output: "4",
        category: "List"
    },
    {
        name: "reverse",
        signature: "(reverse list)",
        description: "Returns a new list with elements in reverse order.",
        example: "(reverse '(1 2 3))",
        output: "(3 2 1)",
        category: "List"
    },
    {
        name: "list-ref",
        signature: "(list-ref list k)",
        description: "Returns the k-th element of the list (zero-based).",
        example: "(list-ref '(a b c d) 2)",
        output: "c",
        category: "List"
    },
    {
        name: "member",
        signature: "(member obj list)",
        description: "Returns the first sublist whose car is equal to obj, or #f if not found.",
        example: "(member 'c '(a b c d))",
        output: "(c d)",
        category: "List"
    },
    {
        name: "assoc",
        signature: "(assoc obj alist)",
        description: "Finds the first pair in alist whose car is equal to obj, or #f if not found.",
        example: "(assoc 'b '((a 1) (b 2) (c 3)))",
        output: "(b 2)",
        category: "List"
    },

    // Higher-Order Functions
    {
        name: "map",
        signature: "(map procedure list)",
        description: "Applies a procedure to each element in the list, returning a new list with the results.",
        example: "(map (lambda (x) (* x x)) '(1 2 3 4))",
        output: "(1 4 9 16)",
        category: "List"
    },
    {
        name: "apply",
        signature: "(apply proc arg ... args)",
        description: "Calls proc with args as arguments, spreading the last argument.",
        example: "(apply + 1 2 '(3 4))",
        output: "10",
        category: "Control"
    },
    {
        name: "eval",
        signature: "(eval expression)",
        description: "Evaluates the given Scheme expression.",
        example: "(eval '(+ 1 2))",
        output: "3",
        category: "Control"
    },

    // Type Predicates
    {
        name: "null?",
        signature: "(null? obj)",
        description: "Returns true if obj is the empty list.",
        example: "(null? '())",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "pair?",
        signature: "(pair? obj)",
        description: "Returns true if obj is a pair.",
        example: "(pair? '(a . b))",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "list?",
        signature: "(list? obj)",
        description: "Returns true if obj is a proper list.",
        example: "(list? '(a b c))",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "symbol?",
        signature: "(symbol? obj)",
        description: "Returns true if obj is a symbol.",
        example: "(symbol? 'abc)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "number?",
        signature: "(number? obj)",
        description: "Returns true if obj is a number.",
        example: "(number? 42)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "string?",
        signature: "(string? obj)",
        description: "Returns true if obj is a string.",
        example: '(string? "hello")',
        output: "#t",
        category: "Predicate"
    },
    {
        name: "boolean?",
        signature: "(boolean? obj)",
        description: "Returns true if obj is either #t or #f.",
        example: "(boolean? #t)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "procedure?",
        signature: "(procedure? obj)",
        description: "Returns true if obj is a procedure.",
        example: "(procedure? car)",
        output: "#t",
        category: "Predicate"
    },

    // String Operations
    {
        name: "string=?",
        signature: "(string=? string1 string2)",
        description: "Returns true if the strings are the same length and contain the same characters.",
        example: '(string=? "hello" "hello")',
        output: "#t",
        category: "String"
    },
    {
        name: "string<?",
        signature: "(string<? string1 string2)",
        description: "Returns true if string1 is lexicographically less than string2.",
        example: '(string<? "apple" "banana")',
        output: "#t",
        category: "String"
    },
    {
        name: "string>?",
        signature: "(string>? string1 string2)",
        description: "Returns true if string1 is lexicographically greater than string2.",
        example: '(string>? "zebra" "apple")',
        output: "#t",
        category: "String"
    },
    {
        name: "string-length",
        signature: "(string-length string)",
        description: "Returns the number of characters in the string.",
        example: '(string-length "hello")',
        output: "5",
        category: "String"
    },
    {
        name: "string-append",
        signature: "(string-append str1 str2 ...)",
        description: "Concatenates multiple strings together.",
        example: '(string-append "Hello" ", " "World!")',
        output: '"Hello, World!"',
        category: "String"
    },
    {
        name: "substring",
        signature: "(substring string start end)",
        description: "Returns a substring from start (inclusive) to end (exclusive).",
        example: '(substring "abcdefg" 2 5)',
        output: '"cde"',
        category: "String"
    },
    {
        name: "string-ref",
        signature: "(string-ref string k)",
        description: "Returns the k-th character of the string (zero-based).",
        example: '(string-ref "abcdefg" 3)',
        output: "#\\d",
        category: "String"
    },
    {
        name: "string->list",
        signature: "(string->list string)",
        description: "Converts a string to a list of characters.",
        example: '(string->list "abc")',
        output: "(#\\a #\\b #\\c)",
        category: "String"
    },
    {
        name: "list->string",
        signature: "(list->string list)",
        description: "Converts a list of characters to a string.",
        example: "(list->string '(#\\h #\\i))",
        output: '"hi"',
        category: "String"
    },

    // Vector Operations
    {
        name: "make-vector",
        signature: "(make-vector k [fill])",
        description: "Creates a vector of length k, optionally filled with the fill value.",
        example: "(make-vector 3 'a)",
        output: "#(a a a)",
        category: "Vector"
    },
    {
        name: "vector",
        signature: "(vector obj ...)",
        description: "Creates a vector containing the given objects.",
        example: "(vector 1 2 3)",
        output: "#(1 2 3)",
        category: "Vector"
    },
    {
        name: "vector-ref",
        signature: "(vector-ref vector k)",
        description: "Returns the k-th element of the vector (zero-based).",
        example: "(vector-ref #(a b c) 1)",
        output: "b",
        category: "Vector"
    },
    {
        name: "vector-set!",
        signature: "(vector-set! vector k obj)",
        description: "Sets the k-th element of the vector to obj.",
        example: "(define v #(a b c)) (vector-set! v 1 'x) v",
        output: "#(a x c)",
        category: "Vector"
    },
    {
        name: "vector-length",
        signature: "(vector-length vector)",
        description: "Returns the length of the vector.",
        example: "(vector-length #(a b c))",
        output: "3",
        category: "Vector"
    },

    // I/O Operations
    {
        name: "display",
        signature: "(display obj [port])",
        description: "Outputs a value to the console or port. Strings are displayed without quotes.",
        example: '(display "Hello, World!")',
        output: "Hello, World!",
        category: "IO"
    },
    {
        name: "write",
        signature: "(write obj [port])",
        description: "Writes a value to the console or port in a way that it can be read by read.",
        example: '(write "Hello, World!")',
        output: '"Hello, World!"',
        category: "IO"
    },
    {
        name: "newline",
        signature: "(newline [port])",
        description: "Outputs a newline character to the console or port.",
        example: '(display "Hello") (newline) (display "World")',
        output: "Hello\nWorld",
        category: "IO"
    },
    {
        name: "read",
        signature: "(read [port])",
        description: "Reads and returns a Scheme object from the input port.",
        example: "(define p (open-input-string \"(1 2 3)\")) (read p)",
        output: "(1 2 3)",
        category: "IO"
    },

    // Other Utility Functions
    {
        name: "not",
        signature: "(not obj)",
        description: "Returns true if obj is false, otherwise returns false.",
        example: "(not #f)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "eq?",
        signature: "(eq? obj1 obj2)",
        description: "Tests whether two objects are the same object (by identity).",
        example: "(eq? 'a 'a)",
        output: "#t",
        category: "Predicate"
    },
    {
        name: "equal?",
        signature: "(equal? obj1 obj2)",
        description: "Tests whether two objects are structurally equal (recursive comparison).",
        example: "(equal? '(1 2 3) '(1 2 3))",
        output: "#t",
        category: "Predicate"
    },

    // Control Flow Syntax
    {
        name: "when",
        signature: "(when test expr ...)",
        description: "If test is true, evaluates expressions in order and returns the last result.",
        example: "(when (> 3 2) (display 'greater) 'done)",
        output: "greater\ndone",
        category: "Syntax"
    },
    {
        name: "unless",
        signature: "(unless test expr ...)",
        description: "If test is false, evaluates expressions in order and returns the last result.",
        example: "(unless (< 3 2) (display 'not-less) 'done)",
        output: "not-less\ndone",
        category: "Syntax"
    },
    {
        name: "cond",
        signature: "(cond (test expr ...) ...)",
        description: "Evaluates each test in order until one returns true, then evaluates the corresponding expressions.",
        example: "(cond ((> 3 2) 'greater) ((< 3 2) 'less) (else 'equal))",
        output: "greater",
        category: "Syntax"
    },
    {
        name: "case",
        signature: "(case key ((datum ...) expr ...) ...)",
        description: "Evaluates key, then finds the clause with a matching datum and evaluates its expressions.",
        example: "(case (+ 1 1) ((1) 'one) ((2) 'two) (else 'many))",
        output: "two",
        category: "Syntax"
    },
    {
        name: "and",
        signature: "(and expr ...)",
        description: "Evaluates expressions from left to right, returning the first false value or the last value if all are true.",
        example: "(and (> 2 1) (< 2 3) 'result)",
        output: "result",
        category: "Syntax"
    },
    {
        name: "or",
        signature: "(or expr ...)",
        description: "Evaluates expressions from left to right, returning the first true value or #f if all are false.",
        example: "(or (> 2 3) (< 2 1) (= 2 2))",
        output: "#t",
        category: "Syntax"
    },
    {
        name: "let",
        signature: "(let ((var expr) ...) body ...)",
        description: "Creates local bindings for variables, then evaluates body expressions.",
        example: "(let ((x 1) (y 2)) (+ x y))",
        output: "3",
        category: "Syntax"
    },
    {
        name: "let*",
        signature: "(let* ((var expr) ...) body ...)",
        description: "Like let, but each binding is available to the expressions of later bindings.",
        example: "(let* ((x 1) (y (+ x 1))) (+ x y))",
        output: "3",
        category: "Syntax"
    },
    {
        name: "for-each-with-index",
        signature: "(for-each-with-index (element index) in list body ...)",
        description: "Executes body for each element in the list with its index.",
        example: "(for-each-with-index (e i) in '(a b c) (display e) (display i) (newline))",
        output: "a0\nb1\nc2",
        category: "Control"
    },
    {
        name: "for-range",
        signature: "(for-range var from to body ...)",
        description: "Executes body for each value from 'from' to 'to' inclusive.",
        example: "(for-range i 1 3 (display i) (newline))",
        output: "1\n2\n3",
        category: "Control"
    },
    {
        name: "while",
        signature: "(while condition body ...)",
        description: "Executes body repeatedly as long as condition is true.",
        example: "(let ((i 0)) (while (< i 3) (display i) (set! i (+ i 1))))",
        output: "012",
        category: "Control"
    },
    {
        name: "for",
        signature: "(for element in list body ...)",
        description: "Maps a lambda over the list, executing body for each element.",
        example: "(for x in '(1 2 3) (display (+ x 10)) (newline))",
        output: "11\n12\n13",
        category: "Control"
    },
    {
        name: "repeat",
        signature: "(repeat n body ...)",
        description: "Executes body n times.",
        example: "(repeat 3 (display 'hi) (newline))",
        output: "hi\nhi\nhi",
        category: "Control"
    }
];

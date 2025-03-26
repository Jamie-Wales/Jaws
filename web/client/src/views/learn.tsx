import { useState, useRef } from 'react';
import { Terminal, TerminalRef } from '@/components/terminal';
import { Button } from '@/components/ui/button';
import { Card, CardContent } from '@/components/ui/card';
import { ChevronLeft, ArrowRight, Parentheses } from 'lucide-react';
import { useNavigate } from 'react-router-dom';
import { HighlightedText } from '@/components/highlightedText';
import { DifficultyBadge } from '@/components/DifficultyBadge';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';

interface Chapter {
    id: string;
    title: string;
    description: string;
    sections: Section[];
}

interface Section {
    id: string;
    title: string;
    content: string;
    code?: string;
    difficulty: 'Beginner' | 'Intermediate' | 'Advanced';
}

const CHAPTERS: Chapter[] = [
    {
        id: 'basics',
        title: 'Scheme Basics',
        description: 'Learn the fundamentals of Scheme programming',
        sections: [
            {
                id: 'intro',
                title: 'Introduction to Scheme',
                content: 'Scheme is a dialect of Lisp that supports multiple paradigms with a focus on functional programming. It is known for its simplicity, elegance, and powerful features such as first-class functions, lexical scoping, and proper tail recursion. Scheme was initially designed for education and research, making it an excellent language for learning programming concepts.',
                code: '(display "Hello, World!")',
                difficulty: 'Beginner'
            },
            {
                id: 'data-types',
                title: 'Data Types',
                content: 'Scheme has several basic data types including numbers, strings, symbols, and lists. Numbers include integers, floating-point, rationals, and complex. Strings are sequences of characters. Symbols are identifiers that are typically used for variables and function names. Lists are the fundamental data structure in Scheme, represented with parentheses.',
                code: `; Numbers
(+ 1 2)
; Strings
(string-append "Hello" " " "World")
; Lists
(list 1 2 3)`,
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
                code: `(define (square x)
  (* x x))

(square 5) ; Returns 25`,
                difficulty: 'Beginner'
            },
            {
                id: 'lambda',
                title: 'Lambda Expressions',
                content: 'Lambda expressions allow you to create anonymous functions in Scheme. These are useful for creating functions that are used only once or as arguments to higher-order functions. The syntax is (lambda (parameters) body).',
                code: `; Creating an anonymous function
(lambda (x) (* x x))

; Using a lambda expression
((lambda (x) (* x x)) 5) ; Returns 25

; Assigning a lambda to a variable
(define square (lambda (x) (* x x)))
(square 5) ; Returns 25`,
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
                id: 'recursion',
                title: 'Recursion Patterns',
                content: 'Scheme excels at recursive solutions due to its proper tail call optimization. This section covers common recursion patterns and techniques for efficient recursive algorithms.',
                code: `(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))

(factorial 5) ; Returns 120`,
                difficulty: 'Intermediate'
            },
            {
                id: 'macros',
                title: 'Macros and Syntax Rules',
                content: 'One of Scheme\'s most powerful features is its macro system, which allows you to extend the language itself. Using define-syntax and syntax-rules, you can create new syntactic forms.',
                code: `(define-syntax begin
  (syntax-rules ()
    ((begin expr)
     (expr))
    ((begin expr expr2 ...)
     ((lambda () expr) (begin expr2 ...)))))

(begin 
  (display "First") 
  (newline)
  (display "Second"))`,
                difficulty: 'Advanced'
            }
        ]
    }
];

export function LearnScheme() {
    const [selectedChapter, setSelectedChapter] = useState<Chapter>(CHAPTERS[0]);
    const [selectedSection, setSelectedSection] = useState<Section>(CHAPTERS[0].sections[0]);
    const terminalRef = useRef<TerminalRef>(null);
    const navigate = useNavigate();

    // Use the JAWS interpreter hook
    const jawsInterpreter = useJawsInterpreter();

    const handleRunExample = async () => {
        if (selectedSection.code && terminalRef.current) {
            // Set the code in the terminal input
            terminalRef.current.setInput(selectedSection.code);

            // Simulate a user command execution by manually triggering the terminal's command execution
            // First, write the input to the terminal display
            terminalRef.current.writeInput(selectedSection.code);

            try {
                // Get the result from the JAWS interpreter directly
                const result = await handleCommand(selectedSection.code);

                // Write the output to the terminal display
                terminalRef.current.writeOutput(result);
            } catch (error) {
                terminalRef.current.writeOutput(`Error: ${error instanceof Error ? error.message : String(error)}`);
            }

            // Clear the input field after execution
            terminalRef.current.clearInput();
        }
    };

    const handleCommand = async (command: string) => {
        // Check if interpreter is ready
        if (jawsInterpreter.loading) {
            return "Loading JAWS interpreter...";
        }

        if (jawsInterpreter.error) {
            return `Interpreter error: ${jawsInterpreter.error}`;
        }

        // Execute command using the JAWS interpreter and directly return the result
        // The updated interpreter will handle stdout captures internally
        try {
            return jawsInterpreter.evaluate(command);
        } catch (error) {
            console.error('Evaluation error:', error);
            return `Error: ${error instanceof Error ? error.message : String(error)}`;
        }
    };

    return (
        <div className="min-h-screen bg-zinc-900 flex flex-col">
            <div className="flex border-b border-zinc-700/50 p-4 items-center gap-2">
                <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => navigate('/')}
                    className="text-white hover:bg-zinc-800/90"
                >
                    <ChevronLeft className="h-5 w-5" />
                </Button>
                <h1 className="text-xl font-semibold text-white flex items-center gap-2">
                    <Parentheses className="h-5 w-5 text-cyan-400" />
                    Learn Scheme
                </h1>
            </div>

            <div className="flex-1 flex overflow-hidden">
                {/* Sidebar */}
                <div className="w-80 border-r border-zinc-700/50 bg-zinc-900 flex flex-col overflow-hidden">
                    <div className="p-4 border-b border-zinc-700/50">
                        <h2 className="text-lg font-semibold text-white">
                            Curriculum
                        </h2>
                    </div>
                    <div className="flex-1 overflow-auto scrollbar-thin scrollbar-thumb-gray-600 scrollbar-track-transparent">
                        {CHAPTERS.map((chapter, index) => (
                            <div
                                key={chapter.id}
                                className="border-b border-zinc-700/50 animate-fadeScale"
                                style={{ animationDelay: `${index * 0.1}s` }}
                            >
                                <button
                                    onClick={() => {
                                        setSelectedChapter(chapter);
                                        setSelectedSection(chapter.sections[0]);
                                    }}
                                    className={`w-full text-left p-4 transition-colors hover:bg-zinc-800/50 ${selectedChapter.id === chapter.id
                                        ? 'bg-zinc-800 text-white'
                                        : 'text-zinc-400 hover:text-white'
                                        }`}
                                >
                                    <h3 className="font-medium text-current">{chapter.title}</h3>
                                    <p className="text-sm mt-1 opacity-80 text-current">{chapter.description}</p>
                                </button>
                                {selectedChapter.id === chapter.id && (
                                    <div className="bg-zinc-800/30 border-t border-zinc-700/50">
                                        {chapter.sections.map((section) => (
                                            <button
                                                key={section.id}
                                                onClick={() => setSelectedSection(section)}
                                                className={`w-full text-left p-3 pl-8 text-sm transition-colors ${selectedSection.id === section.id
                                                    ? 'bg-zinc-800 text-white'
                                                    : 'text-zinc-400 hover:bg-zinc-800/50 hover:text-white'
                                                    }`}
                                            >
                                                <span className="text-current">
                                                    {section.title}
                                                </span>
                                            </button>
                                        ))}
                                    </div>
                                )}
                            </div>
                        ))}
                    </div>
                </div>

                {/* Main content and REPL split */}
                <div className="flex-1 grid md:grid-cols-2 divide-x divide-zinc-700/50 overflow-hidden">
                    {/* Content Area */}
                    <div className="overflow-auto p-8 bg-zinc-900 animate-fadeSlideIn">
                        <div className="flex items-center gap-2 mb-6">
                            <h1 className="text-2xl font-semibold text-white">
                                {selectedSection.title}
                            </h1>
                            <DifficultyBadge difficulty={selectedSection.difficulty} />
                        </div>
                        <div className="prose prose-invert max-w-none text-zinc-300">
                            {selectedSection.content}
                        </div>
                        {selectedSection.code && (
                            <div className="mt-6 animate-fadeScale animation-delay-200">
                                <h3 className="text-lg font-medium text-white mb-2">Try it out:</h3>
                                <Card className="bg-zinc-800 border-zinc-700 overflow-hidden">
                                    <CardContent className="p-4">
                                        <div className="font-mono text-sm text-zinc-100 overflow-x-auto">
                                            <HighlightedText text={selectedSection.code} type="input" />
                                        </div>
                                    </CardContent>
                                </Card>
                                <Button
                                    className="mt-4 bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white hover:-translate-y-1 transition-all duration-200 flex items-center gap-2"
                                    onClick={handleRunExample}
                                    disabled={jawsInterpreter.loading}
                                >
                                    {jawsInterpreter.loading ? 'Loading Interpreter...' : 'Run Example'} <ArrowRight className="h-4 w-4" />
                                </Button>
                            </div>
                        )}
                    </div>

                    {/* REPL */}
                    <div className="bg-zinc-900 border-l border-zinc-700/50 overflow-hidden animate-fadeScale animation-delay-300">
                        {jawsInterpreter.loading ? (
                            <div className="flex items-center justify-center h-full text-zinc-400">
                                <p>Loading JAWS interpreter...</p>
                            </div>
                        ) : jawsInterpreter.error ? (
                            <div className="flex items-center justify-center h-full text-red-400">
                                <p>Error loading interpreter: {jawsInterpreter.error}</p>
                            </div>
                        ) : (
                            <Terminal ref={terminalRef} onCommand={handleCommand} className="h-full" />
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
}

export default LearnScheme;

import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import { githubDark } from '@ddietr/codemirror-themes/github-dark';
import { Prec } from '@codemirror/state';
import { keymap } from '@codemirror/view';
import { autocompletion, CompletionContext, CompletionResult } from '@codemirror/autocomplete';

const schemeCompletions = [
    { label: "define", type: "keyword", info: "Define a variable or function" },
    { label: "define-syntax", type: "keyword", info: "Define a macro" },
    { label: "syntax-rules", type: "keyword", info: "Define macro transformation rules" },
    { label: "lambda", type: "keyword", info: "Create an anonymous function" },
    { label: "let", type: "keyword", info: "Create local bindings" },
    { label: "let*", type: "keyword", info: "Create sequential local bindings" },
    { label: "letrec", type: "keyword", info: "Create recursive local bindings" },
    { label: "let-values", type: "keyword", info: "Bind multiple values" },
    { label: "if", type: "keyword", info: "Conditional expression" },
    { label: "cond", type: "keyword", info: "Multiple conditional expression" },
    { label: "case", type: "keyword", info: "Pattern matching on values" },
    { label: "begin", type: "keyword", info: "Sequence of expressions" },
    { label: "quote", type: "keyword", info: "Quote an expression" },
    { label: "when", type: "keyword", info: "Conditional with multiple expressions" },
    { label: "unless", type: "keyword", info: "Negative conditional" },
    { label: "import", type: "keyword", info: "Import bindings from a module" },
    { label: "base", type: "keyword", info: "Base library module" },
    { label: "list", type: "keyword", info: "List operations module" },
    { label: "register-function", type: "function", info: "Register an external function" },
    { label: "load-library", type: "function", info: "Load an external library" },

    { label: "eq?", type: "function", info: "Test object identity" },
    { label: "eqv?", type: "function", info: "Test object equivalence" },
    { label: "equal?", type: "function", info: "Deep equality comparison" },

    { label: "list", type: "function", info: "Create a new list" },
    { label: "car", type: "function", info: "Get the first element of a list" },
    { label: "cdr", type: "function", info: "Get the rest of the list" },
    { label: "cons", type: "function", info: "Construct a new list" },
    { label: "append", type: "function", info: "Join lists together" },
    { label: "reverse", type: "function", info: "Reverse a list" },
    { label: "length", type: "function", info: "Get length of a list" },
    { label: "list-ref", type: "function", info: "Get element at index" },
    { label: "list-tail", type: "function", info: "Get sublist starting at index" },
    { label: "list-set!", type: "function", info: "Set element at index" },
    { label: "map", type: "function", info: "Apply function to list elements" },

    { label: "cadr", type: "function", info: "Second element" },
    { label: "cddr", type: "function", info: "Rest after second element" },
    { label: "caddr", type: "function", info: "Third element" },
    { label: "cdddr", type: "function", info: "Rest after third element" },
    { label: "cadddr", type: "function", info: "Fourth element" },

    { label: "+", type: "function", info: "Addition" },
    { label: "-", type: "function", info: "Subtraction" },
    { label: "*", type: "function", info: "Multiplication" },
    { label: "/", type: "function", info: "Division" },
    { label: "=", type: "function", info: "Numeric equality" },
    { label: "<", type: "function", info: "Less than" },
    { label: ">", type: "function", info: "Greater than" },
    { label: "<=", type: "function", info: "Less than or equal" },
    { label: ">=", type: "function", info: "Greater than or equal" },
    { label: "zero?", type: "function", info: "Test if number is zero" },

    { label: "number?", type: "function", info: "Check if value is a number" },
    { label: "string?", type: "function", info: "Check if value is a string" },
    { label: "symbol?", type: "function", info: "Check if value is a symbol" },
    { label: "list?", type: "function", info: "Check if value is a list" },
    { label: "pair?", type: "function", info: "Check if value is a pair" },
    { label: "null?", type: "function", info: "Check if list is empty" },
    { label: "procedure?", type: "function", info: "Check if value is a procedure" },
    { label: "boolean?", type: "function", info: "Check if value is a boolean" },
    { label: "port?", type: "function", info: "Check if value is a port" },

    { label: "string-append", type: "function", info: "Join strings together" },
    { label: "string-length", type: "function", info: "Get length of string" },
    { label: "string=?", type: "function", info: "Compare strings for equality" },
    { label: "string<?", type: "function", info: "String less than" },
    { label: "string>?", type: "function", info: "String greater than" },
    { label: "string-ci=?", type: "function", info: "Case-insensitive string equality" },
    { label: "substring", type: "function", info: "Get part of string" },
    { label: "string-ref", type: "function", info: "Get character at index" },
    { label: "string->list", type: "function", info: "Convert string to list" },
    { label: "list->string", type: "function", info: "Convert list to string" },
    { label: "string-copy", type: "function", info: "Copy string" },
    { label: "string-upcase", type: "function", info: "Convert to uppercase" },
    { label: "string-downcase", type: "function", info: "Convert to lowercase" },

    { label: "make-vector", type: "function", info: "Create vector of given size" },
    { label: "vector", type: "function", info: "Create vector from elements" },
    { label: "vector-ref", type: "function", info: "Get vector element" },
    { label: "vector-set!", type: "function", info: "Set vector element" },
    { label: "vector-length", type: "function", info: "Get vector length" },
    { label: "vector-copy", type: "function", info: "Copy vector" },
    { label: "vector-copy!", type: "function", info: "Copy into vector" },
    { label: "vector-fill!", type: "function", info: "Fill vector with value" },

    { label: "display", type: "function", info: "Print a value" },
    { label: "newline", type: "function", info: "Print a newline" },
    { label: "read", type: "function", info: "Read an S-expression" },
    { label: "write", type: "function", info: "Write an S-expression" },
    { label: "open-input-file", type: "function", info: "Open file for reading" },
    { label: "open-output-file", type: "function", info: "Open file for writing" },
    { label: "close-port", type: "function", info: "Close a port" },
    { label: "load-library", type: "function", info: "Load external library" },
    { label: "error", type: "function", info: "Signal an error" },

    // Control flow and higher-order functions
    { label: "eval", type: "function", info: "Evaluate expression" },
    { label: "apply", type: "function", info: "Apply function to arguments" },
    { label: "call/cc", type: "function", info: "Call with current continuation" },
    { label: "values", type: "function", info: "Return multiple values" },

    // Logical operations
    { label: "and", type: "keyword", info: "Logical AND" },
    { label: "or", type: "keyword", info: "Logical OR" },
    { label: "not", type: "function", info: "Logical NOT" },

    // Loop constructs
    { label: "while", type: "keyword", info: "While loop" },
    { label: "for", type: "keyword", info: "For loop" },
    { label: "repeat", type: "keyword", info: "Repeat n times" },
    { label: "until", type: "keyword", info: "Until loop" },
    { label: "do-while", type: "keyword", info: "Do-while loop" },
    { label: "for-each-with-index", type: "keyword", info: "Iterate with index" },
    { label: "for-range", type: "keyword", info: "Range-based loop" },
    { label: "iterate", type: "keyword", info: "Iterate with step" },
    { label: "nested-loop", type: "keyword", info: "Nested loop construct" }
];

function schemeComplete(context: CompletionContext): CompletionResult | null {
    let word = context.matchBefore(/[\w\-+*/<>=?!]+/);
    if (!word) return null;

    return {
        from: word.from,
        options: schemeCompletions,
        validFor: /^[\w\-+*/<>=?!]*$/
    };
}

interface CodeEditorProps {
    value: string;
    onChange: (value: string) => void;
    height?: string;
    fullEditor?: boolean;
    minHeight?: string;
    maxHeight?: string;
    onRun?: () => void;
}

export const CodeEditor = ({
    value,
    onChange,
    height = "200px",
    fullEditor = false,
    minHeight = "24px",
    maxHeight = "150px",
    onRun
}: CodeEditorProps) => {
    const runKeymap = keymap.of([
        {
            key: "Shift-Enter",
            run: () => {
                onRun?.();
                return true;
            }
        }
    ]);

    return (
        <CodeMirror
            value={value}
            height={height}
            minHeight={fullEditor ? undefined : minHeight}
            maxHeight={fullEditor ? undefined : maxHeight}
            theme={githubDark}
            extensions={[
                StreamLanguage.define(scheme),
                Prec.highest(runKeymap),
                autocompletion({
                    override: [schemeComplete],
                    defaultKeymap: true
                })
            ]}
            onChange={onChange}
            className={`border-none w-full 
                [&_.cm-editor]:bg-transparent 
                [&_.cm-focused]:outline-none 
                [&_.cm-gutters]:bg-transparent 
                [&_.cm-gutters]:border-none 
                [&_.cm-line]:bg-transparent 
                [&_.cm-activeLine]:bg-transparent 
                [&_.cm-activeLineGutter]:bg-transparent 
                [&_.cm-scroller]:bg-transparent
                [&_.cm-editor]:bg-zinc-900
                [&_.cm-content]:bg-zinc-900
                ${!fullEditor ? '[&_.cm-editor]:min-h-[24px]' : ''}

                /* Autocomplete styling */
                [&_.cm-tooltip]:bg-zinc-800
                [&_.cm-tooltip]:border
                [&_.cm-tooltip]:border-zinc-700
                [&_.cm-tooltip]:rounded-md
                [&_.cm-tooltip]:shadow-lg
                [&_.cm-tooltip-autocomplete]:font-mono
                [&_.cm-tooltip-autocomplete]:text-base
                [&_.cm-tooltip-autocomplete]:p-1
                [&_.cm-tooltip-autocomplete_ul]:max-h-[300px]

                /* Completion item styling */
                [&_.cm-completionLabel]:text-zinc-100
                [&_.cm-completionLabel]:font-mono
                [&_.cm-completionMatchedText]:text-[#dd3f0c]
                [&_.cm-completionMatchedText]:font-medium
                [&_.cm-completionDetail]:text-zinc-400
                [&_.cm-completionDetail]:font-mono
                [&_.cm-completionInfo]:bg-zinc-800
                [&_.cm-completionInfo]:border-zinc-700
                [&_.cm-completionInfo]:shadow-lg
                [&_.cm-completionInfo]:p-3
                [&_.cm-completionInfo]:text-zinc-100
                [&_.cm-completionInfo]:font-mono

                /* Selected completion item */
                [&_.cm-tooltip-autocomplete_.cm-completionIcon]:hidden
                [&_.cm-tooltip-autocomplete_li]:px-2
                [&_.cm-tooltip-autocomplete_li]:py-1
                [&_.cm-tooltip-autocomplete_li]:hover:bg-zinc-700/50
                [&_.cm-completionMatchedText]:bg-transparent

                /* Active completion item */
                [&_.cm-tooltip-autocomplete_li[aria-selected]]:bg-zinc-700
                [&_.cm-tooltip-autocomplete_li[aria-selected]_.cm-completionLabel]:text-white
                [&_.cm-tooltip-autocomplete_li[aria-selected]_.cm-completionMatchedText]:text-[#dd3f0c]
                [&_.cm-tooltip-autocomplete_li[aria-selected]_.cm-completionDetail]:text-zinc-300
            `}
            basicSetup={{
                lineNumbers: fullEditor,
                highlightActiveLineGutter: fullEditor,
                highlightSpecialChars: true,
                history: true,
                foldGutter: fullEditor,
                drawSelection: true,
                dropCursor: true,
                allowMultipleSelections: fullEditor,
                indentOnInput: true,
                bracketMatching: true,
                closeBrackets: true,
                autocompletion: true,
                rectangularSelection: fullEditor,
                crosshairCursor: fullEditor,
                highlightActiveLine: fullEditor,
                highlightSelectionMatches: fullEditor,
                closeBracketsKeymap: true,
                defaultKeymap: true,
                searchKeymap: fullEditor,
                historyKeymap: true,
                foldKeymap: fullEditor,
                completionKeymap: true,
                lintKeymap: fullEditor,
            }}
        />
    );
};

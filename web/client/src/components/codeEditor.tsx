import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import { githubDark } from '@ddietr/codemirror-themes/github-dark';

interface CodeEditorProps {
    value: string;
    onChange: (value: string) => void;
    height?: string;
    fullEditor?: boolean;
    minHeight?: string;
    maxHeight?: string;
}

export const CodeEditor = ({
    value,
    onChange,
    height = "200px",
    fullEditor = false,
    minHeight = "24px",
    maxHeight = "150px"
}: CodeEditorProps) => {
    return (
        <CodeMirror
            value={value}
            height={height}
            minHeight={fullEditor ? undefined : minHeight}
            maxHeight={fullEditor ? undefined : maxHeight}
            theme={githubDark}
            extensions={[StreamLanguage.define(scheme)]}
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

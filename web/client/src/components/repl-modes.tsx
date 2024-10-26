import { useState, useRef, useEffect, useImperativeHandle, KeyboardEvent, forwardRef } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import {
    Dialog,
    DialogContent,
    DialogHeader,
    DialogTitle,
} from "@/components/ui/dialog";
import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import { githubDark } from '@ddietr/codemirror-themes/github-dark'
import { Play, Maximize2, Minimize2 } from 'lucide-react';
import hljs from 'highlight.js/lib/core';
import 'highlight.js/styles/github-dark-dimmed.css';

interface ReplModesProps {
    onCommand: (command: string) => Promise<string>;
    mode: 'repl' | 'editor';
    initialInput?: string;
}

interface TerminalLine {
    type: 'input' | 'output' | 'system';
    content: string;
}

export interface TerminalRef {
    writeOutput: (content: string) => void;
    writeSystem: (content: string) => void;
}

interface TerminalProps {
    onCommand: (command: string) => Promise<string>;
    onInputChange?: (input: string) => void;
    currentInput?: string;
    className?: string;
}

const HighlightedText = ({ text, type }: { text: string; type: 'input' | 'output' | 'system' }) => {
    const ref = useRef<HTMLPreElement>(null);

    useEffect(() => {
        if (ref.current && (type === 'input' || type === 'output')) {
            hljs.highlightElement(ref.current);
        }
    }, [text, type]);

    if (type === 'system') {
        return <pre className="text-teal-400 font-mono opacity-80">{text}</pre>;
    }

    return (
        <pre
            ref={ref}
            className="hljs language-scheme font-mono whitespace-pre-wrap"
            style={{ background: 'transparent' }}
        >
            {type === 'input' ? '❯ ' : ''}{text}
        </pre>
    );
};

const LiveEditor = ({ value, onChange }: { value: string; onChange: (value: string) => void }) => {
    return (
        <div className="flex items-center gap-2 text-sm">
            <span className="text-blue-400">❯</span>
            <div className="flex-1 relative">
                <CodeMirror
                    value={value}
                    theme={githubDark}
                    extensions={[StreamLanguage.define(scheme)]}
                    onChange={onChange}
                    placeholder="Wrangle some Scheme..."
                    className="min-h-[24px] [&_.cm-editor]:bg-transparent [&_.cm-focused]:outline-none [&_.cm-line]:pl-0 [&_.cm-gutters]:bg-transparent [&_.cm-gutters]:border-none [&_.cm-content]:whitespace-pre [&_.cm-line]:bg-transparent [&_.cm-activeLine]:bg-transparent [&_.cm-activeLineGutter]:bg-transparent"
                    basicSetup={{
                        lineNumbers: false,
                        foldGutter: false,
                        dropCursor: false,
                        allowMultipleSelections: false,
                        indentOnInput: true,
                        bracketMatching: true,
                        closeBrackets: true,
                        autocompletion: true,
                        highlightActiveLine: false,
                        highlightSelectionMatches: false,
                        closeBracketsKeymap: true,
                        defaultKeymap: true,
                        searchKeymap: false,
                        historyKeymap: true,
                        foldKeymap: false,
                        completionKeymap: true,
                        lintKeymap: false,
                    }}
                />
            </div>
        </div>
    );
};

const Terminal = forwardRef<TerminalRef, TerminalProps>(
    ({ onCommand, onInputChange, currentInput, className = "h-[400px]" }, ref) => {
        const [lines, setLines] = useState<TerminalLine[]>([]);
        const [inputValue, setInputValue] = useState('');
        const terminalRef = useRef<HTMLDivElement>(null);

        useEffect(() => {
            if (terminalRef.current) {
                terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
            }
        }, [lines]);

        useEffect(() => {
            if (currentInput !== undefined) {
                setInputValue(currentInput);
            }
        }, [currentInput]);

        const writeOutput = (content: string) => {
            setLines(prev => [...prev, { type: 'output', content }]);
        };

        const writeSystem = (content: string) => {
            setLines(prev => [...prev, { type: 'system', content }]);
        };

        useImperativeHandle(ref, () => ({
            writeOutput,
            writeSystem
        }));

        const handleInputChange = (value: string) => {
            setInputValue(value);
            onInputChange?.(value);
        };

        const handleKeyDown = async (e: KeyboardEvent<HTMLDivElement>) => {
            if (e.key === 'Enter' && !e.shiftKey) {
                const openCount = (inputValue.match(/\(/g) || []).length;
                const closeCount = (inputValue.match(/\)/g) || []).length;

                if (openCount !== closeCount) {
                    return; // Don't submit if parentheses aren't balanced
                }

                const cleanedInput = inputValue
                    .split('\n')
                    .map(line => line.trim())
                    .join(' ')
                    .trim();

                if (!cleanedInput) return;

                e.preventDefault();
                setLines(prev => [...prev, { type: 'input', content: inputValue }]);

                try {
                    const result = await onCommand(cleanedInput);
                    writeOutput(result);
                    setInputValue('');
                    onInputChange?.('');
                } catch (err) {
                    const error = err as Error;
                    writeSystem(`Error: ${error.message}`);
                }
            }
        };

        return (
            <div className={`h-full flex flex-col bg-zinc-900 ${className}`}>
                <div
                    ref={terminalRef}
                    className="flex-1 p-4 overflow-y-auto font-mono text-sm"
                >
                    {lines.map((line, i) => (
                        <div key={i} className="mb-2">
                            <HighlightedText text={line.content} type={line.type} />
                        </div>
                    ))}
                </div>
                <div
                    className="p-4 border-t border-zinc-700"
                    onKeyDown={handleKeyDown}
                >
                    <LiveEditor
                        value={currentInput ?? inputValue}
                        onChange={handleInputChange}
                    />
                </div>
            </div>
        );
    }
);

Terminal.displayName = 'Terminal';

export const IntegratedRepl = forwardRef<TerminalRef, ReplModesProps>(
    ({ onCommand, mode, initialInput }, ref) => {
        const [code, setCode] = useState('');
        const [replInput, setReplInput] = useState(initialInput || '');
        const [isEditorFullscreen, setIsEditorFullscreen] = useState(false);
        const [isReplFullscreen, setIsReplFullscreen] = useState(false);
        const [isFullscreen, setIsFullscreen] = useState(false);
        const terminalRef = useRef<TerminalRef>(null);

        useEffect(() => {
            if (initialInput !== undefined) {
                setReplInput(initialInput);
            }
        }, [initialInput]);

        useImperativeHandle(ref, () => ({
            writeOutput: (content: string) => {
                terminalRef.current?.writeOutput(content);
            },
            writeSystem: (content: string) => {
                terminalRef.current?.writeSystem(content);
            }
        }));

        const handleRun = async () => {
            if (!code.trim()) return;

            terminalRef.current?.writeSystem("Loaded up your code ready to go");
            try {
                const result = await onCommand(code);
                terminalRef.current?.writeOutput(result);
            } catch (err) {
                const error = err as Error;
                terminalRef.current?.writeSystem(`Error: ${error.message}`);
            }
        };

        const handleReplRun = async () => {
            if (!replInput.trim()) return;

            try {
                const result = await onCommand(replInput);
                terminalRef.current?.writeOutput(result);
                setReplInput('');
            } catch (err) {
                const error = err as Error;
                terminalRef.current?.writeSystem(`Error: ${error.message}`);
            }
        };

        const handleToggleFullscreen = () => {
            setIsFullscreen(!isFullscreen);
        };

        const EditorContent = (
            <CodeMirror
                value={code}
                height={isEditorFullscreen ? "calc(100vh - 150px)" : "200px"}
                theme={githubDark}
                extensions={[StreamLanguage.define(scheme)]}
                onChange={(value) => setCode(value)}
                className="border-none [&_.cm-focused]:outline-none"
                basicSetup={{
                    lineNumbers: true,
                    highlightActiveLineGutter: true,
                    highlightSpecialChars: true,
                    history: true,
                    foldGutter: true,
                    drawSelection: true,
                    dropCursor: true,
                    allowMultipleSelections: true,
                    indentOnInput: true,
                    bracketMatching: true,
                    closeBrackets: true,
                    autocompletion: true,
                    rectangularSelection: true,
                    crosshairCursor: true,
                    highlightActiveLine: true,
                    highlightSelectionMatches: true,
                    closeBracketsKeymap: true,
                    defaultKeymap: true,
                    searchKeymap: true,
                    historyKeymap: true,
                    foldKeymap: true,
                    completionKeymap: true,
                    lintKeymap: true,
                }}
            />
        );

        const ReplContent = (
            <Terminal
                ref={terminalRef}
                onCommand={onCommand}
                onInputChange={setReplInput}
                currentInput={replInput}
                className={isReplFullscreen ? "h-[calc(100vh-120px)]" : "h-[300px]"}
            />
        );
        return (
            <div className={`space-y-4 ${isFullscreen ? 'fixed inset-0 z-50 bg-zinc-900 overflow-auto' : ''}`}>
                {mode === 'editor' && (
                    <>
                        <Card className="border-zinc-700 bg-zinc-900">
                            <CardHeader className="border-b border-zinc-700">
                                <div className="flex items-center justify-between">
                                    <CardTitle className="text-zinc-200">Jaws Editor</CardTitle>
                                    <div className="flex items-center gap-2">
                                        <Button
                                            variant="default"
                                            size="icon"
                                            onClick={() => setIsEditorFullscreen(true)}
                                        >
                                            <Maximize2 className="h-4 w-4" />
                                        </Button>
                                        <Button
                                            variant="default"
                                            size="sm"
                                            onClick={handleRun}
                                            className="flex items-center gap-2"
                                        >
                                            <Play className="h-4 w-4" />
                                            Run Program
                                        </Button>
                                    </div>
                                </div>
                            </CardHeader>
                            <CardContent className="p-0">
                                {EditorContent}
                            </CardContent>
                        </Card>

                        <Dialog open={isEditorFullscreen} onOpenChange={setIsEditorFullscreen}>
                            <DialogContent className="max-w-[90vw] h-[90vh]">
                                <DialogHeader>
                                    <DialogTitle>Editor</DialogTitle>
                                </DialogHeader>
                                <div className="flex-1 overflow-hidden">
                                    {EditorContent}
                                </div>
                                <div className="flex justify-end">
                                    <Button onClick={handleRun} className="flex items-center gap-2">
                                        <Play className="h-4 w-4" />
                                        Run Program
                                    </Button>
                                </div>
                            </DialogContent>
                        </Dialog>
                    </>
                )}

                <Card className="border-zinc-700 bg-zinc-900">
                    <CardHeader className="border-b border-zinc-700">
                        <div className="flex items-center justify-between">
                            <div>
                                <CardTitle className="text-zinc-200">Jaws REPL</CardTitle>
                                <CardDescription className="text-zinc-400">
                                    Enter Scheme expressions to evaluate
                                </CardDescription>
                            </div>
                            <div className="flex items-center gap-2">
                                <Button
                                    variant="default"
                                    size="icon"
                                    className=''
                                    onClick={handleToggleFullscreen}
                                >
                                    {isFullscreen ? <Minimize2 className="h-4 w-4" /> : <Maximize2 className="h-4 w-4" />}
                                </Button>
                                <Button
                                    variant="default"
                                    size="sm"
                                    onClick={handleReplRun}
                                    className="flex items-center gap-2"
                                >
                                    <Play className="h-4 w-4" />
                                    Run
                                </Button>
                            </div>
                        </div>
                    </CardHeader>
                    <CardContent className="p-0">
                        {ReplContent}
                    </CardContent>
                </Card>

                <Dialog open={isReplFullscreen} onOpenChange={setIsReplFullscreen}>
                    <DialogContent className="max-w-[90vw] h-[90vh]">
                        <DialogHeader>
                            <DialogTitle>REPL</DialogTitle>
                        </DialogHeader>
                        <div className="flex-1 overflow-hidden">
                            {ReplContent}
                        </div>
                    </DialogContent>
                </Dialog>
            </div>
        );
    }
);

IntegratedRepl.displayName = 'IntegratedRepl';

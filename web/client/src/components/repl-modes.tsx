import { useState, useRef } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import Terminal, { TerminalRef } from './terminal';
import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import { xcodeDark } from '@uiw/codemirror-theme-xcode';

interface ReplModesProps {
    onCommand: (command: string) => Promise<string>;
    mode: 'repl' | 'editor';
}

export function IntegratedRepl({ onCommand, mode }: ReplModesProps) {
    const [code, setCode] = useState('');
    const terminalRef = useRef<TerminalRef>(null);

    const handleRun = async () => {
        if (!code.trim()) return;

        terminalRef.current?.writeSystem("|> Dunu Dunu Dununununu");
        try {
            const result = await onCommand(code);
            terminalRef.current?.writeOutput(result);
        } catch (err) {
            const error = err as Error;
            terminalRef.current?.writeSystem(`Error: ${error.message}`);
        }
    };

    return (
        <div className="space-y-4">
            {mode === 'editor' && (
                <Card className="border-zinc-700 bg-zinc-900">
                    <CardHeader className="border-b border-zinc-700">
                        <div className="flex items-center justify-between">
                            <CardTitle className="text-zinc-200">Jaws Editor</CardTitle>
                            <Button
                                variant="default"
                                size="sm"
                                onClick={handleRun}
                            >
                                Run Program
                            </Button>
                        </div>
                    </CardHeader>
                    <CardContent className="p-0">
                        <CodeMirror
                            value={code}
                            height="200px"
                            theme={xcodeDark}
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
                    </CardContent>
                </Card>
            )}

            <Card className="border-zinc-700 bg-zinc-900">
                <CardHeader className="border-b border-zinc-700">
                    <CardTitle className="text-zinc-200">Jaws REPL</CardTitle>
                    <CardDescription className="text-zinc-400">
                        Enter Scheme expressions to evaluate
                    </CardDescription>
                </CardHeader>
                <CardContent className="p-0 h-[400px]">
                    <Terminal
                        ref={terminalRef}
                        onCommand={onCommand}
                    />
                </CardContent>
            </Card>
        </div>
    );
}

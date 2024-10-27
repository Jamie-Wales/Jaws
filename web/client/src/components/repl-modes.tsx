import { useState, useRef, useEffect, forwardRef, useImperativeHandle } from 'react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Dialog, DialogContent, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Play, Maximize2, Minimize2 } from 'lucide-react';
import { Terminal, TerminalRef } from './terminal';
import { CodeEditor } from './codeEditor';

interface ReplModesProps {
    onCommand: (command: string) => Promise<string>;
    mode: 'repl' | 'editor';
    initialInput?: string;
    ref?: React.ForwardedRef<TerminalRef>;
}

export const ReplModes = forwardRef<TerminalRef, ReplModesProps>(
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
            <CodeEditor
                value={code}
                onChange={setCode}
                height={isEditorFullscreen ? "calc(100vh - 150px)" : "200px"}
                fullEditor={true}
            />
        );

        const ReplContent = (
            <Terminal
                ref={terminalRef}
                onCommand={onCommand}
                onInputChange={setReplInput}
                currentInput={replInput}
                className={isReplFullscreen ? "h-[calc(100vh-120px)]" : undefined}
            />
        );

        return (
            <div className={`space-y-4 ${isFullscreen ? 'fixed inset-0 z-50 bg-zinc-900 overflow-auto p-4' : ''}`}>
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
                                <div className="flex justify-end gap-2 pt-2">
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

ReplModes.displayName = 'ReplModes';

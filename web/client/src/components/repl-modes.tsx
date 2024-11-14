import { useState, useRef, forwardRef, useImperativeHandle } from 'react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Maximize2, Minimize2, Download } from 'lucide-react';
import { Terminal, TerminalRef } from './terminal';
import { CodeEditor } from './codeEditor';

const buttonStyles = {
    backgroundColor: '#dd3f0c',
    color: 'white',
};

interface ReplModesProps {
    onCommand: (command: string) => Promise<string>;
    mode: 'repl' | 'editor';
    initialInput?: string;
}

export const ReplModes = forwardRef<TerminalRef, ReplModesProps>(
    ({ onCommand, mode, initialInput }, ref) => {
        const [code, setCode] = useState('');
        const [isFullscreen, setIsFullscreen] = useState(false);
        const terminalRef = useRef<TerminalRef>(null);

        useImperativeHandle(ref, () => ({
            writeOutput: (content: string) => {
                terminalRef.current?.writeOutput(content);
            },
            writeSystem: (content: string) => {
                terminalRef.current?.writeSystem(content);
            },
            getCurrentInput: () => {
                return terminalRef.current?.getCurrentInput() || '';
            },
            clearInput: () => {
                terminalRef.current?.clearInput();
            },
            setInput: (input: string) => {
                terminalRef.current?.setInput(input);
            }
        }));

        const handleToggleFullscreen = () => {
            setIsFullscreen(!isFullscreen);
        };

        const handleLoadCode = async () => {
            if (!code.trim()) return;
            terminalRef.current?.writeSystem("Loading code into Scheme environment...");
            try {
                const result = await onCommand(code);
                terminalRef.current?.writeOutput(result);
            } catch (err) {
                terminalRef.current?.writeSystem(`Error: ${(err as Error).message}`);
            }
        };

        const containerClasses = isFullscreen
            ? 'fixed inset-0 z-50 bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 overflow-auto p-4'
            : 'space-y-4';

        return (
            <div className={containerClasses}>
                {mode === 'editor' && (
                    <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white">
                        <CardHeader className="border-b border-slate-700/50">
                            <div className="flex items-center justify-between">
                                <div className="space-y-1">
                                    <CardTitle className="text-zinc-200">Jaws Editor</CardTitle>
                                    <CardDescription className="text-slate-400">
                                        Write and load Scheme programs
                                    </CardDescription>
                                </div>
                                <div className="flex items-center gap-2">
                                    <Button
                                        variant="ghost"
                                        size="icon"
                                        onClick={handleToggleFullscreen}
                                        className="text-slate-300 hover:text-white hover:bg-slate-800/60"
                                    >
                                        {isFullscreen ?
                                            <Minimize2 className="h-4 w-4" /> :
                                            <Maximize2 className="h-4 w-4" />
                                        }
                                    </Button>
                                    <Button
                                        variant="default"
                                        size="sm"
                                        onClick={handleLoadCode}
                                        style={buttonStyles}
                                        className="hover:opacity-90"
                                    >
                                        <Download className="h-4 w-4 mr-2" />
                                        Load into REPL
                                    </Button>
                                </div>
                            </div>
                        </CardHeader>
                        <CardContent className="p-0">
                            <CodeEditor
                                value={code}
                                onChange={setCode}
                                height={isFullscreen ? "calc(100vh - 150px)" : "200px"}
                                fullEditor={true}
                            />
                        </CardContent>
                    </Card>
                )}

                <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white">
                    <CardHeader className="border-b border-slate-700/50">
                        <div className="flex items-center justify-between">
                            <div>
                                <CardTitle className="text-zinc-200">Jaws REPL</CardTitle>
                                <CardDescription className="text-slate-400">
                                    Enter Scheme expressions to evaluate
                                </CardDescription>
                            </div>
                            {mode !== 'editor' && (
                                <Button
                                    variant="ghost"
                                    size="icon"
                                    onClick={handleToggleFullscreen}
                                    className="text-slate-300 hover:text-white hover:bg-slate-800/60"
                                >
                                    {isFullscreen ?
                                        <Minimize2 className="h-4 w-4" /> :
                                        <Maximize2 className="h-4 w-4" />
                                    }
                                </Button>
                            )}
                        </div>
                    </CardHeader>
                    <CardContent className="p-0">
                        <Terminal
                            ref={terminalRef}
                            onCommand={onCommand}
                            currentInput={initialInput}
                            className={isFullscreen ? "h-[calc(100vh-120px)]" : undefined}
                        />
                    </CardContent>
                </Card>
            </div>
        );
    }
);

ReplModes.displayName = 'ReplModes';

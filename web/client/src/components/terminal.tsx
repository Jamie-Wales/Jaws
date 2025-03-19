import { useState, useRef, useEffect, forwardRef, useImperativeHandle } from 'react';
import { KeyboardEvent as ReactKeyboardEvent } from 'react';
import { LiveEditor } from './liveEditor';
import { HighlightedText } from './highlightedText';
import { Button } from '@/components/ui/button';
import { Play } from 'lucide-react';

export interface TerminalLine {
    type: 'input' | 'output' | 'system';
    content: string;
    timestamp: string;
    id: string;
}

export interface TerminalRef {
    writeOutput: (content: string) => void;
    writeSystem: (content: string) => void;
    writeInput: (content: string) => void;
    getCurrentInput: () => string;
    clearInput: () => void;
    setInput: (input: string) => void;
    clear: () => void;
}

export interface TerminalProps {
    onCommand: (command: string) => Promise<string>;
    onInputChange?: (input: string) => void;
    currentInput?: string;
    className?: string;
}

export const Terminal = forwardRef<TerminalRef, TerminalProps>(
    ({ onCommand, onInputChange, className }, ref) => {
        const [lines, setLines] = useState<TerminalLine[]>([]);
        const [inputValue, setInputValue] = useState('');
        const [isProcessing, setIsProcessing] = useState(false);
        const terminalRef = useRef<HTMLDivElement>(null);
        const lastCommandId = useRef<string>('');

        const jawsLogo = `     ██╗ █████╗ ██╗    ██╗███████╗
     ██║██╔══██╗██║    ██║██╔════╝
     ██║███████║██║ █╗ ██║███████╗
██   ██║██╔══██║██║███╗██║╚════██║
╚█████╔╝██║  ██║╚███╔███╔╝███████║
 ╚════╝ ╚═╝  ╚═╝ ╚══╝╚══╝ ╚══════╝`;
        useEffect(() => {
            // Initialize with JAWS logo and welcome message
            setLines([
                {
                    type: 'system',
                    content: jawsLogo,
                    timestamp: new Date().toLocaleTimeString(),
                    id: generateId()
                },
                {
                    type: 'system',
                    content: 'Welcome to the Jaws REPL!',
                    timestamp: new Date().toLocaleTimeString(),
                    id: generateId()
                },
                {
                    type: 'system',
                    content: "Type 'exit' to quit, '(help)' for commands.\n",
                    timestamp: new Date().toLocaleTimeString(),
                    id: generateId()
                }
            ]);
        }, []);

        useEffect(() => {
            if (terminalRef.current) {
                terminalRef.current.scrollTop = terminalRef.current.scrollHeight;
            }
        }, [lines]);

        const generateId = () => `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

        const handleCommand = async (input: string) => {
            if (!input.trim() || isProcessing) return;
            setIsProcessing(true);
            const commandId = generateId();
            lastCommandId.current = commandId;

            try {
                const trimmedInput = input.trim();
                const result = await onCommand(trimmedInput);

                if (lastCommandId.current === commandId) {
                    setLines(prevLines => [
                        ...prevLines,
                        {
                            type: 'input',
                            content: trimmedInput,
                            timestamp: new Date().toLocaleTimeString(),
                            id: generateId()
                        }
                    ]);

                    if (result && result !== 'undefined') {
                        setLines(prevLines => [
                            ...prevLines,
                            {
                                type: 'output',
                                content: result,
                                timestamp: new Date().toLocaleTimeString(),
                                id: generateId()
                            }
                        ]);
                    }
                }
            } catch (err) {
                const error = err as Error;
                if (lastCommandId.current === commandId) {
                    setLines(prevLines => [
                        ...prevLines,
                        {
                            type: 'input',
                            content: input.trim(),
                            timestamp: new Date().toLocaleTimeString(),
                            id: generateId()
                        },
                        {
                            type: 'output',
                            content: `Error: ${error.message}`,
                            timestamp: new Date().toLocaleTimeString(),
                            id: generateId()
                        }
                    ]);
                }
            } finally {
                if (lastCommandId.current === commandId) {
                    setIsProcessing(false);
                    clearInput();
                }
            }
        };

        const getCurrentInput = () => inputValue;

        const clearInput = () => {
            setInputValue('');
            onInputChange?.('');
        };

        const setInput = (input: string) => {
            setInputValue(input);
            onInputChange?.(input);
        };

        const clear = () => {
            lastCommandId.current = generateId();
            setLines([
                {
                    type: 'system',
                    content: jawsLogo,
                    timestamp: new Date().toLocaleTimeString(),
                    id: generateId()
                },
                {
                    type: 'system',
                    content: 'Welcome to the Jaws REPL!',
                    timestamp: new Date().toLocaleTimeString(),
                    id: generateId()
                },
                {
                    type: 'system',
                    content: "Type 'exit' to quit, '(help)' for commands.\n",
                    timestamp: new Date().toLocaleTimeString(),
                    id: generateId()
                }
            ]);
        };

        useImperativeHandle(ref, () => ({
            writeOutput: (content: string) => {
                const commandId = generateId();
                lastCommandId.current = commandId;
                if (content && content !== 'undefined') {
                    setLines(prevLines => [
                        ...prevLines,
                        {
                            type: 'output',
                            content: content.trim(),
                            timestamp: new Date().toLocaleTimeString(),
                            id: generateId()
                        }
                    ]);
                }
            },
            writeSystem: (content: string) => {
                const commandId = generateId();
                lastCommandId.current = commandId;
                setLines(prevLines => [
                    ...prevLines,
                    {
                        type: 'system',
                        content,
                        timestamp: new Date().toLocaleTimeString(),
                        id: generateId()
                    }
                ]);
            },
            writeInput: (content: string) => {
                const commandId = generateId();
                lastCommandId.current = commandId;
                setLines(prevLines => [
                    ...prevLines,
                    {
                        type: 'input',
                        content,
                        timestamp: new Date().toLocaleTimeString(),
                        id: generateId()
                    }
                ]);
            },
            getCurrentInput,
            clearInput,
            setInput,
            clear
        }));

        const handleInputChange = (value: string) => {
            setInputValue(value);
            onInputChange?.(value);
        };

        const handleKeyDown = async (e: ReactKeyboardEvent<HTMLDivElement>) => {
            if (e.key === 'Enter' && !e.shiftKey) {
                e.preventDefault();
                e.stopPropagation();

                const openCount = (inputValue.match(/\(/g) || []).length;
                const closeCount = (inputValue.match(/\)/g) || []).length;

                if (openCount !== closeCount) return;

                const trimmedInput = inputValue.trim();
                if (!trimmedInput) return;

                await handleCommand(trimmedInput);
            }
        };

        return (
            <div className={`min-h-[500px] flex flex-col bg-zinc-900 transition-all duration-300 ${className}`}>
                <div
                    ref={terminalRef}
                    className="flex-1 min-h-[400px] p-4 overflow-y-auto code-font scrollbar-thin scrollbar-thumb-gray-600 scrollbar-track-transparent"
                >
                    {lines.map((line) => (
                        <div
                            key={line.id}
                            style={{
                                opacity: 0,
                                animation: `fadeSlideIn 0.3s ease-out forwards`,
                                animationDelay: `0.2s`
                            }}
                            className={`terminal-line ${line.type !== 'system' ? 'mb-2' : 'mb-0'} group`}
                        >
                            <div className={`flex items-start gap-2 rounded-lg ${line.type !== 'system' ? 'p-2' : 'p-0'} transition-all duration-300 ${line.type !== 'system' ? 'hover:bg-zinc-800/30' : ''}`}>
                                {line.type === 'input' && (
                                    <span className="text-blue-400 shrink-0 code-font" style={{
                                        opacity: 0,
                                        animation: 'slideFromLeft 0.3s ease-out forwards',
                                        animationDelay: `0.2s`
                                    }}>jaws: {"|>"}</span>
                                )}
                                <div className="flex-1 terminal-content code-font">
                                    <HighlightedText text={line.content} type={line.type} />
                                </div>
                                {line.type !== 'system' && (
                                    <span className="text-zinc-500 shrink-0 ml-2 opacity-40 code-font">
                                        {line.timestamp}
                                    </span>
                                )}
                            </div>
                        </div>
                    ))}
                </div>
                <div className="border-t border-zinc-700/50">
                    <div className="px-4 py-3 w-full">
                        <LiveEditor
                            onChange={handleInputChange}
                            value={inputValue}
                            onKeyDown={handleKeyDown}
                        />
                        <div className="mt-2 flex justify-end">
                            <Button
                                variant="default"
                                size="sm"
                                onClick={() => handleCommand(inputValue)}
                                className="flex items-center gap-2 transition-all duration-200 
                                 hover:scale-105 disabled:opacity-50 disabled:hover:scale-100
                                 bg-accent hover:bg-accent/90"
                                disabled={isProcessing}
                            >
                                <Play className={`h-4 w-4 ${isProcessing ? 'animate-spin' : ''}`} />
                                {isProcessing ? 'Running...' : 'Run'}
                            </Button>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
);

Terminal.displayName = 'Terminal';

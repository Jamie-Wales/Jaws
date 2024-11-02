import { useState, useRef, useEffect, forwardRef, useImperativeHandle, KeyboardEvent } from 'react';
import { LiveEditor } from './liveEditor';
import { HighlightedText } from './highlightedText';
import { Button } from '@/components/ui/button';
import { Play } from 'lucide-react';

export interface TerminalLine {
    type: 'input' | 'output' | 'system';
    content: string;
    timestamp: string;
}

export interface TerminalRef {
    writeOutput: (content: string) => void;
    writeSystem: (content: string) => void;
    getCurrentInput: () => string;
    clearInput: () => void;
    setInput: (input: string) => void;
}

export interface TerminalProps {
    onCommand: (command: string) => Promise<string>;
    onInputChange?: (input: string) => void;
    currentInput?: string;
    className?: string;
}

export const Terminal = forwardRef<TerminalRef, TerminalProps>(
    ({ onCommand, onInputChange, currentInput, className }, ref) => {
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


        const handleCommand = async (input: string) => {
            if (!input.trim()) return;

            const cleanedInput = input
                .split('\n')
                .map(line => line.trim())
                .join(' ')
                .trim();

            setLines(prev => [...prev, {
                type: 'input',
                content: cleanedInput,
                timestamp: new Date().toLocaleTimeString()
            }]);

            try {
                const result = await onCommand(cleanedInput);
                writeOutput(result);
            } catch (err) {
                const error = err as Error;
                writeSystem(`Error: ${error.message}`);
            } finally {
                clearInput();
            }

        };
        const writeOutput = (content: string) => {
            setLines(prev => [...prev, {
                type: 'output',
                content,
                timestamp: new Date().toLocaleTimeString()
            }]);

        };

        const writeSystem = (content: string) => {
            setLines(prev => [...prev, {
                type: 'system',
                content,
                timestamp: new Date().toLocaleTimeString()
            }]);
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

        useImperativeHandle(ref, () => ({
            writeOutput,
            writeSystem,
            getCurrentInput,
            clearInput,
            setInput
        }));

        const handleInputChange = (value: string) => {
            setInputValue(value);
            onInputChange?.(value);
        };

        const handleKeyDown = async (e: KeyboardEvent<HTMLDivElement>) => {
            if (e.key === 'Enter' && !e.shiftKey) {
                const openCount = (inputValue.match(/\(/g) || []).length;
                const closeCount = (inputValue.match(/\)/g) || []).length;
                if (openCount !== closeCount) return;
                e.preventDefault();
                await handleCommand(inputValue);
                clearInput();
            }
        };

        return (
            <div className={`min-h-[500px] flex flex-col bg-zinc-900 ${className}`}>
                <div
                    ref={terminalRef}
                    className="flex-1 min-h-[400px] p-4 overflow-y-auto font-mono text-sm"
                >
                    {lines.map((line, i) => (
                        <div key={i} className="mb-2">
                            <div className="flex items-start gap-2">
                                {line.type === 'input' && (
                                    <span className="text-blue-400 shrink-0">λ</span>
                                )}
                                <div className="flex-1">
                                    <HighlightedText text={line.content} type={line.type} />
                                </div>
                                <span className="text-zinc-500 text-xs shrink-0 ml-2">
                                    {line.timestamp}
                                </span>
                            </div>
                        </div>
                    ))}
                </div>
                <div className="border-t border-zinc-700">
                    <div
                        className="px-4 py-3 w-full"
                        onKeyDown={handleKeyDown}
                    >
                        <LiveEditor
                            onChange={handleInputChange}
                            value={inputValue}
                        />
                        <div className="mt-2 flex justify-end">
                            <Button
                                variant="default"
                                size="sm"
                                onClick={() => handleCommand(inputValue)}
                                className="flex items-center gap-2"
                            >
                                <Play className="h-4 w-4" />
                                Run
                            </Button>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
);

Terminal.displayName = 'Terminal';

import { useState, useRef, useEffect, forwardRef, useImperativeHandle, KeyboardEvent } from 'react';
import { LiveEditor } from './liveEditor';
import { HighlightedText } from './highlightedText';

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
            <div className={`min-h-[500px] flex flex-col bg-zinc-900 ${className}`}>
                <div
                    ref={terminalRef}
                    className="flex-1 min-h-[400px] p-4 overflow-y-auto font-mono text-sm"
                >
                    {lines.map((line, i) => (
                        <div key={i} className="mb-2">
                            <HighlightedText text={line.content} type={line.type} />
                        </div>
                    ))}
                </div>
                <div
                    className="px-4 py-3 border-t border-zinc-700 w-full"
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

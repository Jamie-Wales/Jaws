import React, { useState, useRef, useEffect } from 'react';
import { ScrollArea } from "@/components/ui/scroll-area";
import { Input } from "@/components/ui/input";

interface TerminalProps {
    onCommand: (command: string) => Promise<string>;
}

export interface TerminalRef {
    writeOutput: (text: string) => void;
    writeSystem: (text: string) => void;
    clear: () => void;
}

interface TerminalEntry {
    type: 'command' | 'system' | 'output';
    content: string;
    timestamp: string;
}

const Terminal = React.forwardRef<TerminalRef, TerminalProps>(({ onCommand }, ref) => {
    const [history, setHistory] = useState<TerminalEntry[]>([]);
    const [inputValue, setInputValue] = useState("");
    const scrollAreaRef = useRef<HTMLDivElement>(null);

    React.useImperativeHandle(ref, () => ({
        writeOutput: (text: string) => {
            setHistory(prev => [...prev, {
                type: 'output',
                content: text,
                timestamp: new Date().toLocaleTimeString()
            }]);
        },
        writeSystem: (text: string) => {
            setHistory(prev => [...prev, {
                type: 'system',
                content: text,
                timestamp: new Date().toLocaleTimeString()
            }]);
        },
        clear: () => {
            setHistory([]);
        }
    }));

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!inputValue.trim()) return;

        setHistory(prev => [...prev, {
            type: 'command',
            content: inputValue,
            timestamp: new Date().toLocaleTimeString()
        }]);

        try {
            const output = await onCommand(inputValue.trim());
            if (output) {
                setHistory(prev => [...prev, {
                    type: 'output',
                    content: output,
                    timestamp: new Date().toLocaleTimeString()
                }]);
            }
        } catch (error: any) {
            setHistory(prev => [...prev, {
                type: 'system',
                content: `Error: ${error.message}`,
                timestamp: new Date().toLocaleTimeString()
            }]);
        }

        setInputValue("");
    };

    useEffect(() => {
        if (scrollAreaRef.current) {
            const scrollArea = scrollAreaRef.current;
            scrollArea.scrollTop = scrollArea.scrollHeight;
        }
    }, [history]);

    return (
        <div className="w-full bg-zinc-900 rounded-lg border border-zinc-700 overflow-hidden">
            <div className="flex items-center px-4 py-2 bg-zinc-800 border-b border-zinc-700">
                <span className="text-lg mr-2 text-zinc-400">λ</span>
                <span className="text-sm font-medium text-zinc-200">Jaws REPL</span>
            </div>

            <ScrollArea
                ref={scrollAreaRef}
                className="h-[400px] p-4 font-mono text-sm"
            >
                {history.map((entry, index) => (
                    <div key={index} className="mb-2">
                        {entry.type === 'command' ? (
                            <div className="flex items-center text-zinc-400">
                                <span className="mr-2">λ</span>
                                <span className="text-zinc-200">{entry.content}</span>
                                <span className="ml-auto text-xs">{entry.timestamp}</span>
                            </div>
                        ) : (
                            <div className={`mt-1 whitespace-pre-wrap ${entry.type === 'system' ? 'text-yellow-300' : 'text-zinc-300'
                                }`}>
                                {entry.content}
                            </div>
                        )}
                    </div>
                ))}

                <form onSubmit={handleSubmit} className="flex items-center mt-2">
                    <span className="mr-2 text-zinc-400">λ</span>
                    <Input
                        value={inputValue}
                        onChange={(e) => setInputValue(e.target.value)}
                        className="flex-1 bg-transparent border-none text-zinc-200 focus-visible:ring-0 focus-visible:ring-offset-0 placeholder:text-zinc-600"
                        placeholder="Wrangle some Scheme"
                    />
                </form>
            </ScrollArea>
        </div>
    );
});

Terminal.displayName = "Terminal";

export default Terminal;

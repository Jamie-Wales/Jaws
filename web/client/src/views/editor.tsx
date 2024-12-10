import { useRef, useState, useEffect } from 'react';
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card';
import { Terminal, TerminalRef } from '@/components/terminal';
import { CodeEditor } from '@/components/codeEditor';
import { Button } from '@/components/ui/button';
import { Play } from 'lucide-react';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';

export function EditorView() {
    const [code, setCode] = useState('');
    const [isLoadedFromExample, setIsLoadedFromExample] = useState(false);
    const terminalRef = useRef<TerminalRef>(null);
    const interpreter = useJawsInterpreter();

    useEffect(() => {
        const savedCode = localStorage.getItem('editorCode');
        if (savedCode) {
            setCode(savedCode);
            setIsLoadedFromExample(true);
            localStorage.removeItem('editorCode');
            terminalRef.current?.writeSystem("Example code loaded! Click 'Run Code' to execute.");
        }
    }, []);

    const handleCommand = async (command: string) => {
        try {
            const result = interpreter.evaluate(command);
            return result; // Don't convert to string here, let Terminal component handle it
        } catch (error) {
            throw new Error(error instanceof Error ? error.message : 'An error occurred');
        }
    };

    const handleRunCode = async () => {
        if (!code.trim() || interpreter.loading) return;

        terminalRef.current?.clear();
        terminalRef.current?.writeSystem("Running code...");

        try {
            // Run the entire code as one unit first
            const fullResult = await handleCommand(code);

            if (fullResult !== undefined && fullResult !== 'undefined') {
                // Show the full input
                terminalRef.current?.writeInput(code);
                // Show the final result
                terminalRef.current?.writeOutput(String(fullResult));
            }

            if (isLoadedFromExample) {
                setIsLoadedFromExample(false);
            }
        } catch (err) {
            terminalRef.current?.writeInput(code);
            terminalRef.current?.writeSystem(`Error: ${(err as Error).message}`);
        }
    };

    return (
        <div className="space-y-6">
            <Card className="gradient-card">
                <CardHeader className="gradient-card-header">
                    <div className="flex flex-row items-center justify-between space-y-0">
                        <div>
                            <CardTitle className="gradient-title">Editor</CardTitle>
                            {isLoadedFromExample && (
                                <CardDescription className="gradient-description">
                                    Example code loaded - ready to run!
                                </CardDescription>
                            )}
                        </div>
                        <Button
                            onClick={handleRunCode}
                            className="primary-button"
                            disabled={interpreter.loading}
                        >
                            <Play className="h-4 w-4 mr-2" />
                            {interpreter.loading ? 'Initializing...' : 'Run Code'}
                        </Button>
                    </div>
                </CardHeader>
                <CardContent className="p-0">
                    <div className="p-4">
                        <CodeEditor
                            value={code}
                            onChange={setCode}
                            height="300px"
                            fullEditor={true}
                        />
                    </div>
                </CardContent>
            </Card>
            <Card className="gradient-card">
                <CardHeader className="gradient-card-header">
                    <CardTitle className="gradient-title">Output</CardTitle>
                    <CardDescription className="gradient-description">
                        See the results of your code execution
                    </CardDescription>
                </CardHeader>
                <CardContent className="p-0">
                    <Terminal
                        ref={terminalRef}
                        onCommand={handleCommand}
                        className="min-h-[300px]"
                    />
                </CardContent>
            </Card>
        </div>
    );
}

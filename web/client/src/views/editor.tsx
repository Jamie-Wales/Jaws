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
            return String(result);
        } catch (error) {
            throw new Error(error instanceof Error ? error.message : 'An error occurred');
        }
    };

    const handleRunCode = async () => {
        if (!code.trim()) return;
        terminalRef.current?.writeSystem("Loading code into Scheme environment...");
        try {
            const result = await handleCommand(code);
            terminalRef.current?.writeOutput(result);
            if (isLoadedFromExample) {
                setIsLoadedFromExample(false);
            }
        } catch (err) {
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
                        <Button onClick={handleRunCode} className="primary-button">
                            <Play className="h-4 w-4 mr-2" />
                            Load Code
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
                    <Terminal ref={terminalRef} onCommand={handleCommand} />
                </CardContent>
            </Card>
        </div>
    );
}

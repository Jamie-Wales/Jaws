import { useRef } from 'react';
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from '@/components/ui/card';
import { Terminal, TerminalRef } from '@/components/terminal';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';

export function ReplView() {
    const terminalRef = useRef<TerminalRef>(null);
    const interpreter = useJawsInterpreter();

    const handleCommand = async (command: string) => {
        try {
            const result = interpreter.evaluate(command);
            return String(result);
        } catch (error) {
            throw new Error(error instanceof Error ? error.message : 'An error occurred');
        }
    };


    return (
        <div className="space-y-6">
            <Card className="gradient-card">
                <CardHeader className="gradient-card-header">
                    <div className="flex flex-row items-center justify-between space-y-0">
                        <div>
                            <CardTitle className="gradient-title">Jaws REPL</CardTitle>
                            <CardDescription className="gradient-description">
                                Enter Scheme expressions to evaluate
                            </CardDescription>
                        </div>
                    </div>
                </CardHeader>
                <CardContent className="p-0">
                    <Terminal
                        ref={terminalRef}
                        onCommand={handleCommand}
                    />
                </CardContent>
            </Card>
        </div>
    );
}

import { useState, useRef } from 'react'
import useJawsInterpreter from './hooks/useJawsInterpreter'
import { Tabs, TabsList, TabsTrigger, TabsContent } from "@/components/ui/tabs"
import { Button } from "@/components/ui/button"
import { ReplModes } from '@/components/repl-modes'
import { GetStartedTab, ExamplesTab, DocumentationTab } from '@/components/tabs'
import type { TerminalRef } from '@/components/terminal'
import './styles/globals.css'
import { WelcomeHeader } from './components/welcome'

export const examples = [
    {
        name: "Hello World",
        code: '(display "Hello, World!")',
        description: "A simple greeting program"
    },
    {
        name: "Factorial",
        code: "(define factorial\n  (lambda (n)\n    (if (<= n 1)\n        1\n        (* n (factorial (- n 1))))))",
        description: "Recursive factorial calculation"
    },
    {
        name: "List Operations",
        code: "(define numbers '(1 2 3 4 5))\n(map (lambda (x) (* x x)) numbers)",
        description: "Working with lists and mapping"
    }
];

export interface Example {
    name: string;
    code: string;
    description: string;
}

function App() {
    const interpreter = useJawsInterpreter();
    const [mode, setMode] = useState<'repl' | 'editor'>('repl');
    const [activeTab, setActiveTab] = useState('repl');
    const [replInput, setReplInput] = useState('');
    const terminalRef = useRef<TerminalRef>(null);

    const handleCommand = async (command: string) => {
        try {
            const result = await interpreter.evaluate(command);
            return String(result);
        } catch (error) {
            throw new Error(error instanceof Error ? error.message : 'An error occurred');
        }
    };

    const handleTryExample = (code: string) => {
        setActiveTab('repl');
        setReplInput(code);
        terminalRef.current?.writeSystem("|> Example loaded and ready to run!");
    };

    return (
        <div className="min-h-screen w-full bg-background">
            <div className="container mx-auto px-4 sm:px-6 lg:px-8 py-8">
                <div className="text-center space-y-2">
                    <h1 className="text-4xl font-bold tracking-tight">Jaws Scheme Interpreter</h1>
                    <p className="text-muted-foreground">A modern Scheme REPL in your browser</p>
                    <div className="flex justify-center gap-4 pt-4">
                        <Button
                            variant={mode === 'repl' ? 'default' : 'outline'}
                            onClick={() => setMode('repl')}
                        >
                            REPL Only
                        </Button>
                        <Button
                            variant={mode === 'editor' ? 'default' : 'outline'}
                            onClick={() => setMode('editor')}
                        >
                            (+ Editor Repl)
                        </Button>
                    </div>
                </div>

                <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-4 mt-8">
                    <TabsList className="grid w-full grid-cols-4">
                        <TabsTrigger value="repl">Interpreter</TabsTrigger>
                        <TabsTrigger value="get-started">Get Started</TabsTrigger>
                        <TabsTrigger value="examples">Examples</TabsTrigger>
                        <TabsTrigger value="documentation">Documentation</TabsTrigger>
                    </TabsList>

                    <TabsContent value="repl">
                        <ReplModes
                            mode={mode}
                            onCommand={handleCommand}
                            ref={terminalRef}
                            initialInput={replInput}
                        />
                    </TabsContent>

                    <GetStartedTab
                        terminalRef={terminalRef}
                        onTabChange={() => {
                            setActiveTab('repl');
                            setReplInput('(+ 1 2 3)');
                        }}
                    />

                    <ExamplesTab
                        examples={examples}
                        handleTryExample={handleTryExample}
                    />

                    <TabsContent value="documentation">
                        <DocumentationTab />
                    </TabsContent>
                </Tabs>
            </div>
        </div>
    );
}

export default App;

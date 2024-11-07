import { useState, useRef } from 'react';
import { Button } from "@/components/ui/button";
import { WelcomePage } from "./components/welcome";
import useJawsInterpreter from './hooks/useJawsInterpreter';
import { ReplModes } from '@/components/repl-modes';
import { Tabs, TabsList, TabsTrigger, TabsContent } from "@/components/ui/tabs";
import { GetStartedTab, ExamplesTab, DocumentationTab } from '@/components/tabs';
import { ParenthesesIcon, ArrowLeft } from 'lucide-react';
import type { TerminalRef } from '@/components/terminal';
import './styles/globals.css'

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
    const [showWelcome, setShowWelcome] = useState(true);
    const [mode, setMode] = useState<'repl' | 'editor'>('repl');
    const [activeTab, setActiveTab] = useState('repl');
    const [replInput, setReplInput] = useState('');

    const interpreter = useJawsInterpreter();
    const terminalRef = useRef<TerminalRef>(null);

    const handleCommand = async (command: string) => {
        try {
            const result = interpreter.evaluate(command);
            return String(result);
        } catch (error) {
            throw new Error(error instanceof Error ? error.message : 'An error occurred');
        }
    };

    const handleTryExample = (code: string) => {
        setActiveTab('repl');
        setReplInput(code);
        setShowWelcome(false);
        terminalRef.current?.writeSystem("|> Example loaded and ready to run!");
    };

    if (showWelcome) {
        return (
            <WelcomePage
                onTryEditor={() => {
                    setShowWelcome(false);
                    setMode('editor');
                }}
                onGetStarted={() => {
                    setShowWelcome(false);
                    setActiveTab('get-started');
                }}
                onViewExamples={() => {
                    setShowWelcome(false);
                    setActiveTab('examples');
                }}
                onViewDocs={() => {
                    setShowWelcome(false);
                    setActiveTab('documentation');
                }}
            />
        );
    }

    return (
        <div className="min-h-screen w-full bg-background">
            {/* Navigation Bar */}
            <nav className="border-b bg-white">
                <div className="container mx-auto px-4 py-3 flex justify-between items-center">
                    <div className="flex items-center gap-4">
                        <Button
                            variant="ghost"
                            onClick={() => setShowWelcome(true)}
                            className="text-slate-600 hover:text-slate-900"
                        >
                            <ArrowLeft className="h-4 w-4 mr-2" />
                            Back to Home
                        </Button>
                        <div className="flex items-center gap-2">
                            <div className="text-xl font-bold text-slate-900">JAWS</div>
                            <ParenthesesIcon className="text-cyan-600" />
                        </div>
                    </div>
                    <div className="flex gap-4">
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
                            (+ Editor REPL)
                        </Button>
                    </div>
                </div>
            </nav>

            {/* Main Content */}
            <div className="container mx-auto px-4 sm:px-6 lg:px-8 py-8">
                <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-4">
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

                    <TabsContent value="get-started">
                        <GetStartedTab
                            terminalRef={terminalRef}
                            onTabChange={() => {
                                setActiveTab('repl');
                                setReplInput('(+ 1 2 3)');
                            }}
                        />
                    </TabsContent>

                    <TabsContent value="examples">
                        <ExamplesTab
                            examples={examples}
                            handleTryExample={handleTryExample}
                        />
                    </TabsContent>

                    <TabsContent value="documentation">
                        <DocumentationTab />
                    </TabsContent>
                </Tabs>
            </div>

        </div>

    );
}

export default App;

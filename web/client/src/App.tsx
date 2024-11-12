import { useState, useRef } from 'react';
import { Button } from "@/components/ui/button";
import { WelcomePage } from "./components/welcome";
import useJawsInterpreter from './hooks/useJawsInterpreter';
import { ReplModes } from '@/components/repl-modes';
import { Tabs, TabsList, TabsTrigger, TabsContent } from "@/components/ui/tabs";
import { GetStartedTab, ExamplesTab, DocumentationTab } from '@/components/tabs';
import { ParenthesesIcon, ArrowLeft, Menu, X } from 'lucide-react';
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
    const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

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

    const MobileMenu = () => (
        <div className={`
            fixed inset-0 bg-black/50 z-40 transition-opacity duration-200
            ${mobileMenuOpen ? 'opacity-100' : 'opacity-0 pointer-events-none'}
        `}>
            <div className={`
                fixed inset-y-0 right-0 w-64 bg-white shadow-lg transform transition-transform duration-200
                ${mobileMenuOpen ? 'translate-x-0' : 'translate-x-full'}
            `}>
                <div className="p-4 space-y-4">
                    <div className="flex justify-between items-center">
                        <span className="font-semibold text-lg">Menu</span>
                        <Button
                            variant="ghost"
                            size="icon"
                            onClick={() => setMobileMenuOpen(false)}
                        >
                            <X className="h-5 w-5" />
                        </Button>
                    </div>
                    <div className="space-y-2">
                        <Button
                            variant={mode === 'repl' ? 'default' : 'outline'}
                            className="w-full justify-start"
                            onClick={() => {
                                setMode('repl');
                                setMobileMenuOpen(false);
                            }}
                        >
                            REPL Only
                        </Button>
                        <Button
                            variant={mode === 'editor' ? 'default' : 'outline'}
                            className="w-full justify-start"
                            onClick={() => {
                                setMode('editor');
                                setMobileMenuOpen(false);
                            }}
                        >
                            (+ Editor REPL)
                        </Button>
                    </div>
                </div>
            </div>
        </div>
    );

    return (
        <div className="min-h-screen w-full bg-background">
            {/* Navigation Bar */}
            <nav className="border-b bg-white sticky top-0 z-30">
                <div className="container mx-auto px-4 py-3">
                    <div className="flex justify-between items-center">
                        <div className="flex items-center gap-2 md:gap-4">
                            <Button
                                variant="ghost"
                                onClick={() => setShowWelcome(true)}
                                className="text-slate-600 hover:text-slate-900"
                            >
                                <ArrowLeft className="h-4 w-4 md:mr-2" />
                                <span className="hidden md:inline">Back to Home</span>
                            </Button>
                            <div className="flex items-center gap-2">
                                <div className="text-xl font-bold text-slate-900">JAWS</div>
                                <ParenthesesIcon className="text-cyan-600" />
                            </div>
                        </div>

                        {/* Desktop mode buttons */}
                        <div className="hidden md:flex gap-4">
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

                        {/* Mobile menu button */}
                        <Button
                            variant="ghost"
                            size="icon"
                            className="md:hidden"
                            onClick={() => setMobileMenuOpen(true)}
                        >
                            <Menu className="h-5 w-5" />
                        </Button>
                    </div>
                </div>
            </nav>

            {/* Mobile menu overlay */}
            <MobileMenu />

            <div className="container mx-auto px-2 sm:px-4 lg:px-8 py-4 sm:py-8">
                <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-4">
                    {/* Removed overflow handling from container div and adjusted padding */}
                    <div className="w-full px-2 sm:px-0">
                        <TabsList className="w-full grid grid-cols-2 sm:grid-cols-4">
                            <TabsTrigger value="repl" className="text-sm sm:text-base">
                                Interpreter
                            </TabsTrigger>
                            <TabsTrigger value="get-started" className="text-sm sm:text-base">
                                Get Started
                            </TabsTrigger>
                            <TabsTrigger value="examples" className="text-sm sm:text-base">
                                Examples
                            </TabsTrigger>
                            <TabsTrigger value="documentation" className="text-sm sm:text-base">
                                Docs
                            </TabsTrigger>
                        </TabsList>
                    </div>

                    <TabsContent value="repl" className="mt-2">
                        <ReplModes
                            mode={mode}
                            onCommand={handleCommand}
                            ref={terminalRef}
                            initialInput={replInput}
                        />
                    </TabsContent>

                    <TabsContent value="get-started" className="mt-2">
                        <GetStartedTab
                            terminalRef={terminalRef}
                            onTabChange={() => {
                                setActiveTab('repl');
                                setReplInput('(+ 1 2 3)');
                            }}
                        />
                    </TabsContent>

                    <TabsContent value="examples" className="mt-2">
                        <ExamplesTab
                            examples={examples}
                            handleTryExample={handleTryExample}
                        />
                    </TabsContent>

                    <TabsContent value="documentation" className="mt-2">
                        <DocumentationTab />
                    </TabsContent>
                </Tabs>
            </div>
        </div>
    );
}

export default App;

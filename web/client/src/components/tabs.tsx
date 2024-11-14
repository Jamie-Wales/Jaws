import React, { useEffect, useRef } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Separator } from "@/components/ui/separator";
import { Button } from "@/components/ui/button";
import { Code2, BookOpen, Lightbulb, Rocket, ArrowRight, Book, Terminal, Github } from "lucide-react";
import { TabsContent } from "@/components/ui/tabs";
import hljs from 'highlight.js/lib/core';
import schemeLanguage from 'highlight.js/lib/languages/scheme';
import 'highlight.js/styles/github-dark-dimmed.css';
import type { TerminalRef } from './terminal';
import type { Example } from '../App';

hljs.registerLanguage('scheme', schemeLanguage);

const buttonStyles = {
    backgroundColor: '#dd3f0c',
    color: 'white',
};

const HighlightedCode = ({ code }: { code: string }) => {
    const codeRef = useRef<HTMLElement>(null);

    useEffect(() => {
        if (codeRef.current) {
            hljs.highlightElement(codeRef.current);
        }
    }, [code]);

    return (
        <pre className="rounded-md text-sm overflow-x-auto p-4 bg-zinc-900/50">
            <code ref={codeRef} className="language-scheme">
                {code}
            </code>
        </pre>
    );
};

interface GetStartedTabProps {
    terminalRef: React.RefObject<TerminalRef>;
    onTabChange: () => void;
}

interface ExamplesTabProps {
    examples: Example[];
    handleTryExample: (code: string) => void;
}

export function GetStartedTab({ terminalRef, onTabChange }: GetStartedTabProps) {
    const handleOpenEditor = () => {
        onTabChange();
        terminalRef.current?.writeSystem("|> Welcome to Jaws Scheme! Let's get started with a simple expression.");
        terminalRef.current?.writeOutput(`Try these examples:

(+ 1 2 3)     ; addition
(* 7 6)       ; multiplication
(- 10 5)      ; subtraction
(/ 15 3)      ; division
(display "Hello!")  ; print to output`);
    };

    return (
        <TabsContent value="get-started" className="space-y-4">
            <div className="grid gap-4 md:grid-cols-2">
                <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white border-none shadow-lg">
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2 text-zinc-200">
                            <Rocket className="h-5 w-5" style={{ color: '#06b6d4' }} />
                            Quick Start
                        </CardTitle>
                        <CardDescription className="text-slate-400">
                            Get up and running with Jaws Scheme
                        </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
                        <div className="space-y-2">
                            <h3 className="font-semibold text-zinc-200">Try Your First Program</h3>
                            <p className="text-sm text-slate-400">
                                Enter this simple expression to get started:
                            </p>
                            <HighlightedCode code="(+ 1 2 3)" />
                            <Button
                                style={buttonStyles}
                                className="w-full mt-4 hover:opacity-90"
                                onClick={handleOpenEditor}
                            >
                                <span className="flex items-center justify-center gap-2">
                                    Try REPL <ArrowRight className="h-4 w-4" />
                                </span>
                            </Button>
                        </div>
                    </CardContent>
                </Card>

                <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white border-none shadow-lg">
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2 text-zinc-200">
                            <BookOpen className="h-5 w-5" style={{ color: '#06b6d4' }} />
                            Features
                        </CardTitle>
                        <CardDescription className="text-slate-400">
                            What you can do with Jaws Scheme
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <ul className="space-y-6">
                            <li className="flex items-start gap-3">
                                <Code2 className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                                <div>
                                    <span className="font-medium text-zinc-200 block mb-1">Modern Editor</span>
                                    <p className="text-sm text-slate-400">Syntax highlighting and auto-completion for a seamless coding experience</p>
                                </div>
                            </li>
                            <li className="flex items-start gap-3">
                                <Terminal className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                                <div>
                                    <span className="font-medium text-zinc-200 block mb-1">Interactive REPL</span>
                                    <p className="text-sm text-slate-400">Evaluate code instantly in your browser with real-time feedback</p>
                                </div>
                            </li>
                            <li className="flex items-start gap-3">
                                <Lightbulb className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                                <div>
                                    <span className="font-medium text-zinc-200 block mb-1">Learning Tools</span>
                                    <p className="text-sm text-slate-400">Built-in examples and documentation to help you learn Scheme</p>
                                </div>
                            </li>
                        </ul>
                    </CardContent>
                </Card>
            </div>
        </TabsContent>
    );
}

export function ExamplesTab({ examples, handleTryExample }: ExamplesTabProps) {
    return (
        <TabsContent value="examples" className="space-y-4">
            <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white border-none shadow-lg">
                <CardHeader>
                    <CardTitle className="flex items-center gap-2 text-zinc-200">
                        <Code2 className="h-5 w-5" style={{ color: '#06b6d4' }} />
                        Example Programs
                    </CardTitle>
                    <CardDescription className="text-slate-400">
                        Learn by example with these code snippets
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="space-y-8">
                        {examples.map((example, index) => (
                            <div key={index} className="space-y-3">
                                <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2">
                                    <div className="space-y-1">
                                        <h3 className="font-semibold text-zinc-200">{example.name}</h3>
                                        <p className="text-sm text-slate-400">{example.description}</p>
                                    </div>
                                    <Button
                                        style={buttonStyles}
                                        size="sm"
                                        onClick={() => handleTryExample(example.code)}
                                        className="hover:opacity-90 border-none w-full sm:w-auto"
                                    >
                                        Try It <ArrowRight className="h-4 w-4 ml-2" />
                                    </Button>
                                </div>
                                <HighlightedCode code={example.code} />
                                {index < examples.length - 1 && (
                                    <Separator className="mt-6 bg-slate-700/50" />
                                )}
                            </div>
                        ))}
                    </div>
                </CardContent>
            </Card>
        </TabsContent>
    );
}

export function DocumentationTab() {
    return (
        <TabsContent value="documentation" className="space-y-4">
            <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white border-none shadow-lg">
                <CardHeader>
                    <CardTitle className="flex items-center gap-2 text-zinc-200">
                        <Book className="h-5 w-5" style={{ color: '#06b6d4' }} />
                        Documentation
                    </CardTitle>
                    <CardDescription className="text-slate-400">
                        Learn more about Jaws Scheme and its features
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-8">
                    <div className="space-y-3">
                        <div className="flex items-start gap-3">
                            <Code2 className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                            <div>
                                <h3 className="font-semibold text-zinc-200 mb-2">Language Support</h3>
                                <p className="text-sm text-slate-400 mb-3">
                                    Jaws implements a subset of R7RS Scheme with support for:
                                </p>
                                <ul className="grid gap-2 text-sm text-slate-400">
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Basic arithmetic operations
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        List manipulation
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        First-class functions
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Lexical scoping
                                    </li>
                                </ul>
                            </div>
                        </div>
                    </div>

                    <Separator className="bg-slate-700/50" />

                    <div className="space-y-3">
                        <div className="flex items-start gap-3">
                            <Terminal className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                            <div>
                                <h3 className="font-semibold text-zinc-200 mb-2">Key Features</h3>
                                <p className="text-sm text-slate-400 mb-3">
                                    Explore these core features:
                                </p>
                                <ul className="grid gap-2 text-sm text-slate-400">
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Interactive REPL with syntax highlighting
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Built-in code editor for larger programs
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Real-time error reporting
                                    </li>
                                </ul>
                            </div>
                        </div>
                    </div>

                    <Separator className="bg-slate-700/50" />

                    <div className="space-y-3">
                        <div className="flex items-start gap-3">
                            <Github className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                            <div>
                                <h3 className="font-semibold text-zinc-200 mb-2">Getting Help</h3>
                                <p className="text-sm text-slate-400 mb-3">
                                    Need help? Try these resources:
                                </p>
                                <ul className="grid gap-2 text-sm text-slate-400">
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Check out the examples tab
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        Read the language reference
                                    </li>
                                    <li className="flex items-center gap-2">
                                        <div className="w-1.5 h-1.5 rounded-full" style={{ backgroundColor: '#06b6d4' }} />
                                        View the source on GitHub
                                    </li>
                                </ul>
                            </div>
                        </div>
                    </div>
                </CardContent>
            </Card>
        </TabsContent>
    );
}

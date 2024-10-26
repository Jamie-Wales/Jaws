import React, { useEffect, useRef } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Separator } from "@/components/ui/separator";
import { Button } from "@/components/ui/button";
import { Code2, BookOpen, Lightbulb, Rocket, ArrowRight } from "lucide-react";
import { TabsContent } from "@/components/ui/tabs";
import hljs from 'highlight.js/lib/core';
import schemeLanguage from 'highlight.js/lib/languages/scheme';
import 'highlight.js/styles/github-dark-dimmed.css';
import type { TerminalRef } from './terminal';
import type { Example } from '../App';

hljs.registerLanguage('scheme', schemeLanguage);

const HighlightedCode = ({ code }: { code: string }) => {
    const codeRef = useRef<HTMLElement>(null);

    useEffect(() => {
        if (codeRef.current) {
            hljs.highlightElement(codeRef.current);
        }
    }, [code]);

    return (
        <pre className="bg-muted p-3 rounded-md text-sm">
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
        <TabsContent value="get-started">
            <div className="grid gap-4 md:grid-cols-2">
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <Rocket className="h-5 w-5" />
                            Quick Start
                        </CardTitle>
                        <CardDescription>
                            Get up and running with Jaws Scheme
                        </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
                        <div className="space-y-2">
                            <h3 className="font-semibold">Try Your First Program</h3>
                            <p className="text-sm text-muted-foreground">
                                Enter this simple expression to get started:
                            </p>
                            <HighlightedCode code="(+ 1 2 3)" />
                            <Button
                                className="w-full mt-2"
                                onClick={handleOpenEditor}
                            >
                                <span className="flex items-center gap-2">
                                    Try REPL <ArrowRight className="h-4 w-4" />
                                </span>
                            </Button>
                        </div>
                    </CardContent>
                </Card>

                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <BookOpen className="h-5 w-5" />
                            Features
                        </CardTitle>
                        <CardDescription>
                            What you can do with Jaws Scheme
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <ul className="space-y-4">
                            <li className="flex items-start gap-2">
                                <Code2 className="h-5 w-5 text-muted-foreground shrink-0" />
                                <div>
                                    <span className="font-medium">Modern Editor</span>
                                    <p className="text-sm text-muted-foreground">Syntax highlighting and auto-completion</p>
                                </div>
                            </li>
                            <li className="flex items-start gap-2">
                                <Lightbulb className="h-5 w-5 text-muted-foreground shrink-0" />
                                <div>
                                    <span className="font-medium">Interactive REPL</span>
                                    <p className="text-sm text-muted-foreground">Evaluate code instantly in your browser</p>
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
        <TabsContent value="examples">
            <Card>
                <CardHeader>
                    <CardTitle>Example Programs</CardTitle>
                    <CardDescription>
                        Learn by example with these code snippets
                    </CardDescription>
                </CardHeader>
                <CardContent>
                    <div className="space-y-6">
                        {examples.map((example, index) => (
                            <div key={index} className="space-y-2">
                                <div className="flex items-center justify-between">
                                    <h3 className="font-semibold">{example.name}</h3>
                                    <Button
                                        variant="outline"
                                        size="sm"
                                        onClick={() => handleTryExample(example.code)}
                                        className="flex items-center gap-2"
                                    >
                                        Try It <ArrowRight className="h-4 w-4" />
                                    </Button>
                                </div>
                                <p className="text-sm text-muted-foreground">{example.description}</p>
                                <HighlightedCode code={example.code} />
                                {index < examples.length - 1 && (
                                    <Separator className="mt-4" />
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
        <TabsContent value="documentation">
            <Card>
                <CardHeader>
                    <CardTitle>Documentation</CardTitle>
                    <CardDescription>
                        Learn more about Jaws Scheme and its features
                    </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                    <div className="space-y-2">
                        <h3 className="font-semibold">Language Support</h3>
                        <p className="text-sm text-muted-foreground">
                            Jaws implements a subset of R7RS Scheme with support for:
                        </p>
                        <ul className="list-disc list-inside text-sm text-muted-foreground space-y-1">
                            <li>Basic arithmetic operations</li>
                            <li>List manipulation</li>
                            <li>First-class functions</li>
                            <li>Lexical scoping</li>
                            <li>Basic I/O operations</li>
                        </ul>
                    </div>

                    <Separator />

                    <div className="space-y-2">
                        <h3 className="font-semibold">Key Features</h3>
                        <p className="text-sm text-muted-foreground">
                            Explore these core features:
                        </p>
                        <ul className="list-disc list-inside text-sm text-muted-foreground space-y-1">
                            <li>Interactive REPL with syntax highlighting</li>
                            <li>Built-in code editor for larger programs</li>
                            <li>Real-time error reporting</li>
                            <li>Example programs to learn from</li>
                        </ul>
                    </div>

                    <Separator />

                    <div className="space-y-2">
                        <h3 className="font-semibold">Getting Help</h3>
                        <p className="text-sm text-muted-foreground">
                            Need help? Try these resources:
                        </p>
                        <ul className="list-disc list-inside text-sm text-muted-foreground space-y-1">
                            <li>Check out the examples tab</li>
                            <li>Read the language reference</li>
                            <li>View the source on GitHub</li>
                        </ul>
                    </div>
                </CardContent>
            </Card>
        </TabsContent>
    );
}

import { useState } from 'react'
import { useJawsInterpreter } from './hooks/useJawsInterpreter'
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Separator } from "@/components/ui/separator"
import { Button } from "@/components/ui/button"
import { Code2, BookOpen, Lightbulb, Rocket } from "lucide-react"
import './styles/globals.css'

function App() {
    const examples = [
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
    ]

    return (
        <div className="min-h-screen w-full bg-background">
            <div className="container mx-auto px-4 sm:px-6 lg:px-8 py-8">
                <div className="text-center space-y-2">
                    <h1 className="text-4xl font-bold tracking-tight">Jaws Scheme Interpreter</h1>
                    <p className="text-muted-foreground">A modern Scheme REPL in your browser</p>
                </div>

                <Tabs defaultValue="get-started" className="space-y-4 mt-8">
                    <TabsList className="grid w-full grid-cols-3">
                        <TabsTrigger value="get-started">Get Started</TabsTrigger>
                        <TabsTrigger value="examples">Examples</TabsTrigger>
                        <TabsTrigger value="documentation">Documentation</TabsTrigger>
                    </TabsList>

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
                                        <pre className="bg-muted p-3 rounded-md text-sm">
                                            (+ 1 2 3)
                                        </pre>
                                        <Button className="w-full mt-2">Open Editor</Button>
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
                                                <Button variant="outline" size="sm">Try It</Button>
                                            </div>
                                            <p className="text-sm text-muted-foreground">{example.description}</p>
                                            <pre className="bg-muted p-3 rounded-md text-sm">
                                                {example.code}
                                            </pre>
                                            {index < examples.length - 1 && (
                                                <Separator className="mt-4" />
                                            )}
                                        </div>
                                    ))}
                                </div>
                            </CardContent>
                        </Card>
                    </TabsContent>

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
                                        Jaws implements a subset of R5RS Scheme with support for:
                                    </p>
                                    <ul className="list-disc list-inside text-sm text-muted-foreground space-y-1">
                                        <li>Basic arithmetic operations</li>
                                        <li>List manipulation</li>
                                        <li>First-class functions</li>
                                        <li>Lexical scoping</li>
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
                </Tabs>
            </div>
        </div>
    )
}

export default App

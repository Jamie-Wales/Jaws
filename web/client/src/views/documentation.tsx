// src/views/documentation.tsx
import React from 'react';
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { ArrowRight, Book, Code2, Terminal, Github } from 'lucide-react';
import { useNavigate } from 'react-router-dom';
import { LoadMarkdown } from '@/utils/loadMarkdown';
import { HighlightedText } from '@/components/highlightedText';

export function DocsView() {
    const navigate = useNavigate();

    function handleTryExample(code: any): void {
        throw new Error('Function not implemented.');
    }

    return (
        <div className="space-y-8">
            <div className="space-y-2">
                <h1 className="text-2xl font-bold text-slate-900">Documentation</h1>
                <p className="text-slate-600">
                    Complete guide to JAWS Scheme implementation and features.
                </p>
            </div>

            <div className="grid md:grid-cols-2 gap-6">
                <Card className="card-background">
                    <CardHeader className="card-header-boarder">
                        <CardTitle className="card-title">Quick Start</CardTitle>
                        <CardDescription className="card-description">
                            Get started with JAWS Scheme in minutes
                        </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
                        <div className="code-block">
                            <HighlightedText text={"(+ 1 2 3 4"} type="input" />
                        </div>
                        <Button onClick={() => handleTryExample("(+ 1 2 3 4)")} className="primary-button">
                            Try It <ArrowRight className="h-4 w-4" />
                        </Button>
                    </CardContent>
                </Card>
                <Card>
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2">
                            <Book className="h-5 w-5 text-cyan-600" />
                            Learning Resources
                        </CardTitle>
                        <CardDescription>
                            Additional materials to help you learn
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <div className="space-y-4">
                            <Button
                                variant="outline"
                                className="w-full justify-start"
                                onClick={() => navigate('/examples')}
                            >
                                <Code2 className="mr-2" />
                                Browse Examples
                            </Button>
                            <Button
                                variant="outline"
                                className="w-full justify-start"
                                asChild
                            >
                                <a href="https://github.com/yourusername/jaws" target="_blank" rel="noopener noreferrer">
                                    <Github className="mr-2" />
                                    View Source
                                </a>
                            </Button>
                        </div>
                    </CardContent>
                </Card>
            </div >
            {/* API Documentation */}
            < div className="space-y-6" >
                <Card>
                    <CardHeader>
                        <CardTitle>Basic Operations</CardTitle>
                    </CardHeader>
                    <CardContent>
                        <LoadMarkdown filePath="/markdown/basic_operations.md" />
                    </CardContent>
                </Card>
                {/* Add more sections as needed */}
            </div >
        </div >
    );
}

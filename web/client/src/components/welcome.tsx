import { useEffect, useRef } from 'react';
import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { ArrowRight, Code, BookOpen, Users, Terminal, Github, ParenthesesIcon, LucideGithub, GithubIcon } from 'lucide-react';
import hljs from 'highlight.js/lib/core';
import schemeLanguage from 'highlight.js/lib/languages/scheme';
import 'highlight.js/styles/github-dark-dimmed.css';
export const WelcomePage = () => {
    const sampleCode = `jaws> (define greet
  (lambda (name)
    (string-append 
      "Hello, " 
      name 
      "!")))

jaws> (greet "friend")
"Hello, friend!"
`;


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

    return (
        <div className="min-h-screen bg-white">
            {/* Hero Section */}
            <header className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white">
                <nav className="container mx-auto px-6 py-4 flex justify-between items-center">
                    <div className="flex items-center gap-2">
                        <div className="text-3xl font-bold">JAWS</div>
                        <ParenthesesIcon />
                    </div>
                    <div className="space-x-4">
                        <Button
                            variant="ghost"
                            className="text-white hover:bg-slate-800/60"
                        >
                            Documentation
                        </Button>
                        <Button
                            variant="ghost"
                            className="text-white hover:bg-slate-800/60"
                        >
                            Examples
                        </Button>
                        <Button
                            className="bg-cyan-500 hover:bg-cyan-600 text-white"
                            onClick={() => {
                            }}>                            Get Started
                        </Button>
                    </div>
                </nav>

                <div className="container mx-auto px-6 py-24 grid grid-cols-2 gap-12">
                    <div className="space-y-6">
                        <div className="space-y-2">
                            <h1 className="text-6xl font-bold leading-tight">
                                JAWS
                            </h1>
                            <p className="text-xl text-cyan-400 font-medium tracking-wide">
                                Jaws Awesomely Wrangles Scheme
                            </p>
                        </div>
                        <p className="text-xl text-slate-300">
                            Dive into Scheme programming with our modern, interactive development environment.
                            Write, test, and learn with the power of JAWS.
                        </p>
                        <div className="space-x-4 pt-4">
                            <Button
                                size="lg"
                                className="bg-cyan-500 hover:bg-cyan-600 text-white"
                            >
                                Try Online Editor <ArrowRight className="ml-2 h-4 w-4" />
                            </Button>
                            <Button
                                variant="outline"
                                size="lg"
                                className="bg-transparent text-white hover:bg-slate-800/60 border-white hover:border-white"
                            >
                                View Examples
                            </Button>
                        </div>
                    </div>
                    <div className="bg-slate-950 rounded-lg p-6 shadow-xl">
                        <HighlightedCode code={sampleCode} />
                    </div>
                </div>
            </header>

            {/* Features Section */}
            <section className="py-20 bg-slate-50">
                <div className="container mx-auto px-6">
                    <h2 className="text-3xl font-bold text-slate-900 text-center mb-12">
                        Deep dive into powerful features
                    </h2>

                    <div className="grid grid-cols-2 lg:grid-cols-4 gap-8">
                        <Card className="bg-white border-slate-200">
                            <CardContent className="pt-6 text-center">
                                <Terminal className="h-12 w-12 mb-4 mx-auto text-cyan-600" />
                                <h3 className="text-xl font-semibold text-slate-900 mb-2">
                                    Interactive REPL
                                </h3>
                                <p className="text-slate-600">
                                    Write and test code directly in your browser
                                </p>
                            </CardContent>
                        </Card>

                        <Card className="bg-white border-slate-200">
                            <CardContent className="pt-6 text-center">
                                <Code className="h-12 w-12 mb-4 mx-auto text-cyan-600" />
                                <h3 className="text-xl font-semibold text-slate-900 mb-2">
                                    Live Examples
                                </h3>
                                <p className="text-slate-600">
                                    Learn from practical, runnable code examples
                                </p>
                            </CardContent>
                        </Card>

                        <Card className="bg-white border-slate-200">
                            <CardContent className="pt-6 text-center">
                                <BookOpen className="h-12 w-12 mb-4 mx-auto text-cyan-600" />
                                <h3 className="text-xl font-semibold text-slate-900 mb-2">
                                    Guided Lessons
                                </h3>
                                <p className="text-slate-600">
                                    Step-by-step tutorials and exercises
                                </p>
                            </CardContent>
                        </Card>

                        <Card className="bg-white border-slate-200">
                            <CardContent className="pt-6 text-center">
                                <Users className="h-12 w-12 mb-4 mx-auto text-cyan-600" />
                                <h3 className="text-xl font-semibold text-slate-900 mb-2">
                                    Community
                                </h3>
                                <p className="text-slate-600">
                                    Share and learn with fellow developers
                                </p>
                            </CardContent>
                        </Card>
                    </div>
                </div>
            </section>

            {/* Getting Started Section */}
            <section className="py-20">
                <div className="container mx-auto px-6 text-center">
                    <h2 className="text-3xl font-bold text-slate-900 mb-12">
                        Ready to take the plunge?
                    </h2>
                    <div className="space-x-4">
                        <Button
                            size="lg"
                            className="bg-cyan-500 hover:bg-cyan-600 text-white"
                        >
                            Start Coding Now
                        </Button>
                        <Button
                            variant="outline"
                            size="lg"
                            className="border-slate-800 text-slate-800 hover:bg-slate-100/60 hover:text-slate-900"
                        >
                            Browse Documentation
                        </Button>
                    </div>
                </div>
            </section>

            {/* Footer */}
            <footer className="bg-slate-900 text-white">
                <div className="container mx-auto px-6 py-8">
                    <div className="flex justify-between items-center">
                        <div className="flex items-center gap-4">
                            <div className="font-bold text-xl">JAWS</div>
                            <span className="text-slate-400">Jaws Awesomely Wrangles Scheme</span>
                        </div>
                        <div className="flex items-center space-x-4">
                            <Button
                                variant="ghost"
                                className="text-slate-300 hover:text-white hover:bg-slate-800/60"
                            >
                                <Github className="h-5 w-5" />
                            </Button>
                            <Button
                                variant="ghost"
                                className="text-slate-300 hover:text-white hover:bg-slate-800/60"
                            >
                                Documentation
                            </Button>
                            <Button
                                variant="ghost"
                                className="text-slate-300 hover:text-white hover:bg-slate-800/60"
                            >
                                Examples
                            </Button>
                        </div>
                    </div>
                </div>
            </footer>
        </div>
    );
};

export default WelcomePage;

import { useNavigate } from 'react-router-dom';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Code2, BookOpen, Lightbulb, Rocket, ArrowRight, Terminal } from "lucide-react";
import { HighlightedText } from '@/components/highlightedText';


export function GetStartedView() {
    const navigate = useNavigate();

    const handleTryExample = (code: string) => {
        localStorage.setItem('editorCode', code);
        navigate('/editor');
    };

    const buttonStyles = {
        backgroundColor: '#dd3f0c',
        color: 'white',
    };

    return (
        <div className="space-y-4">
            <div className="grid gap-4 md:grid-cols-2">
                <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white border-none shadow-lg animate-gradientFlow animate-fadeScale">
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2 text-zinc-200 text-xl">
                            <Rocket className="h-5 w-5" style={{ color: '#06b6d4' }} />
                            Quick Start
                        </CardTitle>
                        <CardDescription className="text-base text-slate-400">
                            Get up and running with Jaws Scheme
                        </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
                        <div className="space-y-2">
                            <h3 className="text-lg font-semibold text-zinc-200">Try Your First Program</h3>
                            <p className="text-sm text-slate-400">
                                Enter this simple expression to get started:
                            </p>
                            <div className="rounded-md text-sm font-mono overflow-x-auto p-4 bg-zinc-900/50">
                                <HighlightedText text="(+ 1 2 3)" type="input" />
                            </div>
                            <Button
                                style={buttonStyles}
                                className="w-full mt-4 hover:opacity-90 transition-all duration-200"
                                onClick={() => handleTryExample("(+ 1 2 3)")}
                            >
                                <span className="flex items-center justify-center gap-2">
                                    Try REPL <ArrowRight className="h-4 w-4" />
                                </span>
                            </Button>
                        </div>
                    </CardContent>
                </Card>

                <Card className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white border-none shadow-lg animate-gradientFlow animate-fadeScale animation-delay-200">
                    <CardHeader>
                        <CardTitle className="flex items-center gap-2 text-zinc-200 text-xl">
                            <BookOpen className="h-5 w-5" style={{ color: '#06b6d4' }} />
                            Features
                        </CardTitle>
                        <CardDescription className="text-base text-slate-400">
                            What you can do with Jaws Scheme
                        </CardDescription>
                    </CardHeader>
                    <CardContent>
                        <ul className="space-y-6">
                            <li className="flex items-start gap-3 animate-fadeSlideIn">
                                <Code2 className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                                <div>
                                    <span className="text-lg font-medium text-zinc-200 block mb-1">Modern Editor</span>
                                    <p className="text-base text-slate-400">Syntax highlighting and auto-completion for a seamless coding experience</p>
                                </div>
                            </li>
                            <li className="flex items-start gap-3 animate-fadeSlideIn animation-delay-100">
                                <Terminal className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                                <div>
                                    <span className="text-lg font-medium text-zinc-200 block mb-1">Interactive REPL</span>
                                    <p className="text-base text-slate-400">Evaluate code instantly in your browser with real-time feedback</p>
                                </div>
                            </li>
                            <li className="flex items-start gap-3 animate-fadeSlideIn animation-delay-200">
                                <Lightbulb className="h-5 w-5 shrink-0 mt-1" style={{ color: '#06b6d4' }} />
                                <div>
                                    <span className="text-lg font-medium text-zinc-200 block mb-1">Learning Tools</span>
                                    <p className="text-base text-slate-400">Built-in examples and documentation to help you learn Scheme</p>
                                </div>
                            </li>
                        </ul>
                    </CardContent>
                </Card>
            </div>
        </div>
    );
}

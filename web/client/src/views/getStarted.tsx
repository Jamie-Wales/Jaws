import { useNavigate } from 'react-router-dom';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Code2, BookOpen, Lightbulb, Rocket, ArrowRight, Terminal } from "lucide-react";
import { HighlightedText } from '@/components/highlightedText';

const buttonStyles = {
    backgroundColor: '#dd3f0c',
    color: 'white',
};

export function GetStartedView() {
    const navigate = useNavigate();

    const handleOpenRepl = () => {
        navigate('/repl');
    };

    return (
        <div className="space-y-4">
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
                            <div className="rounded-md text-sm overflow-x-auto p-4 bg-zinc-900/50">
                                <HighlightedText text="(+ 1 2 3)" type="input" />
                            </div>
                            <Button
                                style={buttonStyles}
                                className="w-full mt-4 hover:opacity-90"
                                onClick={handleOpenRepl}
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
        </div>
    );
}
import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { MultiLineCode } from '@/components/multilineCode'
import {
    ArrowRight,
    Code,
    BookOpen,
    Users,
    Terminal,
    ParenthesesIcon,
    Github,
    Menu,
    X
} from 'lucide-react';

type CodeLine = {
    type: 'input' | 'output';
    content: string;
};

export function WelcomePage() {
    const navigate = useNavigate();
    const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

    const sampleCode: CodeLine[] = [
        {
            type: 'input',
            content: '(define greet\n  (lambda (name)\n    (string-append\n      "Hello, "\n      name\n      "!")))'
        },
        {
            type: 'input',
            content: '(greet "friend")'
        },
        {
            type: 'output',
            content: '"Hello, friend!"'
        }
    ];

    const buttonStyles = {
        backgroundColor: '#dd3f0c',
        color: 'white',
    };

    const MobileMenu = () => (
        <div className={`
            fixed inset-0 bg-black/50 z-50 transition-opacity duration-200
            ${mobileMenuOpen ? 'opacity-100' : 'opacity-0 pointer-events-none'}
        `}>
            <div className={`
                fixed inset-y-0 right-0 w-64 bg-slate-900 shadow-lg transform transition-transform duration-200
                ${mobileMenuOpen ? 'translate-x-0' : 'translate-x-full'}
            `}>
                <div className="p-4 space-y-4">
                    <div className="flex justify-between items-center">
                        <span className="font-semibold text-lg text-white">Menu</span>
                        <Button
                            variant="ghost"
                            size="icon"
                            className="text-slate-300 hover:text-white"
                            onClick={() => setMobileMenuOpen(false)}
                        >
                            <X className="h-5 w-5" />
                        </Button>
                    </div>
                    <div className="space-y-2">
                        <Button
                            variant="ghost"
                            className="w-full justify-start text-white hover:bg-slate-800/60"
                            onClick={() => {
                                navigate('/Jaws/docs');
                                setMobileMenuOpen(false);
                            }}
                        >
                            Documentation
                        </Button>
                        <Button
                            variant="ghost"
                            className="w-full justify-start text-white hover:bg-slate-800/60"
                            onClick={() => {
                                navigate('/Jaws/examples');
                                setMobileMenuOpen(false);
                            }}
                        >
                            Examples
                        </Button>
                        <Button
                            style={buttonStyles}
                            className="w-full justify-start hover:opacity-90"
                            onClick={() => {
                                navigate('/Jaws/get-started');
                                setMobileMenuOpen(false);
                            }}
                        >
                            Get Started
                        </Button>
                    </div>
                </div>
            </div>
        </div>
    );

    return (
        <div className="min-h-screen bg-white">
            <header className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white">
                <nav className="container mx-auto px-4 py-4">
                    <div className="flex flex-col md:flex-row md:items-center md:justify-between space-y-4 md:space-y-0">
                        <div className="flex items-center justify-between">
                            <div className="flex items-center gap-2">
                                <div className="text-2xl md:text-3xl font-bold animate-[slideInRight_0.6s_ease-out_forwards]">JAWS</div>
                                <ParenthesesIcon className="animate-[slideInRight_0.6s_ease-out_forwards]" />
                            </div>
                            <Button
                                variant="ghost"
                                size="icon"
                                className="md:hidden text-white"
                                onClick={() => setMobileMenuOpen(true)}
                            >
                                <Menu className="h-6 w-6" />
                            </Button>
                        </div>
                        <div className="hidden md:flex md:flex-row space-y-2 md:space-y-0 md:space-x-4">
                            <Button
                                variant="ghost"
                                className="text-white hover:bg-slate-800/60 w-full md:w-auto"
                                onClick={() => navigate('/Jaws/docs')}
                            >
                                Documentation
                            </Button>
                            <Button
                                variant="ghost"
                                className="text-white hover:bg-slate-800/60 w-full md:w-auto"
                                onClick={() => navigate('/Jaws/examples')}
                            >
                                Examples
                            </Button>
                            <Button
                                style={buttonStyles}
                                className="hover:opacity-90 hover:-translate-y-1 transition-all duration-300 w-full md:w-auto"
                                onClick={() => navigate('/Jaws/get-started')}
                            >
                                Get Started
                            </Button>
                        </div>
                    </div>
                </nav>

                <MobileMenu />

                <div className="container mx-auto px-4 py-12 md:py-24">
                    <div className="grid md:grid-cols-2 gap-8 md:gap-12">
                        <div className="space-y-6 animate-[floatIn_0.8s_ease-out_forwards]">
                            <div className="space-y-2">
                                <h1 className="text-4xl md:text-6xl font-bold leading-tight">
                                    JAWS
                                </h1>
                                <p className="text-lg md:text-xl text-cyan-400 font-medium tracking-wide bg-gradient-text">
                                    Jaws Awesomely Wrangles Scheme
                                </p>
                            </div>
                            <p className="text-lg md:text-xl text-slate-300">
                                Dive into Scheme programming with our modern, interactive development environment.
                                Write, test, and learn with the power of JAWS.
                            </p>
                            <div className="flex flex-col sm:flex-row gap-4 sm:gap-4 pt-4">
                                <Button
                                    size="lg"
                                    style={buttonStyles}
                                    className="hover:opacity-90 hover:-translate-y-1 transition-all duration-300 w-full sm:w-auto"
                                    onClick={() => navigate('/Jaws/editor')}
                                >
                                    Try Online Editor <ArrowRight className="ml-2 h-4 w-4" />
                                </Button>
                                <Button
                                    variant="outline"
                                    size="lg"
                                    className="bg-transparent text-white hover:bg-slate-800/60 border-white hover:border-white w-full sm:w-auto"
                                    onClick={() => navigate('/Jaws/examples')}
                                >
                                    View Examples
                                </Button>
                            </div>
                        </div>
                        <div className="bg-zinc-900 rounded-lg p-4 md:p-6 shadow-xl ring-1 ring-white/10 overflow-x-auto animate-[fadeScale_0.8s_ease-out_0.4s_forwards] opacity-0">
                            <MultiLineCode code={sampleCode} />
                        </div>
                    </div>
                </div>
            </header>

            <section className="py-12 md:py-20 bg-slate-50">
                <div className="container mx-auto px-4">
                    <h2 className="text-2xl md:text-3xl font-bold text-slate-900 text-center mb-8 md:mb-12">
                        Deep dive into powerful features
                    </h2>

                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-6 md:gap-8">
                        {[
                            {
                                Icon: Terminal,
                                title: "Interactive REPL",
                                description: "Write and test code directly in your browser"
                            },
                            {
                                Icon: Code,
                                title: "Live Examples",
                                description: "Learn from practical, runnable code examples"
                            },
                            {
                                Icon: BookOpen,
                                title: "Guided Lessons",
                                description: "Step-by-step tutorials and exercises"
                            },
                            {
                                Icon: Users,
                                title: "Community",
                                description: "Share and learn with fellow developers"
                            }
                        ].map((feature, index) => (
                            <Card
                                key={feature.title}
                                className="bg-white border-slate-200 hover:transform hover:-translate-y-1 transition-all duration-300 opacity-0"
                                style={{ animation: `fadeScale 0.6s ease-out ${index * 0.1}s forwards` }}
                            >
                                <CardContent className="pt-6 text-center">
                                    <feature.Icon className="h-12 w-12 mb-4 mx-auto transition-transform duration-300 text-cyan-500" />
                                    <h3 className="text-lg md:text-xl font-semibold text-slate-900 mb-2">
                                        {feature.title}
                                    </h3>
                                    <p className="text-slate-600">
                                        {feature.description}
                                    </p>
                                </CardContent>
                            </Card>
                        ))}
                    </div>
                </div>
            </section>

            <section className="py-12 md:py-20">
                <div className="container mx-auto px-4 text-center">
                    <h2 className="text-2xl md:text-3xl font-bold text-slate-900 mb-8 md:mb-12">
                        Ready to take the plunge?
                    </h2>
                    <div className="flex flex-col sm:flex-row justify-center gap-4 sm:gap-4">
                        <Button
                            size="lg"
                            style={buttonStyles}
                            className="hover:opacity-90 hover:-translate-y-1 transition-all duration-300 w-full sm:w-auto"
                            onClick={() => navigate('/Jaws/editor')}
                        >
                            Start Coding Now
                        </Button>
                        <Button
                            variant="outline"
                            size="lg"
                            className="border-slate-800 text-slate-800 hover:bg-slate-100/60 hover:text-slate-900 w-full sm:w-auto"
                            onClick={() => navigate('/Jaws/docs')}
                        >
                            Browse Documentation
                        </Button>
                    </div>
                </div>
            </section>

            <footer className="bg-slate-900 text-white">
                <div className="container mx-auto px-4 py-8">
                    <div className="flex flex-col md:flex-row justify-between items-center space-y-4 md:space-y-0">
                        <div className="flex items-center gap-4">
                            <div className="font-bold text-xl">JAWS</div>
                            <span className="text-slate-400 hidden sm:inline">Jaws Awesomely Wrangles Scheme</span>
                        </div>
                        <div className="flex items-center space-x-4">
                            <Button
                                variant="ghost"
                                className="text-slate-300 hover:text-white hover:bg-slate-800/60 transition-transform hover:-translate-y-1 duration-300"
                                asChild
                            >
                                <a href="https://github.com/jamie-wales/jaws" target="_blank" rel="noopener noreferrer">
                                    <Github className="h-5 w-5" />
                                </a>
                            </Button>
                            <Button
                                variant="ghost"
                                className="text-slate-300 hover:text-white hover:bg-slate-800/60 transition-transform hover:-translate-y-1 duration-300"
                                onClick={() => navigate('/Jaws/docs')}
                            >
                                Docs
                            </Button>
                            <Button
                                variant="ghost"
                                className="text-slate-300 hover:text-white hover:bg-slate-800/60 transition-transform hover:-translate-y-1 duration-300"
                                onClick={() => navigate('/Jaws/examples')}
                            >
                                Examples
                            </Button>
                        </div>
                    </div>
                </div>
            </footer>
        </div>
    );
}

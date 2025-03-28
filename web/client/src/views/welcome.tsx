import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { MultiLineCode } from '@/components/multilineCode';
import { useNavigate, Link, NavLink } from 'react-router-dom';
import {
    ArrowRight,
    Code,
    BookOpen,
    Users,
    Terminal,
    Parentheses,
    Github,
    Menu,
    X
} from 'lucide-react';
import { useState } from 'react';
import { Footer } from '@/components/footer';

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

    return (
        <div className="min-h-screen bg-white flex flex-col">
            <nav className="border-b bg-white sticky top-0 z-30">
                <div className="container mx-auto px-4 py-3">
                    <div className="flex justify-between items-center">
                        <Link
                            to="/"
                            className="flex items-center gap-2 text-xl font-bold text-slate-900"
                        >
                            JAWS
                            <Parentheses className="text-cyan-600" />
                        </Link>

                        {/* Desktop Navigation */}
                        <div className="hidden md:flex items-center gap-4">
                            <NavLink
                                to="/editor"
                                className={({ isActive }) =>
                                    isActive
                                        ? "px-4 py-2 rounded-md bg-blue-900 text-white no-underline hover:no-underline hover:text-white hover:bg-blue-900"
                                        : "px-4 py-2 rounded-md text-slate-600 hover:text-white hover:bg-slate-800"
                                }
                            >
                                Editor
                            </NavLink>
                            <NavLink
                                to="/docs"
                                className={({ isActive }) =>
                                    isActive
                                        ? "px-4 py-2 rounded-md bg-blue-900 text-white no-underline hover:no-underline hover:text-white hover:bg-blue-900"
                                        : "px-4 py-2 rounded-md text-slate-600 hover:text-white hover:bg-slate-800"
                                }
                            >
                                Docs
                            </NavLink>
                            <NavLink
                                to="/compiler-explorer"
                                className={({ isActive }) =>
                                    isActive
                                        ? "px-4 py-2 rounded-md bg-blue-900 text-white no-underline hover:no-underline hover:text-white hover:bg-blue-900"
                                        : "px-4 py-2 rounded-md text-slate-600 hover:text-white hover:bg-slate-800"
                                }
                            >
                                Compiler Explorer
                            </NavLink>
                            <NavLink
                                to="/learn"
                                className="px-4 py-2 rounded-md font-medium bg-[#dd3f0c] text-white no-underline hover:no-underline hover:text-white hover:bg-[#dd3f0c]"
                            >
                                Learn Scheme
                            </NavLink>
                            <Button
                                variant="ghost"
                                size="icon"
                                asChild
                            >
                                <a href="https://github.com/jamie-wales/jaws" target="_blank" rel="noopener noreferrer">
                                    <Github className="h-5 w-5" />
                                </a>
                            </Button>
                        </div>

                        {/* Mobile Menu Button */}
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

            {/* Mobile Menu */}
            {mobileMenuOpen && (
                <div className="fixed inset-0 z-50 bg-black/50 md:hidden">
                    <div className="fixed inset-y-0 right-0 w-64 bg-white p-6 shadow-xl">
                        <div className="flex justify-between items-center mb-8">
                            <Link
                                to="/"
                                className="flex items-center gap-2 text-xl font-bold text-slate-900"
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                JAWS
                                <Parentheses className="text-cyan-600" />
                            </Link>
                            <Button
                                variant="ghost"
                                size="icon"
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                <X className="h-5 w-5" />
                            </Button>
                        </div>
                        <div className="space-y-4">
                            <NavLink
                                to="/editor"
                                className={({ isActive }) =>
                                    isActive
                                        ? "block px-4 py-2 rounded-md bg-blue-900 text-white no-underline hover:no-underline hover:text-white hover:bg-blue-900"
                                        : "block px-4 py-2 rounded-md text-slate-600 hover:text-white hover:bg-slate-800"
                                }
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Editor
                            </NavLink>
                            <NavLink
                                to="/docs"
                                className={({ isActive }) =>
                                    isActive
                                        ? "block px-4 py-2 rounded-md bg-blue-900 text-white no-underline hover:no-underline hover:text-white hover:bg-blue-900"
                                        : "block px-4 py-2 rounded-md text-slate-600 hover:text-white hover:bg-slate-800"
                                }
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Docs
                            </NavLink>
                            <NavLink
                                to="/compiler-explorer"
                                className={({ isActive }) =>
                                    isActive
                                        ? "block px-4 py-2 rounded-md bg-blue-900 text-white no-underline hover:no-underline hover:text-white hover:bg-blue-900"
                                        : "block px-4 py-2 rounded-md text-slate-600 hover:text-white hover:bg-slate-800"
                                }
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Compiler Explorer
                            </NavLink>
                            <NavLink
                                to="/learn"
                                className="block px-4 py-2 rounded-md font-medium w-full text-center bg-[#dd3f0c] text-white no-underline hover:no-underline hover:text-white hover:bg-[#dd3f0c]"
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Learn Scheme
                            </NavLink>
                        </div>
                    </div>
                </div>
            )}

            <header className="bg-gradient-to-r from-slate-900 via-blue-900 to-slate-900 text-white animate-gradientFlow">
                <div className="container mx-auto px-4 py-12 md:py-24">
                    <div className="grid md:grid-cols-2 gap-8 md:gap-12">
                        <div className="space-y-6 animate-floatIn">
                            <div className="space-y-2">
                                <div className="flex items-center gap-2">
                                    <h1 className="text-4xl md:text-6xl font-semibold leading-tight text-white">
                                        JAWS
                                    </h1>
                                    <Parentheses className="h-8 w-8 md:h-12 md:w-12 text-[#dd3f0c]" />
                                </div>
                                <p className="text-lg md:text-xl font-medium tracking-wide text-cyan-400">
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
                                    className="bg-[#dd3f0c] text-white hover:opacity-90 hover:-translate-y-1 transition-all duration-300 w-full sm:w-auto text-base"
                                    onClick={() => navigate('/learn')}
                                >
                                    Learn Scheme <ArrowRight className="ml-2 h-4 w-4" />
                                </Button>
                                <Button
                                    variant="outline"
                                    size="lg"
                                    className="bg-transparent text-white hover:bg-slate-800/60 border-white hover:border-white w-full sm:w-auto text-base"
                                    onClick={() => navigate('/docs')}
                                >
                                    View Docs
                                </Button>
                            </div>
                        </div>
                        <div className="bg-zinc-900 rounded-lg p-4 md:p-6 shadow-xl ring-1 ring-white/10 overflow-x-auto animate-fadeScale animation-delay-400">
                            <MultiLineCode code={sampleCode} />
                        </div>
                    </div>
                </div>
            </header>

            <section className="flex-1 py-12 md:py-20 bg-slate-50">
                <div className="container mx-auto px-4">
                    <h2 className="text-2xl md:text-3xl font-semibold text-slate-900 text-center mb-8 md:mb-12">
                        Deep dive into powerful features
                    </h2>

                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-6 md:gap-8">
                        {[
                            {
                                Icon: Terminal,
                                title: "Interactive Editor",
                                description: "Write and test code directly in your browser"
                            },
                            {
                                Icon: Code,
                                title: "Live Compilation",
                                description: "See how your code transforms at each stage"
                            },
                            {
                                Icon: BookOpen,
                                title: "Rich Examples",
                                description: "Learn from practical, runnable code examples"
                            },
                            {
                                Icon: Users,
                                title: "Community",
                                description: "Share and learn with fellow developers"
                            }
                        ].map((feature, index) => (
                            <Card
                                key={feature.title}
                                className={`bg-white border-slate-200 hover:transform hover:-translate-y-1 transition-all duration-300 animate-fadeScale`}
                                style={{ animationDelay: `${index * 0.1}s` }}
                            >
                                <CardContent className="pt-6 text-center">
                                    <feature.Icon className="h-12 w-12 mb-4 mx-auto transition-transform duration-300 text-cyan-500" />
                                    <h3 className="text-lg md:text-xl font-semibold text-slate-900 mb-2">
                                        {feature.title}
                                    </h3>
                                    <p className="text-base text-slate-600">
                                        {feature.description}
                                    </p>
                                </CardContent>
                            </Card>
                        ))}
                    </div>
                </div>
            </section>

            <section className="py-12 border-t">
                <div className="container mx-auto px-4 text-center">
                    <div className="max-w-md mx-auto">
                        <h2 className="text-2xl md:text-3xl font-semibold text-slate-900 mb-8">
                            Ready to dive in?
                        </h2>
                        <div className="flex flex-col sm:flex-row justify-center gap-4">
                            <Button
                                size="lg"
                                className="bg-[#dd3f0c] text-white hover:opacity-90 hover:-translate-y-1 transition-all duration-300 w-full sm:w-auto text-base"
                                onClick={() => navigate('/learn')}
                            >
                                Learn Scheme <ArrowRight className="ml-2 h-4 w-4" />
                            </Button>
                            <Button
                                variant="outline"
                                size="lg"
                                className="border-slate-800 text-slate-800 hover:bg-slate-100/60 hover:text-slate-900 w-full sm:w-auto text-base"
                                onClick={() => navigate('/docs')}
                            >
                                Browse docs
                            </Button>
                        </div>
                    </div>
                </div>
            </section>

            <Footer />
        </div>
    );
}

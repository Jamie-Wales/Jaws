import { useState } from 'react';
import { Outlet, NavLink, Link } from 'react-router-dom';
import { Button } from "@/components/ui/button";
import { Parentheses, Github, Menu, X } from 'lucide-react';

export function Layout() {
    const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

    return (
        <div className="min-h-screen bg-background">
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

            {/* Main Content */}
            <main className="h-[calc(100vh-57px)]">
                <Outlet />
            </main>
        </div>
    );
}

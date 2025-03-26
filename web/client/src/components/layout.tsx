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
                            to="/Jaws"
                            className="flex items-center gap-2 text-xl font-bold text-slate-900"
                        >
                            JAWS
                            <Parentheses className="text-cyan-600" />
                        </Link>

                        {/* Desktop Navigation */}
                        <div className="hidden md:flex items-center gap-4">
                            <NavLink
                                to="/Jaws/editor"
                                className={({ isActive }) =>
                                    `px-4 py-2 rounded-md transition-colors ${isActive ? 'bg-slate-100 text-slate-900' : 'text-slate-600 hover:text-slate-900'}`
                                }
                            >
                                Editor
                            </NavLink>
                            <NavLink
                                to="/Jaws/examples"
                                className={({ isActive }) =>
                                    `px-4 py-2 rounded-md transition-colors ${isActive ? 'bg-slate-100 text-slate-900' : 'text-slate-600 hover:text-slate-900'}`
                                }
                            >
                                Examples
                            </NavLink>
                            <NavLink
                                to="/Jaws/compiler-explorer"
                                className={({ isActive }) =>
                                    `px-4 py-2 rounded-md transition-colors ${isActive ? 'bg-slate-100 text-slate-900' : 'text-slate-600 hover:text-slate-900'}`
                                }
                            >
                                Compiler Explorer
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
                                to="/Jaws"
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
                                to="/Jaws/editor"
                                className={({ isActive }) =>
                                    `block px-4 py-2 rounded-md transition-colors ${isActive ? 'bg-slate-100 text-slate-900' : 'text-slate-600'}`
                                }
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Editor
                            </NavLink>
                            <NavLink
                                to="/Jaws/examples"
                                className={({ isActive }) =>
                                    `block px-4 py-2 rounded-md transition-colors ${isActive ? 'bg-slate-100 text-slate-900' : 'text-slate-600'}`
                                }
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Examples
                            </NavLink>
                            <NavLink
                                to="/Jaws/compiler-explorer"
                                className={({ isActive }) =>
                                    `block px-4 py-2 rounded-md transition-colors ${isActive ? 'bg-slate-100 text-slate-900' : 'text-slate-600'}`
                                }
                                onClick={() => setMobileMenuOpen(false)}
                            >
                                Compiler Explorer
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

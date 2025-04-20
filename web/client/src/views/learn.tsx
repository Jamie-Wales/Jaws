import { useState, useRef } from 'react';
import { Terminal, TerminalRef } from '@/components/terminal';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { ChevronLeft, ArrowRight, Parentheses, Menu, X, ChevronDown, ChevronRight } from 'lucide-react';
import { HighlightedText } from '@/components/highlightedText';
import { DifficultyBadge } from '@/components/DifficultyBadge';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';
import { CURRICULUM } from './chapters';

interface Chapter {
    id: string;
    title: string;
    description: string;
    sections: Section[];
}

interface CodeExample {
    description: string;
    code: string;
    explanation: string;
}

interface Section {
    id: string;
    title: string;
    content: string | string[];
    codeExamples?: CodeExample[];
    difficulty: 'Beginner' | 'Intermediate' | 'Advanced';
}

export function LearnScheme() {
    const [selectedChapter, setSelectedChapter] = useState<Chapter>(CURRICULUM[0]);
    const [selectedSection, setSelectedSection] = useState<Section>(CURRICULUM[0].sections[0]);
    const [showMenu, setShowMenu] = useState(false);
    const [showTerminal, setShowTerminal] = useState(false);
    const [expandedChapters, setExpandedChapters] = useState<{ [key: string]: boolean }>({
        [CURRICULUM[0].id]: true // Initialize with first chapter expanded
    });
    const terminalRef = useRef<TerminalRef>(null);
    const jawsInterpreter = useJawsInterpreter();

    const handleRunExample = async (code: string) => {
        if (code && terminalRef.current) {
            terminalRef.current.setInput(code);
            terminalRef.current.writeInput(code);
            // Always show the terminal when running an example
            setShowTerminal(true);

            try {
                const result = await handleCommand(code);
                terminalRef.current.writeOutput(result);
            } catch (error) {
                terminalRef.current.writeOutput(`Error: ${error instanceof Error ? error.message : String(error)}`);
            }
            terminalRef.current.clearInput();
        }
    };

    const handleCommand = async (command: string) => {
        if (jawsInterpreter.loading) {
            return "Loading JAWS interpreter...";
        }

        if (jawsInterpreter.error) {
            return `Interpreter error: ${jawsInterpreter.error}`;
        }

        const result = jawsInterpreter.evaluate(command);

        if (typeof result === 'string' &&
            (result.startsWith("Error:") ||
                result.startsWith("Parse Error:") ||
                result.startsWith("Interpreter Error:") ||
                result.startsWith("[Import Error]"))) {
            console.warn("Interpreter returned error:", result);
        }

        return result;
    };

    const toggleMenu = () => {
        setShowMenu(!showMenu);
    };

    const toggleTerminal = () => {
        setShowTerminal(!showTerminal);
    };

    const toggleChapter = (chapterId: string, event: React.MouseEvent) => {
        event.stopPropagation(); // Prevent triggering the chapter selection
        setExpandedChapters(prev => ({
            ...prev,
            [chapterId]: !prev[chapterId]
        }));
    };

    const selectSection = (section: Section) => {
        setSelectedSection(section);
        // On mobile, close the menu when a section is selected
        if (window.innerWidth < 768) {
            setShowMenu(false);
        }
    };

    // Close chapters panel when clicking outside on mobile
    const handleClickOutside = (e: React.MouseEvent) => {
        // Only handle this on mobile
        if (window.innerWidth < 768 && showMenu) {
            const target = e.target as HTMLElement;
            // If click is outside the sidebar
            if (!target.closest('[data-sidebar="true"]')) {
                setShowMenu(false);
            }
        }
    };

    return (
        <main className="min-h-[calc(100vh-57px)] h-full" onClick={handleClickOutside}>
            <div className="h-full bg-gradient-to-br from-slate-900 to-blue-900 flex flex-col">
                <div className="flex border-b border-blue-700/30 bg-slate-900/80 p-4 items-center">
                    <Button
                        variant="ghost"
                        size="icon"
                        onClick={() => window.location.href = '/Jaws'}
                        className="text-white hover:bg-blue-800/30 mr-2"
                    >
                        <ChevronLeft className="h-5 w-5" />
                    </Button>
                    <h1 className="text-xl font-semibold text-white flex items-center gap-2 flex-1">
                        <Parentheses className="h-5 w-5 text-cyan-400" />
                        Learn Scheme
                    </h1>
                    <div className="flex gap-2">
                        {/* Terminal toggle button (visible on all screens) */}
                        <Button
                            variant="outline"
                            size="sm"
                            onClick={toggleTerminal}
                            className="md:hidden text-white border-[#dd3f0c] bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 hover:text-white"
                        >
                            {showTerminal ? 'Hide Terminal' : 'Show Terminal'}
                        </Button>

                        {/* Menu toggle button (only visible on mobile) */}
                        <Button
                            variant="ghost"
                            size="icon"
                            onClick={toggleMenu}
                            className="md:hidden text-white hover:bg-blue-800/30"
                        >
                            {showMenu ? <X className="h-5 w-5" /> : <Menu className="h-5 w-5" />}
                        </Button>
                    </div>
                </div>

                <div className="flex-1 flex flex-col md:flex-row overflow-hidden">
                    {/* Sidebar - hidden by default on mobile, shown when menu button is clicked */}
                    <div
                        className={`${showMenu ? 'block' : 'hidden'
                            } md:block md:w-80 border-r border-blue-700/30 bg-blue-950 md:flex md:flex-col z-10 absolute md:relative inset-0 h-[calc(100vh)] md:h-full overflow-hidden`}
                        data-sidebar="true"
                    >
                        <div className="p-3 border-b border-blue-700/30 bg-blue-950 flex-shrink-0">
                            <h2 className="text-lg font-semibold text-white">
                                Curriculum
                            </h2>
                        </div>
                        <div className="h-[calc(100%-57px)] md:flex-1 overflow-y-scroll md:overflow-y-auto scrollbar-thin scrollbar-thumb-blue-500/50 scrollbar-track-transparent">
                            {CURRICULUM.map((chapter, index) => (
                                <div
                                    key={chapter.id}
                                    className="border-b border-blue-800/30 animate-fadeScale"
                                    style={{ animationDelay: `${index * 0.1}s` }}
                                >
                                    <div
                                        className={`w-full text-left p-4 transition-colors ${selectedChapter.id === chapter.id
                                            ? 'bg-gradient-to-r from-blue-900 to-blue-800 text-white'
                                            : 'text-white hover:bg-blue-800/20'
                                            }`}
                                    >
                                        <div className="flex items-center justify-between">
                                            <button
                                                onClick={() => {
                                                    setSelectedChapter(chapter);
                                                    selectSection(chapter.sections[0]);
                                                    // Ensure chapter is expanded when selected
                                                    setExpandedChapters(prev => ({
                                                        ...prev,
                                                        [chapter.id]: true
                                                    }));
                                                }}
                                                className="flex-1 text-left"
                                            >
                                                <h3 className="font-medium text-current">{chapter.title}</h3>
                                                <p className="text-sm mt-1 text-current">{chapter.description}</p>
                                            </button>
                                            <button
                                                onClick={(e) => toggleChapter(chapter.id, e)}
                                                className="ml-2 p-1 rounded-full hover:bg-blue-700/30 text-white transition-colors"
                                                aria-label={expandedChapters[chapter.id] ? "Collapse chapter" : "Expand chapter"}
                                            >
                                                {expandedChapters[chapter.id] ?
                                                    <ChevronDown className="h-5 w-5" /> :
                                                    <ChevronRight className="h-5 w-5" />
                                                }
                                            </button>
                                        </div>
                                    </div>
                                    {expandedChapters[chapter.id] && (
                                        <div className="bg-blue-900/30 border-t border-blue-700/30">
                                            {chapter.sections.map((section) => (
                                                <button
                                                    key={section.id}
                                                    onClick={() => selectSection(section)}
                                                    className={`w-full text-left p-3 pl-8 text-sm transition-colors flex items-center ${selectedSection.id === section.id
                                                        ? 'bg-blue-800/30 text-white'
                                                        : 'text-blue-100/70 hover:bg-blue-800/30 hover:text-white'
                                                        }`}
                                                >
                                                    {selectedSection.id === section.id && (
                                                        <Parentheses className="h-3.5 w-3.5 text-[#dd3f0c] mr-1.5 flex-shrink-0" />
                                                    )}
                                                    <span className="text-current">
                                                        {section.title}
                                                    </span>
                                                </button>
                                            ))}
                                        </div>
                                    )}
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Content area - full width on mobile */}
                    <div className="flex-1 flex flex-col md:flex-row h-full overflow-hidden">
                        {/* Content section */}
                        <div className={`${showTerminal ? 'hidden' : 'block'} md:block md:w-1/2 h-full overflow-y-auto p-4 md:p-8 bg-slate-900/70 animate-fadeSlideIn`}>
                            <div className="flex items-center gap-2 mb-6">
                                <h1 className="text-xl md:text-2xl font-semibold text-white">
                                    {selectedSection.title}
                                </h1>
                                <DifficultyBadge difficulty={selectedSection.difficulty} />
                            </div>
                            <div className="prose prose-invert max-w-none text-white">
                                {Array.isArray(selectedSection.content)
                                    ? selectedSection.content.map((paragraph, idx) => (
                                        <p key={idx} className="text-blue-100/90">{paragraph}</p>
                                    ))
                                    : <p className="text-blue-100/90">{selectedSection.content}</p>}
                            </div>
                            {selectedSection.codeExamples && selectedSection.codeExamples.length > 0 && (
                                <div className="mt-6 space-y-6">
                                    {selectedSection.codeExamples.map((example, idx) => (
                                        <div key={`code-${selectedSection.id}-${idx}`} className="animate-fadeScale" style={{ animationDelay: `${idx * 0.1 + 0.2}s` }}>
                                            <Card className="bg-zinc-800/80 border-blue-700/30 overflow-hidden shadow-lg">
                                                <CardHeader className="pb-2 text-left">
                                                    <CardTitle className="tracking-tight text-lg font-semibold text-white flex items-center gap-2 justify-start">
                                                        <Parentheses className="h-4 w-4" style={{ color: "#22d3ee" }} />
                                                        {example.description}
                                                    </CardTitle>
                                                    <CardDescription className="text-blue-100 mt-1 text-base">
                                                        {example.explanation}
                                                    </CardDescription>
                                                </CardHeader>
                                                <CardContent className="border-t border-blue-800/30 p-4">
                                                    <div className="font-mono text-sm text-blue-100 overflow-x-auto">
                                                        <HighlightedText text={example.code} type="input" />
                                                    </div>
                                                    <div className="mt-4 flex justify-end">
                                                        <Button
                                                            className="bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white hover:-translate-y-1 transition-all duration-200 flex items-center gap-2 shadow-lg"
                                                            onClick={() => handleRunExample(example.code)}
                                                            disabled={jawsInterpreter.loading}
                                                        >
                                                            {jawsInterpreter.loading ? 'Loading...' : 'Run Example'} <ArrowRight className="h-4 w-4" />
                                                        </Button>
                                                    </div>
                                                </CardContent>
                                            </Card>
                                        </div>
                                    ))}
                                </div>
                            )}
                        </div>

                        {/* Terminal section - toggleable on mobile */}
                        <div className={`${!showTerminal ? 'hidden' : 'flex flex-col'} md:block md:w-1/2 h-full border-l border-blue-700/30 bg-zinc-900`}>
                            {/* Back button (only visible on mobile when terminal is shown) */}
                            <div className="p-2 border-b border-blue-700/30 md:hidden">
                                <Button
                                    variant="ghost"
                                    size="sm"
                                    onClick={() => setShowTerminal(false)}
                                    className="text-white hover:bg-blue-800/30"
                                >
                                    <ChevronLeft className="h-4 w-4 mr-1" /> Back to Content
                                </Button>
                            </div>

                            {jawsInterpreter.loading ? (
                                <div className="flex items-center justify-center h-full text-blue-300">
                                    <p>Loading JAWS interpreter...</p>
                                </div>
                            ) : jawsInterpreter.error ? (
                                <div className="flex items-center justify-center h-full text-red-400">
                                    <p>Error loading interpreter: {jawsInterpreter.error}</p>
                                </div>
                            ) : (
                                <Terminal ref={terminalRef} onCommand={handleCommand} className="h-full" />
                            )}
                        </div>
                    </div>
                </div>
            </div>
        </main>
    );
}

export default LearnScheme;

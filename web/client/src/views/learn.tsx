import { useState, useRef } from 'react';
import { Terminal, TerminalRef } from '@/components/terminal';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { ChevronLeft, ArrowRight, Parentheses } from 'lucide-react';
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
    const terminalRef = useRef<TerminalRef>(null);
    const jawsInterpreter = useJawsInterpreter();

    const handleRunExample = async (code: string) => {
        if (code && terminalRef.current) {
            terminalRef.current.setInput(code);
            terminalRef.current.writeInput(code);

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

        // Use a direct call without try-catch here since you're already handling errors in handleRunExample
        const result = jawsInterpreter.evaluate(command);

        // If the result looks like an error, log it for debugging
        if (typeof result === 'string' &&
            (result.startsWith("Error:") ||
                result.startsWith("Parse Error:") ||
                result.startsWith("Interpreter Error:") ||
                result.startsWith("[Import Error]"))) {
            console.warn("Interpreter returned error:", result);
        }

        return result;
    };

    return (
        <main className="h-[calc(100vh-57px)]">
            <div className="h-full bg-gradient-to-br from-slate-900 to-blue-900 flex flex-col">
                <div className="flex border-b border-blue-700/30 bg-slate-900/80 p-4 items-center gap-2">
                    <Button
                        variant="ghost"
                        size="icon"
                        onClick={() => window.location.href = '/'}
                        className="text-white hover:bg-blue-800/30"
                    >
                        <ChevronLeft className="h-5 w-5" />
                    </Button>
                    <h1 className="text-xl font-semibold text-white flex items-center gap-2">
                        <Parentheses className="h-5 w-5 text-cyan-400" />
                        Learn Scheme
                    </h1>
                </div>

                <div className="flex-1 flex overflow-hidden">
                    <div className="w-80 border-r border-blue-700/30 bg-slate-900/80 flex flex-col overflow-hidden">
                        <div className="p-4 border-b border-blue-700/30 bg-gradient-to-r from-slate-900 to-blue-900">
                            <h2 className="text-lg font-semibold text-white">
                                Curriculum
                            </h2>
                        </div>
                        <div className="flex-1 overflow-auto scrollbar-thin scrollbar-thumb-blue-500/50 scrollbar-track-transparent">
                            {CURRICULUM.map((chapter, index) => (
                                <div
                                    key={chapter.id}
                                    className="border-b border-blue-800/30 animate-fadeScale"
                                    style={{ animationDelay: `${index * 0.1}s` }}
                                >
                                    <button
                                        onClick={() => {
                                            setSelectedChapter(chapter);
                                            setSelectedSection(chapter.sections[0]);
                                        }}
                                        className={`w-full text-left p-4 transition-colors hover:bg-blue-800/20 ${selectedChapter.id === chapter.id
                                            ? 'bg-gradient-to-r from-blue-900 to-blue-800 text-white'
                                            : 'text-blue-100/70 hover:text-white'
                                            }`}
                                    >
                                        <h3 className="font-medium text-current">{chapter.title}</h3>
                                        <p className="text-sm mt-1 opacity-80 text-current">{chapter.description}</p>
                                    </button>
                                    {selectedChapter.id === chapter.id && (
                                        <div className="bg-blue-900/20 border-t border-blue-700/30">
                                            {chapter.sections.map((section) => (
                                                <button
                                                    key={section.id}
                                                    onClick={() => setSelectedSection(section)}
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

                    <div className="flex-1 flex h-full">
                        <div className="w-1/2 h-full overflow-y-auto p-8 bg-slate-900/70 animate-fadeSlideIn">
                            <div className="flex items-center gap-2 mb-6">
                                <h1 className="text-2xl font-semibold text-white">
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
                                <div className="mt-6 space-y-8">
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
                                                            {jawsInterpreter.loading ? 'Loading Interpreter...' : 'Run Example'} <ArrowRight className="h-4 w-4" />
                                                        </Button>
                                                    </div>
                                                </CardContent>
                                            </Card>
                                        </div>
                                    ))}
                                </div>
                            )}
                        </div>

                        <div className="w-1/2 h-full border-l border-blue-700/30 bg-zinc-900">
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

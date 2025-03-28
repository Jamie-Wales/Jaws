import { useRef, useState, useEffect } from 'react';
import { Terminal, TerminalRef } from '@/components/terminal';
import { CodeEditor } from '@/components/codeEditor';
import { Button } from '@/components/ui/button';
import { Maximize2, Minimize2, Expand, Shrink, Play } from 'lucide-react';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';
import { ResizableHandle, ResizablePanel, ResizablePanelGroup } from "@/components/ui/resizable";

export function EditorView() {
    const [code, setCode] = useState('');
    const [_, setIsLoadedFromExample] = useState(false);
    const [isEditorExpanded, setIsEditorExpanded] = useState(true);
    const [isFullscreen, setIsFullscreen] = useState(false);
    const terminalRef = useRef<TerminalRef>(null);
    const interpreter = useJawsInterpreter();

    useEffect(() => {
        const savedCode = localStorage.getItem('editorCode');
        if (savedCode) {
            setCode(savedCode);
            setIsLoadedFromExample(true);
            localStorage.removeItem('editorCode');
            terminalRef.current?.writeSystem("Example code loaded!");
        }
    }, []);

    const handleCommand = async (command: string) => {
        try {
            const result = interpreter.evaluate(command);
            return result;
        } catch (error) {
            throw new Error(error instanceof Error ? error.message : 'An error occurred');
        }
    };

    const runEditorCode = () => {
        try {
            if (!code.trim()) {
                terminalRef.current?.writeSystem("No code to run. Please write some code in the editor first.");
                return;
            }

            terminalRef.current?.writeSystem("Running code from editor...");
            const result = interpreter.evaluate(code);
            terminalRef.current?.writeOutput(result);
        } catch (error) {
            terminalRef.current?.writeOutput(`Error: ${error instanceof Error ? error.message : 'An error occurred while running editor code'}`);
        }
    };

    const toggleEditor = () => {
        setIsEditorExpanded(!isEditorExpanded);
    };

    const toggleFullscreen = () => {
        setIsFullscreen(!isFullscreen);
    };

    return (
        <div className={`${isFullscreen ? 'fixed inset-0 z-50 bg-white' : 'h-[calc(100vh-8rem)]'}`}>
            <div className="flex flex-col gap-4 h-full p-4">
                <div className="flex items-center gap-3 bg-slate-50 px-4 py-2.5 border rounded-lg shadow-sm">
                    <span className="text-sm font-medium text-slate-700">Editor</span>
                    <Button
                        variant="ghost"
                        size="sm"
                        onClick={toggleEditor}
                        className="h-7 px-2 hover:bg-slate-200/80"
                        title={isEditorExpanded ? "Minimize editor" : "Maximize editor"}
                    >
                        {isEditorExpanded ? (
                            <Minimize2 className="h-4 w-4" />
                        ) : (
                            <Maximize2 className="h-4 w-4" />
                        )}
                    </Button>
                    <Button
                        variant="ghost"
                        size="sm"
                        onClick={toggleFullscreen}
                        className="h-7 px-2 hover:bg-slate-200/80"
                        title={isFullscreen ? "Exit fullscreen" : "Enter fullscreen"}
                    >
                        {isFullscreen ? (
                            <Shrink className="h-4 w-4" />
                        ) : (
                            <Expand className="h-4 w-4" />
                        )}
                    </Button>
                    <div className="flex-1"></div>
                    {isEditorExpanded && (
                        <Button
                            variant="default"
                            size="sm"
                            onClick={runEditorCode}
                            className="h-8 px-4 flex items-center gap-2 bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white 
                                shadow-lg hover:-translate-y-0.5 transition-all duration-200"
                            title="Load editor code"
                        >
                            <Play className="h-4 w-4" />
                            Load editor code
                        </Button>
                    )}
                </div>

                <div className="flex-1 min-h-0 border rounded-lg bg-zinc-900 overflow-hidden">
                    <ResizablePanelGroup direction="vertical">
                        <ResizablePanel defaultSize={50} minSize={30} className={!isEditorExpanded ? 'hidden' : ''}>
                            <CodeEditor
                                value={code}
                                onChange={setCode}
                                height="100%"
                                fullEditor={true}
                                onRun={runEditorCode}  // Pass the run function
                            />
                        </ResizablePanel>
                        <ResizableHandle className={!isEditorExpanded ? 'hidden' : 'h-2 bg-zinc-800 hover:bg-zinc-700'} />
                        <ResizablePanel defaultSize={100} minSize={20}>
                            <Terminal
                                ref={terminalRef}
                                onCommand={handleCommand}
                                className="h-full"
                            />
                        </ResizablePanel>
                    </ResizablePanelGroup>
                </div>
            </div>
        </div>
    );
}

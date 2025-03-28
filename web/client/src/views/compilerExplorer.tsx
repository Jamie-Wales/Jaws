import { useState, useEffect } from 'react';
import { CodeEditor } from '@/components/codeEditor';
import { Button } from '@/components/ui/button';
import { Play, ChevronLeft, Parentheses, Code, ActivitySquare, GitBranch, FileCode, Layers, Network } from 'lucide-react';
import { HighlightedText } from '@/components/highlightedText';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';

const INITIAL_CODE = `(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(factorial 5)`;

interface CompilationOutput {
    ast: string;
    macroExpanded: string;
    anf: string;
    optimizedANF: string;
    threeAC: string;
    preDependencyGraph: string;
    postDependencyGraph: string;
    result: string;
}

const CompilerExplorer = () => {
    const [sourceCode, setSourceCode] = useState(INITIAL_CODE);
    const [outputs, setOutputs] = useState<CompilationOutput>({
        ast: '',
        macroExpanded: '',
        anf: '',
        optimizedANF: '',
        threeAC: '',
        preDependencyGraph: '',
        postDependencyGraph: '',
        result: ''
    });
    const [activeTab, setActiveTab] = useState('ast');
    const [isCompiling, setIsCompiling] = useState(false);
    const interpreter = useJawsInterpreter();

    useEffect(() => {
        const savedCode = localStorage.getItem('compiler-explorer-code');
        if (savedCode) {
            setSourceCode(savedCode);
            localStorage.removeItem('compiler-explorer-code');
        }
    }, []);

    useEffect(() => {
    }, [outputs, activeTab]);

    const compileCode = () => {
        if (!interpreter || interpreter.loading) return;

        setIsCompiling(true);

        try {
            const stages = interpreter.getCompilationStages(sourceCode);
            const evaluationResult = interpreter.evaluate(sourceCode);
            if (stages.error) {
                setOutputs({
                    ...outputs,
                    result: stages.error
                });
            } else {
                setOutputs({
                    ast: stages.ast,
                    macroExpanded: stages.macroExpanded,
                    anf: stages.anf,
                    optimizedANF: stages.optimizedANF,
                    threeAC: stages.threeAC,
                    preDependencyGraph: stages.preDependencyGraph,
                    postDependencyGraph: stages.postDependencyGraph,
                    result: evaluationResult
                });
            }
        } catch (error) {
            console.error("Compilation error:", error);
            setOutputs({
                ...outputs,
                result: `Error: ${error instanceof Error ? error.message : String(error)}`
            });
        } finally {
            setIsCompiling(false);
        }
    };

    const renderActiveTabContent = () => {
        switch (activeTab) {
            case 'ast':
                return <HighlightedText text={outputs.ast} type="input" key={`highlight-ast-${outputs.ast.length}`} />;
            case 'macro':
                return <HighlightedText text={outputs.macroExpanded} type="input" key={`highlight-macro-${outputs.macroExpanded.length}`} />;
            case 'anf':
                return <HighlightedText text={outputs.anf} type="input" key={`highlight-anf-${outputs.anf.length}`} />;
            case 'optimized':
                return <HighlightedText text={outputs.optimizedANF} type="input" key={`highlight-optimized-${outputs.optimizedANF.length}`} />;
            case 'threac':
                return <HighlightedText text={outputs.threeAC} type="input" key={`highlight-threac-${outputs.threeAC.length}`} />;
            case 'depgraph':
                return (
                    <div className="grid grid-cols-2 gap-4 h-full">
                        <div className="code-font text-blue-100 bg-zinc-800/80 rounded-lg p-4 overflow-auto border border-blue-700/30">
                            <h3 className="text-lg font-semibold mb-2 text-blue-300">Pre-Optimization Dependencies</h3>
                            <HighlightedText text={outputs.preDependencyGraph} type="input" key={`highlight-pre-${outputs.preDependencyGraph.length}`} />
                        </div>
                        <div className="code-font text-blue-100 bg-zinc-800/80 rounded-lg p-4 overflow-auto border border-blue-700/30">
                            <h3 className="text-lg font-semibold mb-2 text-blue-300">Post-Optimization Dependencies</h3>
                            <HighlightedText text={outputs.postDependencyGraph} type="input" key={`highlight-post-${outputs.postDependencyGraph.length}`} />
                        </div>
                    </div>
                );
            default:
                return <div>Select a tab to view compilation stage</div>;
        }
    };

    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-900 to-blue-900 flex flex-col">
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
                    <Parentheses className="h-5 w-5" style={{ color: "#22d3ee" }} />
                    Scheme Compiler Explorer
                </h1>
            </div>

            <div className="flex-1 flex flex-col min-h-0 bg-slate-900/70">
                <div className="p-4 flex flex-wrap items-center gap-3 border-b border-blue-700/30 bg-blue-900/20">
                    <Button
                        onClick={compileCode}
                        className="bg-green-600 hover:bg-green-700 text-white hover:-translate-y-1 transition-all duration-200 flex items-center gap-2 shadow-lg px-6 h-12 text-base"
                        disabled={interpreter?.loading || isCompiling}
                    >
                        <Play className={`h-5 w-5 ${(interpreter?.loading || isCompiling) ? 'animate-spin' : ''}`} />
                        {interpreter?.loading ? 'Loading...' : isCompiling ? 'Compiling...' : 'Run'}
                    </Button>
                    <div className="flex flex-wrap gap-2">
                        <Button
                            onClick={() => setActiveTab('ast')}
                            className={`h-12 px-4 text-base flex items-center gap-2 ${activeTab === 'ast'
                                ? 'bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white'
                                : 'bg-white hover:bg-white/90 text-blue-800'
                                }`}
                        >
                            <Code className="h-5 w-5" />
                            AST
                        </Button>

                        <Button
                            onClick={() => setActiveTab('macro')}
                            className={`h-12 px-4 text-base flex items-center gap-2 ${activeTab === 'macro'
                                ? 'bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white'
                                : 'bg-white hover:bg-white/90 text-blue-800'
                                }`}
                        >
                            <ActivitySquare className="h-5 w-5" />
                            Macro
                        </Button>

                        <Button
                            onClick={() => setActiveTab('anf')}
                            className={`h-12 px-4 text-base flex items-center gap-2 ${activeTab === 'anf'
                                ? 'bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white'
                                : 'bg-white hover:bg-white/90 text-blue-800'
                                }`}
                        >
                            <FileCode className="h-5 w-5" />
                            ANF
                        </Button>

                        <Button
                            onClick={() => setActiveTab('optimized')}
                            className={`h-12 px-4 text-base flex items-center gap-2 ${activeTab === 'optimized'
                                ? 'bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white'
                                : 'bg-white hover:bg-white/90 text-blue-800'
                                }`}
                        >
                            <Layers className="h-5 w-5" />
                            Optimized
                        </Button>

                        <Button
                            onClick={() => setActiveTab('threac')}
                            className={`h-12 px-4 text-base flex items-center gap-2 ${activeTab === 'threac'
                                ? 'bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white'
                                : 'bg-white hover:bg-white/90 text-blue-800'
                                }`}
                        >
                            <GitBranch className="h-5 w-5" />
                            Three-AC
                        </Button>

                        <Button
                            onClick={() => setActiveTab('depgraph')}
                            className={`h-12 px-4 text-base flex items-center gap-2 ${activeTab === 'depgraph'
                                ? 'bg-[#dd3f0c] hover:bg-[#dd3f0c]/90 text-white'
                                : 'bg-white hover:bg-white/90 text-blue-800'
                                }`}
                        >
                            <Network className="h-5 w-5" />
                            Dep. Graph
                        </Button>
                    </div>
                </div>

                <div className="border-b border-blue-700/30 p-4 h-[25%]">
                    <div className="h-full bg-zinc-800/80 rounded-lg overflow-hidden shadow-lg border border-blue-700/30">
                        <CodeEditor
                            value={sourceCode}
                            onChange={setSourceCode}
                            height="100%"
                            fullEditor={true}
                        />
                    </div>
                </div>

                <div className="flex-1 min-h-0 h-[60%] border-b border-blue-700/30 p-4">
                    <div className="h-full bg-zinc-800/80 rounded-lg overflow-auto border border-blue-700/30 p-4 code-font text-blue-100">
                        <div key={`tab-content-${activeTab}`}>
                            {renderActiveTabContent()}
                        </div>
                    </div>
                </div>

                <div className="p-4 h-[15%] overflow-auto">
                    <div className="code-font text-green-400 bg-zinc-800/80 rounded-lg p-4 h-full overflow-auto border border-blue-700/30">
                        <HighlightedText text={outputs.result} type="output" />
                    </div>
                </div>
            </div>
        </div>
    );
};

export default CompilerExplorer;

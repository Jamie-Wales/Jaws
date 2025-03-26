import { useState } from 'react';
import { CodeEditor } from '@/components/codeEditor';
import { Button } from '@/components/ui/button';
import { Play } from 'lucide-react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
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
        result: ''
    });
    const [isCompiling, setIsCompiling] = useState(false);
    const interpreter = useJawsInterpreter();

    const compileCode = () => {
        if (!interpreter || interpreter.loading) return;

        setIsCompiling(true);

        try {
            // Get the full evaluation result (which may include stdout)
            const evaluationResult = interpreter.evaluate(sourceCode);

            setOutputs({
                ast: interpreter.getAST(sourceCode),
                macroExpanded: interpreter.getMacroExpanded(sourceCode),
                anf: interpreter.getANF(sourceCode),
                optimizedANF: interpreter.getOptimizedANF(sourceCode),
                threeAC: interpreter.getThreeAC(sourceCode),
                result: evaluationResult
            });
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

    return (
        <div className="h-full flex flex-col bg-zinc-900">
            <div className="flex items-center justify-between p-4 border-b border-zinc-700/50">
                <h1 className="text-2xl font-semibold text-white">Scheme Compiler Explorer</h1>
                <Button
                    onClick={compileCode}
                    className="flex items-center gap-2 transition-all duration-200 
                             hover:scale-105 disabled:opacity-50 disabled:hover:scale-100
                             bg-accent hover:bg-accent/90"
                    disabled={interpreter?.loading || isCompiling}
                >
                    <Play className={`h-4 w-4 ${(interpreter?.loading || isCompiling) ? 'animate-spin' : ''}`} />
                    {interpreter?.loading ? 'Loading Interpreter...' : isCompiling ? 'Compiling...' : 'Compile'}
                </Button>
            </div>

            <div className="flex-1 flex flex-col min-h-0">
                {/* Source Code */}
                <div className="border-b border-zinc-700/50 p-4 h-[20%]">
                    <div className="h-full">
                        <CodeEditor
                            value={sourceCode}
                            onChange={setSourceCode}
                            height="100%"
                            fullEditor={true}
                        />
                    </div>
                </div>

                {/* Compiler Stages */}
                <div className="flex-1 min-h-0 h-[65%]">
                    <Tabs defaultValue="ast" className="h-full">
                        <div className="border-b border-zinc-700/50">
                            <TabsList className="bg-transparent w-full h-12 p-0">
                                <TabsTrigger
                                    value="ast"
                                    className="data-[state=active]:bg-zinc-800 data-[state=active]:text-white rounded-none border-r border-zinc-700/50"
                                >
                                    AST
                                </TabsTrigger>
                                <TabsTrigger
                                    value="macro"
                                    className="data-[state=active]:bg-zinc-800 data-[state=active]:text-white rounded-none border-r border-zinc-700/50"
                                >
                                    Macro
                                </TabsTrigger>
                                <TabsTrigger
                                    value="anf"
                                    className="data-[state=active]:bg-zinc-800 data-[state=active]:text-white rounded-none border-r border-zinc-700/50"
                                >
                                    ANF
                                </TabsTrigger>
                                <TabsTrigger
                                    value="optimized"
                                    className="data-[state=active]:bg-zinc-800 data-[state=active]:text-white rounded-none border-r border-zinc-700/50"
                                >
                                    Optimized
                                </TabsTrigger>
                                <TabsTrigger
                                    value="threac"
                                    className="data-[state=active]:bg-zinc-800 data-[state=active]:text-white rounded-none"
                                >
                                    3AC
                                </TabsTrigger>
                            </TabsList>
                        </div>

                        <div className="h-[calc(100%-48px)] overflow-auto p-4">
                            <TabsContent value="ast" className="mt-0 h-full">
                                <div className="code-font text-zinc-200 h-full">
                                    <HighlightedText text={outputs.ast} type="input" />
                                </div>
                            </TabsContent>

                            <TabsContent value="macro" className="mt-0 h-full">
                                <div className="code-font text-zinc-200 h-full">
                                    <HighlightedText text={outputs.macroExpanded} type="input" />
                                </div>
                            </TabsContent>

                            <TabsContent value="anf" className="mt-0 h-full">
                                <div className="code-font text-zinc-200 h-full">
                                    <HighlightedText text={outputs.anf} type="input" />
                                </div>
                            </TabsContent>

                            <TabsContent value="optimized" className="mt-0 h-full">
                                <div className="code-font text-zinc-200 h-full">
                                    <HighlightedText text={outputs.optimizedANF} type="input" />
                                </div>
                            </TabsContent>

                            <TabsContent value="threac" className="mt-0 h-full">
                                <div className="code-font text-zinc-200 h-full">
                                    <HighlightedText text={outputs.threeAC} type="input" />
                                </div>
                            </TabsContent>
                        </div>
                    </Tabs>
                </div>

                {/* Output */}
                <div className="border-t border-zinc-700/50 p-4 h-[15%] overflow-auto">
                    <div className="code-font text-green-400">
                        <HighlightedText text={outputs.result} type="output" />
                    </div>
                </div>
            </div>
        </div>
    );
};

export default CompilerExplorer;

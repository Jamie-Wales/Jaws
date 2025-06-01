import { useState, useEffect } from "react";

interface JawsEvaluationResult {
    result?: string;
    stdout?: string;
    error?: string;
}

interface CompilationStages {
    ast: string;
    macroExpanded: string;
    anf: string;
    optimizedANF: string;
    threeAC: string;
    preDependencyGraph: string;
    postDependencyGraph: string;
    error?: string;
}

interface JawsInstance {
    evaluate: (input: string) => JawsEvaluationResult;
    getAllStages: (input: string) => CompilationStages;
}

interface JawsModule {
    JawsWrapper: new () => JawsInstance;
}

declare global {
    interface Window {
        createJawsModule?: () => Promise<JawsModule>;
    }
}

interface JawsInterpreter {
    evaluate: (input: string) => string;
    getCompilationStages: (input: string) => CompilationStages;
    loading: boolean;
    error: string | null;
    instance: JawsInstance | null;
}

const useJawsInterpreter = (): JawsInterpreter => {
    const [interpreter, setInterpreter] = useState<JawsInstance | null>(null);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const initializeInterpreter = async () => {
            try {
                const checkModule = () => {
                    return new Promise<void>((resolve) => {
                        const check = () => {
                            if (typeof window.createJawsModule === 'function') {
                                resolve();
                            } else {
                                setTimeout(check, 100);
                            }
                        };
                        check();
                    });
                };
                await checkModule();
                if (!window.createJawsModule) {
                    throw new Error('WASM module not loaded');
                }
                const module = await window.createJawsModule();
                const jawsInstance = new module.JawsWrapper();
                setInterpreter(jawsInstance);
            } catch (err) {
                setError(err instanceof Error ? err.message : 'Failed to initialize WASM module');
            } finally {
                setLoading(false);
            }
        };
        initializeInterpreter();
    }, []);

    const getCompilationStages = (input: string): CompilationStages => {
        if (!interpreter) {
            return {
                ast: 'Interpreter not initialized',
                macroExpanded: 'Interpreter not initialized',
                anf: 'Interpreter not initialized',
                optimizedANF: 'Interpreter not initialized',
                threeAC: 'Interpreter not initialized',
                preDependencyGraph: 'Interpreter not initialized',
                postDependencyGraph: 'Interpreter not initialized'
            };
        }

        try {
            return interpreter.getAllStages(input);
        } catch (err) {
            const errorMsg = `Error: ${err instanceof Error ? err.message : 'Unknown error'}`;
            return {
                ast: errorMsg,
                macroExpanded: errorMsg,
                anf: errorMsg,
                optimizedANF: errorMsg,
                threeAC: errorMsg,
                preDependencyGraph: errorMsg,
                postDependencyGraph: errorMsg,
                error: errorMsg
            };
        }
    };

    const evaluate = (input: string): string => {
        if (!interpreter) return 'Interpreter not initialized';
        try {
            const result = interpreter.evaluate(input);
            if (result.error) {
                return result.error;
            }
            if (result.stdout && result.stdout.trim() !== '') {
                return result.stdout.trim();
            }
            return result.result || '';
        } catch (err) {
            console.error(`evaluate error:` + err);
            return `Error: ${err instanceof Error ? err.message : 'Unknown error'}`;
        }
    };

    return {
        evaluate,
        getCompilationStages,
        loading,
        error,
        instance: interpreter
    };
};

export default useJawsInterpreter;

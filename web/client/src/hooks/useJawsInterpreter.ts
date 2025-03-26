import { useState, useEffect } from "react";

interface JawsEvaluationResult {
    result?: string;
    stdout?: string;
    error?: string;
}

interface JawsInstance {
    evaluate: (input: string) => JawsEvaluationResult;
    getMacroExpanded: (input: string) => string;
    getANF: (input: string) => string;
    getOptimizedANF: (input: string) => string;
    getThreeAC: (input: string) => string;
    getAST: (input: string) => string;
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
    evaluateWithOutput: (input: string) => JawsEvaluationResult;
    getMacroExpanded: (input: string) => string;
    getANF: (input: string) => string;
    getOptimizedANF: (input: string) => string;
    getThreeAC: (input: string) => string;
    getAST: (input: string) => string;
    loading: boolean;
    error: string | null;
    instance: JawsInstance | null;
}

type JawsStringFunctionName = keyof Pick<JawsInstance,
    'getMacroExpanded' |
    'getANF' |
    'getOptimizedANF' |
    'getThreeAC' |
    'getAST'
>;

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

    const wrapStringFunction = (fn: JawsStringFunctionName) => (input: string): string => {
        if (!interpreter) return 'Interpreter not initialized';
        try {
            return interpreter[fn](input);
        } catch (err) {
            console.error(`${fn} error:`, err);
            return `Error: ${err instanceof Error ? err.message : 'Unknown error'}`;
        }
    };

    const evaluate = (input: string): string => {
        if (!interpreter) return 'Interpreter not initialized';
        try {
            const result = interpreter.evaluate(input);

            // Handle error returned from C++
            if (result.error) {
                return result.error;
            }

            // If there's stdout output, prioritize it over the result
            if (result.stdout && result.stdout.trim() !== '') {
                return result.stdout.trim();
            }

            // Otherwise return the result
            return result.result || '';
        } catch (err) {
            console.error(`evaluate error:`, err);
            return `Error: ${err instanceof Error ? err.message : 'Unknown error'}`;
        }
    };

    const evaluateWithOutput = (input: string): JawsEvaluationResult => {
        if (!interpreter) return { error: 'Interpreter not initialized' };
        try {
            return interpreter.evaluate(input);
        } catch (err) {
            console.error(`evaluate error:`, err);
            return { error: `Error: ${err instanceof Error ? err.message : 'Unknown error'}` };
        }
    };

    return {
        evaluate,
        evaluateWithOutput,
        getMacroExpanded: wrapStringFunction('getMacroExpanded'),
        getANF: wrapStringFunction('getANF'),
        getOptimizedANF: wrapStringFunction('getOptimizedANF'),
        getThreeAC: wrapStringFunction('getThreeAC'),
        getAST: wrapStringFunction('getAST'),
        loading,
        error,
        instance: interpreter
    };
};

export default useJawsInterpreter;

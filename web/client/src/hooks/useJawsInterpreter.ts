import { useState, useEffect } from "react";

declare global {
    interface Window {
        createJawsModule?: () => Promise<{
            JawsWrapper: new () => {
                evaluate: (input: string) => string;
                getEnvironment: () => string;
                setOption?: (option: string, value: boolean) => void;
            };
        }>;
    }
}

const useJawsInterpreter = () => {
    const [interpreter, setInterpreter] = useState<any>(null);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const initializeInterpreter = async () => {
            try {
                // Wait for the WebAssembly module to load
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

    const evaluate = (input: string): string => {
        if (!interpreter) return 'Interpreter not initialized';
        try {
            return interpreter.evaluate(input);
        } catch (err) {
            console.error("Evaluation error:", err);
            return `Evaluation error: ${err instanceof Error ? err.message : 'Unknown error'}`;
        }
    };

    // Add option setter if available
    const setOption = (option: string, value: boolean): void => {
        if (interpreter && typeof interpreter.setOption === 'function') {
            interpreter.setOption(option, value);
        }
    };

    return {
        evaluate,
        setOption,
        loading,
        error,
        instance: interpreter
    };
};

export default useJawsInterpreter;

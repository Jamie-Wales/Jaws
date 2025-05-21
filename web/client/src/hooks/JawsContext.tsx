import React, { createContext, useState, useEffect, useContext, ReactNode, useMemo, useCallback } from 'react';

// Define the shape of the context value (similar to your hook's return type)
interface JawsContextValue {
    instance: JawsInstance | null;
    loading: boolean;
    error: string | null;
    evaluate: (input: string) => string; // Simplified evaluate signature for context
    getCompilationStages: (input: string) => CompilationStages;
}

// Define the interfaces (copy from your hook file or import if shared)
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

// Default error stage object
const defaultErrorStages: CompilationStages = {
    ast: 'Interpreter not initialized or loading',
    macroExpanded: 'Interpreter not initialized or loading',
    anf: 'Interpreter not initialized or loading',
    optimizedANF: 'Interpreter not initialized or loading',
    threeAC: 'Interpreter not initialized or loading',
    preDependencyGraph: 'Interpreter not initialized or loading',
    postDependencyGraph: 'Interpreter not initialized or loading',
    error: 'Interpreter not initialized or loading'
};

// Create the Context with a default value
const JawsContext = createContext<JawsContextValue>({
    instance: null,
    loading: true,
    error: null,
    evaluate: () => 'Interpreter not initialized or loading',
    getCompilationStages: () => defaultErrorStages,
});

// Create the Provider Component
interface JawsProviderProps {
    children: ReactNode;
}

export const JawsProvider: React.FC<JawsProviderProps> = ({ children }) => {
    const [instance, setInstance] = useState<JawsInstance | null>(null);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        // This effect runs only ONCE when the Provider mounts
        const initializeInterpreter = async () => {
            console.log("Attempting to initialize JAWS WASM..."); // Log initialization attempt
            setLoading(true);
            setError(null);
            try {
                // Enhanced check with timeout
                const checkModule = (timeout = 5000) => { // 5 second timeout
                    return new Promise<void>((resolve, reject) => {
                        const startTime = Date.now();
                        const check = () => {
                            if (typeof window.createJawsModule === 'function') {
                                console.log("createJawsModule found.");
                                resolve();
                            } else if (Date.now() - startTime > timeout) {
                                console.error("createJawsModule timeout.");
                                reject(new Error('WASM module loader timed out.'));
                            }
                            else {
                                // console.log("Checking for createJawsModule..."); // Optional: verbose logging
                                setTimeout(check, 100); // Check again shortly
                            }
                        };
                        check();
                    });
                };

                await checkModule(); // Wait for the function to appear or timeout

                // Redundant check after await, but safe
                if (!window.createJawsModule) {
                    throw new Error('WASM module function (createJawsModule) not found on window.');
                }

                console.log("Calling createJawsModule()...");
                const module = await window.createJawsModule();
                console.log("JawsModule loaded, creating JawsWrapper instance...");
                const jawsInstance = new module.JawsWrapper();
                console.log("JawsWrapper instance created successfully.");
                setInstance(jawsInstance);
            } catch (err) {
                console.error("Error initializing JAWS WASM:", err);
                setError(err instanceof Error ? err.message : 'Failed to initialize WASM module');
                setInstance(null); // Ensure instance is null on error
            } finally {
                console.log("Initialization finished. Loading: false.");
                setLoading(false);
            }
        };

        // Only initialize if not already initialized (or errored and retrying perhaps)
        if (!instance && !error) {
            initializeInterpreter();
        }

        // No cleanup needed that would destroy the instance
    }, [instance, error]); // Dependency array ensures this runs only if instance/error changes externally, which it shouldn't

    // Define evaluate and getCompilationStages based on the current state
    // Use useCallback to prevent these functions from changing on every render unless necessary
    const evaluate = useCallback((input: string): string => {
        if (loading) return 'Interpreter loading...';
        if (error) return `Interpreter Error: ${error}`;
        if (!instance) return 'Interpreter not available.';

        try {
            const result = instance.evaluate(input);
            if (result.error) {
                // Check specifically for continuation error message
                if (result.error.startsWith("Error: Continuation invoked")) {
                    console.warn("Continuation Invocation during evaluate:", result.error);
                    // Return just the value part if desired, or the full error
                    return result.error; // Or potentially parse out the value if needed
                }
                return result.error;
            }
            // Prioritize stdout if present
            if (result.stdout && result.stdout.trim() !== '') {
                // If there's also a result, maybe combine them?
                const finalResult = result.result && result.result !== 'undefined' ? `${result.result}` : '';
                return `${result.stdout.trim()}${finalResult ? `\n${finalResult}` : ''}`;
            }
            return result.result || ''; // Default to result if no stdout
        } catch (err) {
            // Catch potential errors within the WASM call itself
            console.error("Exception during JawsWrapper.evaluate:", err);
            // Check for specific continuation exception from C++ side if not caught above
            if (err instanceof Error && err.message.includes("ContinuationInvocationException")) {
                console.warn("Caught Continuation Invocation Exception in evaluate wrapper");
                // Handle as needed, maybe return a specific message or value
                return "Continuation Invoked (evaluate)"; // Placeholder
            }
            return `Runtime Error: ${err instanceof Error ? err.message : 'Unknown error'}`;
        }
    }, [instance, loading, error]);

    const getCompilationStages = useCallback((input: string): CompilationStages => {
        if (loading) return { ...defaultErrorStages, error: 'Interpreter loading...' };
        if (error) return { ...defaultErrorStages, error: `Interpreter Error: ${error}` };
        if (!instance) return { ...defaultErrorStages, error: 'Interpreter not available.' };

        try {
            const stages = instance.getAllStages(input);
            if (stages.error) {
                console.error("Error reported from getAllStages:", stages.error);
            }
            // Return the stages object, potentially including an error field from C++
            return stages;
        } catch (err) {
            // Catch potential errors within the WASM call itself
            console.error("Exception during JawsWrapper.getAllStages:", err);
            // Check for specific continuation exception from C++ side
            if (err instanceof Error && err.message.includes("ContinuationInvocationException")) {
                console.warn("Caught Continuation Invocation Exception in getCompilationStages wrapper");
                // Return error object indicating continuation
                return {
                    ...defaultErrorStages,
                    error: "Continuation invoked during compilation analysis"
                };
            }
            const errorMsg = `Runtime Error: ${err instanceof Error ? err.message : 'Unknown error'}`;
            return { ...defaultErrorStages, error: errorMsg };
        }
    }, [instance, loading, error]);

    // Memoize the context value to prevent unnecessary re-renders of consumers
    const contextValue = useMemo(() => ({
        instance,
        loading,
        error,
        evaluate,
        getCompilationStages,
    }), [instance, loading, error, evaluate, getCompilationStages]);

    return (
        <JawsContext.Provider value={contextValue}>
            {children}
        </JawsContext.Provider>
    );
};

// Custom hook to consume the context easily
export const useJaws = (): JawsContextValue => {
    const context = useContext(JawsContext);
    if (context === undefined) {
        throw new Error('useJaws must be used within a JawsProvider');
    }
    return context;
};

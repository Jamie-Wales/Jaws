import { useJaws } from './JawsContext'; // Adjust the import path based on your project structure

export interface CompilationStages {
    ast: string;
    macroExpanded: string;
    anf: string;
    optimizedANF: string;
    threeAC: string;
    preDependencyGraph: string;
    postDependencyGraph: string;
    error?: string;
}

export interface JawsEvaluationResult {
    result?: string;
    stdout?: string;
    error?: string;
}

export interface JawsInstance {
    evaluate: (input: string) => JawsEvaluationResult;
    getAllStages: (input: string) => CompilationStages; // Ensure this matches C++ return type signature
}

export interface JawsInterpreter {
    /**
     * Evaluates the given Scheme code string.
     * @param input - The Scheme code to evaluate.
     * @returns A string representing the result, captured stdout, or an error message.
     */
    evaluate: (input: string) => string;

    /**
     * Gets the output from various compilation stages for the given Scheme code.
     * @param input - The Scheme code to analyze.
     * @returns A CompilationStages object containing strings for each stage, or error messages.
     */
    getCompilationStages: (input: string) => CompilationStages;

    loading: boolean;

    error: string | null;

    instance: JawsInstance | null;
}

/**
 * Custom React hook to access the shared JAWS Scheme interpreter.
 *
 * This hook consumes the JawsContext, which is managed by JawsProvider.
 * JawsProvider ensures the underlying WASM module and JawsInstance are
 * initialized only once for the application lifetime.
 *
 * @returns {JawsInterpreter} An object containing the interpreter's state (`loading`, `error`, `instance`)
 * and its core functions (`evaluate`, `getCompilationStages`).
 * @throws {Error} If used outside of a `<JawsProvider>`.
 */
const useJawsInterpreter = (): JawsInterpreter => {
    const jawsContext = useJaws();

    return {
        evaluate: jawsContext.evaluate,
        getCompilationStages: jawsContext.getCompilationStages,
        loading: jawsContext.loading,
        error: jawsContext.error,
        instance: jawsContext.instance,
    };
};
export default useJawsInterpreter;

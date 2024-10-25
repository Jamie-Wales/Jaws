declare module 'createJawsModule' {
    export interface JawsWrapper {
        evaluate: (input: string) => string;
        getEnvironment: () => string;
    }

    export interface CreateJawsModule {
        (): Promise<{ JawsWrapper: { new(): JawsWrapper } }>;
    }

    const createJawsModule: CreateJawsModule;
    export default createJawsModule;
}

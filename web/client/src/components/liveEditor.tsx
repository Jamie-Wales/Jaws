import { CodeEditor } from "./codeEditor";

import { KeyboardEvent as ReactKeyboardEvent } from 'react';

interface LiveEditorProps {
    value: string;
    onChange: (value: string) => void;
    onKeyDown?: (e: ReactKeyboardEvent<HTMLDivElement>) => Promise<void> | void;
}

export const LiveEditor = ({ value, onChange, onKeyDown }: LiveEditorProps) => {
    return (
        <div
            className="flex items-start gap-2 text-sm w-full"
            onKeyDown={onKeyDown}
        >
            <span className="text-blue-400 shrink-0 mt-[3px]">λ</span>
            <div className="flex-1 min-w-0">
                <CodeEditor
                    value={value}
                    onChange={onChange}
                    height="auto"
                    fullEditor={false}
                    minHeight="24px"
                    maxHeight="150px"
                />
            </div>
        </div>)
}

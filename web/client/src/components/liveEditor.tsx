import { CodeEditor } from "./codeEditor";

interface LiveEditorProps {
    value: string;
    onChange: (value: string) => void;
}

export const LiveEditor = ({ value, onChange }: LiveEditorProps) => {
    return (
        <div className="flex items-start gap-2 text-sm w-full">
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
        </div>
    );
};

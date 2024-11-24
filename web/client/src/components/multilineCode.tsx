import { CodeLine } from '@/types/types';
import { HighlightedText } from './highlightedText';

interface MultiLineCodeProps {
    code: CodeLine[];
    className?: string;
}

export function MultiLineCode({ code, className = '' }: MultiLineCodeProps) {
    return (
        <div className={`font-mono text-sm space-y-2 code-font ${className}`}>
            {code.map((line, i) => (
                <div key={i} className="flex items-start gap-2">
                    {line.type === 'input' ? (
                        <span className="text-cyan-400 shrink-0 select-none">λ</span>
                    ) : (
                        <span className="ml-4" />
                    )}
                    <div className="flex-1 group">
                        <HighlightedText
                            text={line.content}
                            type={line.type}
                        />
                    </div>
                </div>
            ))}
        </div>
    );
}

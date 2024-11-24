import { useRef, useEffect } from 'react';
import hljs from 'highlight.js/lib/core';
import schemeLanguage from 'highlight.js/lib/languages/scheme';
import 'highlight.js/styles/github-dark-dimmed.css';

// Register Scheme language once
hljs.registerLanguage('scheme', schemeLanguage);

interface HighlightedTextProps {
    text: string;
    type: 'input' | 'output' | 'system';
}

export const HighlightedText = ({ text, type }: HighlightedTextProps) => {
    const ref = useRef<HTMLPreElement>(null);

    useEffect(() => {
        if (ref.current && (type === 'input' || type === 'output')) {
            hljs.highlightElement(ref.current);
        }
    }, [text, type]);

    if (type === 'system') {
        return (
            <pre className="text-teal-400 font-mono opacity-80 code-font">
                {text}
            </pre>
        );
    }

    const baseClass = type === 'output' ? 'text-green-400' : 'text-zinc-200';

    return (
        <pre
            ref={ref}
            className={`hljs language-scheme font-mono whitespace-pre-wrap code-font ${baseClass}`}
            style={{ background: 'transparent' }}
        >
            {text}
        </pre>
    );
};

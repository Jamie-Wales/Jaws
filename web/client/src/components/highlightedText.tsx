import { useRef, useEffect, useCallback } from 'react';
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
    const hasHighlighted = useRef(false);

    const highlight = useCallback(() => {
        if (ref.current && !hasHighlighted.current && (type === 'input' || type === 'output')) {
            try {
                hljs.highlightElement(ref.current);
                hasHighlighted.current = true;
            } catch (error) {
                console.error('Highlighting failed:', error);
            }
        }
    }, [type]);

    useEffect(() => {
        hasHighlighted.current = false;
        highlight();
        return () => {
            hasHighlighted.current = false;
        };
    }, [text, highlight]);

    if (type === 'system') {
        return (
            <pre className="text-teal-400 font-mono text-sm">
                {text}
            </pre>
        );
    }

    const baseClass = type === 'output' ? 'text-green-400' : 'text-zinc-200';

    return (
        <pre
            ref={ref}
            className={`hljs language-scheme font-mono text-sm whitespace-pre-wrap ${baseClass}`}
            style={{ background: 'transparent' }}
        >
            {text}
        </pre>
    );
};

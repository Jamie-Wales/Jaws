
import { useRef, useEffect } from 'react';
import hljs from 'highlight.js/lib/core';
import 'highlight.js/styles/github-dark-dimmed.css';

interface HighlightedTextProps {
    text: string;
    type: 'input' | 'output' | 'system';
}

export const HighlightedText = ({ text, type }: HighlightedTextProps) => {
    const ref = useRef < HTMLPreElement > (null);

    useEffect(() => {
        if (ref.current && (type === 'input' || type === 'output')) {
            hljs.highlightElement(ref.current);
        }
    }, [text, type]);

    if (type === 'system') {
        return <pre className="text-teal-400 font-mono opacity-80">{text}</pre>;
    }

    return (
        <pre
            ref={ref}
            className="hljs language-scheme font-mono whitespace-pre-wrap"
            style={{ background: 'transparent' }}
        >
            {type === 'input' ? '❯ ' : ''}{text}
        </pre>
    );
};

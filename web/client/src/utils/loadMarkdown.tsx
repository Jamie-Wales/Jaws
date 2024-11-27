// src/utils/loadMarkdown.tsx
import { useState, useEffect } from 'react';
import ReactMarkdown from 'react-markdown';
import gfm from 'remark-gfm';

interface LoadMarkdownProps {
    filePath: string;
}

export const LoadMarkdown = ({ filePath }: LoadMarkdownProps) => {
    const [content, setContent] = useState('');

    useEffect(() => {
        fetch(filePath)
            .then(response => response.text())
            .then(text => setContent(text))
            .catch(error => console.error('Error loading markdown:', error));
    }, [filePath]);

    return (
        <ReactMarkdown className="markdown" remarkPlugins={[gfm]}>
            {content}
        </ReactMarkdown>
    );
};

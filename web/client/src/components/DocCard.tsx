import { useNavigate } from 'react-router-dom';
import { useEffect, useRef } from 'react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Code, ArrowRight } from 'lucide-react';
import { HighlightedText } from '@/components/highlightedText';
import { SchemeDoc } from '@/components/examples';

interface DocCardProps {
    doc: SchemeDoc;
    onTryExample: (code: string) => void;
}

export function DocCard({ doc, onTryExample }: DocCardProps) {
    const navigate = useNavigate();
    const codeContainerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
    }, [doc.name, doc.example]);

    const handleOpenInCompiler = () => {
        localStorage.setItem('compiler-explorer-code', doc.example);
        navigate('/compiler-explorer');
    };

    return (
        <Card className="bg-white text-slate-800 border border-slate-200 shadow-lg transition-all duration-300 hover:-translate-y-1">
            <CardHeader className="pb-4">
                <div className="flex items-start justify-between">
                    <div>
                        <div className="flex items-baseline gap-3">
                            <CardTitle className="text-xl font-semibold text-slate-900 font-mono">
                                {doc.name}
                            </CardTitle>
                            <span className="text-sm text-slate-500 font-mono">
                                {doc.signature}
                            </span>
                        </div>
                        <CardDescription className="text-base text-slate-600 mt-2">
                            {doc.description}
                        </CardDescription>
                    </div>
                    <span className="text-sm font-medium px-2.5 py-0.5 rounded bg-slate-100 text-slate-600">
                        {doc.category}
                    </span>
                </div>
            </CardHeader>
            <CardContent className="pt-0 space-y-4">
                <div
                    ref={codeContainerRef}
                    className="bg-zinc-900 rounded-lg p-4 font-mono text-sm overflow-x-auto"
                >
                    <HighlightedText
                        key={`highlight-${doc.name}-${doc.category}`}
                        text={doc.example}
                        type="input"
                    />
                </div>
                <div className="bg-slate-50 rounded-lg p-4 font-mono text-sm border border-slate-200">
                    <div className="text-slate-500 mb-1 text-xs uppercase tracking-wide">Output:</div>
                    <div className="text-slate-700">
                        {doc.output}
                    </div>
                </div>
                <div className="flex flex-wrap gap-3">
                    <Button
                        onClick={() => onTryExample(doc.example)}
                        className="bg-[#dd3f0c] text-white hover:opacity-90 flex items-center gap-2 
                                transition-all duration-200 hover:-translate-y-1 text-base font-medium"
                    >
                        Try It <ArrowRight className="h-4 w-4" />
                    </Button>
                    <Button
                        onClick={handleOpenInCompiler}
                        className="bg-blue-900 text-white hover:bg-blue-800 flex items-center gap-2
                                transition-all duration-200 hover:-translate-y-1 text-base font-medium"
                    >
                        <Code className="h-4 w-4" /> Compiler Explorer
                    </Button>
                </div>
            </CardContent>
        </Card>
    );
}

import { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { SchemeDoc, SCHEME_DOCS } from '@/components/examples';
import { DocCard } from '@/components/DocCard';
import { Button } from '@/components/ui/button';
import { Footer } from '@/components/footer';
import useJawsInterpreter from '@/hooks/useJawsInterpreter';

export function DocsView() {
    const navigate = useNavigate();
    const [selectedCategory, setSelectedCategory] = useState<SchemeDoc['category'] | 'All'>('All');
    const jawsInterpreter = useJawsInterpreter();

    const handleTryExample = (code: string) => {
        localStorage.setItem('editorCode', code);
        navigate('/editor');
    };

    const filteredDocs = selectedCategory === 'All'
        ? SCHEME_DOCS
        : SCHEME_DOCS.filter(doc => doc.category === selectedCategory);

    const categories: Array<SchemeDoc['category'] | 'All'> = [
        'All', 'Math', 'List', 'String', 'IO', 'Control', 'Vector', 'Predicate', 'Syntax'
    ];

    useEffect(() => {
    }, [selectedCategory]);

    return (
        <div className="min-h-screen bg-white">
            <div className="container mx-auto px-4 py-12">
                <div className="space-y-6">
                    <div className="flex flex-col gap-2 animate-fadeSlideIn">
                        <h1 className="text-2xl md:text-3xl font-semibold text-slate-900">
                            Standard Library Documentation
                        </h1>
                        <p className="text-base text-slate-600">
                            Explore Scheme's standard library functions with interactive examples.
                        </p>
                        {jawsInterpreter.loading && (
                            <p className="text-sm text-blue-600">Loading JAWS interpreter...</p>
                        )}
                        {jawsInterpreter.error && (
                            <p className="text-sm text-red-600">Error loading interpreter: {jawsInterpreter.error}</p>
                        )}
                    </div>
                    <div className="flex gap-2 flex-wrap">
                        {categories.map(category => (
                            <Button
                                key={category}
                                onClick={() => setSelectedCategory(category)}
                                variant={selectedCategory === category ? "default" : "outline"}
                                className={selectedCategory === category
                                    ? "bg-[#dd3f0c] text-white hover:opacity-90"
                                    : ""}
                            >
                                {category}
                            </Button>
                        ))}
                    </div>
                    <div className="grid gap-6">
                        {filteredDocs.map((doc, index) => (
                            <div
                                key={`${selectedCategory}-${doc.name}-${index}`}
                                className="animate-fadeScale"
                                style={{ animationDelay: `${index * 0.1}s` }}
                            >
                                <DocCard
                                    doc={doc}
                                    onTryExample={handleTryExample}
                                    key={`doc-${selectedCategory}-${doc.name}`}
                                />
                            </div>
                        ))}
                    </div>
                </div>
            </div>
            <Footer />
        </div>
    );
}

// The DocCard component is updated separately

import { useNavigate } from 'react-router-dom';
import { SCHEME_EXAMPLES } from '@/components/examples';
import { ExampleCard } from '@/components/ExampleCardsProps'

export function ExamplesView() {
    const navigate = useNavigate();

    const handleTryExample = (code: string) => {
        localStorage.setItem('editorCode', code);
        navigate('/editor');
    };

    return (
        <div className="min-h-screen bg-white">
            <div className="container mx-auto px-4 py-12">
                <div className="space-y-6">
                    <div className="flex flex-col gap-2 animate-fadeSlideIn">
                        <h1 className="text-2xl md:text-3xl font-semibold text-slate-900">
                            Example Programs
                        </h1>
                        <p className="text-base text-slate-600">
                            Learn Scheme through practical examples. Click "Try It" to load any example into the editor.
                        </p>
                    </div>
                    <div className="grid gap-6">
                        {SCHEME_EXAMPLES.map((example, index) => (
                            <div
                                key={index}
                                className="animate-fadeScale"
                                style={{ animationDelay: `${index * 0.1}s` }}
                            >
                                <ExampleCard
                                    example={example}
                                    onTryExample={handleTryExample}
                                />
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}

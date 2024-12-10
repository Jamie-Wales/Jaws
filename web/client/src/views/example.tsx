import { useNavigate } from 'react-router-dom';
import { SCHEME_EXAMPLES } from '@/components/examples';
import { ExampleCard } from '@/components/ExampleCardsProps'

export function ExamplesView() {
    const navigate = useNavigate();

    const handleTryExample = (code: string) => {
        localStorage.setItem('editorCode', code);
        navigate('/Jaws/editor');
    };

    return (
        <div className="min-h-screen bg-white text-gray-800">
            <div className="container mx-auto px-4 py-12">
                <div className="space-y-6">
                    <div className="flex flex-col gap-2">
                        <h1 className="text-2xl md:text-3xl font-bold text-gray-900">
                            Example Programs
                        </h1>
                        <p className="text-gray-600">
                            Learn Scheme through practical examples. Click "Try It" to load any example into the editor.
                        </p>
                    </div>

                    <div className="grid gap-6 animate-[fadeScale_0.6s_ease-out_forwards]">
                        {SCHEME_EXAMPLES.map((example, index) => (
                            <div
                                key={index}
                                style={{
                                    opacity: 0,
                                    animation: `fadeScale 0.6s ease-out forwards`,
                                    animationDelay: `${index * 0.1}s`
                                }}
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

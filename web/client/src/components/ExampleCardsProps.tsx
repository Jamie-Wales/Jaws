import { SchemeExample } from "./examples";
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from "./ui/card";
import { Button } from "./ui/button";
import { HighlightedText } from "./highlightedText";
import { ArrowRight } from "lucide-react";
import { DifficultyBadge } from "./DifficultyBadge";

interface ExampleCardProps {
    example: SchemeExample;
    onTryExample: (code: string) => void;
}

export function ExampleCard({ example, onTryExample }: ExampleCardProps) {
    return (
        <Card className="bg-white text-slate-800 border border-slate-200 shadow-lg transition-all duration-300 hover:-translate-y-1">
            <CardHeader className="pb-4">
                <div className="flex items-start justify-between">
                    <div>
                        <CardTitle className="text-xl font-semibold text-slate-900 mb-1">
                            {example.name}
                        </CardTitle>
                        <CardDescription className="text-base text-slate-600">
                            {example.description}
                        </CardDescription>
                    </div>
                    <DifficultyBadge difficulty={example.difficulty} />
                </div>
            </CardHeader>
            <CardContent className="pt-0 space-y-4">
                <div className="bg-zinc-900 rounded-lg p-4 font-mono text-sm overflow-x-auto">
                    <HighlightedText text={example.code} type="input" />
                </div>
                <Button
                    onClick={() => onTryExample(example.code)}
                    className="bg-[#dd3f0c] text-white hover:opacity-90 flex items-center gap-2 
                             transition-all duration-200 hover:-translate-y-1 text-base font-medium"
                >
                    Try It <ArrowRight className="h-4 w-4" />
                </Button>
            </CardContent>
        </Card>
    );
}

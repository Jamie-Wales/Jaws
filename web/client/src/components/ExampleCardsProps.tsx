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
        <Card className="bg-white text-gray-800 border border-gray-200 shadow-lg transition-all duration-300 hover:-translate-y-1">
            <CardHeader className="border-b border-gray-300">
                <div className="flex items-start justify-between">
                    <div>
                        <CardTitle className="text-gray-900">{example.name}</CardTitle>
                        <CardDescription className="text-gray-600">
                            {example.description}
                        </CardDescription>
                    </div>
                    <DifficultyBadge difficulty={example.difficulty} />
                </div>
            </CardHeader>
            <CardContent className="space-y-4">
                <div className="bg-gray-900 text-white rounded-lg p-4 shadow-inner overflow-x-auto">
                    <HighlightedText text={example.code} type="input" />
                </div>
                <Button
                    onClick={() => onTryExample(example.code)}
                    className="bg-[#dd3f0c] text-white hover:opacity-90 flex items-center gap-2 px-4 py-2 rounded-md 
                             transition-all duration-200 hover:-translate-y-1"
                >
                    Try It <ArrowRight className="h-4 w-4" />
                </Button>
            </CardContent>
        </Card>
    );
}

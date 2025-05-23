interface DifficultyBadgeProps {
    difficulty: 'Beginner' | 'Intermediate' | 'Advanced';
}

export function DifficultyBadge({ difficulty }: DifficultyBadgeProps) {
    const colors = {
        'Beginner': 'text-green-600 bg-green-50',
        'Intermediate': 'text-blue-600 bg-blue-50',
        'Advanced': 'text-purple-600 bg-purple-50',
    } as const;

    return (
        <span className={`px-3 py-1 rounded-full text-sm font-medium ${colors[difficulty]}`}>
            {difficulty}
        </span>
    );
}

export interface Example {
    name: string;
    code: string;
    description: string;
    difficulty: 'Beginner' | 'Intermediate' | 'Advanced';
}

export interface CodeLine {
    type: 'input' | 'output';
    content: string;
}

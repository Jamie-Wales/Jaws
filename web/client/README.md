# Jaws Scheme Web REPL

A modern web-based REPL (Read-Eval-Print Loop) for the Jaws Scheme interpreter, built with React, TypeScript, and Vite.

## Features

- 🚀 Interactive REPL with syntax highlighting
- 📝 Built-in code editor for larger programs
- 🎨 Modern, dark-themed UI using shadcn/ui
- 🔍 Real-time error reporting
- 📚 Example programs and documentation
- ⚡ Fast evaluation with immediate feedback

## Getting Started

### Prerequisites

- Node.js 16.x or higher
- npm or yarn

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/jaws-web
cd jaws-web
```

2. Install dependencies:
```bash
npm install
# or
yarn
```

3. Start the development server:
```bash
npm run dev
# or
yarn dev
```

The application will be available at `http://localhost:5173`

## Usage

### REPL Mode

The REPL provides an interactive environment where you can:
- Type Scheme expressions directly
- See immediate evaluation results
- View syntax-highlighted code
- Multi-line editing with autocompletion

Example usage:
```scheme
;; Basic arithmetic
(+ 1 2 3)

;; Define functions
(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))
```

### Editor Mode

For larger programs, switch to the editor mode which provides:
- Full-screen code editor
- Line numbers
- Advanced syntax highlighting
- Code formatting
- Run button for program execution

## Built With

- [React](https://reactjs.org/) - UI Framework
- [TypeScript](https://www.typescriptlang.org/) - Type Safety
- [Vite](https://vitejs.dev/) - Build Tool
- [shadcn/ui](https://ui.shadcn.com/) - UI Components
- [CodeMirror](https://codemirror.net/) - Code Editor
- [highlight.js](https://highlightjs.org/) - Syntax Highlighting
- [Tailwind CSS](https://tailwindcss.com/) - Styling

## Project Structure

```
src/
  ├── components/
  │   ├── repl-modes.tsx    # REPL and Editor components
  │   ├── terminal.tsx      # Terminal implementation
  │   └── tabs/            # Tab components for different sections
  ├── hooks/
  │   └── useJawsInterpreter.ts   # Scheme interpreter hook
  ├── styles/
  │   └── globals.css      # Global styles
  └── App.tsx              # Main application component
```

## Development

### ESLint Configuration

The project uses ESLint with TypeScript support. To enable type-aware lint rules:

1. Configure parser options:
```js
export default tseslint.config({
  languageOptions: {
    parserOptions: {
      project: ['./tsconfig.node.json', './tsconfig.app.json'],
      tsconfigRootDir: import.meta.dirname,
    },
  },
})
```

2. Install React ESLint plugin:
```bash
npm install eslint-plugin-react --save-dev
```

3. Update ESLint config:
```js
import react from 'eslint-plugin-react'
export default tseslint.config({
  settings: { react: { version: '18.3' } },
  plugins: { react },
  rules: {
    ...react.configs.recommended.rules,
    ...react.configs['jsx-runtime'].rules,
  },
})
```

### Available Scripts

- `npm run dev` - Start development server
- `npm run build` - Build for production
- `npm run lint` - Run ESLint
- `npm run preview` - Preview production build

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- shadcn for the excellent UI components
- The CodeMirror team for the code editor
- highlight.js team for syntax highlighting

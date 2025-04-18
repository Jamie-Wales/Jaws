import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { WelcomePage } from './views/welcome';
import { Layout } from '@/components/layout';
import { EditorView } from './views/editor';
import { DocsView } from '@/views/docsView';
import CompilerExplorer from '@/views/compilerExplorer';
import LearnView from '@/views/learn';
import "@/styles/globals.css"

export default function App() {
    return (
        <BrowserRouter basename="/jaws">
            <Routes>
                <Route path="/" element={<WelcomePage />} />
                <Route element={<Layout />}>
                    <Route path="/compiler-explorer" element={<CompilerExplorer />} />
                    <Route path="/editor" element={<EditorView />} />
                    <Route path="/docs" element={<DocsView />} />
                    <Route path="/learn" element={<LearnView />} />
                </Route>
            </Routes>
        </BrowserRouter>
    );
}

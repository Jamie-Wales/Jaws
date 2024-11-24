import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { WelcomePage } from './views/welcome';
import { Layout } from '@/components/layout';
import { ReplView } from './views/repl';
import { EditorView } from './views/editor';
import { ExamplesView } from '@/views/example';
import { DocsView } from '@/views/documentation';
import { GetStartedView } from '@/views/getStarted';
import "@/styles/globals.css"

export default function App() {
    return (
        <BrowserRouter>
            <Routes>
                <Route path="/" element={<WelcomePage />} />
                <Route element={<Layout />}>
                    <Route path="/get-started" element={<GetStartedView />} />
                    <Route path="/repl" element={<ReplView />} />
                    <Route path="/editor" element={<EditorView />} />
                    <Route path="/examples" element={<ExamplesView />} />
                    <Route path="/docs" element={<DocsView />} />
                </Route>
            </Routes>
        </BrowserRouter>
    );
}

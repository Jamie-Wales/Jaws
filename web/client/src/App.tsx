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
        <BrowserRouter basename={process.env.PUBLIC_URL}>
            <Routes>
                <Route path="/Jaws" element={<WelcomePage />} />
                <Route element={<Layout />}>
                    <Route path="/Jaws/get-started" element={<GetStartedView />} />
                    <Route path="/Jaws/repl" element={<ReplView />} />
                    <Route path="/Jaws/editor" element={<EditorView />} />
                    <Route path="/Jaws/examples" element={<ExamplesView />} />
                    <Route path="/Jaws/docs" element={<DocsView />} />
                </Route>
            </Routes>
        </BrowserRouter>
    );
}

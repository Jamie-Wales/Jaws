import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { WelcomePage } from './views/welcome';
import { Layout } from '@/components/layout';
import { EditorView } from './views/editor';
import { ExamplesView } from '@/views/example';
import { GetStartedView } from '@/views/getStarted';
import CompilerExplorer from '@/views/compilerExplorer';
import LearnView from '@/views/learn';

import "@/styles/globals.css"

const basename = import.meta.env.BASE_URL || '/';

export default function App() {
    return (
        <BrowserRouter basename={basename}>
            <Routes>
                <Route path="/Jaws" element={<WelcomePage />} />
                <Route element={<Layout />}>
                    <Route path="/Jaws/compiler-explorer" element={<CompilerExplorer />} />
                    <Route path="/Jaws/get-started" element={<GetStartedView />} />
                    <Route path="/Jaws/editor" element={<EditorView />} />
                    <Route path="/Jaws/examples" element={<ExamplesView />} />
                    <Route path="/Jaws/learn" element={<LearnView />} />
                </Route>
            </Routes>
        </BrowserRouter>
    );
}

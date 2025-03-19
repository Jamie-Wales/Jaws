import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { WelcomePage } from './views/welcome';
import { Layout } from '@/components/layout';
import { EditorView } from './views/editor';
import { ExamplesView } from '@/views/example';
import { GetStartedView } from '@/views/getStarted';
import "@/styles/globals.css"

const basename = import.meta.env.BASE_URL || '/';

export default function App() {
    return (
        <BrowserRouter basename={basename}>
            <Routes>
                <Route path="/Jaws" element={<WelcomePage />} />
                <Route element={<Layout />}>
                    <Route path="/Jaws/get-started" element={<GetStartedView />} />
                    <Route path="/Jaws/editor" element={<EditorView />} />
                    <Route path="/Jaws/examples" element={<ExamplesView />} />
                    <Route
                        path="/Jaws/docs/*"
                        element={
                            <Navigate
                                to="/docs/html/index.html"
                                replace
                            />
                        }
                    />
                </Route>
            </Routes>
        </BrowserRouter>
    );
}

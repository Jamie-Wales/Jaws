import { Github } from 'lucide-react';

export function Footer() {
    return (
        <footer className="py-6 bg-slate-900 text-white">
            <div className="container mx-auto px-4">
                <div className="flex flex-col md:flex-row justify-between items-center gap-4">
                    <div className="text-sm text-slate-300">
                        Dissertation by Jamie Wales © {new Date().getFullYear()}
                    </div>
                    <div className="flex items-center gap-6">
                        <a
                            href="https://github.com/jamie-wales/jaws"
                            target="_blank"
                            rel="noopener noreferrer"
                            className="text-slate-300 hover:text-white transition-colors"
                        >
                            <Github className="h-5 w-5" />
                        </a>
                    </div>
                </div>
            </div>
        </footer>
    );
}

export default Footer;

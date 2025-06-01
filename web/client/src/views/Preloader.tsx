import { Parentheses } from 'lucide-react';

export function Preloader() {
    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-gradient-to-br from-slate-900 via-blue-900 to-slate-900">
            <div className="relative">
                {/* Animated parentheses */}
                <div className="relative flex items-center justify-center">
                    {/* Main animated parentheses icon */}
                    <div className="animate-paren-bounce">
                        <Parentheses className="w-24 h-24 text-cyan-400" strokeWidth={2} />
                    </div>

                    {/* Rotating ring around parentheses */}
                    <div className="absolute inset-0 flex items-center justify-center">
                        <div className="w-32 h-32 border-2 border-transparent border-t-[#dd3f0c] border-r-[#dd3f0c] rounded-full animate-spin-slow"></div>
                    </div>

                    {/* Pulsing glow effect */}
                    <div className="absolute inset-0 flex items-center justify-center">
                        <div className="w-28 h-28 bg-cyan-400/20 rounded-full animate-ping-slow"></div>
                    </div>
                </div>

                {/* Text below */}
                <div className="text-center mt-12 space-y-3">
                    <h2 className="text-3xl font-bold text-white">JAWS</h2>
                    <div className="flex items-center justify-center space-x-2">
                        <div className="w-2 h-2 bg-cyan-400 rounded-full animate-bounce-dot animation-delay-0"></div>
                        <div className="w-2 h-2 bg-cyan-400 rounded-full animate-bounce-dot animation-delay-200"></div>
                        <div className="w-2 h-2 bg-cyan-400 rounded-full animate-bounce-dot animation-delay-400"></div>
                    </div>
                    <p className="text-cyan-300 text-sm mt-2">Initializing Scheme interpreter</p>
                </div>
            </div>

            {/* Background gradient animation */}
            <div className="absolute inset-0 opacity-30">
                <div className="absolute inset-0 bg-gradient-to-r from-transparent via-cyan-500/20 to-transparent -skew-x-12 animate-slide-right"></div>
            </div>
        </div>
    );
}

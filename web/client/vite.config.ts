import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'
import fs from 'fs'
import type { Plugin } from 'vite'

const copyWasmPlugin = (): Plugin => ({
    name: 'copy-wasm',
    writeBundle() {
        const dir = 'dist';
        const jsDir = path.join(dir, 'javascript');
        if (!fs.existsSync(jsDir)) {
            fs.mkdirSync(jsDir, { recursive: true });
        }
        const sourceDir = path.resolve(__dirname, '../runtime');
        ['jaws.js', 'jaws.wasm'].forEach(file => {
            const sourcePath = path.join(sourceDir, file);
            const destPath = path.join(jsDir, file);
            if (fs.existsSync(sourcePath)) {
                fs.copyFileSync(sourcePath, destPath);
            }
        });
    }
});

export default defineConfig({
    plugins: [
        react(),
        copyWasmPlugin()
    ],
    base: '/jaws/',
    resolve: {
        alias: {
            '@': path.resolve(__dirname, './src'),
        },
    },
    build: {
        outDir: 'dist',
        assetsDir: 'assets',
        copyPublicDir: true,
        rollupOptions: {
            external: ['/javascript/jaws.js', '/javascript/jaws.wasm'],
            output: {
                manualChunks: {
                    'react-vendor': ['react', 'react-dom'],
                    'codemirror-vendor': [
                        '@uiw/react-codemirror',
                        '@codemirror/language',
                        '@uiw/codemirror-theme-xcode',
                    ],
                    'ui-vendor': [
                        '@radix-ui/react-icons',
                        '@radix-ui/react-scroll-area',
                        '@radix-ui/react-separator',
                        '@radix-ui/react-slot',
                        '@radix-ui/react-tabs',
                        'lucide-react'
                    ]
                }
            }
        }
    }
})

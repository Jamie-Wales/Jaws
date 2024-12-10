import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'
import fs from 'fs'
import type { Plugin } from 'vite'

const wasmPlugin = (): Plugin => ({
    name: 'wasm-plugin',
    configureServer(server) {
        const devJsDir = path.join('public', 'javascript');
        if (!fs.existsSync(devJsDir)) {
            fs.mkdirSync(devJsDir, { recursive: true });
        }
        ['jaws.js', 'jaws.wasm'].forEach(file => {
            const sourcePath = path.join('public', 'wasm', file);
            const destPath = path.join(devJsDir, file);
            if (fs.existsSync(sourcePath)) {
                fs.copyFileSync(sourcePath, destPath);
            }
        });
    },
    writeBundle() {
        const dir = 'dist';
        const jsDir = path.join(dir, 'javascript');
        if (!fs.existsSync(jsDir)) {
            fs.mkdirSync(jsDir, { recursive: true });
        }
        ['jaws.js', 'jaws.wasm'].forEach(file => {
            const sourcePath = path.join('public', 'wasm', file);
            const destPath = path.join(jsDir, file);
            if (fs.existsSync(sourcePath)) {
                fs.copyFileSync(sourcePath, destPath);
            }
        });
        const wasmDir = path.join(dir, 'wasm');
        if (fs.existsSync(wasmDir)) {
            fs.rmSync(wasmDir, { recursive: true });
        }
        const indexPath = path.join(dir, 'index.html');
        const notFoundPath = path.join(dir, '404.html');
        if (fs.existsSync(indexPath)) {
            fs.copyFileSync(indexPath, notFoundPath);
        }
    }
});

const markdownPlugin = (): Plugin => ({
    name: 'markdown-plugin',
    transform(src, id) {
        if (id.endsWith('.md')) {
            return `export default ${JSON.stringify(src)}`;
        }
    },
});

const docsPlugin = (): Plugin => ({
    name: 'docs-plugin',
    configureServer(server) {
        const sourceDir = path.join('../../docs/html');
        const destDir = path.join('public', 'docs');
        if (fs.existsSync(sourceDir)) {
            if (!fs.existsSync(destDir)) {
                fs.mkdirSync(destDir, { recursive: true });
            }
            fs.cpSync(sourceDir, destDir, { recursive: true });

            // Copy search directory specifically
            const searchDir = path.join(sourceDir, 'search');
            const destSearchDir = path.join(destDir, 'search');
            if (fs.existsSync(searchDir)) {
                fs.cpSync(searchDir, destSearchDir, { recursive: true });
            }
        }
    },
    writeBundle() {
        // Copy docs to build directory
        const sourceDir = path.join('../../docs/html');
        const destDir = path.join('dist', 'docs');
        if (fs.existsSync(sourceDir)) {
            if (!fs.existsSync(destDir)) {
                fs.mkdirSync(destDir, { recursive: true });
            }
            fs.cpSync(sourceDir, destDir, { recursive: true });
        }
    }
});

export default defineConfig(({ command }) => ({
    plugins: [
        react(),
        wasmPlugin(),
        markdownPlugin(),
        docsPlugin()
    ],
    base: command === 'serve' ? '/' : './',
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
            external: [command === 'serve' ? '/javascript/jaws.js' : './javascript/jaws.js'],
            output: {
                entryFileNames: 'assets/[name]-[hash].js',
                chunkFileNames: 'assets/[name]-[hash].js',
                assetFileNames: 'assets/[name]-[hash].[ext]',
                manualChunks: {
                    vendor: ['react', 'react-dom', 'react/jsx-runtime'],
                    ui: [
                        '@radix-ui/react-icons',
                        '@radix-ui/react-scroll-area',
                        '@radix-ui/react-separator',
                        '@radix-ui/react-slot',
                        '@radix-ui/react-tabs',
                        'lucide-react',
                    ],
                },
            },
        },
    },
}));

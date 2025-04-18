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

// Plugin to handle the lib directory
const libPlugin = (): Plugin => ({
    name: 'lib-plugin',
    configureServer(server) {
        // Create a symbolic link or copy lib directory for development
        const devLibDir = path.join('public', 'lib');
        if (fs.existsSync(devLibDir)) {
            // Create a symlink for development
            const destLibDir = path.join('dist', 'lib');
            if (!fs.existsSync(destLibDir)) {
                fs.mkdirSync(destLibDir, { recursive: true });
            }
            // Copy all scheme files
            fs.readdirSync(devLibDir).forEach(file => {
                if (file.endsWith('.scm')) {
                    fs.copyFileSync(
                        path.join(devLibDir, file),
                        path.join(destLibDir, file)
                    );
                }
            });
        }
    },
    writeBundle() {
        // Copy lib directory to build output
        const sourceLibDir = path.join('public', 'lib');
        const destLibDir = path.join('dist', 'lib');
        if (fs.existsSync(sourceLibDir)) {
            if (!fs.existsSync(destLibDir)) {
                fs.mkdirSync(destLibDir, { recursive: true });
            }
            // Copy all scheme files
            fs.readdirSync(sourceLibDir).forEach(file => {
                if (file.endsWith('.scm')) {
                    fs.copyFileSync(
                        path.join(sourceLibDir, file),
                        path.join(destLibDir, file)
                    );
                }
            });
        }
    }
});

export default defineConfig(({ command }) => ({
    plugins: [
        react(),
        wasmPlugin(),
        libPlugin() // Only keep the necessary plugins
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

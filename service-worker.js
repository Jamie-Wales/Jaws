// public/service-worker.js (with logging)

self.addEventListener('install', () => {
    self.skipWaiting();
    console.log('SW: Installed @', new Date().toLocaleTimeString()); // SW Console
});

self.addEventListener('activate', (event) => {
    event.waitUntil(self.clients.claim());
    console.log('SW: Activated and claimed clients @', new Date().toLocaleTimeString()); // SW Console
});

self.addEventListener('fetch', (event) => {
    const url = event.request.url;
    const isDocRequest = event.request.mode === 'navigate' || url.endsWith('/') || url.endsWith('index.html') || url.includes('compiler-explorer'); // Try to catch doc requests

    if (isDocRequest) {
        console.log(`SW: Handling potential document request: ${url}`); // SW Console
        event.respondWith(
            fetch(event.request)
                .then((response) => {
                    console.log(`SW: Fetched response for ${url}, status: ${response.status}, type: ${response.type}`); // SW Console
                    if (!response || response.status === 0 || response.type === 'opaque' || response.type === 'opaqueredirect') {
                        // Don't modify non-clonable/error responses. Status 0 can occur for fetch errors.
                        console.log(`SW: Not modifying headers for ${url} (status: ${response.status}, type: ${response.type})`); // SW Console
                        return response;
                    }
                    // Log original headers for debugging
                    // console.log('SW: Original headers:', Object.fromEntries(response.headers.entries()));

                    const newHeaders = new Headers(response.headers);
                    newHeaders.set('Cross-Origin-Opener-Policy', 'same-origin');
                    newHeaders.set('Cross-Origin-Embedder-Policy', 'require-corp');
                    console.log(`SW: Added COOP/COEP headers for ${url}`); // SW Console

                    return new Response(response.body, {
                        status: response.status,
                        statusText: response.statusText,
                        headers: newHeaders,
                    });
                })
                .catch((error) => {
                    console.error(`SW: Fetch failed for ${url}:`, error); // SW Console
                    return new Response(`Service Worker fetch failed: ${error}`, { status: 500 });
                })
        );
    } else {
        // Let other requests like assets (JS, CSS, Wasm) pass through without modification
        event.respondWith(fetch(event.request));
    }
});

console.log('SW: Script loaded (public/service-worker.js) @', new Date().toLocaleTimeString()); // SW Console

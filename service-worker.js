// public/service-worker.js

self.addEventListener('install', () => {
    // Skip waiting to activate the new service worker immediately.
    self.skipWaiting();
    console.log('Service Worker: Installed');
});

self.addEventListener('activate', (event) => {
    // Take control of all clients (tabs) immediately.
    event.waitUntil(self.clients.claim());
    console.log('Service Worker: Activated');
});

self.addEventListener('fetch', (event) => {
    // We only need to modify navigation requests or requests for the main document
    // to set the COOP/COEP headers effectively for the page context.
    // However, applying to all same-origin responses is often simpler and safe.

    // Check if the request is for the same origin. Avoid modifying cross-origin requests.
    if (event.request.url.startsWith(self.origin)) {
        event.respondWith(
            fetch(event.request)
                .then((response) => {
                    if (!response) {
                        return response;
                    }

                    // Check if we can clone the response (needed to modify headers)
                    // Opaque responses (e.g., from no-cors cross-origin requests) cannot be cloned.
                    if (response.status === 0 || response.type === 'opaque' || response.type === 'opaqueredirect') {
                        return response; // Cannot modify headers, return original response
                    }

                    // Clone the headers and add the COI headers.
                    const newHeaders = new Headers(response.headers);
                    newHeaders.set('Cross-Origin-Opener-Policy', 'same-origin');
                    newHeaders.set('Cross-Origin-Embedder-Policy', 'require-corp');

                    // Return a new response with the original body and status, but new headers.
                    return new Response(response.body, {
                        status: response.status,
                        statusText: response.statusText,
                        headers: newHeaders,
                    });
                })
                .catch((error) => {
                    console.error('Service Worker: Fetch failed:', error);
                    // Fallback or re-throw
                    return new Response(`Service Worker fetch failed: ${error}`, { status: 500 });
                })
        );
    } else {
        // For cross-origin requests, let them pass through without modification.
        // Or handle them differently if needed, but usually passing through is fine.
        event.respondWith(fetch(event.request));
    }
});

console.log('Service Worker: Script loaded (public/service-worker.js)');

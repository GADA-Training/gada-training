// Service Worker GADA Training
const CACHE_NAME = 'gada-training-v1';

// Fichiers à mettre en cache pour le mode hors ligne
const ASSETS_TO_CACHE = [
    './',
    './index.html',
    './images/logo-gada.png',
    './images/GADA-Sol.png',
    './manifest.json',
    'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css',
    'https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&family=Sora:wght@400;600;700&display=swap'
];

// Installation : mise en cache des ressources
self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => {
            return cache.addAll(ASSETS_TO_CACHE).catch(err => {
                console.log('Cache partiel:', err);
            });
        })
    );
    self.skipWaiting();
});

// Activation : nettoyage des anciens caches
self.addEventListener('activate', (event) => {
    event.waitUntil(
        caches.keys().then((keys) => {
            return Promise.all(
                keys.filter(key => key !== CACHE_NAME)
                    .map(key => caches.delete(key))
            );
        })
    );
    self.clients.claim();
});

// Fetch : stratégie Network First pour Firebase, Cache First pour les assets
self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url);
    
    // Firebase et API : toujours réseau (pas de cache)
    if (url.hostname.includes('firebase') || 
        url.hostname.includes('groq') ||
        url.hostname.includes('googleapis.com') && url.pathname.includes('firestore')) {
        return;
    }
    
    // Pour les assets statiques : Cache First
    event.respondWith(
        caches.match(event.request).then((cached) => {
            if (cached) return cached;
            return fetch(event.request).then((response) => {
                // Mettre en cache seulement les requêtes GET réussies
                if (event.request.method === 'GET' && response.status === 200) {
                    const responseClone = response.clone();
                    caches.open(CACHE_NAME).then((cache) => {
                        cache.put(event.request, responseClone);
                    });
                }
                return response;
            }).catch(() => {
                // Hors ligne : retourner la page principale
                if (event.request.destination === 'document') {
                    return caches.match('./index.html');
                }
            });
        })
    );
});

// Notifications push (optionnel, pour les futurs messages)
self.addEventListener('push', (event) => {
    const data = event.data ? event.data.json() : {};
    const title = data.title || 'GADA Training';
    const options = {
        body: data.body || 'Nouvelle notification',
        icon: './images/logo-gada.png',
        badge: './images/logo-gada.png',
        vibrate: [200, 100, 200],
        data: data
    };
    event.waitUntil(self.registration.showNotification(title, options));
});
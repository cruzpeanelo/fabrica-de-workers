/**
 * Service Worker - Fabrica de Agentes PWA
 * ========================================
 * Progressive Web App completo (Issue #259)
 *
 * Estrategias de cache:
 * - Cache-first para assets estaticos (CSS, JS, imagens)
 * - Network-first para dados da API
 * - Stale-while-revalidate para HTML
 *
 * Recursos:
 * - Precaching de assets criticos
 * - Background Sync para operacoes offline
 * - Push Notifications
 * - Splash Screen support
 */

const CACHE_VERSION = 'fda-v2.0.0';
const STATIC_CACHE = `${CACHE_VERSION}-static`;
const DATA_CACHE = `${CACHE_VERSION}-data`;
const IMAGE_CACHE = `${CACHE_VERSION}-images`;
const OFFLINE_PAGE = '/static/offline.html';

// Assets para cache imediato (precache) - recursos criticos
const PRECACHE_ASSETS = [
  '/',
  '/static/offline.html',
  '/static/manifest.json',
  '/static/icons/icon-72x72.png',
  '/static/icons/icon-96x96.png',
  '/static/icons/icon-128x128.png',
  '/static/icons/icon-144x144.png',
  '/static/icons/icon-152x152.png',
  '/static/icons/icon-192x192.png',
  '/static/icons/icon-384x384.png',
  '/static/icons/icon-512x512.png',
  '/static/design-tokens.css',
  '/static/design-tokens.js',
  '/static/mobile-responsive.css',
  '/static/accessibility.css',
  '/static/a11y.js',
  '/static/pwa-init.js',
  '/static/offline-db.js',
  '/static/offline-ui.js'
];

// CDN assets - cache on first use
const CDN_ASSETS = [
  'https://unpkg.com/vue@3/dist/vue.global.prod.js',
  'https://cdn.tailwindcss.com',
  'https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js',
  'https://cdn.jsdelivr.net/npm/marked/marked.min.js',
  'https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js',
  'https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css',
  'https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js',
  'https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap'
];

// Padroes de URL para cada estrategia
const API_PATTERNS = [
  '/api/stories',
  '/api/projects',
  '/api/epics',
  '/api/sprints',
  '/api/story-tasks',
  '/api/chat',
  '/api/pwa',
  '/api/analytics',
  '/api/users'
];

// ============================================================================
// INSTALL - Precache de assets estaticos
// ============================================================================
self.addEventListener('install', (event) => {
  console.log('[SW] Installing Service Worker v2.0.0...');

  event.waitUntil(
    caches.open(STATIC_CACHE)
      .then((cache) => {
        console.log('[SW] Precaching critical assets');

        // Precache local assets
        const localPromises = PRECACHE_ASSETS.map(url =>
          cache.add(url).catch(err => {
            console.warn(`[SW] Failed to cache local: ${url}`, err);
          })
        );

        // Cache CDN assets (best effort)
        const cdnPromises = CDN_ASSETS.map(url =>
          cache.add(url).catch(err => {
            console.warn(`[SW] Failed to cache CDN: ${url}`, err);
          })
        );

        return Promise.allSettled([...localPromises, ...cdnPromises]);
      })
      .then(() => {
        console.log('[SW] Installation complete');
        return self.skipWaiting();
      })
  );
});

// ============================================================================
// ACTIVATE - Limpar caches antigos
// ============================================================================
self.addEventListener('activate', (event) => {
  console.log('[SW] Activating Service Worker v2.0.0...');

  event.waitUntil(
    caches.keys()
      .then((cacheNames) => {
        return Promise.all(
          cacheNames
            .filter((name) => {
              // Remover caches de versoes antigas
              return name.startsWith('fda-') &&
                     name !== STATIC_CACHE &&
                     name !== DATA_CACHE &&
                     name !== IMAGE_CACHE;
            })
            .map((name) => {
              console.log('[SW] Deleting old cache:', name);
              return caches.delete(name);
            })
        );
      })
      .then(() => {
        console.log('[SW] Activation complete - taking control');
        return self.clients.claim();
      })
  );
});

// ============================================================================
// FETCH - Estrategias de cache
// ============================================================================
self.addEventListener('fetch', (event) => {
  const { request } = event;
  const url = new URL(request.url);

  // Ignorar requisicoes nao-GET
  if (request.method !== 'GET') {
    return;
  }

  // Ignorar WebSocket e Chrome extensions
  if (url.protocol === 'ws:' || url.protocol === 'wss:' ||
      url.protocol === 'chrome-extension:') {
    return;
  }

  // Determinar estrategia baseada no tipo de requisicao
  if (isApiRequest(url)) {
    event.respondWith(networkFirst(request, DATA_CACHE));
  } else if (isImageRequest(url)) {
    event.respondWith(cacheFirst(request, IMAGE_CACHE));
  } else if (isStaticAsset(url)) {
    event.respondWith(cacheFirst(request, STATIC_CACHE));
  } else if (isNavigationRequest(request)) {
    event.respondWith(staleWhileRevalidate(request, STATIC_CACHE));
  } else {
    event.respondWith(networkFirst(request, STATIC_CACHE));
  }
});

// ============================================================================
// ESTRATEGIAS DE CACHE
// ============================================================================

/**
 * Cache-first: Retorna do cache, usa rede como fallback
 * Ideal para assets estaticos que raramente mudam
 */
async function cacheFirst(request, cacheName) {
  const cache = await caches.open(cacheName);
  const cachedResponse = await cache.match(request);

  if (cachedResponse) {
    // Atualizar cache em background para proxima vez
    updateCacheInBackground(request, cache);
    return cachedResponse;
  }

  try {
    const networkResponse = await fetch(request);
    if (networkResponse.ok) {
      cache.put(request, networkResponse.clone());
    }
    return networkResponse;
  } catch (error) {
    console.error('[SW] Fetch failed:', error);
    return caches.match(OFFLINE_PAGE);
  }
}

/**
 * Network-first: Tenta rede primeiro, cache como fallback
 * Ideal para dados da API que precisam estar atualizados
 */
async function networkFirst(request, cacheName) {
  const cache = await caches.open(cacheName);

  try {
    const networkResponse = await fetch(request);

    if (networkResponse.ok) {
      cache.put(request, networkResponse.clone());

      notifyClients({
        type: 'DATA_UPDATED',
        url: request.url,
        timestamp: Date.now()
      });
    }

    return networkResponse;
  } catch (error) {
    console.log('[SW] Network failed, using cache:', request.url);
    const cachedResponse = await cache.match(request);

    if (cachedResponse) {
      notifyClients({
        type: 'OFFLINE_DATA',
        url: request.url,
        timestamp: Date.now()
      });
      return cachedResponse;
    }

    // Retornar resposta de erro estruturada para API
    return new Response(
      JSON.stringify({
        error: 'offline',
        message: 'Voce esta offline. Os dados serao sincronizados quando a conexao for restaurada.',
        cached: false
      }),
      {
        status: 503,
        headers: { 'Content-Type': 'application/json' }
      }
    );
  }
}

/**
 * Stale-while-revalidate: Retorna cache imediatamente e atualiza em background
 * Ideal para HTML e paginas que podem ter versoes ligeiramente desatualizadas
 */
async function staleWhileRevalidate(request, cacheName) {
  const cache = await caches.open(cacheName);
  const cachedResponse = await cache.match(request);

  const fetchPromise = fetch(request)
    .then((networkResponse) => {
      if (networkResponse.ok) {
        cache.put(request, networkResponse.clone());

        // Notificar se houve atualizacao
        if (cachedResponse) {
          notifyClients({
            type: 'CONTENT_UPDATED',
            url: request.url,
            timestamp: Date.now()
          });
        }
      }
      return networkResponse;
    })
    .catch((error) => {
      console.log('[SW] Background fetch failed:', error);
      return null;
    });

  if (cachedResponse) {
    return cachedResponse;
  }

  const networkResponse = await fetchPromise;
  if (networkResponse) {
    return networkResponse;
  }

  return caches.match(OFFLINE_PAGE);
}

/**
 * Atualiza cache em background sem bloquear
 */
async function updateCacheInBackground(request, cache) {
  try {
    const networkResponse = await fetch(request);
    if (networkResponse.ok) {
      await cache.put(request, networkResponse);
    }
  } catch (error) {
    // Silenciosamente falhar - estamos offline
  }
}

// ============================================================================
// HELPERS
// ============================================================================

function isApiRequest(url) {
  return API_PATTERNS.some(pattern => url.pathname.startsWith(pattern));
}

function isStaticAsset(url) {
  const staticExtensions = ['.js', '.css', '.woff', '.woff2', '.ttf', '.eot'];
  return staticExtensions.some(ext => url.pathname.endsWith(ext)) ||
         url.pathname.startsWith('/static/') ||
         (url.hostname !== self.location.hostname &&
          !url.pathname.match(/\.(png|jpg|jpeg|gif|svg|webp|ico)$/i));
}

function isImageRequest(url) {
  const imageExtensions = ['.png', '.jpg', '.jpeg', '.gif', '.svg', '.webp', '.ico'];
  return imageExtensions.some(ext => url.pathname.toLowerCase().endsWith(ext));
}

function isNavigationRequest(request) {
  return request.mode === 'navigate' ||
         (request.method === 'GET' && request.headers.get('accept')?.includes('text/html'));
}

async function notifyClients(message) {
  const clients = await self.clients.matchAll({ type: 'window' });
  clients.forEach(client => {
    client.postMessage(message);
  });
}

// ============================================================================
// BACKGROUND SYNC - Sincronizacao offline
// ============================================================================
self.addEventListener('sync', (event) => {
  console.log('[SW] Background sync:', event.tag);

  switch (event.tag) {
    case 'sync-stories':
      event.waitUntil(syncData('stories'));
      break;
    case 'sync-tasks':
      event.waitUntil(syncData('tasks'));
      break;
    case 'sync-all':
      event.waitUntil(syncAllData());
      break;
    case 'sync-pending':
      event.waitUntil(syncPendingOperations());
      break;
  }
});

async function syncData(entity) {
  notifyClients({
    type: 'SYNC_REQUEST',
    entity: entity,
    timestamp: Date.now()
  });
}

async function syncAllData() {
  await syncData('stories');
  await syncData('tasks');
  await syncData('projects');
}

async function syncPendingOperations() {
  notifyClients({
    type: 'SYNC_PENDING_OPERATIONS',
    timestamp: Date.now()
  });
}

// ============================================================================
// PUSH NOTIFICATIONS
// ============================================================================
self.addEventListener('push', (event) => {
  console.log('[SW] Push received');

  let data = { title: 'Fabrica de Agentes', body: 'Nova atualizacao' };

  if (event.data) {
    try {
      data = event.data.json();
    } catch (e) {
      data.body = event.data.text();
    }
  }

  const options = {
    body: data.body || 'Nova atualizacao disponivel',
    icon: '/static/icons/icon-192x192.png',
    badge: '/static/icons/icon-96x96.png',
    vibrate: [100, 50, 100],
    tag: data.tag || 'fda-notification',
    renotify: true,
    requireInteraction: data.requireInteraction || false,
    data: {
      url: data.url || '/',
      dateOfArrival: Date.now(),
      ...data.data
    },
    actions: data.actions || [
      { action: 'open', title: 'Abrir' },
      { action: 'close', title: 'Fechar' }
    ]
  };

  event.waitUntil(
    self.registration.showNotification(data.title || 'Fabrica de Agentes', options)
  );
});

self.addEventListener('notificationclick', (event) => {
  console.log('[SW] Notification click:', event.action);

  event.notification.close();

  if (event.action === 'close') {
    return;
  }

  const urlToOpen = event.notification.data?.url || '/';

  event.waitUntil(
    self.clients.matchAll({ type: 'window', includeUncontrolled: true })
      .then((clientList) => {
        // Tentar focar em janela existente
        for (const client of clientList) {
          if (client.url.includes(self.location.origin) && 'focus' in client) {
            client.navigate(urlToOpen);
            return client.focus();
          }
        }
        // Abrir nova janela
        if (self.clients.openWindow) {
          return self.clients.openWindow(urlToOpen);
        }
      })
  );
});

// ============================================================================
// MESSAGES FROM CLIENTS
// ============================================================================
self.addEventListener('message', (event) => {
  console.log('[SW] Message received:', event.data?.type);

  const { type, payload } = event.data || {};

  switch (type) {
    case 'SKIP_WAITING':
      self.skipWaiting();
      break;

    case 'CACHE_URLS':
      event.waitUntil(
        caches.open(STATIC_CACHE)
          .then(cache => Promise.all(
            (payload?.urls || []).map(url =>
              cache.add(url).catch(err => console.warn(`Failed to cache: ${url}`, err))
            )
          ))
      );
      break;

    case 'CLEAR_CACHE':
      event.waitUntil(
        caches.keys()
          .then(names => Promise.all(names.map(name => caches.delete(name))))
          .then(() => notifyClients({ type: 'CACHE_CLEARED', timestamp: Date.now() }))
      );
      break;

    case 'GET_CACHE_STATUS':
      event.waitUntil(
        getCacheStatus().then(status => {
          event.source.postMessage({ type: 'CACHE_STATUS', payload: status });
        })
      );
      break;

    case 'GET_SW_STATUS':
      event.source.postMessage({
        type: 'SW_STATUS',
        payload: {
          version: CACHE_VERSION,
          isActive: true,
          caches: [STATIC_CACHE, DATA_CACHE, IMAGE_CACHE]
        }
      });
      break;

    case 'PRECACHE_URLS':
      event.waitUntil(precacheUrls(payload?.urls || []));
      break;
  }
});

async function getCacheStatus() {
  const cacheNames = await caches.keys();
  const status = {
    version: CACHE_VERSION,
    caches: {}
  };

  for (const name of cacheNames) {
    const cache = await caches.open(name);
    const keys = await cache.keys();
    status.caches[name] = {
      count: keys.length,
      urls: keys.slice(0, 50).map(req => req.url) // Limitar para performance
    };
  }

  return status;
}

async function precacheUrls(urls) {
  const cache = await caches.open(STATIC_CACHE);
  await Promise.allSettled(
    urls.map(url => cache.add(url).catch(err => console.warn(`Precache failed: ${url}`, err)))
  );
}

// ============================================================================
// PERIODIC BACKGROUND SYNC
// ============================================================================
self.addEventListener('periodicsync', (event) => {
  console.log('[SW] Periodic sync:', event.tag);

  if (event.tag === 'sync-data') {
    event.waitUntil(syncAllData());
  }
});

console.log('[SW] Service Worker v2.0.0 loaded');

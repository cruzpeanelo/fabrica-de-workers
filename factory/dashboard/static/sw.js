/**
 * Service Worker - Fabrica de Agentes PWA
 * ========================================
 * Estrategias de cache:
 * - Cache-first para assets estaticos (CSS, JS, imagens)
 * - Network-first para dados da API
 * - Stale-while-revalidate para HTML
 */

const CACHE_VERSION = 'fda-v1.0.0';
const STATIC_CACHE = `${CACHE_VERSION}-static`;
const DATA_CACHE = `${CACHE_VERSION}-data`;
const OFFLINE_PAGE = '/offline.html';

// Assets para cache imediato (precache)
const STATIC_ASSETS = [
  '/',
  '/offline.html',
  '/static/manifest.json',
  '/static/icons/icon-192x192.png',
  '/static/icons/icon-512x512.png',
  // CDN assets (cache on first use)
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
  '/api/chat'
];

// ============================================================================
// INSTALL - Precache de assets estaticos
// ============================================================================
self.addEventListener('install', (event) => {
  console.log('[SW] Installing Service Worker...');

  event.waitUntil(
    caches.open(STATIC_CACHE)
      .then((cache) => {
        console.log('[SW] Precaching static assets');
        // Usar addAll com catch para nao falhar se algum asset nao estiver disponivel
        return Promise.allSettled(
          STATIC_ASSETS.map(url =>
            cache.add(url).catch(err => console.warn(`[SW] Failed to cache: ${url}`, err))
          )
        );
      })
      .then(() => {
        console.log('[SW] Installation complete');
        // Ativar imediatamente
        return self.skipWaiting();
      })
  );
});

// ============================================================================
// ACTIVATE - Limpar caches antigos
// ============================================================================
self.addEventListener('activate', (event) => {
  console.log('[SW] Activating Service Worker...');

  event.waitUntil(
    caches.keys()
      .then((cacheNames) => {
        return Promise.all(
          cacheNames
            .filter((name) => name.startsWith('fda-') && name !== STATIC_CACHE && name !== DATA_CACHE)
            .map((name) => {
              console.log('[SW] Deleting old cache:', name);
              return caches.delete(name);
            })
        );
      })
      .then(() => {
        console.log('[SW] Activation complete');
        // Tomar controle imediato de todas as paginas
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

  // Ignorar WebSocket
  if (url.protocol === 'ws:' || url.protocol === 'wss:') {
    return;
  }

  // Determinar estrategia baseada no tipo de requisicao
  if (isApiRequest(url)) {
    // Network-first para API
    event.respondWith(networkFirst(request, DATA_CACHE));
  } else if (isStaticAsset(url)) {
    // Cache-first para assets estaticos
    event.respondWith(cacheFirst(request, STATIC_CACHE));
  } else {
    // Stale-while-revalidate para HTML
    event.respondWith(staleWhileRevalidate(request, STATIC_CACHE));
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
    console.log('[SW] Cache hit:', request.url);
    return cachedResponse;
  }

  console.log('[SW] Cache miss, fetching:', request.url);
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
    console.log('[SW] Network first:', request.url);
    const networkResponse = await fetch(request);

    if (networkResponse.ok) {
      // Salvar no cache para uso offline
      cache.put(request, networkResponse.clone());

      // Notificar clientes sobre atualizacao de dados
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
      // Notificar clientes que estamos offline
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

  // Fetch em background para atualizar cache
  const fetchPromise = fetch(request)
    .then((networkResponse) => {
      if (networkResponse.ok) {
        cache.put(request, networkResponse.clone());
      }
      return networkResponse;
    })
    .catch((error) => {
      console.log('[SW] Background fetch failed:', error);
      return null;
    });

  // Retornar cache se disponivel, senao aguardar rede
  if (cachedResponse) {
    console.log('[SW] Returning stale cache:', request.url);
    return cachedResponse;
  }

  console.log('[SW] No cache, waiting for network:', request.url);
  const networkResponse = await fetchPromise;

  if (networkResponse) {
    return networkResponse;
  }

  return caches.match(OFFLINE_PAGE);
}

// ============================================================================
// HELPERS
// ============================================================================

function isApiRequest(url) {
  return API_PATTERNS.some(pattern => url.pathname.startsWith(pattern));
}

function isStaticAsset(url) {
  const staticExtensions = ['.js', '.css', '.png', '.jpg', '.jpeg', '.gif', '.svg', '.woff', '.woff2', '.ttf'];
  return staticExtensions.some(ext => url.pathname.endsWith(ext)) ||
         url.hostname !== self.location.hostname;
}

async function notifyClients(message) {
  const clients = await self.clients.matchAll({ type: 'window' });
  clients.forEach(client => {
    client.postMessage(message);
  });
}

// ============================================================================
// BACKGROUND SYNC
// ============================================================================

self.addEventListener('sync', (event) => {
  console.log('[SW] Background sync:', event.tag);

  if (event.tag === 'sync-stories') {
    event.waitUntil(syncStories());
  } else if (event.tag === 'sync-tasks') {
    event.waitUntil(syncTasks());
  } else if (event.tag === 'sync-all') {
    event.waitUntil(syncAll());
  }
});

async function syncStories() {
  // Comunicar com IndexedDB via postMessage
  notifyClients({
    type: 'SYNC_REQUEST',
    entity: 'stories'
  });
}

async function syncTasks() {
  notifyClients({
    type: 'SYNC_REQUEST',
    entity: 'tasks'
  });
}

async function syncAll() {
  notifyClients({
    type: 'SYNC_REQUEST',
    entity: 'all'
  });
}

// ============================================================================
// PUSH NOTIFICATIONS
// ============================================================================

self.addEventListener('push', (event) => {
  console.log('[SW] Push received:', event.data?.text());

  const options = {
    body: event.data?.text() || 'Nova atualizacao disponivel',
    icon: '/static/icons/icon-192x192.png',
    badge: '/static/icons/icon-96x96.png',
    vibrate: [100, 50, 100],
    data: {
      dateOfArrival: Date.now(),
      primaryKey: 1
    },
    actions: [
      { action: 'open', title: 'Abrir', icon: '/static/icons/icon-72x72.png' },
      { action: 'close', title: 'Fechar', icon: '/static/icons/icon-72x72.png' }
    ]
  };

  event.waitUntil(
    self.registration.showNotification('Fabrica de Agentes', options)
  );
});

self.addEventListener('notificationclick', (event) => {
  console.log('[SW] Notification click:', event.action);

  event.notification.close();

  if (event.action === 'open' || !event.action) {
    event.waitUntil(
      self.clients.matchAll({ type: 'window' })
        .then((clientList) => {
          // Focar em janela existente ou abrir nova
          for (const client of clientList) {
            if (client.url === '/' && 'focus' in client) {
              return client.focus();
            }
          }
          if (self.clients.openWindow) {
            return self.clients.openWindow('/');
          }
        })
    );
  }
});

// ============================================================================
// MESSAGES FROM CLIENTS
// ============================================================================

self.addEventListener('message', (event) => {
  console.log('[SW] Message received:', event.data);

  const { type, payload } = event.data;

  switch (type) {
    case 'SKIP_WAITING':
      self.skipWaiting();
      break;

    case 'CACHE_URLS':
      event.waitUntil(
        caches.open(STATIC_CACHE)
          .then(cache => cache.addAll(payload.urls))
      );
      break;

    case 'CLEAR_CACHE':
      event.waitUntil(
        caches.keys()
          .then(names => Promise.all(names.map(name => caches.delete(name))))
          .then(() => notifyClients({ type: 'CACHE_CLEARED' }))
      );
      break;

    case 'GET_CACHE_STATUS':
      event.waitUntil(
        getCacheStatus().then(status => {
          event.source.postMessage({ type: 'CACHE_STATUS', payload: status });
        })
      );
      break;
  }
});

async function getCacheStatus() {
  const cacheNames = await caches.keys();
  const status = {};

  for (const name of cacheNames) {
    const cache = await caches.open(name);
    const keys = await cache.keys();
    status[name] = {
      count: keys.length,
      urls: keys.map(req => req.url)
    };
  }

  return status;
}

console.log('[SW] Service Worker loaded');

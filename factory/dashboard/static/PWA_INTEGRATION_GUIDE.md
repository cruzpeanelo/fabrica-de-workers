# PWA Integration Guide - Issue #68

This guide explains how to integrate the PWA features into the dashboard.

## Files Created

### Static Files (in `factory/dashboard/static/`):

1. **manifest.json** - PWA manifest with app metadata
2. **sw.js** - Service Worker with caching strategies
3. **offline.html** - Offline fallback page
4. **offline-db.js** - IndexedDB for local data storage
5. **offline-ui.js** - Offline status indicator and sync button
6. **pwa-init.js** - PWA initialization and Service Worker registration

### Icons (in `factory/dashboard/static/icons/`):

- icon-72x72.png through icon-512x512.png
- icon.svg (vector source)

### Route Handler:

- **pwa_routes.py** - FastAPI routes for serving PWA files

## Manual Integration Steps

### 1. Add Static Files Mount (app_v6_agile.py)

After the `UPLOAD_DIR` configuration, add:

```python
# Diretorio de arquivos estaticos (PWA)
STATIC_DIR = Path(r'C:\Users\lcruz\Fabrica de Agentes\factory\dashboard\static')
STATIC_DIR.mkdir(exist_ok=True)

# Montar arquivos estaticos
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")
```

### 2. Add PWA Routes Import

After the database imports, add:

```python
# PWA Routes (Issue #68)
from factory.dashboard.pwa_routes import register_pwa_routes
```

After creating the FastAPI app, add:

```python
# Registrar rotas PWA (Issue #68)
register_pwa_routes(app)
```

### 3. Add PWA Meta Tags to HTML_TEMPLATE

In the `<head>` section, after `<title>`, add:

```html
<!-- PWA Meta Tags (Issue #68) -->
<meta name="theme-color" content="#003B4A">
<meta name="apple-mobile-web-app-capable" content="yes">
<meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
<meta name="apple-mobile-web-app-title" content="FdA">
<link rel="manifest" href="/manifest.json">
<link rel="apple-touch-icon" href="/static/icons/icon-192x192.png">
```

### 4. Add PWA Scripts to HTML_TEMPLATE

Before the closing `</body>` tag, add:

```html
<!-- PWA Scripts (Issue #68) -->
<script src="/static/offline-db.js"></script>
<script src="/static/offline-ui.js"></script>
<script src="/static/pwa-init.js"></script>
```

## Features Implemented

### Service Worker (sw.js)
- Cache-first strategy for static assets (CSS, JS, images)
- Network-first strategy for API data
- Stale-while-revalidate for HTML
- Offline fallback page
- Background sync support
- Push notification handlers

### IndexedDB (offline-db.js)
- Local storage for stories, tasks, projects
- Pending actions queue for offline changes
- Automatic sync when coming online
- Conflict resolution support

### Offline UI (offline-ui.js)
- Online/offline status indicator
- Pending changes badge
- "Sync Now" button
- Toast notifications for sync events

### PWA Init (pwa-init.js)
- Service Worker registration
- Install prompt handler
- Background sync setup
- Update notification

## Testing

1. Start the dashboard: `python factory/dashboard/app_v6_agile.py`
2. Open http://localhost:9001 in Chrome
3. Open DevTools > Application > Service Workers
4. Verify SW is registered and active
5. Go offline (DevTools > Network > Offline)
6. Verify offline indicator appears
7. Make changes while offline
8. Go back online
9. Verify changes sync automatically

## Browser Support

- Chrome 67+
- Firefox 67+
- Safari 11.1+ (limited SW support)
- Edge 79+

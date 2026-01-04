# -*- coding: utf-8 -*-
"""
PWA Setup - Plataforma E (Issue #259)
============================================
Progressive Web App completo com:
- Manifest.json dinâmico
- Service Worker
- Instalação como app nativo
- Splash screen
- API de status
"""

from fastapi import FastAPI, Request
from fastapi.responses import FileResponse, JSONResponse, HTMLResponse
from pathlib import Path
from datetime import datetime
import json


# Diretório de arquivos estáticos
STATIC_DIR = Path(__file__).parent / "static"

# Versão do PWA
PWA_VERSION = "2.0.0"


def register_pwa_routes(app: FastAPI):
    """
    Registra rotas PWA no FastAPI app.

    Routes:
    - GET /manifest.json - Serve o manifest
    - GET /sw.js - Serve o service worker
    - GET /api/pwa/status - Status da instalação
    - GET /api/pwa/config - Configuração do PWA
    - POST /api/pwa/subscribe - Registrar push notification
    """

    @app.get("/manifest.json", response_class=FileResponse)
    async def serve_manifest():
        """Serve o Web App Manifest."""
        manifest_path = STATIC_DIR / "manifest.json"
        if manifest_path.exists():
            return FileResponse(
                path=str(manifest_path),
                media_type="application/manifest+json",
                headers={
                    "Cache-Control": "public, max-age=86400",
                    "Service-Worker-Allowed": "/"
                }
            )
        return JSONResponse(
            status_code=404,
            content={"error": "manifest.json not found"}
        )

    @app.get("/sw.js", response_class=FileResponse)
    async def serve_service_worker():
        """Serve o Service Worker com headers corretos."""
        sw_path = STATIC_DIR / "sw.js"
        if sw_path.exists():
            return FileResponse(
                path=str(sw_path),
                media_type="application/javascript",
                headers={
                    "Cache-Control": "no-cache, no-store, must-revalidate",
                    "Service-Worker-Allowed": "/",
                    "Content-Type": "application/javascript; charset=utf-8"
                }
            )
        return JSONResponse(
            status_code=404,
            content={"error": "sw.js not found"}
        )

    @app.get("/api/pwa/status")
    async def pwa_status(request: Request):
        """
        Retorna o status da instalação PWA.

        Returns:
            - version: Versão do PWA
            - installed: Se está instalado (baseado em display-mode)
            - sw_registered: Se o SW está registrado
            - features: Features disponíveis
            - manifest_url: URL do manifest
            - sw_url: URL do service worker
        """
        # Verificar se existem os arquivos necessários
        manifest_exists = (STATIC_DIR / "manifest.json").exists()
        sw_exists = (STATIC_DIR / "sw.js").exists()
        offline_exists = (STATIC_DIR / "offline.html").exists()

        # Verificar ícones
        icons_dir = STATIC_DIR / "icons"
        icons = []
        if icons_dir.exists():
            icons = [f.name for f in icons_dir.glob("*.png")]

        return {
            "version": PWA_VERSION,
            "status": "ready" if (manifest_exists and sw_exists) else "incomplete",
            "manifest": {
                "exists": manifest_exists,
                "url": "/manifest.json"
            },
            "service_worker": {
                "exists": sw_exists,
                "url": "/sw.js",
                "scope": "/"
            },
            "offline_page": {
                "exists": offline_exists,
                "url": "/static/offline.html"
            },
            "icons": {
                "count": len(icons),
                "available": icons
            },
            "features": {
                "offline_support": sw_exists and offline_exists,
                "push_notifications": True,
                "background_sync": True,
                "installable": manifest_exists and sw_exists,
                "share_target": True,
                "file_handling": True
            },
            "timestamp": datetime.utcnow().isoformat()
        }

    @app.get("/api/pwa/config")
    async def pwa_config():
        """
        Retorna a configuração do PWA para o cliente.

        Usado pelo pwa-init.js para configurar dinamicamente.
        """
        return {
            "version": PWA_VERSION,
            "cache_name": f"fda-v{PWA_VERSION}",
            "theme_color": "#003B4A",
            "background_color": "#003B4A",
            "display": "standalone",
            "start_url": "/",
            "scope": "/",
            "update_check_interval": 3600000,  # 1 hora em ms
            "precache_urls": [
                "/",
                "/static/offline.html",
                "/static/manifest.json",
                "/static/design-tokens.css",
                "/static/mobile-responsive.css",
                "/static/accessibility.css"
            ],
            "api_cache_patterns": [
                "/api/stories",
                "/api/projects",
                "/api/epics",
                "/api/sprints"
            ],
            "sync_tags": {
                "stories": "sync-stories",
                "tasks": "sync-tasks",
                "all": "sync-all"
            },
            "push": {
                "vapid_public_key": None,  # Configurar em produção
                "endpoint": "/api/pwa/subscribe"
            }
        }

    @app.post("/api/pwa/subscribe")
    async def pwa_subscribe(request: Request):
        """
        Registra uma subscription para push notifications.

        Body:
            - endpoint: URL do endpoint de push
            - keys: Chaves de criptografia (p256dh, auth)
        """
        try:
            body = await request.json()
            endpoint = body.get("endpoint")
            keys = body.get("keys", {})

            if not endpoint:
                return JSONResponse(
                    status_code=400,
                    content={"error": "endpoint is required"}
                )

            # TODO: Salvar subscription no banco de dados
            # Por enquanto, apenas retorna sucesso
            return {
                "success": True,
                "message": "Push notification subscription registered",
                "subscription_id": f"sub_{datetime.utcnow().timestamp()}"
            }
        except Exception as e:
            return JSONResponse(
                status_code=500,
                content={"error": str(e)}
            )

    @app.post("/api/pwa/unsubscribe")
    async def pwa_unsubscribe(request: Request):
        """Remove uma subscription de push notifications."""
        try:
            body = await request.json()
            subscription_id = body.get("subscription_id")

            # TODO: Remover subscription do banco de dados
            return {
                "success": True,
                "message": "Push notification subscription removed"
            }
        except Exception as e:
            return JSONResponse(
                status_code=500,
                content={"error": str(e)}
            )

    @app.get("/api/pwa/cache-status")
    async def cache_status():
        """
        Retorna informações sobre o cache do PWA.

        Nota: O cache real é gerenciado pelo Service Worker no cliente.
        Esta rota retorna metadados sobre a configuração de cache.
        """
        return {
            "version": PWA_VERSION,
            "cache_name": f"fda-v{PWA_VERSION}",
            "strategies": {
                "static_assets": "cache-first",
                "api_requests": "network-first",
                "html_pages": "stale-while-revalidate",
                "images": "cache-first"
            },
            "cache_types": [
                f"fda-v{PWA_VERSION}-static",
                f"fda-v{PWA_VERSION}-data",
                f"fda-v{PWA_VERSION}-images"
            ],
            "precache_count": 18,
            "max_age": {
                "static": 86400,
                "data": 300,
                "images": 604800
            }
        }

    @app.post("/share-target")
    async def share_target(request: Request):
        """
        Handle Web Share Target API.

        Recebe dados compartilhados de outros apps.
        """
        try:
            form = await request.form()
            title = form.get("title", "")
            text = form.get("text", "")
            url = form.get("url", "")
            files = form.getlist("files")

            # Redirecionar para a página principal com os dados
            redirect_url = f"/?shared=true&title={title}&text={text}&url={url}"

            return HTMLResponse(
                content=f"""
                <!DOCTYPE html>
                <html>
                <head>
                    <meta http-equiv="refresh" content="0;url={redirect_url}">
                    <script>window.location.href = "{redirect_url}";</script>
                </head>
                <body>
                    <p>Redirecionando...</p>
                </body>
                </html>
                """,
                status_code=200
            )
        except Exception as e:
            return JSONResponse(
                status_code=500,
                content={"error": str(e)}
            )

    @app.get("/open-file")
    async def open_file(request: Request):
        """
        Handle File Handling API.

        Abre arquivos associados ao app.
        """
        # Redirecionar para a página principal
        return HTMLResponse(
            content="""
            <!DOCTYPE html>
            <html>
            <head>
                <meta http-equiv="refresh" content="0;url=/">
                <script>window.location.href = "/";</script>
            </head>
            <body>
                <p>Abrindo arquivo...</p>
            </body>
            </html>
            """,
            status_code=200
        )

    @app.get("/handle-protocol")
    async def handle_protocol(url: str = ""):
        """
        Handle Protocol Handler (web+fda://).

        Processa URLs com protocolo customizado.
        """
        # Redirecionar para a página principal com a URL
        redirect_url = f"/?protocol_url={url}"
        return HTMLResponse(
            content=f"""
            <!DOCTYPE html>
            <html>
            <head>
                <meta http-equiv="refresh" content="0;url={redirect_url}">
                <script>window.location.href = "{redirect_url}";</script>
            </head>
            <body>
                <p>Processando...</p>
            </body>
            </html>
            """,
            status_code=200
        )

    print("[PWA] Routes registered: /manifest.json, /sw.js, /api/pwa/*")


def get_pwa_meta_tags() -> str:
    """
    Retorna as meta tags necessárias para PWA.

    Incluir no <head> do HTML principal.
    """
    return """
    <!-- PWA Meta Tags -->
    <meta name="theme-color" content="#003B4A">
    <meta name="mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
    <meta name="apple-mobile-web-app-title" content="FdA Agile">
    <meta name="application-name" content="Plataforma E">
    <meta name="msapplication-TileColor" content="#003B4A">
    <meta name="msapplication-TileImage" content="/static/icons/icon-144x144.png">
    <meta name="msapplication-config" content="none">

    <!-- PWA Manifest -->
    <link rel="manifest" href="/manifest.json" crossorigin="use-credentials">

    <!-- PWA Icons -->
    <link rel="icon" type="image/png" sizes="32x32" href="/static/icons/icon-96x96.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/static/icons/icon-72x72.png">
    <link rel="apple-touch-icon" href="/static/icons/icon-192x192.png">
    <link rel="apple-touch-icon" sizes="152x152" href="/static/icons/icon-152x152.png">
    <link rel="apple-touch-icon" sizes="180x180" href="/static/icons/icon-192x192.png">
    <link rel="apple-touch-icon" sizes="167x167" href="/static/icons/icon-192x192.png">

    <!-- Splash Screens para iOS -->
    <link rel="apple-touch-startup-image" href="/static/icons/icon-512x512.png">

    <!-- Preconnect para performance -->
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link rel="preconnect" href="https://cdn.jsdelivr.net">

    <!-- DNS Prefetch -->
    <link rel="dns-prefetch" href="https://unpkg.com">
    <link rel="dns-prefetch" href="https://cdn.tailwindcss.com">
    """


def get_pwa_init_script() -> str:
    """
    Retorna o script de inicialização do PWA.

    Incluir antes de </body> no HTML principal.
    """
    return """
    <!-- PWA Initialization -->
    <script src="/static/pwa-init.js" defer></script>
    <script src="/static/offline-db.js" defer></script>
    <script src="/static/offline-ui.js" defer></script>

    <script>
    // Registro do Service Worker inline (fallback)
    if ('serviceWorker' in navigator) {
        window.addEventListener('load', function() {
            navigator.serviceWorker.register('/sw.js', { scope: '/' })
                .then(function(registration) {
                    console.log('[App] SW registered:', registration.scope);
                })
                .catch(function(error) {
                    console.warn('[App] SW registration failed:', error);
                });
        });
    }

    // Detectar modo de exibição (instalado vs navegador)
    if (window.matchMedia('(display-mode: standalone)').matches) {
        document.body.classList.add('pwa-installed');
        console.log('[App] Running in standalone mode (installed)');
    }

    // Listener para instalação
    window.addEventListener('appinstalled', function() {
        console.log('[App] PWA installed successfully');
        document.body.classList.add('pwa-installed');
    });
    </script>
    """


# Para uso como módulo
__all__ = [
    'register_pwa_routes',
    'get_pwa_meta_tags',
    'get_pwa_init_script',
    'PWA_VERSION'
]

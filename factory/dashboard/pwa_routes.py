# -*- coding: utf-8 -*-
"""
PWA Routes - Plataforma E
================================
Rotas para Progressive Web App:
- Service Worker
- Manifest
- Arquivos estaticos offline
"""

from pathlib import Path
from fastapi import APIRouter
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse

# Diretorio de arquivos estaticos (PWA)
STATIC_DIR = Path(__file__).parent / 'static'
STATIC_DIR.mkdir(exist_ok=True)


def register_pwa_routes(app):
    """Registrar rotas PWA no app FastAPI"""

    # Montar arquivos estaticos
    app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")

    @app.get("/sw.js")
    async def service_worker():
        """Servir Service Worker do root path"""
        sw_path = STATIC_DIR / "sw.js"
        if sw_path.exists():
            return FileResponse(
                sw_path,
                media_type="application/javascript",
                headers={"Service-Worker-Allowed": "/"}
            )
        return {"error": "Service Worker not found"}

    @app.get("/manifest.json")
    async def manifest():
        """Servir Manifest do root path"""
        manifest_path = STATIC_DIR / "manifest.json"
        if manifest_path.exists():
            return FileResponse(
                manifest_path,
                media_type="application/manifest+json"
            )
        return {"error": "Manifest not found"}

    @app.get("/offline.html")
    async def offline_page():
        """Servir pagina offline"""
        offline_path = STATIC_DIR / "offline.html"
        if offline_path.exists():
            return FileResponse(offline_path)
        return {"error": "Offline page not found"}

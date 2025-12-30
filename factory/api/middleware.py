"""
Middleware de Logging para API
==============================

Middleware FastAPI para logging automatico de requests.
"""

import time
import uuid
from typing import Callable
from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware

try:
    from factory.core.logging_system import get_logger, log_api_request
    HAS_LOGGING = True
except ImportError:
    HAS_LOGGING = False


class RequestLoggingMiddleware(BaseHTTPMiddleware):
    """Middleware para logging de requests HTTP"""

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        # Gera ID de correlacao
        correlation_id = request.headers.get("X-Correlation-ID", str(uuid.uuid4()))

        # Adiciona ao state da request
        request.state.correlation_id = correlation_id

        # Configura logger com correlation ID
        if HAS_LOGGING:
            logger = get_logger()
            logger.set_correlation_id(correlation_id)

        # Marca inicio
        start_time = time.time()

        # Processa request
        response = await call_next(request)

        # Calcula duracao
        duration_ms = (time.time() - start_time) * 1000

        # Log do request
        if HAS_LOGGING:
            log_api_request(
                method=request.method,
                path=request.url.path,
                status_code=response.status_code,
                duration_ms=duration_ms,
                user_id=getattr(request.state, 'user_id', None),
                query_params=dict(request.query_params),
                user_agent=request.headers.get("User-Agent", "unknown")[:100]
            )

        # Adiciona headers de correlacao na resposta
        response.headers["X-Correlation-ID"] = correlation_id
        response.headers["X-Response-Time"] = f"{duration_ms:.2f}ms"

        return response


class SecurityHeadersMiddleware(BaseHTTPMiddleware):
    """Middleware para headers de seguranca"""

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        response = await call_next(request)

        # Headers de seguranca
        response.headers["X-Content-Type-Options"] = "nosniff"
        response.headers["X-Frame-Options"] = "DENY"
        response.headers["X-XSS-Protection"] = "1; mode=block"
        response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"

        return response


# Issue #190: Limite de tamanho de request body
MAX_BODY_SIZE = 10 * 1024 * 1024  # 10MB default
MAX_UPLOAD_SIZE = 50 * 1024 * 1024  # 50MB for uploads


class RequestSizeLimitMiddleware(BaseHTTPMiddleware):
    """
    Middleware para limitar tamanho do request body (Issue #190)

    Previne ataques DoS via payloads enormes.

    Limites:
    - Geral: 10MB
    - Uploads: 50MB (para /upload, /api/upload)
    """

    def __init__(self, app, max_size: int = MAX_BODY_SIZE, max_upload_size: int = MAX_UPLOAD_SIZE):
        super().__init__(app)
        self.max_size = max_size
        self.max_upload_size = max_upload_size

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        # Verificar Content-Length header
        content_length = request.headers.get("content-length")

        if content_length:
            content_length = int(content_length)

            # Determinar limite baseado na rota
            path = request.url.path.lower()
            is_upload = "/upload" in path or "/file" in path or "/attachment" in path

            limit = self.max_upload_size if is_upload else self.max_size

            if content_length > limit:
                from fastapi.responses import JSONResponse
                limit_mb = limit / (1024 * 1024)
                return JSONResponse(
                    status_code=413,
                    content={
                        "detail": f"Request body too large. Maximum size: {limit_mb:.0f}MB",
                        "max_size_bytes": limit
                    }
                )

        return await call_next(request)

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

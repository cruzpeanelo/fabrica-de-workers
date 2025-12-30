"""
Idempotency Middleware - Issue #108
===================================

Middleware para garantir idempotencia em endpoints de criacao.

Previne duplicacao de recursos quando:
- Cliente reenvia requisicao por timeout
- Rede tem problemas de conectividade
- Usuario clica multiplas vezes em botao

Uso:
    1. Cliente envia header "Idempotency-Key: <uuid>"
    2. Middleware verifica se ja existe resposta para essa key
    3. Se existe, retorna resposta cacheada
    4. Se nao, processa e cacheia resultado

Headers:
    - Idempotency-Key: UUID unico por operacao
    - X-Idempotency-Replayed: "true" se resposta foi do cache
"""

import hashlib
import json
import time
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, Optional, Tuple
from functools import wraps

from fastapi import Request, Response, HTTPException, status
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel

# Tentar importar Redis, fallback para cache em memoria
try:
    import redis.asyncio as redis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False


class IdempotencyRecord(BaseModel):
    """Registro de idempotencia armazenado"""
    key: str
    request_hash: str
    status_code: int
    body: str
    headers: Dict[str, str]
    created_at: datetime
    expires_at: datetime


class InMemoryIdempotencyStore:
    """
    Store de idempotencia em memoria.
    Para desenvolvimento e sistemas single-instance.
    """

    def __init__(self, ttl_seconds: int = 86400):
        self._store: Dict[str, IdempotencyRecord] = {}
        self._ttl = ttl_seconds
        self._last_cleanup = time.time()
        self._cleanup_interval = 300  # 5 minutos

    async def get(self, key: str) -> Optional[IdempotencyRecord]:
        """Busca registro por key"""
        self._maybe_cleanup()

        record = self._store.get(key)
        if record and record.expires_at > datetime.utcnow():
            return record

        # Expirado, remover
        if record:
            del self._store[key]

        return None

    async def set(
        self,
        key: str,
        request_hash: str,
        status_code: int,
        body: str,
        headers: Dict[str, str]
    ) -> IdempotencyRecord:
        """Armazena registro"""
        self._maybe_cleanup()

        now = datetime.utcnow()
        record = IdempotencyRecord(
            key=key,
            request_hash=request_hash,
            status_code=status_code,
            body=body,
            headers=headers,
            created_at=now,
            expires_at=now + timedelta(seconds=self._ttl)
        )

        self._store[key] = record
        return record

    async def exists(self, key: str) -> bool:
        """Verifica se key existe e nao expirou"""
        record = await self.get(key)
        return record is not None

    async def delete(self, key: str) -> bool:
        """Remove registro"""
        if key in self._store:
            del self._store[key]
            return True
        return False

    def _maybe_cleanup(self):
        """Limpa registros expirados periodicamente"""
        now = time.time()
        if now - self._last_cleanup < self._cleanup_interval:
            return

        self._last_cleanup = now
        cutoff = datetime.utcnow()

        expired_keys = [
            k for k, v in self._store.items()
            if v.expires_at < cutoff
        ]

        for key in expired_keys:
            del self._store[key]


class RedisIdempotencyStore:
    """
    Store de idempotencia usando Redis.
    Para producao e sistemas distribuidos.
    """

    def __init__(self, redis_url: str, ttl_seconds: int = 86400):
        self._redis_url = redis_url
        self._ttl = ttl_seconds
        self._client: Optional[redis.Redis] = None

    async def _get_client(self) -> redis.Redis:
        """Obtem cliente Redis (lazy initialization)"""
        if self._client is None:
            self._client = await redis.from_url(
                self._redis_url,
                encoding="utf-8",
                decode_responses=True
            )
        return self._client

    async def get(self, key: str) -> Optional[IdempotencyRecord]:
        """Busca registro por key"""
        try:
            client = await self._get_client()
            data = await client.get(f"idemp:{key}")

            if data:
                return IdempotencyRecord.model_validate_json(data)

            return None
        except Exception:
            return None

    async def set(
        self,
        key: str,
        request_hash: str,
        status_code: int,
        body: str,
        headers: Dict[str, str]
    ) -> IdempotencyRecord:
        """Armazena registro com TTL"""
        now = datetime.utcnow()
        record = IdempotencyRecord(
            key=key,
            request_hash=request_hash,
            status_code=status_code,
            body=body,
            headers=headers,
            created_at=now,
            expires_at=now + timedelta(seconds=self._ttl)
        )

        try:
            client = await self._get_client()
            await client.setex(
                f"idemp:{key}",
                self._ttl,
                record.model_dump_json()
            )
        except Exception:
            pass  # Falha silenciosa, nao bloqueia a requisicao

        return record

    async def exists(self, key: str) -> bool:
        """Verifica se key existe"""
        try:
            client = await self._get_client()
            return await client.exists(f"idemp:{key}") > 0
        except Exception:
            return False

    async def delete(self, key: str) -> bool:
        """Remove registro"""
        try:
            client = await self._get_client()
            return await client.delete(f"idemp:{key}") > 0
        except Exception:
            return False


class IdempotencyMiddleware(BaseHTTPMiddleware):
    """
    Middleware FastAPI para idempotencia.

    Intercepta requisicoes POST/PUT/PATCH e verifica header Idempotency-Key.
    """

    # Metodos que suportam idempotencia
    IDEMPOTENT_METHODS = {"POST", "PUT", "PATCH"}

    # Paths que nao precisam de idempotencia
    EXCLUDED_PATHS = {
        "/api/v1/auth/login",
        "/api/v1/auth/refresh",
        "/api/v1/health",
        "/docs",
        "/redoc",
        "/openapi.json"
    }

    def __init__(
        self,
        app,
        store=None,
        redis_url: str = None,
        ttl_seconds: int = 86400,
        require_key: bool = True  # Issue #173: obrigatório por padrão
    ):
        """
        Inicializa middleware.

        Args:
            app: Aplicacao FastAPI
            store: Store customizado (opcional)
            redis_url: URL do Redis (opcional)
            ttl_seconds: TTL dos registros (padrao 24h)
            require_key: Se True, rejeita POST sem Idempotency-Key (default: True)
        """
        super().__init__(app)
        self.ttl = ttl_seconds
        self.require_key = require_key

        if store:
            self.store = store
        elif redis_url and REDIS_AVAILABLE:
            self.store = RedisIdempotencyStore(redis_url, ttl_seconds)
        else:
            self.store = InMemoryIdempotencyStore(ttl_seconds)

    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Processa requisicao com verificacao de idempotencia"""

        # Ignorar metodos que nao precisam de idempotencia
        if request.method not in self.IDEMPOTENT_METHODS:
            return await call_next(request)

        # Ignorar paths excluidos
        if request.url.path in self.EXCLUDED_PATHS:
            return await call_next(request)

        # Verificar header Idempotency-Key
        idempotency_key = request.headers.get("Idempotency-Key")

        if not idempotency_key:
            if self.require_key:
                return JSONResponse(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    content={
                        "error_code": "MISSING_IDEMPOTENCY_KEY",
                        "message": "Header Idempotency-Key e obrigatorio para operacoes POST",
                        "success": False
                    }
                )
            # Sem key, processar normalmente
            return await call_next(request)

        # Calcular hash da requisicao para validar consistencia
        body = await request.body()
        request_hash = self._hash_request(request, body)

        # Verificar se ja existe registro
        existing = await self.store.get(idempotency_key)

        if existing:
            # Validar que a requisicao e identica
            if existing.request_hash != request_hash:
                return JSONResponse(
                    status_code=status.HTTP_409_CONFLICT,
                    content={
                        "error_code": "IDEMPOTENCY_CONFLICT",
                        "message": "Idempotency-Key ja usado com requisicao diferente",
                        "success": False
                    }
                )

            # Retornar resposta cacheada
            response = JSONResponse(
                status_code=existing.status_code,
                content=json.loads(existing.body) if existing.body else None
            )

            # Adicionar headers da resposta original
            for key, value in existing.headers.items():
                response.headers[key] = value

            # Indicar que foi replay
            response.headers["X-Idempotency-Replayed"] = "true"
            response.headers["X-Idempotency-Key"] = idempotency_key

            return response

        # Processar requisicao
        # Precisamos recriar o request com o body (ja foi consumido)
        request._body = body
        response = await call_next(request)

        # Cachear resposta se foi sucesso
        if 200 <= response.status_code < 500:
            # Ler body da resposta
            response_body = b""
            async for chunk in response.body_iterator:
                response_body += chunk

            # Armazenar
            await self.store.set(
                key=idempotency_key,
                request_hash=request_hash,
                status_code=response.status_code,
                body=response_body.decode() if response_body else "",
                headers=dict(response.headers)
            )

            # Recriar resposta (body ja foi consumido)
            return Response(
                content=response_body,
                status_code=response.status_code,
                headers=dict(response.headers),
                media_type=response.media_type
            )

        return response

    def _hash_request(self, request: Request, body: bytes) -> str:
        """Gera hash da requisicao para validacao"""
        components = [
            request.method,
            str(request.url.path),
            body.decode() if body else ""
        ]

        content = "|".join(components)
        return hashlib.sha256(content.encode()).hexdigest()


# Decorator para endpoints especificos
def idempotent(store=None):
    """
    Decorator para marcar endpoint como idempotente.

    Uso:
        @app.post("/items")
        @idempotent()
        async def create_item(item: ItemCreate, request: Request):
            ...
    """
    _store = store or InMemoryIdempotencyStore()

    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            # Extrair request dos kwargs
            request = kwargs.get("request")
            if not request:
                for arg in args:
                    if isinstance(arg, Request):
                        request = arg
                        break

            if not request:
                return await func(*args, **kwargs)

            idempotency_key = request.headers.get("Idempotency-Key")
            if not idempotency_key:
                return await func(*args, **kwargs)

            # Verificar cache
            existing = await _store.get(idempotency_key)
            if existing:
                return JSONResponse(
                    status_code=existing.status_code,
                    content=json.loads(existing.body) if existing.body else None,
                    headers={"X-Idempotency-Replayed": "true"}
                )

            # Executar funcao
            result = await func(*args, **kwargs)

            # Cachear resultado
            if isinstance(result, Response):
                body = result.body if hasattr(result, "body") else ""
                await _store.set(
                    key=idempotency_key,
                    request_hash="",
                    status_code=result.status_code,
                    body=body.decode() if isinstance(body, bytes) else str(body),
                    headers=dict(result.headers)
                )

            return result

        return wrapper
    return decorator

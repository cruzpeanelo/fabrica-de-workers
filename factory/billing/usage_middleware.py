# -*- coding: utf-8 -*-
"""
Middleware de Tracking de Uso Automatico
==========================================

Implementa tracking automatico de:
- API Calls por endpoint
- Tokens LLM (input + output Claude)
- Storage (arquivos, uploads)
- Compute (tempo de worker)
- Logins/sessoes ativas

Issue #118 - Sistema de Metering e Usage Tracking Completo

Autor: Plataforma E
"""

import time
import uuid
import logging
from typing import Optional, Callable, List, Any
from functools import wraps

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from sqlalchemy.orm import Session

from .middleware import get_current_tenant_id
from .usage_service import UsageService
from .models import UsageEventType

# Configurar logging
logger = logging.getLogger(__name__)


class UsageTrackingMiddleware(BaseHTTPMiddleware):
    """
    Middleware para tracking automatico de chamadas API.

    Registra automaticamente cada request/response para metering.
    """

    def __init__(
        self,
        app,
        get_db: Callable,
        exempt_paths: Optional[List[str]] = None,
        track_successful_only: bool = False,
        include_query_params: bool = False
    ):
        """
        Inicializa o middleware.

        Args:
            app: Aplicacao FastAPI
            get_db: Funcao para obter sessao do banco
            exempt_paths: Paths que nao devem ser rastreados
            track_successful_only: Se True, so rastreia status 2xx
            include_query_params: Se True, inclui query params nos metadados
        """
        super().__init__(app)
        self.get_db = get_db
        self.track_successful_only = track_successful_only
        self.include_query_params = include_query_params
        self.exempt_paths = exempt_paths or [
            "/health",
            "/healthz",
            "/ready",
            "/metrics",
            "/docs",
            "/redoc",
            "/openapi.json",
            "/favicon.ico",
            "/static",
        ]

    async def dispatch(self, request: Request, call_next) -> Response:
        """Processa a requisicao com tracking de uso."""
        # Verificar paths isentos
        path = request.url.path
        if self._is_exempt_path(path):
            return await call_next(request)

        # Gerar correlation ID
        correlation_id = request.headers.get("X-Correlation-ID")
        if not correlation_id:
            correlation_id = f"REQ-{uuid.uuid4().hex[:12].upper()}"

        # Adicionar ao request state
        request.state.correlation_id = correlation_id

        # Iniciar timer
        start_time = time.time()

        # Processar request
        response = await call_next(request)

        # Calcular duracao
        duration_ms = (time.time() - start_time) * 1000

        # Adicionar correlation ID na response
        response.headers["X-Correlation-ID"] = correlation_id

        # Verificar se deve rastrear
        if self.track_successful_only and response.status_code >= 400:
            return response

        # Obter tenant_id do contexto
        tenant_id = get_current_tenant_id()

        if tenant_id:
            # Rastrear chamada API
            try:
                await self._track_api_call(
                    request=request,
                    response=response,
                    tenant_id=tenant_id,
                    duration_ms=duration_ms,
                    correlation_id=correlation_id
                )
            except Exception as e:
                logger.error(f"Erro ao rastrear API call: {e}")

        return response

    def _is_exempt_path(self, path: str) -> bool:
        """Verifica se o path esta isento de tracking."""
        for exempt in self.exempt_paths:
            if path == exempt or path.startswith(exempt):
                return True
        return False

    async def _track_api_call(
        self,
        request: Request,
        response: Response,
        tenant_id: str,
        duration_ms: float,
        correlation_id: str
    ) -> None:
        """
        Registra chamada de API no sistema de metering.

        Args:
            request: Request FastAPI
            response: Response
            tenant_id: ID do tenant
            duration_ms: Duracao em ms
            correlation_id: ID de correlacao
        """
        db: Session = None
        try:
            db = next(self.get_db())
            usage_service = UsageService(db)

            # Extrair informacoes do request
            method = request.method
            path = request.url.path

            # Extrair user_id se disponivel
            user_id = getattr(request.state, "user_id", None)
            project_id = self._extract_project_id(path)

            # IP do cliente
            ip_address = self._get_client_ip(request)

            # User-Agent
            user_agent = request.headers.get("User-Agent", "")

            # Registrar evento
            usage_service.track_api_call(
                tenant_id=tenant_id,
                method=method,
                path=path,
                status_code=response.status_code,
                duration_ms=duration_ms,
                user_id=user_id,
                project_id=project_id,
                ip_address=ip_address,
                user_agent=user_agent,
                correlation_id=correlation_id
            )

        except Exception as e:
            logger.error(f"Erro ao registrar API call: {e}")

        finally:
            if db:
                db.close()

    def _get_client_ip(self, request: Request) -> str:
        """Extrai IP do cliente considerando proxies."""
        # Verificar headers de proxy
        forwarded = request.headers.get("X-Forwarded-For")
        if forwarded:
            return forwarded.split(",")[0].strip()

        real_ip = request.headers.get("X-Real-IP")
        if real_ip:
            return real_ip

        # Fallback para client host
        if request.client:
            return request.client.host

        return "unknown"

    def _extract_project_id(self, path: str) -> Optional[str]:
        """
        Tenta extrair project_id do path.

        Args:
            path: Path da URL

        Returns:
            project_id ou None
        """
        parts = path.split("/")
        for i, part in enumerate(parts):
            if part == "projects" and i + 1 < len(parts):
                potential_id = parts[i + 1]
                if potential_id.startswith("PRJ-") or potential_id.isalnum():
                    return potential_id
        return None


class LLMUsageTracker:
    """
    Tracker para uso de tokens LLM.

    Uso:
        tracker = LLMUsageTracker(db_session)

        # Antes de chamar a API Claude
        response = await claude.complete(...)

        # Registrar uso
        tracker.track_completion(
            tenant_id="TEN-123",
            model="claude-sonnet-4-20250514",
            input_tokens=response.usage.input_tokens,
            output_tokens=response.usage.output_tokens,
            job_id="JOB-456"
        )
    """

    def __init__(self, db: Session):
        """
        Inicializa o tracker.

        Args:
            db: Sessao do banco de dados
        """
        self.db = db
        self.usage_service = UsageService(db)

    def track_completion(
        self,
        tenant_id: str,
        model: str,
        input_tokens: int,
        output_tokens: int,
        job_id: Optional[str] = None,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None,
        correlation_id: Optional[str] = None
    ) -> dict:
        """
        Registra uso de tokens de uma completion.

        Args:
            tenant_id: ID do tenant
            model: Nome do modelo
            input_tokens: Tokens de entrada
            output_tokens: Tokens de saida
            job_id: ID do job
            project_id: ID do projeto
            user_id: ID do usuario
            correlation_id: ID de correlacao

        Returns:
            Dict com evento e status de limite
        """
        event, is_over_limit = self.usage_service.track_llm_tokens(
            tenant_id=tenant_id,
            model=model,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            job_id=job_id,
            project_id=project_id,
            user_id=user_id,
            correlation_id=correlation_id
        )

        return {
            "event_id": event.event_id,
            "total_tokens": input_tokens + output_tokens,
            "cost_cents": event.cost_cents,
            "is_over_limit": is_over_limit
        }

    def track_streaming(
        self,
        tenant_id: str,
        model: str,
        prompt_tokens: int,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None
    ):
        """
        Contexto para tracking de streaming.

        Uso:
            with tracker.track_streaming("TEN-123", "claude-3-sonnet"):
                async for chunk in stream:
                    yield chunk
                # Tokens finais sao registrados automaticamente

        Args:
            tenant_id: ID do tenant
            model: Nome do modelo
            prompt_tokens: Tokens do prompt (conhecidos antecipadamente)
            project_id: ID do projeto
            user_id: ID do usuario

        Returns:
            Context manager
        """
        return StreamingTracker(
            tracker=self,
            tenant_id=tenant_id,
            model=model,
            prompt_tokens=prompt_tokens,
            project_id=project_id,
            user_id=user_id
        )


class StreamingTracker:
    """Context manager para tracking de streams LLM."""

    def __init__(
        self,
        tracker: LLMUsageTracker,
        tenant_id: str,
        model: str,
        prompt_tokens: int,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None
    ):
        self.tracker = tracker
        self.tenant_id = tenant_id
        self.model = model
        self.prompt_tokens = prompt_tokens
        self.project_id = project_id
        self.user_id = user_id
        self.output_tokens = 0

    def add_tokens(self, count: int):
        """Adiciona tokens de output ao contador."""
        self.output_tokens += count

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        # Registrar uso final
        if self.output_tokens > 0:
            self.tracker.track_completion(
                tenant_id=self.tenant_id,
                model=self.model,
                input_tokens=self.prompt_tokens,
                output_tokens=self.output_tokens,
                project_id=self.project_id,
                user_id=self.user_id
            )


class StorageUsageTracker:
    """
    Tracker para uso de storage.

    Uso:
        tracker = StorageUsageTracker(db_session)

        # Apos upload
        tracker.track_upload(
            tenant_id="TEN-123",
            file_name="documento.pdf",
            file_size=1024000
        )
    """

    def __init__(self, db: Session):
        """
        Inicializa o tracker.

        Args:
            db: Sessao do banco de dados
        """
        self.db = db
        self.usage_service = UsageService(db)

    def track_upload(
        self,
        tenant_id: str,
        file_name: str,
        file_size: int,
        file_type: Optional[str] = None,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None
    ) -> dict:
        """
        Registra upload de arquivo.

        Args:
            tenant_id: ID do tenant
            file_name: Nome do arquivo
            file_size: Tamanho em bytes
            file_type: Tipo MIME
            user_id: ID do usuario
            project_id: ID do projeto

        Returns:
            Dict com evento
        """
        event = self.usage_service.track_storage(
            tenant_id=tenant_id,
            operation="upload",
            file_name=file_name,
            file_size=file_size,
            file_type=file_type,
            user_id=user_id,
            project_id=project_id
        )

        return {
            "event_id": event.event_id,
            "file_size": file_size,
            "cost_cents": event.cost_cents
        }

    def track_download(
        self,
        tenant_id: str,
        file_name: str,
        file_size: int,
        user_id: Optional[str] = None,
        project_id: Optional[str] = None
    ) -> dict:
        """
        Registra download de arquivo.

        Args:
            tenant_id: ID do tenant
            file_name: Nome do arquivo
            file_size: Tamanho em bytes
            user_id: ID do usuario
            project_id: ID do projeto

        Returns:
            Dict com evento
        """
        event = self.usage_service.track_storage(
            tenant_id=tenant_id,
            operation="download",
            file_name=file_name,
            file_size=file_size,
            user_id=user_id,
            project_id=project_id
        )

        return {
            "event_id": event.event_id,
            "file_size": file_size
        }


class ComputeUsageTracker:
    """
    Tracker para uso de compute (workers).

    Uso:
        tracker = ComputeUsageTracker(db_session)

        start_time = time.time()
        # ... executar job ...
        duration = time.time() - start_time

        tracker.track_job(
            tenant_id="TEN-123",
            worker_id="WRK-01",
            job_id="JOB-456",
            duration_seconds=int(duration)
        )
    """

    def __init__(self, db: Session):
        """
        Inicializa o tracker.

        Args:
            db: Sessao do banco de dados
        """
        self.db = db
        self.usage_service = UsageService(db)

    def track_job(
        self,
        tenant_id: str,
        worker_id: str,
        job_id: str,
        duration_seconds: int,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None
    ) -> dict:
        """
        Registra execucao de job.

        Args:
            tenant_id: ID do tenant
            worker_id: ID do worker
            job_id: ID do job
            duration_seconds: Duracao em segundos
            project_id: ID do projeto
            user_id: ID do usuario

        Returns:
            Dict com evento
        """
        event = self.usage_service.track_compute(
            tenant_id=tenant_id,
            worker_id=worker_id,
            job_id=job_id,
            duration_seconds=duration_seconds,
            project_id=project_id,
            user_id=user_id
        )

        return {
            "event_id": event.event_id,
            "duration_seconds": duration_seconds,
            "cost_cents": event.cost_cents
        }

    def track_job_context(
        self,
        tenant_id: str,
        worker_id: str,
        job_id: str,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None
    ):
        """
        Context manager para tracking automatico de duracao.

        Uso:
            with tracker.track_job_context("TEN-123", "WRK-01", "JOB-456"):
                # ... executar job ...
                pass
            # Duracao registrada automaticamente

        Args:
            tenant_id: ID do tenant
            worker_id: ID do worker
            job_id: ID do job
            project_id: ID do projeto
            user_id: ID do usuario

        Returns:
            Context manager
        """
        return JobTracker(
            tracker=self,
            tenant_id=tenant_id,
            worker_id=worker_id,
            job_id=job_id,
            project_id=project_id,
            user_id=user_id
        )


class JobTracker:
    """Context manager para tracking de jobs."""

    def __init__(
        self,
        tracker: ComputeUsageTracker,
        tenant_id: str,
        worker_id: str,
        job_id: str,
        project_id: Optional[str] = None,
        user_id: Optional[str] = None
    ):
        self.tracker = tracker
        self.tenant_id = tenant_id
        self.worker_id = worker_id
        self.job_id = job_id
        self.project_id = project_id
        self.user_id = user_id
        self.start_time = None

    def __enter__(self):
        self.start_time = time.time()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.start_time:
            duration = int(time.time() - self.start_time)
            if duration > 0:
                self.tracker.track_job(
                    tenant_id=self.tenant_id,
                    worker_id=self.worker_id,
                    job_id=self.job_id,
                    duration_seconds=duration,
                    project_id=self.project_id,
                    user_id=self.user_id
                )


class AuthUsageTracker:
    """
    Tracker para eventos de autenticacao.

    Uso:
        tracker = AuthUsageTracker(db_session)

        # Apos login bem sucedido
        tracker.track_login(
            tenant_id="TEN-123",
            user_id="USR-456",
            ip_address="192.168.1.1"
        )
    """

    def __init__(self, db: Session):
        """
        Inicializa o tracker.

        Args:
            db: Sessao do banco de dados
        """
        self.db = db
        self.usage_service = UsageService(db)

    def track_login(
        self,
        tenant_id: str,
        user_id: str,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        success: bool = True
    ) -> dict:
        """
        Registra evento de login.

        Args:
            tenant_id: ID do tenant
            user_id: ID do usuario
            ip_address: IP do cliente
            user_agent: User-Agent
            success: Se login foi bem sucedido

        Returns:
            Dict com evento
        """
        event = self.usage_service.track_login(
            tenant_id=tenant_id,
            user_id=user_id,
            ip_address=ip_address,
            user_agent=user_agent,
            success=success
        )

        return {
            "event_id": event.event_id,
            "success": success
        }

    def track_session(
        self,
        tenant_id: str,
        user_id: str,
        session_id: str
    ) -> dict:
        """
        Registra sessao ativa.

        Args:
            tenant_id: ID do tenant
            user_id: ID do usuario
            session_id: ID da sessao

        Returns:
            Dict com evento
        """
        event = self.usage_service.track_session_active(
            tenant_id=tenant_id,
            user_id=user_id,
            session_id=session_id
        )

        return {
            "event_id": event.event_id,
            "session_id": session_id
        }


# =============================================================================
# DECORATORS PARA TRACKING
# =============================================================================

def track_api_usage(get_db: Callable):
    """
    Decorator para tracking de uso de API em endpoints especificos.

    Uso:
        @router.post("/expensive-operation")
        @track_api_usage(get_db)
        async def expensive_operation(request: Request):
            ...

    Args:
        get_db: Funcao para obter sessao do banco
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            request = kwargs.get("request")
            if not request:
                for arg in args:
                    if isinstance(arg, Request):
                        request = arg
                        break

            start_time = time.time()
            result = await func(*args, **kwargs)
            duration_ms = (time.time() - start_time) * 1000

            # Track usage
            tenant_id = get_current_tenant_id()
            if tenant_id and request:
                db = next(get_db())
                try:
                    usage_service = UsageService(db)
                    usage_service.track_api_call(
                        tenant_id=tenant_id,
                        method=request.method,
                        path=request.url.path,
                        status_code=200,
                        duration_ms=duration_ms,
                        user_id=getattr(request.state, "user_id", None),
                        ip_address=request.client.host if request.client else None,
                        user_agent=request.headers.get("User-Agent")
                    )
                except Exception as e:
                    logger.error(f"Erro no decorator track_api_usage: {e}")
                finally:
                    db.close()

            return result
        return wrapper
    return decorator


def track_llm_usage(get_db: Callable, model: str = "claude-sonnet-4-20250514"):
    """
    Decorator para tracking de uso de LLM.

    Uso:
        @track_llm_usage(get_db, model="claude-3-opus")
        async def generate_code(prompt: str) -> str:
            response = await claude.complete(prompt)
            return response

    Args:
        get_db: Funcao para obter sessao do banco
        model: Nome do modelo LLM
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        async def wrapper(*args, **kwargs):
            result = await func(*args, **kwargs)

            # Extrair tokens do resultado se disponivel
            input_tokens = 0
            output_tokens = 0

            if hasattr(result, "usage"):
                input_tokens = getattr(result.usage, "input_tokens", 0)
                output_tokens = getattr(result.usage, "output_tokens", 0)
            elif isinstance(result, dict) and "usage" in result:
                usage = result["usage"]
                input_tokens = usage.get("input_tokens", 0)
                output_tokens = usage.get("output_tokens", 0)

            if input_tokens > 0 or output_tokens > 0:
                tenant_id = get_current_tenant_id()
                if tenant_id:
                    db = next(get_db())
                    try:
                        tracker = LLMUsageTracker(db)
                        tracker.track_completion(
                            tenant_id=tenant_id,
                            model=model,
                            input_tokens=input_tokens,
                            output_tokens=output_tokens
                        )
                    except Exception as e:
                        logger.error(f"Erro no decorator track_llm_usage: {e}")
                    finally:
                        db.close()

            return result
        return wrapper
    return decorator

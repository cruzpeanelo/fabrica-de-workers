"""
Fabrica de Agentes - Python SDK
===============================

Cliente Python oficial para a API da Fabrica de Agentes.

Instalacao:
    pip install fabrica-agentes-sdk

Uso:
    from fabrica_client import FabricaClient

    client = FabricaClient(api_key="sua-api-key")

    # Listar projetos
    projects = client.projects.list()

    # Criar uma story
    story = client.stories.create(
        project_id="proj-001",
        title="Implementar login",
        persona="usuario",
        action="fazer login com email",
        benefit="acessar minha conta"
    )

    # Executar job
    job = client.jobs.create(
        description="Criar API REST para usuarios",
        tech_stack="python,fastapi",
        features=["crud", "auth", "pagination"]
    )

    # Acompanhar progresso
    job = client.jobs.wait_for_completion(job.job_id)
"""

from __future__ import annotations

import time
import json
import base64
from datetime import datetime
from enum import Enum
from typing import (
    Any, Callable, Dict, Generic, Iterator, List,
    Optional, TypeVar, Union, overload
)
from dataclasses import dataclass, field
from urllib.parse import urljoin, urlencode

# Tentar importar httpx, fallback para requests
try:
    import httpx
    HTTP_CLIENT = "httpx"
except ImportError:
    try:
        import requests
        HTTP_CLIENT = "requests"
    except ImportError:
        raise ImportError(
            "Instale httpx ou requests: pip install httpx"
        )

__version__ = "1.0.0"
__author__ = "Fabrica de Agentes Team"


# =============================================================================
# Types and Enums
# =============================================================================

T = TypeVar("T")


class JobStatus(str, Enum):
    """Status de um job"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class StoryStatus(str, Enum):
    """Status de uma story"""
    BACKLOG = "backlog"
    READY = "ready"
    IN_PROGRESS = "in_progress"
    REVIEW = "review"
    TESTING = "testing"
    DONE = "done"


class Priority(str, Enum):
    """Prioridade de uma story"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    URGENT = "urgent"


class Complexity(str, Enum):
    """Complexidade de um job"""
    SIMPLE = "simple"
    MEDIUM = "medium"
    COMPLEX = "complex"
    VERY_COMPLEX = "very_complex"


# =============================================================================
# Exceptions
# =============================================================================

class FabricaError(Exception):
    """Excecao base do SDK"""
    pass


class APIError(FabricaError):
    """Erro retornado pela API"""

    def __init__(
        self,
        message: str,
        status_code: int = None,
        error_code: str = None,
        details: Dict[str, Any] = None
    ):
        super().__init__(message)
        self.status_code = status_code
        self.error_code = error_code
        self.details = details or {}

    def __str__(self):
        msg = f"{self.message}"
        if self.status_code:
            msg = f"[{self.status_code}] {msg}"
        if self.error_code:
            msg = f"{msg} ({self.error_code})"
        return msg


class AuthenticationError(APIError):
    """Erro de autenticacao"""
    pass


class RateLimitError(APIError):
    """Rate limit excedido"""

    def __init__(
        self,
        message: str,
        retry_after: int = None,
        **kwargs
    ):
        super().__init__(message, **kwargs)
        self.retry_after = retry_after


class ValidationError(APIError):
    """Erro de validacao"""
    pass


class NotFoundError(APIError):
    """Recurso nao encontrado"""
    pass


class TimeoutError(FabricaError):
    """Timeout na requisicao"""
    pass


# =============================================================================
# Models
# =============================================================================

@dataclass
class Project:
    """Modelo de Projeto"""
    project_id: str
    name: str
    description: Optional[str] = None
    tech_stack: Optional[str] = None
    status: str = "active"
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Project":
        return cls(
            project_id=data.get("project_id") or data.get("id"),
            name=data.get("name"),
            description=data.get("description"),
            tech_stack=data.get("tech_stack"),
            status=data.get("status", "active"),
            created_at=_parse_datetime(data.get("created_at")),
            updated_at=_parse_datetime(data.get("updated_at"))
        )


@dataclass
class Story:
    """Modelo de User Story"""
    story_id: str
    title: str
    project_id: Optional[str] = None
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: List[str] = field(default_factory=list)
    definition_of_done: List[str] = field(default_factory=list)
    story_points: Optional[int] = None
    complexity: Optional[str] = None
    status: str = "backlog"
    priority: str = "medium"
    epic_id: Optional[str] = None
    sprint_id: Optional[str] = None
    progress: int = 0
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Story":
        return cls(
            story_id=data.get("story_id") or data.get("id"),
            title=data.get("title"),
            project_id=data.get("project_id"),
            persona=data.get("persona"),
            action=data.get("action"),
            benefit=data.get("benefit"),
            acceptance_criteria=data.get("acceptance_criteria", []),
            definition_of_done=data.get("definition_of_done", []),
            story_points=data.get("story_points"),
            complexity=data.get("complexity"),
            status=data.get("status", "backlog"),
            priority=data.get("priority", "medium"),
            epic_id=data.get("epic_id"),
            sprint_id=data.get("sprint_id"),
            progress=data.get("progress", 0),
            created_at=_parse_datetime(data.get("created_at")),
            updated_at=_parse_datetime(data.get("updated_at"))
        )


@dataclass
class Job:
    """Modelo de Job"""
    job_id: str
    description: str
    tech_stack: Optional[str] = None
    features: List[str] = field(default_factory=list)
    status: str = "pending"
    current_step: str = ""
    progress: float = 0.0
    worker_id: Optional[str] = None
    output_path: Optional[str] = None
    error_message: Optional[str] = None
    model: Optional[str] = None
    queued_at: Optional[datetime] = None
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Job":
        return cls(
            job_id=data.get("job_id") or data.get("id"),
            description=data.get("description"),
            tech_stack=data.get("tech_stack"),
            features=data.get("features", []),
            status=data.get("status", "pending"),
            current_step=data.get("current_step", ""),
            progress=data.get("progress", 0.0),
            worker_id=data.get("worker_id"),
            output_path=data.get("output_path"),
            error_message=data.get("error_message"),
            model=data.get("model"),
            queued_at=_parse_datetime(data.get("queued_at")),
            started_at=_parse_datetime(data.get("started_at")),
            completed_at=_parse_datetime(data.get("completed_at"))
        )

    @property
    def is_completed(self) -> bool:
        return self.status == JobStatus.COMPLETED

    @property
    def is_failed(self) -> bool:
        return self.status == JobStatus.FAILED

    @property
    def is_running(self) -> bool:
        return self.status == JobStatus.RUNNING

    @property
    def is_pending(self) -> bool:
        return self.status == JobStatus.PENDING


@dataclass
class Worker:
    """Modelo de Worker"""
    worker_id: str
    status: str
    current_job_id: Optional[str] = None
    model: str = "sonnet"
    mcp_tools: List[str] = field(default_factory=list)
    jobs_completed: int = 0
    jobs_failed: int = 0
    avg_job_duration: float = 0.0
    last_heartbeat: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Worker":
        return cls(
            worker_id=data.get("worker_id") or data.get("id"),
            status=data.get("status"),
            current_job_id=data.get("current_job_id"),
            model=data.get("model", "sonnet"),
            mcp_tools=data.get("mcp_tools", []),
            jobs_completed=data.get("jobs_completed", 0),
            jobs_failed=data.get("jobs_failed", 0),
            avg_job_duration=data.get("avg_job_duration", 0.0),
            last_heartbeat=_parse_datetime(data.get("last_heartbeat"))
        )


@dataclass
class Agent:
    """Modelo de Agente"""
    agent_id: str
    name: str
    role: str
    department: Optional[str] = None
    skills: List[str] = field(default_factory=list)
    level: str = "analyst"
    status: str = "available"

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Agent":
        return cls(
            agent_id=data.get("agent_id") or data.get("id"),
            name=data.get("name"),
            role=data.get("role"),
            department=data.get("department"),
            skills=data.get("skills", []),
            level=data.get("level", "analyst"),
            status=data.get("status", "available")
        )


@dataclass
class PaginatedResponse(Generic[T]):
    """Resposta paginada"""
    items: List[T]
    cursor: Optional[str] = None
    has_more: bool = False
    total_count: Optional[int] = None

    def __iter__(self) -> Iterator[T]:
        return iter(self.items)

    def __len__(self) -> int:
        return len(self.items)


# =============================================================================
# Utilities
# =============================================================================

def _parse_datetime(value: Any) -> Optional[datetime]:
    """Parse datetime from string or None"""
    if value is None:
        return None
    if isinstance(value, datetime):
        return value
    if isinstance(value, str):
        try:
            # ISO format
            return datetime.fromisoformat(value.replace("Z", "+00:00"))
        except ValueError:
            pass
    return None


# =============================================================================
# HTTP Client
# =============================================================================

class HTTPClient:
    """Cliente HTTP abstrato"""

    def __init__(
        self,
        base_url: str,
        api_key: str = None,
        token: str = None,
        timeout: int = 30,
        headers: Dict[str, str] = None,
        api_version: str = "v1"
    ):
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.token = token
        self.timeout = timeout
        self.api_version = api_version
        self._extra_headers = headers or {}

        # Cliente HTTP
        if HTTP_CLIENT == "httpx":
            self._client = httpx.Client(timeout=timeout)
        else:
            self._session = requests.Session()

    def _get_headers(self, extra: Dict[str, str] = None) -> Dict[str, str]:
        """Monta headers da requisicao"""
        headers = {
            "Content-Type": "application/json",
            "Accept": "application/json",
            "User-Agent": f"FabricaSDK-Python/{__version__}",
            "X-API-Version": self.api_version,
            **self._extra_headers
        }

        if self.api_key:
            headers["X-API-Key"] = self.api_key

        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"

        if extra:
            headers.update(extra)

        return headers

    def _handle_response(self, response) -> Dict[str, Any]:
        """Processa resposta HTTP"""
        status_code = response.status_code

        # Parse body
        try:
            if HTTP_CLIENT == "httpx":
                body = response.json()
            else:
                body = response.json()
        except (json.JSONDecodeError, ValueError):
            body = {"message": response.text}

        # Check for errors
        if status_code >= 400:
            error_code = body.get("error_code", "UNKNOWN_ERROR")
            message = body.get("message") or body.get("detail", "Unknown error")

            if status_code == 401:
                raise AuthenticationError(
                    message,
                    status_code=status_code,
                    error_code=error_code
                )
            elif status_code == 404:
                raise NotFoundError(
                    message,
                    status_code=status_code,
                    error_code=error_code
                )
            elif status_code == 422:
                raise ValidationError(
                    message,
                    status_code=status_code,
                    error_code=error_code,
                    details=body.get("detail", [])
                )
            elif status_code == 429:
                retry_after = response.headers.get("Retry-After")
                raise RateLimitError(
                    message,
                    status_code=status_code,
                    error_code=error_code,
                    retry_after=int(retry_after) if retry_after else None
                )
            else:
                raise APIError(
                    message,
                    status_code=status_code,
                    error_code=error_code,
                    details=body
                )

        return body

    def request(
        self,
        method: str,
        path: str,
        params: Dict[str, Any] = None,
        json_data: Dict[str, Any] = None,
        headers: Dict[str, str] = None,
        idempotency_key: str = None
    ) -> Dict[str, Any]:
        """Executa requisicao HTTP"""
        url = urljoin(self.base_url, path)
        req_headers = self._get_headers(headers)

        if idempotency_key:
            req_headers["Idempotency-Key"] = idempotency_key

        if HTTP_CLIENT == "httpx":
            response = self._client.request(
                method=method,
                url=url,
                params=params,
                json=json_data,
                headers=req_headers
            )
        else:
            response = self._session.request(
                method=method,
                url=url,
                params=params,
                json=json_data,
                headers=req_headers,
                timeout=self.timeout
            )

        return self._handle_response(response)

    def get(self, path: str, **kwargs) -> Dict[str, Any]:
        return self.request("GET", path, **kwargs)

    def post(self, path: str, **kwargs) -> Dict[str, Any]:
        return self.request("POST", path, **kwargs)

    def put(self, path: str, **kwargs) -> Dict[str, Any]:
        return self.request("PUT", path, **kwargs)

    def patch(self, path: str, **kwargs) -> Dict[str, Any]:
        return self.request("PATCH", path, **kwargs)

    def delete(self, path: str, **kwargs) -> Dict[str, Any]:
        return self.request("DELETE", path, **kwargs)

    def close(self):
        """Fecha conexao"""
        if HTTP_CLIENT == "httpx":
            self._client.close()
        else:
            self._session.close()


# =============================================================================
# Resource Handlers
# =============================================================================

class ProjectsResource:
    """Gerenciamento de Projetos"""

    def __init__(self, client: HTTPClient):
        self._client = client

    def list(
        self,
        status: str = None,
        cursor: str = None,
        limit: int = 20
    ) -> PaginatedResponse[Project]:
        """Lista projetos"""
        params = {"limit": limit}
        if status:
            params["status"] = status
        if cursor:
            params["cursor"] = cursor

        data = self._client.get("/api/v1/projects", params=params)

        items = [Project.from_dict(p) for p in data.get("items", data)]
        return PaginatedResponse(
            items=items,
            cursor=data.get("cursor"),
            has_more=data.get("has_more", False),
            total_count=data.get("total_count")
        )

    def get(self, project_id: str) -> Project:
        """Busca projeto por ID"""
        data = self._client.get(f"/api/v1/projects/{project_id}")
        return Project.from_dict(data)

    def create(
        self,
        name: str,
        description: str = None,
        tech_stack: str = None,
        **kwargs
    ) -> Project:
        """Cria novo projeto"""
        payload = {
            "name": name,
            "description": description,
            "tech_stack": tech_stack,
            **kwargs
        }
        data = self._client.post("/api/v1/projects", json_data=payload)
        return Project.from_dict(data)

    def update(self, project_id: str, **kwargs) -> Project:
        """Atualiza projeto"""
        data = self._client.put(
            f"/api/v1/projects/{project_id}",
            json_data=kwargs
        )
        return Project.from_dict(data)

    def delete(self, project_id: str) -> bool:
        """Remove projeto"""
        self._client.delete(f"/api/v1/projects/{project_id}")
        return True


class StoriesResource:
    """Gerenciamento de User Stories"""

    def __init__(self, client: HTTPClient):
        self._client = client

    def list(
        self,
        project_id: str = None,
        status: str = None,
        epic_id: str = None,
        sprint_id: str = None,
        cursor: str = None,
        limit: int = 20
    ) -> PaginatedResponse[Story]:
        """Lista stories"""
        params = {"limit": limit}
        if project_id:
            params["project_id"] = project_id
        if status:
            params["status"] = status
        if epic_id:
            params["epic_id"] = epic_id
        if sprint_id:
            params["sprint_id"] = sprint_id
        if cursor:
            params["cursor"] = cursor

        data = self._client.get("/api/stories", params=params)

        items = [Story.from_dict(s) for s in data.get("items", data)]
        return PaginatedResponse(
            items=items,
            cursor=data.get("cursor"),
            has_more=data.get("has_more", False),
            total_count=data.get("total_count")
        )

    def get(self, story_id: str) -> Story:
        """Busca story por ID"""
        data = self._client.get(f"/api/stories/{story_id}")
        return Story.from_dict(data)

    def create(
        self,
        title: str,
        project_id: str = None,
        persona: str = None,
        action: str = None,
        benefit: str = None,
        acceptance_criteria: List[str] = None,
        story_points: int = None,
        priority: str = "medium",
        **kwargs
    ) -> Story:
        """Cria nova story"""
        payload = {
            "title": title,
            "project_id": project_id,
            "persona": persona,
            "action": action,
            "benefit": benefit,
            "acceptance_criteria": acceptance_criteria or [],
            "story_points": story_points,
            "priority": priority,
            **kwargs
        }
        data = self._client.post("/api/stories", json_data=payload)
        return Story.from_dict(data)

    def update(self, story_id: str, **kwargs) -> Story:
        """Atualiza story"""
        data = self._client.put(f"/api/stories/{story_id}", json_data=kwargs)
        return Story.from_dict(data)

    def delete(self, story_id: str) -> bool:
        """Remove story"""
        self._client.delete(f"/api/stories/{story_id}")
        return True

    def move(self, story_id: str, status: str) -> Story:
        """Move story no Kanban"""
        data = self._client.patch(
            f"/api/stories/{story_id}/move",
            json_data={"status": status}
        )
        return Story.from_dict(data)


class JobsResource:
    """Gerenciamento de Jobs"""

    def __init__(self, client: HTTPClient):
        self._client = client

    def list(
        self,
        status: str = None,
        cursor: str = None,
        limit: int = 50
    ) -> PaginatedResponse[Job]:
        """Lista jobs"""
        params = {"limit": limit}
        if status:
            params["status"] = status
        if cursor:
            params["cursor"] = cursor

        data = self._client.get("/api/v1/jobs", params=params)

        items = [Job.from_dict(j) for j in data.get("items", data)]
        return PaginatedResponse(
            items=items,
            cursor=data.get("cursor"),
            has_more=data.get("has_more", False),
            total_count=data.get("total_count")
        )

    def get(self, job_id: str) -> Job:
        """Busca job por ID"""
        data = self._client.get(f"/api/v1/jobs/{job_id}")
        return Job.from_dict(data)

    def create(
        self,
        description: str,
        tech_stack: str = None,
        features: List[str] = None,
        project_id: str = None,
        model: str = None,
        complexity: str = None,
        story_points: int = None,
        idempotency_key: str = None
    ) -> Job:
        """
        Cria novo job.

        Args:
            description: Descricao do que construir
            tech_stack: Stack tecnologica (python, fastapi, react, etc)
            features: Lista de features
            project_id: ID do projeto associado
            model: Modelo Claude (opus, sonnet, haiku)
            complexity: Complexidade (simple, medium, complex, very_complex)
            story_points: Story points para selecao de modelo
            idempotency_key: Chave de idempotencia

        Returns:
            Job criado
        """
        payload = {
            "description": description,
            "tech_stack": tech_stack,
            "features": features or [],
            "project_id": project_id,
            "model": model,
            "complexity": complexity,
            "story_points": story_points
        }
        # Remove None values
        payload = {k: v for k, v in payload.items() if v is not None}

        data = self._client.post(
            "/api/v1/jobs",
            json_data=payload,
            idempotency_key=idempotency_key
        )
        return Job.from_dict(data)

    def cancel(self, job_id: str) -> bool:
        """Cancela job pendente"""
        self._client.delete(f"/api/v1/jobs/{job_id}")
        return True

    def wait_for_completion(
        self,
        job_id: str,
        timeout: int = 600,
        poll_interval: int = 5,
        callback: Callable[[Job], None] = None
    ) -> Job:
        """
        Aguarda conclusao de um job.

        Args:
            job_id: ID do job
            timeout: Timeout em segundos
            poll_interval: Intervalo de polling em segundos
            callback: Funcao chamada a cada polling

        Returns:
            Job concluido

        Raises:
            TimeoutError: Se timeout for excedido
            APIError: Se job falhar
        """
        start_time = time.time()

        while True:
            job = self.get(job_id)

            if callback:
                callback(job)

            if job.is_completed:
                return job

            if job.is_failed:
                raise APIError(
                    f"Job falhou: {job.error_message}",
                    error_code="JOB_FAILED"
                )

            if job.status == JobStatus.CANCELLED:
                raise APIError(
                    "Job foi cancelado",
                    error_code="JOB_CANCELLED"
                )

            elapsed = time.time() - start_time
            if elapsed >= timeout:
                raise TimeoutError(
                    f"Timeout aguardando job {job_id} (elapsed: {elapsed}s)"
                )

            time.sleep(poll_interval)


class WorkersResource:
    """Gerenciamento de Workers"""

    def __init__(self, client: HTTPClient):
        self._client = client

    def list(self) -> List[Worker]:
        """Lista todos os workers"""
        data = self._client.get("/api/v1/workers")
        return [Worker.from_dict(w) for w in data]

    def get(self, worker_id: str) -> Worker:
        """Busca worker por ID"""
        data = self._client.get(f"/api/v1/workers/{worker_id}")
        return Worker.from_dict(data)

    def list_active(self) -> List[Worker]:
        """Lista workers ativos"""
        data = self._client.get("/api/v1/workers/active")
        return [Worker.from_dict(w) for w in data]


class AgentsResource:
    """Gerenciamento de Agentes"""

    def __init__(self, client: HTTPClient):
        self._client = client

    def list(
        self,
        department: str = None,
        role: str = None,
        cursor: str = None,
        limit: int = 50
    ) -> PaginatedResponse[Agent]:
        """Lista agentes"""
        params = {"limit": limit}
        if department:
            params["department"] = department
        if role:
            params["role"] = role
        if cursor:
            params["cursor"] = cursor

        data = self._client.get("/api/v1/agents", params=params)

        items = [Agent.from_dict(a) for a in data.get("items", data)]
        return PaginatedResponse(
            items=items,
            cursor=data.get("cursor"),
            has_more=data.get("has_more", False),
            total_count=data.get("total_count")
        )

    def get(self, agent_id: str) -> Agent:
        """Busca agente por ID"""
        data = self._client.get(f"/api/v1/agents/{agent_id}")
        return Agent.from_dict(data)


class QueueResource:
    """Informacoes da Fila"""

    def __init__(self, client: HTTPClient):
        self._client = client

    def stats(self) -> Dict[str, int]:
        """Retorna estatisticas da fila"""
        return self._client.get("/api/v1/queue/stats")

    def peek(self, limit: int = 10) -> List[Job]:
        """Lista jobs na fila sem remover"""
        data = self._client.get("/api/v1/queue/peek", params={"limit": limit})
        return [Job.from_dict(j) for j in data]


# =============================================================================
# Main Client
# =============================================================================

class FabricaClient:
    """
    Cliente principal da API Fabrica de Agentes.

    Uso:
        client = FabricaClient(
            api_key="sua-api-key",
            base_url="http://localhost:9001"
        )

        # Operacoes com projetos
        projects = client.projects.list()
        project = client.projects.create(name="Meu Projeto")

        # Operacoes com stories
        story = client.stories.create(
            title="Implementar login",
            project_id=project.project_id
        )

        # Operacoes com jobs
        job = client.jobs.create(description="Criar API REST")
        job = client.jobs.wait_for_completion(job.job_id)
    """

    def __init__(
        self,
        api_key: str = None,
        token: str = None,
        base_url: str = "http://localhost:9001",
        timeout: int = 30,
        api_version: str = "v1"
    ):
        """
        Inicializa o cliente.

        Args:
            api_key: API Key para autenticacao
            token: Token JWT para autenticacao
            base_url: URL base da API
            timeout: Timeout das requisicoes em segundos
            api_version: Versao da API a usar
        """
        self._http = HTTPClient(
            base_url=base_url,
            api_key=api_key,
            token=token,
            timeout=timeout,
            api_version=api_version
        )

        # Resources
        self.projects = ProjectsResource(self._http)
        self.stories = StoriesResource(self._http)
        self.jobs = JobsResource(self._http)
        self.workers = WorkersResource(self._http)
        self.agents = AgentsResource(self._http)
        self.queue = QueueResource(self._http)

    def health(self) -> Dict[str, Any]:
        """Verifica saude da API"""
        return self._http.get("/api/v1/health")

    def health_detailed(self) -> Dict[str, Any]:
        """Verifica saude detalhada da API"""
        return self._http.get("/api/v1/health/detailed")

    def login(self, username: str, password: str) -> str:
        """
        Autentica e retorna token JWT.

        Args:
            username: Nome de usuario
            password: Senha

        Returns:
            Token JWT
        """
        data = self._http.post(
            "/api/v1/auth/login",
            json_data={"username": username, "password": password}
        )
        token = data.get("access_token")
        self._http.token = token
        return token

    def logout(self) -> bool:
        """Encerra sessao"""
        try:
            self._http.post("/api/v1/auth/logout")
        except APIError:
            pass
        self._http.token = None
        return True

    def close(self):
        """Fecha conexao"""
        self._http.close()

    def __enter__(self) -> "FabricaClient":
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


# =============================================================================
# Async Client (opcional)
# =============================================================================

# O cliente async pode ser implementado usando httpx.AsyncClient
# Deixado como extensao futura para manter o SDK simples


# =============================================================================
# Convenience functions
# =============================================================================

def create_client(
    api_key: str = None,
    base_url: str = "http://localhost:9001",
    **kwargs
) -> FabricaClient:
    """
    Funcao de conveniencia para criar cliente.

    Uso:
        from fabrica_client import create_client

        client = create_client(api_key="...")
    """
    return FabricaClient(api_key=api_key, base_url=base_url, **kwargs)

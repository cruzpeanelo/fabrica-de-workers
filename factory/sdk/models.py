# -*- coding: utf-8 -*-
"""
Modelos do SDK - Plataforma E
===================================

Classes de modelo para representar recursos da API.
"""
from dataclasses import dataclass, field
from datetime import datetime
from typing import List, Optional, Dict, Any


def _parse_datetime(value: str) -> Optional[datetime]:
    """Parse string ISO para datetime"""
    if not value:
        return None
    try:
        return datetime.fromisoformat(value.replace("Z", "+00:00"))
    except (ValueError, AttributeError):
        return None


@dataclass
class APIKeyInfo:
    """
    Informacoes de uma API Key.

    Attributes:
        key_id: ID publico da chave
        name: Nome amigavel
        tier: Tier (free, basic, pro, enterprise)
        scopes: Lista de permissoes
        rate_limits: Limites de rate limiting
    """
    key_id: str
    name: str
    tier: str
    scopes: List[str]
    rate_limits: Dict[str, int]
    requests_today: int = 0
    requests_total: int = 0
    last_used_at: Optional[datetime] = None
    expires_at: Optional[datetime] = None
    created_at: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: dict) -> "APIKeyInfo":
        """Cria instancia a partir de dicionario"""
        return cls(
            key_id=data.get("key_id", ""),
            name=data.get("name", ""),
            tier=data.get("tier", "free"),
            scopes=data.get("scopes", []),
            rate_limits=data.get("rate_limits", {}),
            requests_today=data.get("requests_today", 0),
            requests_total=data.get("requests_total", 0),
            last_used_at=_parse_datetime(data.get("last_used_at")),
            expires_at=_parse_datetime(data.get("expires_at")),
            created_at=_parse_datetime(data.get("created_at")),
        )


@dataclass
class Project:
    """
    Representa um projeto na Plataforma E.

    Attributes:
        id: ID unico do projeto (ex: PRJ-ABC123)
        name: Nome do projeto
        description: Descricao
        project_type: Tipo (web-app, api-service, etc)
        status: Status atual (PLANNING, IN_PROGRESS, COMPLETED, etc)
        progress: Progresso em porcentagem (0-100)
        folder_path: Caminho local dos arquivos
        github_url: URL do repositorio GitHub
        created_at: Data de criacao
        updated_at: Data de atualizacao
    """
    id: str
    name: str
    description: Optional[str] = None
    project_type: str = "web-app"
    status: str = "PLANNING"
    progress: float = 0.0
    folder_path: Optional[str] = None
    github_url: Optional[str] = None
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None

    # Referencia ao cliente (para operacoes)
    _client: Any = field(default=None, repr=False)

    @classmethod
    def from_dict(cls, data: dict, client=None) -> "Project":
        """Cria instancia a partir de dicionario da API"""
        return cls(
            id=data.get("project_id", ""),
            name=data.get("name", ""),
            description=data.get("description"),
            project_type=data.get("project_type", "web-app"),
            status=data.get("status", "PLANNING"),
            progress=data.get("progress", 0),
            folder_path=data.get("folder_path"),
            github_url=data.get("github_url"),
            created_at=_parse_datetime(data.get("created_at")),
            updated_at=_parse_datetime(data.get("updated_at")),
            _client=client,
        )

    def to_dict(self) -> dict:
        """Converte para dicionario"""
        return {
            "project_id": self.id,
            "name": self.name,
            "description": self.description,
            "project_type": self.project_type,
            "status": self.status,
            "progress": self.progress,
            "folder_path": self.folder_path,
            "github_url": self.github_url,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
        }

    def list_stories(self, status: str = None) -> List["Story"]:
        """Lista stories deste projeto"""
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        return self._client.stories.list(project_id=self.id, status=status)

    def create_story(self, **kwargs) -> "Story":
        """Cria uma story neste projeto"""
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        return self._client.stories.create(project_id=self.id, **kwargs)

    def refresh(self) -> "Project":
        """Atualiza dados do projeto"""
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        updated = self._client.projects.get(self.id)
        self.__dict__.update(updated.__dict__)
        return self

    def __str__(self):
        return f"Project({self.id}: {self.name} [{self.status}])"


@dataclass
class Story:
    """
    Representa uma User Story.

    Attributes:
        id: ID unico (ex: STR-ABC123)
        project_id: ID do projeto pai
        title: Titulo da story
        persona: "Como um [tipo de usuario]"
        action: "Eu quero [funcionalidade]"
        benefit: "Para que [beneficio]"
        narrative: Narrativa completa formatada
        acceptance_criteria: Lista de criterios de aceite
        status: Status (backlog, ready, in_progress, review, testing, done)
        story_points: Pontos (Fibonacci: 1,2,3,5,8,13,21)
        priority: Prioridade (low, medium, high, urgent)
        progress: Progresso em porcentagem
        tasks_total: Total de tasks
        tasks_completed: Tasks completadas
    """
    id: str
    project_id: str
    title: str
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    narrative: Optional[str] = None
    acceptance_criteria: List[str] = field(default_factory=list)
    status: str = "backlog"
    story_points: int = 0
    priority: str = "medium"
    progress: float = 0.0
    tasks_total: int = 0
    tasks_completed: int = 0
    created_at: Optional[datetime] = None

    _client: Any = field(default=None, repr=False)

    @classmethod
    def from_dict(cls, data: dict, client=None) -> "Story":
        """Cria instancia a partir de dicionario da API"""
        return cls(
            id=data.get("story_id", ""),
            project_id=data.get("project_id", ""),
            title=data.get("title", ""),
            persona=data.get("persona"),
            action=data.get("action"),
            benefit=data.get("benefit"),
            narrative=data.get("narrative"),
            acceptance_criteria=data.get("acceptance_criteria", []),
            status=data.get("status", "backlog"),
            story_points=data.get("story_points", 0),
            priority=data.get("priority", "medium"),
            progress=data.get("progress", 0),
            tasks_total=data.get("tasks_total", 0),
            tasks_completed=data.get("tasks_completed", 0),
            created_at=_parse_datetime(data.get("created_at")),
            _client=client,
        )

    def to_dict(self) -> dict:
        """Converte para dicionario"""
        return {
            "story_id": self.id,
            "project_id": self.project_id,
            "title": self.title,
            "persona": self.persona,
            "action": self.action,
            "benefit": self.benefit,
            "narrative": self.narrative,
            "acceptance_criteria": self.acceptance_criteria,
            "status": self.status,
            "story_points": self.story_points,
            "priority": self.priority,
            "progress": self.progress,
            "tasks_total": self.tasks_total,
            "tasks_completed": self.tasks_completed,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def execute(self) -> "Job":
        """
        Inicia desenvolvimento autonomo desta story.

        Returns:
            Job: Job de desenvolvimento criado
        """
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        return self._client.stories.execute(self.id)

    def refresh(self) -> "Story":
        """Atualiza dados da story"""
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        updated = self._client.stories.get(self.id)
        self.__dict__.update(updated.__dict__)
        return self

    def __str__(self):
        return f"Story({self.id}: {self.title} [{self.status}] {self.story_points}pts)"


@dataclass
class Job:
    """
    Representa um Job de desenvolvimento.

    Attributes:
        id: ID unico (ex: JOB-ABC123)
        description: Descricao do que construir
        status: Status (pending, queued, running, completed, failed, cancelled)
        current_step: Passo atual (queued, generating, testing, etc)
        progress: Progresso em porcentagem
        output_path: Caminho dos arquivos gerados
        files_created: Lista de arquivos criados
        error_message: Mensagem de erro (se falhou)
        created_at: Data de criacao
        completed_at: Data de conclusao
    """
    id: str
    description: str
    status: str = "pending"
    current_step: str = "queued"
    progress: float = 0.0
    output_path: Optional[str] = None
    files_created: List[str] = field(default_factory=list)
    error_message: Optional[str] = None
    created_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None

    _client: Any = field(default=None, repr=False)

    @classmethod
    def from_dict(cls, data: dict, client=None) -> "Job":
        """Cria instancia a partir de dicionario da API"""
        return cls(
            id=data.get("job_id", ""),
            description=data.get("description", ""),
            status=data.get("status", "pending"),
            current_step=data.get("current_step", "queued"),
            progress=data.get("progress", 0),
            output_path=data.get("output_path"),
            files_created=data.get("files_created", []),
            error_message=data.get("error_message"),
            created_at=_parse_datetime(data.get("created_at")),
            completed_at=_parse_datetime(data.get("completed_at")),
            _client=client,
        )

    def to_dict(self) -> dict:
        """Converte para dicionario"""
        return {
            "job_id": self.id,
            "description": self.description,
            "status": self.status,
            "current_step": self.current_step,
            "progress": self.progress,
            "output_path": self.output_path,
            "files_created": self.files_created,
            "error_message": self.error_message,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
        }

    @property
    def is_completed(self) -> bool:
        """Verifica se job foi completado"""
        return self.status == "completed"

    @property
    def is_failed(self) -> bool:
        """Verifica se job falhou"""
        return self.status == "failed"

    @property
    def is_running(self) -> bool:
        """Verifica se job esta em execucao"""
        return self.status in ("pending", "queued", "running")

    def refresh(self) -> "Job":
        """Atualiza dados do job"""
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        updated = self._client.jobs.get(self.id)
        self.__dict__.update(updated.__dict__)
        return self

    def wait_for_completion(
        self,
        timeout: int = 600,
        poll_interval: int = 5,
        callback=None
    ) -> "Job":
        """
        Aguarda conclusao do job.

        Args:
            timeout: Timeout em segundos (default: 600 = 10 min)
            poll_interval: Intervalo entre verificacoes em segundos
            callback: Funcao chamada a cada verificacao com o job atualizado

        Returns:
            Job: Job atualizado apos conclusao

        Raises:
            TimeoutError: Se exceder timeout
            FabricaError: Se job falhar
        """
        import time
        from factory.sdk.exceptions import TimeoutError, FabricaError

        start_time = time.time()

        while True:
            self.refresh()

            if callback:
                callback(self)

            if self.is_completed:
                return self

            if self.is_failed:
                raise FabricaError(
                    f"Job {self.id} falhou: {self.error_message}",
                    error_code="job_failed"
                )

            elapsed = time.time() - start_time
            if elapsed >= timeout:
                raise TimeoutError(
                    f"Timeout aguardando job {self.id} (elapsed: {elapsed:.0f}s)"
                )

            time.sleep(poll_interval)

    def __str__(self):
        return f"Job({self.id}: {self.status} [{self.current_step}] {self.progress:.0f}%)"


@dataclass
class Webhook:
    """
    Representa um Webhook configurado.

    Attributes:
        id: ID unico (ex: WHK-ABC123)
        name: Nome amigavel
        url: URL de destino
        events: Lista de eventos assinados
        status: Status (active, paused, failed)
        deliveries_total: Total de entregas
        deliveries_failed: Entregas falhas
        created_at: Data de criacao
    """
    id: str
    name: str
    url: str
    events: List[str] = field(default_factory=list)
    status: str = "active"
    deliveries_total: int = 0
    deliveries_failed: int = 0
    created_at: Optional[datetime] = None
    secret: Optional[str] = None  # Mostrado apenas na criacao

    _client: Any = field(default=None, repr=False)

    @classmethod
    def from_dict(cls, data: dict, client=None) -> "Webhook":
        """Cria instancia a partir de dicionario da API"""
        return cls(
            id=data.get("webhook_id", ""),
            name=data.get("name", ""),
            url=data.get("url", ""),
            events=data.get("events", []),
            status=data.get("status", "active"),
            deliveries_total=data.get("deliveries_total", 0),
            deliveries_failed=data.get("deliveries_failed", 0),
            created_at=_parse_datetime(data.get("created_at")),
            secret=data.get("secret"),
            _client=client,
        )

    def to_dict(self) -> dict:
        """Converte para dicionario"""
        return {
            "webhook_id": self.id,
            "name": self.name,
            "url": self.url,
            "events": self.events,
            "status": self.status,
            "deliveries_total": self.deliveries_total,
            "deliveries_failed": self.deliveries_failed,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }

    def delete(self) -> bool:
        """Remove este webhook"""
        if not self._client:
            raise RuntimeError("Cliente nao configurado")
        return self._client.webhooks.delete(self.id)

    def __str__(self):
        return f"Webhook({self.id}: {self.name} [{self.status}])"


@dataclass
class PaginatedList:
    """
    Lista paginada de resultados.

    Attributes:
        items: Lista de itens
        total: Total de itens (todas as paginas)
        page: Pagina atual
        per_page: Itens por pagina
        has_next: Se existe proxima pagina
    """
    items: List[Any]
    total: int
    page: int = 1
    per_page: int = 20
    has_next: bool = False

    def __iter__(self):
        return iter(self.items)

    def __len__(self):
        return len(self.items)

    def __getitem__(self, index):
        return self.items[index]

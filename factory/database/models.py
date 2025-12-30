"""
Modelos SQLAlchemy para a Fabrica de Agentes v4.0
Arquitetura Worker-based (Single Claude + Tools per Worker)

Multi-Tenancy v5.0 (Issues #81, #82):
- Todos os modelos principais possuem tenant_id para isolamento de dados
- Indices compostos (tenant_id + campo) para performance
- Soft delete com deleted_at e deleted_by para auditoria
- Relacionamentos corretos entre Tenant e demais entidades
"""
from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean, Float, Index, UniqueConstraint
from sqlalchemy.orm import relationship
from datetime import datetime
from enum import Enum
from typing import Optional, Dict, Any

# Import Base
try:
    from .connection import Base
except ImportError:
    from connection import Base


# =============================================================================
# MIXIN CLASSES PARA MULTI-TENANCY
# =============================================================================

class TenantMixin:
    """
    Mixin para adicionar suporte a multi-tenancy aos modelos.
    Inclui tenant_id e metodos auxiliares para filtragem.
    """
    tenant_id = Column(String(50), nullable=True, index=True)

    @classmethod
    def tenant_filter(cls, query, tenant_id: str):
        """Filtra query pelo tenant_id"""
        return query.filter(cls.tenant_id == tenant_id)


class SoftDeleteMixin:
    """
    Mixin para soft delete.
    Registros nao sao deletados fisicamente, apenas marcados.
    """
    is_deleted = Column(Boolean, default=False, index=True)
    deleted_at = Column(DateTime, nullable=True)
    deleted_by = Column(String(100), nullable=True)

    def soft_delete(self, deleted_by: str = "system"):
        """Marca o registro como deletado"""
        self.is_deleted = True
        self.deleted_at = datetime.utcnow()
        self.deleted_by = deleted_by

    def restore(self):
        """Restaura um registro deletado"""
        self.is_deleted = False
        self.deleted_at = None
        self.deleted_by = None

    @classmethod
    def active_filter(cls, query):
        """Filtra apenas registros ativos (nao deletados)"""
        return query.filter(cls.is_deleted == False)


# =============================================================================
# ENUMS
# =============================================================================

class ProjectStatus(str, Enum):
    PLANNING = "PLANNING"
    IN_PROGRESS = "IN_PROGRESS"
    PAUSED = "PAUSED"
    COMPLETED = "COMPLETED"
    ARCHIVED = "ARCHIVED"


class JobStatus(str, Enum):
    """Status do Job"""
    PENDING = "pending"
    QUEUED = "queued"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class JobStep(str, Enum):
    """Passos do Autonomous Loop"""
    QUEUED = "queued"
    PARSING = "parsing"
    GENERATING = "generating"
    LINTING = "linting"
    TYPE_CHECKING = "type_checking"
    TESTING = "testing"
    SECURITY_SCAN = "security_scan"
    COMMITTING = "committing"
    COMPLETED = "completed"
    FAILED = "failed"


class WorkerStatus(str, Enum):
    """Status do Worker"""
    IDLE = "idle"
    BUSY = "busy"
    OFFLINE = "offline"


# =============================================================================
# PROJECT - Projetos construidos pela fabrica
# =============================================================================

class Project(Base):
    """
    Modelo para Projetos - cada aplicacao construida pela fabrica.

    Multi-Tenancy (Issue #81):
    - tenant_id para isolamento de dados
    - Indices compostos para queries eficientes
    - Soft delete para auditoria
    - Relacionamento com Tenant (cascade delete)
    """
    __tablename__ = "projects"

    id = Column(Integer, primary_key=True, autoincrement=True)
    project_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Associacao com Tenant (Issue #81)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    name = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)

    # Tipo e template
    project_type = Column(String(50), nullable=False)
    template_used = Column(String(100), nullable=True)

    # Status e progresso
    status = Column(String(50), default=ProjectStatus.PLANNING.value)
    progress = Column(Float, default=0.0)

    # Diretorio do projeto
    folder_path = Column(String(500), nullable=True)
    github_url = Column(String(500), nullable=True)

    # Configuracoes do projeto
    config = Column(JSON, default=dict)
    settings = Column(JSON, default=dict)
    tags = Column(JSON, default=list)

    # Metadados
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    completed_at = Column(DateTime, nullable=True)
    created_by = Column(String(100), default="system")

    # Soft Delete (Issue #81)
    is_deleted = Column(Boolean, default=False, index=True)
    deleted_at = Column(DateTime, nullable=True)
    deleted_by = Column(String(100), nullable=True)

    # Indices compostos para multi-tenancy (Issue #81)
    __table_args__ = (
        Index('ix_projects_tenant_status', 'tenant_id', 'status'),
        Index('ix_projects_tenant_created', 'tenant_id', 'created_at'),
        Index('ix_projects_tenant_deleted', 'tenant_id', 'is_deleted'),
    )

    # Relacionamentos
    tenant = relationship("Tenant", back_populates="projects")
    jobs = relationship("Job", back_populates="project", cascade="all, delete-orphan")
    tasks = relationship("Task", back_populates="project", cascade="all, delete-orphan")

    def soft_delete(self, deleted_by: str = "system"):
        """Marca o projeto como deletado (soft delete)"""
        self.is_deleted = True
        self.deleted_at = datetime.utcnow()
        self.deleted_by = deleted_by

    def restore(self):
        """Restaura um projeto deletado"""
        self.is_deleted = False
        self.deleted_at = None
        self.deleted_by = None

    def to_dict(self):
        return {
            "project_id": self.project_id,
            "tenant_id": self.tenant_id,
            "name": self.name,
            "description": self.description,
            "project_type": self.project_type,
            "template_used": self.template_used,
            "status": self.status,
            "progress": self.progress,
            "folder_path": self.folder_path,
            "github_url": self.github_url,
            "config": self.config or {},
            "settings": self.settings or {},
            "tags": self.tags or [],
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "is_deleted": self.is_deleted,
            "jobs_count": len(self.jobs) if self.jobs else 0,
            "tasks_count": len(self.tasks) if self.tasks else 0
        }

    def __repr__(self):
        return f"<Project {self.project_id}: {self.name} [{self.status}]>"


# =============================================================================
# JOB - Jobs Assincronos (Unidade de trabalho principal)
# =============================================================================

class Job(Base):
    """
    Modelo para Jobs - Unidade de trabalho principal
    Processado por workers usando Claude Agent SDK

    Multi-Tenancy (Issue #81):
    - tenant_id herdado do projeto ou definido diretamente
    - Indices compostos para queries por tenant
    - Soft delete para auditoria
    """
    __tablename__ = "jobs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Associacao com Tenant (Issue #81)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamento com projeto (opcional)
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=True, index=True)
    project = relationship("Project", back_populates="jobs")

    # Input do usuario
    description = Column(Text, nullable=False)
    tech_stack = Column(String(200), nullable=True)
    features = Column(JSON, default=list)

    # Status e progresso
    status = Column(String(20), default=JobStatus.PENDING.value, index=True)
    current_step = Column(String(30), default=JobStep.QUEUED.value)
    progress = Column(Float, default=0.0)

    # Controle do loop autonomo
    current_attempt = Column(Integer, default=0)
    max_attempts = Column(Integer, default=5)
    total_iterations = Column(Integer, default=0)

    # Worker que esta processando
    worker_id = Column(String(50), nullable=True, index=True)

    # Resultado
    result = Column(JSON, default=dict)
    output_path = Column(String(500), nullable=True)
    github_url = Column(String(500), nullable=True)
    artifacts = Column(JSON, default=list)

    # Erros e logs
    error_message = Column(Text, nullable=True)
    step_logs = Column(JSON, default=list)

    # Session (para Redis cache)
    session_data = Column(JSON, default=dict)

    # Timestamps
    queued_at = Column(DateTime, default=datetime.utcnow)
    started_at = Column(DateTime, nullable=True)
    completed_at = Column(DateTime, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Criado por
    created_by = Column(String(100), nullable=True)

    # Soft Delete (Issue #81)
    is_deleted = Column(Boolean, default=False, index=True)
    deleted_at = Column(DateTime, nullable=True)
    deleted_by = Column(String(100), nullable=True)

    # Indices compostos para multi-tenancy (Issue #81)
    __table_args__ = (
        Index('ix_jobs_tenant_status', 'tenant_id', 'status'),
        Index('ix_jobs_tenant_created', 'tenant_id', 'created_at'),
        Index('ix_jobs_tenant_worker', 'tenant_id', 'worker_id'),
    )

    # Relacionamentos
    tenant = relationship("Tenant", back_populates="jobs")
    failures = relationship("FailureHistory", back_populates="job", cascade="all, delete-orphan")

    def soft_delete(self, deleted_by: str = "system"):
        """Marca o job como deletado (soft delete)"""
        self.is_deleted = True
        self.deleted_at = datetime.utcnow()
        self.deleted_by = deleted_by

    def restore(self):
        """Restaura um job deletado"""
        self.is_deleted = False
        self.deleted_at = None
        self.deleted_by = None

    def to_dict(self):
        return {
            "job_id": self.job_id,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "description": self.description,
            "tech_stack": self.tech_stack,
            "features": self.features or [],
            "status": self.status,
            "current_step": self.current_step,
            "progress": self.progress,
            "current_attempt": self.current_attempt,
            "max_attempts": self.max_attempts,
            "total_iterations": self.total_iterations,
            "worker_id": self.worker_id,
            "result": self.result or {},
            "output_path": self.output_path,
            "github_url": self.github_url,
            "artifacts": self.artifacts or [],
            "error_message": self.error_message,
            "step_logs": self.step_logs or [],
            "queued_at": self.queued_at.isoformat() if self.queued_at else None,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "created_by": self.created_by,
            "is_deleted": self.is_deleted
        }

    def __repr__(self):
        return f"<Job {self.job_id}: {self.status} [{self.current_step}]>"


# =============================================================================
# WORKER - Workers do Pool (Claude Agent SDK instances)
# =============================================================================

class Worker(Base):
    """
    Modelo para Workers - Instancias do Claude Agent SDK
    Cada worker processa jobs da fila Redis
    """
    __tablename__ = "workers"

    id = Column(Integer, primary_key=True, autoincrement=True)
    worker_id = Column(String(50), unique=True, nullable=False, index=True)

    # Status
    status = Column(String(20), default=WorkerStatus.IDLE.value, index=True)
    current_job_id = Column(String(50), nullable=True)

    # Configuracao
    model = Column(String(50), default="claude-sonnet-4-20250514")
    mcp_tools = Column(JSON, default=list)

    # Metricas
    jobs_completed = Column(Integer, default=0)
    jobs_failed = Column(Integer, default=0)
    total_processing_time = Column(Integer, default=0)
    avg_job_duration = Column(Float, default=0.0)

    # Heartbeat
    last_heartbeat = Column(DateTime, nullable=True)
    started_at = Column(DateTime, default=datetime.utcnow)

    # Metadados
    hostname = Column(String(100), nullable=True)
    ip_address = Column(String(50), nullable=True)

    # Indices para performance em queries de workers
    __table_args__ = (
        Index('ix_workers_status_heartbeat', 'status', 'last_heartbeat'),
        Index('ix_workers_current_job', 'current_job_id'),
    )

    def to_dict(self):
        return {
            "worker_id": self.worker_id,
            "status": self.status,
            "current_job_id": self.current_job_id,
            "model": self.model,
            "mcp_tools": self.mcp_tools or [],
            "jobs_completed": self.jobs_completed,
            "jobs_failed": self.jobs_failed,
            "total_processing_time": self.total_processing_time,
            "avg_job_duration": self.avg_job_duration,
            "last_heartbeat": self.last_heartbeat.isoformat() if self.last_heartbeat else None,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "hostname": self.hostname,
            "ip_address": self.ip_address
        }

    def __repr__(self):
        return f"<Worker {self.worker_id}: {self.status}>"


# =============================================================================
# FAILURE_HISTORY - Historico de Falhas
# =============================================================================

class FailureHistory(Base):
    """
    Modelo para Historico de Falhas
    Previne retry loops infinitos e permite analise de erros
    """
    __tablename__ = "failure_history"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamentos
    job_id = Column(String(50), ForeignKey("jobs.job_id"), nullable=False, index=True)
    job = relationship("Job", back_populates="failures")
    project_id = Column(String(50), nullable=True, index=True)

    # Detalhes da falha
    step = Column(String(30), nullable=False)
    attempt = Column(Integer, nullable=False)
    error_type = Column(String(100), nullable=True)
    error_message = Column(Text, nullable=False)
    stack_trace = Column(Text, nullable=True)

    # Contexto da falha
    input_data = Column(JSON, default=dict)
    step_output = Column(JSON, default=dict)

    # Resolucao
    resolved = Column(Boolean, default=False)
    resolution_notes = Column(Text, nullable=True)
    resolved_at = Column(DateTime, nullable=True)

    # Timestamp
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    # Indices para analise de falhas
    __table_args__ = (
        Index('ix_failure_history_job_step', 'job_id', 'step'),
        Index('ix_failure_history_error_type', 'error_type'),
        Index('ix_failure_history_resolved', 'resolved', 'created_at'),
    )

    def to_dict(self):
        return {
            "id": self.id,
            "job_id": self.job_id,
            "project_id": self.project_id,
            "step": self.step,
            "attempt": self.attempt,
            "error_type": self.error_type,
            "error_message": self.error_message,
            "stack_trace": self.stack_trace,
            "input_data": self.input_data or {},
            "step_output": self.step_output or {},
            "resolved": self.resolved,
            "resolution_notes": self.resolution_notes,
            "resolved_at": self.resolved_at.isoformat() if self.resolved_at else None,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<FailureHistory {self.job_id}:{self.step} attempt={self.attempt}>"


# =============================================================================
# USER - Usuarios do Sistema
# =============================================================================

class User(Base):
    """Modelo para Usuarios do Sistema"""
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, autoincrement=True)
    username = Column(String(100), unique=True, nullable=False, index=True)
    password_hash = Column(String(255), nullable=False)
    email = Column(String(255), nullable=True)
    role = Column(String(20), nullable=False, default="VIEWER")
    active = Column(Boolean, default=True, nullable=False)

    # Quotas
    quotas = Column(JSON, default=lambda: {
        "max_jobs_per_day": 10,
        "max_concurrent_jobs": 2,
        "max_projects": 20,
        "api_tier": "free"
    })

    # Billing
    billing = Column(JSON, default=lambda: {
        "plan": "free",
        "tokens_used": 0,
        "tokens_limit": 100000,
        "cost_accumulated": 0.0,
        "budget_limit": 50.0
    })

    # Rate Limiting
    rate_limit_tokens = Column(Integer, default=0)
    rate_limit_reset = Column(DateTime, nullable=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow, nullable=False)
    last_login = Column(DateTime, nullable=True)

    # Indices para busca e autenticacao de usuarios
    __table_args__ = (
        Index('ix_users_email', 'email'),
        Index('ix_users_role_active', 'role', 'active'),
        Index('ix_users_last_login', 'last_login'),
    )

    def to_dict(self):
        return {
            "id": self.id,
            "username": self.username,
            "email": self.email,
            "role": self.role,
            "active": self.active,
            "quotas": self.quotas or {},
            "billing": self.billing or {},
            "rate_limit_tokens": self.rate_limit_tokens,
            "rate_limit_reset": self.rate_limit_reset.isoformat() if self.rate_limit_reset else None,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "last_login": self.last_login.isoformat() if self.last_login else None
        }

    def check_quota(self, quota_name: str) -> bool:
        """Verifica se usuario tem quota disponivel"""
        if not self.quotas:
            return True
        return self.quotas.get(quota_name, 0) > 0

    def __repr__(self):
        return f"<User {self.username} [{self.role}]>"


# =============================================================================
# ACTIVITY_LOG - Logs de Atividades
# =============================================================================

class ActivityLog(Base):
    """
    Modelo para Logs de Atividades

    Multi-Tenancy (Issue #188):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "activity_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Multi-Tenant: Isolamento de dados (Issue #188)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Origem
    source = Column(String(50), nullable=False, index=True)
    source_id = Column(String(50), nullable=True)

    # Contexto
    project_id = Column(String(50), nullable=True, index=True)
    job_id = Column(String(50), nullable=True, index=True)
    worker_id = Column(String(50), nullable=True, index=True)

    # Tipo e nivel
    level = Column(String(20), default="INFO", index=True)
    event_type = Column(String(50), nullable=False, index=True)

    # Dados
    message = Column(Text, nullable=False)
    details = Column(JSON, default=dict)

    # Timestamp
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)

    # Indices compostos para queries de logs
    __table_args__ = (
        Index('ix_activity_logs_project_timestamp', 'project_id', 'timestamp'),
        Index('ix_activity_logs_level_timestamp', 'level', 'timestamp'),
        Index('ix_activity_logs_source_event', 'source', 'event_type'),
        Index('ix_activity_logs_job_timestamp', 'job_id', 'timestamp'),
    )

    def to_dict(self):
        return {
            "id": self.id,
            "tenant_id": self.tenant_id,
            "source": self.source,
            "source_id": self.source_id,
            "project_id": self.project_id,
            "job_id": self.job_id,
            "worker_id": self.worker_id,
            "level": self.level,
            "event_type": self.event_type,
            "message": self.message,
            "details": self.details or {},
            "timestamp": self.timestamp.isoformat() if self.timestamp else None
        }

    def __repr__(self):
        return f"<ActivityLog [{self.level}] {self.source}: {self.message[:50]}...>"


# =============================================================================
# TASK - Tarefas Kanban (Gestao de Projetos estilo Monday/Jira)
# =============================================================================

class TaskStatus(str, Enum):
    """Status da tarefa no Kanban"""
    BACKLOG = "backlog"
    TODO = "todo"
    IN_DEVELOPMENT = "in_development"
    IN_TESTING = "in_testing"
    READY_TO_DEPLOY = "ready_to_deploy"
    DONE = "done"


class TaskPriority(str, Enum):
    """Prioridade da tarefa"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    URGENT = "urgent"


class Task(Base):
    """
    Modelo para Tarefas Kanban
    Permite gestao de projetos estilo Monday/Jira

    Multi-Tenancy (Issue #81):
    - tenant_id para isolamento de dados
    - Indices compostos para queries eficientes
    - Soft delete para auditoria
    """
    __tablename__ = "tasks"

    id = Column(Integer, primary_key=True, autoincrement=True)
    task_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Associacao com Tenant (Issue #81)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)
    project = relationship("Project", back_populates="tasks")

    # Dados principais
    title = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)

    # Kanban
    status = Column(String(30), default=TaskStatus.BACKLOG.value, index=True)
    priority = Column(String(20), default=TaskPriority.MEDIUM.value)
    kanban_order = Column(Integer, default=0)

    # Atribuicao
    assignee = Column(String(100), nullable=True)

    # Tags e metadados
    tags = Column(JSON, default=list)
    extra_data = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    due_date = Column(DateTime, nullable=True)
    started_at = Column(DateTime, nullable=True)
    completed_at = Column(DateTime, nullable=True)

    # Criado por
    created_by = Column(String(100), default="system")

    # Soft Delete (Issue #81)
    is_deleted = Column(Boolean, default=False, index=True)
    deleted_at = Column(DateTime, nullable=True)
    deleted_by = Column(String(100), nullable=True)

    # Indices compostos para multi-tenancy (Issue #81)
    __table_args__ = (
        Index('ix_tasks_tenant_status', 'tenant_id', 'status'),
        Index('ix_tasks_tenant_project', 'tenant_id', 'project_id'),
        Index('ix_tasks_tenant_assignee', 'tenant_id', 'assignee'),
    )

    # Relacionamentos
    tenant = relationship("Tenant", back_populates="tasks")

    def soft_delete(self, deleted_by: str = "system"):
        """Marca a task como deletada (soft delete)"""
        self.is_deleted = True
        self.deleted_at = datetime.utcnow()
        self.deleted_by = deleted_by

    def restore(self):
        """Restaura uma task deletada"""
        self.is_deleted = False
        self.deleted_at = None
        self.deleted_by = None

    def to_dict(self):
        return {
            "task_id": self.task_id,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "title": self.title,
            "description": self.description,
            "status": self.status,
            "priority": self.priority,
            "kanban_order": self.kanban_order,
            "assignee": self.assignee,
            "tags": self.tags or [],
            "extra_data": self.extra_data or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "due_date": self.due_date.isoformat() if self.due_date else None,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "created_by": self.created_by,
            "is_deleted": self.is_deleted
        }

    def __repr__(self):
        return f"<Task {self.task_id}: {self.title[:30]} [{self.status}]>"


# =============================================================================
# STORY - User Stories Agile com narrativa completa
# =============================================================================

class StoryStatus(str, Enum):
    """Status da Story no Kanban"""
    BACKLOG = "backlog"
    READY = "ready"
    IN_PROGRESS = "in_progress"
    REVIEW = "review"
    TESTING = "testing"
    DONE = "done"


class StoryCategory(str, Enum):
    """Categoria da Story"""
    FEATURE = "feature"
    BUG = "bug"
    TECH_DEBT = "tech_debt"
    SPIKE = "spike"
    IMPROVEMENT = "improvement"


class StoryComplexity(str, Enum):
    """Complexidade da Story"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    VERY_HIGH = "very_high"


class Story(Base):
    """
    Modelo para User Stories Agile
    Com narrativa completa: Como um [persona], Eu quero [action], Para que [benefit]

    Multi-Tenancy (Issue #81):
    - tenant_id para isolamento de dados
    - Indices compostos para queries eficientes
    - Soft delete para auditoria
    """
    __tablename__ = "stories"

    id = Column(Integer, primary_key=True, autoincrement=True)
    story_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Associacao com Tenant (Issue #81)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)
    project = relationship("Project", backref="stories")

    # Dados principais
    title = Column(String(300), nullable=False)
    description = Column(Text, nullable=True)

    # Narrativa Agile (As a... I want... So that...)
    persona = Column(String(200), nullable=True)  # "Como um [usuario]"
    action = Column(Text, nullable=True)           # "Eu quero [funcionalidade]"
    benefit = Column(Text, nullable=True)          # "Para que [beneficio]"

    # Criterios de Aceite e DoD
    acceptance_criteria = Column(JSON, default=list)  # Lista de criterios
    definition_of_done = Column(JSON, default=list)   # Lista de DoD

    # Regras de Negocio e Notas Tecnicas
    business_rules = Column(JSON, default=list)
    technical_notes = Column(Text, nullable=True)

    # Organizacao
    epic_id = Column(String(50), nullable=True, index=True)
    sprint_id = Column(String(50), nullable=True, index=True)
    category = Column(String(30), default=StoryCategory.FEATURE.value)

    # Estimativa
    story_points = Column(Integer, default=0)  # Fibonacci: 1,2,3,5,8,13,21
    complexity = Column(String(20), default=StoryComplexity.MEDIUM.value)
    estimated_hours = Column(Float, default=0.0)

    # Kanban
    status = Column(String(30), default=StoryStatus.BACKLOG.value, index=True)
    priority = Column(String(20), default=TaskPriority.MEDIUM.value)
    kanban_order = Column(Integer, default=0)

    # Atribuicao
    assignee = Column(String(100), nullable=True)
    reporter = Column(String(100), nullable=True)

    # Tags e dependencias
    tags = Column(JSON, default=list)
    dependencies = Column(JSON, default=list)  # IDs de stories dependentes
    blocked_by = Column(JSON, default=list)    # IDs de stories bloqueadoras

    # Metricas
    progress = Column(Float, default=0.0)  # 0-100, calculado das tasks
    tasks_total = Column(Integer, default=0)
    tasks_completed = Column(Integer, default=0)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    due_date = Column(DateTime, nullable=True)
    started_at = Column(DateTime, nullable=True)
    completed_at = Column(DateTime, nullable=True)

    # Criado por
    created_by = Column(String(100), default="system")

    # Soft Delete (Issue #81)
    is_deleted = Column(Boolean, default=False, index=True)
    deleted_at = Column(DateTime, nullable=True)
    deleted_by = Column(String(100), nullable=True)

    # Indices compostos para multi-tenancy (Issue #81)
    __table_args__ = (
        Index('ix_stories_tenant_status', 'tenant_id', 'status'),
        Index('ix_stories_tenant_project', 'tenant_id', 'project_id'),
        Index('ix_stories_tenant_sprint', 'tenant_id', 'sprint_id'),
        Index('ix_stories_tenant_epic', 'tenant_id', 'epic_id'),
        Index('ix_stories_tenant_assignee', 'tenant_id', 'assignee'),
    )

    # Relacionamentos
    tenant = relationship("Tenant", back_populates="stories")
    story_tasks = relationship("StoryTask", back_populates="story", cascade="all, delete-orphan")
    documentation = relationship("StoryDocumentation", back_populates="story", cascade="all, delete-orphan")
    designs = relationship("StoryDesign", back_populates="story", cascade="all, delete-orphan")
    attachments = relationship("Attachment", back_populates="story", cascade="all, delete-orphan",
                               foreign_keys="Attachment.story_id")

    def soft_delete(self, deleted_by: str = "system"):
        """Marca a story como deletada (soft delete)"""
        self.is_deleted = True
        self.deleted_at = datetime.utcnow()
        self.deleted_by = deleted_by

    def restore(self):
        """Restaura uma story deletada"""
        self.is_deleted = False
        self.deleted_at = None
        self.deleted_by = None

    def to_dict(self):
        return {
            "story_id": self.story_id,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "title": self.title,
            "description": self.description,
            "persona": self.persona,
            "action": self.action,
            "benefit": self.benefit,
            "narrative": f"Como um {self.persona or '[persona]'}, eu quero {self.action or '[acao]'}, para que {self.benefit or '[beneficio]'}",
            "acceptance_criteria": self.acceptance_criteria or [],
            "definition_of_done": self.definition_of_done or [],
            "business_rules": self.business_rules or [],
            "technical_notes": self.technical_notes,
            "epic_id": self.epic_id,
            "sprint_id": self.sprint_id,
            "category": self.category,
            "story_points": self.story_points,
            "complexity": self.complexity,
            "estimated_hours": self.estimated_hours,
            "status": self.status,
            "priority": self.priority,
            "kanban_order": self.kanban_order,
            "assignee": self.assignee,
            "reporter": self.reporter,
            "tags": self.tags or [],
            "dependencies": self.dependencies or [],
            "blocked_by": self.blocked_by or [],
            "progress": self.progress,
            "tasks_total": self.tasks_total,
            "tasks_completed": self.tasks_completed,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "due_date": self.due_date.isoformat() if self.due_date else None,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "created_by": self.created_by,
            "is_deleted": self.is_deleted
        }

    def update_progress(self):
        """Atualiza progresso baseado nas tasks"""
        if self.tasks_total > 0:
            self.progress = (self.tasks_completed / self.tasks_total) * 100
        else:
            self.progress = 0

    def __repr__(self):
        return f"<Story {self.story_id}: {self.title[:30]} [{self.status}] {self.story_points}pts>"


# =============================================================================
# STORY_ESTIMATE - Historico de Estimativas de Story Points
# =============================================================================

class StoryEstimate(Base):
    """
    Modelo para Historico de Estimativas de Story Points
    Armazena estimativas feitas por IA e humanos para comparacao
    """
    __tablename__ = "story_estimates"

    id = Column(Integer, primary_key=True, autoincrement=True)
    estimate_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com story
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=False, index=True)

    # Estimativa
    estimated_points = Column(Integer, nullable=False)  # Pontos estimados (1,2,3,5,8,13,21)
    confidence = Column(Float, default=0.8)  # Nivel de confianca (0-1)
    complexity_detected = Column(String(20), nullable=True)  # low, medium, high, very_high

    # Justificativa da IA
    justification = Column(Text, nullable=True)
    factors = Column(JSON, default=list)  # Lista de fatores considerados

    # Comparacao com stories similares
    similar_stories = Column(JSON, default=list)  # [{story_id, title, points, similarity}]

    # Tipo de estimativa
    estimate_type = Column(String(20), default="ai")  # ai, human, adjusted

    # Se foi aceita pelo usuario
    accepted = Column(Boolean, default=False)
    accepted_at = Column(DateTime, nullable=True)
    adjusted_to = Column(Integer, nullable=True)  # Se o usuario ajustou para outro valor

    # Quem fez a estimativa
    estimated_by = Column(String(100), default="claude-ai")

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "estimate_id": self.estimate_id,
            "story_id": self.story_id,
            "estimated_points": self.estimated_points,
            "confidence": self.confidence,
            "complexity_detected": self.complexity_detected,
            "justification": self.justification,
            "factors": self.factors or [],
            "similar_stories": self.similar_stories or [],
            "estimate_type": self.estimate_type,
            "accepted": self.accepted,
            "accepted_at": self.accepted_at.isoformat() if self.accepted_at else None,
            "adjusted_to": self.adjusted_to,
            "estimated_by": self.estimated_by,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<StoryEstimate {self.estimate_id}: {self.estimated_points}pts [{self.estimate_type}]>"


# =============================================================================
# STORY_TASK - Subtarefas de uma Story
# =============================================================================

class StoryTaskType(str, Enum):
    """Tipo de tarefa"""
    DEVELOPMENT = "development"
    REVIEW = "review"
    TEST = "test"
    DOCUMENTATION = "documentation"
    DESIGN = "design"
    RESEARCH = "research"


class StoryTaskStatus(str, Enum):
    """Status da tarefa"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"
    CANCELLED = "cancelled"


class StoryTask(Base):
    """
    Modelo para Tarefas de uma Story
    Subtarefas que compoem o trabalho da Story

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "story_tasks"

    id = Column(Integer, primary_key=True, autoincrement=True)
    task_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamento com story
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=False, index=True)
    story = relationship("Story", back_populates="story_tasks")

    # Dados principais
    title = Column(String(200), nullable=False)
    description = Column(Text, nullable=True)

    # Tipo e status
    task_type = Column(String(30), default=StoryTaskType.DEVELOPMENT.value)
    status = Column(String(30), default=StoryTaskStatus.PENDING.value, index=True)

    # Atribuicao
    agent_id = Column(String(20), nullable=True)  # Ex: AGT-08
    assignee = Column(String(100), nullable=True)

    # Progresso
    progress = Column(Integer, default=0)  # 0-100

    # Estimativa
    estimated_hours = Column(Float, default=0.0)
    actual_hours = Column(Float, default=0.0)

    # Output Tecnico (o que foi feito)
    files_created = Column(JSON, default=list)   # Lista de arquivos criados
    files_modified = Column(JSON, default=list)  # Lista de arquivos modificados
    code_output = Column(Text, nullable=True)    # Codigo gerado (resumo)
    test_results = Column(JSON, default=dict)    # Resultados de testes

    # Criterios de aceite especificos da task
    acceptance_criteria = Column(JSON, default=list)
    checklist = Column(JSON, default=list)  # Checklist de subtarefas

    # Code Review IA
    review_result = Column(JSON, default=dict)  # {score, summary, issues, suggestions, positives, reviewed_at}

    # Ordem
    task_order = Column(Integer, default=0)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    started_at = Column(DateTime, nullable=True)
    completed_at = Column(DateTime, nullable=True)

    # Relacionamentos
    documentation = relationship("StoryDocumentation", back_populates="task", cascade="all, delete-orphan",
                                 foreign_keys="StoryDocumentation.task_id")

    def to_dict(self):
        return {
            "task_id": self.task_id,
            "tenant_id": self.tenant_id,
            "story_id": self.story_id,
            "title": self.title,
            "description": self.description,
            "task_type": self.task_type,
            "status": self.status,
            "agent_id": self.agent_id,
            "assignee": self.assignee,
            "progress": self.progress,
            "estimated_hours": self.estimated_hours,
            "actual_hours": self.actual_hours,
            "files_created": self.files_created or [],
            "files_modified": self.files_modified or [],
            "code_output": self.code_output,
            "test_results": self.test_results or {},
            "review_result": self.review_result or {},
            "acceptance_criteria": self.acceptance_criteria or [],
            "checklist": self.checklist or [],
            "task_order": self.task_order,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None
        }

    def __repr__(self):
        return f"<StoryTask {self.task_id}: {self.title[:30]} [{self.status}]>"


# =============================================================================
# STORY_DOCUMENTATION - Documentacao Tecnica
# =============================================================================

class DocType(str, Enum):
    """Tipo de documentacao"""
    TECHNICAL = "technical"
    USER = "user"
    TEST = "test"
    DEPLOYMENT = "deployment"
    API = "api"


class StoryDocumentation(Base):
    """
    Modelo para Documentacao de Stories/Tasks
    Armazena o que foi feito, como testar, documentacao para usuario

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "story_documentation"

    id = Column(Integer, primary_key=True, autoincrement=True)
    doc_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamentos
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=False, index=True)
    story = relationship("Story", back_populates="documentation")

    task_id = Column(String(50), ForeignKey("story_tasks.task_id"), nullable=True, index=True)
    task = relationship("StoryTask", back_populates="documentation", foreign_keys=[task_id])

    # Tipo e titulo
    doc_type = Column(String(30), default=DocType.TECHNICAL.value)
    title = Column(String(300), nullable=False)

    # Conteudo (Markdown)
    content = Column(Text, nullable=True)

    # Arquivos relacionados
    files_created = Column(JSON, default=list)
    files_modified = Column(JSON, default=list)

    # Instrucoes de teste
    test_instructions = Column(Text, nullable=True)
    test_cases = Column(JSON, default=list)  # Lista de casos de teste

    # Instrucoes de deploy
    deploy_instructions = Column(Text, nullable=True)

    # Versao
    version = Column(String(20), default="1.0")

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Autor
    created_by = Column(String(100), default="system")

    def to_dict(self):
        return {
            "doc_id": self.doc_id,
            "tenant_id": self.tenant_id,
            "story_id": self.story_id,
            "task_id": self.task_id,
            "doc_type": self.doc_type,
            "title": self.title,
            "content": self.content,
            "files_created": self.files_created or [],
            "files_modified": self.files_modified or [],
            "test_instructions": self.test_instructions,
            "test_cases": self.test_cases or [],
            "deploy_instructions": self.deploy_instructions,
            "version": self.version,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<StoryDocumentation {self.doc_id}: {self.title[:30]} [{self.doc_type}]>"


# =============================================================================
# STORY_DESIGN - Mockups, Wireframes, Diagramas (Draw.io)
# =============================================================================

class DesignType(str, Enum):
    """Tipos de design/diagrama"""
    WIREFRAME = "wireframe"
    ARCHITECTURE = "architecture"
    FLOW = "flow"
    DATABASE = "database"
    UI_MOCKUP = "ui_mockup"
    SEQUENCE = "sequence"
    OTHER = "other"


class StoryDesign(Base):
    """
    Modelo para Designs de Stories
    Armazena mockups, wireframes, diagramas de arquitetura (Draw.io)

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "story_designs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    design_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamentos
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=True, index=True)
    story = relationship("Story", back_populates="designs")

    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=True, index=True)

    # Tipo e titulo
    design_type = Column(String(30), default=DesignType.WIREFRAME.value)
    title = Column(String(300), nullable=False)
    description = Column(Text, nullable=True)

    # Conteudo do Draw.io (XML)
    content = Column(Text, nullable=True)

    # Thumbnail (Base64 PNG)
    thumbnail = Column(Text, nullable=True)

    # Arquivo exportado
    file_path = Column(String(500), nullable=True)
    file_format = Column(String(10), default="drawio")  # drawio, png, svg, pdf

    # Metadados
    tags = Column(JSON, default=list)
    version = Column(String(20), default="1.0")

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Autor
    created_by = Column(String(100), default="system")

    def to_dict(self):
        return {
            "design_id": self.design_id,
            "tenant_id": self.tenant_id,
            "story_id": self.story_id,
            "project_id": self.project_id,
            "design_type": self.design_type,
            "title": self.title,
            "description": self.description,
            "content": self.content,
            "thumbnail": self.thumbnail,
            "file_path": self.file_path,
            "file_format": self.file_format,
            "tags": self.tags or [],
            "version": self.version,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<StoryDesign {self.design_id}: {self.title[:30]} [{self.design_type}]>"


# =============================================================================
# CHAT_MESSAGE - Mensagens do Assistente
# =============================================================================

class MessageRole(str, Enum):
    """Role da mensagem"""
    USER = "user"
    ASSISTANT = "assistant"
    SYSTEM = "system"


class ChatMessage(Base):
    """
    Modelo para Mensagens do Chat do Assistente
    Permite interacao com IA para editar stories, tirar duvidas, etc.

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "chat_messages"

    id = Column(Integer, primary_key=True, autoincrement=True)
    message_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Contexto (opcional)
    project_id = Column(String(50), nullable=True, index=True)
    story_id = Column(String(50), nullable=True, index=True)

    # Role e conteudo
    role = Column(String(20), default=MessageRole.USER.value)
    content = Column(Text, nullable=False)

    # Anexos
    attachments = Column(JSON, default=list)  # Lista de attachment_ids

    # Acoes executadas pelo assistente
    actions = Column(JSON, default=list)  # Ex: [{"type": "edit_story", "story_id": "..."}]

    # Usuario
    user_id = Column(String(100), nullable=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "message_id": self.message_id,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "story_id": self.story_id,
            "role": self.role,
            "content": self.content,
            "attachments": self.attachments or [],
            "actions": self.actions or [],
            "user_id": self.user_id,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<ChatMessage {self.message_id}: [{self.role}] {self.content[:30]}...>"


# =============================================================================
# ATTACHMENT - Arquivos Anexados
# =============================================================================

class Attachment(Base):
    """
    Modelo para Arquivos Anexados
    Pode ser anexado a stories, tasks ou mensagens de chat

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "attachments"

    id = Column(Integer, primary_key=True, autoincrement=True)
    attachment_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamentos (um dos tres)
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=True, index=True)
    story = relationship("Story", back_populates="attachments", foreign_keys=[story_id])

    task_id = Column(String(50), ForeignKey("story_tasks.task_id"), nullable=True, index=True)

    chat_message_id = Column(String(50), nullable=True, index=True)

    # Dados do arquivo
    filename = Column(String(300), nullable=False)
    original_filename = Column(String(300), nullable=False)
    file_path = Column(String(500), nullable=False)
    file_size = Column(Integer, default=0)  # bytes
    mime_type = Column(String(100), nullable=True)

    # Descricao
    description = Column(Text, nullable=True)

    # Upload
    uploaded_by = Column(String(100), nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "attachment_id": self.attachment_id,
            "tenant_id": self.tenant_id,
            "story_id": self.story_id,
            "task_id": self.task_id,
            "chat_message_id": self.chat_message_id,
            "filename": self.filename,
            "original_filename": self.original_filename,
            "file_path": self.file_path,
            "file_size": self.file_size,
            "mime_type": self.mime_type,
            "description": self.description,
            "uploaded_by": self.uploaded_by,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<Attachment {self.attachment_id}: {self.filename}>"


# =============================================================================
# EPIC - Epicos (agrupamento de Stories)
# =============================================================================

class EpicStatus(str, Enum):
    """Status de Epic - Issue #193"""
    ACTIVE = "active"
    COMPLETED = "completed"
    ARCHIVED = "archived"


class Epic(Base):
    """
    Modelo para Epicos
    Agrupamento de Stories relacionadas

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "epics"

    id = Column(Integer, primary_key=True, autoincrement=True)
    epic_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)

    # Dados
    title = Column(String(300), nullable=False)
    description = Column(Text, nullable=True)
    color = Column(String(20), default="#003B4A")  # Cor para UI

    # Status - Issue #193: usar Enum para validação
    status = Column(String(30), default=EpicStatus.ACTIVE.value)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {
            "epic_id": self.epic_id,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "title": self.title,
            "description": self.description,
            "color": self.color,
            "status": self.status,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    @staticmethod
    def validate_status(status: str) -> bool:
        """Valida status - Issue #193"""
        return status in [s.value for s in EpicStatus]

    def __repr__(self):
        return f"<Epic {self.epic_id}: {self.title[:30]}>"


# =============================================================================
# SPRINT - Sprints (periodo de trabalho)
# =============================================================================

class SprintStatus(str, Enum):
    """Status de Sprint - Issue #193"""
    PLANNED = "planned"
    ACTIVE = "active"
    COMPLETED = "completed"
    CANCELLED = "cancelled"


class Sprint(Base):
    """
    Modelo para Sprints
    Periodo de trabalho com stories atribuidas

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "sprints"

    id = Column(Integer, primary_key=True, autoincrement=True)
    sprint_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)

    # Dados
    name = Column(String(200), nullable=False)
    goal = Column(Text, nullable=True)

    # Periodo
    start_date = Column(DateTime, nullable=True)
    end_date = Column(DateTime, nullable=True)

    # Status - Issue #193: usar Enum para validação
    status = Column(String(30), default=SprintStatus.PLANNED.value)

    # Metricas
    velocity = Column(Integer, default=0)  # story points completados
    capacity = Column(Integer, default=0)  # story points planejados

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {
            "sprint_id": self.sprint_id,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "name": self.name,
            "goal": self.goal,
            "start_date": self.start_date.isoformat() if self.start_date else None,
            "end_date": self.end_date.isoformat() if self.end_date else None,
            "status": self.status,
            "velocity": self.velocity,
            "capacity": self.capacity,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    @staticmethod
    def validate_status(status: str) -> bool:
        """Valida status - Issue #193"""
        return status in [s.value for s in SprintStatus]

    def __repr__(self):
        return f"<Sprint {self.sprint_id}: {self.name} [{self.status}]>"


# =============================================================================
# EXECUTION_LOG - Replay e Debug de Execucoes
# =============================================================================

class ExecutionStatus(str, Enum):
    """Status da execucao"""
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    CANCELLED = "cancelled"


class ExecutionLog(Base):
    """
    Modelo para Logs de Execucao - Permite replay e debug de tarefas executadas

    Multi-Tenancy (Issue #149):
    - tenant_id para isolamento de dados
    """
    __tablename__ = "execution_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    execution_id = Column(String(50), unique=True, nullable=False, index=True)

    # Multi-Tenant: Isolamento de dados (Issue #149)
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=True, index=True)

    task_id = Column(String(50), ForeignKey("story_tasks.task_id"), nullable=True, index=True)
    story_id = Column(String(50), nullable=True, index=True)
    project_id = Column(String(50), nullable=True, index=True)
    job_id = Column(String(50), nullable=True, index=True)
    status = Column(String(20), default=ExecutionStatus.RUNNING.value, index=True)
    started_at = Column(DateTime, default=datetime.utcnow)
    ended_at = Column(DateTime, nullable=True)
    duration_ms = Column(Integer, default=0)
    steps = Column(JSON, default=list)
    original_input = Column(JSON, default=dict)
    output = Column(JSON, default=dict)
    files_created = Column(JSON, default=list)
    files_modified = Column(JSON, default=list)
    error_message = Column(Text, nullable=True)
    error_type = Column(String(100), nullable=True)
    stack_trace = Column(Text, nullable=True)
    worker_id = Column(String(50), nullable=True)
    agent_model = Column(String(50), nullable=True)
    total_tokens = Column(Integer, default=0)
    total_cost = Column(Float, default=0.0)
    replay_of = Column(String(50), nullable=True)
    replay_count = Column(Integer, default=0)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = Column(String(100), default="system")

    def to_dict(self):
        return {
            "execution_id": self.execution_id, "tenant_id": self.tenant_id, "task_id": self.task_id,
            "story_id": self.story_id, "project_id": self.project_id,
            "job_id": self.job_id, "status": self.status,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "ended_at": self.ended_at.isoformat() if self.ended_at else None,
            "duration_ms": self.duration_ms, "steps": self.steps or [],
            "original_input": self.original_input or {}, "output": self.output or {},
            "files_created": self.files_created or [], "files_modified": self.files_modified or [],
            "error_message": self.error_message, "error_type": self.error_type,
            "stack_trace": self.stack_trace, "worker_id": self.worker_id,
            "agent_model": self.agent_model, "total_tokens": self.total_tokens,
            "total_cost": self.total_cost, "replay_of": self.replay_of,
            "replay_count": self.replay_count,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<ExecutionLog {self.execution_id}: {self.status} [{self.duration_ms}ms]>"


# =============================================================================
# CODE VERSION - Versionamento de Codigo (Issue #58)
# =============================================================================

class CodeVersion(Base):
    """
    Modelo para Versoes de Codigo - Snapshots do codigo gerado
    Permite versionamento, diff visual, rollback e branches
    """
    __tablename__ = "code_versions"

    id = Column(Integer, primary_key=True, autoincrement=True)
    version_hash = Column(String(20), unique=True, nullable=False, index=True)

    # Relacionamentos
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=False, index=True)
    task_id = Column(String(50), nullable=True, index=True)

    # Metadados da versao
    message = Column(Text, nullable=False)  # Mensagem descritiva
    author = Column(String(100), default="system")  # Agente/worker que criou

    # Conteudo dos arquivos (JSON: {path: content})
    files_content = Column(JSON, default=dict)  # Conteudo completo
    file_hashes = Column(JSON, default=dict)    # Hashes dos arquivos

    # Historico
    parent_hash = Column(String(20), nullable=True, index=True)  # Versao anterior
    branch = Column(String(100), default="main", index=True)     # Nome da branch

    # Metadados extras (renomeado pois 'metadata' e reservado pelo SQLAlchemy)
    version_metadata = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    def to_dict(self):
        return {
            "version_hash": self.version_hash,
            "story_id": self.story_id,
            "task_id": self.task_id,
            "message": self.message,
            "author": self.author,
            "files_count": len(self.files_content) if self.files_content else 0,
            "file_hashes": self.file_hashes or {},
            "parent_hash": self.parent_hash,
            "branch": self.branch,
            "version_metadata": self.version_metadata or {},
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def get_files_list(self) -> list:
        """Retorna lista de arquivos sem conteudo"""
        if not self.files_content:
            return []
        return list(self.files_content.keys())

    def __repr__(self):
        return f"<CodeVersion {self.version_hash}: {self.message[:30]} [{self.branch}]>"


class CodeBranch(Base):
    """
    Modelo para Branches de Codigo - Permite experimentos paralelos
    """
    __tablename__ = "code_branches"

    id = Column(Integer, primary_key=True, autoincrement=True)
    branch_id = Column(String(20), unique=True, nullable=False, index=True)

    # Relacionamentos
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=False, index=True)

    # Dados da branch
    branch_name = Column(String(100), nullable=False)
    description = Column(Text, nullable=True)
    base_hash = Column(String(20), nullable=True)  # Hash de onde a branch foi criada

    # Status
    is_default = Column(Boolean, default=False)
    is_merged = Column(Boolean, default=False)
    merged_into = Column(String(100), nullable=True)  # Branch onde foi merged

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    merged_at = Column(DateTime, nullable=True)

    # Criado por
    created_by = Column(String(100), default="system")

    def to_dict(self):
        return {
            "branch_id": self.branch_id,
            "story_id": self.story_id,
            "branch_name": self.branch_name,
            "description": self.description,
            "base_hash": self.base_hash,
            "is_default": self.is_default,
            "is_merged": self.is_merged,
            "merged_into": self.merged_into,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "merged_at": self.merged_at.isoformat() if self.merged_at else None,
            "created_by": self.created_by
        }

    def __repr__(self):
        return f"<CodeBranch {self.branch_name} ({self.story_id})>"


# =============================================================================
# PRODUCTIVITY METRICS - Analytics de Produtividade (Issue #65)
# =============================================================================

class MetricType(str, Enum):
    """Tipo de metrica de produtividade"""
    VELOCITY = "velocity"
    THROUGHPUT = "throughput"
    CYCLE_TIME = "cycle_time"
    LEAD_TIME = "lead_time"
    WIP = "wip"
    QUALITY = "quality"
    COLLABORATION = "collaboration"


class ProductivityMetric(Base):
    """Modelo para Metricas de Produtividade - snapshots de metricas para analise historica."""
    __tablename__ = "productivity_metrics"
    id = Column(Integer, primary_key=True, autoincrement=True)
    metric_id = Column(String(50), unique=True, nullable=False, index=True)
    project_id = Column(String(50), nullable=True, index=True)
    sprint_id = Column(String(50), nullable=True, index=True)
    assignee = Column(String(100), nullable=True, index=True)
    metric_type = Column(String(30), default=MetricType.VELOCITY.value, index=True)
    period_start = Column(DateTime, nullable=False)
    period_end = Column(DateTime, nullable=False)
    period_type = Column(String(20), default="week")
    value = Column(Float, default=0.0)
    previous_value = Column(Float, nullable=True)
    target_value = Column(Float, nullable=True)
    unit = Column(String(50), nullable=True)
    trend_direction = Column(String(20), nullable=True)
    change_percentage = Column(Float, default=0.0)
    details = Column(JSON, default=dict)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)


class ProductivitySnapshot(Base):
    """Modelo para Snapshots de Produtividade - estado completo das metricas."""
    __tablename__ = "productivity_snapshots"
    id = Column(Integer, primary_key=True, autoincrement=True)
    snapshot_id = Column(String(50), unique=True, nullable=False, index=True)
    project_id = Column(String(50), nullable=True, index=True)
    sprint_id = Column(String(50), nullable=True, index=True)
    period_type = Column(String(20), default="sprint")
    period_label = Column(String(100), nullable=True)
    total_stories = Column(Integer, default=0)
    stories_completed = Column(Integer, default=0)
    total_points = Column(Integer, default=0)
    points_delivered = Column(Integer, default=0)
    velocity = Column(Float, default=0.0)
    throughput = Column(Float, default=0.0)
    avg_cycle_time_days = Column(Float, default=0.0)
    avg_lead_time_days = Column(Float, default=0.0)
    wip_count = Column(Integer, default=0)
    predictability_score = Column(Float, default=0.0)
    collaboration_rate = Column(Float, default=0.0)
    status_distribution = Column(JSON, default=dict)
    category_distribution = Column(JSON, default=dict)
    assignee_distribution = Column(JSON, default=dict)
    developer_metrics = Column(JSON, default=list)
    alerts = Column(JSON, default=list)
    insights = Column(JSON, default=list)
    captured_at = Column(DateTime, default=datetime.utcnow, index=True)
    created_at = Column(DateTime, default=datetime.utcnow)


class AgentPerformance(Base):
    """Modelo para Performance de Agentes IA vs desenvolvedores humanos."""
    __tablename__ = "agent_performance"
    id = Column(Integer, primary_key=True, autoincrement=True)
    performance_id = Column(String(50), unique=True, nullable=False, index=True)
    agent_id = Column(String(50), nullable=False, index=True)
    agent_type = Column(String(20), default="ai")
    agent_name = Column(String(100), nullable=True)
    period_start = Column(DateTime, nullable=False)
    period_end = Column(DateTime, nullable=False)
    period_type = Column(String(20), default="week")
    stories_completed = Column(Integer, default=0)
    points_delivered = Column(Integer, default=0)
    tasks_completed = Column(Integer, default=0)
    lines_of_code = Column(Integer, default=0)
    files_created = Column(Integer, default=0)
    files_modified = Column(Integer, default=0)
    success_rate = Column(Float, default=0.0)
    rework_rate = Column(Float, default=0.0)
    bugs_introduced = Column(Integer, default=0)
    bugs_fixed = Column(Integer, default=0)
    test_coverage = Column(Float, default=0.0)
    code_review_score = Column(Float, default=0.0)
    avg_task_time_hours = Column(Float, default=0.0)
    avg_story_time_hours = Column(Float, default=0.0)
    total_time_spent_hours = Column(Float, default=0.0)
    tokens_used = Column(Integer, default=0)
    cost = Column(Float, default=0.0)
    specializations = Column(JSON, default=list)
    tech_stacks = Column(JSON, default=list)
    vs_team_avg_velocity = Column(Float, default=0.0)
    rank_in_team = Column(Integer, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)


# =============================================================================
# AB TEST - Teste A/B de Codigo Gerado (Issue #71)
# =============================================================================

class ABTestStatus(str, Enum):
    PENDING = "pending"
    GENERATING = "generating"
    TESTING = "testing"
    COMPLETED = "completed"
    WINNER_SELECTED = "winner_selected"
    CANCELLED = "cancelled"


class VariantStatus(str, Enum):
    PENDING = "pending"
    GENERATING = "generating"
    GENERATED = "generated"
    TESTING = "testing"
    TESTED = "tested"
    WINNER = "winner"
    DISCARDED = "discarded"
    FAILED = "failed"


class ABTest(Base):
    __tablename__ = "ab_tests"

    id = Column(Integer, primary_key=True, autoincrement=True)
    test_id = Column(String(50), unique=True, nullable=False, index=True)
    story_id = Column(String(50), ForeignKey("stories.story_id"), nullable=False, index=True)
    title = Column(String(300), nullable=False)
    description = Column(Text, nullable=True)
    status = Column(String(30), default=ABTestStatus.PENDING.value, index=True)
    winner_id = Column(String(50), nullable=True)
    recommendation = Column(JSON, default=dict)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    completed_at = Column(DateTime, nullable=True)
    created_by = Column(String(100), default="system")
    variants = relationship("ABTestVariant", back_populates="ab_test", cascade="all, delete-orphan")

    def to_dict(self):
        return {
            "test_id": self.test_id, "story_id": self.story_id, "title": self.title,
            "description": self.description, "status": self.status, "winner_id": self.winner_id,
            "recommendation": self.recommendation or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "created_by": self.created_by,
            "variants": [v.to_dict() for v in self.variants] if self.variants else []
        }


class ABTestVariant(Base):
    __tablename__ = "ab_test_variants"

    id = Column(Integer, primary_key=True, autoincrement=True)
    variant_id = Column(String(50), unique=True, nullable=False, index=True)
    test_id = Column(String(50), ForeignKey("ab_tests.test_id"), nullable=False, index=True)
    ab_test = relationship("ABTest", back_populates="variants")
    approach = Column(String(30), nullable=False)
    status = Column(String(30), default=VariantStatus.PENDING.value, index=True)
    code = Column(Text, nullable=True)
    metrics = Column(JSON, default=dict)
    test_results = Column(JSON, default=dict)
    score = Column(Float, default=0.0)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    generated_at = Column(DateTime, nullable=True)
    tested_at = Column(DateTime, nullable=True)

    def to_dict(self):
        return {
            "variant_id": self.variant_id, "test_id": self.test_id, "approach": self.approach,
            "status": self.status, "code": self.code, "metrics": self.metrics or {},
            "test_results": self.test_results or {}, "score": self.score,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "generated_at": self.generated_at.isoformat() if self.generated_at else None,
            "tested_at": self.tested_at.isoformat() if self.tested_at else None
        }


# =============================================================================
# API MODELS (importados de api_models.py)
# =============================================================================

# Modelos para API publica e webhooks estao em api_models.py:
# - APIKey: Chaves de API para desenvolvedores externos
# - Webhook: Configuracao de webhooks
# - WebhookDelivery: Historico de entregas de webhook
# - APIRequestLog: Log de requisicoes para analytics

try:
    from factory.database.api_models import (
        APIKey,
        APIKeyTier,
        APIKeyStatus,
        Webhook,
        WebhookDelivery,
        WebhookEventType,
        WebhookStatus,
        APIRequestLog,
    )
except ImportError:
    pass  # api_models pode nao estar disponivel em todas as instalacoes


# =============================================================================
# MARKETPLACE - Templates, Skills e Configuracoes (Issue #56)
# =============================================================================

class MarketplaceCategory(str, Enum):
    """Categorias do Marketplace"""
    PROJECT_TEMPLATE = "project_template"
    STORY_TEMPLATE = "story_template"
    SKILL = "skill"
    AGENT_CONFIG = "agent_config"


class MarketplaceItem(Base):
    """
    Modelo base para itens do Marketplace
    Templates, Skills e Configuracoes de Agentes
    """
    __tablename__ = "marketplace_items"

    id = Column(Integer, primary_key=True, autoincrement=True)
    item_id = Column(String(50), unique=True, nullable=False, index=True)

    # Dados principais
    name = Column(String(300), nullable=False)
    description = Column(Text, nullable=True)
    category = Column(String(30), default=MarketplaceCategory.STORY_TEMPLATE.value, index=True)

    # Autor e versao
    author = Column(String(100), default="Fabrica de Agentes")
    version = Column(String(20), default="1.0.0")

    # Tags para busca
    tags = Column(JSON, default=list)

    # Metricas
    downloads = Column(Integer, default=0)
    rating = Column(Float, default=0.0)
    reviews_count = Column(Integer, default=0)

    # Status
    verified = Column(Boolean, default=False)
    featured = Column(Boolean, default=False)
    published = Column(Boolean, default=True)

    # Thumbnail/imagem
    thumbnail = Column(Text, nullable=True)

    # Conteudo do item (JSON com dados especificos de cada tipo)
    content = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relacionamentos
    reviews = relationship("MarketplaceReview", back_populates="item", cascade="all, delete-orphan")

    def to_dict(self):
        return {
            "item_id": self.item_id,
            "name": self.name,
            "description": self.description,
            "category": self.category,
            "author": self.author,
            "version": self.version,
            "tags": self.tags or [],
            "downloads": self.downloads,
            "rating": self.rating,
            "reviews_count": self.reviews_count,
            "verified": self.verified,
            "featured": self.featured,
            "published": self.published,
            "thumbnail": self.thumbnail,
            "content": self.content or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def __repr__(self):
        return f"<MarketplaceItem {self.item_id}: {self.name[:30]} [{self.category}]>"


class MarketplaceReview(Base):
    """
    Modelo para Reviews/Avaliacoes de itens do Marketplace
    """
    __tablename__ = "marketplace_reviews"

    id = Column(Integer, primary_key=True, autoincrement=True)
    review_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com item
    item_id = Column(String(50), ForeignKey("marketplace_items.item_id"), nullable=False, index=True)
    item = relationship("MarketplaceItem", back_populates="reviews")

    # Usuario que fez a review
    user_id = Column(String(100), nullable=False)

    # Avaliacao
    rating = Column(Integer, nullable=False)  # 1-5
    comment = Column(Text, nullable=True)

    # Utilidade
    helpful_count = Column(Integer, default=0)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "review_id": self.review_id,
            "item_id": self.item_id,
            "user_id": self.user_id,
            "rating": self.rating,
            "comment": self.comment,
            "helpful_count": self.helpful_count,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<MarketplaceReview {self.review_id}: {self.rating} stars>"


class MarketplaceDownload(Base):
    """
    Modelo para rastrear downloads de itens do Marketplace
    """
    __tablename__ = "marketplace_downloads"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Item baixado
    item_id = Column(String(50), nullable=False, index=True)

    # Usuario que baixou
    user_id = Column(String(100), nullable=False, index=True)

    # Projeto onde foi instalado (opcional)
    project_id = Column(String(50), nullable=True, index=True)

    # Timestamps
    downloaded_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "item_id": self.item_id,
            "user_id": self.user_id,
            "project_id": self.project_id,
            "downloaded_at": self.downloaded_at.isoformat() if self.downloaded_at else None
        }

    def __repr__(self):
        return f"<MarketplaceDownload {self.item_id} by {self.user_id}>"


# =============================================================================
# RBAC - Role-Based Access Control (v6.1 - Issue #12)
# =============================================================================

class Permission(Base):
    """
    Modelo para Permissoes granulares
    Define acoes permitidas em recursos especificos
    """
    __tablename__ = "permissions"

    id = Column(Integer, primary_key=True, autoincrement=True)
    permission_id = Column(String(50), unique=True, nullable=False, index=True)

    # Recurso e acao
    resource = Column(String(50), nullable=False, index=True)  # stories, tasks, projects, users, etc.
    action = Column(String(20), nullable=False)  # create, read, update, delete, manage

    # Descricao
    description = Column(String(200), nullable=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    def to_dict(self):
        return {
            "permission_id": self.permission_id,
            "resource": self.resource,
            "action": self.action,
            "description": self.description,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<Permission {self.resource}:{self.action}>"


class Role(Base):
    """
    Modelo para Roles (Papeis)
    Agrupa permissoes para facilitar atribuicao a usuarios
    """
    __tablename__ = "roles"

    id = Column(Integer, primary_key=True, autoincrement=True)
    role_id = Column(String(50), unique=True, nullable=False, index=True)

    # Dados
    name = Column(String(100), nullable=False, unique=True)
    description = Column(Text, nullable=True)

    # Permissoes (JSON array de permission_ids ou resource:action strings)
    permissions = Column(JSON, default=list)

    # Nivel hierarquico (quanto maior, mais privilegios)
    level = Column(Integer, default=0)

    # Role do sistema (nao pode ser deletado)
    is_system = Column(Boolean, default=False)

    # Status
    active = Column(Boolean, default=True)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relacionamentos
    user_roles = relationship("UserRole", back_populates="role", cascade="all, delete-orphan")

    def to_dict(self):
        return {
            "role_id": self.role_id,
            "name": self.name,
            "description": self.description,
            "permissions": self.permissions or [],
            "level": self.level,
            "is_system": self.is_system,
            "active": self.active,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def has_permission(self, resource: str, action: str) -> bool:
        """Verifica se a role tem a permissao especificada"""
        if not self.permissions:
            return False

        permission_key = f"{resource}:{action}"

        # Verificar permissao exata
        if permission_key in self.permissions:
            return True

        # Verificar wildcard (ex: stories:* permite todas acoes em stories)
        if f"{resource}:*" in self.permissions:
            return True

        # Verificar permissao total (admin)
        if "*:*" in self.permissions:
            return True

        return False

    def __repr__(self):
        return f"<Role {self.name} [level={self.level}]>"


class UserRole(Base):
    """
    Modelo para Associacao Usuario-Role
    Permite atribuir roles a usuarios, opcionalmente por projeto
    """
    __tablename__ = "user_roles"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamentos
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False, index=True)
    role_id = Column(String(50), ForeignKey("roles.role_id"), nullable=False, index=True)

    # Escopo opcional (se None, aplica globalmente)
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=True, index=True)

    # Relacionamentos ORM
    user = relationship("User", backref="user_roles")
    role = relationship("Role", back_populates="user_roles")

    # Atribuido por
    assigned_by = Column(String(100), nullable=True)
    assigned_at = Column(DateTime, default=datetime.utcnow)

    # Expiracao opcional
    expires_at = Column(DateTime, nullable=True)

    def to_dict(self):
        return {
            "id": self.id,
            "user_id": self.user_id,
            "role_id": self.role_id,
            "project_id": self.project_id,
            "assigned_by": self.assigned_by,
            "assigned_at": self.assigned_at.isoformat() if self.assigned_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "role": self.role.to_dict() if self.role else None
        }

    def is_valid(self) -> bool:
        """Verifica se a atribuicao ainda e valida"""
        if self.expires_at and datetime.utcnow() > self.expires_at:
            return False
        return True

    def __repr__(self):
        scope = f" in {self.project_id}" if self.project_id else " (global)"
        return f"<UserRole user={self.user_id} role={self.role_id}{scope}>"


class AuditLog(Base):
    """
    Modelo para Log de Auditoria de Acoes RBAC
    Registra todas as acoes de usuarios para compliance
    """
    __tablename__ = "audit_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Quem fez
    user_id = Column(Integer, nullable=True, index=True)
    username = Column(String(100), nullable=True)

    # O que fez
    action = Column(String(50), nullable=False, index=True)  # create, read, update, delete, login, etc.
    resource = Column(String(50), nullable=False, index=True)
    resource_id = Column(String(100), nullable=True)

    # Detalhes
    details = Column(JSON, default=dict)
    ip_address = Column(String(50), nullable=True)
    user_agent = Column(String(500), nullable=True)

    # Resultado
    success = Column(Boolean, default=True)
    error_message = Column(Text, nullable=True)

    # Timestamp
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)

    def to_dict(self):
        return {
            "id": self.id,
            "user_id": self.user_id,
            "username": self.username,
            "action": self.action,
            "resource": self.resource,
            "resource_id": self.resource_id,
            "details": self.details or {},
            "ip_address": self.ip_address,
            "success": self.success,
            "error_message": self.error_message,
            "timestamp": self.timestamp.isoformat() if self.timestamp else None
        }

    def __repr__(self):
        return f"<AuditLog {self.username}:{self.action}:{self.resource}>"


# =============================================================================
# MULTI-TENANCY MODELS (Issues #81, #82)
# =============================================================================

class TenantStatus(str, Enum):
    """Status do Tenant"""
    ACTIVE = "active"
    SUSPENDED = "suspended"
    TRIAL = "trial"
    CANCELLED = "cancelled"
    PENDING = "pending"


class TenantPlan(str, Enum):
    """Planos disponiveis"""
    FREE = "free"
    STARTER = "starter"
    PROFESSIONAL = "professional"
    ENTERPRISE = "enterprise"
    CUSTOM = "custom"


class MemberRole(str, Enum):
    """Roles de membros do tenant"""
    OWNER = "owner"
    ADMIN = "admin"
    MEMBER = "member"
    VIEWER = "viewer"
    BILLING = "billing"


class MemberStatus(str, Enum):
    """Status do membro no tenant"""
    ACTIVE = "active"
    INVITED = "invited"
    SUSPENDED = "suspended"
    DEACTIVATED = "deactivated"


class InviteStatus(str, Enum):
    """Status do convite"""
    PENDING = "pending"
    ACCEPTED = "accepted"
    EXPIRED = "expired"
    REVOKED = "revoked"


class Tenant(Base):
    """
    Modelo consolidado para Tenant - Organizacao/Empresa (Issues #81, #82)

    Cada tenant tem isolamento completo de dados e pode personalizar
    a aparencia da plataforma (white label).

    Relacionamentos com cascading delete:
    - projects: Projetos do tenant
    - stories: Stories do tenant
    - tasks: Tasks do tenant
    - jobs: Jobs do tenant
    - members: Membros do tenant
    - settings: Configuracoes do tenant
    - branding: Personalizacao visual
    - invites: Convites pendentes
    - usage_logs: Logs de uso para billing
    """
    __tablename__ = "tenants"

    id = Column(Integer, primary_key=True, autoincrement=True)
    tenant_id = Column(String(50), unique=True, nullable=False, index=True)

    # Identificacao
    name = Column(String(200), nullable=False)
    slug = Column(String(100), unique=True, nullable=False, index=True)
    description = Column(Text, nullable=True)

    # Dominio customizado (white label)
    custom_domain = Column(String(255), unique=True, nullable=True)
    primary_domain = Column(String(255), nullable=True)

    # Contato
    email = Column(String(255), nullable=False)
    phone = Column(String(50), nullable=True)

    # Endereco (para faturamento)
    address = Column(JSON, default=dict)

    # Status e Plano
    status = Column(String(30), default=TenantStatus.TRIAL.value, index=True)
    plan = Column(String(30), default=TenantPlan.FREE.value)

    # Trial
    trial_ends_at = Column(DateTime, nullable=True)

    # Configuracoes
    timezone = Column(String(50), default="America/Sao_Paulo")
    locale = Column(String(10), default="pt-BR")

    # Features habilitadas
    features = Column(JSON, default=lambda: {
        "stories": True,
        "kanban": True,
        "workers": True,
        "chat_assistant": True,
        "custom_branding": False,
        "sso": False,
        "api_access": True,
        "webhooks": False,
        "audit_logs": False,
        "custom_domain": False
    })

    # Metadados
    tenant_metadata = Column(JSON, default=dict)
    tags = Column(JSON, default=list)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    activated_at = Column(DateTime, nullable=True)
    suspended_at = Column(DateTime, nullable=True)

    # Soft Delete
    is_deleted = Column(Boolean, default=False, index=True)
    deleted_at = Column(DateTime, nullable=True)
    deleted_by = Column(String(100), nullable=True)

    # Indices compostos
    __table_args__ = (
        Index('ix_tenants_status_plan', 'status', 'plan'),
        Index('ix_tenants_created', 'created_at'),
    )

    # Relacionamentos com cascade delete (Issue #82)
    settings = relationship("TenantSettings", back_populates="tenant", uselist=False, cascade="all, delete-orphan")
    branding = relationship("BrandingConfig", back_populates="tenant", uselist=False, cascade="all, delete-orphan")
    members = relationship("TenantMember", back_populates="tenant", cascade="all, delete-orphan")
    invites = relationship("TenantInvite", back_populates="tenant", cascade="all, delete-orphan")
    usage_logs = relationship("TenantUsageLog", back_populates="tenant", cascade="all, delete-orphan")

    # Relacionamentos com entidades principais (Issue #81)
    projects = relationship("Project", back_populates="tenant", cascade="all, delete-orphan")
    stories = relationship("Story", back_populates="tenant", cascade="all, delete-orphan")
    tasks = relationship("Task", back_populates="tenant", cascade="all, delete-orphan")
    jobs = relationship("Job", back_populates="tenant", cascade="all, delete-orphan")

    def soft_delete(self, deleted_by: str = "system"):
        """Marca o tenant como deletado (soft delete)"""
        self.is_deleted = True
        self.deleted_at = datetime.utcnow()
        self.deleted_by = deleted_by

    def restore(self):
        """Restaura um tenant deletado"""
        self.is_deleted = False
        self.deleted_at = None
        self.deleted_by = None

    def to_dict(self, include_sensitive: bool = False) -> Dict[str, Any]:
        """Converte para dicionario"""
        data = {
            "tenant_id": self.tenant_id,
            "name": self.name,
            "slug": self.slug,
            "description": self.description,
            "custom_domain": self.custom_domain,
            "email": self.email,
            "status": self.status,
            "plan": self.plan,
            "trial_ends_at": self.trial_ends_at.isoformat() if self.trial_ends_at else None,
            "timezone": self.timezone,
            "locale": self.locale,
            "features": self.features or {},
            "tags": self.tags or [],
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "is_deleted": self.is_deleted,
            "members_count": len(self.members) if self.members else 0,
            "projects_count": len(self.projects) if self.projects else 0
        }

        if include_sensitive:
            data["phone"] = self.phone
            data["address"] = self.address
            data["primary_domain"] = self.primary_domain
            data["tenant_metadata"] = self.tenant_metadata

        return data

    def is_active(self) -> bool:
        """Verifica se tenant esta ativo"""
        return self.status in [TenantStatus.ACTIVE.value, TenantStatus.TRIAL.value] and not self.is_deleted

    def has_feature(self, feature_name: str) -> bool:
        """Verifica se tenant tem feature habilitada"""
        if not self.features:
            return False
        return self.features.get(feature_name, False)

    def __repr__(self):
        return f"<Tenant {self.tenant_id}: {self.name} [{self.plan}]>"


class TenantSettings(Base):
    """
    Configuracoes especificas do Tenant (Issue #82)

    Define limites, modelos de IA preferidos, integracoes ativas, etc.
    """
    __tablename__ = "tenant_settings"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), unique=True, nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="settings")

    # Limites de Uso
    max_projects = Column(Integer, default=10)
    max_stories_per_project = Column(Integer, default=100)
    max_members = Column(Integer, default=5)
    max_storage_gb = Column(Float, default=5.0)
    max_api_calls_per_day = Column(Integer, default=1000)
    max_tokens_per_month = Column(Integer, default=100000)
    max_concurrent_workers = Column(Integer, default=2)

    # Modelo Claude Preferido
    preferred_model = Column(String(50), default="claude-sonnet-4-20250514")
    allowed_models = Column(JSON, default=lambda: [
        "claude-sonnet-4-20250514",
        "claude-3-5-sonnet-20241022",
        "claude-3-haiku-20240307"
    ])

    # Configuracoes de IA
    ai_settings = Column(JSON, default=lambda: {
        "temperature": 0.7,
        "max_tokens": 4096,
        "auto_suggest_stories": True,
        "auto_estimate_points": True,
        "code_review_enabled": True
    })

    # Integracoes Ativas
    integrations = Column(JSON, default=lambda: {
        "github": {"enabled": False, "org": None, "token": None},
        "slack": {"enabled": False, "webhook_url": None},
        "jira": {"enabled": False, "url": None, "token": None},
        "azure_devops": {"enabled": False, "org": None, "token": None}
    })

    # Webhooks
    webhooks = Column(JSON, default=lambda: {
        "story_created": None,
        "story_completed": None,
        "task_completed": None,
        "project_completed": None
    })

    # Notificacoes
    notifications = Column(JSON, default=lambda: {
        "email_on_story_complete": True,
        "email_on_task_blocked": True,
        "email_daily_summary": False,
        "slack_notifications": False
    })

    # Personalizacao do Workflow
    workflow_settings = Column(JSON, default=lambda: {
        "story_statuses": ["backlog", "ready", "in_progress", "review", "testing", "done"],
        "require_review": True,
        "require_testing": True,
        "auto_move_on_complete": True
    })

    # Seguranca
    security_settings = Column(JSON, default=lambda: {
        "require_2fa": False,
        "session_timeout_minutes": 480,
        "ip_whitelist": [],
        "password_policy": {
            "min_length": 8,
            "require_uppercase": True,
            "require_numbers": True,
            "require_special": False
        }
    })

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self, include_sensitive: bool = False) -> Dict[str, Any]:
        """Converte para dicionario"""
        data = {
            "tenant_id": self.tenant_id,
            "max_projects": self.max_projects,
            "max_stories_per_project": self.max_stories_per_project,
            "max_members": self.max_members,
            "max_storage_gb": self.max_storage_gb,
            "max_api_calls_per_day": self.max_api_calls_per_day,
            "max_tokens_per_month": self.max_tokens_per_month,
            "max_concurrent_workers": self.max_concurrent_workers,
            "preferred_model": self.preferred_model,
            "allowed_models": self.allowed_models or [],
            "ai_settings": self.ai_settings or {},
            "workflow_settings": self.workflow_settings or {},
            "notifications": self.notifications or {},
            "security_settings": self.security_settings or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

        if include_sensitive:
            data["integrations"] = self.integrations
            data["webhooks"] = self.webhooks

        return data

    def check_limit(self, limit_name: str, current_value: int) -> bool:
        """Verifica se ainda tem quota disponivel"""
        limit = getattr(self, limit_name, None)
        if limit is None:
            return True
        return current_value < limit

    def __repr__(self):
        return f"<TenantSettings {self.tenant_id}>"


class BrandingConfig(Base):
    """
    Personalizacao Visual do Tenant - White Label (Issue #82)
    """
    __tablename__ = "branding_configs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), unique=True, nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="branding")

    # Logo e Favicon
    logo_url = Column(String(500), nullable=True)
    logo_dark_url = Column(String(500), nullable=True)
    favicon_url = Column(String(500), nullable=True)
    logo_width = Column(Integer, default=180)
    logo_height = Column(Integer, default=50)

    # Nome exibido
    display_name = Column(String(200), nullable=True)
    tagline = Column(String(300), nullable=True)

    # Cores Principais (CSS Variables)
    colors = Column(JSON, default=lambda: {
        "primary": "#003B4A",
        "primary_hover": "#00526A",
        "primary_light": "#E6F0F2",
        "secondary": "#FF6C00",
        "secondary_hover": "#E65C00",
        "success": "#10B981",
        "warning": "#F59E0B",
        "error": "#EF4444",
        "info": "#3B82F6",
        "background": "#F3F4F6",
        "surface": "#FFFFFF",
        "text_primary": "#1F2937",
        "text_secondary": "#6B7280",
        "border": "#E5E7EB",
        "header_bg": "#003B4A",
        "header_text": "#FFFFFF"
    })

    # Dark Mode
    dark_mode_enabled = Column(Boolean, default=True)
    dark_colors = Column(JSON, default=lambda: {
        "primary": "#00A3CC",
        "background": "#111827",
        "surface": "#1F2937",
        "text_primary": "#F9FAFB",
        "text_secondary": "#D1D5DB",
        "border": "#374151"
    })

    # Tipografia
    fonts = Column(JSON, default=lambda: {
        "primary": "'Inter', 'Segoe UI', sans-serif",
        "heading": "'Inter', 'Segoe UI', sans-serif",
        "monospace": "'JetBrains Mono', 'Fira Code', monospace",
        "size_base": "14px"
    })

    # CSS Customizado
    custom_css = Column(Text, nullable=True)

    # Footer
    footer = Column(JSON, default=lambda: {
        "show_footer": True,
        "text": None,
        "links": [],
        "copyright": None,
        "show_powered_by": True
    })

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "logo_url": self.logo_url,
            "logo_dark_url": self.logo_dark_url,
            "favicon_url": self.favicon_url,
            "display_name": self.display_name,
            "tagline": self.tagline,
            "colors": self.colors or {},
            "dark_mode_enabled": self.dark_mode_enabled,
            "dark_colors": self.dark_colors or {},
            "fonts": self.fonts or {},
            "footer": self.footer or {},
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def __repr__(self):
        return f"<BrandingConfig {self.tenant_id}>"


class TenantMember(Base):
    """
    Membros de um Tenant (Issue #82)

    Define quem tem acesso ao tenant e com qual role.
    """
    __tablename__ = "tenant_members"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamentos
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="members")

    user_id = Column(Integer, ForeignKey("users.id", ondelete="CASCADE"), nullable=False, index=True)

    # Role no tenant
    tenant_role = Column(String(30), default=MemberRole.MEMBER.value, nullable=False)

    # Status do membro
    status = Column(String(30), default=MemberStatus.ACTIVE.value, index=True)
    active = Column(Boolean, default=True)

    # Permissoes customizadas
    custom_permissions = Column(JSON, default=dict)

    # Configuracoes do membro
    settings = Column(JSON, default=lambda: {
        "notifications_enabled": True,
        "email_digest": "daily",
        "default_project": None
    })

    # Timestamps
    joined_at = Column(DateTime, default=datetime.utcnow)
    last_active_at = Column(DateTime, nullable=True)
    invited_by = Column(String(100), nullable=True)
    invited_at = Column(DateTime, nullable=True)
    suspended_at = Column(DateTime, nullable=True)
    suspension_reason = Column(Text, nullable=True)

    # Constraint para evitar duplicatas
    __table_args__ = (
        UniqueConstraint('tenant_id', 'user_id', name='uix_tenant_member'),
        Index('ix_tenant_members_tenant_status', 'tenant_id', 'status'),
    )

    @property
    def role(self):
        return self.tenant_role

    @role.setter
    def role(self, value):
        self.tenant_role = value

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "tenant_role": self.tenant_role,
            "role": self.tenant_role,
            "status": self.status,
            "active": self.active,
            "custom_permissions": self.custom_permissions or {},
            "settings": self.settings or {},
            "joined_at": self.joined_at.isoformat() if self.joined_at else None,
            "last_active_at": self.last_active_at.isoformat() if self.last_active_at else None,
            "invited_by": self.invited_by
        }

    def is_active_member(self) -> bool:
        return self.status == MemberStatus.ACTIVE.value and self.active

    def has_permission(self, permission: str) -> bool:
        """Verifica se membro tem permissao especifica"""
        role_permissions = {
            MemberRole.OWNER.value: ["*"],
            MemberRole.ADMIN.value: [
                "manage_members", "manage_projects", "manage_settings",
                "view_billing", "create_stories", "delete_stories"
            ],
            MemberRole.MEMBER.value: [
                "create_projects", "create_stories", "edit_stories"
            ],
            MemberRole.VIEWER.value: ["view"],
            MemberRole.BILLING.value: ["view_billing", "manage_billing"]
        }

        if self.tenant_role == MemberRole.OWNER.value:
            return True

        if self.custom_permissions and permission in self.custom_permissions:
            return self.custom_permissions[permission]

        permissions = role_permissions.get(self.tenant_role, [])
        return permission in permissions

    def __repr__(self):
        return f"<TenantMember {self.tenant_id}:{self.user_id} [{self.tenant_role}]>"


class TenantInvite(Base):
    """
    Convites de Membros para Tenant (Issue #82)
    """
    __tablename__ = "tenant_invites"

    id = Column(Integer, primary_key=True, autoincrement=True)
    invite_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="invites")

    # Convite
    email = Column(String(255), nullable=False, index=True)
    role = Column(String(30), default=MemberRole.MEMBER.value)

    # Token
    token = Column(String(255), unique=True, nullable=False)

    # Status
    status = Column(String(30), default=InviteStatus.PENDING.value)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    expires_at = Column(DateTime, nullable=False)
    accepted_at = Column(DateTime, nullable=True)

    # Quem convidou
    invited_by = Column(String(100), nullable=False)
    message = Column(Text, nullable=True)

    # Indices
    __table_args__ = (
        Index('ix_tenant_invites_tenant_status', 'tenant_id', 'status'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "invite_id": self.invite_id,
            "tenant_id": self.tenant_id,
            "email": self.email,
            "role": self.role,
            "status": self.status,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "accepted_at": self.accepted_at.isoformat() if self.accepted_at else None,
            "invited_by": self.invited_by
        }

    def is_expired(self) -> bool:
        return datetime.utcnow() > self.expires_at

    def is_valid(self) -> bool:
        return self.status == InviteStatus.PENDING.value and not self.is_expired()

    def __repr__(self):
        return f"<TenantInvite {self.invite_id}: {self.email} [{self.status}]>"


class TenantUsageLog(Base):
    """
    Registro de Uso do Tenant para Billing (Issue #82)
    """
    __tablename__ = "tenant_usage_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento
    tenant_id = Column(String(50), ForeignKey("tenants.tenant_id", ondelete="CASCADE"), nullable=False, index=True)
    tenant = relationship("Tenant", back_populates="usage_logs")

    # Periodo
    period_start = Column(DateTime, nullable=False, index=True)
    period_end = Column(DateTime, nullable=False)

    # Metricas de Uso
    tokens_input = Column(Integer, default=0)
    tokens_output = Column(Integer, default=0)
    api_calls = Column(Integer, default=0)
    storage_bytes = Column(Integer, default=0)
    workers_executed = Column(Integer, default=0)
    stories_created = Column(Integer, default=0)
    projects_created = Column(Integer, default=0)

    # Custo Calculado
    cost_tokens = Column(Float, default=0.0)
    cost_storage = Column(Float, default=0.0)
    cost_workers = Column(Float, default=0.0)
    cost_total = Column(Float, default=0.0)

    # Detalhes
    details = Column(JSON, default=dict)

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)

    # Indices
    __table_args__ = (
        Index('ix_tenant_usage_tenant_period', 'tenant_id', 'period_start'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "period_start": self.period_start.isoformat() if self.period_start else None,
            "period_end": self.period_end.isoformat() if self.period_end else None,
            "tokens_input": self.tokens_input,
            "tokens_output": self.tokens_output,
            "tokens_total": self.tokens_input + self.tokens_output,
            "api_calls": self.api_calls,
            "storage_bytes": self.storage_bytes,
            "storage_gb": round(self.storage_bytes / (1024**3), 2),
            "workers_executed": self.workers_executed,
            "stories_created": self.stories_created,
            "projects_created": self.projects_created,
            "cost_tokens": self.cost_tokens,
            "cost_storage": self.cost_storage,
            "cost_workers": self.cost_workers,
            "cost_total": self.cost_total,
            "created_at": self.created_at.isoformat() if self.created_at else None
        }

    def __repr__(self):
        return f"<TenantUsageLog {self.tenant_id}: {self.period_start.date()} - ${self.cost_total:.2f}>"


class ProjectRole(str, Enum):
    """Roles de membros em um projeto"""
    OWNER = "owner"
    ADMIN = "admin"
    DEVELOPER = "developer"
    VIEWER = "viewer"


class ProjectMember(Base):
    """
    Membros de um Projeto (Issue #82)

    Permite controle granular de acesso por projeto dentro de um tenant.
    """
    __tablename__ = "project_members"

    id = Column(Integer, primary_key=True, autoincrement=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id", ondelete="CASCADE"), nullable=False, index=True)

    # Relacionamento com usuario
    user_id = Column(Integer, ForeignKey("users.id", ondelete="CASCADE"), nullable=False, index=True)

    # Role no projeto
    project_role = Column(String(30), default=ProjectRole.DEVELOPER.value, nullable=False)

    # Permissoes customizadas
    permissions = Column(JSON, default=lambda: {
        "can_create_stories": True,
        "can_edit_stories": True,
        "can_delete_stories": False,
        "can_manage_sprints": False,
        "can_manage_members": False,
        "can_export_data": False,
        "can_run_workers": True
    })

    # Status
    status = Column(String(30), default=MemberStatus.ACTIVE.value, index=True)
    active = Column(Boolean, default=True)

    # Timestamps
    added_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    added_by = Column(String(100), nullable=True)

    # Constraint para evitar duplicatas
    __table_args__ = (
        UniqueConstraint('project_id', 'user_id', name='uix_project_member'),
        Index('ix_project_members_project_status', 'project_id', 'status'),
    )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "project_id": self.project_id,
            "user_id": self.user_id,
            "project_role": self.project_role,
            "permissions": self.permissions or {},
            "status": self.status,
            "active": self.active,
            "added_at": self.added_at.isoformat() if self.added_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "added_by": self.added_by
        }

    def has_permission(self, permission: str) -> bool:
        """Verifica se membro tem permissao especifica no projeto"""
        role_permissions = {
            ProjectRole.OWNER.value: ["*"],
            ProjectRole.ADMIN.value: [
                "can_create_stories", "can_edit_stories", "can_delete_stories",
                "can_manage_sprints", "can_manage_members", "can_export_data", "can_run_workers"
            ],
            ProjectRole.DEVELOPER.value: [
                "can_create_stories", "can_edit_stories", "can_run_workers"
            ],
            ProjectRole.VIEWER.value: []
        }

        if self.project_role == ProjectRole.OWNER.value:
            return True

        if self.permissions and permission in self.permissions:
            return self.permissions[permission]

        permissions = role_permissions.get(self.project_role, [])
        return permission in permissions

    def __repr__(self):
        return f"<ProjectMember {self.project_id}:{self.user_id} [{self.project_role}]>"


# =============================================================================
# DEPRECATED MODELS (mantidos para compatibilidade durante migracao)
# =============================================================================

# Os modelos abaixo foram removidos na v4.0:
# - Agent: substituido por Worker
# - Skill: MCP tools gerenciados diretamente pelo Claude
# - Template: simplificado (config no projeto)
# - FactoryEvent: merged com ActivityLog

# tenant_models.py foi unificado com models.py na v5.0 (Issues #81, #82)
# Os seguintes modelos foram consolidados aqui:
# - Tenant, TenantSettings, BrandingConfig, TenantMember
# - TenantInvite, TenantUsageLog, ProjectMember

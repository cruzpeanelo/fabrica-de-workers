"""
Modelos SQLAlchemy para a Fabrica de Agentes v4.0
Arquitetura Worker-based (Single Claude + Tools per Worker)
"""
from sqlalchemy import Column, Integer, String, Text, DateTime, JSON, ForeignKey, Boolean, Float
from sqlalchemy.orm import relationship
from datetime import datetime
from enum import Enum

# Import Base
try:
    from .connection import Base
except ImportError:
    from connection import Base


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
    """Modelo para Projetos - cada aplicacao construida pela fabrica"""
    __tablename__ = "projects"

    id = Column(Integer, primary_key=True, autoincrement=True)
    project_id = Column(String(50), unique=True, nullable=False, index=True)
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

    # Relacionamentos
    jobs = relationship("Job", back_populates="project", cascade="all, delete-orphan")
    tasks = relationship("Task", back_populates="project", cascade="all, delete-orphan")

    def to_dict(self):
        return {
            "project_id": self.project_id,
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
    """
    __tablename__ = "jobs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    job_id = Column(String(50), unique=True, nullable=False, index=True)

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

    # Relacionamentos
    failures = relationship("FailureHistory", back_populates="job", cascade="all, delete-orphan")

    def to_dict(self):
        return {
            "job_id": self.job_id,
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
            "created_by": self.created_by
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
    """Modelo para Logs de Atividades"""
    __tablename__ = "activity_logs"

    id = Column(Integer, primary_key=True, autoincrement=True)

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

    def to_dict(self):
        return {
            "id": self.id,
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
    """
    __tablename__ = "tasks"

    id = Column(Integer, primary_key=True, autoincrement=True)
    task_id = Column(String(50), unique=True, nullable=False, index=True)

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

    def to_dict(self):
        return {
            "task_id": self.task_id,
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
            "created_by": self.created_by
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
    """
    __tablename__ = "stories"

    id = Column(Integer, primary_key=True, autoincrement=True)
    story_id = Column(String(50), unique=True, nullable=False, index=True)

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

    # Relacionamentos
    story_tasks = relationship("StoryTask", back_populates="story", cascade="all, delete-orphan")
    documentation = relationship("StoryDocumentation", back_populates="story", cascade="all, delete-orphan")
    designs = relationship("StoryDesign", back_populates="story", cascade="all, delete-orphan")
    attachments = relationship("Attachment", back_populates="story", cascade="all, delete-orphan",
                               foreign_keys="Attachment.story_id")

    def to_dict(self):
        return {
            "story_id": self.story_id,
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
            "created_by": self.created_by
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
    """
    __tablename__ = "story_tasks"

    id = Column(Integer, primary_key=True, autoincrement=True)
    task_id = Column(String(50), unique=True, nullable=False, index=True)

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
    """
    __tablename__ = "story_documentation"

    id = Column(Integer, primary_key=True, autoincrement=True)
    doc_id = Column(String(50), unique=True, nullable=False, index=True)

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
    """
    __tablename__ = "story_designs"

    id = Column(Integer, primary_key=True, autoincrement=True)
    design_id = Column(String(50), unique=True, nullable=False, index=True)

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
    """
    __tablename__ = "chat_messages"

    id = Column(Integer, primary_key=True, autoincrement=True)
    message_id = Column(String(50), unique=True, nullable=False, index=True)

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
    """
    __tablename__ = "attachments"

    id = Column(Integer, primary_key=True, autoincrement=True)
    attachment_id = Column(String(50), unique=True, nullable=False, index=True)

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

class Epic(Base):
    """
    Modelo para Epicos
    Agrupamento de Stories relacionadas
    """
    __tablename__ = "epics"

    id = Column(Integer, primary_key=True, autoincrement=True)
    epic_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)

    # Dados
    title = Column(String(300), nullable=False)
    description = Column(Text, nullable=True)
    color = Column(String(20), default="#003B4A")  # Cor para UI

    # Status
    status = Column(String(30), default="active")

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {
            "epic_id": self.epic_id,
            "project_id": self.project_id,
            "title": self.title,
            "description": self.description,
            "color": self.color,
            "status": self.status,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    def __repr__(self):
        return f"<Epic {self.epic_id}: {self.title[:30]}>"


# =============================================================================
# SPRINT - Sprints (periodo de trabalho)
# =============================================================================

class Sprint(Base):
    """
    Modelo para Sprints
    Periodo de trabalho com stories atribuidas
    """
    __tablename__ = "sprints"

    id = Column(Integer, primary_key=True, autoincrement=True)
    sprint_id = Column(String(50), unique=True, nullable=False, index=True)

    # Relacionamento com projeto
    project_id = Column(String(50), ForeignKey("projects.project_id"), nullable=False, index=True)

    # Dados
    name = Column(String(200), nullable=False)
    goal = Column(Text, nullable=True)

    # Periodo
    start_date = Column(DateTime, nullable=True)
    end_date = Column(DateTime, nullable=True)

    # Status
    status = Column(String(30), default="planned")  # planned, active, completed

    # Metricas
    velocity = Column(Integer, default=0)  # story points completados
    capacity = Column(Integer, default=0)  # story points planejados

    # Timestamps
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    def to_dict(self):
        return {
            "sprint_id": self.sprint_id,
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

    def __repr__(self):
        return f"<Sprint {self.sprint_id}: {self.name} [{self.status}]>"


# =============================================================================
# DEPRECATED MODELS (mantidos para compatibilidade durante migracao)
# =============================================================================

# Os modelos abaixo foram removidos na v4.0:
# - Agent: substituido por Worker
# - Skill: MCP tools gerenciados diretamente pelo Claude
# - Template: simplificado (config no projeto)
# - FactoryEvent: merged com ActivityLog

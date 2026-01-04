"""
Pydantic Schemas for API Documentation
======================================

Complete schema definitions for OpenAPI/Swagger documentation.
"""

from datetime import datetime
from typing import Dict, List, Optional, Any
from pydantic import BaseModel, Field, field_validator
from enum import Enum


# =============================================================================
# ENUMS
# =============================================================================

class ProjectStatusEnum(str, Enum):
    """Status do projeto"""
    PLANNING = "PLANNING"
    IN_PROGRESS = "IN_PROGRESS"
    PAUSED = "PAUSED"
    COMPLETED = "COMPLETED"
    ARCHIVED = "ARCHIVED"


class AgentStatusEnum(str, Enum):
    """Status do agente"""
    STANDBY = "STANDBY"
    READY = "READY"
    EXECUTING = "EXECUTING"
    PAUSED = "PAUSED"
    ERROR = "ERROR"


class TaskStatusEnum(str, Enum):
    """Status da tarefa"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class StoryStatusEnum(str, Enum):
    """Status da story"""
    BACKLOG = "BACKLOG"
    TO_DO = "TO_DO"
    IN_PROGRESS = "IN_PROGRESS"
    TESTING = "TESTING"
    BLOCKED = "BLOCKED"
    DONE = "DONE"


class SkillTypeEnum(str, Enum):
    """Tipo de skill"""
    CORE = "core"
    MCP = "mcp"
    VESSEL = "vessel"
    CUSTOM = "custom"


class UserRoleEnum(str, Enum):
    """Role do usuario"""
    ADMIN = "ADMIN"
    MANAGER = "MANAGER"
    VIEWER = "VIEWER"


# =============================================================================
# REQUEST SCHEMAS
# =============================================================================

class LoginRequest(BaseModel):
    """Request de login"""
    username: str = Field(..., description="Nome de usuario", min_length=3)
    password: str = Field(..., description="Senha", min_length=6)

    class Config:
        json_schema_extra = {
            "example": {
                "username": "admin",
                "password": "admin123"
            }
        }


class ProjectCreate(BaseModel):
    """Dados para criar um projeto"""
    name: str = Field(..., description="Nome do projeto", min_length=1, max_length=200)
    description: Optional[str] = Field(None, description="Descricao do projeto")
    project_type: str = Field(..., description="Tipo do projeto (web-app, api-service, data-analysis)")
    template_id: Optional[str] = Field(None, description="ID do template a usar")
    config: Optional[Dict[str, Any]] = Field(None, description="Configuracoes adicionais")

    class Config:
        json_schema_extra = {
            "example": {
                "name": "Meu Novo Projeto",
                "description": "Um projeto de exemplo",
                "project_type": "web-app",
                "template_id": None,
                "config": {"framework": "fastapi"}
            }
        }


class ProjectUpdate(BaseModel):
    """Dados para atualizar um projeto"""
    name: Optional[str] = Field(None, description="Nome do projeto")
    description: Optional[str] = Field(None, description="Descricao do projeto")
    status: Optional[ProjectStatusEnum] = Field(None, description="Status do projeto")
    progress: Optional[float] = Field(None, ge=0, le=100, description="Progresso (0-100)")

    class Config:
        json_schema_extra = {
            "example": {
                "name": "Projeto Atualizado",
                "status": "IN_PROGRESS",
                "progress": 50.0
            }
        }


class StoryCreate(BaseModel):
    """
    Dados para criar uma user story

    Issues #495-498, #518-521: Validacao de campos
    - #518: title nao pode ser vazio ou apenas espacos
    - #519: title tem max_length=500 caracteres
    - #520: campos obrigatorios nao aceitam null
    - #521: points deve ser >= 0 (valores positivos)
    - points: valores Fibonacci (0,1,2,3,5,8,13,21)
    """
    title: str = Field(..., description="Titulo da story (1-500 caracteres)", min_length=1, max_length=500)
    description: Optional[str] = Field(None, description="Descricao detalhada (max 5000 caracteres)", max_length=5000)
    project_id: Optional[str] = Field(None, description="ID do projeto")
    sprint: int = Field(1, ge=1, description="Numero do sprint")
    points: int = Field(0, ge=0, description="Story points (Fibonacci: 0,1,2,3,5,8,13,21, valores positivos)")
    priority: int = Field(5, ge=1, le=9, description="Prioridade (1=critica, 9=backlog)")
    narrative_persona: Optional[str] = Field(None, description="Como [tipo de usuario]")
    narrative_action: Optional[str] = Field(None, description="Eu quero [acao]")
    narrative_benefit: Optional[str] = Field(None, description="Para que [beneficio]")
    acceptance_criteria: Optional[List[str]] = Field(None, description="Criterios de aceite")

    @field_validator('points')
    @classmethod
    def validate_fibonacci_points(cls, v):
        """Issues #498, #521: Valida que points seja um valor Fibonacci valido e positivo"""
        if v is None:
            return 0
        if not isinstance(v, int):
            raise ValueError('points deve ser um numero inteiro')
        if v < 0:
            raise ValueError('points deve ser maior ou igual a 0 (valores negativos nao sao permitidos)')
        valid_points = [0, 1, 2, 3, 5, 8, 13, 21]
        if v not in valid_points:
            raise ValueError(f'points deve ser um valor Fibonacci: {valid_points}')
        return v

    @field_validator('title')
    @classmethod
    def validate_title_not_empty(cls, v):
        """Issues #495, #518, #520: Valida que titulo nao seja vazio, null ou apenas espacos"""
        if v is None:
            raise ValueError('title e obrigatorio e nao pode ser null')
        if not isinstance(v, str):
            raise ValueError('title deve ser uma string')
        if v.strip() == '':
            raise ValueError('title nao pode ser vazio ou conter apenas espacos')
        return v.strip()

    class Config:
        json_schema_extra = {
            "example": {
                "title": "Implementar login de usuario",
                "description": "Sistema de autenticacao basico",
                "project_id": "PRJ-001",
                "sprint": 1,
                "points": 5,
                "priority": 3,
                "narrative_persona": "usuario do sistema",
                "narrative_action": "fazer login com email e senha",
                "narrative_benefit": "acessar funcionalidades protegidas",
                "acceptance_criteria": [
                    "DADO que estou na pagina de login QUANDO informo credenciais validas ENTAO sou redirecionado ao dashboard",
                    "DADO que estou na pagina de login QUANDO informo credenciais invalidas ENTAO vejo mensagem de erro"
                ]
            }
        }


class StoryUpdate(BaseModel):
    """
    Dados para atualizar uma story

    Issues #495-498, #518-521: Validacao de campos
    - #518: title nao pode ser vazio ou apenas espacos (se fornecido)
    - #519: title tem max_length=500 caracteres
    - #521: points deve ser >= 0 (valores positivos)
    - points: valores Fibonacci (0,1,2,3,5,8,13,21)
    """
    title: Optional[str] = Field(None, description="Titulo da story (1-500 caracteres)", min_length=1, max_length=500)
    description: Optional[str] = Field(None, description="Descricao (max 5000 caracteres)", max_length=5000)
    status: Optional[StoryStatusEnum] = Field(None, description="Status")
    sprint: Optional[int] = Field(None, ge=1, description="Sprint")
    points: Optional[int] = Field(None, ge=0, description="Story points (Fibonacci: 0,1,2,3,5,8,13,21, valores positivos)")
    priority: Optional[int] = Field(None, ge=1, le=9, description="Prioridade")

    @field_validator('points')
    @classmethod
    def validate_fibonacci_points(cls, v):
        """Issues #498, #521: Valida que points seja um valor Fibonacci valido e positivo"""
        if v is None:
            return v  # None e permitido em update
        if not isinstance(v, int):
            raise ValueError('points deve ser um numero inteiro')
        if v < 0:
            raise ValueError('points deve ser maior ou igual a 0 (valores negativos nao sao permitidos)')
        valid_points = [0, 1, 2, 3, 5, 8, 13, 21]
        if v not in valid_points:
            raise ValueError(f'points deve ser um valor Fibonacci: {valid_points}')
        return v

    @field_validator('title')
    @classmethod
    def validate_title_not_empty(cls, v):
        """Issues #495, #518: Valida que titulo nao seja vazio ou apenas espacos"""
        if v is None:
            return v  # None e permitido em update (significa nao atualizar)
        if not isinstance(v, str):
            raise ValueError('title deve ser uma string')
        if v.strip() == '':
            raise ValueError('title nao pode ser vazio ou conter apenas espacos')
        return v.strip()


class AgentUpdate(BaseModel):
    """Dados para atualizar um agente"""
    status: Optional[AgentStatusEnum] = Field(None, description="Status do agente")
    priority: Optional[int] = Field(None, ge=1, le=10, description="Prioridade")
    enabled: Optional[bool] = Field(None, description="Agente habilitado")


class SprintCreate(BaseModel):
    """Dados para criar um sprint"""
    project_id: str = Field(..., description="ID do projeto")
    sprint_number: int = Field(..., ge=1, description="Numero do sprint")
    name: Optional[str] = Field(None, description="Nome do sprint")
    goal: Optional[str] = Field(None, description="Objetivo do sprint")
    start_date: Optional[str] = Field(None, description="Data de inicio (ISO format)")
    end_date: Optional[str] = Field(None, description="Data de fim (ISO format)")


class SprintUpdate(BaseModel):
    """Dados para atualizar um sprint"""
    name: Optional[str] = Field(None, description="Nome")
    goal: Optional[str] = Field(None, description="Objetivo")
    status: Optional[str] = Field(None, description="Status (planned, active, completed)")
    planned_points: Optional[int] = Field(None, ge=0, description="Pontos planejados")


class TaskCreate(BaseModel):
    """Dados para criar uma tarefa"""
    task_type: str = Field(..., description="Tipo da tarefa")
    project_id: Optional[str] = Field(None, description="ID do projeto")
    agent_id: Optional[str] = Field(None, description="ID do agente")
    story_id: Optional[str] = Field(None, description="ID da story")
    title: Optional[str] = Field(None, description="Titulo")
    description: Optional[str] = Field(None, description="Descricao")
    priority: int = Field(5, ge=1, le=10, description="Prioridade")


# =============================================================================
# RESPONSE SCHEMAS
# =============================================================================

class ProjectResponse(BaseModel):
    """Resposta com dados do projeto"""
    project_id: str = Field(..., description="ID unico do projeto")
    name: str = Field(..., description="Nome do projeto")
    description: Optional[str] = Field(None, description="Descricao")
    project_type: str = Field(..., description="Tipo do projeto")
    template_used: Optional[str] = Field(None, description="Template utilizado")
    status: str = Field(..., description="Status atual")
    progress: float = Field(..., description="Progresso (0-100)")
    folder_path: Optional[str] = Field(None, description="Caminho da pasta")
    github_url: Optional[str] = Field(None, description="URL do repositorio GitHub")
    config: Dict[str, Any] = Field(default_factory=dict, description="Configuracoes")
    tags: List[str] = Field(default_factory=list, description="Tags")
    created_at: Optional[str] = Field(None, description="Data de criacao")
    updated_at: Optional[str] = Field(None, description="Data de atualizacao")
    stories_count: int = Field(0, description="Numero de stories")
    tasks_count: int = Field(0, description="Numero de tasks")
    sprints_count: int = Field(0, description="Numero de sprints")


class StoryResponse(BaseModel):
    """Resposta com dados da story"""
    story_id: str = Field(..., description="ID da story")
    project_id: str = Field(..., description="ID do projeto")
    title: str = Field(..., description="Titulo")
    description: Optional[str] = Field(None, description="Descricao")
    status: str = Field(..., description="Status")
    sprint: int = Field(..., description="Sprint")
    points: int = Field(..., description="Story points")
    priority: int = Field(..., description="Prioridade")
    narrative: Dict[str, str] = Field(default_factory=dict, description="Narrativa")
    acceptance_criteria: List[str] = Field(default_factory=list, description="Criterios")
    created_at: Optional[str] = Field(None, description="Data de criacao")


class AgentResponse(BaseModel):
    """Resposta com dados do agente"""
    agent_id: str = Field(..., description="ID do agente")
    name: str = Field(..., description="Nome")
    role: Optional[str] = Field(None, description="Funcao")
    description: Optional[str] = Field(None, description="Descricao")
    domain: Optional[str] = Field(None, description="Dominio")
    status: str = Field(..., description="Status atual")
    capabilities: List[str] = Field(default_factory=list, description="Capacidades")
    skills: List[str] = Field(default_factory=list, description="Skills")
    enabled: bool = Field(..., description="Habilitado")
    metrics: Dict[str, int] = Field(default_factory=dict, description="Metricas")


class SkillResponse(BaseModel):
    """Resposta com dados da skill"""
    skill_id: str = Field(..., description="ID da skill")
    name: str = Field(..., description="Nome")
    description: Optional[str] = Field(None, description="Descricao")
    skill_type: str = Field(..., description="Tipo")
    category: Optional[str] = Field(None, description="Categoria")
    enabled: bool = Field(..., description="Habilitada")
    version: str = Field(..., description="Versao")


class SprintResponse(BaseModel):
    """Resposta com dados do sprint"""
    id: int = Field(..., description="ID interno")
    project_id: str = Field(..., description="ID do projeto")
    sprint_number: int = Field(..., description="Numero do sprint")
    name: str = Field(..., description="Nome")
    status: str = Field(..., description="Status")
    goal: Optional[str] = Field(None, description="Objetivo")
    planned_points: int = Field(0, description="Pontos planejados")
    completed_points: int = Field(0, description="Pontos completados")
    velocity: float = Field(0, description="Velocidade")


class TaskResponse(BaseModel):
    """Resposta com dados da tarefa"""
    task_id: str = Field(..., description="ID da tarefa")
    task_type: str = Field(..., description="Tipo")
    project_id: Optional[str] = Field(None, description="ID do projeto")
    agent_id: Optional[str] = Field(None, description="ID do agente")
    status: str = Field(..., description="Status")
    priority: int = Field(..., description="Prioridade")
    created_at: Optional[str] = Field(None, description="Data de criacao")


class UserResponse(BaseModel):
    """Resposta com dados do usuario (sem senha)"""
    id: int = Field(..., description="ID interno")
    username: str = Field(..., description="Nome de usuario")
    email: Optional[str] = Field(None, description="Email")
    role: str = Field(..., description="Role")
    active: bool = Field(..., description="Ativo")
    created_at: Optional[str] = Field(None, description="Data de criacao")
    last_login: Optional[str] = Field(None, description="Ultimo login")


class LogResponse(BaseModel):
    """Resposta com dados do log"""
    id: int = Field(..., description="ID")
    source: str = Field(..., description="Fonte")
    level: str = Field(..., description="Nivel")
    event_type: str = Field(..., description="Tipo de evento")
    message: str = Field(..., description="Mensagem")
    timestamp: str = Field(..., description="Timestamp")


class TemplateResponse(BaseModel):
    """Resposta com dados do template"""
    template_id: str = Field(..., description="ID")
    name: str = Field(..., description="Nome")
    description: Optional[str] = Field(None, description="Descricao")
    project_type: str = Field(..., description="Tipo de projeto")
    category: Optional[str] = Field(None, description="Categoria")
    stack: Dict[str, str] = Field(default_factory=dict, description="Stack tecnologica")


# =============================================================================
# LIST RESPONSE SCHEMAS
# =============================================================================

class ProjectListResponse(BaseModel):
    """Lista de projetos"""
    projects: List[ProjectResponse] = Field(..., description="Lista de projetos")


class StoryListResponse(BaseModel):
    """Lista de stories"""
    stories: List[StoryResponse] = Field(..., description="Lista de stories")


class AgentListResponse(BaseModel):
    """Lista de agentes"""
    agents: List[AgentResponse] = Field(..., description="Lista de agentes")


class SkillListResponse(BaseModel):
    """Lista de skills"""
    skills: List[SkillResponse] = Field(..., description="Lista de skills")


class SprintListResponse(BaseModel):
    """Lista de sprints"""
    sprints: List[SprintResponse] = Field(..., description="Lista de sprints")


class LogListResponse(BaseModel):
    """Lista de logs"""
    logs: List[LogResponse] = Field(..., description="Lista de logs")


class TemplateListResponse(BaseModel):
    """Lista de templates"""
    templates: List[TemplateResponse] = Field(..., description="Lista de templates")


# =============================================================================
# SPECIAL RESPONSE SCHEMAS
# =============================================================================

class LoginResponse(BaseModel):
    """Resposta de login"""
    success: bool = Field(..., description="Login bem-sucedido")
    token: str = Field(..., description="JWT token")
    user: UserResponse = Field(..., description="Dados do usuario")


class SuccessResponse(BaseModel):
    """Resposta de sucesso generica"""
    success: bool = Field(True, description="Operacao bem-sucedida")
    message: str = Field(..., description="Mensagem")


class ErrorResponse(BaseModel):
    """Resposta de erro"""
    detail: str = Field(..., description="Detalhes do erro")


class FactoryStatus(BaseModel):
    """Status da fabrica"""
    name: str = Field(..., description="Nome da fabrica")
    version: str = Field(..., description="Versao")
    status: str = Field(..., description="Status (running, stopped, error)")


class ProjectStats(BaseModel):
    """Estatisticas de projetos"""
    total: int = Field(..., description="Total de projetos")
    by_status: Dict[str, int] = Field(..., description="Projetos por status")


class AgentStats(BaseModel):
    """Estatisticas de agentes"""
    total: int = Field(..., description="Total de agentes")
    by_status: Dict[str, int] = Field(..., description="Agentes por status")


class StatusResponse(BaseModel):
    """Resposta do endpoint /api/status"""
    factory: FactoryStatus = Field(..., description="Status da fabrica")
    projects: Dict[str, Any] = Field(..., description="Info de projetos")
    agents: Dict[str, Any] = Field(..., description="Info de agentes")
    skills: Dict[str, int] = Field(..., description="Info de skills")
    recent_logs: List[LogResponse] = Field(..., description="Logs recentes")
    timestamp: str = Field(..., description="Timestamp da resposta")


class ProjectCreateResponse(BaseModel):
    """Resposta da criacao de projeto"""
    success: bool = Field(True, description="Sucesso")
    project: ProjectResponse = Field(..., description="Projeto criado")


class StoryCreateResponse(BaseModel):
    """Resposta da criacao de story"""
    success: bool = Field(True, description="Sucesso")
    story: StoryResponse = Field(..., description="Story criada")


class SprintCreateResponse(BaseModel):
    """Resposta da criacao de sprint"""
    success: bool = Field(True, description="Sucesso")
    sprint: SprintResponse = Field(..., description="Sprint criado")

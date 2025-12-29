"""
API v1 Schemas - Fabrica de Agentes
===================================

Schemas padrao para API v1 incluindo:
- APIResponse: Resposta padrao com data, meta e pagination
- PaginationMeta: Metadados de paginacao cursor-based
- CursorPaginationParams: Parametros de paginacao
- Modelos de erro padronizados
"""

from datetime import datetime
from typing import Any, Generic, List, Optional, TypeVar
from pydantic import BaseModel, Field
import uuid

# Type variable para dados genericos
T = TypeVar("T")


class PaginationMeta(BaseModel):
    """
    Metadados de paginacao baseada em cursor.

    Campos:
        cursor: Cursor opaco para proxima pagina (base64 encoded)
        has_more: Se existem mais resultados
        total_count: Contagem total (opcional, pode ser caro calcular)
        limit: Limite de itens por pagina
    """
    cursor: Optional[str] = Field(None, description="Cursor para proxima pagina")
    has_more: bool = Field(False, description="Se existem mais resultados")
    total_count: Optional[int] = Field(None, description="Total de itens (quando disponivel)")
    limit: int = Field(20, description="Limite de itens por pagina")

    class Config:
        json_schema_extra = {
            "example": {
                "cursor": "eyJsYXN0X2lkIjogImFiYzEyMyIsICJkaXJlY3Rpb24iOiAibmV4dCJ9",
                "has_more": True,
                "total_count": 150,
                "limit": 20
            }
        }


class RequestMeta(BaseModel):
    """
    Metadados da requisicao incluidos em toda resposta.

    Campos:
        request_id: ID unico da requisicao para rastreamento
        timestamp: Timestamp da resposta
        tenant_id: ID do tenant (multi-tenant)
        api_version: Versao da API
    """
    request_id: str = Field(default_factory=lambda: str(uuid.uuid4()), description="ID unico da requisicao")
    timestamp: datetime = Field(default_factory=datetime.utcnow, description="Timestamp da resposta")
    tenant_id: Optional[str] = Field(None, description="ID do tenant")
    api_version: str = Field("v1", description="Versao da API")

    class Config:
        json_schema_extra = {
            "example": {
                "request_id": "550e8400-e29b-41d4-a716-446655440000",
                "timestamp": "2024-12-29T10:30:00Z",
                "tenant_id": "tenant_001",
                "api_version": "v1"
            }
        }


class APIResponse(BaseModel, Generic[T]):
    """
    Resposta padrao da API v1.

    Todas as respostas da API seguem este formato para consistencia.

    Campos:
        data: Dados da resposta (pode ser qualquer tipo)
        meta: Metadados da requisicao
        pagination: Metadados de paginacao (quando aplicavel)
        success: Indica se a operacao foi bem sucedida
        message: Mensagem opcional
    """
    data: Any = Field(..., description="Dados da resposta")
    meta: RequestMeta = Field(default_factory=RequestMeta, description="Metadados da requisicao")
    pagination: Optional[PaginationMeta] = Field(None, description="Metadados de paginacao")
    success: bool = Field(True, description="Indica sucesso da operacao")
    message: Optional[str] = Field(None, description="Mensagem opcional")

    class Config:
        json_schema_extra = {
            "example": {
                "data": {"story_id": "STR-001", "title": "Implementar login"},
                "meta": {
                    "request_id": "550e8400-e29b-41d4-a716-446655440000",
                    "timestamp": "2024-12-29T10:30:00Z",
                    "tenant_id": "tenant_001",
                    "api_version": "v1"
                },
                "pagination": None,
                "success": True,
                "message": None
            }
        }


class APIListResponse(BaseModel, Generic[T]):
    """
    Resposta padrao para listas paginadas.
    """
    data: List[Any] = Field(default_factory=list, description="Lista de itens")
    meta: RequestMeta = Field(default_factory=RequestMeta, description="Metadados da requisicao")
    pagination: PaginationMeta = Field(default_factory=PaginationMeta, description="Metadados de paginacao")
    success: bool = Field(True, description="Indica sucesso da operacao")

    class Config:
        json_schema_extra = {
            "example": {
                "data": [
                    {"story_id": "STR-001", "title": "Story 1"},
                    {"story_id": "STR-002", "title": "Story 2"}
                ],
                "meta": {
                    "request_id": "550e8400-e29b-41d4-a716-446655440000",
                    "timestamp": "2024-12-29T10:30:00Z",
                    "api_version": "v1"
                },
                "pagination": {
                    "cursor": "eyJsYXN0X2lkIjogIlNUUi0wMDIifQ==",
                    "has_more": True,
                    "limit": 20
                },
                "success": True
            }
        }


class APIError(BaseModel):
    """
    Resposta de erro padronizada.

    Campos:
        error_code: Codigo de erro unico
        message: Mensagem de erro legivel
        details: Detalhes adicionais do erro
        field_errors: Erros de validacao por campo
    """
    error_code: str = Field(..., description="Codigo de erro unico")
    message: str = Field(..., description="Mensagem de erro")
    details: Optional[dict] = Field(None, description="Detalhes adicionais")
    field_errors: Optional[dict] = Field(None, description="Erros por campo")
    meta: RequestMeta = Field(default_factory=RequestMeta, description="Metadados da requisicao")
    success: bool = Field(False)

    class Config:
        json_schema_extra = {
            "example": {
                "error_code": "VALIDATION_ERROR",
                "message": "Erro de validacao nos dados enviados",
                "details": {"reason": "Campo obrigatorio ausente"},
                "field_errors": {"title": "Campo obrigatorio"},
                "meta": {
                    "request_id": "550e8400-e29b-41d4-a716-446655440000",
                    "timestamp": "2024-12-29T10:30:00Z"
                },
                "success": False
            }
        }


class CursorPaginationParams(BaseModel):
    """
    Parametros de paginacao baseada em cursor.

    Campos:
        cursor: Cursor da pagina anterior (base64)
        limit: Numero de itens por pagina (1-100)
        direction: Direcao da paginacao (next/prev)
    """
    cursor: Optional[str] = Field(None, description="Cursor da pagina anterior")
    limit: int = Field(20, ge=1, le=100, description="Itens por pagina")
    direction: str = Field("next", pattern="^(next|prev)$", description="Direcao da paginacao")

    class Config:
        json_schema_extra = {
            "example": {
                "cursor": "eyJsYXN0X2lkIjogIlNUUi0wMTAifQ==",
                "limit": 20,
                "direction": "next"
            }
        }


# Codigos de erro padrao
class ErrorCodes:
    """Codigos de erro padronizados da API"""

    # Erros de validacao (400)
    VALIDATION_ERROR = "VALIDATION_ERROR"
    INVALID_CURSOR = "INVALID_CURSOR"
    INVALID_REQUEST = "INVALID_REQUEST"

    # Erros de autenticacao (401)
    UNAUTHORIZED = "UNAUTHORIZED"
    INVALID_TOKEN = "INVALID_TOKEN"
    TOKEN_EXPIRED = "TOKEN_EXPIRED"

    # Erros de autorizacao (403)
    FORBIDDEN = "FORBIDDEN"
    INSUFFICIENT_PERMISSIONS = "INSUFFICIENT_PERMISSIONS"
    QUOTA_EXCEEDED = "QUOTA_EXCEEDED"

    # Erros de recurso (404)
    NOT_FOUND = "NOT_FOUND"
    RESOURCE_NOT_FOUND = "RESOURCE_NOT_FOUND"

    # Erros de conflito (409)
    CONFLICT = "CONFLICT"
    DUPLICATE_RESOURCE = "DUPLICATE_RESOURCE"
    IDEMPOTENCY_CONFLICT = "IDEMPOTENCY_CONFLICT"

    # Erros de rate limit (429)
    RATE_LIMIT_EXCEEDED = "RATE_LIMIT_EXCEEDED"

    # Erros internos (500)
    INTERNAL_ERROR = "INTERNAL_ERROR"
    DATABASE_ERROR = "DATABASE_ERROR"
    EXTERNAL_SERVICE_ERROR = "EXTERNAL_SERVICE_ERROR"


# Schemas para recursos especificos

class StoryCreate(BaseModel):
    """Schema para criacao de Story"""
    title: str = Field(..., min_length=1, max_length=300, description="Titulo da story")
    description: Optional[str] = Field(None, description="Descricao detalhada")
    project_id: str = Field(..., description="ID do projeto")
    persona: Optional[str] = Field(None, max_length=200, description="Como um [usuario]")
    action: Optional[str] = Field(None, description="Eu quero [funcionalidade]")
    benefit: Optional[str] = Field(None, description="Para que [beneficio]")
    acceptance_criteria: Optional[List[str]] = Field(default_factory=list, description="Criterios de aceite")
    story_points: Optional[int] = Field(None, ge=0, le=21, description="Story points (Fibonacci)")
    priority: Optional[str] = Field("medium", description="Prioridade: low, medium, high, urgent")
    epic_id: Optional[str] = Field(None, description="ID do epic")
    sprint_id: Optional[str] = Field(None, description="ID do sprint")

    class Config:
        json_schema_extra = {
            "example": {
                "title": "Implementar autenticacao OAuth",
                "description": "Sistema de login usando OAuth 2.0",
                "project_id": "PRJ-001",
                "persona": "usuario do sistema",
                "action": "fazer login com minha conta Google",
                "benefit": "nao precisar criar outra senha",
                "acceptance_criteria": [
                    "DADO que estou na pagina de login QUANDO clico em 'Login com Google' ENTAO sou redirecionado ao Google",
                    "DADO que autorizei no Google QUANDO retorno ao sistema ENTAO estou logado"
                ],
                "story_points": 5,
                "priority": "high"
            }
        }


class StoryUpdate(BaseModel):
    """Schema para atualizacao de Story"""
    title: Optional[str] = Field(None, min_length=1, max_length=300)
    description: Optional[str] = None
    persona: Optional[str] = Field(None, max_length=200)
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: Optional[List[str]] = None
    story_points: Optional[int] = Field(None, ge=0, le=21)
    priority: Optional[str] = None
    status: Optional[str] = None
    assignee: Optional[str] = None
    epic_id: Optional[str] = None
    sprint_id: Optional[str] = None


class TaskCreate(BaseModel):
    """Schema para criacao de Task"""
    title: str = Field(..., min_length=1, max_length=200, description="Titulo da task")
    description: Optional[str] = Field(None, description="Descricao")
    story_id: str = Field(..., description="ID da story pai")
    task_type: str = Field("development", description="Tipo: development, review, test, documentation, design")
    estimated_hours: Optional[float] = Field(None, ge=0, description="Horas estimadas")
    assignee: Optional[str] = Field(None, description="Responsavel")


class TaskUpdate(BaseModel):
    """Schema para atualizacao de Task"""
    title: Optional[str] = Field(None, min_length=1, max_length=200)
    description: Optional[str] = None
    task_type: Optional[str] = None
    status: Optional[str] = None
    progress: Optional[int] = Field(None, ge=0, le=100)
    estimated_hours: Optional[float] = Field(None, ge=0)
    actual_hours: Optional[float] = Field(None, ge=0)
    assignee: Optional[str] = None


class ProjectCreate(BaseModel):
    """Schema para criacao de Projeto"""
    name: str = Field(..., min_length=1, max_length=200, description="Nome do projeto")
    description: Optional[str] = Field(None, description="Descricao")
    project_type: str = Field("web-app", description="Tipo: web-app, api-service, data-analysis, automation")
    config: Optional[dict] = Field(default_factory=dict, description="Configuracoes do projeto")
    tags: Optional[List[str]] = Field(default_factory=list, description="Tags")


class ProjectUpdate(BaseModel):
    """Schema para atualizacao de Projeto"""
    name: Optional[str] = Field(None, min_length=1, max_length=200)
    description: Optional[str] = None
    status: Optional[str] = None
    progress: Optional[float] = Field(None, ge=0, le=100)
    config: Optional[dict] = None
    tags: Optional[List[str]] = None


# Schemas para filtros e busca

class StoryFilters(BaseModel):
    """Filtros para busca de Stories"""
    project_id: Optional[str] = None
    status: Optional[str] = None
    priority: Optional[str] = None
    assignee: Optional[str] = None
    epic_id: Optional[str] = None
    sprint_id: Optional[str] = None
    search: Optional[str] = Field(None, description="Busca textual em titulo/descricao")
    min_points: Optional[int] = Field(None, ge=0)
    max_points: Optional[int] = Field(None, le=21)
    created_after: Optional[datetime] = None
    created_before: Optional[datetime] = None


class TaskFilters(BaseModel):
    """Filtros para busca de Tasks"""
    story_id: Optional[str] = None
    status: Optional[str] = None
    task_type: Optional[str] = None
    assignee: Optional[str] = None
    search: Optional[str] = None


class ProjectFilters(BaseModel):
    """Filtros para busca de Projetos"""
    status: Optional[str] = None
    project_type: Optional[str] = None
    search: Optional[str] = None
    tags: Optional[List[str]] = None

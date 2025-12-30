# -*- coding: utf-8 -*-
"""
Approval Routes - API REST para Workflow de Aprovacao
=======================================================

Issue #282 - [APPROVAL] Workflow de aprovacao configuravel

Este modulo define as rotas REST para o sistema de aprovacao:

Workflows:
    GET    /api/v1/approval/workflows              - Listar workflows
    POST   /api/v1/approval/workflows              - Criar workflow
    GET    /api/v1/approval/workflows/{id}         - Buscar workflow
    PUT    /api/v1/approval/workflows/{id}         - Atualizar workflow
    DELETE /api/v1/approval/workflows/{id}         - Deletar workflow
    POST   /api/v1/approval/workflows/{id}/stages  - Adicionar estagio

Requests:
    GET    /api/v1/approval/requests               - Listar solicitacoes
    POST   /api/v1/approval/requests               - Submeter para aprovacao
    GET    /api/v1/approval/requests/{id}          - Buscar solicitacao
    GET    /api/v1/approval/requests/{id}/status   - Status detalhado
    POST   /api/v1/approval/requests/{id}/cancel   - Cancelar solicitacao

Actions:
    POST   /api/v1/approval/requests/{id}/approve  - Aprovar
    POST   /api/v1/approval/requests/{id}/reject   - Rejeitar
    POST   /api/v1/approval/requests/{id}/request-changes - Solicitar mudancas

User:
    GET    /api/v1/approval/pending                - Aprovacoes pendentes do usuario
"""

from datetime import datetime
from typing import Optional, List, Dict, Any

from fastapi import APIRouter, HTTPException, Depends, Query
from pydantic import BaseModel, Field

# Auth
from factory.api.auth import get_current_user, TokenData

# Approval Workflow Service
from factory.core.approval_workflow import (
    get_approval_service,
    ApprovalWorkflowService,
    ApprovalWorkflow,
    ApprovalStage,
    ApprovalRule,
    ApprovalStageType,
    ApprovalRuleType,
    ApprovalStatus,
    create_default_story_workflow,
    create_simple_approval_workflow
)


# =============================================================================
# ROUTER
# =============================================================================

router = APIRouter(prefix="/api/v1/approval", tags=["approval"])


# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class ApprovalRuleSchema(BaseModel):
    """Schema para regra de aprovacao"""
    rule_type: str = Field("any_one", description="Tipo: any_one, all, threshold, specific_user, role_based")
    required_roles: List[str] = Field(default=[], description="Roles requeridas para aprovacao")
    required_users: List[str] = Field(default=[], description="Usuarios especificos requeridos")
    threshold_percentage: float = Field(50.0, ge=0, le=100, description="Porcentagem minima de aprovadores")
    min_approvers: int = Field(1, ge=1, description="Minimo de aprovadores")
    allow_self_approval: bool = Field(False, description="Permite auto-aprovacao")
    require_comment: bool = Field(False, description="Comentario obrigatorio")


class ApprovalStageSchema(BaseModel):
    """Schema para estagio de aprovacao"""
    name: str = Field(..., min_length=1, max_length=200, description="Nome do estagio")
    stage_type: str = Field("review", description="Tipo: review, qa, security, management, compliance, custom")
    order: Optional[int] = Field(None, ge=1, description="Ordem do estagio")
    eligible_approvers: List[str] = Field(default=[], description="IDs de usuarios elegiveis")
    eligible_roles: List[str] = Field(default=[], description="Roles elegiveis")
    rules: List[ApprovalRuleSchema] = Field(default=[], description="Regras do estagio")
    is_optional: bool = Field(False, description="Estagio opcional")
    timeout_hours: Optional[int] = Field(None, ge=1, description="Timeout em horas")
    notify_on_enter: bool = Field(True, description="Notificar ao entrar no estagio")
    notify_on_complete: bool = Field(True, description="Notificar ao completar")


class WorkflowCreate(BaseModel):
    """Schema para criacao de workflow"""
    name: str = Field(..., min_length=1, max_length=200, description="Nome do workflow")
    description: str = Field("", max_length=1000, description="Descricao do workflow")
    resource_type: str = Field("story", description="Tipo de recurso: story, task, project")
    stages: List[ApprovalStageSchema] = Field(default=[], description="Estagios do workflow")
    config: Dict[str, Any] = Field(default={}, description="Configuracoes adicionais")


class WorkflowUpdate(BaseModel):
    """Schema para atualizacao de workflow"""
    name: Optional[str] = Field(None, min_length=1, max_length=200)
    description: Optional[str] = Field(None, max_length=1000)
    is_active: Optional[bool] = None
    config: Optional[Dict[str, Any]] = None


class SubmitForApprovalRequest(BaseModel):
    """Schema para submissao de aprovacao"""
    workflow_id: str = Field(..., description="ID do workflow a usar")
    resource_type: str = Field("story", description="Tipo de recurso")
    resource_id: str = Field(..., description="ID do recurso")
    title: str = Field("", max_length=300, description="Titulo da solicitacao")
    description: str = Field("", max_length=2000, description="Descricao/justificativa")
    priority: str = Field("normal", description="Prioridade: low, normal, high, urgent")


class ApprovalActionRequest(BaseModel):
    """Schema para acao de aprovacao"""
    comment: Optional[str] = Field(None, max_length=2000, description="Comentario")


class RejectRequest(BaseModel):
    """Schema para rejeicao"""
    comment: str = Field(..., min_length=1, max_length=2000, description="Motivo da rejeicao")


class CancelRequest(BaseModel):
    """Schema para cancelamento"""
    reason: str = Field("", max_length=1000, description="Motivo do cancelamento")


# =============================================================================
# RESPONSE SCHEMAS
# =============================================================================

class WorkflowResponse(BaseModel):
    """Response de workflow"""
    workflow_id: str
    name: str
    description: str
    resource_type: str
    is_active: bool
    stages_count: int
    created_at: Optional[str]
    updated_at: Optional[str]


class WorkflowDetailResponse(BaseModel):
    """Response detalhado de workflow"""
    workflow_id: str
    name: str
    description: str
    resource_type: str
    is_active: bool
    stages: List[Dict[str, Any]]
    config: Dict[str, Any]
    created_at: Optional[str]
    updated_at: Optional[str]
    created_by: Optional[str]
    version: int


class ApprovalRequestResponse(BaseModel):
    """Response de solicitacao de aprovacao"""
    request_id: str
    workflow_id: str
    resource_type: str
    resource_id: str
    status: str
    current_stage_id: Optional[str]
    title: str
    priority: str
    submitter_id: str
    created_at: Optional[str]


class ApprovalStatusResponse(BaseModel):
    """Response de status detalhado"""
    request: Dict[str, Any]
    workflow: Dict[str, Any]
    current_stage: Optional[Dict[str, Any]]
    stages: List[Dict[str, Any]]
    progress: Dict[str, Any]


# =============================================================================
# DEPENDENCIES
# =============================================================================

def get_service() -> ApprovalWorkflowService:
    """Dependency para obter o servico de aprovacao"""
    return get_approval_service()


# =============================================================================
# WORKFLOW ROUTES
# =============================================================================

@router.get("/workflows", response_model=List[WorkflowResponse])
async def list_workflows(
    resource_type: Optional[str] = Query(None, description="Filtrar por tipo de recurso"),
    is_active: Optional[bool] = Query(None, description="Filtrar por status ativo"),
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Lista todos os workflows disponiveis.

    Filtros:
    - resource_type: Tipo de recurso (story, task, project)
    - is_active: Status de ativacao
    """
    workflows = service.get_workflows(
        tenant_id=current_user.tenant_id,
        resource_type=resource_type,
        is_active=is_active
    )

    return [
        WorkflowResponse(
            workflow_id=w.workflow_id,
            name=w.name,
            description=w.description,
            resource_type=w.resource_type,
            is_active=w.is_active,
            stages_count=len(w.stages),
            created_at=w.created_at.isoformat() if w.created_at else None,
            updated_at=w.updated_at.isoformat() if w.updated_at else None
        )
        for w in workflows
    ]


@router.post("/workflows", response_model=WorkflowDetailResponse, status_code=201)
async def create_workflow(
    data: WorkflowCreate,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Cria um novo workflow de aprovacao.

    Permite definir estagios e regras de aprovacao customizados.
    """
    workflow = service.create_workflow(
        name=data.name,
        description=data.description,
        resource_type=data.resource_type,
        tenant_id=current_user.tenant_id,
        created_by=current_user.username
    )

    # Adicionar estagios
    for stage_data in data.stages:
        stage = ApprovalStage(
            name=stage_data.name,
            stage_type=ApprovalStageType(stage_data.stage_type),
            order=stage_data.order or 1,
            eligible_approvers=stage_data.eligible_approvers,
            eligible_roles=stage_data.eligible_roles,
            is_optional=stage_data.is_optional,
            timeout_hours=stage_data.timeout_hours,
            notify_on_enter=stage_data.notify_on_enter,
            notify_on_complete=stage_data.notify_on_complete
        )

        # Adicionar regras
        for rule_data in stage_data.rules:
            rule = ApprovalRule(
                rule_type=ApprovalRuleType(rule_data.rule_type),
                required_roles=rule_data.required_roles,
                required_users=rule_data.required_users,
                threshold_percentage=rule_data.threshold_percentage,
                min_approvers=rule_data.min_approvers,
                allow_self_approval=rule_data.allow_self_approval,
                require_comment=rule_data.require_comment
            )
            stage.rules.append(rule)

        workflow.add_stage(stage)

    # Aplicar configuracoes adicionais
    if data.config:
        workflow.config.update(data.config)

    return WorkflowDetailResponse(
        workflow_id=workflow.workflow_id,
        name=workflow.name,
        description=workflow.description,
        resource_type=workflow.resource_type,
        is_active=workflow.is_active,
        stages=[s.to_dict() for s in workflow.stages],
        config=workflow.config,
        created_at=workflow.created_at.isoformat() if workflow.created_at else None,
        updated_at=workflow.updated_at.isoformat() if workflow.updated_at else None,
        created_by=workflow.created_by,
        version=workflow.version
    )


@router.post("/workflows/default", response_model=WorkflowDetailResponse, status_code=201)
async def create_default_workflow(
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Cria o workflow padrao para Stories.

    Este workflow inclui 3 estagios:
    1. Code Review
    2. QA Validation
    3. Product Approval
    """
    workflow = create_default_story_workflow(tenant_id=current_user.tenant_id)
    workflow.created_by = current_user.username

    # Registrar no servico
    service._workflows[workflow.workflow_id] = workflow

    return WorkflowDetailResponse(
        workflow_id=workflow.workflow_id,
        name=workflow.name,
        description=workflow.description,
        resource_type=workflow.resource_type,
        is_active=workflow.is_active,
        stages=[s.to_dict() for s in workflow.stages],
        config=workflow.config,
        created_at=workflow.created_at.isoformat() if workflow.created_at else None,
        updated_at=workflow.updated_at.isoformat() if workflow.updated_at else None,
        created_by=workflow.created_by,
        version=workflow.version
    )


@router.get("/workflows/{workflow_id}", response_model=WorkflowDetailResponse)
async def get_workflow(
    workflow_id: str,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Busca um workflow pelo ID.
    """
    workflow = service.get_workflow(workflow_id)

    if not workflow:
        raise HTTPException(status_code=404, detail="Workflow not found")

    # Verificar tenant
    if workflow.tenant_id and workflow.tenant_id != current_user.tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    return WorkflowDetailResponse(
        workflow_id=workflow.workflow_id,
        name=workflow.name,
        description=workflow.description,
        resource_type=workflow.resource_type,
        is_active=workflow.is_active,
        stages=[s.to_dict() for s in workflow.stages],
        config=workflow.config,
        created_at=workflow.created_at.isoformat() if workflow.created_at else None,
        updated_at=workflow.updated_at.isoformat() if workflow.updated_at else None,
        created_by=workflow.created_by,
        version=workflow.version
    )


@router.put("/workflows/{workflow_id}", response_model=WorkflowDetailResponse)
async def update_workflow(
    workflow_id: str,
    data: WorkflowUpdate,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Atualiza um workflow existente.
    """
    workflow = service.get_workflow(workflow_id)

    if not workflow:
        raise HTTPException(status_code=404, detail="Workflow not found")

    if workflow.tenant_id and workflow.tenant_id != current_user.tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    updates = data.dict(exclude_unset=True)
    workflow = service.update_workflow(workflow_id, updates)

    return WorkflowDetailResponse(
        workflow_id=workflow.workflow_id,
        name=workflow.name,
        description=workflow.description,
        resource_type=workflow.resource_type,
        is_active=workflow.is_active,
        stages=[s.to_dict() for s in workflow.stages],
        config=workflow.config,
        created_at=workflow.created_at.isoformat() if workflow.created_at else None,
        updated_at=workflow.updated_at.isoformat() if workflow.updated_at else None,
        created_by=workflow.created_by,
        version=workflow.version
    )


@router.delete("/workflows/{workflow_id}")
async def delete_workflow(
    workflow_id: str,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Deleta um workflow.
    """
    workflow = service.get_workflow(workflow_id)

    if not workflow:
        raise HTTPException(status_code=404, detail="Workflow not found")

    if workflow.tenant_id and workflow.tenant_id != current_user.tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    # Verificar se ha solicitacoes ativas
    active_requests = service.get_requests(
        workflow_id=workflow_id,
        status=ApprovalStatus.IN_PROGRESS
    )

    if active_requests:
        raise HTTPException(
            status_code=400,
            detail=f"Cannot delete workflow with {len(active_requests)} active requests"
        )

    service.delete_workflow(workflow_id)

    return {"message": "Workflow deleted successfully"}


@router.post("/workflows/{workflow_id}/stages", response_model=WorkflowDetailResponse)
async def add_stage_to_workflow(
    workflow_id: str,
    stage_data: ApprovalStageSchema,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Adiciona um novo estagio a um workflow existente.
    """
    workflow = service.get_workflow(workflow_id)

    if not workflow:
        raise HTTPException(status_code=404, detail="Workflow not found")

    if workflow.tenant_id and workflow.tenant_id != current_user.tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    stage = ApprovalStage(
        name=stage_data.name,
        stage_type=ApprovalStageType(stage_data.stage_type),
        order=stage_data.order or len(workflow.stages) + 1,
        eligible_approvers=stage_data.eligible_approvers,
        eligible_roles=stage_data.eligible_roles,
        is_optional=stage_data.is_optional,
        timeout_hours=stage_data.timeout_hours,
        notify_on_enter=stage_data.notify_on_enter,
        notify_on_complete=stage_data.notify_on_complete
    )

    for rule_data in stage_data.rules:
        rule = ApprovalRule(
            rule_type=ApprovalRuleType(rule_data.rule_type),
            required_roles=rule_data.required_roles,
            required_users=rule_data.required_users,
            threshold_percentage=rule_data.threshold_percentage,
            min_approvers=rule_data.min_approvers,
            allow_self_approval=rule_data.allow_self_approval,
            require_comment=rule_data.require_comment
        )
        stage.rules.append(rule)

    workflow.add_stage(stage)

    return WorkflowDetailResponse(
        workflow_id=workflow.workflow_id,
        name=workflow.name,
        description=workflow.description,
        resource_type=workflow.resource_type,
        is_active=workflow.is_active,
        stages=[s.to_dict() for s in workflow.stages],
        config=workflow.config,
        created_at=workflow.created_at.isoformat() if workflow.created_at else None,
        updated_at=workflow.updated_at.isoformat() if workflow.updated_at else None,
        created_by=workflow.created_by,
        version=workflow.version
    )


# =============================================================================
# APPROVAL REQUEST ROUTES
# =============================================================================

@router.get("/requests", response_model=List[ApprovalRequestResponse])
async def list_requests(
    workflow_id: Optional[str] = Query(None, description="Filtrar por workflow"),
    resource_type: Optional[str] = Query(None, description="Filtrar por tipo de recurso"),
    resource_id: Optional[str] = Query(None, description="Filtrar por recurso"),
    status: Optional[str] = Query(None, description="Filtrar por status"),
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Lista solicitacoes de aprovacao.
    """
    status_enum = ApprovalStatus(status) if status else None

    requests = service.get_requests(
        workflow_id=workflow_id,
        resource_type=resource_type,
        resource_id=resource_id,
        status=status_enum,
        tenant_id=current_user.tenant_id
    )

    return [
        ApprovalRequestResponse(
            request_id=r.request_id,
            workflow_id=r.workflow_id,
            resource_type=r.resource_type,
            resource_id=r.resource_id,
            status=r.status.value if hasattr(r.status, 'value') else r.status,
            current_stage_id=r.current_stage_id,
            title=r.title,
            priority=r.priority,
            submitter_id=r.submitter_id,
            created_at=r.created_at.isoformat() if r.created_at else None
        )
        for r in requests
    ]


@router.post("/requests", response_model=ApprovalRequestResponse, status_code=201)
async def submit_for_approval(
    data: SubmitForApprovalRequest,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Submete um recurso para aprovacao.

    Inicia o fluxo de aprovacao no primeiro estagio do workflow.
    """
    request = service.submit_for_approval(
        workflow_id=data.workflow_id,
        resource_type=data.resource_type,
        resource_id=data.resource_id,
        submitter_id=current_user.username,
        title=data.title,
        description=data.description,
        priority=data.priority,
        tenant_id=current_user.tenant_id
    )

    if not request:
        raise HTTPException(status_code=400, detail="Failed to create approval request")

    return ApprovalRequestResponse(
        request_id=request.request_id,
        workflow_id=request.workflow_id,
        resource_type=request.resource_type,
        resource_id=request.resource_id,
        status=request.status.value if hasattr(request.status, 'value') else request.status,
        current_stage_id=request.current_stage_id,
        title=request.title,
        priority=request.priority,
        submitter_id=request.submitter_id,
        created_at=request.created_at.isoformat() if request.created_at else None
    )


@router.get("/requests/{request_id}", response_model=ApprovalRequestResponse)
async def get_request(
    request_id: str,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Busca uma solicitacao de aprovacao pelo ID.
    """
    request = service.get_request(request_id)

    if not request:
        raise HTTPException(status_code=404, detail="Request not found")

    if request.tenant_id and request.tenant_id != current_user.tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    return ApprovalRequestResponse(
        request_id=request.request_id,
        workflow_id=request.workflow_id,
        resource_type=request.resource_type,
        resource_id=request.resource_id,
        status=request.status.value if hasattr(request.status, 'value') else request.status,
        current_stage_id=request.current_stage_id,
        title=request.title,
        priority=request.priority,
        submitter_id=request.submitter_id,
        created_at=request.created_at.isoformat() if request.created_at else None
    )


@router.get("/requests/{request_id}/status", response_model=ApprovalStatusResponse)
async def get_request_status(
    request_id: str,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Retorna o status detalhado de uma solicitacao.

    Inclui informacoes sobre todos os estagios, aprovacoes e progresso.
    """
    request = service.get_request(request_id)

    if not request:
        raise HTTPException(status_code=404, detail="Request not found")

    if request.tenant_id and request.tenant_id != current_user.tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    status = service.get_request_status(request_id)

    if not status:
        raise HTTPException(status_code=404, detail="Status not available")

    return ApprovalStatusResponse(**status)


@router.post("/requests/{request_id}/cancel")
async def cancel_request(
    request_id: str,
    data: CancelRequest,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Cancela uma solicitacao de aprovacao.

    Apenas o submitter pode cancelar a solicitacao.
    """
    result = service.cancel_request(
        request_id=request_id,
        user_id=current_user.username,
        reason=data.reason
    )

    if not result.get("success"):
        raise HTTPException(status_code=400, detail=result.get("error", "Failed to cancel"))

    return {"message": "Request cancelled successfully", "status": result.get("status")}


# =============================================================================
# APPROVAL ACTION ROUTES
# =============================================================================

@router.post("/requests/{request_id}/approve")
async def approve_request(
    request_id: str,
    data: ApprovalActionRequest = ApprovalActionRequest(),
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Aprova uma solicitacao no estagio atual.

    A aprovacao avanca o workflow para o proximo estagio quando
    as regras de aprovacao do estagio atual forem satisfeitas.
    """
    # Obter roles do usuario (simplificado - em producao viria do sistema de auth)
    user_roles = current_user.roles if hasattr(current_user, 'roles') else []

    result = service.approve(
        request_id=request_id,
        user_id=current_user.username,
        comment=data.comment,
        user_roles=user_roles
    )

    if not result.get("success"):
        raise HTTPException(status_code=400, detail=result.get("error", "Failed to approve"))

    return {
        "message": "Approval recorded",
        "stage_completed": result.get("stage_completed", False),
        "workflow_completed": result.get("workflow_completed", False),
        "next_stage": result.get("next_stage")
    }


@router.post("/requests/{request_id}/reject")
async def reject_request(
    request_id: str,
    data: RejectRequest,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Rejeita uma solicitacao.

    A rejeicao encerra o fluxo de aprovacao.
    """
    user_roles = current_user.roles if hasattr(current_user, 'roles') else []

    result = service.reject(
        request_id=request_id,
        user_id=current_user.username,
        comment=data.comment,
        user_roles=user_roles
    )

    if not result.get("success"):
        raise HTTPException(status_code=400, detail=result.get("error", "Failed to reject"))

    return {"message": "Request rejected", "status": result.get("status")}


@router.post("/requests/{request_id}/request-changes")
async def request_changes(
    request_id: str,
    data: RejectRequest,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Solicita mudancas em uma solicitacao.

    Diferente da rejeicao, permite que o submitter faca ajustes e resubmeta.
    """
    user_roles = current_user.roles if hasattr(current_user, 'roles') else []

    result = service.request_changes(
        request_id=request_id,
        user_id=current_user.username,
        comment=data.comment,
        user_roles=user_roles
    )

    if not result.get("success"):
        raise HTTPException(status_code=400, detail=result.get("error", "Failed to request changes"))

    return {"message": result.get("message", "Changes requested")}


# =============================================================================
# USER PENDING APPROVALS
# =============================================================================

@router.get("/pending", response_model=List[ApprovalRequestResponse])
async def get_pending_approvals(
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Lista aprovacoes pendentes para o usuario atual.

    Retorna solicitacoes onde o usuario pode aprovar/rejeitar
    no estagio atual do workflow.
    """
    user_roles = current_user.roles if hasattr(current_user, 'roles') else []

    requests = service.get_pending_requests_for_user(
        user_id=current_user.username,
        user_roles=user_roles,
        tenant_id=current_user.tenant_id
    )

    return [
        ApprovalRequestResponse(
            request_id=r.request_id,
            workflow_id=r.workflow_id,
            resource_type=r.resource_type,
            resource_id=r.resource_id,
            status=r.status.value if hasattr(r.status, 'value') else r.status,
            current_stage_id=r.current_stage_id,
            title=r.title,
            priority=r.priority,
            submitter_id=r.submitter_id,
            created_at=r.created_at.isoformat() if r.created_at else None
        )
        for r in requests
    ]


# =============================================================================
# STORY INTEGRATION
# =============================================================================

@router.post("/stories/{story_id}/submit")
async def submit_story_for_approval(
    story_id: str,
    workflow_id: str = Query(..., description="ID do workflow a usar"),
    title: Optional[str] = Query(None, description="Titulo da solicitacao"),
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Submete uma Story para aprovacao.

    Endpoint de conveniencia para integrar o workflow de aprovacao
    com o sistema de Stories.
    """
    request = service.submit_for_approval(
        workflow_id=workflow_id,
        resource_type="story",
        resource_id=story_id,
        submitter_id=current_user.username,
        title=title or f"Approval request for Story {story_id}",
        tenant_id=current_user.tenant_id
    )

    if not request:
        raise HTTPException(status_code=400, detail="Failed to create approval request")

    return {
        "message": "Story submitted for approval",
        "request_id": request.request_id,
        "status": request.status.value if hasattr(request.status, 'value') else request.status,
        "current_stage_id": request.current_stage_id
    }


@router.get("/stories/{story_id}/approval-status")
async def get_story_approval_status(
    story_id: str,
    current_user: TokenData = Depends(get_current_user),
    service: ApprovalWorkflowService = Depends(get_service)
):
    """
    Retorna o status de aprovacao de uma Story.
    """
    requests = service.get_requests(
        resource_type="story",
        resource_id=story_id,
        tenant_id=current_user.tenant_id
    )

    if not requests:
        return {"has_approval": False, "message": "No approval request found for this story"}

    # Retornar a solicitacao mais recente
    latest = max(requests, key=lambda r: r.created_at)
    status = service.get_request_status(latest.request_id)

    return {
        "has_approval": True,
        "request_id": latest.request_id,
        "status": latest.status.value if hasattr(latest.status, 'value') else latest.status,
        "details": status
    }

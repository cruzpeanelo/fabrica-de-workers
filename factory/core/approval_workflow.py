# -*- coding: utf-8 -*-
"""
Approval Workflow - Sistema de Workflow de Aprovacao Configuravel
===================================================================

Issue #282 - [APPROVAL] Workflow de aprovacao configuravel

Este modulo implementa um sistema de workflow de aprovacao configuravel que permite:

- Criacao de workflows com multiplos estagios (review, QA, management, etc.)
- Regras de aprovacao flexiveis (por role, por usuario, por threshold)
- Rastreamento de status e historico de aprovacoes
- Integracao com Stories e outros recursos

Arquitetura:
    ApprovalWorkflow -> ApprovalStage -> ApprovalRule
                                      -> ApprovalRequest -> ApprovalAction
"""

from datetime import datetime
from enum import Enum
from typing import Optional, List, Dict, Any
from dataclasses import dataclass, field
import uuid
import json


# =============================================================================
# ENUMS
# =============================================================================

class ApprovalStageType(str, Enum):
    """Tipos de estagios de aprovacao"""
    REVIEW = "review"           # Revisao de codigo/conteudo
    QA = "qa"                   # Quality Assurance
    SECURITY = "security"       # Revisao de seguranca
    MANAGEMENT = "management"   # Aprovacao gerencial
    COMPLIANCE = "compliance"   # Conformidade regulatoria
    CUSTOM = "custom"           # Estagio customizado


class ApprovalRuleType(str, Enum):
    """Tipos de regras de aprovacao"""
    ANY_ONE = "any_one"         # Qualquer um aprovador
    ALL = "all"                 # Todos os aprovadores
    THRESHOLD = "threshold"     # Porcentagem minima de aprovadores
    SPECIFIC_USER = "specific_user"  # Usuario especifico
    ROLE_BASED = "role_based"   # Baseado em role


class ApprovalStatus(str, Enum):
    """Status de uma solicitacao de aprovacao"""
    PENDING = "pending"         # Aguardando aprovacao
    IN_PROGRESS = "in_progress" # Em andamento
    APPROVED = "approved"       # Aprovado
    REJECTED = "rejected"       # Rejeitado
    CANCELLED = "cancelled"     # Cancelado
    EXPIRED = "expired"         # Expirado


class ApprovalActionType(str, Enum):
    """Tipos de acoes de aprovacao"""
    APPROVE = "approve"
    REJECT = "reject"
    REQUEST_CHANGES = "request_changes"
    DELEGATE = "delegate"
    ESCALATE = "escalate"


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class ApprovalRule:
    """
    Regra de aprovacao para um estagio.
    Define quem pode aprovar e qual criterio usar.
    """
    rule_id: str = field(default_factory=lambda: f"RULE-{uuid.uuid4().hex[:8].upper()}")
    rule_type: ApprovalRuleType = ApprovalRuleType.ANY_ONE

    # Para rule_type = ROLE_BASED
    required_roles: List[str] = field(default_factory=list)

    # Para rule_type = SPECIFIC_USER
    required_users: List[str] = field(default_factory=list)

    # Para rule_type = THRESHOLD
    threshold_percentage: float = 50.0  # Porcentagem minima de aprovacoes
    min_approvers: int = 1              # Minimo de aprovadores

    # Configuracoes adicionais
    allow_self_approval: bool = False
    require_comment: bool = False
    auto_approve_conditions: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "rule_id": self.rule_id,
            "rule_type": self.rule_type.value if isinstance(self.rule_type, Enum) else self.rule_type,
            "required_roles": self.required_roles,
            "required_users": self.required_users,
            "threshold_percentage": self.threshold_percentage,
            "min_approvers": self.min_approvers,
            "allow_self_approval": self.allow_self_approval,
            "require_comment": self.require_comment,
            "auto_approve_conditions": self.auto_approve_conditions
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApprovalRule":
        rule_type = data.get("rule_type", "any_one")
        if isinstance(rule_type, str):
            rule_type = ApprovalRuleType(rule_type)
        return cls(
            rule_id=data.get("rule_id", f"RULE-{uuid.uuid4().hex[:8].upper()}"),
            rule_type=rule_type,
            required_roles=data.get("required_roles", []),
            required_users=data.get("required_users", []),
            threshold_percentage=data.get("threshold_percentage", 50.0),
            min_approvers=data.get("min_approvers", 1),
            allow_self_approval=data.get("allow_self_approval", False),
            require_comment=data.get("require_comment", False),
            auto_approve_conditions=data.get("auto_approve_conditions", {})
        )


@dataclass
class ApprovalStage:
    """
    Estagio de aprovacao em um workflow.
    Cada estagio tem suas proprias regras e aprovadores.
    """
    stage_id: str = field(default_factory=lambda: f"STG-{uuid.uuid4().hex[:8].upper()}")
    name: str = ""
    stage_type: ApprovalStageType = ApprovalStageType.REVIEW
    order: int = 1

    # Regras do estagio
    rules: List[ApprovalRule] = field(default_factory=list)

    # Aprovadores elegíveis
    eligible_approvers: List[str] = field(default_factory=list)  # Lista de user_ids
    eligible_roles: List[str] = field(default_factory=list)      # Lista de roles

    # Configuracoes
    is_optional: bool = False
    skip_conditions: Dict[str, Any] = field(default_factory=dict)
    timeout_hours: Optional[int] = None
    escalation_user_id: Optional[str] = None

    # Notificacoes
    notify_on_enter: bool = True
    notify_on_complete: bool = True
    notification_channels: List[str] = field(default_factory=lambda: ["email", "in_app"])

    def to_dict(self) -> Dict[str, Any]:
        return {
            "stage_id": self.stage_id,
            "name": self.name,
            "stage_type": self.stage_type.value if isinstance(self.stage_type, Enum) else self.stage_type,
            "order": self.order,
            "rules": [r.to_dict() for r in self.rules],
            "eligible_approvers": self.eligible_approvers,
            "eligible_roles": self.eligible_roles,
            "is_optional": self.is_optional,
            "skip_conditions": self.skip_conditions,
            "timeout_hours": self.timeout_hours,
            "escalation_user_id": self.escalation_user_id,
            "notify_on_enter": self.notify_on_enter,
            "notify_on_complete": self.notify_on_complete,
            "notification_channels": self.notification_channels
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApprovalStage":
        stage_type = data.get("stage_type", "review")
        if isinstance(stage_type, str):
            stage_type = ApprovalStageType(stage_type)
        return cls(
            stage_id=data.get("stage_id", f"STG-{uuid.uuid4().hex[:8].upper()}"),
            name=data.get("name", ""),
            stage_type=stage_type,
            order=data.get("order", 1),
            rules=[ApprovalRule.from_dict(r) for r in data.get("rules", [])],
            eligible_approvers=data.get("eligible_approvers", []),
            eligible_roles=data.get("eligible_roles", []),
            is_optional=data.get("is_optional", False),
            skip_conditions=data.get("skip_conditions", {}),
            timeout_hours=data.get("timeout_hours"),
            escalation_user_id=data.get("escalation_user_id"),
            notify_on_enter=data.get("notify_on_enter", True),
            notify_on_complete=data.get("notify_on_complete", True),
            notification_channels=data.get("notification_channels", ["email", "in_app"])
        )


@dataclass
class ApprovalAction:
    """
    Acao tomada em uma solicitacao de aprovacao.
    Registra quem fez o que e quando.
    """
    action_id: str = field(default_factory=lambda: f"ACT-{uuid.uuid4().hex[:8].upper()}")
    user_id: str = ""
    action_type: ApprovalActionType = ApprovalActionType.APPROVE
    stage_id: str = ""

    comment: Optional[str] = None
    delegated_to: Optional[str] = None

    created_at: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "action_id": self.action_id,
            "user_id": self.user_id,
            "action_type": self.action_type.value if isinstance(self.action_type, Enum) else self.action_type,
            "stage_id": self.stage_id,
            "comment": self.comment,
            "delegated_to": self.delegated_to,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "metadata": self.metadata
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApprovalAction":
        action_type = data.get("action_type", "approve")
        if isinstance(action_type, str):
            action_type = ApprovalActionType(action_type)
        created_at = data.get("created_at")
        if isinstance(created_at, str):
            created_at = datetime.fromisoformat(created_at)
        return cls(
            action_id=data.get("action_id", f"ACT-{uuid.uuid4().hex[:8].upper()}"),
            user_id=data.get("user_id", ""),
            action_type=action_type,
            stage_id=data.get("stage_id", ""),
            comment=data.get("comment"),
            delegated_to=data.get("delegated_to"),
            created_at=created_at or datetime.utcnow(),
            metadata=data.get("metadata", {})
        )


# =============================================================================
# APPROVAL WORKFLOW CLASS
# =============================================================================

class ApprovalWorkflow:
    """
    Classe principal para gerenciar workflows de aprovacao.

    Um workflow define a sequencia de estagios de aprovacao
    e as regras para cada estagio.

    Exemplo de uso:
        workflow = ApprovalWorkflow(
            name="Story Approval",
            description="Workflow de aprovacao para stories"
        )

        # Adicionar estagios
        workflow.add_stage(ApprovalStage(
            name="Code Review",
            stage_type=ApprovalStageType.REVIEW,
            eligible_roles=["developer", "tech_lead"]
        ))

        workflow.add_stage(ApprovalStage(
            name="QA Validation",
            stage_type=ApprovalStageType.QA,
            eligible_roles=["qa_engineer"]
        ))
    """

    def __init__(
        self,
        workflow_id: Optional[str] = None,
        name: str = "",
        description: str = "",
        tenant_id: Optional[str] = None,
        resource_type: str = "story",
        is_active: bool = True
    ):
        self.workflow_id = workflow_id or f"WF-{uuid.uuid4().hex[:8].upper()}"
        self.name = name
        self.description = description
        self.tenant_id = tenant_id
        self.resource_type = resource_type
        self.is_active = is_active

        self.stages: List[ApprovalStage] = []
        self.created_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()
        self.created_by: Optional[str] = None
        self.version: int = 1

        # Configuracoes globais
        self.config: Dict[str, Any] = {
            "allow_parallel_stages": False,
            "require_all_stages": True,
            "auto_complete_on_final_approval": True,
            "notify_submitter": True,
            "track_sla": True,
            "sla_hours": 24
        }

    def add_stage(self, stage: ApprovalStage) -> None:
        """Adiciona um estagio ao workflow"""
        # Auto-assign order se nao especificado
        if stage.order == 1 and self.stages:
            stage.order = max(s.order for s in self.stages) + 1

        self.stages.append(stage)
        self.stages.sort(key=lambda s: s.order)
        self.updated_at = datetime.utcnow()

    def remove_stage(self, stage_id: str) -> bool:
        """Remove um estagio do workflow"""
        initial_count = len(self.stages)
        self.stages = [s for s in self.stages if s.stage_id != stage_id]

        if len(self.stages) < initial_count:
            # Reordenar estagios
            for i, stage in enumerate(self.stages, 1):
                stage.order = i
            self.updated_at = datetime.utcnow()
            return True
        return False

    def get_stage(self, stage_id: str) -> Optional[ApprovalStage]:
        """Busca um estagio pelo ID"""
        for stage in self.stages:
            if stage.stage_id == stage_id:
                return stage
        return None

    def get_stage_by_order(self, order: int) -> Optional[ApprovalStage]:
        """Busca um estagio pela ordem"""
        for stage in self.stages:
            if stage.order == order:
                return stage
        return None

    def get_next_stage(self, current_stage_id: str) -> Optional[ApprovalStage]:
        """Retorna o proximo estagio apos o atual"""
        current_stage = self.get_stage(current_stage_id)
        if not current_stage:
            return None

        return self.get_stage_by_order(current_stage.order + 1)

    def get_first_stage(self) -> Optional[ApprovalStage]:
        """Retorna o primeiro estagio do workflow"""
        if not self.stages:
            return None
        return self.stages[0]

    def get_last_stage(self) -> Optional[ApprovalStage]:
        """Retorna o ultimo estagio do workflow"""
        if not self.stages:
            return None
        return self.stages[-1]

    def validate(self) -> Dict[str, Any]:
        """Valida o workflow e retorna erros se houver"""
        errors = []
        warnings = []

        if not self.name:
            errors.append("Workflow name is required")

        if not self.stages:
            errors.append("Workflow must have at least one stage")

        # Verificar ordens duplicadas
        orders = [s.order for s in self.stages]
        if len(orders) != len(set(orders)):
            errors.append("Duplicate stage orders found")

        # Verificar estagios sem aprovadores
        for stage in self.stages:
            if not stage.eligible_approvers and not stage.eligible_roles:
                warnings.append(f"Stage '{stage.name}' has no eligible approvers or roles")

        return {
            "is_valid": len(errors) == 0,
            "errors": errors,
            "warnings": warnings
        }

    def to_dict(self) -> Dict[str, Any]:
        """Serializa o workflow para dicionario"""
        return {
            "workflow_id": self.workflow_id,
            "name": self.name,
            "description": self.description,
            "tenant_id": self.tenant_id,
            "resource_type": self.resource_type,
            "is_active": self.is_active,
            "stages": [s.to_dict() for s in self.stages],
            "config": self.config,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "created_by": self.created_by,
            "version": self.version
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApprovalWorkflow":
        """Deserializa um dicionario para workflow"""
        workflow = cls(
            workflow_id=data.get("workflow_id"),
            name=data.get("name", ""),
            description=data.get("description", ""),
            tenant_id=data.get("tenant_id"),
            resource_type=data.get("resource_type", "story"),
            is_active=data.get("is_active", True)
        )

        workflow.stages = [ApprovalStage.from_dict(s) for s in data.get("stages", [])]
        workflow.config = data.get("config", workflow.config)

        created_at = data.get("created_at")
        if isinstance(created_at, str):
            workflow.created_at = datetime.fromisoformat(created_at)

        updated_at = data.get("updated_at")
        if isinstance(updated_at, str):
            workflow.updated_at = datetime.fromisoformat(updated_at)

        workflow.created_by = data.get("created_by")
        workflow.version = data.get("version", 1)

        return workflow


# =============================================================================
# APPROVAL REQUEST CLASS
# =============================================================================

class ApprovalRequest:
    """
    Solicitacao de aprovacao para um recurso especifico.

    Representa uma instancia de execucao de um workflow,
    rastreando o progresso atraves dos estagios.
    """

    def __init__(
        self,
        request_id: Optional[str] = None,
        workflow_id: str = "",
        resource_type: str = "story",
        resource_id: str = "",
        submitter_id: str = "",
        tenant_id: Optional[str] = None
    ):
        self.request_id = request_id or f"REQ-{uuid.uuid4().hex[:8].upper()}"
        self.workflow_id = workflow_id
        self.resource_type = resource_type
        self.resource_id = resource_id
        self.submitter_id = submitter_id
        self.tenant_id = tenant_id

        self.status = ApprovalStatus.PENDING
        self.current_stage_id: Optional[str] = None
        self.current_stage_order: int = 0

        self.actions: List[ApprovalAction] = []
        self.stage_statuses: Dict[str, ApprovalStatus] = {}

        self.title: str = ""
        self.description: str = ""
        self.priority: str = "normal"

        self.created_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()
        self.completed_at: Optional[datetime] = None
        self.expires_at: Optional[datetime] = None

        self.metadata: Dict[str, Any] = {}

    def add_action(self, action: ApprovalAction) -> None:
        """Adiciona uma acao a solicitacao"""
        self.actions.append(action)
        self.updated_at = datetime.utcnow()

    def get_actions_for_stage(self, stage_id: str) -> List[ApprovalAction]:
        """Retorna as acoes de um estagio especifico"""
        return [a for a in self.actions if a.stage_id == stage_id]

    def get_approvals_for_stage(self, stage_id: str) -> List[ApprovalAction]:
        """Retorna as aprovacoes de um estagio especifico"""
        return [a for a in self.actions
                if a.stage_id == stage_id and a.action_type == ApprovalActionType.APPROVE]

    def get_rejections_for_stage(self, stage_id: str) -> List[ApprovalAction]:
        """Retorna as rejeicoes de um estagio especifico"""
        return [a for a in self.actions
                if a.stage_id == stage_id and a.action_type == ApprovalActionType.REJECT]

    def is_stage_complete(self, stage: ApprovalStage) -> bool:
        """Verifica se um estagio esta completo baseado nas regras"""
        approvals = self.get_approvals_for_stage(stage.stage_id)
        rejections = self.get_rejections_for_stage(stage.stage_id)

        # Se houver rejeicao, o estagio nao esta completo
        if rejections:
            return False

        # Verificar cada regra do estagio
        for rule in stage.rules:
            if not self._check_rule(rule, stage, approvals):
                return False

        # Se nao houver regras, apenas verificar se tem pelo menos uma aprovacao
        if not stage.rules:
            return len(approvals) >= 1

        return True

    def _check_rule(self, rule: ApprovalRule, stage: ApprovalStage, approvals: List[ApprovalAction]) -> bool:
        """Verifica se uma regra esta satisfeita"""
        approver_ids = [a.user_id for a in approvals]

        if rule.rule_type == ApprovalRuleType.ANY_ONE:
            return len(approvals) >= 1

        elif rule.rule_type == ApprovalRuleType.ALL:
            # Todos os aprovadores elegiveis devem aprovar
            if stage.eligible_approvers:
                return all(a in approver_ids for a in stage.eligible_approvers)
            return len(approvals) >= rule.min_approvers

        elif rule.rule_type == ApprovalRuleType.THRESHOLD:
            if not stage.eligible_approvers:
                return len(approvals) >= rule.min_approvers

            total_approvers = len(stage.eligible_approvers)
            approved_count = len([a for a in approver_ids if a in stage.eligible_approvers])
            percentage = (approved_count / total_approvers) * 100

            return percentage >= rule.threshold_percentage and approved_count >= rule.min_approvers

        elif rule.rule_type == ApprovalRuleType.SPECIFIC_USER:
            return all(u in approver_ids for u in rule.required_users)

        elif rule.rule_type == ApprovalRuleType.ROLE_BASED:
            # Esta verificacao requer acesso ao sistema de roles
            # Por enquanto, verifica se ha aprovacoes suficientes
            return len(approvals) >= rule.min_approvers

        return False

    def advance_stage(self, workflow: ApprovalWorkflow) -> Optional[ApprovalStage]:
        """Avanca para o proximo estagio do workflow"""
        if not self.current_stage_id:
            first_stage = workflow.get_first_stage()
            if first_stage:
                self.current_stage_id = first_stage.stage_id
                self.current_stage_order = first_stage.order
                self.status = ApprovalStatus.IN_PROGRESS
                self.updated_at = datetime.utcnow()
            return first_stage

        next_stage = workflow.get_next_stage(self.current_stage_id)
        if next_stage:
            self.stage_statuses[self.current_stage_id] = ApprovalStatus.APPROVED
            self.current_stage_id = next_stage.stage_id
            self.current_stage_order = next_stage.order
            self.updated_at = datetime.utcnow()
            return next_stage

        # Nao ha mais estagios - workflow completo
        self.stage_statuses[self.current_stage_id] = ApprovalStatus.APPROVED
        self.status = ApprovalStatus.APPROVED
        self.completed_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()
        return None

    def reject(self, comment: Optional[str] = None) -> None:
        """Rejeita a solicitacao"""
        self.status = ApprovalStatus.REJECTED
        if self.current_stage_id:
            self.stage_statuses[self.current_stage_id] = ApprovalStatus.REJECTED
        self.completed_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()

    def cancel(self, reason: Optional[str] = None) -> None:
        """Cancela a solicitacao"""
        self.status = ApprovalStatus.CANCELLED
        self.completed_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()
        self.metadata["cancel_reason"] = reason

    def to_dict(self) -> Dict[str, Any]:
        """Serializa a solicitacao para dicionario"""
        return {
            "request_id": self.request_id,
            "workflow_id": self.workflow_id,
            "resource_type": self.resource_type,
            "resource_id": self.resource_id,
            "submitter_id": self.submitter_id,
            "tenant_id": self.tenant_id,
            "status": self.status.value if isinstance(self.status, Enum) else self.status,
            "current_stage_id": self.current_stage_id,
            "current_stage_order": self.current_stage_order,
            "actions": [a.to_dict() for a in self.actions],
            "stage_statuses": {k: v.value if isinstance(v, Enum) else v for k, v in self.stage_statuses.items()},
            "title": self.title,
            "description": self.description,
            "priority": self.priority,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "metadata": self.metadata
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApprovalRequest":
        """Deserializa um dicionario para solicitacao"""
        request = cls(
            request_id=data.get("request_id"),
            workflow_id=data.get("workflow_id", ""),
            resource_type=data.get("resource_type", "story"),
            resource_id=data.get("resource_id", ""),
            submitter_id=data.get("submitter_id", ""),
            tenant_id=data.get("tenant_id")
        )

        status = data.get("status", "pending")
        if isinstance(status, str):
            request.status = ApprovalStatus(status)
        else:
            request.status = status

        request.current_stage_id = data.get("current_stage_id")
        request.current_stage_order = data.get("current_stage_order", 0)
        request.actions = [ApprovalAction.from_dict(a) for a in data.get("actions", [])]

        stage_statuses = data.get("stage_statuses", {})
        request.stage_statuses = {
            k: ApprovalStatus(v) if isinstance(v, str) else v
            for k, v in stage_statuses.items()
        }

        request.title = data.get("title", "")
        request.description = data.get("description", "")
        request.priority = data.get("priority", "normal")

        for date_field in ["created_at", "updated_at", "completed_at", "expires_at"]:
            value = data.get(date_field)
            if isinstance(value, str):
                setattr(request, date_field, datetime.fromisoformat(value))

        request.metadata = data.get("metadata", {})

        return request


# =============================================================================
# APPROVAL WORKFLOW SERVICE
# =============================================================================

class ApprovalWorkflowService:
    """
    Servico para gerenciar workflows e solicitacoes de aprovacao.

    Este servico fornece uma interface de alto nivel para:
    - Criar e configurar workflows
    - Submeter recursos para aprovacao
    - Processar acoes de aprovacao/rejeicao
    - Consultar status de aprovacoes
    """

    def __init__(self, session=None):
        self.session = session
        self._workflows: Dict[str, ApprovalWorkflow] = {}
        self._requests: Dict[str, ApprovalRequest] = {}

    # -------------------------------------------------------------------------
    # WORKFLOW MANAGEMENT
    # -------------------------------------------------------------------------

    def create_workflow(
        self,
        name: str,
        description: str = "",
        resource_type: str = "story",
        tenant_id: Optional[str] = None,
        created_by: Optional[str] = None
    ) -> ApprovalWorkflow:
        """Cria um novo workflow"""
        workflow = ApprovalWorkflow(
            name=name,
            description=description,
            resource_type=resource_type,
            tenant_id=tenant_id
        )
        workflow.created_by = created_by

        self._workflows[workflow.workflow_id] = workflow
        return workflow

    def get_workflow(self, workflow_id: str) -> Optional[ApprovalWorkflow]:
        """Busca um workflow pelo ID"""
        return self._workflows.get(workflow_id)

    def get_workflows(
        self,
        tenant_id: Optional[str] = None,
        resource_type: Optional[str] = None,
        is_active: Optional[bool] = None
    ) -> List[ApprovalWorkflow]:
        """Lista workflows com filtros"""
        workflows = list(self._workflows.values())

        if tenant_id is not None:
            workflows = [w for w in workflows if w.tenant_id == tenant_id]

        if resource_type is not None:
            workflows = [w for w in workflows if w.resource_type == resource_type]

        if is_active is not None:
            workflows = [w for w in workflows if w.is_active == is_active]

        return workflows

    def update_workflow(self, workflow_id: str, updates: Dict[str, Any]) -> Optional[ApprovalWorkflow]:
        """Atualiza um workflow"""
        workflow = self.get_workflow(workflow_id)
        if not workflow:
            return None

        for key, value in updates.items():
            if hasattr(workflow, key) and key not in ["workflow_id", "created_at"]:
                setattr(workflow, key, value)

        workflow.updated_at = datetime.utcnow()
        workflow.version += 1

        return workflow

    def delete_workflow(self, workflow_id: str) -> bool:
        """Remove um workflow"""
        if workflow_id in self._workflows:
            del self._workflows[workflow_id]
            return True
        return False

    def add_stage_to_workflow(
        self,
        workflow_id: str,
        stage: ApprovalStage
    ) -> Optional[ApprovalWorkflow]:
        """Adiciona um estagio a um workflow"""
        workflow = self.get_workflow(workflow_id)
        if not workflow:
            return None

        workflow.add_stage(stage)
        return workflow

    # -------------------------------------------------------------------------
    # APPROVAL REQUESTS
    # -------------------------------------------------------------------------

    def submit_for_approval(
        self,
        workflow_id: str,
        resource_type: str,
        resource_id: str,
        submitter_id: str,
        title: str = "",
        description: str = "",
        priority: str = "normal",
        tenant_id: Optional[str] = None
    ) -> Optional[ApprovalRequest]:
        """Submete um recurso para aprovacao"""
        workflow = self.get_workflow(workflow_id)
        if not workflow or not workflow.is_active:
            return None

        request = ApprovalRequest(
            workflow_id=workflow_id,
            resource_type=resource_type,
            resource_id=resource_id,
            submitter_id=submitter_id,
            tenant_id=tenant_id or workflow.tenant_id
        )

        request.title = title
        request.description = description
        request.priority = priority

        # Iniciar no primeiro estagio
        request.advance_stage(workflow)

        self._requests[request.request_id] = request
        return request

    def get_request(self, request_id: str) -> Optional[ApprovalRequest]:
        """Busca uma solicitacao pelo ID"""
        return self._requests.get(request_id)

    def get_requests(
        self,
        workflow_id: Optional[str] = None,
        resource_type: Optional[str] = None,
        resource_id: Optional[str] = None,
        status: Optional[ApprovalStatus] = None,
        submitter_id: Optional[str] = None,
        tenant_id: Optional[str] = None
    ) -> List[ApprovalRequest]:
        """Lista solicitacoes com filtros"""
        requests = list(self._requests.values())

        if workflow_id is not None:
            requests = [r for r in requests if r.workflow_id == workflow_id]

        if resource_type is not None:
            requests = [r for r in requests if r.resource_type == resource_type]

        if resource_id is not None:
            requests = [r for r in requests if r.resource_id == resource_id]

        if status is not None:
            requests = [r for r in requests if r.status == status]

        if submitter_id is not None:
            requests = [r for r in requests if r.submitter_id == submitter_id]

        if tenant_id is not None:
            requests = [r for r in requests if r.tenant_id == tenant_id]

        return requests

    def get_pending_requests_for_user(
        self,
        user_id: str,
        user_roles: List[str] = None,
        tenant_id: Optional[str] = None
    ) -> List[ApprovalRequest]:
        """Retorna solicitacoes pendentes para um usuario"""
        pending = []

        for request in self._requests.values():
            if request.status not in [ApprovalStatus.PENDING, ApprovalStatus.IN_PROGRESS]:
                continue

            if tenant_id and request.tenant_id != tenant_id:
                continue

            workflow = self.get_workflow(request.workflow_id)
            if not workflow:
                continue

            current_stage = workflow.get_stage(request.current_stage_id)
            if not current_stage:
                continue

            # Verificar se o usuario pode aprovar este estagio
            if self._can_user_approve(user_id, user_roles or [], current_stage, request):
                pending.append(request)

        return pending

    def _can_user_approve(
        self,
        user_id: str,
        user_roles: List[str],
        stage: ApprovalStage,
        request: ApprovalRequest
    ) -> bool:
        """Verifica se um usuario pode aprovar um estagio"""
        # Verificar se ja aprovou
        approvals = request.get_approvals_for_stage(stage.stage_id)
        if any(a.user_id == user_id for a in approvals):
            return False

        # Verificar self-approval
        for rule in stage.rules:
            if not rule.allow_self_approval and request.submitter_id == user_id:
                return False

        # Verificar se esta na lista de aprovadores
        if user_id in stage.eligible_approvers:
            return True

        # Verificar se tem role elegivel
        if any(role in stage.eligible_roles for role in user_roles):
            return True

        return False

    # -------------------------------------------------------------------------
    # APPROVAL ACTIONS
    # -------------------------------------------------------------------------

    def approve(
        self,
        request_id: str,
        user_id: str,
        comment: Optional[str] = None,
        user_roles: List[str] = None
    ) -> Dict[str, Any]:
        """Aprova uma solicitacao no estagio atual"""
        request = self.get_request(request_id)
        if not request:
            return {"success": False, "error": "Request not found"}

        if request.status not in [ApprovalStatus.PENDING, ApprovalStatus.IN_PROGRESS]:
            return {"success": False, "error": f"Request is {request.status.value}"}

        workflow = self.get_workflow(request.workflow_id)
        if not workflow:
            return {"success": False, "error": "Workflow not found"}

        current_stage = workflow.get_stage(request.current_stage_id)
        if not current_stage:
            return {"success": False, "error": "Current stage not found"}

        # Verificar permissao
        if not self._can_user_approve(user_id, user_roles or [], current_stage, request):
            return {"success": False, "error": "User cannot approve this request"}

        # Verificar se comentario e obrigatorio
        for rule in current_stage.rules:
            if rule.require_comment and not comment:
                return {"success": False, "error": "Comment is required"}

        # Registrar acao
        action = ApprovalAction(
            user_id=user_id,
            action_type=ApprovalActionType.APPROVE,
            stage_id=current_stage.stage_id,
            comment=comment
        )
        request.add_action(action)

        # Verificar se o estagio esta completo
        if request.is_stage_complete(current_stage):
            next_stage = request.advance_stage(workflow)
            return {
                "success": True,
                "stage_completed": True,
                "next_stage": next_stage.to_dict() if next_stage else None,
                "workflow_completed": request.status == ApprovalStatus.APPROVED
            }

        return {
            "success": True,
            "stage_completed": False,
            "approvals_count": len(request.get_approvals_for_stage(current_stage.stage_id))
        }

    def reject(
        self,
        request_id: str,
        user_id: str,
        comment: str,
        user_roles: List[str] = None
    ) -> Dict[str, Any]:
        """Rejeita uma solicitacao"""
        request = self.get_request(request_id)
        if not request:
            return {"success": False, "error": "Request not found"}

        if request.status not in [ApprovalStatus.PENDING, ApprovalStatus.IN_PROGRESS]:
            return {"success": False, "error": f"Request is {request.status.value}"}

        workflow = self.get_workflow(request.workflow_id)
        if not workflow:
            return {"success": False, "error": "Workflow not found"}

        current_stage = workflow.get_stage(request.current_stage_id)
        if not current_stage:
            return {"success": False, "error": "Current stage not found"}

        # Verificar permissao
        if not self._can_user_approve(user_id, user_roles or [], current_stage, request):
            return {"success": False, "error": "User cannot reject this request"}

        # Registrar acao
        action = ApprovalAction(
            user_id=user_id,
            action_type=ApprovalActionType.REJECT,
            stage_id=current_stage.stage_id,
            comment=comment
        )
        request.add_action(action)
        request.reject(comment)

        return {
            "success": True,
            "status": request.status.value
        }

    def request_changes(
        self,
        request_id: str,
        user_id: str,
        comment: str,
        user_roles: List[str] = None
    ) -> Dict[str, Any]:
        """Solicita mudancas em uma solicitacao"""
        request = self.get_request(request_id)
        if not request:
            return {"success": False, "error": "Request not found"}

        workflow = self.get_workflow(request.workflow_id)
        if not workflow:
            return {"success": False, "error": "Workflow not found"}

        current_stage = workflow.get_stage(request.current_stage_id)
        if not current_stage:
            return {"success": False, "error": "Current stage not found"}

        action = ApprovalAction(
            user_id=user_id,
            action_type=ApprovalActionType.REQUEST_CHANGES,
            stage_id=current_stage.stage_id,
            comment=comment
        )
        request.add_action(action)

        return {
            "success": True,
            "message": "Changes requested"
        }

    def cancel_request(
        self,
        request_id: str,
        user_id: str,
        reason: str = ""
    ) -> Dict[str, Any]:
        """Cancela uma solicitacao"""
        request = self.get_request(request_id)
        if not request:
            return {"success": False, "error": "Request not found"}

        # Apenas o submitter ou admin pode cancelar
        if request.submitter_id != user_id:
            return {"success": False, "error": "Only submitter can cancel"}

        request.cancel(reason)

        return {
            "success": True,
            "status": request.status.value
        }

    def get_request_status(self, request_id: str) -> Optional[Dict[str, Any]]:
        """Retorna o status detalhado de uma solicitacao"""
        request = self.get_request(request_id)
        if not request:
            return None

        workflow = self.get_workflow(request.workflow_id)
        if not workflow:
            return None

        current_stage = workflow.get_stage(request.current_stage_id) if request.current_stage_id else None

        stages_info = []
        for stage in workflow.stages:
            stage_status = request.stage_statuses.get(stage.stage_id, ApprovalStatus.PENDING)
            if stage.stage_id == request.current_stage_id:
                stage_status = ApprovalStatus.IN_PROGRESS

            stages_info.append({
                "stage_id": stage.stage_id,
                "name": stage.name,
                "order": stage.order,
                "status": stage_status.value if isinstance(stage_status, Enum) else stage_status,
                "is_current": stage.stage_id == request.current_stage_id,
                "approvals": [a.to_dict() for a in request.get_approvals_for_stage(stage.stage_id)],
                "rejections": [a.to_dict() for a in request.get_rejections_for_stage(stage.stage_id)]
            })

        return {
            "request": request.to_dict(),
            "workflow": {
                "workflow_id": workflow.workflow_id,
                "name": workflow.name,
                "total_stages": len(workflow.stages)
            },
            "current_stage": current_stage.to_dict() if current_stage else None,
            "stages": stages_info,
            "progress": {
                "completed_stages": len([s for s in stages_info if s["status"] == "approved"]),
                "total_stages": len(stages_info),
                "percentage": (len([s for s in stages_info if s["status"] == "approved"]) / len(stages_info) * 100) if stages_info else 0
            }
        }


# =============================================================================
# FACTORY FUNCTIONS
# =============================================================================

def create_default_story_workflow(tenant_id: Optional[str] = None) -> ApprovalWorkflow:
    """
    Cria um workflow padrao para aprovacao de Stories.

    Estagios:
    1. Code Review - Revisao tecnica por desenvolvedores
    2. QA Validation - Validacao por QA
    3. Product Approval - Aprovacao pelo Product Owner
    """
    workflow = ApprovalWorkflow(
        name="Story Approval Workflow",
        description="Workflow padrao para aprovacao de User Stories",
        resource_type="story",
        tenant_id=tenant_id
    )

    # Stage 1: Code Review
    review_stage = ApprovalStage(
        name="Code Review",
        stage_type=ApprovalStageType.REVIEW,
        order=1,
        eligible_roles=["developer", "tech_lead", "architect"],
        rules=[
            ApprovalRule(
                rule_type=ApprovalRuleType.ANY_ONE,
                min_approvers=1,
                allow_self_approval=False,
                require_comment=False
            )
        ]
    )
    workflow.add_stage(review_stage)

    # Stage 2: QA Validation
    qa_stage = ApprovalStage(
        name="QA Validation",
        stage_type=ApprovalStageType.QA,
        order=2,
        eligible_roles=["qa_engineer", "qa_lead"],
        rules=[
            ApprovalRule(
                rule_type=ApprovalRuleType.ANY_ONE,
                min_approvers=1,
                require_comment=True
            )
        ]
    )
    workflow.add_stage(qa_stage)

    # Stage 3: Product Approval
    product_stage = ApprovalStage(
        name="Product Approval",
        stage_type=ApprovalStageType.MANAGEMENT,
        order=3,
        eligible_roles=["product_owner", "product_manager"],
        is_optional=False,
        rules=[
            ApprovalRule(
                rule_type=ApprovalRuleType.ROLE_BASED,
                required_roles=["product_owner"],
                min_approvers=1
            )
        ]
    )
    workflow.add_stage(product_stage)

    return workflow


def create_simple_approval_workflow(
    name: str,
    approver_roles: List[str],
    tenant_id: Optional[str] = None
) -> ApprovalWorkflow:
    """
    Cria um workflow simples com um unico estagio de aprovacao.
    """
    workflow = ApprovalWorkflow(
        name=name,
        description=f"Workflow simples: {name}",
        resource_type="generic",
        tenant_id=tenant_id
    )

    stage = ApprovalStage(
        name="Approval",
        stage_type=ApprovalStageType.REVIEW,
        order=1,
        eligible_roles=approver_roles,
        rules=[
            ApprovalRule(
                rule_type=ApprovalRuleType.ANY_ONE,
                min_approvers=1
            )
        ]
    )
    workflow.add_stage(stage)

    return workflow


# =============================================================================
# SINGLETON SERVICE INSTANCE
# =============================================================================

_service_instance: Optional[ApprovalWorkflowService] = None


def get_approval_service() -> ApprovalWorkflowService:
    """Retorna a instancia singleton do servico de aprovacao"""
    global _service_instance
    if _service_instance is None:
        _service_instance = ApprovalWorkflowService()
    return _service_instance

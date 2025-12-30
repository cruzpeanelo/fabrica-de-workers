# -*- coding: utf-8 -*-
"""
Deploy Models
=============
Modelos de dados para sistema de deploy.

Terminal 5 - Issue #300
"""

import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional

from .config import DeployEnvironment, DeployStatus, ApprovalStatus


@dataclass
class DeployArtifact:
    """Artefato a ser deployado"""
    artifact_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    artifact_type: str = ""  # code, config, metadata
    source_path: str = ""
    target_path: str = ""
    content: Optional[bytes] = None
    checksum: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class DeployRequest:
    """
    Requisicao de deploy.

    Attributes:
        request_id: ID unico da requisicao
        tenant_id: ID do tenant
        integration: Integracao alvo (sap_s4, salesforce, etc)
        environment: Ambiente de deploy
        artifacts: Artefatos a deployar
        description: Descricao do deploy
        requested_by: Usuario que solicitou
        scheduled_at: Data/hora agendada (opcional)
        metadata: Metadados adicionais
    """
    request_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    tenant_id: str = ""
    integration: str = ""
    environment: DeployEnvironment = DeployEnvironment.DEVELOPMENT
    artifacts: List[DeployArtifact] = field(default_factory=list)
    description: str = ""
    requested_by: str = ""
    scheduled_at: Optional[datetime] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    status: DeployStatus = DeployStatus.PENDING
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "request_id": self.request_id,
            "tenant_id": self.tenant_id,
            "integration": self.integration,
            "environment": self.environment.value,
            "artifact_count": len(self.artifacts),
            "description": self.description,
            "requested_by": self.requested_by,
            "scheduled_at": self.scheduled_at.isoformat() if self.scheduled_at else None,
            "created_at": self.created_at.isoformat(),
            "status": self.status.value,
            "metadata": self.metadata
        }


@dataclass
class ApprovalRequest:
    """
    Requisicao de aprovacao.

    Attributes:
        approval_id: ID unico da aprovacao
        deploy_request_id: ID do deploy associado
        tenant_id: ID do tenant
        status: Status da aprovacao
        approvers_required: Numero de aprovadores necessarios
        approvers: Lista de aprovadores
        approved_by: Lista de quem aprovou
        rejected_by: Quem rejeitou (se aplicavel)
        expires_at: Data de expiracao
        comments: Comentarios
    """
    approval_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    deploy_request_id: str = ""
    tenant_id: str = ""
    status: ApprovalStatus = ApprovalStatus.PENDING
    approvers_required: int = 1
    approvers: List[str] = field(default_factory=list)
    approved_by: List[str] = field(default_factory=list)
    rejected_by: Optional[str] = None
    rejection_reason: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    expires_at: Optional[datetime] = None
    comments: List[Dict[str, Any]] = field(default_factory=list)

    @property
    def is_approved(self) -> bool:
        """Verifica se tem aprovacoes suficientes"""
        return len(self.approved_by) >= self.approvers_required

    @property
    def is_expired(self) -> bool:
        """Verifica se expirou"""
        if self.expires_at:
            return datetime.utcnow() > self.expires_at
        return False

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "approval_id": self.approval_id,
            "deploy_request_id": self.deploy_request_id,
            "tenant_id": self.tenant_id,
            "status": self.status.value,
            "approvers_required": self.approvers_required,
            "approvers": self.approvers,
            "approved_by": self.approved_by,
            "rejected_by": self.rejected_by,
            "rejection_reason": self.rejection_reason,
            "created_at": self.created_at.isoformat(),
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "comments": self.comments
        }


@dataclass
class DeployStep:
    """Passo de um deploy"""
    step_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    order: int = 0
    status: str = "pending"  # pending, running, completed, failed, skipped
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    output: str = ""
    error: Optional[str] = None


@dataclass
class DeployResult:
    """
    Resultado de um deploy.

    Attributes:
        result_id: ID unico do resultado
        request_id: ID do deploy request
        tenant_id: ID do tenant
        success: Se o deploy foi bem sucedido
        status: Status final
        started_at: Inicio do deploy
        completed_at: Fim do deploy
        steps: Passos executados
        artifacts_deployed: Artefatos deployados
        rollback_available: Se rollback esta disponivel
        backup_id: ID do backup para rollback
        errors: Lista de erros
        warnings: Lista de avisos
        metadata: Metadados adicionais
    """
    result_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    request_id: str = ""
    tenant_id: str = ""
    success: bool = False
    status: DeployStatus = DeployStatus.PENDING
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    steps: List[DeployStep] = field(default_factory=list)
    artifacts_deployed: List[str] = field(default_factory=list)
    rollback_available: bool = False
    backup_id: Optional[str] = None
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)

    @property
    def duration_seconds(self) -> Optional[float]:
        """Calcula duracao do deploy"""
        if self.started_at and self.completed_at:
            return (self.completed_at - self.started_at).total_seconds()
        return None

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "result_id": self.result_id,
            "request_id": self.request_id,
            "tenant_id": self.tenant_id,
            "success": self.success,
            "status": self.status.value,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration_seconds": self.duration_seconds,
            "step_count": len(self.steps),
            "artifacts_deployed": self.artifacts_deployed,
            "rollback_available": self.rollback_available,
            "backup_id": self.backup_id,
            "errors": self.errors,
            "warnings": self.warnings,
            "metadata": self.metadata
        }


@dataclass
class DeployLog:
    """
    Log de deploy para auditoria.

    Attributes:
        log_id: ID unico do log
        tenant_id: ID do tenant
        request_id: ID do deploy request
        event_type: Tipo de evento
        message: Mensagem do log
        level: Nivel (info, warning, error)
        user_id: Usuario que executou a acao
        timestamp: Data/hora do evento
        metadata: Metadados adicionais
    """
    log_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    tenant_id: str = ""
    request_id: str = ""
    event_type: str = ""  # request_created, approval_requested, approved, rejected, started, completed, failed, rolled_back
    message: str = ""
    level: str = "info"
    user_id: Optional[str] = None
    timestamp: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "log_id": self.log_id,
            "tenant_id": self.tenant_id,
            "request_id": self.request_id,
            "event_type": self.event_type,
            "message": self.message,
            "level": self.level,
            "user_id": self.user_id,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata
        }


@dataclass
class DeployBackup:
    """Backup para rollback"""
    backup_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    request_id: str = ""
    tenant_id: str = ""
    integration: str = ""
    environment: str = ""
    created_at: datetime = field(default_factory=datetime.utcnow)
    expires_at: Optional[datetime] = None
    artifacts: List[Dict[str, Any]] = field(default_factory=list)
    storage_path: str = ""
    size_bytes: int = 0
    checksum: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)

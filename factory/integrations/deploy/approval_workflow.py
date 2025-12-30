# -*- coding: utf-8 -*-
"""
Approval Workflow
=================
Sistema de aprovacao multi-nivel para deploys.

Terminal 5 - Issue #300
"""

import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional

from .config import DeployConfig, ApprovalStatus
from .models import DeployRequest, ApprovalRequest

logger = logging.getLogger(__name__)


class ApprovalWorkflow:
    """
    Gerenciador de workflow de aprovacao.

    Suporta:
    - Aprovacao multi-nivel
    - Expiracao automatica
    - Notificacao de aprovadores
    - Comentarios

    Exemplo:
        workflow = ApprovalWorkflow(config)

        # Criar aprovacao
        approval = await workflow.create_approval(deploy_request)

        # Aprovar
        is_complete = await workflow.approve(request_id, "admin@empresa.com")

        # Verificar status
        status = workflow.get_approval_status(request_id)
    """

    def __init__(self, config: DeployConfig):
        """
        Inicializa o workflow.

        Args:
            config: Configuracao de deploy do tenant
        """
        self.config = config
        self._approvals: Dict[str, ApprovalRequest] = {}

    async def create_approval(
        self,
        deploy_request: DeployRequest
    ) -> ApprovalRequest:
        """
        Cria requisicao de aprovacao para um deploy.

        Args:
            deploy_request: Requisicao de deploy

        Returns:
            Requisicao de aprovacao criada
        """
        env_config = self.config.get_environment_config(deploy_request.environment)
        approval_config = env_config.approval_config

        # Calcula expiracao
        expires_at = datetime.utcnow() + timedelta(
            hours=approval_config.approval_timeout_hours
        )

        approval = ApprovalRequest(
            deploy_request_id=deploy_request.request_id,
            tenant_id=self.config.tenant_id,
            status=ApprovalStatus.PENDING,
            approvers_required=approval_config.required_approvers,
            approvers=self._get_approvers(approval_config.approver_roles),
            expires_at=expires_at
        )

        self._approvals[deploy_request.request_id] = approval
        logger.info(
            f"Aprovacao criada para deploy {deploy_request.request_id} "
            f"(requer {approval_config.required_approvers} aprovadores)"
        )

        return approval

    def _get_approvers(self, roles: List[str]) -> List[str]:
        """Obtem lista de aprovadores baseado em roles"""
        # TODO: Integrar com sistema de usuarios/roles
        # Por enquanto retorna lista vazia (qualquer um pode aprovar)
        return []

    async def approve(
        self,
        request_id: str,
        approver: str,
        comment: Optional[str] = None
    ) -> bool:
        """
        Registra aprovacao de um deploy.

        Args:
            request_id: ID do deploy
            approver: Email/ID do aprovador
            comment: Comentario opcional

        Returns:
            True se aprovacoes suficientes foram atingidas
        """
        approval = self._approvals.get(request_id)
        if not approval:
            raise ValueError(f"Aprovacao nao encontrada: {request_id}")

        if approval.status != ApprovalStatus.PENDING:
            raise ValueError(f"Aprovacao nao esta pendente: {approval.status}")

        if approval.is_expired:
            approval.status = ApprovalStatus.EXPIRED
            raise ValueError("Aprovacao expirada")

        # Verifica se ja aprovou
        if approver in approval.approved_by:
            logger.warning(f"{approver} ja aprovou este deploy")
            return approval.is_approved

        # Verifica self-approval
        env_config = self.config.get_environment_config(
            self._get_deploy_environment(request_id)
        )
        if not env_config.approval_config.allow_self_approval:
            deploy_request = self._get_deploy_request(request_id)
            if deploy_request and deploy_request.requested_by == approver:
                raise ValueError("Self-approval nao permitido")

        # Registra aprovacao
        approval.approved_by.append(approver)
        if comment:
            approval.comments.append({
                "user": approver,
                "comment": comment,
                "timestamp": datetime.utcnow().isoformat(),
                "action": "approved"
            })

        logger.info(
            f"Deploy {request_id} aprovado por {approver} "
            f"({len(approval.approved_by)}/{approval.approvers_required})"
        )

        # Verifica se atingiu quorum
        if approval.is_approved:
            approval.status = ApprovalStatus.APPROVED
            logger.info(f"Deploy {request_id} totalmente aprovado")
            return True

        return False

    async def reject(
        self,
        request_id: str,
        rejector: str,
        reason: str
    ):
        """
        Rejeita um deploy.

        Args:
            request_id: ID do deploy
            rejector: Email/ID de quem rejeitou
            reason: Motivo da rejeicao
        """
        approval = self._approvals.get(request_id)
        if not approval:
            raise ValueError(f"Aprovacao nao encontrada: {request_id}")

        approval.status = ApprovalStatus.REJECTED
        approval.rejected_by = rejector
        approval.rejection_reason = reason
        approval.comments.append({
            "user": rejector,
            "comment": reason,
            "timestamp": datetime.utcnow().isoformat(),
            "action": "rejected"
        })

        logger.info(f"Deploy {request_id} rejeitado por {rejector}: {reason}")

    def get_approval_status(self, request_id: str) -> Optional[ApprovalRequest]:
        """Obtem status de aprovacao"""
        approval = self._approvals.get(request_id)
        if approval and approval.status == ApprovalStatus.PENDING:
            # Verifica expiracao
            if approval.is_expired:
                approval.status = ApprovalStatus.EXPIRED
        return approval

    def list_pending_approvals(self) -> List[ApprovalRequest]:
        """Lista aprovacoes pendentes"""
        pending = []
        for approval in self._approvals.values():
            if approval.status == ApprovalStatus.PENDING:
                if approval.is_expired:
                    approval.status = ApprovalStatus.EXPIRED
                else:
                    pending.append(approval)
        return pending

    def get_my_pending_approvals(self, user: str) -> List[ApprovalRequest]:
        """Lista aprovacoes pendentes para um usuario"""
        pending = self.list_pending_approvals()
        # Se ha lista de aprovadores, filtra
        # Caso contrario, retorna todas
        return [
            a for a in pending
            if not a.approvers or user in a.approvers
        ]

    def _get_deploy_environment(self, request_id: str):
        """Obtem ambiente do deploy"""
        # TODO: Buscar do deploy request
        from .config import DeployEnvironment
        return DeployEnvironment.PRODUCTION

    def _get_deploy_request(self, request_id: str):
        """Obtem deploy request"""
        # TODO: Buscar do deploy manager
        return None

    async def check_and_expire_approvals(self):
        """Verifica e expira aprovacoes antigas"""
        env_config = self.config.get_environment_config(
            self._get_deploy_environment("")
        )

        for approval in self._approvals.values():
            if approval.status == ApprovalStatus.PENDING and approval.is_expired:
                approval.status = ApprovalStatus.EXPIRED
                if env_config.approval_config.auto_reject_on_timeout:
                    approval.rejection_reason = "Aprovacao expirada automaticamente"
                    logger.info(
                        f"Aprovacao {approval.approval_id} expirada automaticamente"
                    )

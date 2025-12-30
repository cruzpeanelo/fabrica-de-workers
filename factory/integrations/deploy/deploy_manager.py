# -*- coding: utf-8 -*-
"""
Deploy Manager
==============
Gerenciador principal de deploys com suporte a multi-tenant.

Terminal 5 - Issue #300
"""

import asyncio
import logging
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Callable

from .config import DeployConfig, DeployMode, DeployEnvironment, DeployStatus
from .models import (
    DeployRequest, DeployResult, DeployLog, DeployStep,
    DeployArtifact, DeployBackup
)
from .approval_workflow import ApprovalWorkflow
from .rollback_handler import RollbackHandler

logger = logging.getLogger(__name__)


class DeployManager:
    """
    Gerenciador de deploys configuravel por tenant.

    Permite:
    - Deploy manual, automatico ou hibrido
    - Aprovacao multi-nivel
    - Rollback automatico em falhas
    - Auditoria completa
    - Notificacoes

    Exemplo:
        config = DeployConfig(
            tenant_id="TENANT-001",
            default_mode=DeployMode.HYBRID
        )
        manager = DeployManager(config)

        # Criar requisicao de deploy
        request = DeployRequest(
            tenant_id="TENANT-001",
            integration="sap_s4",
            environment=DeployEnvironment.PRODUCTION,
            description="Deploy de nova feature"
        )

        # Submeter deploy
        result = await manager.submit_deploy(request)

        # Se aprovacao necessaria
        if result.status == DeployStatus.PENDING:
            await manager.approve_deploy(request.request_id, "admin@empresa.com")
    """

    def __init__(self, config: DeployConfig):
        """
        Inicializa o gerenciador.

        Args:
            config: Configuracao de deploy do tenant
        """
        self.config = config
        self._approval_workflow = ApprovalWorkflow(config)
        self._rollback_handler = RollbackHandler(config)
        self._logs: List[DeployLog] = []
        self._requests: Dict[str, DeployRequest] = {}
        self._results: Dict[str, DeployResult] = {}
        self._backups: Dict[str, DeployBackup] = {}

        # Callbacks para extensao
        self._pre_deploy_hooks: List[Callable] = []
        self._post_deploy_hooks: List[Callable] = []
        self._notification_handler: Optional[Callable] = None

    def _log_event(
        self,
        event_type: str,
        message: str,
        request_id: str = "",
        level: str = "info",
        user_id: Optional[str] = None,
        metadata: Optional[Dict] = None
    ):
        """Registra evento no log"""
        log = DeployLog(
            tenant_id=self.config.tenant_id,
            request_id=request_id,
            event_type=event_type,
            message=message,
            level=level,
            user_id=user_id,
            metadata=metadata or {}
        )
        self._logs.append(log)

        if level == "error":
            logger.error(f"[{self.config.tenant_id}] {event_type}: {message}")
        elif level == "warning":
            logger.warning(f"[{self.config.tenant_id}] {event_type}: {message}")
        else:
            logger.info(f"[{self.config.tenant_id}] {event_type}: {message}")

    async def _notify(
        self,
        event_type: str,
        request: DeployRequest,
        result: Optional[DeployResult] = None
    ):
        """Envia notificacao"""
        if self._notification_handler:
            try:
                await self._notification_handler(event_type, request, result)
            except Exception as e:
                logger.warning(f"Erro ao enviar notificacao: {e}")

    # =========================================================================
    # Submissao e Execucao de Deploy
    # =========================================================================

    async def submit_deploy(
        self,
        request: DeployRequest,
        skip_approval: bool = False
    ) -> DeployResult:
        """
        Submete uma requisicao de deploy.

        Args:
            request: Requisicao de deploy
            skip_approval: Pular aprovacao (requer permissao)

        Returns:
            Resultado do deploy
        """
        # Valida requisicao
        if request.tenant_id != self.config.tenant_id:
            raise ValueError("tenant_id da requisicao nao corresponde ao config")

        if request.integration not in self.config.enabled_integrations:
            raise ValueError(f"Integracao {request.integration} nao habilitada")

        # Armazena requisicao
        self._requests[request.request_id] = request
        self._log_event(
            "request_created",
            f"Deploy request criado: {request.description}",
            request.request_id,
            user_id=request.requested_by
        )

        # Verifica modo de deploy
        env_config = self.config.get_environment_config(request.environment)
        deploy_mode = env_config.deploy_mode

        # Verifica se precisa aprovacao
        needs_approval = (
            env_config.approval_required and
            not skip_approval and
            deploy_mode != DeployMode.AUTOMATIC
        )

        if needs_approval:
            # Cria requisicao de aprovacao
            approval = await self._approval_workflow.create_approval(request)
            request.status = DeployStatus.PENDING

            self._log_event(
                "approval_requested",
                f"Aprovacao solicitada para deploy",
                request.request_id,
                metadata={"approval_id": approval.approval_id}
            )

            await self._notify("approval_requested", request)

            return DeployResult(
                request_id=request.request_id,
                tenant_id=self.config.tenant_id,
                status=DeployStatus.PENDING,
                metadata={"approval_id": approval.approval_id}
            )

        # Executa deploy imediatamente
        return await self._execute_deploy(request)

    async def _execute_deploy(self, request: DeployRequest) -> DeployResult:
        """Executa o deploy"""
        result = DeployResult(
            request_id=request.request_id,
            tenant_id=self.config.tenant_id,
            status=DeployStatus.IN_PROGRESS,
            started_at=datetime.utcnow()
        )
        self._results[result.result_id] = result

        request.status = DeployStatus.IN_PROGRESS
        self._log_event("deploy_started", "Deploy iniciado", request.request_id)
        await self._notify("deploy_started", request, result)

        try:
            # Cria backup para rollback
            backup = await self._create_backup(request)
            if backup:
                result.backup_id = backup.backup_id
                result.rollback_available = True

            # Executa pre-deploy hooks
            for hook in self._pre_deploy_hooks:
                step = DeployStep(name="pre_deploy_hook", order=len(result.steps))
                result.steps.append(step)
                step.status = "running"
                step.started_at = datetime.utcnow()
                try:
                    await hook(request)
                    step.status = "completed"
                except Exception as e:
                    step.status = "failed"
                    step.error = str(e)
                    raise
                finally:
                    step.completed_at = datetime.utcnow()

            # Executa deploy dos artefatos
            for artifact in request.artifacts:
                step = DeployStep(
                    name=f"deploy_{artifact.name}",
                    order=len(result.steps)
                )
                result.steps.append(step)
                step.status = "running"
                step.started_at = datetime.utcnow()

                try:
                    await self._deploy_artifact(artifact, request)
                    step.status = "completed"
                    result.artifacts_deployed.append(artifact.artifact_id)
                except Exception as e:
                    step.status = "failed"
                    step.error = str(e)
                    result.errors.append(f"Falha ao deployar {artifact.name}: {e}")
                finally:
                    step.completed_at = datetime.utcnow()

            # Executa post-deploy hooks
            for hook in self._post_deploy_hooks:
                step = DeployStep(name="post_deploy_hook", order=len(result.steps))
                result.steps.append(step)
                step.status = "running"
                step.started_at = datetime.utcnow()
                try:
                    await hook(request, result)
                    step.status = "completed"
                except Exception as e:
                    step.status = "failed"
                    step.error = str(e)
                    result.warnings.append(f"Post-deploy hook falhou: {e}")
                finally:
                    step.completed_at = datetime.utcnow()

            # Verifica resultado
            failed_steps = [s for s in result.steps if s.status == "failed"]
            if failed_steps:
                result.success = False
                result.status = DeployStatus.FAILED

                # Tenta rollback automatico
                env_config = self.config.get_environment_config(request.environment)
                if env_config.rollback_config.auto_rollback_on_failure:
                    await self._rollback_handler.rollback(result)
                    result.status = DeployStatus.ROLLED_BACK
            else:
                result.success = True
                result.status = DeployStatus.COMPLETED

            result.completed_at = datetime.utcnow()
            request.status = result.status

            self._log_event(
                "deploy_completed" if result.success else "deploy_failed",
                f"Deploy {'concluido' if result.success else 'falhou'}",
                request.request_id,
                level="info" if result.success else "error",
                metadata={"duration": result.duration_seconds}
            )

            await self._notify(
                "deploy_success" if result.success else "deploy_failure",
                request, result
            )

        except Exception as e:
            result.success = False
            result.status = DeployStatus.FAILED
            result.completed_at = datetime.utcnow()
            result.errors.append(str(e))

            self._log_event(
                "deploy_error",
                f"Erro no deploy: {e}",
                request.request_id,
                level="error"
            )

            # Tenta rollback
            env_config = self.config.get_environment_config(request.environment)
            if env_config.rollback_config.auto_rollback_on_failure:
                try:
                    await self._rollback_handler.rollback(result)
                    result.status = DeployStatus.ROLLED_BACK
                except Exception as rollback_error:
                    result.errors.append(f"Rollback falhou: {rollback_error}")

            await self._notify("deploy_failure", request, result)

        return result

    async def _deploy_artifact(
        self,
        artifact: DeployArtifact,
        request: DeployRequest
    ):
        """Deploy de um artefato individual"""
        # Implementacao especifica por integracao
        # TODO: Integrar com conectores SAP, Salesforce, etc.
        logger.info(f"Deployando artefato: {artifact.name} para {request.integration}")
        await asyncio.sleep(0.1)  # Simula operacao

    async def _create_backup(self, request: DeployRequest) -> Optional[DeployBackup]:
        """Cria backup antes do deploy"""
        try:
            backup = DeployBackup(
                request_id=request.request_id,
                tenant_id=self.config.tenant_id,
                integration=request.integration,
                environment=request.environment.value,
                expires_at=datetime.utcnow() + timedelta(days=30)
            )
            self._backups[backup.backup_id] = backup
            self._log_event(
                "backup_created",
                f"Backup criado: {backup.backup_id}",
                request.request_id
            )
            return backup
        except Exception as e:
            logger.warning(f"Falha ao criar backup: {e}")
            return None

    # =========================================================================
    # Aprovacao
    # =========================================================================

    async def approve_deploy(
        self,
        request_id: str,
        approver: str,
        comment: Optional[str] = None
    ) -> DeployResult:
        """
        Aprova um deploy pendente.

        Args:
            request_id: ID da requisicao
            approver: Email/ID do aprovador
            comment: Comentario opcional

        Returns:
            Resultado do deploy (executado se aprovado)
        """
        request = self._requests.get(request_id)
        if not request:
            raise ValueError(f"Requisicao nao encontrada: {request_id}")

        if request.status != DeployStatus.PENDING:
            raise ValueError(f"Requisicao nao esta pendente: {request.status}")

        # Registra aprovacao
        is_approved = await self._approval_workflow.approve(
            request_id, approver, comment
        )

        self._log_event(
            "approval_given",
            f"Aprovacao registrada por {approver}",
            request_id,
            user_id=approver
        )

        if is_approved:
            request.status = DeployStatus.APPROVED
            self._log_event(
                "deploy_approved",
                "Deploy aprovado - iniciando execucao",
                request_id
            )
            return await self._execute_deploy(request)

        # Ainda precisa de mais aprovacoes
        return DeployResult(
            request_id=request_id,
            tenant_id=self.config.tenant_id,
            status=DeployStatus.PENDING,
            metadata={"awaiting_approvals": True}
        )

    async def reject_deploy(
        self,
        request_id: str,
        rejector: str,
        reason: str
    ) -> DeployResult:
        """
        Rejeita um deploy pendente.

        Args:
            request_id: ID da requisicao
            rejector: Email/ID de quem rejeitou
            reason: Motivo da rejeicao

        Returns:
            Resultado com status rejeitado
        """
        request = self._requests.get(request_id)
        if not request:
            raise ValueError(f"Requisicao nao encontrada: {request_id}")

        await self._approval_workflow.reject(request_id, rejector, reason)
        request.status = DeployStatus.REJECTED

        self._log_event(
            "deploy_rejected",
            f"Deploy rejeitado: {reason}",
            request_id,
            user_id=rejector
        )

        await self._notify("deploy_rejected", request)

        return DeployResult(
            request_id=request_id,
            tenant_id=self.config.tenant_id,
            success=False,
            status=DeployStatus.REJECTED,
            errors=[f"Rejeitado por {rejector}: {reason}"]
        )

    # =========================================================================
    # Rollback
    # =========================================================================

    async def rollback(self, request_id: str, user: str) -> DeployResult:
        """
        Executa rollback de um deploy.

        Args:
            request_id: ID do deploy a reverter
            user: Usuario solicitando rollback

        Returns:
            Resultado do rollback
        """
        result = None
        for r in self._results.values():
            if r.request_id == request_id:
                result = r
                break

        if not result:
            raise ValueError(f"Deploy nao encontrado: {request_id}")

        if not result.rollback_available:
            raise ValueError("Rollback nao disponivel para este deploy")

        self._log_event(
            "rollback_requested",
            f"Rollback solicitado por {user}",
            request_id,
            user_id=user
        )

        rollback_result = await self._rollback_handler.rollback(result)
        result.status = DeployStatus.ROLLED_BACK

        self._log_event(
            "rollback_completed",
            "Rollback concluido",
            request_id
        )

        return rollback_result

    # =========================================================================
    # Consultas
    # =========================================================================

    def get_deploy_request(self, request_id: str) -> Optional[DeployRequest]:
        """Obtem requisicao de deploy"""
        return self._requests.get(request_id)

    def get_deploy_result(self, request_id: str) -> Optional[DeployResult]:
        """Obtem resultado de deploy"""
        for result in self._results.values():
            if result.request_id == request_id:
                return result
        return None

    def list_deploys(
        self,
        status: Optional[DeployStatus] = None,
        environment: Optional[DeployEnvironment] = None,
        limit: int = 50
    ) -> List[DeployRequest]:
        """Lista deploys do tenant"""
        deploys = list(self._requests.values())

        if status:
            deploys = [d for d in deploys if d.status == status]
        if environment:
            deploys = [d for d in deploys if d.environment == environment]

        # Ordena por data
        deploys.sort(key=lambda x: x.created_at, reverse=True)
        return deploys[:limit]

    def get_audit_log(
        self,
        request_id: Optional[str] = None,
        limit: int = 100
    ) -> List[DeployLog]:
        """Obtem log de auditoria"""
        logs = self._logs
        if request_id:
            logs = [l for l in logs if l.request_id == request_id]
        logs.sort(key=lambda x: x.timestamp, reverse=True)
        return logs[:limit]

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas de deploy"""
        total = len(self._requests)
        completed = len([r for r in self._requests.values() if r.status == DeployStatus.COMPLETED])
        failed = len([r for r in self._requests.values() if r.status == DeployStatus.FAILED])
        pending = len([r for r in self._requests.values() if r.status == DeployStatus.PENDING])

        return {
            "tenant_id": self.config.tenant_id,
            "total_deploys": total,
            "completed": completed,
            "failed": failed,
            "pending_approval": pending,
            "success_rate": (completed / total * 100) if total > 0 else 0,
            "backups_available": len(self._backups)
        }

    # =========================================================================
    # Hooks e Extensao
    # =========================================================================

    def add_pre_deploy_hook(self, hook: Callable):
        """Adiciona hook pre-deploy"""
        self._pre_deploy_hooks.append(hook)

    def add_post_deploy_hook(self, hook: Callable):
        """Adiciona hook post-deploy"""
        self._post_deploy_hooks.append(hook)

    def set_notification_handler(self, handler: Callable):
        """Define handler de notificacoes"""
        self._notification_handler = handler

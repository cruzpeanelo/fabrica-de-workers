# -*- coding: utf-8 -*-
"""
Rollback Handler
================
Sistema de rollback automatico para deploys.

Terminal 5 - Issue #300
Terminal A - Issue #332: Restauracao por integracao (SAP, Salesforce, Azure DevOps)
"""

import asyncio
import logging
from datetime import datetime
from typing import Dict, List, Optional, TYPE_CHECKING

from .config import DeployConfig, DeployStatus, DeployEnvironment
from .models import DeployResult, DeployStep, DeployBackup

if TYPE_CHECKING:
    from ..sap_s4hana import SAPS4HanaIntegration
    from ..salesforce_connector import SalesforceConnector
    from ..azure_devops import AzureDevOpsIntegration

logger = logging.getLogger(__name__)


class RollbackHandler:
    """
    Gerenciador de rollback para deploys.

    Suporta:
    - Rollback automatico em falhas
    - Rollback manual
    - Multiplas tentativas
    - Backup e restauracao

    Exemplo:
        handler = RollbackHandler(config)

        # Rollback automatico
        result = await handler.rollback(deploy_result)

        # Restaurar backup especifico
        result = await handler.restore_backup(backup_id)
    """

    def __init__(self, config: DeployConfig):
        """
        Inicializa o handler.

        Args:
            config: Configuracao de deploy do tenant
        """
        self.config = config
        self._backups: Dict[str, DeployBackup] = {}
        self._rollback_history: List[Dict] = []

    async def rollback(
        self,
        deploy_result: DeployResult,
        target_backup_id: Optional[str] = None
    ) -> DeployResult:
        """
        Executa rollback de um deploy.

        Args:
            deploy_result: Resultado do deploy a reverter
            target_backup_id: ID do backup especifico (opcional)

        Returns:
            Resultado do rollback
        """
        backup_id = target_backup_id or deploy_result.backup_id

        if not backup_id:
            raise ValueError("Nenhum backup disponivel para rollback")

        backup = self._backups.get(backup_id)
        if not backup:
            # Tenta buscar backup externo
            backup = await self._fetch_backup(backup_id)
            if not backup:
                raise ValueError(f"Backup nao encontrado: {backup_id}")

        logger.info(
            f"Iniciando rollback do deploy {deploy_result.request_id} "
            f"usando backup {backup_id}"
        )

        rollback_result = DeployResult(
            request_id=deploy_result.request_id,
            tenant_id=self.config.tenant_id,
            status=DeployStatus.IN_PROGRESS,
            started_at=datetime.utcnow(),
            metadata={"rollback_from": deploy_result.result_id}
        )

        env = DeployEnvironment(backup.environment)
        env_config = self.config.get_environment_config(env)
        max_attempts = env_config.rollback_config.max_rollback_attempts
        timeout = env_config.rollback_config.rollback_timeout_seconds

        for attempt in range(max_attempts):
            try:
                # Adiciona step para esta tentativa
                step = DeployStep(
                    name=f"rollback_attempt_{attempt + 1}",
                    order=len(rollback_result.steps),
                    status="running"
                )
                step.started_at = datetime.utcnow()
                rollback_result.steps.append(step)

                # Executa rollback com timeout
                await asyncio.wait_for(
                    self._execute_rollback(backup, deploy_result),
                    timeout=timeout
                )

                step.status = "completed"
                step.completed_at = datetime.utcnow()
                rollback_result.success = True
                rollback_result.status = DeployStatus.ROLLED_BACK

                logger.info(
                    f"Rollback concluido na tentativa {attempt + 1}"
                )
                break

            except asyncio.TimeoutError:
                step.status = "failed"
                step.error = f"Timeout apos {timeout}s"
                step.completed_at = datetime.utcnow()
                rollback_result.errors.append(
                    f"Tentativa {attempt + 1}: timeout"
                )
                logger.warning(
                    f"Rollback timeout na tentativa {attempt + 1}"
                )

            except Exception as e:
                step.status = "failed"
                step.error = str(e)
                step.completed_at = datetime.utcnow()
                rollback_result.errors.append(
                    f"Tentativa {attempt + 1}: {e}"
                )
                logger.error(
                    f"Erro no rollback tentativa {attempt + 1}: {e}"
                )

            if attempt < max_attempts - 1:
                # Aguarda antes de tentar novamente
                await asyncio.sleep(2 ** attempt)  # Backoff exponencial

        if not rollback_result.success:
            rollback_result.status = DeployStatus.FAILED
            rollback_result.errors.append(
                f"Rollback falhou apos {max_attempts} tentativas"
            )

        rollback_result.completed_at = datetime.utcnow()

        # Registra no historico
        self._rollback_history.append({
            "timestamp": datetime.utcnow().isoformat(),
            "deploy_id": deploy_result.request_id,
            "backup_id": backup_id,
            "success": rollback_result.success,
            "attempts": len(rollback_result.steps)
        })

        return rollback_result

    async def _execute_rollback(
        self,
        backup: DeployBackup,
        deploy_result: DeployResult
    ):
        """
        Executa a restauracao do backup.

        Issue #332 - Terminal A: Restauracao por integracao
        - SAP: Transporte reverso ou reimplantacao
        - Salesforce: Metadata API deploy da versao anterior
        - Azure DevOps: Revert de commits
        """
        logger.info(f"Restaurando backup {backup.backup_id}")

        integration = backup.integration.lower()

        # Para cada artefato no backup, restaura
        for artifact in backup.artifacts:
            artifact_name = artifact.get("name", "unknown")
            artifact_type = artifact.get("type", "generic")
            artifact_content = artifact.get("content", "")
            artifact_metadata = artifact.get("metadata", {})

            logger.debug(f"Restaurando artefato: {artifact_name}")

            try:
                if integration in ("sap_s4", "sap_s4hana", "sap"):
                    await self._restore_sap_artifact(
                        artifact_name, artifact_type, artifact_content, artifact_metadata
                    )

                elif integration in ("salesforce", "sfdc"):
                    await self._restore_salesforce_artifact(
                        artifact_name, artifact_type, artifact_content, artifact_metadata
                    )

                elif integration in ("azure_devops", "azdo", "azure"):
                    await self._restore_azure_artifact(
                        artifact_name, artifact_type, artifact_content, artifact_metadata
                    )

                else:
                    logger.warning(f"Integracao {integration} nao suporta rollback automatico")
                    await asyncio.sleep(0.1)

            except Exception as e:
                logger.error(f"Erro ao restaurar artefato {artifact_name}: {e}")
                raise

        logger.info(f"Backup {backup.backup_id} restaurado")

    async def _restore_sap_artifact(
        self,
        name: str,
        artifact_type: str,
        content: str,
        metadata: dict
    ):
        """
        Restaura artefato SAP via transporte reverso.

        Estrategias:
        - CDS View: Reimplanta versao anterior
        - ABAP Class: Reimplanta versao anterior
        - Transporte: Importa transporte de rollback
        """
        try:
            from ..sap_s4hana import SAPS4HanaIntegration

            sap = SAPS4HanaIntegration.from_environment()
            if not sap:
                raise ValueError("Integracao SAP nao configurada")

            await sap.connect()

            logger.info(f"Restaurando SAP [{artifact_type}]: {name}")

            transport = metadata.get("original_transport")

            if artifact_type == "cds_view":
                await sap.deploy_cds_view(
                    view_name=name,
                    source=content,
                    package=metadata.get("package", "$TMP"),
                    transport=transport
                )

            elif artifact_type == "abap_class":
                await sap.deploy_abap_class(
                    class_name=name,
                    source=content,
                    package=metadata.get("package", "$TMP"),
                    transport=transport
                )

            elif artifact_type == "transport":
                if transport:
                    await sap.import_transport(transport)

            else:
                logger.warning(f"Rollback SAP para tipo {artifact_type} nao implementado")

        except ImportError:
            logger.error("Modulo SAP S/4 HANA nao disponivel para rollback")
        except Exception as e:
            logger.error(f"Erro no rollback SAP: {e}")
            raise

    async def _restore_salesforce_artifact(
        self,
        name: str,
        artifact_type: str,
        content: str,
        metadata: dict
    ):
        """
        Restaura artefato Salesforce via Metadata API.
        """
        try:
            from ..salesforce_connector import SalesforceConnector
            from ..salesforce import SalesforceConfig

            config = SalesforceConfig.from_env()
            if not config.username:
                raise ValueError("Connector Salesforce nao configurado")

            sf = SalesforceConnector(config)
            await sf.connect()

            logger.info(f"Restaurando Salesforce [{artifact_type}]: {name}")

            if artifact_type == "apex_class":
                await sf.deploy_apex_class(
                    class_name=name,
                    body=content,
                    api_version=metadata.get("api_version", "59.0")
                )

            elif artifact_type == "apex_trigger":
                await sf.deploy_apex_trigger(
                    trigger_name=name,
                    body=content,
                    sobject=metadata.get("sobject"),
                    api_version=metadata.get("api_version", "59.0")
                )

            elif artifact_type == "lwc":
                files = metadata.get("files", {})
                if files:
                    await sf.deploy_lwc(
                        component_name=name,
                        files=files,
                        api_version=metadata.get("api_version", "59.0")
                    )

            elif artifact_type == "flow":
                await sf.deploy_flow(
                    flow_name=name,
                    flow_definition=content
                )

            else:
                await sf.deploy_metadata(
                    metadata_type=artifact_type,
                    full_name=name,
                    content=content
                )

        except ImportError:
            logger.error("Modulo Salesforce nao disponivel para rollback")
        except Exception as e:
            logger.error(f"Erro no rollback Salesforce: {e}")
            raise

    async def _restore_azure_artifact(
        self,
        name: str,
        artifact_type: str,
        content: str,
        metadata: dict
    ):
        """
        Restaura artefato Azure DevOps via revert.
        """
        try:
            from ..azure_devops import AzureDevOpsIntegration, AzureDevOpsConfig

            config = AzureDevOpsConfig.from_env()
            if not config.is_valid():
                raise ValueError("Integracao Azure DevOps nao configurada")

            azure = AzureDevOpsIntegration(config)
            await azure.connect()

            logger.info(f"Restaurando Azure DevOps [{artifact_type}]: {name}")

            if artifact_type == "repository":
                commit_id = metadata.get("commit_id")
                repo_id = metadata.get("repo_id")
                if commit_id and repo_id:
                    await azure.revert_commit(
                        repo_id=repo_id,
                        commit_id=commit_id
                    )

            elif artifact_type == "work_item":
                work_item_id = metadata.get("work_item_id")
                original_state = metadata.get("original_state", {})
                if work_item_id and original_state:
                    await azure.update_work_item(
                        work_item_id=work_item_id,
                        fields=original_state
                    )

            elif artifact_type == "pipeline":
                logger.warning("Rollback de pipeline nao suportado")

            else:
                logger.warning(f"Rollback Azure DevOps para tipo {artifact_type} nao implementado")

        except ImportError:
            logger.error("Modulo Azure DevOps nao disponivel para rollback")
        except Exception as e:
            logger.error(f"Erro no rollback Azure DevOps: {e}")
            raise

    async def _fetch_backup(self, backup_id: str) -> Optional[DeployBackup]:
        """Busca backup do storage externo"""
        # TODO: Implementar busca de backup em storage persistente
        return None

    async def create_backup(
        self,
        integration: str,
        environment: str,
        request_id: str
    ) -> DeployBackup:
        """
        Cria backup do estado atual para rollback futuro.

        Args:
            integration: Nome da integracao
            environment: Ambiente
            request_id: ID do deploy associado

        Returns:
            Backup criado
        """
        backup = DeployBackup(
            request_id=request_id,
            tenant_id=self.config.tenant_id,
            integration=integration,
            environment=environment
        )

        # Coleta artefatos atuais
        artifacts = await self._collect_current_artifacts(integration, environment)
        backup.artifacts = artifacts

        # Armazena backup
        self._backups[backup.backup_id] = backup

        logger.info(
            f"Backup criado: {backup.backup_id} "
            f"({len(artifacts)} artefatos)"
        )

        return backup

    async def _collect_current_artifacts(
        self,
        integration: str,
        environment: str
    ) -> List[Dict]:
        """Coleta artefatos atuais para backup"""
        # TODO: Implementar coleta por integracao
        # - SAP: Exportar configuracoes/codigo atual
        # - Salesforce: Retrieve metadata
        # - Azure DevOps: Snapshot de estado

        return []

    def list_backups(
        self,
        integration: Optional[str] = None,
        limit: int = 20
    ) -> List[DeployBackup]:
        """Lista backups disponiveis"""
        backups = list(self._backups.values())

        if integration:
            backups = [b for b in backups if b.integration == integration]

        # Ordena por data
        backups.sort(key=lambda x: x.created_at, reverse=True)

        # Limita quantidade
        return backups[:limit]

    def get_backup(self, backup_id: str) -> Optional[DeployBackup]:
        """Obtem backup por ID"""
        return self._backups.get(backup_id)

    def delete_backup(self, backup_id: str) -> bool:
        """Remove backup"""
        if backup_id in self._backups:
            del self._backups[backup_id]
            logger.info(f"Backup removido: {backup_id}")
            return True
        return False

    async def cleanup_old_backups(self):
        """Remove backups expirados"""
        now = datetime.utcnow()
        expired = []

        for backup_id, backup in self._backups.items():
            if backup.expires_at and backup.expires_at < now:
                expired.append(backup_id)

        for backup_id in expired:
            self.delete_backup(backup_id)

        if expired:
            logger.info(f"Removidos {len(expired)} backups expirados")

    def get_rollback_history(self, limit: int = 50) -> List[Dict]:
        """Retorna historico de rollbacks"""
        return self._rollback_history[-limit:]

    async def restore_backup(self, backup_id: str) -> DeployResult:
        """
        Restaura backup especifico.

        Args:
            backup_id: ID do backup

        Returns:
            Resultado da restauracao
        """
        backup = self.get_backup(backup_id)
        if not backup:
            raise ValueError(f"Backup nao encontrado: {backup_id}")

        # Cria resultado dummy para rollback
        dummy_result = DeployResult(
            request_id=backup.request_id,
            tenant_id=self.config.tenant_id,
            backup_id=backup_id,
            rollback_available=True
        )

        return await self.rollback(dummy_result, backup_id)

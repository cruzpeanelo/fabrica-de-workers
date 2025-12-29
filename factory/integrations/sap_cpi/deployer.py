# -*- coding: utf-8 -*-
"""
SAP CPI Deployer
================

Deploy e operação de artefatos no SAP Cloud Platform Integration.

Funcionalidades:
- Deploy de iFlows para runtime
- Undeploy de iFlows
- Restart de iFlows
- Monitoramento de status de deploy
- Análise de logs de mensagens
- Gerenciamento de artefatos em runtime
"""

import logging
import asyncio
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, TYPE_CHECKING
from enum import Enum

if TYPE_CHECKING:
    from .client import SAPCPIClient

logger = logging.getLogger(__name__)


class DeployStatus(str, Enum):
    """Status de deploy"""
    PENDING = "PENDING"
    DEPLOYING = "DEPLOYING"
    DEPLOYED = "DEPLOYED"
    FAILED = "FAILED"
    UNDEPLOYING = "UNDEPLOYING"


class RuntimeStatus(str, Enum):
    """Status de runtime"""
    STARTED = "STARTED"
    STOPPED = "STOPPED"
    STARTING = "STARTING"
    STOPPING = "STOPPING"
    ERROR = "ERROR"


class MessageStatus(str, Enum):
    """Status de processamento de mensagem"""
    COMPLETED = "COMPLETED"
    FAILED = "FAILED"
    PROCESSING = "PROCESSING"
    RETRY = "RETRY"
    ESCALATED = "ESCALATED"
    DISCARDED = "DISCARDED"


@dataclass
class DeployResult:
    """Resultado de uma operação de deploy"""
    success: bool
    artifact_id: str
    status: DeployStatus
    version: str = ""
    deployed_by: str = ""
    deployed_on: str = ""
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    error_message: str = ""
    error_details: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "artifact_id": self.artifact_id,
            "status": self.status.value,
            "version": self.version,
            "deployed_by": self.deployed_by,
            "deployed_on": self.deployed_on,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "error_message": self.error_message,
            "error_details": self.error_details,
            "duration_seconds": (self.completed_at - self.started_at).total_seconds() if self.started_at and self.completed_at else None
        }


@dataclass
class MessageLog:
    """Log de mensagem processada"""
    message_guid: str
    correlation_id: str
    status: MessageStatus
    log_level: str = ""
    sender: str = ""
    receiver: str = ""
    integration_flow: str = ""
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    custom_header_properties: Dict[str, str] = field(default_factory=dict)
    error_message: str = ""

    @classmethod
    def from_odata(cls, data: Dict[str, Any]) -> "MessageLog":
        """Cria MessageLog a partir de resposta OData"""
        return cls(
            message_guid=data.get("MessageGuid", ""),
            correlation_id=data.get("CorrelationId", ""),
            status=MessageStatus(data.get("Status", "PROCESSING")),
            log_level=data.get("LogLevel", ""),
            sender=data.get("Sender", ""),
            receiver=data.get("Receiver", ""),
            integration_flow=data.get("IntegrationFlowName", ""),
            started_at=cls._parse_datetime(data.get("LogStart")),
            completed_at=cls._parse_datetime(data.get("LogEnd")),
            error_message=data.get("ErrorMessage", ""),
        )

    @staticmethod
    def _parse_datetime(value: Any) -> Optional[datetime]:
        """Converte valor OData para datetime"""
        if not value:
            return None
        if isinstance(value, str):
            if "/Date(" in value:
                try:
                    timestamp = int(value.replace("/Date(", "").replace(")/", "")) / 1000
                    return datetime.fromtimestamp(timestamp)
                except (ValueError, TypeError):
                    return None
            try:
                return datetime.fromisoformat(value.replace("Z", "+00:00"))
            except (ValueError, TypeError):
                return None
        return None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "message_guid": self.message_guid,
            "correlation_id": self.correlation_id,
            "status": self.status.value,
            "log_level": self.log_level,
            "sender": self.sender,
            "receiver": self.receiver,
            "integration_flow": self.integration_flow,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "error_message": self.error_message,
            "duration_ms": int((self.completed_at - self.started_at).total_seconds() * 1000) if self.started_at and self.completed_at else None
        }


class CPIDeployer:
    """
    Deployer de artefatos para SAP CPI.

    Exemplo:
    ```python
    deployer = CPIDeployer(client)

    # Deploy de iFlow
    result = await deployer.deploy_iflow("MyPackage", "MyIFlow")
    if result.success:
        print(f"Deploy concluído em {result.deployed_on}")
    else:
        print(f"Erro: {result.error_message}")

    # Verificar status
    status = await deployer.get_runtime_status("MyIFlow")
    print(f"Status: {status}")

    # Restart de iFlow
    await deployer.restart_iflow("MyIFlow")

    # Buscar logs de mensagens
    logs = await deployer.get_message_logs("MyIFlow", hours=24)
    for log in logs:
        print(f"{log.message_guid}: {log.status}")
    ```
    """

    # Endpoints
    DEPLOY_ENDPOINT = "/api/v1/IntegrationDesigntimeArtifacts(Id='{id}',Version='{version}')"
    RUNTIME_ENDPOINT = "/api/v1/IntegrationRuntimeArtifacts"
    BUILD_DEPLOY_ENDPOINT = "/api/v1/BuildAndDeployStatus"
    LOGS_ENDPOINT = "/api/v1/MessageProcessingLogs"
    LOG_RUNS_ENDPOINT = "/api/v1/MessageProcessingLogRuns"

    def __init__(self, client: "SAPCPIClient"):
        """
        Inicializa o deployer.

        Args:
            client: Cliente SAPCPIClient autenticado
        """
        self.client = client

    async def deploy_iflow(
        self,
        package_id: str,
        iflow_id: str,
        version: str = "Active",
        wait_completion: bool = True,
        timeout_seconds: int = 300
    ) -> DeployResult:
        """
        Faz deploy de um iFlow.

        Args:
            package_id: ID do package
            iflow_id: ID do iFlow
            version: Versão a deployer (default: Active)
            wait_completion: Aguardar conclusão do deploy
            timeout_seconds: Timeout em segundos

        Returns:
            DeployResult com resultado do deploy
        """
        result = DeployResult(
            success=False,
            artifact_id=iflow_id,
            status=DeployStatus.PENDING,
            started_at=datetime.utcnow()
        )

        try:
            # Inicia deploy
            endpoint = f"/api/v1/DeployIntegrationDesigntimeArtifact?Id='{iflow_id}'&Version='{version}'"

            logger.info(f"Iniciando deploy de {iflow_id}...")
            result.status = DeployStatus.DEPLOYING

            response = await self.client.post(endpoint)

            if response is None:
                result.error_message = self.client.last_error or "Falha ao iniciar deploy"
                result.status = DeployStatus.FAILED
                return result

            # Se não precisa aguardar, retorna
            if not wait_completion:
                result.status = DeployStatus.DEPLOYING
                result.success = True
                return result

            # Aguarda conclusão do deploy
            result = await self._wait_deploy_completion(
                iflow_id,
                result,
                timeout_seconds
            )

        except Exception as e:
            result.error_message = str(e)
            result.error_details = f"Exceção durante deploy: {type(e).__name__}"
            result.status = DeployStatus.FAILED
            logger.error(f"Erro no deploy de {iflow_id}: {e}")

        result.completed_at = datetime.utcnow()
        return result

    async def _wait_deploy_completion(
        self,
        iflow_id: str,
        result: DeployResult,
        timeout_seconds: int
    ) -> DeployResult:
        """Aguarda conclusão do deploy"""
        start_time = datetime.utcnow()
        check_interval = 5  # segundos

        while True:
            # Verifica timeout
            elapsed = (datetime.utcnow() - start_time).total_seconds()
            if elapsed > timeout_seconds:
                result.error_message = f"Timeout após {timeout_seconds} segundos"
                result.status = DeployStatus.FAILED
                break

            # Verifica status
            status = await self.get_runtime_status(iflow_id)

            if status is None:
                # Ainda não está no runtime, aguarda
                await asyncio.sleep(check_interval)
                continue

            runtime_status = status.get("status", "")

            if runtime_status == RuntimeStatus.STARTED.value:
                result.success = True
                result.status = DeployStatus.DEPLOYED
                result.deployed_by = status.get("deployed_by", "")
                result.deployed_on = status.get("deployed_on", "")
                result.version = status.get("version", "")
                logger.info(f"Deploy de {iflow_id} concluído com sucesso")
                break

            elif runtime_status == RuntimeStatus.ERROR.value:
                result.success = False
                result.status = DeployStatus.FAILED
                result.error_message = status.get("error_info", "Erro no deploy")
                logger.error(f"Deploy de {iflow_id} falhou: {result.error_message}")
                break

            elif runtime_status in [RuntimeStatus.STARTING.value, RuntimeStatus.STOPPING.value]:
                # Ainda em progresso
                await asyncio.sleep(check_interval)
                continue

            else:
                # Status desconhecido, aguarda
                await asyncio.sleep(check_interval)

        return result

    async def undeploy_iflow(
        self,
        iflow_id: str,
        wait_completion: bool = True,
        timeout_seconds: int = 120
    ) -> DeployResult:
        """
        Remove deploy de um iFlow (undeploy).

        Args:
            iflow_id: ID do iFlow
            wait_completion: Aguardar conclusão
            timeout_seconds: Timeout em segundos

        Returns:
            DeployResult com resultado
        """
        result = DeployResult(
            success=False,
            artifact_id=iflow_id,
            status=DeployStatus.UNDEPLOYING,
            started_at=datetime.utcnow()
        )

        try:
            endpoint = f"{self.RUNTIME_ENDPOINT}('{iflow_id}')"

            logger.info(f"Iniciando undeploy de {iflow_id}...")

            success = await self.client.delete(endpoint)

            if not success:
                result.error_message = self.client.last_error or "Falha no undeploy"
                result.status = DeployStatus.FAILED
                return result

            if wait_completion:
                # Aguarda até que o artefato não esteja mais no runtime
                start_time = datetime.utcnow()
                while True:
                    elapsed = (datetime.utcnow() - start_time).total_seconds()
                    if elapsed > timeout_seconds:
                        result.error_message = f"Timeout após {timeout_seconds} segundos"
                        break

                    status = await self.get_runtime_status(iflow_id)
                    if status is None:
                        # Não está mais no runtime - sucesso
                        result.success = True
                        result.status = DeployStatus.PENDING
                        break

                    await asyncio.sleep(3)
            else:
                result.success = True

            logger.info(f"Undeploy de {iflow_id} concluído")

        except Exception as e:
            result.error_message = str(e)
            result.status = DeployStatus.FAILED
            logger.error(f"Erro no undeploy de {iflow_id}: {e}")

        result.completed_at = datetime.utcnow()
        return result

    async def restart_iflow(
        self,
        iflow_id: str,
        wait_completion: bool = True
    ) -> DeployResult:
        """
        Reinicia um iFlow (undeploy + deploy).

        Args:
            iflow_id: ID do iFlow
            wait_completion: Aguardar conclusão

        Returns:
            DeployResult com resultado
        """
        logger.info(f"Reiniciando iFlow {iflow_id}...")

        # Primeiro faz undeploy
        undeploy_result = await self.undeploy_iflow(iflow_id, wait_completion=True)

        if not undeploy_result.success and "não encontrado" not in (undeploy_result.error_message or "").lower():
            return undeploy_result

        # Pequena pausa entre operações
        await asyncio.sleep(2)

        # Depois faz deploy novamente
        # Nota: precisamos obter o package_id - aqui assumimos que está disponível
        # Em produção, seria necessário buscar essa informação
        deploy_result = await self.deploy_iflow(
            package_id="",  # API de restart não precisa do package
            iflow_id=iflow_id,
            wait_completion=wait_completion
        )

        return deploy_result

    async def get_runtime_status(
        self,
        iflow_id: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtém status de runtime de um iFlow.

        Args:
            iflow_id: ID do iFlow

        Returns:
            Dict com informações ou None se não encontrado
        """
        endpoint = f"{self.RUNTIME_ENDPOINT}('{iflow_id}')"

        response = await self.client.get(endpoint)

        if response is None:
            return None

        data = response.get("d", response)

        return {
            "id": data.get("Id", ""),
            "version": data.get("Version", ""),
            "name": data.get("Name", ""),
            "type": data.get("Type", ""),
            "deployed_by": data.get("DeployedBy", ""),
            "deployed_on": data.get("DeployedOn", ""),
            "status": data.get("Status", ""),
            "error_info": data.get("ErrorInformation", ""),
        }

    async def get_all_runtime_artifacts(
        self,
        top: int = 100,
        skip: int = 0,
        filter_status: Optional[RuntimeStatus] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista todos os artefatos no runtime.

        Args:
            top: Máximo de resultados
            skip: Pular N primeiros
            filter_status: Filtrar por status

        Returns:
            Lista de artefatos
        """
        params: Dict[str, Any] = {
            "$top": top,
            "$skip": skip,
        }

        if filter_status:
            params["$filter"] = f"Status eq '{filter_status.value}'"

        response = await self.client.get(self.RUNTIME_ENDPOINT, params=params)

        if response is None:
            return []

        results = response.get("d", {}).get("results", [])

        artifacts = []
        for item in results:
            artifacts.append({
                "id": item.get("Id", ""),
                "version": item.get("Version", ""),
                "name": item.get("Name", ""),
                "type": item.get("Type", ""),
                "deployed_by": item.get("DeployedBy", ""),
                "deployed_on": item.get("DeployedOn", ""),
                "status": item.get("Status", ""),
                "error_info": item.get("ErrorInformation", ""),
            })

        return artifacts

    async def get_message_logs(
        self,
        iflow_name: Optional[str] = None,
        status: Optional[MessageStatus] = None,
        hours: int = 24,
        top: int = 100
    ) -> List[MessageLog]:
        """
        Busca logs de mensagens processadas.

        Args:
            iflow_name: Nome do iFlow para filtrar
            status: Status para filtrar
            hours: Horas para buscar (default: 24)
            top: Máximo de resultados

        Returns:
            Lista de MessageLog
        """
        # Calcula data/hora de início
        start_time = datetime.utcnow() - timedelta(hours=hours)
        start_str = start_time.strftime("/Date(%d)/") % int(start_time.timestamp() * 1000)

        params: Dict[str, Any] = {
            "$top": top,
            "$orderby": "LogStart desc",
        }

        # Constrói filtros
        filters = [f"LogStart ge datetime'{start_time.isoformat()}'"]

        if iflow_name:
            filters.append(f"IntegrationFlowName eq '{iflow_name}'")

        if status:
            filters.append(f"Status eq '{status.value}'")

        if filters:
            params["$filter"] = " and ".join(filters)

        response = await self.client.get(self.LOGS_ENDPOINT, params=params)

        if response is None:
            return []

        results = response.get("d", {}).get("results", [])

        logs = []
        for item in results:
            log = MessageLog.from_odata(item)
            logs.append(log)

        return logs

    async def get_message_log_details(
        self,
        message_guid: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtém detalhes de um log de mensagem.

        Args:
            message_guid: GUID da mensagem

        Returns:
            Dict com detalhes ou None
        """
        endpoint = f"{self.LOGS_ENDPOINT}(MessageGuid='{message_guid}')"
        params = {"$expand": "MessageStoreEntries,Attachments,ErrorInformation"}

        response = await self.client.get(endpoint, params=params)

        if response is None:
            return None

        data = response.get("d", response)

        return {
            "message_guid": data.get("MessageGuid", ""),
            "correlation_id": data.get("CorrelationId", ""),
            "status": data.get("Status", ""),
            "log_level": data.get("LogLevel", ""),
            "integration_flow": data.get("IntegrationFlowName", ""),
            "sender": data.get("Sender", ""),
            "receiver": data.get("Receiver", ""),
            "log_start": data.get("LogStart"),
            "log_end": data.get("LogEnd"),
            "error_information": data.get("ErrorInformation", {}),
            "attachments": data.get("Attachments", {}).get("results", []),
            "message_store_entries": data.get("MessageStoreEntries", {}).get("results", []),
        }

    async def get_failed_messages(
        self,
        hours: int = 24,
        top: int = 100
    ) -> List[MessageLog]:
        """
        Busca mensagens com falha.

        Args:
            hours: Horas para buscar
            top: Máximo de resultados

        Returns:
            Lista de MessageLog com falha
        """
        return await self.get_message_logs(
            status=MessageStatus.FAILED,
            hours=hours,
            top=top
        )

    async def retry_failed_message(
        self,
        message_guid: str
    ) -> bool:
        """
        Reenvia uma mensagem com falha.

        Args:
            message_guid: GUID da mensagem

        Returns:
            bool: True se reenviado com sucesso

        Nota:
            Só funciona para mensagens que suportam retry (configuração do iFlow).
        """
        endpoint = f"{self.LOG_RUNS_ENDPOINT}('{message_guid}')/Retry"

        response = await self.client.post(endpoint)

        if response:
            logger.info(f"Mensagem {message_guid} reenviada")
            return True
        else:
            logger.error(f"Falha ao reenviar mensagem {message_guid}")
            return False

    async def get_deployment_statistics(self) -> Dict[str, Any]:
        """
        Retorna estatísticas de deploy.

        Returns:
            Dict com estatísticas
        """
        artifacts = await self.get_all_runtime_artifacts(top=1000)

        by_status: Dict[str, int] = {}
        by_type: Dict[str, int] = {}
        errors = []

        for artifact in artifacts:
            status = artifact.get("status", "Unknown")
            by_status[status] = by_status.get(status, 0) + 1

            artifact_type = artifact.get("type", "Unknown")
            by_type[artifact_type] = by_type.get(artifact_type, 0) + 1

            if status == RuntimeStatus.ERROR.value:
                errors.append({
                    "id": artifact.get("id"),
                    "name": artifact.get("name"),
                    "error": artifact.get("error_info")
                })

        return {
            "total_artifacts": len(artifacts),
            "by_status": by_status,
            "by_type": by_type,
            "errors": errors,
            "healthy_percentage": round(
                by_status.get(RuntimeStatus.STARTED.value, 0) / max(len(artifacts), 1) * 100,
                2
            )
        }

    async def get_message_statistics(
        self,
        hours: int = 24
    ) -> Dict[str, Any]:
        """
        Retorna estatísticas de mensagens.

        Args:
            hours: Período em horas

        Returns:
            Dict com estatísticas
        """
        logs = await self.get_message_logs(hours=hours, top=1000)

        by_status: Dict[str, int] = {}
        by_iflow: Dict[str, Dict[str, int]] = {}
        total_duration = 0
        completed_count = 0

        for log in logs:
            status = log.status.value
            by_status[status] = by_status.get(status, 0) + 1

            iflow = log.integration_flow
            if iflow not in by_iflow:
                by_iflow[iflow] = {"total": 0, "failed": 0}
            by_iflow[iflow]["total"] += 1
            if log.status == MessageStatus.FAILED:
                by_iflow[iflow]["failed"] += 1

            if log.started_at and log.completed_at:
                duration = (log.completed_at - log.started_at).total_seconds() * 1000
                total_duration += duration
                completed_count += 1

        avg_duration = total_duration / max(completed_count, 1)

        return {
            "period_hours": hours,
            "total_messages": len(logs),
            "by_status": by_status,
            "by_iflow": by_iflow,
            "average_duration_ms": round(avg_duration, 2),
            "success_rate": round(
                by_status.get(MessageStatus.COMPLETED.value, 0) / max(len(logs), 1) * 100,
                2
            )
        }

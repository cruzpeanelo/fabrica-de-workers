# -*- coding: utf-8 -*-
"""
SAP CPI Integration
===================

Classe principal de integração com SAP Cloud Platform Integration.

Combina todos os componentes:
- Client: Comunicação HTTP/OAuth
- PackageManager: Gerenciamento de packages
- IFlowManager: Gerenciamento de iFlows
- CPIDeployer: Deploy e monitoramento
- Analyzers: Análise de artefatos
- Generators: Geração de artefatos
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional

from ..base import IntegrationBase, IntegrationStatus, SyncResult
from .client import SAPCPIClient, SAPCPIConfig
from .package_manager import PackageManager, IntegrationPackage
from .iflow_manager import IFlowManager, IntegrationFlow
from .deployer import CPIDeployer, DeployResult, MessageLog

logger = logging.getLogger(__name__)


class SAPCPIIntegration(IntegrationBase):
    """
    Integração completa com SAP Cloud Platform Integration.

    Esta classe fornece acesso a todas as funcionalidades do SAP CPI:
    - Gerenciamento de packages e iFlows
    - Deploy e operação de integrações
    - Análise e geração de artefatos
    - Monitoramento de mensagens

    Exemplo:
    ```python
    from factory.integrations.sap_cpi import SAPCPIIntegration, SAPCPIConfig

    # Configuração via ambiente ou manual
    config = SAPCPIConfig.from_env()
    # ou
    config = SAPCPIConfig(
        tenant_url="https://xxx.it-cpi001.cfapps.us10.hana.ondemand.com",
        token_url="https://xxx.authentication.us10.hana.ondemand.com/oauth/token",
        client_id="seu-client-id",
        client_secret="seu-client-secret",
        enabled=True
    )

    # Cria integração
    cpi = SAPCPIIntegration(config)

    # Conecta
    if await cpi.connect():
        # Lista packages
        packages = await cpi.package_manager.list_packages()

        # Lista iFlows
        iflows = await cpi.iflow_manager.list_iflows()

        # Faz deploy
        result = await cpi.deployer.deploy_iflow("MyPackage", "MyIFlow")

        # Monitora mensagens
        logs = await cpi.deployer.get_message_logs(hours=24)

    # Desconecta
    await cpi.disconnect()
    ```
    """

    def __init__(self, config: SAPCPIConfig):
        """
        Inicializa a integração SAP CPI.

        Args:
            config: Configuração SAPCPIConfig
        """
        super().__init__(config)
        self.config: SAPCPIConfig = config

        # Cliente HTTP
        self.client = SAPCPIClient(config)

        # Managers (inicializados após conexão)
        self._package_manager: Optional[PackageManager] = None
        self._iflow_manager: Optional[IFlowManager] = None
        self._deployer: Optional[CPIDeployer] = None

    @property
    def package_manager(self) -> PackageManager:
        """Gerenciador de packages"""
        if self._package_manager is None:
            self._package_manager = PackageManager(self.client)
        return self._package_manager

    @property
    def iflow_manager(self) -> IFlowManager:
        """Gerenciador de iFlows"""
        if self._iflow_manager is None:
            self._iflow_manager = IFlowManager(self.client)
        return self._iflow_manager

    @property
    def deployer(self) -> CPIDeployer:
        """Deployer de artefatos"""
        if self._deployer is None:
            self._deployer = CPIDeployer(self.client)
        return self._deployer

    async def connect(self) -> bool:
        """
        Conecta ao SAP CPI.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuração inválida. Verifique tenant_url, token_url, client_id e client_secret."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao SAP CPI: {self.config.tenant_url}")

        try:
            if await self.client.connect():
                self.status = IntegrationStatus.CONNECTED
                logger.info("Conectado ao SAP CPI com sucesso")
                return True
            else:
                self._last_error = self.client.last_error
                self.status = IntegrationStatus.ERROR
                return False

        except Exception as e:
            self._last_error = f"Erro ao conectar: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta do SAP CPI.

        Returns:
            bool: True se desconectado com sucesso
        """
        await self.client.disconnect()
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do SAP CPI")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexão com o SAP CPI.

        Returns:
            bool: True se a conexão está funcionando
        """
        return await self.client.test_connection()

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """
        Sincroniza stories para o SAP CPI (não aplicável).

        O SAP CPI é um middleware de integração, não um sistema de gestão
        de tarefas. Este método está implementado para conformidade com
        a interface base.

        Args:
            stories: Lista de stories

        Returns:
            SyncResult indicando que a operação não é suportada
        """
        return SyncResult(
            success=False,
            errors=["SAP CPI não suporta sincronização de stories"]
        )

    async def sync_from_external(self, project_id: str) -> SyncResult:
        """
        Sincroniza do SAP CPI para stories (não aplicável).

        O SAP CPI é um middleware de integração, não um sistema de gestão
        de tarefas. Este método está implementado para conformidade com
        a interface base.

        Args:
            project_id: ID do projeto

        Returns:
            SyncResult indicando que a operação não é suportada
        """
        return SyncResult(
            success=False,
            errors=["SAP CPI não suporta sincronização de stories"]
        )

    async def handle_webhook(self, payload: Dict) -> bool:
        """
        Processa webhook do SAP CPI.

        O SAP CPI pode enviar webhooks para eventos como:
        - Falha de mensagem
        - Alertas de monitoramento
        - Notificações de deploy

        Args:
            payload: Payload do webhook

        Returns:
            bool: True se processado com sucesso
        """
        try:
            event_type = payload.get("eventType", "")
            logger.info(f"Webhook SAP CPI recebido: {event_type}")

            if event_type == "MessageFailed":
                # Processa falha de mensagem
                message_guid = payload.get("messageGuid")
                iflow_name = payload.get("integrationFlowName")
                error = payload.get("errorMessage")
                logger.warning(f"Mensagem falhou - iFlow: {iflow_name}, Erro: {error}")
                return True

            elif event_type == "ArtifactDeployed":
                # Processa notificação de deploy
                artifact_id = payload.get("artifactId")
                status = payload.get("status")
                logger.info(f"Artefato deployado: {artifact_id} - Status: {status}")
                return True

            elif event_type == "Alert":
                # Processa alerta
                alert_type = payload.get("alertType")
                message = payload.get("message")
                logger.warning(f"Alerta CPI [{alert_type}]: {message}")
                return True

            logger.debug(f"Webhook não tratado: {event_type}")
            return True

        except Exception as e:
            logger.error(f"Erro ao processar webhook: {e}")
            return False

    def get_status(self) -> Dict[str, Any]:
        """
        Retorna status detalhado da integração.

        Returns:
            Dict com informações de status
        """
        base_status = super().get_status()
        base_status.update({
            "system": "sap_cpi",
            "tenant_url": self.config.tenant_url,
            "api_version": self.config.api_version,
            "token_valid": self.client._token is not None and not self.client._token.is_expired if self.client._token else False,
        })
        return base_status

    # Métodos de conveniência

    async def list_all_packages(self) -> List[IntegrationPackage]:
        """Lista todos os packages"""
        return await self.package_manager.list_packages()

    async def list_all_iflows(self, package_id: Optional[str] = None) -> List[IntegrationFlow]:
        """Lista todos os iFlows"""
        return await self.iflow_manager.list_iflows(package_id=package_id)

    async def deploy_iflow(
        self,
        package_id: str,
        iflow_id: str,
        wait_completion: bool = True
    ) -> DeployResult:
        """
        Faz deploy de um iFlow.

        Args:
            package_id: ID do package
            iflow_id: ID do iFlow
            wait_completion: Aguardar conclusão

        Returns:
            DeployResult
        """
        return await self.deployer.deploy_iflow(
            package_id=package_id,
            iflow_id=iflow_id,
            wait_completion=wait_completion
        )

    async def undeploy_iflow(self, iflow_id: str) -> DeployResult:
        """
        Remove deploy de um iFlow.

        Args:
            iflow_id: ID do iFlow

        Returns:
            DeployResult
        """
        return await self.deployer.undeploy_iflow(iflow_id)

    async def get_runtime_status(self, iflow_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtém status de runtime de um iFlow.

        Args:
            iflow_id: ID do iFlow

        Returns:
            Dict com status ou None
        """
        return await self.deployer.get_runtime_status(iflow_id)

    async def get_message_logs(
        self,
        iflow_name: Optional[str] = None,
        hours: int = 24,
        top: int = 100
    ) -> List[MessageLog]:
        """
        Busca logs de mensagens.

        Args:
            iflow_name: Nome do iFlow para filtrar
            hours: Período em horas
            top: Máximo de resultados

        Returns:
            Lista de MessageLog
        """
        return await self.deployer.get_message_logs(
            iflow_name=iflow_name,
            hours=hours,
            top=top
        )

    async def get_health_status(self) -> Dict[str, Any]:
        """
        Obtém status de saúde geral do tenant CPI.

        Returns:
            Dict com métricas de saúde
        """
        try:
            deployment_stats = await self.deployer.get_deployment_statistics()
            message_stats = await self.deployer.get_message_statistics(hours=24)

            return {
                "status": "healthy" if deployment_stats.get("healthy_percentage", 0) > 90 else "degraded",
                "timestamp": datetime.utcnow().isoformat(),
                "tenant_url": self.config.tenant_url,
                "deployments": deployment_stats,
                "messages_24h": message_stats,
                "alerts": {
                    "failed_artifacts": len(deployment_stats.get("errors", [])),
                    "failed_messages": message_stats.get("by_status", {}).get("FAILED", 0),
                }
            }
        except Exception as e:
            return {
                "status": "error",
                "error": str(e),
                "timestamp": datetime.utcnow().isoformat(),
            }


# Instância global (singleton)
_sap_cpi_instance: Optional[SAPCPIIntegration] = None


def get_sap_cpi_integration() -> SAPCPIIntegration:
    """
    Retorna instância global da integração SAP CPI.

    Returns:
        SAPCPIIntegration
    """
    global _sap_cpi_instance
    if _sap_cpi_instance is None:
        config = SAPCPIConfig.from_env()
        _sap_cpi_instance = SAPCPIIntegration(config)
    return _sap_cpi_instance


async def init_sap_cpi_integration() -> Optional[SAPCPIIntegration]:
    """
    Inicializa e conecta a integração SAP CPI se configurada.

    Returns:
        SAPCPIIntegration conectada ou None se não configurada
    """
    cpi = get_sap_cpi_integration()
    if cpi.config.is_valid() and cpi.config.enabled:
        if await cpi.connect():
            return cpi
    return None

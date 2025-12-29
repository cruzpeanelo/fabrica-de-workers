# -*- coding: utf-8 -*-
"""
SAP CPI Deploy Skill
====================

Skill para deploy e operação de integrações SAP CPI.

Funcionalidades:
- Deploy de iFlows
- Undeploy de iFlows
- Restart de iFlows
- Monitoramento de status
- Análise de logs
- Retry de mensagens com falha
"""

import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from ..sap_cpi import SAPCPIIntegration, get_sap_cpi_integration
from ..deployer import DeployResult, RuntimeStatus, MessageStatus

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado de execução de skill"""
    success: bool
    data: Any = None
    message: str = ""
    error: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "error": self.error
        }


class CPIDeploySkill:
    """
    Skill para deploy e operação de integrações SAP CPI.

    Esta skill permite que agentes IA façam deploy, undeploy,
    restart e monitorem integrações.

    Exemplo:
    ```python
    skill = CPIDeploySkill()

    # Conecta ao CPI
    await skill.connect()

    # Deploy de iFlow
    result = await skill.deploy_iflow("MyPackage", "MyIFlow")
    if result.success:
        print(f"Deploy concluído: {result.data['status']}")

    # Verifica status
    result = await skill.get_runtime_status("MyIFlow")
    print(f"Status: {result.data['status']}")

    # Busca mensagens com falha
    result = await skill.get_failed_messages(hours=24)
    for msg in result.data:
        print(f"Falha: {msg['correlation_id']} - {msg['error_message']}")
    ```
    """

    def __init__(self, integration: Optional[SAPCPIIntegration] = None):
        """
        Inicializa a skill.

        Args:
            integration: Integração SAP CPI (usa global se não fornecida)
        """
        self.integration = integration or get_sap_cpi_integration()

    async def connect(self) -> SkillResult:
        """Conecta ao SAP CPI"""
        try:
            if await self.integration.connect():
                return SkillResult(
                    success=True,
                    message="Conectado ao SAP CPI"
                )
            else:
                return SkillResult(
                    success=False,
                    error=self.integration.last_error or "Falha na conexão"
                )
        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao conectar: {str(e)}"
            )

    async def deploy_iflow(
        self,
        package_id: str,
        iflow_id: str,
        version: str = "Active",
        wait_completion: bool = True,
        timeout_seconds: int = 300
    ) -> SkillResult:
        """
        Faz deploy de um iFlow.

        Args:
            package_id: ID do package
            iflow_id: ID do iFlow
            version: Versão a deployer
            wait_completion: Aguardar conclusão
            timeout_seconds: Timeout em segundos

        Returns:
            SkillResult com resultado do deploy
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            result = await self.integration.deployer.deploy_iflow(
                package_id=package_id,
                iflow_id=iflow_id,
                version=version,
                wait_completion=wait_completion,
                timeout_seconds=timeout_seconds
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Deploy {'concluído' if result.success else 'falhou'}: {iflow_id}",
                error=result.error_message if not result.success else ""
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro no deploy: {str(e)}"
            )

    async def undeploy_iflow(
        self,
        iflow_id: str,
        wait_completion: bool = True,
        timeout_seconds: int = 120
    ) -> SkillResult:
        """
        Remove deploy de um iFlow.

        Args:
            iflow_id: ID do iFlow
            wait_completion: Aguardar conclusão
            timeout_seconds: Timeout em segundos

        Returns:
            SkillResult com resultado
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            result = await self.integration.deployer.undeploy_iflow(
                iflow_id=iflow_id,
                wait_completion=wait_completion,
                timeout_seconds=timeout_seconds
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Undeploy {'concluído' if result.success else 'falhou'}: {iflow_id}",
                error=result.error_message if not result.success else ""
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro no undeploy: {str(e)}"
            )

    async def restart_iflow(
        self,
        iflow_id: str,
        wait_completion: bool = True
    ) -> SkillResult:
        """
        Reinicia um iFlow (undeploy + deploy).

        Args:
            iflow_id: ID do iFlow
            wait_completion: Aguardar conclusão

        Returns:
            SkillResult com resultado
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            result = await self.integration.deployer.restart_iflow(
                iflow_id=iflow_id,
                wait_completion=wait_completion
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Restart {'concluído' if result.success else 'falhou'}: {iflow_id}",
                error=result.error_message if not result.success else ""
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro no restart: {str(e)}"
            )

    async def get_runtime_status(self, iflow_id: str) -> SkillResult:
        """
        Obtém status de runtime de um iFlow.

        Args:
            iflow_id: ID do iFlow

        Returns:
            SkillResult com status
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            status = await self.integration.deployer.get_runtime_status(iflow_id)

            if status is None:
                return SkillResult(
                    success=False,
                    error=f"iFlow não encontrado no runtime: {iflow_id}"
                )

            return SkillResult(
                success=True,
                data=status,
                message=f"Status: {status.get('status', 'desconhecido')}"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter status: {str(e)}"
            )

    async def list_runtime_artifacts(
        self,
        status_filter: Optional[str] = None,
        limit: int = 100
    ) -> SkillResult:
        """
        Lista artefatos no runtime.

        Args:
            status_filter: Filtrar por status (STARTED, STOPPED, ERROR)
            limit: Máximo de resultados

        Returns:
            SkillResult com lista de artefatos
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            filter_status = None
            if status_filter:
                try:
                    filter_status = RuntimeStatus(status_filter)
                except ValueError:
                    pass

            artifacts = await self.integration.deployer.get_all_runtime_artifacts(
                top=limit,
                filter_status=filter_status
            )

            return SkillResult(
                success=True,
                data=artifacts,
                message=f"Encontrados {len(artifacts)} artefatos"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao listar artefatos: {str(e)}"
            )

    async def get_deployment_statistics(self) -> SkillResult:
        """
        Obtém estatísticas de deploy.

        Returns:
            SkillResult com estatísticas
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            stats = await self.integration.deployer.get_deployment_statistics()

            return SkillResult(
                success=True,
                data=stats,
                message=f"Total: {stats['total_artifacts']} artefatos, {stats['healthy_percentage']}% saudáveis"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter estatísticas: {str(e)}"
            )

    async def get_message_logs(
        self,
        iflow_name: Optional[str] = None,
        status: Optional[str] = None,
        hours: int = 24,
        limit: int = 100
    ) -> SkillResult:
        """
        Busca logs de mensagens processadas.

        Args:
            iflow_name: Nome do iFlow para filtrar
            status: Status para filtrar (COMPLETED, FAILED, etc)
            hours: Período em horas
            limit: Máximo de resultados

        Returns:
            SkillResult com logs de mensagens
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            filter_status = None
            if status:
                try:
                    filter_status = MessageStatus(status)
                except ValueError:
                    pass

            logs = await self.integration.deployer.get_message_logs(
                iflow_name=iflow_name,
                status=filter_status,
                hours=hours,
                top=limit
            )

            data = [log.to_dict() for log in logs]

            return SkillResult(
                success=True,
                data=data,
                message=f"Encontrados {len(data)} logs"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao buscar logs: {str(e)}"
            )

    async def get_failed_messages(
        self,
        hours: int = 24,
        limit: int = 100
    ) -> SkillResult:
        """
        Busca mensagens com falha.

        Args:
            hours: Período em horas
            limit: Máximo de resultados

        Returns:
            SkillResult com mensagens que falharam
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            logs = await self.integration.deployer.get_failed_messages(
                hours=hours,
                top=limit
            )

            data = [log.to_dict() for log in logs]

            return SkillResult(
                success=True,
                data=data,
                message=f"Encontradas {len(data)} mensagens com falha nas últimas {hours}h"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao buscar falhas: {str(e)}"
            )

    async def get_message_details(self, message_guid: str) -> SkillResult:
        """
        Obtém detalhes de uma mensagem específica.

        Args:
            message_guid: GUID da mensagem

        Returns:
            SkillResult com detalhes da mensagem
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            details = await self.integration.deployer.get_message_log_details(message_guid)

            if details is None:
                return SkillResult(
                    success=False,
                    error=f"Mensagem não encontrada: {message_guid}"
                )

            return SkillResult(
                success=True,
                data=details,
                message=f"Detalhes obtidos para {message_guid}"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter detalhes: {str(e)}"
            )

    async def retry_failed_message(self, message_guid: str) -> SkillResult:
        """
        Reenvia uma mensagem com falha.

        Args:
            message_guid: GUID da mensagem

        Returns:
            SkillResult indicando sucesso ou erro
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            success = await self.integration.deployer.retry_failed_message(message_guid)

            return SkillResult(
                success=success,
                message=f"Mensagem {message_guid} {'reenviada' if success else 'não pôde ser reenviada'}"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao reenviar: {str(e)}"
            )

    async def get_message_statistics(self, hours: int = 24) -> SkillResult:
        """
        Obtém estatísticas de mensagens.

        Args:
            hours: Período em horas

        Returns:
            SkillResult com estatísticas
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            stats = await self.integration.deployer.get_message_statistics(hours=hours)

            return SkillResult(
                success=True,
                data=stats,
                message=f"Total: {stats['total_messages']} mensagens, {stats['success_rate']}% sucesso"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter estatísticas: {str(e)}"
            )

    async def get_iflows_with_errors(self) -> SkillResult:
        """
        Lista iFlows que estão com erro.

        Returns:
            SkillResult com lista de iFlows com erro
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            artifacts = await self.integration.deployer.get_all_runtime_artifacts(
                filter_status=RuntimeStatus.ERROR
            )

            return SkillResult(
                success=True,
                data=artifacts,
                message=f"Encontrados {len(artifacts)} iFlows com erro"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao buscar iFlows: {str(e)}"
            )

    async def get_health_report(self) -> SkillResult:
        """
        Gera relatório de saúde completo.

        Returns:
            SkillResult com relatório de saúde
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            health = await self.integration.get_health_status()

            # Gera resumo textual
            summary_lines = [
                f"Status Geral: {health.get('status', 'desconhecido').upper()}",
                f"Tenant: {health.get('tenant_url', 'N/A')}",
                "",
                "DEPLOYMENTS:",
            ]

            deployments = health.get("deployments", {})
            summary_lines.append(f"  Total de artefatos: {deployments.get('total_artifacts', 0)}")
            summary_lines.append(f"  Saudáveis: {deployments.get('healthy_percentage', 0)}%")

            by_status = deployments.get("by_status", {})
            for status, count in by_status.items():
                summary_lines.append(f"    {status}: {count}")

            summary_lines.append("")
            summary_lines.append("MENSAGENS (últimas 24h):")

            messages = health.get("messages_24h", {})
            summary_lines.append(f"  Total: {messages.get('total_messages', 0)}")
            summary_lines.append(f"  Taxa de sucesso: {messages.get('success_rate', 0)}%")
            summary_lines.append(f"  Tempo médio: {messages.get('average_duration_ms', 0):.0f}ms")

            alerts = health.get("alerts", {})
            if alerts.get("failed_artifacts") or alerts.get("failed_messages"):
                summary_lines.append("")
                summary_lines.append("ALERTAS:")
                if alerts.get("failed_artifacts"):
                    summary_lines.append(f"  Artefatos com falha: {alerts['failed_artifacts']}")
                if alerts.get("failed_messages"):
                    summary_lines.append(f"  Mensagens com falha: {alerts['failed_messages']}")

            health["summary"] = "\n".join(summary_lines)

            return SkillResult(
                success=True,
                data=health,
                message=f"Status: {health.get('status', 'desconhecido')}"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar relatório: {str(e)}"
            )

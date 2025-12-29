# -*- coding: utf-8 -*-
"""
SAP CPI Read Skill
==================

Skill para leitura e análise de artefatos SAP CPI.

Funcionalidades:
- Listar packages e iFlows
- Analisar estrutura de iFlows
- Analisar mapeamentos
- Analisar scripts Groovy
- Buscar informações de runtime
- Consultar logs de mensagens
"""

import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from ..sap_cpi import SAPCPIIntegration, get_sap_cpi_integration
from ..analyzers import IFlowAnalyzer, MappingAnalyzer, ScriptAnalyzer

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


class CPIReadSkill:
    """
    Skill para leitura e análise de artefatos SAP CPI.

    Esta skill permite que agentes IA leiam e analisem artefatos
    do SAP CPI como packages, iFlows, mapeamentos e scripts.

    Exemplo:
    ```python
    skill = CPIReadSkill()

    # Conecta ao CPI
    await skill.connect()

    # Lista packages
    result = await skill.list_packages()
    if result.success:
        for pkg in result.data:
            print(f"Package: {pkg['name']}")

    # Analisa um iFlow
    result = await skill.analyze_iflow("MyPackage", "MyIFlow")
    if result.success:
        analysis = result.data
        print(f"Steps: {analysis['total_steps']}")
        print(f"Warnings: {len(analysis['warnings'])}")
    ```
    """

    def __init__(self, integration: Optional[SAPCPIIntegration] = None):
        """
        Inicializa a skill.

        Args:
            integration: Integração SAP CPI (usa global se não fornecida)
        """
        self.integration = integration or get_sap_cpi_integration()
        self.iflow_analyzer = IFlowAnalyzer()
        self.mapping_analyzer = MappingAnalyzer()
        self.script_analyzer = ScriptAnalyzer()

    async def connect(self) -> SkillResult:
        """
        Conecta ao SAP CPI.

        Returns:
            SkillResult indicando sucesso ou erro
        """
        try:
            if await self.integration.connect():
                return SkillResult(
                    success=True,
                    message="Conectado ao SAP CPI com sucesso"
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

    async def list_packages(
        self,
        search: Optional[str] = None,
        limit: int = 100
    ) -> SkillResult:
        """
        Lista packages disponíveis no CPI.

        Args:
            search: Texto para buscar (opcional)
            limit: Máximo de resultados

        Returns:
            SkillResult com lista de packages
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            if search:
                packages = await self.integration.package_manager.search_packages(search)
            else:
                packages = await self.integration.package_manager.list_packages(top=limit)

            data = [pkg.to_dict() for pkg in packages]

            return SkillResult(
                success=True,
                data=data,
                message=f"Encontrados {len(data)} packages"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao listar packages: {str(e)}"
            )

    async def get_package(self, package_id: str) -> SkillResult:
        """
        Obtém detalhes de um package específico.

        Args:
            package_id: ID do package

        Returns:
            SkillResult com detalhes do package
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            package = await self.integration.package_manager.get_package(
                package_id,
                expand_artifacts=True
            )

            if package is None:
                return SkillResult(
                    success=False,
                    error=f"Package não encontrado: {package_id}"
                )

            # Busca artefatos do package
            artifacts = await self.integration.package_manager.get_package_artifacts(package_id)

            data = package.to_dict()
            data["artifacts"] = artifacts

            return SkillResult(
                success=True,
                data=data,
                message=f"Package {package_id} com {len(artifacts)} artefatos"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter package: {str(e)}"
            )

    async def list_iflows(
        self,
        package_id: Optional[str] = None,
        search: Optional[str] = None,
        limit: int = 100
    ) -> SkillResult:
        """
        Lista iFlows disponíveis.

        Args:
            package_id: Filtrar por package (opcional)
            search: Texto para buscar (opcional)
            limit: Máximo de resultados

        Returns:
            SkillResult com lista de iFlows
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            if search:
                iflows = await self.integration.iflow_manager.search_iflows(
                    search,
                    package_id=package_id
                )
            else:
                iflows = await self.integration.iflow_manager.list_iflows(
                    package_id=package_id,
                    top=limit
                )

            data = [iflow.to_dict() for iflow in iflows]

            return SkillResult(
                success=True,
                data=data,
                message=f"Encontrados {len(data)} iFlows"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao listar iFlows: {str(e)}"
            )

    async def get_iflow(
        self,
        iflow_id: str,
        version: str = "Active"
    ) -> SkillResult:
        """
        Obtém detalhes de um iFlow específico.

        Args:
            iflow_id: ID do iFlow
            version: Versão (default: Active)

        Returns:
            SkillResult com detalhes do iFlow
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            iflow = await self.integration.iflow_manager.get_iflow(
                iflow_id,
                version=version,
                expand_configurations=True
            )

            if iflow is None:
                return SkillResult(
                    success=False,
                    error=f"iFlow não encontrado: {iflow_id}"
                )

            # Busca status de runtime
            runtime_status = await self.integration.deployer.get_runtime_status(iflow_id)

            data = iflow.to_dict()
            data["runtime"] = runtime_status

            return SkillResult(
                success=True,
                data=data,
                message=f"iFlow {iflow_id} obtido com sucesso"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter iFlow: {str(e)}"
            )

    async def analyze_iflow(
        self,
        package_id: str,
        iflow_id: str,
        version: str = "Active"
    ) -> SkillResult:
        """
        Analisa estrutura e qualidade de um iFlow.

        Args:
            package_id: ID do package
            iflow_id: ID do iFlow
            version: Versão (default: Active)

        Returns:
            SkillResult com análise detalhada
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            # Faz download do iFlow
            content = await self.integration.iflow_manager.download_iflow(
                iflow_id,
                version=version
            )

            if content is None:
                return SkillResult(
                    success=False,
                    error=f"Não foi possível baixar iFlow: {iflow_id}"
                )

            # Analisa o conteúdo
            analysis = self.iflow_analyzer.analyze_from_zip(content)

            # Gera diagrama de fluxo
            diagram = self.iflow_analyzer.get_flow_diagram(analysis)

            data = analysis.to_dict()
            data["diagram"] = diagram
            data["summary"] = analysis.get_summary()

            return SkillResult(
                success=True,
                data=data,
                message=f"Análise concluída: {len(analysis.steps)} steps, {len(analysis.warnings)} avisos"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao analisar iFlow: {str(e)}"
            )

    async def analyze_script(
        self,
        script_content: str,
        script_name: str = "script.groovy"
    ) -> SkillResult:
        """
        Analisa um script Groovy.

        Args:
            script_content: Conteúdo do script
            script_name: Nome do script

        Returns:
            SkillResult com análise do script
        """
        try:
            analysis = self.script_analyzer.analyze(script_content, script_name)

            return SkillResult(
                success=True,
                data=analysis.to_dict(),
                message=f"Script analisado: {len(analysis.functions)} funções, score de risco: {analysis._calculate_risk_score()}"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao analisar script: {str(e)}"
            )

    async def get_runtime_status(self, iflow_id: str) -> SkillResult:
        """
        Obtém status de runtime de um iFlow.

        Args:
            iflow_id: ID do iFlow

        Returns:
            SkillResult com status de runtime
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
                    error=f"iFlow não está deployado: {iflow_id}"
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

            from ..deployer import RuntimeStatus

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
                message=f"Encontrados {len(artifacts)} artefatos em runtime"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao listar artefatos: {str(e)}"
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
            iflow_name: Nome do iFlow para filtrar (opcional)
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

            from ..deployer import MessageStatus

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
                message=f"Encontrados {len(data)} logs de mensagens"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao buscar logs: {str(e)}"
            )

    async def get_health_status(self) -> SkillResult:
        """
        Obtém status de saúde geral do tenant CPI.

        Returns:
            SkillResult com métricas de saúde
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            health = await self.integration.get_health_status()

            return SkillResult(
                success=True,
                data=health,
                message=f"Status: {health.get('status', 'desconhecido')}"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter status de saúde: {str(e)}"
            )

    async def get_statistics(self) -> SkillResult:
        """
        Obtém estatísticas de uso do CPI.

        Returns:
            SkillResult com estatísticas
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            # Coleta estatísticas de várias fontes
            pkg_stats = await self.integration.package_manager.get_statistics()
            iflow_stats = await self.integration.iflow_manager.get_statistics()
            deploy_stats = await self.integration.deployer.get_deployment_statistics()
            msg_stats = await self.integration.deployer.get_message_statistics(hours=24)

            data = {
                "packages": pkg_stats,
                "iflows": iflow_stats,
                "deployments": deploy_stats,
                "messages_24h": msg_stats
            }

            return SkillResult(
                success=True,
                data=data,
                message="Estatísticas coletadas com sucesso"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao obter estatísticas: {str(e)}"
            )

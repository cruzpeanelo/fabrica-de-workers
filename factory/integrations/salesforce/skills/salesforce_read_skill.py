# -*- coding: utf-8 -*-
"""
Salesforce Read Skill
=====================
Skill para leitura e analise de metadados Salesforce.

Funcionalidades:
- Analise de objetos
- Analise de campos
- Analise de relacionamentos
- Analise de flows
- Geracao de documentacao

Suporta isolamento multi-tenant atraves do tenant_id do SalesforceClient (Issue #314).

Uso pelos agentes:
    from factory.integrations.salesforce import SalesforceClient, SalesforceConfig
    from factory.integrations.salesforce.skills import SalesforceReadSkill

    config = SalesforceConfig(
        tenant_id="TENANT-001",
        username="user@empresa.com",
        password="senha123",
        security_token="token"
    )
    sf_client = SalesforceClient(config)
    await sf_client.connect()

    skill = SalesforceReadSkill(sf_client)

    # Analisar objeto
    result = await skill.analyze_object("Account")

    # Listar objetos customizados
    objects = await skill.list_custom_objects()
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from ..client import SalesforceClient

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado de uma skill"""
    success: bool
    data: Any = None
    message: str = ""
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "errors": self.errors
        }


class SalesforceReadSkill:
    """
    Skill de leitura para Salesforce

    Fornece funcionalidades de leitura e analise
    para agentes especializados Salesforce.

    Herda o contexto de tenant do SalesforceClient para isolamento multi-tenant.
    """

    def __init__(self, sf_client: "SalesforceClient"):
        """
        Inicializa a skill

        Args:
            sf_client: SalesforceClient autenticado (deve ter tenant_id configurado)
        """
        self.sf = sf_client
        self._object_analyzer = None
        self._apex_analyzer = None
        self._flow_analyzer = None
        self._lwc_analyzer = None

    @property
    def tenant_id(self) -> str:
        """ID do tenant para isolamento (herdado do SalesforceClient)"""
        return self.sf.tenant_id

    @property
    def object_analyzer(self):
        if self._object_analyzer is None:
            from ..analyzers import ObjectAnalyzer
            self._object_analyzer = ObjectAnalyzer(self.sf)
        return self._object_analyzer

    @property
    def apex_analyzer(self):
        if self._apex_analyzer is None:
            from ..analyzers import ApexAnalyzer
            self._apex_analyzer = ApexAnalyzer(self.sf)
        return self._apex_analyzer

    @property
    def flow_analyzer(self):
        if self._flow_analyzer is None:
            from ..analyzers import FlowAnalyzer
            self._flow_analyzer = FlowAnalyzer(self.sf)
        return self._flow_analyzer

    @property
    def lwc_analyzer(self):
        if self._lwc_analyzer is None:
            from ..analyzers import LWCAnalyzer
            self._lwc_analyzer = LWCAnalyzer(self.sf)
        return self._lwc_analyzer

    # ==================== OBJETOS ====================

    async def analyze_object(self, sobject: str) -> SkillResult:
        """
        Analisa um objeto Salesforce

        Args:
            sobject: Nome do objeto

        Returns:
            SkillResult com analise detalhada
        """
        try:
            analysis = await self.object_analyzer.analyze(sobject)

            return SkillResult(
                success=True,
                data=analysis.to_dict(),
                message=f"Objeto {sobject} analisado: {analysis.total_fields} campos, "
                        f"{len(analysis.relationships)} relacionamentos"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar objeto {sobject}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao analisar objeto: {str(e)}",
                errors=[str(e)]
            )

    async def list_custom_objects(self) -> SkillResult:
        """
        Lista todos os objetos customizados

        Returns:
            SkillResult com lista de objetos
        """
        try:
            objects = await self.object_analyzer.list_all_objects(
                include_standard=False,
                include_custom=True
            )

            return SkillResult(
                success=True,
                data=objects,
                message=f"Encontrados {len(objects)} objetos customizados"
            )

        except Exception as e:
            logger.error(f"Erro ao listar objetos: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def list_all_objects(self) -> SkillResult:
        """
        Lista todos os objetos (standard e custom)

        Returns:
            SkillResult com lista de objetos
        """
        try:
            objects = await self.object_analyzer.list_all_objects(
                include_standard=True,
                include_custom=True
            )

            return SkillResult(
                success=True,
                data=objects,
                message=f"Encontrados {len(objects)} objetos"
            )

        except Exception as e:
            logger.error(f"Erro ao listar objetos: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_object_fields(self, sobject: str) -> SkillResult:
        """
        Lista campos de um objeto

        Args:
            sobject: Nome do objeto

        Returns:
            SkillResult com lista de campos
        """
        try:
            fields = await self.sf.get_fields(sobject)

            # Simplificar resposta
            simplified = []
            for f in fields:
                simplified.append({
                    "name": f.get("name"),
                    "label": f.get("label"),
                    "type": f.get("type"),
                    "required": not f.get("nillable", True),
                    "custom": f.get("custom", False)
                })

            return SkillResult(
                success=True,
                data=simplified,
                message=f"Encontrados {len(simplified)} campos em {sobject}"
            )

        except Exception as e:
            logger.error(f"Erro ao listar campos: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_object_documentation(
        self,
        sobject: str,
        format: str = "markdown"
    ) -> SkillResult:
        """
        Gera documentacao de um objeto

        Args:
            sobject: Nome do objeto
            format: Formato (markdown)

        Returns:
            SkillResult com documentacao
        """
        try:
            doc = await self.object_analyzer.generate_documentation(sobject, format)

            return SkillResult(
                success=True,
                data=doc,
                message=f"Documentacao gerada para {sobject}"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar documentacao: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== APEX ====================

    async def analyze_apex_class(self, class_name: str) -> SkillResult:
        """
        Analisa uma classe Apex

        Args:
            class_name: Nome da classe

        Returns:
            SkillResult com analise
        """
        try:
            analysis = await self.apex_analyzer.analyze_class(class_name)

            return SkillResult(
                success=True,
                data={
                    "name": analysis.name,
                    "is_valid": analysis.is_valid,
                    "patterns": [p.value for p in analysis.patterns],
                    "metrics": {
                        "lines_of_code": analysis.metrics.lines_of_code,
                        "method_count": analysis.metrics.method_count,
                        "complexity": analysis.metrics.cyclomatic_complexity
                    },
                    "dependencies": analysis.dependencies,
                    "issues": [
                        {"severity": i.severity, "message": i.message, "line": i.line}
                        for i in analysis.issues
                    ],
                    "coverage_percentage": analysis.coverage_percentage
                },
                message=f"Classe {class_name} analisada: {len(analysis.methods)} metodos, "
                        f"{len(analysis.issues)} problemas identificados"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar classe {class_name}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def list_apex_classes(
        self,
        include_tests: bool = False
    ) -> SkillResult:
        """
        Lista classes Apex

        Args:
            include_tests: Incluir classes de teste

        Returns:
            SkillResult com lista de classes
        """
        try:
            classes = await self.apex_analyzer.list_all_classes(
                include_tests=include_tests
            )

            return SkillResult(
                success=True,
                data=classes,
                message=f"Encontradas {len(classes)} classes Apex"
            )

        except Exception as e:
            logger.error(f"Erro ao listar classes: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_apex_class_code(self, class_name: str) -> SkillResult:
        """
        Obtem codigo de uma classe Apex

        Args:
            class_name: Nome da classe

        Returns:
            SkillResult com codigo
        """
        try:
            from ..tooling_client import ToolingClient
            tooling = ToolingClient(self.sf)

            code = await tooling.get_apex_class_body(class_name)

            if code:
                return SkillResult(
                    success=True,
                    data=code,
                    message=f"Codigo da classe {class_name} obtido"
                )
            else:
                return SkillResult(
                    success=False,
                    message=f"Classe {class_name} nao encontrada"
                )

        except Exception as e:
            logger.error(f"Erro ao obter codigo: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_org_coverage(self) -> SkillResult:
        """
        Obtem coverage geral da org

        Returns:
            SkillResult com metricas de coverage
        """
        try:
            coverage = await self.apex_analyzer.get_org_coverage()

            return SkillResult(
                success=True,
                data=coverage,
                message=f"Coverage da org: {coverage['org_coverage_percentage']}%"
            )

        except Exception as e:
            logger.error(f"Erro ao obter coverage: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== FLOWS ====================

    async def list_flows(self, active_only: bool = False) -> SkillResult:
        """
        Lista flows da org

        Args:
            active_only: Apenas flows ativos

        Returns:
            SkillResult com lista de flows
        """
        try:
            flows = await self.flow_analyzer.list_flows(active_only=active_only)

            return SkillResult(
                success=True,
                data=flows,
                message=f"Encontrados {len(flows)} flows"
            )

        except Exception as e:
            logger.error(f"Erro ao listar flows: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def analyze_flow(self, flow_name: str) -> SkillResult:
        """
        Analisa um flow

        Args:
            flow_name: Nome do flow

        Returns:
            SkillResult com analise
        """
        try:
            analysis = await self.flow_analyzer.analyze(flow_name)

            return SkillResult(
                success=True,
                data=analysis.to_dict(),
                message=f"Flow {flow_name} analisado: {analysis.metrics.element_count} elementos, "
                        f"{len(analysis.issues)} problemas identificados"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar flow {flow_name}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_flow_documentation(
        self,
        flow_name: str,
        format: str = "markdown"
    ) -> SkillResult:
        """
        Gera documentacao de um flow

        Args:
            flow_name: Nome do flow
            format: Formato

        Returns:
            SkillResult com documentacao
        """
        try:
            doc = await self.flow_analyzer.generate_documentation(flow_name, format)

            return SkillResult(
                success=True,
                data=doc,
                message=f"Documentacao gerada para flow {flow_name}"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar documentacao: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== LWC ====================

    async def list_lwc_components(self) -> SkillResult:
        """
        Lista componentes LWC

        Returns:
            SkillResult com lista de componentes
        """
        try:
            components = await self.lwc_analyzer.list_components()

            return SkillResult(
                success=True,
                data=components,
                message=f"Encontrados {len(components)} componentes LWC"
            )

        except Exception as e:
            logger.error(f"Erro ao listar componentes: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def analyze_lwc(self, component_name: str) -> SkillResult:
        """
        Analisa um componente LWC

        Args:
            component_name: Nome do componente

        Returns:
            SkillResult com analise
        """
        try:
            analysis = await self.lwc_analyzer.analyze(component_name)

            return SkillResult(
                success=True,
                data=analysis.to_dict(),
                message=f"Componente {component_name} analisado: "
                        f"{analysis.metrics.property_count} propriedades, "
                        f"{analysis.metrics.method_count} metodos"
            )

        except Exception as e:
            logger.error(f"Erro ao analisar componente {component_name}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== QUERIES ====================

    async def query(self, soql: str) -> SkillResult:
        """
        Executa query SOQL

        Args:
            soql: Query SOQL

        Returns:
            SkillResult com registros
        """
        try:
            result = await self.sf.query(soql)

            return SkillResult(
                success=True,
                data={
                    "records": result.records,
                    "total_size": result.total_size,
                    "done": result.done
                },
                message=f"Query retornou {result.total_size} registro(s)"
            )

        except Exception as e:
            logger.error(f"Erro ao executar query: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def search(self, sosl: str) -> SkillResult:
        """
        Executa busca SOSL

        Args:
            sosl: Query SOSL

        Returns:
            SkillResult com resultados
        """
        try:
            results = await self.sf.search(sosl)

            return SkillResult(
                success=True,
                data=results,
                message=f"Busca retornou {len(results)} resultado(s)"
            )

        except Exception as e:
            logger.error(f"Erro ao executar busca: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_limits(self) -> SkillResult:
        """
        Obtem limites da org

        Returns:
            SkillResult com limites
        """
        try:
            limits = await self.sf.get_limits()

            return SkillResult(
                success=True,
                data=limits,
                message="Limites da org obtidos"
            )

        except Exception as e:
            logger.error(f"Erro ao obter limites: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

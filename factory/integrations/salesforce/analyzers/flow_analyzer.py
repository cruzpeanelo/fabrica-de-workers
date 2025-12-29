# -*- coding: utf-8 -*-
"""
Salesforce Flow Analyzer
========================
Analisador de Flows e Process Builders do Salesforce.

Funcionalidades:
- Analise de estrutura de flows
- Identificacao de elementos e conexoes
- Deteccao de problemas potenciais
- Analise de performance
- Geracao de documentacao

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient
    from factory.integrations.salesforce.analyzers import FlowAnalyzer

    sf = SalesforceClient(config)
    await sf.connect()

    analyzer = FlowAnalyzer(sf)

    # Listar flows
    flows = await analyzer.list_flows()

    # Analisar flow
    analysis = await analyzer.analyze("My_Account_Flow")
    print(f"Elementos: {analysis.element_count}")
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


class FlowType(str, Enum):
    """Tipos de Flow"""
    SCREEN_FLOW = "Screen Flow"
    AUTOLAUNCHED_FLOW = "Autolaunched Flow"
    RECORD_TRIGGERED_FLOW = "Record-Triggered Flow"
    SCHEDULED_FLOW = "Scheduled Flow"
    PLATFORM_EVENT_FLOW = "Platform Event-Triggered Flow"
    PROCESS_BUILDER = "Process Builder"


class FlowStatus(str, Enum):
    """Status do Flow"""
    ACTIVE = "Active"
    DRAFT = "Draft"
    OBSOLETE = "Obsolete"
    INVALID_DRAFT = "InvalidDraft"


class FlowElementType(str, Enum):
    """Tipos de elementos de Flow"""
    START = "Start"
    SCREEN = "Screen"
    DECISION = "Decision"
    ASSIGNMENT = "Assignment"
    RECORD_CREATE = "RecordCreate"
    RECORD_UPDATE = "RecordUpdate"
    RECORD_DELETE = "RecordDelete"
    RECORD_LOOKUP = "RecordLookup"
    LOOP = "Loop"
    SUBFLOW = "Subflow"
    ACTION_CALL = "ActionCall"
    APEX_CALL = "ApexCall"
    EMAIL_ALERT = "EmailAlert"
    WAIT = "Wait"
    PAUSE = "Pause"
    CUSTOM_ERROR = "CustomError"


@dataclass
class FlowElement:
    """Elemento de um Flow"""
    name: str
    type: str
    label: str
    description: Optional[str] = None
    connector: Optional[str] = None
    fault_connector: Optional[str] = None
    object_type: Optional[str] = None
    filters: List[Dict[str, Any]] = field(default_factory=list)
    input_assignments: List[Dict[str, Any]] = field(default_factory=list)
    output_assignments: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class FlowVariable:
    """Variavel de um Flow"""
    name: str
    data_type: str
    is_input: bool = False
    is_output: bool = False
    is_collection: bool = False
    object_type: Optional[str] = None
    default_value: Optional[str] = None


@dataclass
class FlowIssue:
    """Problema identificado no Flow"""
    severity: str
    message: str
    element: Optional[str] = None
    rule: str = ""
    suggestion: Optional[str] = None


@dataclass
class FlowMetrics:
    """Metricas do Flow"""
    element_count: int = 0
    screen_count: int = 0
    decision_count: int = 0
    loop_count: int = 0
    dml_count: int = 0
    soql_count: int = 0
    apex_call_count: int = 0
    subflow_count: int = 0
    variable_count: int = 0
    formula_count: int = 0
    max_depth: int = 0


@dataclass
class FlowAnalysis:
    """Resultado da analise de um Flow"""
    name: str
    api_name: str
    label: str
    flow_type: str
    status: str
    process_type: str
    api_version: str
    description: Optional[str] = None

    # Trigger (para Record-Triggered Flows)
    trigger_object: Optional[str] = None
    trigger_type: Optional[str] = None
    record_trigger_type: Optional[str] = None

    # Estrutura
    elements: List[FlowElement] = field(default_factory=list)
    variables: List[FlowVariable] = field(default_factory=list)
    formulas: List[Dict[str, Any]] = field(default_factory=list)

    # Analise
    metrics: FlowMetrics = field(default_factory=FlowMetrics)
    issues: List[FlowIssue] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)

    # Meta
    created_date: Optional[datetime] = None
    last_modified_date: Optional[datetime] = None
    analyzed_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        """Converte analise para dicionario"""
        return {
            "name": self.name,
            "api_name": self.api_name,
            "label": self.label,
            "flow_type": self.flow_type,
            "status": self.status,
            "trigger_object": self.trigger_object,
            "metrics": {
                "elements": self.metrics.element_count,
                "screens": self.metrics.screen_count,
                "decisions": self.metrics.decision_count,
                "loops": self.metrics.loop_count,
                "dml_operations": self.metrics.dml_count,
                "queries": self.metrics.soql_count,
                "apex_calls": self.metrics.apex_call_count,
                "subflows": self.metrics.subflow_count,
                "variables": self.metrics.variable_count
            },
            "issues_count": len(self.issues),
            "issues": [
                {"severity": i.severity, "message": i.message, "element": i.element}
                for i in self.issues
            ],
            "dependencies": self.dependencies,
            "analyzed_at": self.analyzed_at.isoformat() if self.analyzed_at else None
        }


class FlowAnalyzer:
    """
    Analisador de Flows Salesforce

    Fornece analise de flows incluindo estrutura,
    metricas e problemas potenciais.
    """

    def __init__(self, sf_client):
        """
        Inicializa o analisador

        Args:
            sf_client: SalesforceClient autenticado
        """
        self.sf = sf_client
        self._metadata = None

    @property
    def metadata(self):
        """Lazy loading do MetadataClient"""
        if self._metadata is None:
            from ..metadata_client import MetadataClient
            self._metadata = MetadataClient(self.sf)
        return self._metadata

    async def list_flows(
        self,
        active_only: bool = False,
        flow_type: Optional[FlowType] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista todos os Flows

        Args:
            active_only: Apenas flows ativos
            flow_type: Filtrar por tipo

        Returns:
            Lista de flows com informacoes basicas
        """
        # Query via Tooling API
        soql = """
            SELECT Id, DeveloperName, MasterLabel, ProcessType,
                   TriggerType, Status, ApiVersion, Description,
                   CreatedDate, LastModifiedDate
            FROM Flow
        """

        conditions = []
        if active_only:
            conditions.append("Status = 'Active'")
        if flow_type:
            conditions.append(f"ProcessType = '{flow_type.value}'")

        if conditions:
            soql += " WHERE " + " AND ".join(conditions)

        soql += " ORDER BY MasterLabel"

        from ..tooling_client import ToolingClient
        tooling = ToolingClient(self.sf)

        records = await tooling.query(soql)

        return [
            {
                "id": r.get("Id"),
                "api_name": r.get("DeveloperName"),
                "label": r.get("MasterLabel"),
                "process_type": r.get("ProcessType"),
                "trigger_type": r.get("TriggerType"),
                "status": r.get("Status"),
                "api_version": str(r.get("ApiVersion")),
                "description": r.get("Description"),
                "created_date": r.get("CreatedDate"),
                "last_modified_date": r.get("LastModifiedDate")
            }
            for r in records
        ]

    async def analyze(self, flow_name: str) -> FlowAnalysis:
        """
        Analisa um Flow

        Args:
            flow_name: API name do flow

        Returns:
            FlowAnalysis com detalhes
        """
        logger.info(f"Analisando flow: {flow_name}")

        # Obter metadata do flow
        flow_data = await self.metadata.read_metadata("Flow", [flow_name])

        if not flow_data:
            raise ValueError(f"Flow nao encontrado: {flow_name}")

        flow_meta = flow_data[0]

        # Criar analise
        analysis = FlowAnalysis(
            name=flow_name,
            api_name=flow_name,
            label=flow_meta.get("label", flow_name),
            flow_type=self._get_flow_type(flow_meta),
            status=flow_meta.get("status", "Draft"),
            process_type=flow_meta.get("processType", ""),
            api_version=str(flow_meta.get("apiVersion", "")),
            description=flow_meta.get("description"),
            analyzed_at=datetime.now()
        )

        # Trigger info
        start_elem = flow_meta.get("start", {})
        if isinstance(start_elem, dict):
            analysis.trigger_object = start_elem.get("object")
            analysis.trigger_type = start_elem.get("triggerType")
            analysis.record_trigger_type = start_elem.get("recordTriggerType")

        # Parsear elementos
        self._parse_elements(flow_meta, analysis)

        # Parsear variaveis
        self._parse_variables(flow_meta, analysis)

        # Calcular metricas
        self._calculate_metrics(analysis)

        # Identificar problemas
        self._identify_issues(analysis)

        # Identificar dependencias
        self._identify_dependencies(analysis)

        return analysis

    def _get_flow_type(self, flow_meta: Dict[str, Any]) -> str:
        """Determina o tipo do flow"""
        process_type = flow_meta.get("processType", "")
        trigger_type = flow_meta.get("start", {}).get("triggerType") if isinstance(flow_meta.get("start"), dict) else None

        if process_type == "Flow":
            if trigger_type:
                if trigger_type == "RecordAfterSave" or trigger_type == "RecordBeforeSave":
                    return FlowType.RECORD_TRIGGERED_FLOW.value
                elif trigger_type == "Scheduled":
                    return FlowType.SCHEDULED_FLOW.value
                elif trigger_type == "PlatformEvent":
                    return FlowType.PLATFORM_EVENT_FLOW.value
            return FlowType.AUTOLAUNCHED_FLOW.value
        elif process_type == "AutoLaunchedFlow":
            return FlowType.AUTOLAUNCHED_FLOW.value
        elif process_type == "Workflow":
            return FlowType.PROCESS_BUILDER.value

        return FlowType.SCREEN_FLOW.value

    def _parse_elements(self, flow_meta: Dict[str, Any], analysis: FlowAnalysis):
        """Parseia elementos do flow"""
        element_types = [
            ("screens", FlowElementType.SCREEN),
            ("decisions", FlowElementType.DECISION),
            ("assignments", FlowElementType.ASSIGNMENT),
            ("recordCreates", FlowElementType.RECORD_CREATE),
            ("recordUpdates", FlowElementType.RECORD_UPDATE),
            ("recordDeletes", FlowElementType.RECORD_DELETE),
            ("recordLookups", FlowElementType.RECORD_LOOKUP),
            ("loops", FlowElementType.LOOP),
            ("subflows", FlowElementType.SUBFLOW),
            ("actionCalls", FlowElementType.ACTION_CALL),
            ("apexPluginCalls", FlowElementType.APEX_CALL),
            ("waits", FlowElementType.WAIT)
        ]

        for key, elem_type in element_types:
            elements = flow_meta.get(key, [])
            if not isinstance(elements, list):
                elements = [elements] if elements else []

            for elem in elements:
                if not isinstance(elem, dict):
                    continue

                flow_elem = FlowElement(
                    name=elem.get("name", ""),
                    type=elem_type.value,
                    label=elem.get("label", elem.get("name", "")),
                    description=elem.get("description"),
                    object_type=elem.get("object")
                )

                # Connector
                connector = elem.get("connector", {})
                if isinstance(connector, dict):
                    flow_elem.connector = connector.get("targetReference")

                # Fault connector
                fault_connector = elem.get("faultConnector", {})
                if isinstance(fault_connector, dict):
                    flow_elem.fault_connector = fault_connector.get("targetReference")

                analysis.elements.append(flow_elem)

    def _parse_variables(self, flow_meta: Dict[str, Any], analysis: FlowAnalysis):
        """Parseia variaveis do flow"""
        variables = flow_meta.get("variables", [])
        if not isinstance(variables, list):
            variables = [variables] if variables else []

        for var in variables:
            if not isinstance(var, dict):
                continue

            flow_var = FlowVariable(
                name=var.get("name", ""),
                data_type=var.get("dataType", ""),
                is_input=var.get("isInput", False),
                is_output=var.get("isOutput", False),
                is_collection=var.get("isCollection", False),
                object_type=var.get("objectType"),
                default_value=str(var.get("value", "")) if var.get("value") else None
            )

            analysis.variables.append(flow_var)

        # Formulas
        formulas = flow_meta.get("formulas", [])
        if not isinstance(formulas, list):
            formulas = [formulas] if formulas else []

        for formula in formulas:
            if isinstance(formula, dict):
                analysis.formulas.append({
                    "name": formula.get("name", ""),
                    "dataType": formula.get("dataType", ""),
                    "expression": formula.get("expression", "")
                })

    def _calculate_metrics(self, analysis: FlowAnalysis):
        """Calcula metricas do flow"""
        metrics = FlowMetrics()

        metrics.element_count = len(analysis.elements)
        metrics.variable_count = len(analysis.variables)
        metrics.formula_count = len(analysis.formulas)

        for elem in analysis.elements:
            if elem.type == FlowElementType.SCREEN.value:
                metrics.screen_count += 1
            elif elem.type == FlowElementType.DECISION.value:
                metrics.decision_count += 1
            elif elem.type == FlowElementType.LOOP.value:
                metrics.loop_count += 1
            elif elem.type in (
                FlowElementType.RECORD_CREATE.value,
                FlowElementType.RECORD_UPDATE.value,
                FlowElementType.RECORD_DELETE.value
            ):
                metrics.dml_count += 1
            elif elem.type == FlowElementType.RECORD_LOOKUP.value:
                metrics.soql_count += 1
            elif elem.type == FlowElementType.APEX_CALL.value:
                metrics.apex_call_count += 1
            elif elem.type == FlowElementType.SUBFLOW.value:
                metrics.subflow_count += 1

        analysis.metrics = metrics

    def _identify_issues(self, analysis: FlowAnalysis):
        """Identifica problemas no flow"""
        # Muitos elementos
        if analysis.metrics.element_count > 50:
            analysis.issues.append(FlowIssue(
                severity="warning",
                message=f"Flow com muitos elementos ({analysis.metrics.element_count})",
                rule="TOO_MANY_ELEMENTS",
                suggestion="Considere dividir em subflows"
            ))

        # DML em loop
        loops = [e for e in analysis.elements if e.type == FlowElementType.LOOP.value]
        dml_ops = [e for e in analysis.elements if e.type in (
            FlowElementType.RECORD_CREATE.value,
            FlowElementType.RECORD_UPDATE.value,
            FlowElementType.RECORD_DELETE.value
        )]

        # Simplificacao: se tem loop e DML, alertar
        if loops and dml_ops:
            analysis.issues.append(FlowIssue(
                severity="error",
                message="Flow pode ter DML dentro de loop",
                rule="DML_IN_LOOP",
                suggestion="Verifique se operacoes DML estao fora dos loops"
            ))

        # Query em loop
        queries = [e for e in analysis.elements if e.type == FlowElementType.RECORD_LOOKUP.value]
        if loops and queries:
            analysis.issues.append(FlowIssue(
                severity="warning",
                message="Flow pode ter queries dentro de loop",
                rule="QUERY_IN_LOOP",
                suggestion="Considere buscar todos os registros antes do loop"
            ))

        # Flow sem tratamento de erro
        has_fault_handler = any(e.fault_connector for e in analysis.elements)
        if not has_fault_handler and analysis.metrics.dml_count > 0:
            analysis.issues.append(FlowIssue(
                severity="info",
                message="Flow com DML sem tratamento de erro",
                rule="NO_ERROR_HANDLING",
                suggestion="Adicione fault paths para tratar erros"
            ))

        # Muitas decisoes (complexidade)
        if analysis.metrics.decision_count > 10:
            analysis.issues.append(FlowIssue(
                severity="info",
                message=f"Flow com muitas decisoes ({analysis.metrics.decision_count})",
                rule="COMPLEX_LOGIC",
                suggestion="Considere simplificar a logica ou usar formulas"
            ))

    def _identify_dependencies(self, analysis: FlowAnalysis):
        """Identifica dependencias do flow"""
        dependencies = set()

        # Objeto de trigger
        if analysis.trigger_object:
            dependencies.add(analysis.trigger_object)

        # Objetos usados em elementos
        for elem in analysis.elements:
            if elem.object_type:
                dependencies.add(elem.object_type)

        # Variaveis de objetos
        for var in analysis.variables:
            if var.object_type:
                dependencies.add(var.object_type)

        analysis.dependencies = sorted(list(dependencies))

    async def generate_documentation(
        self,
        flow_name: str,
        format: str = "markdown"
    ) -> str:
        """
        Gera documentacao do flow

        Args:
            flow_name: API name do flow
            format: Formato da documentacao

        Returns:
            Documentacao formatada
        """
        analysis = await self.analyze(flow_name)

        if format == "markdown":
            return self._generate_markdown_doc(analysis)
        else:
            raise ValueError(f"Formato nao suportado: {format}")

    def _generate_markdown_doc(self, analysis: FlowAnalysis) -> str:
        """Gera documentacao em Markdown"""
        lines = []

        # Cabecalho
        lines.append(f"# {analysis.label}")
        lines.append("")
        lines.append(f"**API Name:** `{analysis.api_name}`")
        lines.append(f"**Tipo:** {analysis.flow_type}")
        lines.append(f"**Status:** {analysis.status}")
        lines.append(f"**API Version:** {analysis.api_version}")
        lines.append("")

        if analysis.description:
            lines.append("## Descricao")
            lines.append(analysis.description)
            lines.append("")

        # Trigger
        if analysis.trigger_object:
            lines.append("## Trigger")
            lines.append(f"- **Objeto:** {analysis.trigger_object}")
            lines.append(f"- **Tipo:** {analysis.trigger_type or 'N/A'}")
            if analysis.record_trigger_type:
                lines.append(f"- **Evento:** {analysis.record_trigger_type}")
            lines.append("")

        # Metricas
        lines.append("## Metricas")
        lines.append(f"- Total de elementos: {analysis.metrics.element_count}")
        lines.append(f"- Telas: {analysis.metrics.screen_count}")
        lines.append(f"- Decisoes: {analysis.metrics.decision_count}")
        lines.append(f"- Loops: {analysis.metrics.loop_count}")
        lines.append(f"- Operacoes DML: {analysis.metrics.dml_count}")
        lines.append(f"- Queries: {analysis.metrics.soql_count}")
        lines.append(f"- Chamadas Apex: {analysis.metrics.apex_call_count}")
        lines.append(f"- Subflows: {analysis.metrics.subflow_count}")
        lines.append(f"- Variaveis: {analysis.metrics.variable_count}")
        lines.append("")

        # Elementos
        if analysis.elements:
            lines.append("## Elementos")
            lines.append("")
            lines.append("| Nome | Tipo | Label |")
            lines.append("|------|------|-------|")
            for elem in analysis.elements:
                lines.append(f"| {elem.name} | {elem.type} | {elem.label} |")
            lines.append("")

        # Variaveis
        if analysis.variables:
            lines.append("## Variaveis")
            lines.append("")
            lines.append("| Nome | Tipo | Input | Output |")
            lines.append("|------|------|-------|--------|")
            for var in analysis.variables:
                input_str = "Sim" if var.is_input else "Nao"
                output_str = "Sim" if var.is_output else "Nao"
                lines.append(f"| {var.name} | {var.data_type} | {input_str} | {output_str} |")
            lines.append("")

        # Problemas
        if analysis.issues:
            lines.append("## Problemas Identificados")
            lines.append("")
            for issue in analysis.issues:
                icon = "**ERRO**" if issue.severity == "error" else "**AVISO**" if issue.severity == "warning" else "INFO"
                lines.append(f"- {icon}: {issue.message}")
                if issue.suggestion:
                    lines.append(f"  - *Sugestao: {issue.suggestion}*")
            lines.append("")

        # Dependencias
        if analysis.dependencies:
            lines.append("## Dependencias")
            lines.append("")
            for dep in analysis.dependencies:
                lines.append(f"- {dep}")
            lines.append("")

        lines.append("---")
        lines.append(f"*Analise gerada em: {analysis.analyzed_at.strftime('%Y-%m-%d %H:%M:%S')}*")

        return "\n".join(lines)

    async def get_flow_statistics(self) -> Dict[str, Any]:
        """
        Obtem estatisticas gerais dos flows

        Returns:
            Dict com estatisticas
        """
        flows = await self.list_flows()

        stats = {
            "total_flows": len(flows),
            "by_status": {},
            "by_type": {},
            "active_count": 0,
            "draft_count": 0
        }

        for flow in flows:
            # Por status
            status = flow.get("status", "Unknown")
            stats["by_status"][status] = stats["by_status"].get(status, 0) + 1

            if status == "Active":
                stats["active_count"] += 1
            elif status == "Draft":
                stats["draft_count"] += 1

            # Por tipo
            process_type = flow.get("process_type", "Unknown")
            stats["by_type"][process_type] = stats["by_type"].get(process_type, 0) + 1

        return stats

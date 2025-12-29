# -*- coding: utf-8 -*-
"""
Salesforce Lightning Web Components Analyzer
============================================
Analisador de Lightning Web Components (LWC).

Funcionalidades:
- Analise de estrutura de componentes
- Deteccao de dependencias
- Analise de performance
- Identificacao de problemas
- Metricas de codigo

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient
    from factory.integrations.salesforce.analyzers import LWCAnalyzer

    sf = SalesforceClient(config)
    await sf.connect()

    analyzer = LWCAnalyzer(sf)

    # Listar componentes
    components = await analyzer.list_components()

    # Analisar componente
    analysis = await analyzer.analyze("myComponent")
"""

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set

logger = logging.getLogger(__name__)


class LWCExposure(str, Enum):
    """Exposicao do componente"""
    GLOBAL = "global"
    NAMESPACEACCESS = "namespaceAccess"


class LWCTarget(str, Enum):
    """Alvos onde o componente pode ser usado"""
    APP_PAGE = "lightning__AppPage"
    RECORD_PAGE = "lightning__RecordPage"
    HOME_PAGE = "lightning__HomePage"
    FLOW_SCREEN = "lightning__FlowScreen"
    TAB = "lightning__Tab"
    UTILITY_BAR = "lightning__UtilityBar"
    INBOX_EMAIL = "lightning__Inbox"
    COMMUNITIES = "lightningCommunity__Page"
    SNAPIN = "lightningSnapin__ChatMessage"


@dataclass
class LWCProperty:
    """Propriedade de um componente LWC"""
    name: str
    type: str
    is_public: bool = False
    is_reactive: bool = False
    is_api: bool = False
    default_value: Optional[str] = None
    description: Optional[str] = None


@dataclass
class LWCMethod:
    """Metodo de um componente LWC"""
    name: str
    is_public: bool = False
    is_api: bool = False
    is_wire: bool = False
    wire_adapter: Optional[str] = None
    parameters: List[str] = field(default_factory=list)
    is_async: bool = False


@dataclass
class LWCImport:
    """Importacao em um componente LWC"""
    module: str
    imports: List[str]
    is_wire: bool = False
    is_apex: bool = False
    is_schema: bool = False


@dataclass
class LWCMetrics:
    """Metricas do componente"""
    js_lines: int = 0
    html_lines: int = 0
    css_lines: int = 0
    property_count: int = 0
    method_count: int = 0
    wire_count: int = 0
    apex_call_count: int = 0
    import_count: int = 0
    event_count: int = 0


@dataclass
class LWCIssue:
    """Problema identificado no componente"""
    severity: str
    message: str
    file: str
    line: Optional[int] = None
    rule: str = ""
    suggestion: Optional[str] = None


@dataclass
class LWCAnalysis:
    """Resultado da analise de um componente LWC"""
    name: str
    api_name: str
    namespace: Optional[str] = None
    api_version: str = ""
    description: Optional[str] = None

    # Configuracao
    is_exposed: bool = False
    targets: List[str] = field(default_factory=list)
    master_label: Optional[str] = None

    # Arquivos
    has_js: bool = True
    has_html: bool = True
    has_css: bool = False
    has_svg: bool = False

    # Estrutura
    properties: List[LWCProperty] = field(default_factory=list)
    methods: List[LWCMethod] = field(default_factory=list)
    imports: List[LWCImport] = field(default_factory=list)
    events_dispatched: List[str] = field(default_factory=list)
    events_handled: List[str] = field(default_factory=list)

    # Dependencias
    child_components: List[str] = field(default_factory=list)
    apex_methods: List[str] = field(default_factory=list)
    wire_adapters: List[str] = field(default_factory=list)

    # Analise
    metrics: LWCMetrics = field(default_factory=LWCMetrics)
    issues: List[LWCIssue] = field(default_factory=list)

    # Conteudo
    js_content: Optional[str] = None
    html_content: Optional[str] = None
    css_content: Optional[str] = None
    meta_content: Optional[str] = None

    # Meta
    analyzed_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        """Converte analise para dicionario"""
        return {
            "name": self.name,
            "api_name": self.api_name,
            "is_exposed": self.is_exposed,
            "targets": self.targets,
            "metrics": {
                "js_lines": self.metrics.js_lines,
                "html_lines": self.metrics.html_lines,
                "css_lines": self.metrics.css_lines,
                "properties": self.metrics.property_count,
                "methods": self.metrics.method_count,
                "wire_calls": self.metrics.wire_count,
                "apex_calls": self.metrics.apex_call_count
            },
            "dependencies": {
                "child_components": self.child_components,
                "apex_methods": self.apex_methods,
                "wire_adapters": self.wire_adapters
            },
            "issues_count": len(self.issues),
            "analyzed_at": self.analyzed_at.isoformat() if self.analyzed_at else None
        }


class LWCAnalyzer:
    """
    Analisador de Lightning Web Components

    Fornece analise de componentes LWC incluindo estrutura,
    metricas e problemas potenciais.
    """

    # Regex patterns
    PROPERTY_PATTERN = re.compile(
        r'@(api|track|wire)\s*(?:\([^)]*\))?\s*(\w+)\s*(?:=\s*([^;]+))?;',
        re.MULTILINE
    )

    METHOD_PATTERN = re.compile(
        r'(?:@(api|wire)(?:\([^)]*\))?\s*)?(async\s+)?(\w+)\s*\(([^)]*)\)\s*\{',
        re.MULTILINE
    )

    IMPORT_PATTERN = re.compile(
        r'import\s+(?:\{([^}]+)\}|(\w+))\s+from\s+[\'"]([^\'"]+)[\'"]',
        re.MULTILINE
    )

    EVENT_DISPATCH_PATTERN = re.compile(
        r'this\.dispatchEvent\s*\(\s*new\s+CustomEvent\s*\(\s*[\'"](\w+)[\'"]',
        re.MULTILINE
    )

    COMPONENT_TAG_PATTERN = re.compile(
        r'<(c-[\w-]+|lightning-[\w-]+)',
        re.MULTILINE
    )

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

    async def list_components(
        self,
        include_managed: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Lista todos os componentes LWC

        Args:
            include_managed: Incluir componentes de pacotes gerenciados

        Returns:
            Lista de componentes
        """
        # Listar via Metadata API
        components = await self.metadata.list_metadata("LightningComponentBundle")

        result = []
        for comp in components:
            namespace = comp.get("namespacePrefix")

            # Filtrar managed
            if namespace and not include_managed:
                continue

            result.append({
                "id": comp.get("id"),
                "api_name": comp.get("fullName"),
                "namespace": namespace,
                "created_date": comp.get("createdDate"),
                "last_modified_date": comp.get("lastModifiedDate"),
                "type": comp.get("type")
            })

        return result

    async def analyze(self, component_name: str) -> LWCAnalysis:
        """
        Analisa um componente LWC

        Args:
            component_name: Nome do componente

        Returns:
            LWCAnalysis com detalhes
        """
        logger.info(f"Analisando componente: {component_name}")

        # Obter metadata via retrieve
        result = await self.metadata.retrieve_and_wait({
            "LightningComponentBundle": [component_name]
        })

        if not result.success or not result.zip_file:
            raise ValueError(f"Componente nao encontrado: {component_name}")

        # Extrair arquivos do ZIP
        import zipfile
        import io

        files = {}
        with zipfile.ZipFile(io.BytesIO(result.zip_file), 'r') as zf:
            for file_info in zf.filelist:
                if component_name in file_info.filename:
                    content = zf.read(file_info.filename).decode('utf-8', errors='ignore')
                    ext = file_info.filename.split('.')[-1].lower()
                    files[ext] = content

        # Criar analise
        analysis = LWCAnalysis(
            name=component_name,
            api_name=component_name,
            analyzed_at=datetime.now()
        )

        # Parsear arquivos
        if 'js' in files:
            analysis.has_js = True
            analysis.js_content = files['js']
            self._parse_javascript(analysis)

        if 'html' in files:
            analysis.has_html = True
            analysis.html_content = files['html']
            self._parse_html(analysis)

        if 'css' in files:
            analysis.has_css = True
            analysis.css_content = files['css']

        if 'xml' in files:
            analysis.meta_content = files['xml']
            self._parse_meta(analysis)

        if 'svg' in files:
            analysis.has_svg = True

        # Calcular metricas
        self._calculate_metrics(analysis)

        # Identificar problemas
        self._identify_issues(analysis)

        return analysis

    def _parse_javascript(self, analysis: LWCAnalysis):
        """Parseia arquivo JavaScript"""
        if not analysis.js_content:
            return

        js = analysis.js_content

        # Parsear imports
        for match in self.IMPORT_PATTERN.finditer(js):
            named_imports = match.group(1)
            default_import = match.group(2)
            module = match.group(3)

            imports = []
            if named_imports:
                imports = [i.strip() for i in named_imports.split(',')]
            elif default_import:
                imports = [default_import]

            lwc_import = LWCImport(
                module=module,
                imports=imports,
                is_wire='@salesforce/apex' in module,
                is_apex='@salesforce/apex' in module,
                is_schema='@salesforce/schema' in module
            )

            analysis.imports.append(lwc_import)

            # Identificar Apex methods
            if lwc_import.is_apex:
                for imp in imports:
                    analysis.apex_methods.append(imp)

        # Parsear propriedades
        for match in self.PROPERTY_PATTERN.finditer(js):
            decorator = match.group(1)
            name = match.group(2)
            default = match.group(3)

            prop = LWCProperty(
                name=name,
                type="any",
                is_api=decorator == "api",
                is_reactive=decorator == "track",
                default_value=default.strip() if default else None
            )

            analysis.properties.append(prop)

        # Parsear metodos
        for match in self.METHOD_PATTERN.finditer(js):
            decorator = match.group(1)
            is_async = bool(match.group(2))
            name = match.group(3)
            params = match.group(4)

            # Ignorar constructor e lifecycle hooks
            if name in ('constructor', 'connectedCallback', 'disconnectedCallback',
                       'renderedCallback', 'errorCallback'):
                continue

            method = LWCMethod(
                name=name,
                is_api=decorator == "api",
                is_wire=decorator == "wire",
                is_async=is_async,
                parameters=[p.strip() for p in params.split(',') if p.strip()]
            )

            if method.is_wire:
                analysis.wire_adapters.append(name)

            analysis.methods.append(method)

        # Eventos disparados
        for match in self.EVENT_DISPATCH_PATTERN.finditer(js):
            event_name = match.group(1)
            analysis.events_dispatched.append(event_name)

    def _parse_html(self, analysis: LWCAnalysis):
        """Parseia arquivo HTML"""
        if not analysis.html_content:
            return

        html = analysis.html_content

        # Componentes filhos
        for match in self.COMPONENT_TAG_PATTERN.finditer(html):
            component_tag = match.group(1)
            if component_tag not in analysis.child_components:
                analysis.child_components.append(component_tag)

        # Eventos handler (on*)
        event_handler_pattern = r'on(\w+)='
        for match in re.finditer(event_handler_pattern, html):
            event_name = match.group(1)
            if event_name not in analysis.events_handled:
                analysis.events_handled.append(event_name)

    def _parse_meta(self, analysis: LWCAnalysis):
        """Parseia arquivo meta XML"""
        if not analysis.meta_content:
            return

        import xml.etree.ElementTree as ET

        try:
            root = ET.fromstring(analysis.meta_content)

            for elem in root.iter():
                tag = elem.tag.split('}')[-1].lower()

                if tag == 'apiversion':
                    analysis.api_version = elem.text or ""
                elif tag == 'isexposed':
                    analysis.is_exposed = elem.text.lower() == 'true' if elem.text else False
                elif tag == 'masterlabel':
                    analysis.master_label = elem.text
                elif tag == 'description':
                    analysis.description = elem.text
                elif tag == 'target':
                    if elem.text:
                        analysis.targets.append(elem.text)

        except ET.ParseError as e:
            logger.warning(f"Erro ao parsear meta XML: {e}")

    def _calculate_metrics(self, analysis: LWCAnalysis):
        """Calcula metricas do componente"""
        metrics = LWCMetrics()

        if analysis.js_content:
            metrics.js_lines = len([l for l in analysis.js_content.split('\n') if l.strip()])

        if analysis.html_content:
            metrics.html_lines = len([l for l in analysis.html_content.split('\n') if l.strip()])

        if analysis.css_content:
            metrics.css_lines = len([l for l in analysis.css_content.split('\n') if l.strip()])

        metrics.property_count = len(analysis.properties)
        metrics.method_count = len(analysis.methods)
        metrics.wire_count = len([m for m in analysis.methods if m.is_wire])
        metrics.apex_call_count = len(analysis.apex_methods)
        metrics.import_count = len(analysis.imports)
        metrics.event_count = len(analysis.events_dispatched)

        analysis.metrics = metrics

    def _identify_issues(self, analysis: LWCAnalysis):
        """Identifica problemas no componente"""
        # Componente nao exposto
        if not analysis.is_exposed and analysis.targets:
            analysis.issues.append(LWCIssue(
                severity="warning",
                message="Componente tem targets mas nao esta exposto",
                file="meta.xml",
                rule="NOT_EXPOSED",
                suggestion="Adicione <isExposed>true</isExposed> ao meta XML"
            ))

        # Muitas propriedades @api
        api_props = [p for p in analysis.properties if p.is_api]
        if len(api_props) > 10:
            analysis.issues.append(LWCIssue(
                severity="info",
                message=f"Componente com muitas propriedades @api ({len(api_props)})",
                file=f"{analysis.name}.js",
                rule="TOO_MANY_API_PROPS",
                suggestion="Considere agrupar propriedades relacionadas em um objeto"
            ))

        # Arquivo JS muito grande
        if analysis.metrics.js_lines > 500:
            analysis.issues.append(LWCIssue(
                severity="warning",
                message=f"Arquivo JS muito grande ({analysis.metrics.js_lines} linhas)",
                file=f"{analysis.name}.js",
                rule="LARGE_JS_FILE",
                suggestion="Considere dividir a logica em modulos ou componentes menores"
            ))

        # Muitas chamadas Apex
        if analysis.metrics.apex_call_count > 5:
            analysis.issues.append(LWCIssue(
                severity="info",
                message=f"Componente com muitas chamadas Apex ({analysis.metrics.apex_call_count})",
                file=f"{analysis.name}.js",
                rule="MANY_APEX_CALLS",
                suggestion="Considere consolidar chamadas ou usar @wire quando possivel"
            ))

        # Eventos sem namespace
        for event in analysis.events_dispatched:
            if '_' not in event and not event.startswith('c'):
                analysis.issues.append(LWCIssue(
                    severity="info",
                    message=f"Evento '{event}' sem namespace pode conflitar",
                    file=f"{analysis.name}.js",
                    rule="EVENT_NO_NAMESPACE",
                    suggestion="Use prefixo para evitar conflitos (ex: mycompany_eventname)"
                ))

    async def generate_documentation(
        self,
        component_name: str,
        format: str = "markdown"
    ) -> str:
        """
        Gera documentacao do componente

        Args:
            component_name: Nome do componente
            format: Formato da documentacao

        Returns:
            Documentacao formatada
        """
        analysis = await self.analyze(component_name)

        if format == "markdown":
            return self._generate_markdown_doc(analysis)
        else:
            raise ValueError(f"Formato nao suportado: {format}")

    def _generate_markdown_doc(self, analysis: LWCAnalysis) -> str:
        """Gera documentacao em Markdown"""
        lines = []

        # Cabecalho
        lines.append(f"# {analysis.master_label or analysis.name}")
        lines.append("")
        lines.append(f"**API Name:** `{analysis.api_name}`")
        lines.append(f"**API Version:** {analysis.api_version}")
        lines.append(f"**Exposto:** {'Sim' if analysis.is_exposed else 'Nao'}")
        lines.append("")

        if analysis.description:
            lines.append("## Descricao")
            lines.append(analysis.description)
            lines.append("")

        # Targets
        if analysis.targets:
            lines.append("## Onde usar")
            for target in analysis.targets:
                lines.append(f"- {target}")
            lines.append("")

        # Arquivos
        lines.append("## Arquivos")
        lines.append(f"- JavaScript: {'Sim' if analysis.has_js else 'Nao'}")
        lines.append(f"- HTML: {'Sim' if analysis.has_html else 'Nao'}")
        lines.append(f"- CSS: {'Sim' if analysis.has_css else 'Nao'}")
        lines.append(f"- SVG: {'Sim' if analysis.has_svg else 'Nao'}")
        lines.append("")

        # Propriedades @api
        api_props = [p for p in analysis.properties if p.is_api]
        if api_props:
            lines.append("## Propriedades Publicas (@api)")
            lines.append("")
            lines.append("| Nome | Valor Padrao |")
            lines.append("|------|--------------|")
            for prop in api_props:
                default = prop.default_value or "-"
                lines.append(f"| {prop.name} | {default} |")
            lines.append("")

        # Metodos @api
        api_methods = [m for m in analysis.methods if m.is_api]
        if api_methods:
            lines.append("## Metodos Publicos (@api)")
            lines.append("")
            for method in api_methods:
                params = ", ".join(method.parameters) if method.parameters else ""
                lines.append(f"- `{method.name}({params})`")
            lines.append("")

        # Eventos
        if analysis.events_dispatched:
            lines.append("## Eventos Disparados")
            lines.append("")
            for event in analysis.events_dispatched:
                lines.append(f"- `{event}`")
            lines.append("")

        # Dependencias
        if analysis.child_components or analysis.apex_methods:
            lines.append("## Dependencias")
            lines.append("")

            if analysis.child_components:
                lines.append("### Componentes")
                for comp in analysis.child_components:
                    lines.append(f"- `{comp}`")
                lines.append("")

            if analysis.apex_methods:
                lines.append("### Metodos Apex")
                for apex in analysis.apex_methods:
                    lines.append(f"- `{apex}`")
                lines.append("")

        # Metricas
        lines.append("## Metricas")
        lines.append(f"- Linhas JS: {analysis.metrics.js_lines}")
        lines.append(f"- Linhas HTML: {analysis.metrics.html_lines}")
        lines.append(f"- Linhas CSS: {analysis.metrics.css_lines}")
        lines.append(f"- Propriedades: {analysis.metrics.property_count}")
        lines.append(f"- Metodos: {analysis.metrics.method_count}")
        lines.append(f"- Chamadas @wire: {analysis.metrics.wire_count}")
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

        lines.append("---")
        lines.append(f"*Analise gerada em: {analysis.analyzed_at.strftime('%Y-%m-%d %H:%M:%S')}*")

        return "\n".join(lines)

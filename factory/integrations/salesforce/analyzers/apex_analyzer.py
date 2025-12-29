# -*- coding: utf-8 -*-
"""
Salesforce Apex Analyzer
========================
Analisador de codigo Apex (classes e triggers).

Funcionalidades:
- Analise de estrutura de classes
- Deteccao de padroes (Singleton, Factory, etc.)
- Analise de dependencias
- Identificacao de problemas potenciais
- Metricas de codigo
- Analise de test coverage

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient
    from factory.integrations.salesforce.analyzers import ApexAnalyzer

    sf = SalesforceClient(config)
    await sf.connect()

    analyzer = ApexAnalyzer(sf)

    # Analisar classe
    analysis = await analyzer.analyze_class("AccountService")

    # Obter metricas
    print(f"Linhas: {analysis.metrics.lines_of_code}")
    print(f"Metodos: {analysis.metrics.method_count}")
"""

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set, Tuple

logger = logging.getLogger(__name__)


class ApexModifier(str, Enum):
    """Modificadores de acesso Apex"""
    PUBLIC = "public"
    PRIVATE = "private"
    PROTECTED = "protected"
    GLOBAL = "global"
    STATIC = "static"
    VIRTUAL = "virtual"
    ABSTRACT = "abstract"
    OVERRIDE = "override"
    TESTMETHOD = "testMethod"
    WEBSERVICE = "webService"


class ApexPattern(str, Enum):
    """Padroes de design identificaveis"""
    SINGLETON = "Singleton"
    FACTORY = "Factory"
    SERVICE = "Service"
    SELECTOR = "Selector"
    DOMAIN = "Domain"
    TRIGGER_HANDLER = "TriggerHandler"
    TEST_CLASS = "TestClass"
    BATCH = "Batch"
    SCHEDULED = "Scheduled"
    QUEUEABLE = "Queueable"
    REST_RESOURCE = "RestResource"
    CONTROLLER = "Controller"


@dataclass
class MethodInfo:
    """Informacoes de um metodo Apex"""
    name: str
    return_type: str
    parameters: List[Tuple[str, str]]  # (tipo, nome)
    modifiers: List[str]
    line_start: int
    line_end: int
    is_test: bool = False
    is_constructor: bool = False
    is_getter: bool = False
    is_setter: bool = False
    annotations: List[str] = field(default_factory=list)
    complexity: int = 1  # Complexidade ciclomatica basica


@dataclass
class PropertyInfo:
    """Informacoes de uma propriedade Apex"""
    name: str
    type: str
    modifiers: List[str]
    has_getter: bool = True
    has_setter: bool = True
    line_number: int = 0


@dataclass
class VariableInfo:
    """Informacoes de uma variavel"""
    name: str
    type: str
    modifiers: List[str]
    is_static: bool = False
    is_final: bool = False
    line_number: int = 0


@dataclass
class CodeMetrics:
    """Metricas de codigo"""
    lines_of_code: int = 0
    lines_of_comments: int = 0
    blank_lines: int = 0
    method_count: int = 0
    property_count: int = 0
    variable_count: int = 0
    inner_class_count: int = 0
    avg_method_length: float = 0.0
    max_method_length: int = 0
    cyclomatic_complexity: int = 1
    test_methods_count: int = 0


@dataclass
class CodeIssue:
    """Problema identificado no codigo"""
    severity: str  # error, warning, info
    message: str
    line: int
    rule: str
    suggestion: Optional[str] = None


@dataclass
class ApexClassAnalysis:
    """Resultado da analise de uma classe Apex"""
    name: str
    id: str
    api_version: str
    is_valid: bool
    status: str

    # Estrutura
    modifiers: List[str]
    extends: Optional[str] = None
    implements: List[str] = field(default_factory=list)
    annotations: List[str] = field(default_factory=list)

    # Membros
    methods: List[MethodInfo] = field(default_factory=list)
    properties: List[PropertyInfo] = field(default_factory=list)
    variables: List[VariableInfo] = field(default_factory=list)
    inner_classes: List[str] = field(default_factory=list)

    # Analise
    patterns: List[ApexPattern] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)
    issues: List[CodeIssue] = field(default_factory=list)
    metrics: CodeMetrics = field(default_factory=CodeMetrics)

    # Coverage
    coverage_percentage: float = 0.0
    lines_covered: int = 0
    lines_uncovered: int = 0

    # Meta
    body: Optional[str] = None
    analyzed_at: Optional[datetime] = None


@dataclass
class ApexTriggerAnalysis:
    """Resultado da analise de um trigger"""
    name: str
    id: str
    sobject: str
    api_version: str
    is_valid: bool
    status: str

    # Eventos
    events: List[str] = field(default_factory=list)

    # Analise
    calls_handler: bool = False
    handler_class: Optional[str] = None
    has_logic: bool = False
    issues: List[CodeIssue] = field(default_factory=list)
    metrics: CodeMetrics = field(default_factory=CodeMetrics)

    # Meta
    body: Optional[str] = None
    analyzed_at: Optional[datetime] = None


class ApexAnalyzer:
    """
    Analisador de codigo Apex

    Fornece analise estatica de classes e triggers Apex,
    incluindo metricas, padroes e problemas potenciais.
    """

    # Regex para parsing de Apex
    CLASS_PATTERN = re.compile(
        r'(?P<modifiers>(?:public|private|global|abstract|virtual|with\s+sharing|without\s+sharing|inherited\s+sharing)\s+)*'
        r'class\s+(?P<name>\w+)'
        r'(?:\s+extends\s+(?P<extends>\w+))?'
        r'(?:\s+implements\s+(?P<implements>[\w,\s]+))?',
        re.IGNORECASE
    )

    METHOD_PATTERN = re.compile(
        r'(?P<annotations>(?:@\w+(?:\([^)]*\))?\s+)*)'
        r'(?P<modifiers>(?:public|private|protected|global|static|virtual|abstract|override|testMethod|webService)\s+)*'
        r'(?P<return>\w+(?:<[\w,\s]+>)?)\s+'
        r'(?P<name>\w+)\s*\((?P<params>[^)]*)\)',
        re.IGNORECASE
    )

    PROPERTY_PATTERN = re.compile(
        r'(?P<modifiers>(?:public|private|protected|global|static|transient)\s+)*'
        r'(?P<type>\w+(?:<[\w,\s]+>)?)\s+'
        r'(?P<name>\w+)\s*\{',
        re.IGNORECASE
    )

    VARIABLE_PATTERN = re.compile(
        r'(?P<modifiers>(?:public|private|protected|static|final|transient)\s+)*'
        r'(?P<type>\w+(?:<[\w,\s]+>)?)\s+'
        r'(?P<name>\w+)\s*(?:=|;)',
        re.IGNORECASE
    )

    def __init__(self, sf_client):
        """
        Inicializa o analisador

        Args:
            sf_client: SalesforceClient autenticado
        """
        self.sf = sf_client
        self._tooling = None

    @property
    def tooling(self):
        """Lazy loading do ToolingClient"""
        if self._tooling is None:
            from ..tooling_client import ToolingClient
            self._tooling = ToolingClient(self.sf)
        return self._tooling

    async def analyze_class(
        self,
        name: str,
        include_coverage: bool = True
    ) -> ApexClassAnalysis:
        """
        Analisa uma classe Apex

        Args:
            name: Nome da classe
            include_coverage: Incluir analise de coverage

        Returns:
            ApexClassAnalysis com detalhes
        """
        logger.info(f"Analisando classe: {name}")

        # Obter classe
        apex_class = await self.tooling.get_apex_class_by_name(name)

        if not apex_class:
            raise ValueError(f"Classe nao encontrada: {name}")

        # Criar analise
        analysis = ApexClassAnalysis(
            name=apex_class.name,
            id=apex_class.id,
            api_version=apex_class.api_version,
            is_valid=apex_class.is_valid,
            status=apex_class.status,
            body=apex_class.body,
            analyzed_at=datetime.now()
        )

        # Parsear codigo
        self._parse_class_structure(analysis)

        # Calcular metricas
        self._calculate_metrics(analysis)

        # Identificar padroes
        self._identify_patterns(analysis)

        # Identificar dependencias
        self._identify_dependencies(analysis)

        # Identificar problemas
        self._identify_issues(analysis)

        # Coverage
        if include_coverage:
            await self._get_coverage(analysis)

        return analysis

    async def analyze_trigger(self, name: str) -> ApexTriggerAnalysis:
        """
        Analisa um trigger Apex

        Args:
            name: Nome do trigger

        Returns:
            ApexTriggerAnalysis com detalhes
        """
        logger.info(f"Analisando trigger: {name}")

        # Obter trigger
        trigger = await self.tooling.get_apex_trigger_by_name(name)

        if not trigger:
            raise ValueError(f"Trigger nao encontrado: {name}")

        # Criar analise
        analysis = ApexTriggerAnalysis(
            name=trigger.name,
            id=trigger.id,
            sobject=trigger.table_enum_or_id,
            api_version=trigger.api_version,
            is_valid=trigger.is_valid,
            status=trigger.status,
            body=trigger.body,
            analyzed_at=datetime.now()
        )

        # Identificar eventos
        analysis.events = [
            event for event, is_used in [
                ("before insert", trigger.usage_before_insert),
                ("after insert", trigger.usage_after_insert),
                ("before update", trigger.usage_before_update),
                ("after update", trigger.usage_after_update),
                ("before delete", trigger.usage_before_delete),
                ("after delete", trigger.usage_after_delete),
                ("after undelete", trigger.usage_after_undelete)
            ] if is_used
        ]

        # Analisar estrutura
        self._analyze_trigger_structure(analysis)

        # Calcular metricas
        if analysis.body:
            lines = analysis.body.split('\n')
            analysis.metrics.lines_of_code = len([l for l in lines if l.strip()])
            analysis.metrics.lines_of_comments = len([l for l in lines if l.strip().startswith('//')])

        return analysis

    def _parse_class_structure(self, analysis: ApexClassAnalysis):
        """Parseia estrutura da classe"""
        if not analysis.body:
            return

        body = analysis.body

        # Parsear declaracao da classe
        class_match = self.CLASS_PATTERN.search(body)
        if class_match:
            if class_match.group("modifiers"):
                analysis.modifiers = class_match.group("modifiers").split()
            if class_match.group("extends"):
                analysis.extends = class_match.group("extends")
            if class_match.group("implements"):
                analysis.implements = [
                    i.strip() for i in class_match.group("implements").split(",")
                ]

        # Extrair annotations
        annotation_pattern = r'@(\w+)(?:\([^)]*\))?'
        annotations = re.findall(annotation_pattern, body[:500])  # Apenas no inicio
        analysis.annotations = annotations

        # Parsear metodos
        for match in self.METHOD_PATTERN.finditer(body):
            method_name = match.group("name")
            return_type = match.group("return")
            modifiers = match.group("modifiers").split() if match.group("modifiers") else []

            # Parsear parametros
            params_str = match.group("params")
            parameters = []
            if params_str.strip():
                for param in params_str.split(","):
                    parts = param.strip().split()
                    if len(parts) >= 2:
                        parameters.append((parts[0], parts[-1]))

            # Extrair annotations do metodo
            method_annotations = re.findall(annotation_pattern, match.group("annotations") or "")

            # Identificar tipo de metodo
            is_test = "testMethod" in modifiers or "Test" in method_annotations
            is_constructor = method_name == analysis.name

            method = MethodInfo(
                name=method_name,
                return_type=return_type,
                parameters=parameters,
                modifiers=modifiers,
                line_start=body[:match.start()].count('\n') + 1,
                line_end=0,  # Dificil determinar sem parser completo
                is_test=is_test,
                is_constructor=is_constructor,
                annotations=method_annotations
            )

            analysis.methods.append(method)

        # Parsear propriedades (simplificado)
        for match in self.PROPERTY_PATTERN.finditer(body):
            prop = PropertyInfo(
                name=match.group("name"),
                type=match.group("type"),
                modifiers=match.group("modifiers").split() if match.group("modifiers") else [],
                line_number=body[:match.start()].count('\n') + 1
            )
            analysis.properties.append(prop)

    def _calculate_metrics(self, analysis: ApexClassAnalysis):
        """Calcula metricas de codigo"""
        if not analysis.body:
            return

        lines = analysis.body.split('\n')

        analysis.metrics.lines_of_code = len([l for l in lines if l.strip() and not l.strip().startswith('//')])
        analysis.metrics.lines_of_comments = len([l for l in lines if l.strip().startswith('//')])
        analysis.metrics.blank_lines = len([l for l in lines if not l.strip()])
        analysis.metrics.method_count = len(analysis.methods)
        analysis.metrics.property_count = len(analysis.properties)
        analysis.metrics.test_methods_count = len([m for m in analysis.methods if m.is_test])

        # Complexidade ciclomatica basica
        complexity_keywords = ['if', 'else', 'for', 'while', 'switch', 'case', 'catch', '&&', '||', '?']
        body_lower = analysis.body.lower()
        analysis.metrics.cyclomatic_complexity = 1 + sum(
            body_lower.count(kw) for kw in complexity_keywords
        )

    def _identify_patterns(self, analysis: ApexClassAnalysis):
        """Identifica padroes de design"""
        if not analysis.body:
            return

        body_lower = analysis.body.lower()
        name_lower = analysis.name.lower()

        # Singleton
        if 'private static' in body_lower and 'getinstance' in body_lower:
            analysis.patterns.append(ApexPattern.SINGLETON)

        # Factory
        if 'factory' in name_lower or 'create' in body_lower.split('public')[0]:
            analysis.patterns.append(ApexPattern.FACTORY)

        # Service
        if name_lower.endswith('service'):
            analysis.patterns.append(ApexPattern.SERVICE)

        # Selector
        if name_lower.endswith('selector') or 'select' in [m.name.lower()[:6] for m in analysis.methods]:
            analysis.patterns.append(ApexPattern.SELECTOR)

        # Domain
        if name_lower.endswith('domain') or 'trigger.new' in body_lower:
            analysis.patterns.append(ApexPattern.DOMAIN)

        # Trigger Handler
        if 'triggerhandler' in name_lower or 'handlertrigger' in body_lower:
            analysis.patterns.append(ApexPattern.TRIGGER_HANDLER)

        # Test Class
        if 'istest' in [a.lower() for a in analysis.annotations] or name_lower.endswith('test'):
            analysis.patterns.append(ApexPattern.TEST_CLASS)

        # Batch
        if 'batchable' in body_lower or 'database.batchable' in body_lower:
            analysis.patterns.append(ApexPattern.BATCH)

        # Scheduled
        if 'schedulable' in body_lower:
            analysis.patterns.append(ApexPattern.SCHEDULED)

        # Queueable
        if 'queueable' in body_lower:
            analysis.patterns.append(ApexPattern.QUEUEABLE)

        # REST Resource
        if 'restresource' in [a.lower() for a in analysis.annotations]:
            analysis.patterns.append(ApexPattern.REST_RESOURCE)

        # Controller
        if name_lower.endswith('controller'):
            analysis.patterns.append(ApexPattern.CONTROLLER)

    def _identify_dependencies(self, analysis: ApexClassAnalysis):
        """Identifica dependencias da classe"""
        if not analysis.body:
            return

        # Pattern para identificar referencias a outras classes
        class_ref_pattern = r'\b([A-Z][a-zA-Z0-9_]+)\.'

        found_classes = set(re.findall(class_ref_pattern, analysis.body))

        # Remover classes padrao e a propria classe
        standard_classes = {
            'System', 'String', 'Integer', 'Boolean', 'List', 'Map', 'Set',
            'Date', 'DateTime', 'Decimal', 'Double', 'Id', 'Blob', 'Time',
            'Database', 'Test', 'Schema', 'UserInfo', 'Trigger', 'JSON',
            'HttpRequest', 'HttpResponse', 'Http', 'RestContext', 'RestRequest',
            'RestResponse', 'ApexPages', 'PageReference', 'Messaging', 'Math',
            analysis.name
        }

        analysis.dependencies = sorted(list(found_classes - standard_classes))

    def _identify_issues(self, analysis: ApexClassAnalysis):
        """Identifica problemas potenciais no codigo"""
        if not analysis.body:
            return

        body = analysis.body
        lines = body.split('\n')

        # SOQL sem limite
        soql_without_limit = re.findall(
            r'\[SELECT[^]]+\]',
            body,
            re.IGNORECASE
        )
        for soql in soql_without_limit:
            if 'limit' not in soql.lower():
                line_num = body[:body.find(soql)].count('\n') + 1
                analysis.issues.append(CodeIssue(
                    severity="warning",
                    message="Query SOQL sem LIMIT pode retornar muitos registros",
                    line=line_num,
                    rule="SOQL_NO_LIMIT",
                    suggestion="Adicione LIMIT para restringir resultados"
                ))

        # SOQL/DML em loop
        for_loops = re.finditer(r'\bfor\s*\([^)]+\)\s*\{', body, re.IGNORECASE)
        for match in for_loops:
            # Encontrar o bloco do for
            start = match.end()
            brace_count = 1
            end = start
            while brace_count > 0 and end < len(body):
                if body[end] == '{':
                    brace_count += 1
                elif body[end] == '}':
                    brace_count -= 1
                end += 1

            loop_body = body[start:end]

            if re.search(r'\[SELECT', loop_body, re.IGNORECASE):
                line_num = body[:match.start()].count('\n') + 1
                analysis.issues.append(CodeIssue(
                    severity="error",
                    message="Query SOQL dentro de loop pode exceder limites de governor",
                    line=line_num,
                    rule="SOQL_IN_LOOP",
                    suggestion="Mova a query para fora do loop e use uma colecao"
                ))

            if re.search(r'\b(insert|update|delete|upsert)\b', loop_body, re.IGNORECASE):
                line_num = body[:match.start()].count('\n') + 1
                analysis.issues.append(CodeIssue(
                    severity="error",
                    message="Operacao DML dentro de loop pode exceder limites de governor",
                    line=line_num,
                    rule="DML_IN_LOOP",
                    suggestion="Colecione os registros e execute DML fora do loop"
                ))

        # Metodos muito longos (mais de 100 linhas)
        if analysis.metrics.lines_of_code > 0:
            avg_method_lines = analysis.metrics.lines_of_code / max(analysis.metrics.method_count, 1)
            if avg_method_lines > 50:
                analysis.issues.append(CodeIssue(
                    severity="info",
                    message=f"Metodos com media de {avg_method_lines:.0f} linhas podem ser complexos demais",
                    line=1,
                    rule="LONG_METHODS",
                    suggestion="Considere dividir em metodos menores"
                ))

        # Complexidade muito alta
        if analysis.metrics.cyclomatic_complexity > 20:
            analysis.issues.append(CodeIssue(
                severity="warning",
                message=f"Complexidade ciclomatica alta ({analysis.metrics.cyclomatic_complexity})",
                line=1,
                rule="HIGH_COMPLEXITY",
                suggestion="Considere refatorar para reduzir complexidade"
            ))

        # Classe sem testes
        if ApexPattern.TEST_CLASS not in analysis.patterns:
            has_test_methods = analysis.metrics.test_methods_count > 0
            if not has_test_methods and len(analysis.methods) > 0:
                analysis.issues.append(CodeIssue(
                    severity="info",
                    message="Classe sem metodos de teste vissiveis",
                    line=1,
                    rule="NO_TESTS",
                    suggestion="Crie uma classe de teste para garantir cobertura"
                ))

    def _analyze_trigger_structure(self, analysis: ApexTriggerAnalysis):
        """Analisa estrutura do trigger"""
        if not analysis.body:
            return

        body = analysis.body

        # Verifica se chama handler
        handler_pattern = r'(\w+Handler)\.(?:handle|run|execute)'
        handler_match = re.search(handler_pattern, body)

        if handler_match:
            analysis.calls_handler = True
            analysis.handler_class = handler_match.group(1)

        # Verifica se tem logica direta
        logic_indicators = ['if', 'for', 'while', 'insert', 'update', 'delete', '[select']
        body_lower = body.lower()

        if any(ind in body_lower for ind in logic_indicators):
            analysis.has_logic = True

            if not analysis.calls_handler:
                analysis.issues.append(CodeIssue(
                    severity="warning",
                    message="Trigger contem logica direta sem usar handler",
                    line=1,
                    rule="TRIGGER_NO_HANDLER",
                    suggestion="Mova a logica para uma classe handler"
                ))

    async def _get_coverage(self, analysis: ApexClassAnalysis):
        """Obtem coverage da classe"""
        try:
            coverage_list = await self.tooling.get_code_coverage([analysis.name])

            for cov in coverage_list:
                if cov["name"] == analysis.name:
                    analysis.lines_covered = cov.get("linesCovered", 0)
                    analysis.lines_uncovered = cov.get("linesUncovered", 0)
                    analysis.coverage_percentage = cov.get("coveragePercentage", 0)
                    break
        except Exception as e:
            logger.warning(f"Erro ao obter coverage: {e}")

    async def list_all_classes(
        self,
        include_tests: bool = True,
        include_managed: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Lista todas as classes Apex

        Args:
            include_tests: Incluir classes de teste
            include_managed: Incluir classes de pacotes gerenciados

        Returns:
            Lista de classes com informacoes basicas
        """
        classes = await self.tooling.list_apex_classes()

        result = []
        for cls in classes:
            # Filtrar managed packages
            if cls.namespace_prefix and not include_managed:
                continue

            # Identificar se e classe de teste
            is_test = cls.name.lower().endswith('test') or '@istest' in (cls.body or '').lower()[:500]

            if is_test and not include_tests:
                continue

            result.append({
                "id": cls.id,
                "name": cls.name,
                "api_version": cls.api_version,
                "status": cls.status,
                "is_valid": cls.is_valid,
                "is_test": is_test,
                "namespace": cls.namespace_prefix,
                "length": cls.length_without_comments
            })

        return result

    async def get_org_coverage(self) -> Dict[str, Any]:
        """
        Obtem coverage geral da org

        Returns:
            Dict com metricas de coverage
        """
        coverage_list = await self.tooling.get_code_coverage()

        total_covered = sum(c.get("linesCovered", 0) for c in coverage_list)
        total_uncovered = sum(c.get("linesUncovered", 0) for c in coverage_list)
        total_lines = total_covered + total_uncovered

        org_coverage = (total_covered / total_lines * 100) if total_lines > 0 else 0

        # Classes com baixa cobertura
        low_coverage = [
            c for c in coverage_list
            if c.get("coveragePercentage", 100) < 75
        ]

        return {
            "org_coverage_percentage": round(org_coverage, 2),
            "total_lines": total_lines,
            "covered_lines": total_covered,
            "uncovered_lines": total_uncovered,
            "classes_analyzed": len(coverage_list),
            "classes_below_75": len(low_coverage),
            "low_coverage_classes": sorted(
                low_coverage,
                key=lambda x: x.get("coveragePercentage", 0)
            )[:10]
        }

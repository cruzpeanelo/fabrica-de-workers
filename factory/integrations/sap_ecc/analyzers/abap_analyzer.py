# -*- coding: utf-8 -*-
"""
ABAP Analyzer
=============
Analisador de codigo ABAP do SAP.

Este modulo fornece:
- Listagem de programas ABAP
- Analise de codigo fonte
- Extracao de estruturas e variaveis
- Deteccao de chamadas de funcoes
- Analise de performance e boas praticas

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.analyzers import ABAPAnalyzer
from factory.integrations.sap_ecc.rfc_client import SAPRFCClient, SAPRFCConfig

config = SAPRFCConfig(...)
client = SAPRFCClient(config)
await client.connect()

analyzer = ABAPAnalyzer(client)

# Listar programas
programs = await analyzer.list_programs(prefix="Z")

# Analisar codigo
analysis = await analyzer.analyze_program("ZSAMPLE_REPORT")
print(analysis.complexity_score)
print(analysis.called_functions)
```
"""

import re
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set, Tuple

logger = logging.getLogger(__name__)


class ABAPObjectType(str, Enum):
    """Tipos de objetos ABAP"""
    REPORT = "PROG"
    FUNCTION_MODULE = "FUGR"
    FUNCTION = "FUNC"
    CLASS = "CLAS"
    METHOD = "METH"
    INTERFACE = "INTF"
    TABLE = "TABL"
    STRUCTURE = "STRU"
    DATA_ELEMENT = "DTEL"
    DOMAIN = "DOMA"
    TABLE_TYPE = "TTYP"
    TRANSACTION = "TRAN"
    ENHANCEMENT = "ENHS"
    BADI_IMPL = "BADI"


class ABAPComplexity(str, Enum):
    """Niveis de complexidade ABAP"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    VERY_HIGH = "very_high"


@dataclass
class ABAPVariable:
    """Variavel ABAP"""
    name: str
    data_type: str
    is_table: bool = False
    is_structure: bool = False
    is_reference: bool = False
    initial_value: Optional[str] = None
    line_number: int = 0


@dataclass
class ABAPMethod:
    """Metodo ABAP"""
    name: str
    visibility: str = "PUBLIC"  # PUBLIC, PROTECTED, PRIVATE
    is_static: bool = False
    is_abstract: bool = False
    is_final: bool = False
    importing: List[Dict] = field(default_factory=list)
    exporting: List[Dict] = field(default_factory=list)
    changing: List[Dict] = field(default_factory=list)
    returning: Optional[Dict] = None
    exceptions: List[str] = field(default_factory=list)
    line_count: int = 0


@dataclass
class ABAPFunctionCall:
    """Chamada de funcao no codigo"""
    function_name: str
    line_number: int
    is_rfc: bool = False
    is_bapi: bool = False
    parameters: Dict[str, str] = field(default_factory=dict)


@dataclass
class ABAPSelectStatement:
    """Statement SELECT no codigo"""
    table_name: str
    fields: List[str] = field(default_factory=list)
    where_clause: Optional[str] = None
    line_number: int = 0
    is_single: bool = False
    has_for_all_entries: bool = False
    has_index: bool = False


@dataclass
class ABAPCodeIssue:
    """Problema encontrado no codigo"""
    severity: str  # error, warning, info
    message: str
    line_number: int
    rule: str
    suggestion: Optional[str] = None


@dataclass
class ABAPAnalysisResult:
    """Resultado da analise de codigo ABAP"""
    program_name: str
    object_type: ABAPObjectType = ABAPObjectType.REPORT
    title: Optional[str] = None
    author: Optional[str] = None
    created_date: Optional[datetime] = None
    changed_date: Optional[datetime] = None
    package: Optional[str] = None

    # Metricas
    total_lines: int = 0
    code_lines: int = 0
    comment_lines: int = 0
    blank_lines: int = 0

    # Complexidade
    complexity_score: int = 0
    complexity_level: ABAPComplexity = ABAPComplexity.LOW
    cyclomatic_complexity: int = 0

    # Estrutura
    variables: List[ABAPVariable] = field(default_factory=list)
    methods: List[ABAPMethod] = field(default_factory=list)
    called_functions: List[ABAPFunctionCall] = field(default_factory=list)
    select_statements: List[ABAPSelectStatement] = field(default_factory=list)
    includes: List[str] = field(default_factory=list)
    forms: List[str] = field(default_factory=list)

    # Classes e interfaces usadas
    used_classes: Set[str] = field(default_factory=set)
    implemented_interfaces: Set[str] = field(default_factory=set)

    # Problemas
    issues: List[ABAPCodeIssue] = field(default_factory=list)

    # Codigo fonte
    source_code: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "program_name": self.program_name,
            "object_type": self.object_type.value,
            "title": self.title,
            "author": self.author,
            "package": self.package,
            "metrics": {
                "total_lines": self.total_lines,
                "code_lines": self.code_lines,
                "comment_lines": self.comment_lines,
                "blank_lines": self.blank_lines
            },
            "complexity": {
                "score": self.complexity_score,
                "level": self.complexity_level.value,
                "cyclomatic": self.cyclomatic_complexity
            },
            "structure": {
                "variables_count": len(self.variables),
                "methods_count": len(self.methods),
                "function_calls": len(self.called_functions),
                "select_count": len(self.select_statements),
                "includes": self.includes,
                "forms": self.forms
            },
            "issues_count": len(self.issues),
            "issues_by_severity": {
                "error": len([i for i in self.issues if i.severity == "error"]),
                "warning": len([i for i in self.issues if i.severity == "warning"]),
                "info": len([i for i in self.issues if i.severity == "info"])
            }
        }


class ABAPAnalyzer:
    """
    Analisador de codigo ABAP.

    Fornece analise estatica de programas ABAP incluindo:
    - Metricas de codigo (linhas, comentarios, etc.)
    - Complexidade ciclomatica
    - Chamadas de funcoes e BAPIs
    - Statements SQL
    - Deteccao de problemas e boas praticas
    """

    # Padroes regex para analise ABAP
    PATTERNS = {
        "comment": re.compile(r'^\s*[*"]', re.IGNORECASE),
        "data_declaration": re.compile(
            r'^\s*DATA[:\s]+(\w+)\s+TYPE\s+(\w+)',
            re.IGNORECASE
        ),
        "table_declaration": re.compile(
            r'^\s*DATA[:\s]+(\w+)\s+TYPE\s+(?:STANDARD\s+)?TABLE\s+OF\s+(\w+)',
            re.IGNORECASE
        ),
        "function_call": re.compile(
            r"CALL\s+FUNCTION\s+['\"](\w+)['\"]",
            re.IGNORECASE
        ),
        "method_call": re.compile(
            r'(?:CALL\s+METHOD|->|=>)\s*(\w+)',
            re.IGNORECASE
        ),
        "select_single": re.compile(
            r'SELECT\s+SINGLE\s+(.+?)\s+FROM\s+(\w+)',
            re.IGNORECASE | re.DOTALL
        ),
        "select": re.compile(
            r'SELECT\s+(.+?)\s+FROM\s+(\w+)',
            re.IGNORECASE | re.DOTALL
        ),
        "perform": re.compile(
            r'PERFORM\s+(\w+)',
            re.IGNORECASE
        ),
        "form": re.compile(
            r'FORM\s+(\w+)',
            re.IGNORECASE
        ),
        "endform": re.compile(r'ENDFORM', re.IGNORECASE),
        "include": re.compile(
            r'INCLUDE\s+(\w+)',
            re.IGNORECASE
        ),
        "class_definition": re.compile(
            r'CLASS\s+(\w+)\s+DEFINITION',
            re.IGNORECASE
        ),
        "class_implementation": re.compile(
            r'CLASS\s+(\w+)\s+IMPLEMENTATION',
            re.IGNORECASE
        ),
        "method_definition": re.compile(
            r'METHODS?\s+(\w+)',
            re.IGNORECASE
        ),
        "if_statement": re.compile(r'^\s*IF\b', re.IGNORECASE),
        "elseif_statement": re.compile(r'^\s*ELSEIF\b', re.IGNORECASE),
        "case_statement": re.compile(r'^\s*CASE\b', re.IGNORECASE),
        "when_statement": re.compile(r'^\s*WHEN\b', re.IGNORECASE),
        "loop_statement": re.compile(r'^\s*(?:LOOP|DO|WHILE)\b', re.IGNORECASE),
        "create_object": re.compile(
            r'CREATE\s+OBJECT\s+(\w+)\s+TYPE\s+(\w+)',
            re.IGNORECASE
        ),
        "new_expression": re.compile(
            r'NEW\s+(\w+)\s*\(',
            re.IGNORECASE
        )
    }

    def __init__(self, rfc_client):
        """
        Inicializa o analisador.

        Args:
            rfc_client: Cliente RFC conectado ao SAP
        """
        self.rfc_client = rfc_client

    async def list_programs(
        self,
        prefix: str = "Z",
        package: Optional[str] = None,
        author: Optional[str] = None,
        max_results: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Lista programas ABAP.

        Args:
            prefix: Prefixo do programa (ex: Z, Y)
            package: Pacote/Devclass
            author: Autor do programa
            max_results: Maximo de resultados

        Returns:
            Lista de programas
        """
        # Montar filtros
        options = []
        if prefix:
            options.append({"TEXT": f"PROGNAME LIKE '{prefix}%'"})
        if package:
            options.append({"TEXT": f"AND DEVCLASS = '{package}'"})
        if author:
            options.append({"TEXT": f"AND AUTHOR = '{author}'"})

        # Buscar na tabela TRDIR (diretorio de programas)
        programs = await self.rfc_client.read_table(
            "TRDIR",
            fields=["NAME", "SUBC", "CNAM", "CDAT", "UNAM", "UDAT", "DEVCLASS"],
            options=options,
            max_rows=max_results
        )

        # Enriquecer com titulo
        result = []
        for prog in programs:
            result.append({
                "name": prog.get("NAME", ""),
                "type": self._get_program_type(prog.get("SUBC", "")),
                "author": prog.get("CNAM", ""),
                "created_date": prog.get("CDAT", ""),
                "changed_by": prog.get("UNAM", ""),
                "changed_date": prog.get("UDAT", ""),
                "package": prog.get("DEVCLASS", "")
            })

        return result

    async def get_program_source(self, program_name: str) -> List[str]:
        """
        Obtem codigo fonte de um programa ABAP.

        Args:
            program_name: Nome do programa

        Returns:
            Lista de linhas do codigo fonte
        """
        result = await self.rfc_client.call_function(
            "RPY_PROGRAM_READ",
            {
                "PROGRAM_NAME": program_name.upper(),
                "WITH_LOWERCASE": "X"
            }
        )

        if not result.success:
            logger.error(f"Erro ao ler programa {program_name}: {result.error}")
            return []

        # Extrair linhas do codigo
        source_lines = []
        source_table = result.tables.get("SOURCE", [])
        for line in source_table:
            source_lines.append(line.get("LINE", ""))

        return source_lines

    async def analyze_program(self, program_name: str) -> ABAPAnalysisResult:
        """
        Analisa um programa ABAP.

        Args:
            program_name: Nome do programa

        Returns:
            ABAPAnalysisResult com a analise completa
        """
        # Obter codigo fonte
        source_code = await self.get_program_source(program_name)

        if not source_code:
            return ABAPAnalysisResult(
                program_name=program_name,
                issues=[ABAPCodeIssue(
                    severity="error",
                    message="Programa nao encontrado ou sem codigo fonte",
                    line_number=0,
                    rule="program_exists"
                )]
            )

        # Obter metadata do programa
        metadata = await self._get_program_metadata(program_name)

        # Criar resultado
        result = ABAPAnalysisResult(
            program_name=program_name,
            source_code=source_code,
            title=metadata.get("title"),
            author=metadata.get("author"),
            package=metadata.get("package"),
            created_date=metadata.get("created_date"),
            changed_date=metadata.get("changed_date")
        )

        # Analisar codigo
        self._analyze_lines(source_code, result)
        self._extract_declarations(source_code, result)
        self._extract_function_calls(source_code, result)
        self._extract_select_statements(source_code, result)
        self._calculate_complexity(source_code, result)
        self._check_best_practices(source_code, result)

        return result

    async def _get_program_metadata(self, program_name: str) -> Dict[str, Any]:
        """Obtem metadata de um programa"""
        try:
            data = await self.rfc_client.read_table(
                "TRDIR",
                fields=["NAME", "CNAM", "CDAT", "UNAM", "UDAT", "DEVCLASS"],
                options=[{"TEXT": f"NAME = '{program_name.upper()}'"}],
                max_rows=1
            )

            if data:
                prog = data[0]
                return {
                    "author": prog.get("CNAM", ""),
                    "created_date": prog.get("CDAT"),
                    "changed_by": prog.get("UNAM", ""),
                    "changed_date": prog.get("UDAT"),
                    "package": prog.get("DEVCLASS", "")
                }

            # Buscar titulo na TRDIRT
            titles = await self.rfc_client.read_table(
                "TRDIRT",
                fields=["NAME", "TEXT"],
                options=[{"TEXT": f"NAME = '{program_name.upper()}' AND SPRSL = 'P'"}],
                max_rows=1
            )

            if titles:
                return {"title": titles[0].get("TEXT", "")}

        except Exception as e:
            logger.warning(f"Erro ao obter metadata de {program_name}: {e}")

        return {}

    def _analyze_lines(self, source: List[str], result: ABAPAnalysisResult):
        """Analisa metricas de linhas"""
        result.total_lines = len(source)

        for line in source:
            stripped = line.strip()

            if not stripped:
                result.blank_lines += 1
            elif self.PATTERNS["comment"].match(line):
                result.comment_lines += 1
            else:
                result.code_lines += 1

    def _extract_declarations(self, source: List[str], result: ABAPAnalysisResult):
        """Extrai declaracoes de variaveis"""
        for i, line in enumerate(source, 1):
            # Variaveis simples
            match = self.PATTERNS["data_declaration"].search(line)
            if match:
                result.variables.append(ABAPVariable(
                    name=match.group(1),
                    data_type=match.group(2),
                    line_number=i
                ))
                continue

            # Tabelas internas
            match = self.PATTERNS["table_declaration"].search(line)
            if match:
                result.variables.append(ABAPVariable(
                    name=match.group(1),
                    data_type=match.group(2),
                    is_table=True,
                    line_number=i
                ))
                continue

            # Includes
            match = self.PATTERNS["include"].search(line)
            if match:
                result.includes.append(match.group(1))

            # Forms
            match = self.PATTERNS["form"].search(line)
            if match:
                result.forms.append(match.group(1))

    def _extract_function_calls(self, source: List[str], result: ABAPAnalysisResult):
        """Extrai chamadas de funcoes"""
        for i, line in enumerate(source, 1):
            match = self.PATTERNS["function_call"].search(line)
            if match:
                func_name = match.group(1)
                result.called_functions.append(ABAPFunctionCall(
                    function_name=func_name,
                    line_number=i,
                    is_rfc=func_name.startswith("RFC_") or "_RFC" in func_name,
                    is_bapi=func_name.startswith("BAPI_")
                ))

    def _extract_select_statements(self, source: List[str], result: ABAPAnalysisResult):
        """Extrai statements SELECT"""
        full_source = "\n".join(source)

        # SELECT SINGLE
        for match in self.PATTERNS["select_single"].finditer(full_source):
            fields = match.group(1).strip()
            table = match.group(2)

            result.select_statements.append(ABAPSelectStatement(
                table_name=table,
                fields=fields.split() if fields != "*" else ["*"],
                is_single=True,
                line_number=self._get_line_number(source, match.start())
            ))

        # SELECT (nao single)
        for match in self.PATTERNS["select"].finditer(full_source):
            if "SINGLE" not in match.group(0).upper():
                fields = match.group(1).strip()
                table = match.group(2)

                has_fae = "FOR ALL ENTRIES" in match.group(0).upper()

                result.select_statements.append(ABAPSelectStatement(
                    table_name=table,
                    fields=fields.split() if fields != "*" else ["*"],
                    is_single=False,
                    has_for_all_entries=has_fae,
                    line_number=self._get_line_number(source, match.start())
                ))

    def _calculate_complexity(self, source: List[str], result: ABAPAnalysisResult):
        """Calcula complexidade ciclomatica"""
        complexity = 1  # Base

        for line in source:
            # Decisoes
            if self.PATTERNS["if_statement"].search(line):
                complexity += 1
            if self.PATTERNS["elseif_statement"].search(line):
                complexity += 1
            if self.PATTERNS["case_statement"].search(line):
                complexity += 1
            if self.PATTERNS["when_statement"].search(line):
                complexity += 1
            if self.PATTERNS["loop_statement"].search(line):
                complexity += 1

        result.cyclomatic_complexity = complexity

        # Pontuacao geral
        score = complexity
        score += len(result.select_statements) * 2
        score += len(result.called_functions)
        score += result.code_lines // 100

        result.complexity_score = score

        # Nivel de complexidade
        if score <= 10:
            result.complexity_level = ABAPComplexity.LOW
        elif score <= 25:
            result.complexity_level = ABAPComplexity.MEDIUM
        elif score <= 50:
            result.complexity_level = ABAPComplexity.HIGH
        else:
            result.complexity_level = ABAPComplexity.VERY_HIGH

    def _check_best_practices(self, source: List[str], result: ABAPAnalysisResult):
        """Verifica boas praticas ABAP"""

        # SELECT * (evitar)
        for i, line in enumerate(source, 1):
            if re.search(r'SELECT\s+\*\s+FROM', line, re.IGNORECASE):
                result.issues.append(ABAPCodeIssue(
                    severity="warning",
                    message="Evite SELECT * - especifique os campos necessarios",
                    line_number=i,
                    rule="no_select_star",
                    suggestion="Especifique apenas os campos que serao usados"
                ))

        # SELECT sem WHERE em tabela grande
        for select in result.select_statements:
            if not select.is_single and not select.where_clause:
                result.issues.append(ABAPCodeIssue(
                    severity="warning",
                    message=f"SELECT na tabela {select.table_name} sem WHERE",
                    line_number=select.line_number,
                    rule="select_without_where",
                    suggestion="Adicione clausula WHERE para limitar resultados"
                ))

        # Codigo muito longo
        if result.code_lines > 500:
            result.issues.append(ABAPCodeIssue(
                severity="info",
                message=f"Programa com {result.code_lines} linhas - considere modularizar",
                line_number=0,
                rule="code_length",
                suggestion="Divida em includes ou use orientacao a objetos"
            ))

        # Muitos SELECTs
        if len(result.select_statements) > 20:
            result.issues.append(ABAPCodeIssue(
                severity="warning",
                message=f"Programa com {len(result.select_statements)} SELECTs",
                line_number=0,
                rule="too_many_selects",
                suggestion="Considere consolidar queries ou usar JOINs"
            ))

        # FOR ALL ENTRIES sem verificacao de tabela vazia
        for select in result.select_statements:
            if select.has_for_all_entries:
                # Verificar se ha CHECK ou IF antes
                result.issues.append(ABAPCodeIssue(
                    severity="warning",
                    message="FOR ALL ENTRIES - verifique se a tabela interna nao esta vazia",
                    line_number=select.line_number,
                    rule="for_all_entries_check",
                    suggestion="Adicione IF lt_table IS NOT INITIAL antes do SELECT"
                ))

    def _get_line_number(self, source: List[str], char_position: int) -> int:
        """Calcula numero da linha a partir da posicao do caractere"""
        current_pos = 0
        for i, line in enumerate(source, 1):
            current_pos += len(line) + 1  # +1 para o \n
            if current_pos > char_position:
                return i
        return len(source)

    def _get_program_type(self, subc: str) -> str:
        """Converte tipo de programa"""
        types = {
            "1": "Report",
            "I": "Include",
            "M": "Module Pool",
            "F": "Function Group",
            "S": "Subroutine Pool",
            "J": "Interface Pool",
            "K": "Class Pool",
            "T": "Type Pool"
        }
        return types.get(subc, "Unknown")

    async def list_function_modules(
        self,
        function_group: Optional[str] = None,
        prefix: str = "Z"
    ) -> List[Dict[str, Any]]:
        """
        Lista Function Modules.

        Args:
            function_group: Grupo de funcoes
            prefix: Prefixo (Z, Y, etc.)

        Returns:
            Lista de function modules
        """
        options = []
        if prefix:
            options.append({"TEXT": f"FUNCNAME LIKE '{prefix}%'"})
        if function_group:
            options.append({"TEXT": f"AND PNAME = '{function_group}'"})

        functions = await self.rfc_client.read_table(
            "TFDIR",
            fields=["FUNCNAME", "PNAME", "FMODE"],
            options=options,
            max_rows=100
        )

        result = []
        for func in functions:
            result.append({
                "name": func.get("FUNCNAME", ""),
                "group": func.get("PNAME", ""),
                "mode": func.get("FMODE", ""),
                "is_rfc": func.get("FMODE", "") == "R"
            })

        return result

    async def get_function_interface(
        self,
        function_name: str
    ) -> Dict[str, Any]:
        """
        Obtem interface de uma Function Module.

        Args:
            function_name: Nome da funcao

        Returns:
            Dicionario com parametros da funcao
        """
        return await self.rfc_client.get_function_interface(function_name)

    async def list_classes(
        self,
        prefix: str = "Z",
        package: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista classes ABAP.

        Args:
            prefix: Prefixo da classe
            package: Pacote

        Returns:
            Lista de classes
        """
        options = []
        if prefix:
            options.append({"TEXT": f"CLSNAME LIKE '{prefix}%'"})
        if package:
            options.append({"TEXT": f"AND DEVCLASS = '{package}'"})

        classes = await self.rfc_client.read_table(
            "SEOCLASS",
            fields=["CLSNAME", "VERSION", "CATEGORY", "EXPOSURE",
                    "STATE", "AUTHOR", "CREATEDON"],
            options=options,
            max_rows=100
        )

        result = []
        for cls in classes:
            result.append({
                "name": cls.get("CLSNAME", ""),
                "category": cls.get("CATEGORY", ""),
                "visibility": cls.get("EXPOSURE", ""),
                "state": cls.get("STATE", ""),
                "author": cls.get("AUTHOR", ""),
                "created_on": cls.get("CREATEDON", "")
            })

        return result

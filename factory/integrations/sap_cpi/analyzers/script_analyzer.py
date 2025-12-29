# -*- coding: utf-8 -*-
"""
SAP CPI Script Analyzer
=======================

Analisador de scripts Groovy do SAP CPI.

Funcionalidades:
- Análise de estrutura de scripts
- Identificação de funções e classes
- Detecção de variáveis de mensagem CPI
- Análise de dependências e imports
- Validação de boas práticas
- Detecção de vulnerabilidades comuns
- Análise de performance
"""

import logging
import re
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple
from enum import Enum

logger = logging.getLogger(__name__)


class ScriptType(str, Enum):
    """Tipos de scripts CPI"""
    GROOVY = "groovy"
    JAVASCRIPT = "javascript"
    UNKNOWN = "unknown"


class SecurityRisk(str, Enum):
    """Níveis de risco de segurança"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class ScriptFunction:
    """Representa uma função em um script"""
    name: str
    parameters: List[str] = field(default_factory=list)
    return_type: str = ""
    line_number: int = 0
    is_public: bool = True
    is_entry_point: bool = False
    body_lines: int = 0
    complexity: int = 1
    description: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "parameters": self.parameters,
            "return_type": self.return_type,
            "line_number": self.line_number,
            "is_public": self.is_public,
            "is_entry_point": self.is_entry_point,
            "body_lines": self.body_lines,
            "complexity": self.complexity,
            "description": self.description,
        }


@dataclass
class SecurityIssue:
    """Representa um problema de segurança"""
    risk_level: SecurityRisk
    issue_type: str
    description: str
    line_number: int = 0
    code_snippet: str = ""
    recommendation: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "risk_level": self.risk_level.value,
            "issue_type": self.issue_type,
            "description": self.description,
            "line_number": self.line_number,
            "code_snippet": self.code_snippet,
            "recommendation": self.recommendation,
        }


@dataclass
class PerformanceIssue:
    """Representa um problema de performance"""
    severity: str
    issue_type: str
    description: str
    line_number: int = 0
    recommendation: str = ""

    def to_dict(self) -> Dict[str, str]:
        return {
            "severity": self.severity,
            "issue_type": self.issue_type,
            "description": self.description,
            "line_number": str(self.line_number),
            "recommendation": self.recommendation,
        }


@dataclass
class ScriptAnalysis:
    """Resultado da análise de um script"""
    script_name: str
    script_type: ScriptType
    total_lines: int = 0
    code_lines: int = 0
    comment_lines: int = 0
    blank_lines: int = 0
    imports: List[str] = field(default_factory=list)
    classes: List[str] = field(default_factory=list)
    functions: List[ScriptFunction] = field(default_factory=list)
    cpi_variables_used: List[str] = field(default_factory=list)
    external_calls: List[str] = field(default_factory=list)
    security_issues: List[SecurityIssue] = field(default_factory=list)
    performance_issues: List[PerformanceIssue] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    best_practices_violations: List[str] = field(default_factory=list)
    avg_complexity: float = 0.0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "script_name": self.script_name,
            "script_type": self.script_type.value,
            "total_lines": self.total_lines,
            "code_lines": self.code_lines,
            "comment_lines": self.comment_lines,
            "blank_lines": self.blank_lines,
            "imports": self.imports,
            "classes": self.classes,
            "functions": [f.to_dict() for f in self.functions],
            "cpi_variables_used": self.cpi_variables_used,
            "external_calls": self.external_calls,
            "security_issues": [s.to_dict() for s in self.security_issues],
            "performance_issues": [p.to_dict() for p in self.performance_issues],
            "warnings": self.warnings,
            "best_practices_violations": self.best_practices_violations,
            "avg_complexity": self.avg_complexity,
            "has_security_issues": len(self.security_issues) > 0,
            "risk_score": self._calculate_risk_score(),
        }

    def _calculate_risk_score(self) -> int:
        """Calcula score de risco (0-100)"""
        score = 0
        for issue in self.security_issues:
            if issue.risk_level == SecurityRisk.CRITICAL:
                score += 40
            elif issue.risk_level == SecurityRisk.HIGH:
                score += 25
            elif issue.risk_level == SecurityRisk.MEDIUM:
                score += 10
            elif issue.risk_level == SecurityRisk.LOW:
                score += 5
        return min(score, 100)

    def get_summary(self) -> str:
        """Retorna resumo textual da análise"""
        lines = [
            f"Script: {self.script_name}",
            f"Tipo: {self.script_type.value}",
            f"Linhas: {self.total_lines} (código: {self.code_lines}, comentários: {self.comment_lines})",
            f"Funções: {len(self.functions)}",
            f"Complexidade média: {self.avg_complexity:.1f}",
            f"Score de risco: {self._calculate_risk_score()}/100",
        ]

        if self.cpi_variables_used:
            lines.append(f"\nVariáveis CPI utilizadas: {', '.join(self.cpi_variables_used)}")

        if self.security_issues:
            lines.append(f"\nProblemas de segurança ({len(self.security_issues)}):")
            for issue in self.security_issues:
                lines.append(f"  [{issue.risk_level.value.upper()}] {issue.description}")

        if self.performance_issues:
            lines.append(f"\nProblemas de performance ({len(self.performance_issues)}):")
            for issue in self.performance_issues:
                lines.append(f"  [{issue.severity}] {issue.description}")

        if self.warnings:
            lines.append(f"\nAvisos ({len(self.warnings)}):")
            for warning in self.warnings:
                lines.append(f"  - {warning}")

        return "\n".join(lines)


class ScriptAnalyzer:
    """
    Analisador de scripts Groovy do SAP CPI.

    Exemplo:
    ```python
    analyzer = ScriptAnalyzer()

    with open("script.groovy", "r", encoding="utf-8") as f:
        content = f.read()

    analysis = analyzer.analyze(content, "meu_script.groovy")
    print(analysis.get_summary())

    # Verificar problemas de segurança
    for issue in analysis.security_issues:
        if issue.risk_level == SecurityRisk.HIGH:
            print(f"ALERTA: {issue.description}")
    ```
    """

    # Variáveis CPI padrão
    CPI_VARIABLES = {
        "message": "Mensagem principal do pipeline",
        "messageLog": "Logger para trace/debug",
        "exchange": "Exchange do Camel",
        "body": "Corpo da mensagem (atalho)",
        "headers": "Headers da mensagem",
        "properties": "Propriedades do exchange",
        "attachments": "Anexos da mensagem",
    }

    # Padrões para detecção de problemas de segurança
    SECURITY_PATTERNS = {
        "sql_injection": (
            r'(?:executeQuery|executeUpdate|prepareStatement)\s*\([^)]*\+',
            SecurityRisk.CRITICAL,
            "Possível SQL Injection - concatenação de strings em query",
            "Use prepared statements com parâmetros"
        ),
        "command_injection": (
            r'(?:Runtime\.getRuntime\(\)|ProcessBuilder)\s*\.\s*exec',
            SecurityRisk.CRITICAL,
            "Execução de comandos do sistema - risco de Command Injection",
            "Evite execução de comandos do sistema em scripts CPI"
        ),
        "hardcoded_credentials": (
            r'(?:password|secret|apikey|api_key|token)\s*=\s*["\'][^"\']+["\']',
            SecurityRisk.HIGH,
            "Credencial possivelmente hardcoded",
            "Use Secure Store/Credential para armazenar credenciais"
        ),
        "hardcoded_url": (
            r'(?:http|https|ftp)://[^\s"\')]+',
            SecurityRisk.MEDIUM,
            "URL hardcoded no script",
            "Externalize URLs para parâmetros configuráveis"
        ),
        "eval_usage": (
            r'(?:evaluate|Eval\.me|GroovyShell\(\)\.evaluate)',
            SecurityRisk.HIGH,
            "Uso de evaluate/Eval - risco de injeção de código",
            "Evite evaluate com entrada não confiável"
        ),
        "file_operations": (
            r'new\s+File\s*\(|FileWriter|FileReader|FileInputStream|FileOutputStream',
            SecurityRisk.MEDIUM,
            "Operações de arquivo - verificar permissões",
            "CPI tem restrições de acesso ao sistema de arquivos"
        ),
        "network_sockets": (
            r'new\s+(?:Socket|ServerSocket|DatagramSocket)',
            SecurityRisk.HIGH,
            "Abertura de sockets de rede",
            "Use adaptadores CPI para comunicação de rede"
        ),
        "reflection": (
            r'\.getClass\(\)\.forName|Class\.forName|getDeclaredMethod|setAccessible\(true\)',
            SecurityRisk.MEDIUM,
            "Uso de reflection - pode contornar restrições de segurança",
            "Evite reflection a menos que absolutamente necessário"
        ),
    }

    # Padrões para problemas de performance
    PERFORMANCE_PATTERNS = {
        "string_concat_loop": (
            r'for\s*\([^)]+\)[^{]*\{[^}]*\+\s*=\s*["\']',
            "HIGH",
            "Concatenação de String em loop",
            "Use StringBuilder para concatenação em loops"
        ),
        "large_payload_memory": (
            r'message\.getBody\(String\.class\)|exchange\.getIn\(\)\.getBody\(String\.class\)',
            "MEDIUM",
            "Carregamento de payload completo na memória",
            "Para payloads grandes, considere streaming"
        ),
        "nested_loops": (
            r'for\s*\([^)]+\)[^{]*\{[^}]*for\s*\([^)]+\)',
            "MEDIUM",
            "Loops aninhados detectados",
            "Verifique complexidade e otimize se possível"
        ),
        "unbounded_collection": (
            r'new\s+(?:ArrayList|HashMap|LinkedList)\s*\(\)',
            "LOW",
            "Coleção sem tamanho inicial definido",
            "Defina tamanho inicial para coleções grandes"
        ),
    }

    def __init__(self):
        """Inicializa o analisador"""
        pass

    def analyze(
        self,
        content: str,
        script_name: str = "script.groovy",
        check_security: bool = True,
        check_performance: bool = True
    ) -> ScriptAnalysis:
        """
        Analisa um script Groovy.

        Args:
            content: Conteúdo do script
            script_name: Nome do script
            check_security: Verificar problemas de segurança
            check_performance: Verificar problemas de performance

        Returns:
            ScriptAnalysis com resultado
        """
        # Detecta tipo do script
        script_type = self._detect_script_type(script_name, content)

        analysis = ScriptAnalysis(
            script_name=script_name,
            script_type=script_type
        )

        # Análise de linhas
        self._analyze_lines(content, analysis)

        # Extrai imports
        self._extract_imports(content, analysis)

        # Extrai classes
        self._extract_classes(content, analysis)

        # Extrai funções
        self._extract_functions(content, analysis)

        # Identifica variáveis CPI
        self._identify_cpi_variables(content, analysis)

        # Identifica chamadas externas
        self._identify_external_calls(content, analysis)

        # Verifica segurança
        if check_security:
            self._check_security(content, analysis)

        # Verifica performance
        if check_performance:
            self._check_performance(content, analysis)

        # Valida boas práticas
        self._validate_best_practices(content, analysis)

        # Calcula complexidade média
        if analysis.functions:
            analysis.avg_complexity = sum(f.complexity for f in analysis.functions) / len(analysis.functions)

        return analysis

    def _detect_script_type(self, name: str, content: str) -> ScriptType:
        """Detecta tipo do script baseado em nome e conteúdo"""
        if name.endswith('.groovy') or name.endswith('.gvy'):
            return ScriptType.GROOVY
        elif name.endswith('.js') or name.endswith('.javascript'):
            return ScriptType.JAVASCRIPT

        # Tenta detectar pelo conteúdo
        if 'def ' in content or 'import groovy' in content:
            return ScriptType.GROOVY
        elif 'function ' in content or 'var ' in content:
            return ScriptType.JAVASCRIPT

        return ScriptType.UNKNOWN

    def _analyze_lines(self, content: str, analysis: ScriptAnalysis) -> None:
        """Analisa estatísticas de linhas"""
        lines = content.split('\n')
        analysis.total_lines = len(lines)

        in_multiline_comment = False

        for line in lines:
            stripped = line.strip()

            if not stripped:
                analysis.blank_lines += 1
            elif in_multiline_comment:
                analysis.comment_lines += 1
                if '*/' in stripped:
                    in_multiline_comment = False
            elif stripped.startswith('/*'):
                analysis.comment_lines += 1
                if '*/' not in stripped:
                    in_multiline_comment = True
            elif stripped.startswith('//') or stripped.startswith('#'):
                analysis.comment_lines += 1
            else:
                analysis.code_lines += 1

    def _extract_imports(self, content: str, analysis: ScriptAnalysis) -> None:
        """Extrai imports do script"""
        # Groovy/Java imports
        groovy_imports = re.findall(r'^import\s+([^\s;]+)', content, re.MULTILINE)
        analysis.imports.extend(groovy_imports)

        # JavaScript requires (se aplicável)
        js_requires = re.findall(r'require\s*\(["\']([^"\']+)["\']\)', content)
        analysis.imports.extend(js_requires)

    def _extract_classes(self, content: str, analysis: ScriptAnalysis) -> None:
        """Extrai classes do script"""
        # Groovy/Java classes
        classes = re.findall(r'(?:class|interface|enum)\s+(\w+)', content)
        analysis.classes.extend(classes)

    def _extract_functions(self, content: str, analysis: ScriptAnalysis) -> None:
        """Extrai funções do script"""
        lines = content.split('\n')

        # Padrões para funções Groovy
        groovy_func_pattern = r'(?:def|public|private|protected|static)?\s*(?:def\s+)?(\w+)\s*\(([^)]*)\)\s*\{'
        js_func_pattern = r'function\s+(\w+)\s*\(([^)]*)\)\s*\{'

        for i, line in enumerate(lines):
            # Groovy
            match = re.search(groovy_func_pattern, line)
            if match:
                func = self._create_function(match, i + 1, lines, i)
                analysis.functions.append(func)
                continue

            # JavaScript
            match = re.search(js_func_pattern, line)
            if match:
                func = self._create_function(match, i + 1, lines, i)
                analysis.functions.append(func)

    def _create_function(
        self,
        match: re.Match,
        line_number: int,
        lines: List[str],
        start_index: int
    ) -> ScriptFunction:
        """Cria objeto ScriptFunction a partir do match"""
        name = match.group(1)
        params_str = match.group(2)

        # Parse parâmetros
        params = []
        if params_str.strip():
            params = [p.strip() for p in params_str.split(',')]

        # Detecta se é entry point CPI
        is_entry_point = name in ['processData', 'processBody', 'process', 'main']

        # Detecta visibilidade
        is_public = 'private' not in lines[start_index].lower()

        # Conta linhas do corpo e calcula complexidade
        body_lines, complexity = self._analyze_function_body(lines, start_index)

        # Extrai descrição do comentário anterior
        description = ""
        if start_index > 0:
            prev_line = lines[start_index - 1].strip()
            if prev_line.startswith('//'):
                description = prev_line[2:].strip()
            elif prev_line.endswith('*/'):
                # Busca início do comentário
                for j in range(start_index - 1, -1, -1):
                    if '/**' in lines[j] or '/*' in lines[j]:
                        description = ' '.join(
                            l.strip().lstrip('/*').rstrip('*/').strip()
                            for l in lines[j:start_index]
                        )
                        break

        return ScriptFunction(
            name=name,
            parameters=params,
            line_number=line_number,
            is_public=is_public,
            is_entry_point=is_entry_point,
            body_lines=body_lines,
            complexity=complexity,
            description=description[:200] if description else ""
        )

    def _analyze_function_body(
        self,
        lines: List[str],
        start_index: int
    ) -> Tuple[int, int]:
        """Analisa corpo da função e retorna (linhas, complexidade)"""
        brace_count = 0
        body_lines = 0
        complexity = 1  # Complexidade ciclomática base
        started = False

        for i in range(start_index, len(lines)):
            line = lines[i]

            # Conta chaves
            brace_count += line.count('{') - line.count('}')

            if '{' in line and not started:
                started = True

            if started:
                body_lines += 1

                # Incrementa complexidade para estruturas de decisão
                complexity += len(re.findall(r'\b(?:if|else if|elif|while|for|case|catch|&&|\|\|)\b', line))

            # Fim da função
            if started and brace_count == 0:
                break

        return body_lines, complexity

    def _identify_cpi_variables(self, content: str, analysis: ScriptAnalysis) -> None:
        """Identifica variáveis CPI utilizadas no script"""
        for var_name in self.CPI_VARIABLES:
            if re.search(rf'\b{var_name}\b', content):
                analysis.cpi_variables_used.append(var_name)

    def _identify_external_calls(self, content: str, analysis: ScriptAnalysis) -> None:
        """Identifica chamadas externas (HTTP, BD, etc)"""
        patterns = [
            (r'new\s+URL\s*\(', 'URL'),
            (r'HttpURLConnection', 'HTTP'),
            (r'HttpClient', 'HTTP Client'),
            (r'Connection\s*=|DriverManager\.getConnection', 'JDBC'),
            (r'new\s+Socket\s*\(', 'Socket'),
            (r'restTemplate', 'REST Template'),
            (r'webServiceTemplate', 'SOAP Client'),
        ]

        for pattern, name in patterns:
            if re.search(pattern, content):
                if name not in analysis.external_calls:
                    analysis.external_calls.append(name)

    def _check_security(self, content: str, analysis: ScriptAnalysis) -> None:
        """Verifica problemas de segurança"""
        lines = content.split('\n')

        for issue_name, (pattern, risk, desc, recommendation) in self.SECURITY_PATTERNS.items():
            matches = list(re.finditer(pattern, content, re.IGNORECASE))

            for match in matches:
                # Encontra número da linha
                line_num = content[:match.start()].count('\n') + 1

                # Extrai snippet
                line_content = lines[line_num - 1].strip() if line_num <= len(lines) else ""

                issue = SecurityIssue(
                    risk_level=risk,
                    issue_type=issue_name,
                    description=desc,
                    line_number=line_num,
                    code_snippet=line_content[:100],
                    recommendation=recommendation
                )
                analysis.security_issues.append(issue)

    def _check_performance(self, content: str, analysis: ScriptAnalysis) -> None:
        """Verifica problemas de performance"""
        lines = content.split('\n')

        for issue_name, (pattern, severity, desc, recommendation) in self.PERFORMANCE_PATTERNS.items():
            matches = list(re.finditer(pattern, content, re.IGNORECASE | re.DOTALL))

            for match in matches:
                line_num = content[:match.start()].count('\n') + 1

                issue = PerformanceIssue(
                    severity=severity,
                    issue_type=issue_name,
                    description=desc,
                    line_number=line_num,
                    recommendation=recommendation
                )
                analysis.performance_issues.append(issue)

    def _validate_best_practices(self, content: str, analysis: ScriptAnalysis) -> None:
        """Valida boas práticas de codificação"""

        # 1. Verifica se há função entry point
        has_entry_point = any(f.is_entry_point for f in analysis.functions)
        if not has_entry_point and analysis.functions:
            analysis.warnings.append(
                "Nenhuma função entry point encontrada (processData, process, etc). "
                "O CPI pode não conseguir executar o script."
            )

        # 2. Verifica funções muito complexas
        for func in analysis.functions:
            if func.complexity > 10:
                analysis.best_practices_violations.append(
                    f"Função '{func.name}' tem complexidade alta ({func.complexity}). "
                    "Considere refatorar em funções menores."
                )

        # 3. Verifica funções muito longas
        for func in analysis.functions:
            if func.body_lines > 50:
                analysis.best_practices_violations.append(
                    f"Função '{func.name}' muito longa ({func.body_lines} linhas). "
                    "Considere dividir em funções menores."
                )

        # 4. Verifica tratamento de exceções
        if 'try' in content and 'catch' not in content:
            analysis.warnings.append(
                "Bloco try sem catch encontrado. Certifique-se de tratar exceções."
            )

        # 5. Verifica uso de System.out/println
        if 'System.out' in content or 'println' in content:
            analysis.best_practices_violations.append(
                "Uso de System.out/println. Use messageLog para logging no CPI."
            )

        # 6. Verifica comentários insuficientes
        if analysis.code_lines > 20 and analysis.comment_lines < analysis.code_lines * 0.1:
            analysis.warnings.append(
                f"Poucos comentários ({analysis.comment_lines} linhas de comentário para "
                f"{analysis.code_lines} linhas de código). Considere documentar melhor."
            )

        # 7. Verifica imports não utilizados
        for imp in analysis.imports:
            # Extrai nome da classe do import
            class_name = imp.split('.')[-1]
            if class_name not in content or content.count(class_name) == 1:
                analysis.warnings.append(f"Import possivelmente não utilizado: {imp}")

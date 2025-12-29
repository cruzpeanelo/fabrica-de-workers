"""
Code Reviewer - Revisao de Codigo Assistida por IA
==================================================

Este modulo fornece revisao automatizada de codigo usando Claude AI.
Analisa codigo para encontrar:
- Bugs potenciais
- Vulnerabilidades de seguranca
- Violacoes de boas praticas
- Problemas de performance
- Oportunidades de melhoria

GitHub Issue: #52
"""

import os
import json
import re
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path

# Tenta importar anthropic
try:
    import anthropic
    HAS_ANTHROPIC = True
except ImportError:
    HAS_ANTHROPIC = False


class IssueSeverity(str, Enum):
    """Severidade do problema encontrado"""
    CRITICAL = "critical"    # Bugs graves, vulnerabilidades de seguranca
    WARNING = "warning"      # Problemas que devem ser corrigidos
    INFO = "info"           # Sugestoes e melhorias opcionais


class IssueType(str, Enum):
    """Tipo de problema encontrado"""
    BUG = "bug"                      # Bug potencial ou real
    SECURITY = "security"            # Vulnerabilidade de seguranca
    STYLE = "style"                  # Violacao de estilo/padrao
    PERFORMANCE = "performance"      # Problema de performance
    MAINTAINABILITY = "maintainability"  # Dificil de manter/entender
    BEST_PRACTICE = "best_practice"  # Violacao de boas praticas
    DOCUMENTATION = "documentation"  # Falta de documentacao


@dataclass
class ReviewIssue:
    """Problema encontrado no code review"""
    file_path: str
    line_start: int
    line_end: Optional[int]
    issue_type: IssueType
    severity: IssueSeverity
    title: str
    description: str
    suggestion: str
    code_snippet: Optional[str] = None

    def to_dict(self) -> Dict:
        return {
            "file_path": self.file_path,
            "line_start": self.line_start,
            "line_end": self.line_end,
            "issue_type": self.issue_type.value,
            "severity": self.severity.value,
            "title": self.title,
            "description": self.description,
            "suggestion": self.suggestion,
            "code_snippet": self.code_snippet
        }


@dataclass
class ReviewResult:
    """Resultado completo do code review"""
    success: bool
    score: int  # 0-100
    summary: str
    issues: List[ReviewIssue] = field(default_factory=list)
    suggestions: List[str] = field(default_factory=list)
    positives: List[str] = field(default_factory=list)
    metrics: Dict[str, Any] = field(default_factory=dict)
    reviewed_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())
    error: Optional[str] = None

    def to_dict(self) -> Dict:
        return {
            "success": self.success,
            "score": self.score,
            "summary": self.summary,
            "issues": [i.to_dict() for i in self.issues],
            "suggestions": self.suggestions,
            "positives": self.positives,
            "metrics": self.metrics,
            "reviewed_at": self.reviewed_at,
            "error": self.error
        }

    @property
    def critical_count(self) -> int:
        return len([i for i in self.issues if i.severity == IssueSeverity.CRITICAL])

    @property
    def warning_count(self) -> int:
        return len([i for i in self.issues if i.severity == IssueSeverity.WARNING])

    @property
    def info_count(self) -> int:
        return len([i for i in self.issues if i.severity == IssueSeverity.INFO])


class CodeReviewer:
    """
    Revisor de codigo usando Claude AI

    Analisa codigo para encontrar problemas e sugerir melhorias.
    Pode revisar arquivos individuais ou projetos inteiros.
    """

    DEFAULT_MODEL = "claude-sonnet-4-20250514"
    MAX_TOKENS = 4096

    def __init__(self, api_key: Optional[str] = None):
        """
        Inicializa o revisor de codigo

        Args:
            api_key: Chave da API Anthropic. Se None, usa ANTHROPIC_API_KEY do ambiente
        """
        self.api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        self.client = None

        if HAS_ANTHROPIC and self.api_key:
            self.client = anthropic.Anthropic(api_key=self.api_key)
            print("[CodeReviewer] Inicializado com sucesso")
        elif not HAS_ANTHROPIC:
            print("[CodeReviewer] AVISO: anthropic nao instalado")
        else:
            print("[CodeReviewer] AVISO: API key nao configurada")

    def is_available(self) -> bool:
        """Verifica se o revisor esta disponivel"""
        return self.client is not None

    def review_code(
        self,
        code: str,
        file_path: str = "unknown",
        language: str = "python",
        context: Optional[str] = None
    ) -> ReviewResult:
        """
        Revisa um trecho de codigo

        Args:
            code: Codigo a ser revisado
            file_path: Caminho do arquivo (para referencia)
            language: Linguagem do codigo
            context: Contexto adicional sobre o projeto

        Returns:
            ReviewResult com issues encontradas, score e sugestoes
        """
        if not self.is_available():
            return self._fallback_review(code, file_path, language)

        try:
            response = self._call_claude_review(code, file_path, language, context)
            return self._parse_review_response(response, code, file_path)
        except Exception as e:
            print(f"[CodeReviewer] Erro ao revisar: {e}")
            return ReviewResult(
                success=False,
                score=0,
                summary=f"Erro ao revisar codigo: {str(e)}",
                error=str(e)
            )

    def review_file(self, file_path: str, context: Optional[str] = None) -> ReviewResult:
        """
        Revisa um arquivo completo

        Args:
            file_path: Caminho do arquivo
            context: Contexto adicional

        Returns:
            ReviewResult
        """
        path = Path(file_path)
        if not path.exists():
            return ReviewResult(
                success=False,
                score=0,
                summary=f"Arquivo nao encontrado: {file_path}",
                error="File not found"
            )

        try:
            code = path.read_text(encoding='utf-8')
            language = self._detect_language(path.suffix)
            return self.review_code(code, str(path), language, context)
        except Exception as e:
            return ReviewResult(
                success=False,
                score=0,
                summary=f"Erro ao ler arquivo: {str(e)}",
                error=str(e)
            )

    def review_story_code(
        self,
        story_data: Dict,
        tasks: List[Dict]
    ) -> ReviewResult:
        """
        Revisa todo o codigo de uma Story

        Args:
            story_data: Dados da story
            tasks: Lista de tasks com code_output

        Returns:
            ReviewResult agregado
        """
        all_issues = []
        all_suggestions = []
        all_positives = []
        total_score = 0
        reviewed_count = 0

        for task in tasks:
            code = task.get("code_output")
            if not code:
                continue

            # Detecta linguagem pelo conteudo
            language = self._detect_language_from_code(code)

            # Constroi contexto da story
            context = self._build_story_context(story_data)

            # Revisa codigo da task
            result = self.review_code(
                code=code,
                file_path=f"task_{task.get('task_id', 'unknown')}",
                language=language,
                context=context
            )

            if result.success:
                all_issues.extend(result.issues)
                all_suggestions.extend(result.suggestions)
                all_positives.extend(result.positives)
                total_score += result.score
                reviewed_count += 1

        if reviewed_count == 0:
            return ReviewResult(
                success=False,
                score=0,
                summary="Nenhum codigo encontrado para revisar",
                error="No code to review"
            )

        # Calcula score medio
        avg_score = total_score // reviewed_count

        # Remove duplicatas
        unique_suggestions = list(set(all_suggestions))
        unique_positives = list(set(all_positives))

        # Gera resumo
        summary = self._generate_summary(
            all_issues, avg_score, reviewed_count
        )

        return ReviewResult(
            success=True,
            score=avg_score,
            summary=summary,
            issues=all_issues,
            suggestions=unique_suggestions[:10],  # Limita a 10
            positives=unique_positives[:5],       # Limita a 5
            metrics={
                "tasks_reviewed": reviewed_count,
                "total_issues": len(all_issues),
                "critical_issues": len([i for i in all_issues if i.severity == IssueSeverity.CRITICAL]),
                "warning_issues": len([i for i in all_issues if i.severity == IssueSeverity.WARNING]),
                "info_issues": len([i for i in all_issues if i.severity == IssueSeverity.INFO])
            }
        )

    def _call_claude_review(
        self,
        code: str,
        file_path: str,
        language: str,
        context: Optional[str]
    ) -> str:
        """Chama Claude API para fazer review"""

        system_prompt = """Voce e um revisor de codigo senior especializado em detectar problemas e sugerir melhorias.

Analise o codigo fornecido e identifique:
1. BUGS: Erros logicos, condicoes de corrida, null pointer, etc.
2. SEGURANCA: SQL injection, XSS, autenticacao fraca, vazamento de dados, etc.
3. PERFORMANCE: Loops ineficientes, queries N+1, memory leaks, etc.
4. BOAS PRATICAS: Nomes ruins, funcoes muito longas, codigo duplicado, etc.
5. ESTILO: Formatacao, convencoes, documentacao ausente, etc.

Responda APENAS com JSON valido no formato:
{
    "score": 85,
    "summary": "Resumo em uma frase do review",
    "issues": [
        {
            "line_start": 10,
            "line_end": 15,
            "issue_type": "bug|security|performance|style|maintainability|best_practice|documentation",
            "severity": "critical|warning|info",
            "title": "Titulo curto do problema",
            "description": "Descricao detalhada do problema",
            "suggestion": "Como corrigir o problema",
            "code_snippet": "codigo problemático"
        }
    ],
    "suggestions": ["Sugestao geral 1", "Sugestao geral 2"],
    "positives": ["Ponto positivo 1", "Ponto positivo 2"]
}

IMPORTANTE:
- Score de 0-100 (100 = codigo perfeito)
- Seja especifico nas linhas (line_start/line_end)
- Forneca sugestoes actionaveis
- Seja construtivo nos comentarios"""

        user_message = f"""Revise o seguinte codigo {language}:

Arquivo: {file_path}

```{language}
{code}
```

{f"Contexto adicional: {context}" if context else ""}

Faca o code review completo e retorne JSON."""

        response = self.client.messages.create(
            model=self.DEFAULT_MODEL,
            max_tokens=self.MAX_TOKENS,
            system=system_prompt,
            messages=[{"role": "user", "content": user_message}]
        )

        # Extrai texto da resposta
        content = ""
        if response.content:
            for block in response.content:
                if hasattr(block, 'text'):
                    content += block.text

        return content

    def _parse_review_response(
        self,
        response: str,
        code: str,
        file_path: str
    ) -> ReviewResult:
        """Parseia resposta do Claude em ReviewResult"""

        try:
            # Remove markdown code blocks se presentes
            content = response.strip()
            if content.startswith("```"):
                lines = content.split("\n")
                content = "\n".join(lines[1:-1])

            data = json.loads(content)

            # Converte issues
            issues = []
            for issue_data in data.get("issues", []):
                try:
                    issue = ReviewIssue(
                        file_path=file_path,
                        line_start=issue_data.get("line_start", 0),
                        line_end=issue_data.get("line_end"),
                        issue_type=IssueType(issue_data.get("issue_type", "style")),
                        severity=IssueSeverity(issue_data.get("severity", "info")),
                        title=issue_data.get("title", ""),
                        description=issue_data.get("description", ""),
                        suggestion=issue_data.get("suggestion", ""),
                        code_snippet=issue_data.get("code_snippet")
                    )
                    issues.append(issue)
                except (ValueError, KeyError) as e:
                    print(f"[CodeReviewer] Erro ao parsear issue: {e}")
                    continue

            return ReviewResult(
                success=True,
                score=data.get("score", 50),
                summary=data.get("summary", "Review concluido"),
                issues=issues,
                suggestions=data.get("suggestions", []),
                positives=data.get("positives", [])
            )

        except json.JSONDecodeError as e:
            print(f"[CodeReviewer] Erro ao parsear JSON: {e}")
            # Tenta extrair informacoes basicas do texto
            return ReviewResult(
                success=True,
                score=50,
                summary=response[:200] if response else "Review inconclusivo",
                issues=[],
                suggestions=[response] if response else []
            )

    def _fallback_review(
        self,
        code: str,
        file_path: str,
        language: str
    ) -> ReviewResult:
        """
        Review basico quando Claude nao esta disponivel
        Usa heuristicas simples para detectar problemas comuns
        """
        issues = []
        suggestions = []
        positives = []
        score = 100

        lines = code.split('\n')

        # Analise basica por linha
        for i, line in enumerate(lines, 1):
            line_lower = line.lower()

            # Detecta TODO/FIXME
            if 'todo' in line_lower or 'fixme' in line_lower:
                issues.append(ReviewIssue(
                    file_path=file_path,
                    line_start=i,
                    line_end=i,
                    issue_type=IssueType.MAINTAINABILITY,
                    severity=IssueSeverity.INFO,
                    title="TODO/FIXME encontrado",
                    description="Ha um comentario TODO ou FIXME pendente",
                    suggestion="Resolva ou remova o TODO/FIXME",
                    code_snippet=line.strip()
                ))
                score -= 2

            # Detecta passwords hardcoded
            if 'password' in line_lower and '=' in line:
                if '"' in line or "'" in line:
                    issues.append(ReviewIssue(
                        file_path=file_path,
                        line_start=i,
                        line_end=i,
                        issue_type=IssueType.SECURITY,
                        severity=IssueSeverity.CRITICAL,
                        title="Possivel senha hardcoded",
                        description="Senhas nao devem ser hardcoded no codigo",
                        suggestion="Use variaveis de ambiente ou secrets manager",
                        code_snippet=line.strip()
                    ))
                    score -= 20

            # Detecta print statements em Python
            if language == "python" and line.strip().startswith('print('):
                issues.append(ReviewIssue(
                    file_path=file_path,
                    line_start=i,
                    line_end=i,
                    issue_type=IssueType.BEST_PRACTICE,
                    severity=IssueSeverity.INFO,
                    title="print() encontrado",
                    description="Use logging ao inves de print em producao",
                    suggestion="Substitua por logging.debug() ou similar",
                    code_snippet=line.strip()
                ))
                score -= 1

            # Detecta linhas muito longas
            if len(line) > 120:
                issues.append(ReviewIssue(
                    file_path=file_path,
                    line_start=i,
                    line_end=i,
                    issue_type=IssueType.STYLE,
                    severity=IssueSeverity.INFO,
                    title="Linha muito longa",
                    description=f"Linha com {len(line)} caracteres (max recomendado: 120)",
                    suggestion="Quebre a linha em multiplas linhas",
                    code_snippet=line[:50] + "..."
                ))
                score -= 1

            # Detecta SQL injection potencial
            if 'execute' in line_lower or 'query' in line_lower:
                if '%s' in line or 'format' in line_lower or 'f"' in line or "f'" in line:
                    if 'select' in line_lower or 'insert' in line_lower or 'update' in line_lower or 'delete' in line_lower:
                        issues.append(ReviewIssue(
                            file_path=file_path,
                            line_start=i,
                            line_end=i,
                            issue_type=IssueType.SECURITY,
                            severity=IssueSeverity.CRITICAL,
                            title="Possivel SQL Injection",
                            description="Query SQL pode estar vulneravel a injection",
                            suggestion="Use parametros preparados ao inves de concatenacao",
                            code_snippet=line.strip()
                        ))
                        score -= 15

        # Analise de funcoes longas (Python)
        if language == "python":
            in_function = False
            function_start = 0
            function_lines = 0
            function_name = ""

            for i, line in enumerate(lines, 1):
                if line.strip().startswith('def ') or line.strip().startswith('async def '):
                    if in_function and function_lines > 50:
                        issues.append(ReviewIssue(
                            file_path=file_path,
                            line_start=function_start,
                            line_end=i-1,
                            issue_type=IssueType.MAINTAINABILITY,
                            severity=IssueSeverity.WARNING,
                            title=f"Funcao muito longa: {function_name}",
                            description=f"Funcao com {function_lines} linhas (max recomendado: 50)",
                            suggestion="Divida em funcoes menores com responsabilidades unicas"
                        ))
                        score -= 5

                    in_function = True
                    function_start = i
                    function_lines = 0
                    # Extrai nome da funcao
                    match = re.search(r'def\s+(\w+)', line)
                    function_name = match.group(1) if match else "unknown"
                elif in_function:
                    function_lines += 1

        # Verifica presenca de docstrings
        if language == "python":
            has_docstring = '"""' in code or "'''" in code
            if has_docstring:
                positives.append("Codigo possui docstrings")
            else:
                suggestions.append("Adicione docstrings para documentar funcoes e classes")
                score -= 5

        # Verifica imports nao utilizados (basico)
        if language == "python":
            imports = re.findall(r'^import (\w+)|^from \w+ import (\w+)', code, re.MULTILINE)
            for imp in imports:
                module = imp[0] or imp[1]
                # Conta uso simplificado
                usage_count = len(re.findall(rf'\b{module}\b', code))
                if usage_count <= 1:  # So aparece no import
                    suggestions.append(f"Verifique se o import '{module}' esta sendo usado")

        # Pontos positivos basicos
        if len(issues) == 0:
            positives.append("Codigo sem problemas obvios detectados")
        if len(code.split('\n')) < 200:
            positives.append("Arquivo de tamanho adequado")

        # Ajusta score minimo
        score = max(0, score)

        summary = f"Review basico: {len(issues)} issues encontradas"
        if not self.is_available():
            summary += " (Claude nao disponivel, usando analise heuristica)"

        return ReviewResult(
            success=True,
            score=score,
            summary=summary,
            issues=issues,
            suggestions=suggestions,
            positives=positives
        )

    def _detect_language(self, file_extension: str) -> str:
        """Detecta linguagem pelo extension do arquivo"""
        ext_map = {
            '.py': 'python',
            '.js': 'javascript',
            '.ts': 'typescript',
            '.jsx': 'javascript',
            '.tsx': 'typescript',
            '.java': 'java',
            '.go': 'go',
            '.rs': 'rust',
            '.rb': 'ruby',
            '.php': 'php',
            '.cs': 'csharp',
            '.cpp': 'cpp',
            '.c': 'c',
            '.swift': 'swift',
            '.kt': 'kotlin',
            '.sql': 'sql',
            '.html': 'html',
            '.css': 'css',
            '.vue': 'vue',
            '.svelte': 'svelte'
        }
        return ext_map.get(file_extension.lower(), 'text')

    def _detect_language_from_code(self, code: str) -> str:
        """Detecta linguagem analisando o codigo"""
        # Indicadores Python
        if 'def ' in code and ':' in code:
            if 'import ' in code or 'from ' in code:
                return 'python'

        # Indicadores JavaScript/TypeScript
        if 'function ' in code or 'const ' in code or 'let ' in code:
            if 'interface ' in code or ': string' in code or ': number' in code:
                return 'typescript'
            return 'javascript'

        # Indicadores Java
        if 'public class ' in code or 'private void ' in code:
            return 'java'

        # Indicadores Go
        if 'func ' in code and 'package ' in code:
            return 'go'

        # Indicadores Rust
        if 'fn ' in code and 'let mut ' in code:
            return 'rust'

        return 'python'  # Default

    def _build_story_context(self, story_data: Dict) -> str:
        """Constroi contexto da story para o review"""
        parts = []

        if story_data.get('title'):
            parts.append(f"Story: {story_data.get('title')}")

        if story_data.get('description'):
            parts.append(f"Descricao: {story_data.get('description')}")

        criteria = story_data.get('acceptance_criteria', [])
        if criteria:
            parts.append(f"Criterios de aceite: {', '.join(criteria[:3])}")

        tech_notes = story_data.get('technical_notes')
        if tech_notes:
            parts.append(f"Notas tecnicas: {tech_notes}")

        return ' | '.join(parts) if parts else ""

    def _generate_summary(
        self,
        issues: List[ReviewIssue],
        score: int,
        reviewed_count: int
    ) -> str:
        """Gera resumo do review"""
        critical = len([i for i in issues if i.severity == IssueSeverity.CRITICAL])
        warnings = len([i for i in issues if i.severity == IssueSeverity.WARNING])
        info = len([i for i in issues if i.severity == IssueSeverity.INFO])

        if score >= 90 and critical == 0:
            quality = "Excelente"
        elif score >= 70 and critical == 0:
            quality = "Bom"
        elif score >= 50:
            quality = "Aceitavel"
        else:
            quality = "Precisa melhorias"

        parts = [f"Score: {score}/100 - {quality}"]

        if critical > 0:
            parts.append(f"{critical} problema(s) critico(s)")
        if warnings > 0:
            parts.append(f"{warnings} aviso(s)")
        if info > 0:
            parts.append(f"{info} sugestao(oes)")

        parts.append(f"{reviewed_count} task(s) revisada(s)")

        return " | ".join(parts)


# Instancia global do revisor
_code_reviewer: Optional[CodeReviewer] = None


def get_code_reviewer() -> CodeReviewer:
    """Retorna instancia global do CodeReviewer"""
    global _code_reviewer
    if _code_reviewer is None:
        _code_reviewer = CodeReviewer()
    return _code_reviewer


def review_code(
    code: str,
    file_path: str = "unknown",
    language: str = "python",
    context: Optional[str] = None
) -> Dict:
    """
    Funcao helper para revisar codigo

    Args:
        code: Codigo a revisar
        file_path: Caminho do arquivo
        language: Linguagem
        context: Contexto adicional

    Returns:
        Dict com resultado do review
    """
    reviewer = get_code_reviewer()
    result = reviewer.review_code(code, file_path, language, context)
    return result.to_dict()

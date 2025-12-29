# -*- coding: utf-8 -*-
"""
Refactoring Engine - Motor de Refatoracao Automatica
=====================================================

Sistema proativo de identificacao e correcao de technical debt.
Implementa deteccao de code smells, calcular debt score e aplicar
refatoracoes automaticamente.

Features:
- Debt Scanner: Analisa codigo para identificar problemas
- Debt Score: Pontuacao de 0-100 para qualidade do codigo
- Refatoracao Automatica: Aplica melhorias seguras
- Geracao de Stories de Debt: Cria stories para resolver debitos

Issue #60: Refatoracao Automatica e Debt Reduction
"""

import os
import re
import ast
import hashlib
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any, Set
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime
from collections import defaultdict


# =============================================================================
# ENUMS E CONSTANTES
# =============================================================================

class SmellType(str, Enum):
    """Tipos de code smells detectaveis"""
    DUPLICATE_CODE = "duplicate_code"
    LONG_METHOD = "long_method"
    LARGE_CLASS = "large_class"
    DEAD_CODE = "dead_code"
    COMPLEX_CONDITIONAL = "complex_conditional"
    LONG_PARAMETER_LIST = "long_parameter_list"
    GOD_CLASS = "god_class"
    MAGIC_NUMBERS = "magic_numbers"
    DEEP_NESTING = "deep_nesting"
    POOR_NAMING = "poor_naming"


class SmellSeverity(str, Enum):
    """Severidade do code smell"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class RefactoringType(str, Enum):
    """Tipos de refatoracao disponiveis"""
    EXTRACT_METHOD = "extract_method"
    RENAME_VARIABLE = "rename_variable"
    REMOVE_DEAD_CODE = "remove_dead_code"
    SIMPLIFY_CONDITIONAL = "simplify_conditional"
    EXTRACT_CONSTANT = "extract_constant"
    SPLIT_CLASS = "split_class"
    REDUCE_NESTING = "reduce_nesting"
    INLINE_VARIABLE = "inline_variable"


# Limites para deteccao
THRESHOLDS = {
    "max_method_lines": 30,
    "max_class_lines": 300,
    "max_class_methods": 15,
    "max_parameters": 5,
    "max_cyclomatic_complexity": 10,
    "max_nesting_depth": 4,
    "min_variable_name_length": 3,
    "duplicate_code_min_lines": 5,
}


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class CodeSmell:
    """Representa um code smell detectado"""
    smell_type: SmellType
    severity: SmellSeverity
    file_path: str
    line_start: int
    line_end: int
    message: str
    suggestion: str
    code_snippet: str = ""
    estimated_fix_time_minutes: int = 5
    can_auto_fix: bool = False

    def to_dict(self) -> Dict:
        return {
            "type": self.smell_type.value,
            "severity": self.severity.value,
            "file": self.file_path,
            "line_start": self.line_start,
            "line_end": self.line_end,
            "message": self.message,
            "suggestion": self.suggestion,
            "code_snippet": self.code_snippet[:200] if self.code_snippet else "",
            "fix_time_minutes": self.estimated_fix_time_minutes,
            "can_auto_fix": self.can_auto_fix
        }


@dataclass
class RefactoringAction:
    """Representa uma acao de refatoracao"""
    refactoring_type: RefactoringType
    file_path: str
    line_start: int
    line_end: int
    original_code: str
    refactored_code: str
    description: str
    impact: str
    is_safe: bool = True

    def to_dict(self) -> Dict:
        return {
            "type": self.refactoring_type.value,
            "file": self.file_path,
            "line_start": self.line_start,
            "line_end": self.line_end,
            "original_code": self.original_code[:500],
            "refactored_code": self.refactored_code[:500],
            "description": self.description,
            "impact": self.impact,
            "is_safe": self.is_safe
        }


@dataclass
class DebtScore:
    """Pontuacao de Technical Debt"""
    overall_score: int  # 0-100, maior = melhor
    duplication_score: int
    complexity_score: int
    maintainability_score: int
    readability_score: int
    total_issues: int
    critical_issues: int
    estimated_fix_hours: float
    trend: str = "stable"  # improving, stable, declining

    def to_dict(self) -> Dict:
        return {
            "overall": self.overall_score,
            "duplication": self.duplication_score,
            "complexity": self.complexity_score,
            "maintainability": self.maintainability_score,
            "readability": self.readability_score,
            "total_issues": self.total_issues,
            "critical_issues": self.critical_issues,
            "fix_hours": round(self.estimated_fix_hours, 1),
            "trend": self.trend
        }


@dataclass
class DebtItem:
    """Item de debt para rastreamento"""
    id: str
    smell: CodeSmell
    status: str = "open"  # open, in_progress, resolved
    created_at: datetime = field(default_factory=datetime.now)
    resolved_at: Optional[datetime] = None
    story_id: Optional[str] = None


# =============================================================================
# AST VISITORS
# =============================================================================

class ComplexityVisitor(ast.NodeVisitor):
    """Calcula complexidade ciclomatica de um AST"""

    def __init__(self):
        self.complexity = 1  # Base complexity

    def visit_If(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_For(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_While(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_ExceptHandler(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_With(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_Assert(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_comprehension(self, node):
        self.complexity += 1
        self.generic_visit(node)

    def visit_BoolOp(self, node):
        self.complexity += len(node.values) - 1
        self.generic_visit(node)

    def visit_IfExp(self, node):
        self.complexity += 1
        self.generic_visit(node)


class FunctionVisitor(ast.NodeVisitor):
    """Extrai informacoes sobre funcoes/metodos"""

    def __init__(self, source_lines: List[str]):
        self.functions = []
        self.source_lines = source_lines

    def visit_FunctionDef(self, node):
        func_info = {
            "name": node.name,
            "line_start": node.lineno,
            "line_end": node.end_lineno or node.lineno,
            "num_params": len(node.args.args),
            "decorators": [self._get_decorator_name(d) for d in node.decorator_list],
            "docstring": ast.get_docstring(node) or "",
            "is_method": False,
            "complexity": self._calculate_complexity(node),
            "nesting_depth": self._calculate_nesting_depth(node)
        }
        self.functions.append(func_info)
        self.generic_visit(node)

    def visit_AsyncFunctionDef(self, node):
        self.visit_FunctionDef(node)

    def _get_decorator_name(self, decorator):
        if isinstance(decorator, ast.Name):
            return decorator.id
        elif isinstance(decorator, ast.Attribute):
            return decorator.attr
        elif isinstance(decorator, ast.Call):
            if isinstance(decorator.func, ast.Name):
                return decorator.func.id
            elif isinstance(decorator.func, ast.Attribute):
                return decorator.func.attr
        return "unknown"

    def _calculate_complexity(self, node) -> int:
        visitor = ComplexityVisitor()
        visitor.visit(node)
        return visitor.complexity

    def _calculate_nesting_depth(self, node, depth=0) -> int:
        max_depth = depth
        for child in ast.walk(node):
            if isinstance(child, (ast.If, ast.For, ast.While, ast.With, ast.Try)):
                child_depth = self._calculate_nesting_depth(child, depth + 1)
                max_depth = max(max_depth, child_depth)
        return max_depth


class ClassVisitor(ast.NodeVisitor):
    """Extrai informacoes sobre classes"""

    def __init__(self, source_lines: List[str]):
        self.classes = []
        self.source_lines = source_lines

    def visit_ClassDef(self, node):
        methods = []
        attributes = []

        for item in node.body:
            if isinstance(item, (ast.FunctionDef, ast.AsyncFunctionDef)):
                methods.append(item.name)
            elif isinstance(item, ast.Assign):
                for target in item.targets:
                    if isinstance(target, ast.Name):
                        attributes.append(target.id)

        class_info = {
            "name": node.name,
            "line_start": node.lineno,
            "line_end": node.end_lineno or node.lineno,
            "num_methods": len(methods),
            "num_attributes": len(attributes),
            "methods": methods,
            "attributes": attributes,
            "bases": [self._get_base_name(b) for b in node.bases],
            "docstring": ast.get_docstring(node) or ""
        }
        self.classes.append(class_info)
        self.generic_visit(node)

    def _get_base_name(self, base):
        if isinstance(base, ast.Name):
            return base.id
        elif isinstance(base, ast.Attribute):
            return base.attr
        return "unknown"


# =============================================================================
# REFACTORING ENGINE
# =============================================================================

class RefactoringEngine:
    """
    Motor principal de refatoracao.

    Analisa codigo, detecta code smells, calcula debt score
    e aplica refatoracoes automaticamente.
    """

    PROJECTS_DIR = Path(r"C:\Users\lcruz\Fabrica de Agentes\projects")

    def __init__(self, project_id: str):
        self.project_id = project_id
        self.project_path = self._find_project_path()
        self.smells: List[CodeSmell] = []
        self.refactorings: List[RefactoringAction] = []
        self.debt_items: List[DebtItem] = []
        self._code_hashes: Dict[str, Set[str]] = defaultdict(set)

    def _find_project_path(self) -> Optional[Path]:
        """Encontra o caminho do projeto"""
        possible_names = [
            self.project_id,
            self.project_id.lower(),
            self.project_id.lower().replace("-", "_"),
            self.project_id.replace("-", "_"),
        ]

        for name in possible_names:
            path = self.PROJECTS_DIR / name
            if path.exists():
                return path

        return None

    # =========================================================================
    # ANALISE DE CODIGO
    # =========================================================================

    def analyze(self) -> Dict[str, Any]:
        """
        Analisa o projeto completo.

        Returns:
            Resultado da analise com smells, score e sugestoes
        """
        if not self.project_path or not self.project_path.exists():
            return {
                "status": "error",
                "message": "Projeto nao encontrado",
                "project_id": self.project_id
            }

        self.smells = []
        self._code_hashes = defaultdict(set)

        # Analisar todos os arquivos Python
        py_files = list(self.project_path.rglob("*.py"))

        for py_file in py_files:
            try:
                self._analyze_file(py_file)
            except Exception as e:
                print(f"[RefactoringEngine] Erro ao analisar {py_file}: {e}")

        # Detectar duplicatas entre arquivos
        self._detect_cross_file_duplicates()

        # Calcular score
        debt_score = self._calculate_debt_score()

        # Gerar sugestoes de refatoracao
        self._generate_refactoring_suggestions()

        return {
            "status": "success",
            "project_id": self.project_id,
            "project_path": str(self.project_path),
            "files_analyzed": len(py_files),
            "smells": [s.to_dict() for s in self.smells],
            "smells_by_type": self._group_smells_by_type(),
            "smells_by_severity": self._group_smells_by_severity(),
            "debt_score": debt_score.to_dict(),
            "refactorings": [r.to_dict() for r in self.refactorings],
            "summary": self._generate_summary(debt_score)
        }

    def _analyze_file(self, file_path: Path):
        """Analisa um arquivo Python"""
        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")
            lines = content.split("\n")

            # Parse AST
            try:
                tree = ast.parse(content)
            except SyntaxError:
                # Arquivo com erro de sintaxe - apenas detectar smells textuais
                self._detect_textual_smells(file_path, lines)
                return

            relative_path = str(file_path.relative_to(self.project_path))

            # Detectar smells usando AST
            self._detect_long_methods(tree, lines, relative_path)
            self._detect_large_classes(tree, lines, relative_path)
            self._detect_complex_conditionals(tree, lines, relative_path)
            self._detect_long_parameter_lists(tree, lines, relative_path)
            self._detect_deep_nesting(tree, lines, relative_path)
            self._detect_poor_naming(tree, lines, relative_path)

            # Detectar smells textuais
            self._detect_textual_smells(file_path, lines)
            self._detect_magic_numbers(lines, relative_path)
            self._collect_code_hashes(lines, relative_path)

        except Exception as e:
            print(f"[RefactoringEngine] Erro processando {file_path}: {e}")

    def _detect_long_methods(self, tree: ast.AST, lines: List[str], file_path: str):
        """Detecta metodos longos"""
        visitor = FunctionVisitor(lines)
        visitor.visit(tree)

        for func in visitor.functions:
            num_lines = func["line_end"] - func["line_start"] + 1

            if num_lines > THRESHOLDS["max_method_lines"]:
                severity = SmellSeverity.MEDIUM
                if num_lines > THRESHOLDS["max_method_lines"] * 2:
                    severity = SmellSeverity.HIGH
                if num_lines > THRESHOLDS["max_method_lines"] * 3:
                    severity = SmellSeverity.CRITICAL

                code_snippet = "\n".join(
                    lines[func["line_start"]-1:min(func["line_end"], func["line_start"]+10)]
                )

                self.smells.append(CodeSmell(
                    smell_type=SmellType.LONG_METHOD,
                    severity=severity,
                    file_path=file_path,
                    line_start=func["line_start"],
                    line_end=func["line_end"],
                    message=f"Metodo '{func['name']}' tem {num_lines} linhas (maximo recomendado: {THRESHOLDS['max_method_lines']})",
                    suggestion=f"Considere extrair partes do metodo em metodos auxiliares menores",
                    code_snippet=code_snippet,
                    estimated_fix_time_minutes=15,
                    can_auto_fix=False
                ))

    def _detect_large_classes(self, tree: ast.AST, lines: List[str], file_path: str):
        """Detecta classes grandes"""
        visitor = ClassVisitor(lines)
        visitor.visit(tree)

        for cls in visitor.classes:
            num_lines = cls["line_end"] - cls["line_start"] + 1
            num_methods = cls["num_methods"]

            issues = []
            severity = SmellSeverity.LOW

            if num_lines > THRESHOLDS["max_class_lines"]:
                issues.append(f"{num_lines} linhas")
                severity = SmellSeverity.MEDIUM
            if num_methods > THRESHOLDS["max_class_methods"]:
                issues.append(f"{num_methods} metodos")
                severity = SmellSeverity.HIGH
            if num_lines > THRESHOLDS["max_class_lines"] * 2 and num_methods > THRESHOLDS["max_class_methods"]:
                severity = SmellSeverity.CRITICAL

            if issues:
                smell_type = SmellType.GOD_CLASS if severity == SmellSeverity.CRITICAL else SmellType.LARGE_CLASS

                self.smells.append(CodeSmell(
                    smell_type=smell_type,
                    severity=severity,
                    file_path=file_path,
                    line_start=cls["line_start"],
                    line_end=cls["line_end"],
                    message=f"Classe '{cls['name']}' e muito grande: {', '.join(issues)}",
                    suggestion="Considere dividir a classe em classes menores com responsabilidades mais especificas (SRP)",
                    estimated_fix_time_minutes=60,
                    can_auto_fix=False
                ))

    def _detect_complex_conditionals(self, tree: ast.AST, lines: List[str], file_path: str):
        """Detecta condicionais complexos"""
        visitor = FunctionVisitor(lines)
        visitor.visit(tree)

        for func in visitor.functions:
            if func["complexity"] > THRESHOLDS["max_cyclomatic_complexity"]:
                severity = SmellSeverity.MEDIUM
                if func["complexity"] > THRESHOLDS["max_cyclomatic_complexity"] * 2:
                    severity = SmellSeverity.HIGH

                self.smells.append(CodeSmell(
                    smell_type=SmellType.COMPLEX_CONDITIONAL,
                    severity=severity,
                    file_path=file_path,
                    line_start=func["line_start"],
                    line_end=func["line_end"],
                    message=f"Metodo '{func['name']}' tem complexidade ciclomatica {func['complexity']} (maximo: {THRESHOLDS['max_cyclomatic_complexity']})",
                    suggestion="Simplifique a logica condicional ou extraia condicoes em metodos/variaveis auxiliares",
                    estimated_fix_time_minutes=20,
                    can_auto_fix=False
                ))

    def _detect_long_parameter_lists(self, tree: ast.AST, lines: List[str], file_path: str):
        """Detecta listas de parametros longas"""
        visitor = FunctionVisitor(lines)
        visitor.visit(tree)

        for func in visitor.functions:
            if func["num_params"] > THRESHOLDS["max_parameters"]:
                self.smells.append(CodeSmell(
                    smell_type=SmellType.LONG_PARAMETER_LIST,
                    severity=SmellSeverity.MEDIUM,
                    file_path=file_path,
                    line_start=func["line_start"],
                    line_end=func["line_start"],
                    message=f"Metodo '{func['name']}' tem {func['num_params']} parametros (maximo: {THRESHOLDS['max_parameters']})",
                    suggestion="Considere usar um objeto de parametros (dataclass/dict) ou dividir a funcao",
                    estimated_fix_time_minutes=15,
                    can_auto_fix=False
                ))

    def _detect_deep_nesting(self, tree: ast.AST, lines: List[str], file_path: str):
        """Detecta aninhamento profundo"""
        visitor = FunctionVisitor(lines)
        visitor.visit(tree)

        for func in visitor.functions:
            if func["nesting_depth"] > THRESHOLDS["max_nesting_depth"]:
                self.smells.append(CodeSmell(
                    smell_type=SmellType.DEEP_NESTING,
                    severity=SmellSeverity.MEDIUM,
                    file_path=file_path,
                    line_start=func["line_start"],
                    line_end=func["line_end"],
                    message=f"Metodo '{func['name']}' tem aninhamento de {func['nesting_depth']} niveis (maximo: {THRESHOLDS['max_nesting_depth']})",
                    suggestion="Use early returns, extraia metodos ou simplifique a logica",
                    estimated_fix_time_minutes=15,
                    can_auto_fix=False
                ))

    def _detect_poor_naming(self, tree: ast.AST, lines: List[str], file_path: str):
        """Detecta nomes de variaveis ruins"""
        poor_names = []

        for node in ast.walk(tree):
            if isinstance(node, ast.Name) and isinstance(node.ctx, ast.Store):
                name = node.id
                # Ignorar variaveis especiais e underscore
                if name.startswith("_") or name in ("i", "j", "k", "x", "y", "n"):
                    continue
                if len(name) < THRESHOLDS["min_variable_name_length"]:
                    poor_names.append((name, node.lineno))

        if poor_names and len(poor_names) > 3:
            self.smells.append(CodeSmell(
                smell_type=SmellType.POOR_NAMING,
                severity=SmellSeverity.LOW,
                file_path=file_path,
                line_start=poor_names[0][1],
                line_end=poor_names[-1][1],
                message=f"Variaveis com nomes curtos encontradas: {', '.join(p[0] for p in poor_names[:5])}...",
                suggestion="Use nomes descritivos que expliquem o proposito da variavel",
                estimated_fix_time_minutes=10,
                can_auto_fix=True
            ))

    def _detect_magic_numbers(self, lines: List[str], file_path: str):
        """Detecta numeros magicos no codigo"""
        magic_pattern = r'(?<![a-zA-Z_])([2-9]\d{2,}|[1-9]\d{3,})(?![a-zA-Z_\d])'
        magic_numbers = []

        for i, line in enumerate(lines, 1):
            # Ignorar comentarios e strings
            if line.strip().startswith("#"):
                continue
            # Encontrar numeros magicos
            matches = re.findall(magic_pattern, line)
            for match in matches:
                if int(match) not in (100, 200, 404, 500):  # Ignorar HTTP status codes comuns
                    magic_numbers.append((match, i))

        if magic_numbers and len(magic_numbers) > 2:
            self.smells.append(CodeSmell(
                smell_type=SmellType.MAGIC_NUMBERS,
                severity=SmellSeverity.LOW,
                file_path=file_path,
                line_start=magic_numbers[0][1],
                line_end=magic_numbers[-1][1],
                message=f"Numeros magicos encontrados: {', '.join(m[0] for m in magic_numbers[:5])}",
                suggestion="Extraia numeros para constantes com nomes significativos",
                estimated_fix_time_minutes=10,
                can_auto_fix=True
            ))

    def _detect_textual_smells(self, file_path: Path, lines: List[str]):
        """Detecta smells baseados em texto"""
        relative_path = str(file_path.relative_to(self.project_path))

        # Detectar TODO/FIXME/HACK
        debt_markers = []
        for i, line in enumerate(lines, 1):
            if re.search(r'#\s*(TODO|FIXME|HACK|XXX|BUG):', line, re.IGNORECASE):
                debt_markers.append(i)

        if debt_markers and len(debt_markers) > 3:
            self.smells.append(CodeSmell(
                smell_type=SmellType.DEAD_CODE,
                severity=SmellSeverity.LOW,
                file_path=relative_path,
                line_start=debt_markers[0],
                line_end=debt_markers[-1],
                message=f"{len(debt_markers)} marcadores de debt (TODO/FIXME/HACK) encontrados",
                suggestion="Resolva ou crie stories para cada marcador de debt",
                estimated_fix_time_minutes=5 * len(debt_markers)
            ))

        # Detectar codigo comentado
        commented_code_lines = []
        for i, line in enumerate(lines, 1):
            stripped = line.strip()
            if stripped.startswith("#") and not stripped.startswith("##"):
                # Verificar se parece codigo (tem = ou : ou def ou class)
                rest = stripped[1:].strip()
                if re.search(r'\b(def|class|if|for|while|import|from|return)\b', rest):
                    commented_code_lines.append(i)

        if len(commented_code_lines) > 5:
            self.smells.append(CodeSmell(
                smell_type=SmellType.DEAD_CODE,
                severity=SmellSeverity.LOW,
                file_path=relative_path,
                line_start=commented_code_lines[0],
                line_end=commented_code_lines[-1],
                message=f"{len(commented_code_lines)} linhas de codigo comentado encontradas",
                suggestion="Remova codigo comentado - use controle de versao para historico",
                estimated_fix_time_minutes=5,
                can_auto_fix=True
            ))

    def _collect_code_hashes(self, lines: List[str], file_path: str):
        """Coleta hashes de blocos de codigo para detectar duplicatas"""
        min_lines = THRESHOLDS["duplicate_code_min_lines"]

        # Criar blocos de N linhas e calcular hash
        for i in range(len(lines) - min_lines + 1):
            block = "\n".join(lines[i:i+min_lines])
            # Normalizar (remover espacos extras, converter para lowercase)
            normalized = re.sub(r'\s+', ' ', block.lower()).strip()
            if len(normalized) > 50:  # Ignorar blocos muito pequenos
                block_hash = hashlib.md5(normalized.encode()).hexdigest()
                self._code_hashes[block_hash].add(f"{file_path}:{i+1}")

    def _detect_cross_file_duplicates(self):
        """Detecta codigo duplicado entre arquivos"""
        for block_hash, locations in self._code_hashes.items():
            if len(locations) > 1:
                loc_list = list(locations)
                first_loc = loc_list[0]
                file_path, line = first_loc.rsplit(":", 1)

                self.smells.append(CodeSmell(
                    smell_type=SmellType.DUPLICATE_CODE,
                    severity=SmellSeverity.MEDIUM if len(locations) <= 3 else SmellSeverity.HIGH,
                    file_path=file_path,
                    line_start=int(line),
                    line_end=int(line) + THRESHOLDS["duplicate_code_min_lines"],
                    message=f"Bloco de codigo duplicado em {len(locations)} locais: {', '.join(loc_list[:3])}{'...' if len(loc_list) > 3 else ''}",
                    suggestion="Extraia o codigo duplicado para uma funcao/metodo reutilizavel",
                    estimated_fix_time_minutes=20,
                    can_auto_fix=False
                ))

    # =========================================================================
    # DEBT SCORE
    # =========================================================================

    def _calculate_debt_score(self) -> DebtScore:
        """Calcula o score de technical debt"""
        total_issues = len(self.smells)
        critical_issues = sum(1 for s in self.smells if s.severity == SmellSeverity.CRITICAL)
        high_issues = sum(1 for s in self.smells if s.severity == SmellSeverity.HIGH)
        medium_issues = sum(1 for s in self.smells if s.severity == SmellSeverity.MEDIUM)

        # Calcular scores individuais (0-100, maior = melhor)
        duplication_smells = sum(1 for s in self.smells if s.smell_type == SmellType.DUPLICATE_CODE)
        complexity_smells = sum(1 for s in self.smells if s.smell_type in [SmellType.COMPLEX_CONDITIONAL, SmellType.DEEP_NESTING])
        size_smells = sum(1 for s in self.smells if s.smell_type in [SmellType.LONG_METHOD, SmellType.LARGE_CLASS, SmellType.GOD_CLASS])
        naming_smells = sum(1 for s in self.smells if s.smell_type in [SmellType.POOR_NAMING, SmellType.MAGIC_NUMBERS])

        duplication_score = max(0, 100 - duplication_smells * 15)
        complexity_score = max(0, 100 - complexity_smells * 10)
        maintainability_score = max(0, 100 - size_smells * 10)
        readability_score = max(0, 100 - naming_smells * 5)

        # Score geral ponderado
        overall = (
            duplication_score * 0.25 +
            complexity_score * 0.30 +
            maintainability_score * 0.25 +
            readability_score * 0.20
        )

        # Penalizar por issues criticos/altos
        overall -= critical_issues * 10
        overall -= high_issues * 5
        overall = max(0, min(100, overall))

        # Estimar tempo de correcao
        fix_hours = sum(s.estimated_fix_time_minutes for s in self.smells) / 60

        return DebtScore(
            overall_score=int(overall),
            duplication_score=duplication_score,
            complexity_score=complexity_score,
            maintainability_score=maintainability_score,
            readability_score=readability_score,
            total_issues=total_issues,
            critical_issues=critical_issues,
            estimated_fix_hours=fix_hours
        )

    # =========================================================================
    # REFATORACOES
    # =========================================================================

    def _generate_refactoring_suggestions(self):
        """Gera sugestoes de refatoracao baseado nos smells"""
        for smell in self.smells:
            if smell.can_auto_fix:
                self._suggest_refactoring(smell)

    def _suggest_refactoring(self, smell: CodeSmell):
        """Sugere refatoracao para um smell especifico"""
        if smell.smell_type == SmellType.MAGIC_NUMBERS:
            self.refactorings.append(RefactoringAction(
                refactoring_type=RefactoringType.EXTRACT_CONSTANT,
                file_path=smell.file_path,
                line_start=smell.line_start,
                line_end=smell.line_end,
                original_code=smell.code_snippet,
                refactored_code="# Constantes extraidas automaticamente",
                description="Extrair numeros magicos para constantes nomeadas",
                impact="Melhora legibilidade e manutencao",
                is_safe=True
            ))
        elif smell.smell_type == SmellType.DEAD_CODE and "comentado" in smell.message.lower():
            self.refactorings.append(RefactoringAction(
                refactoring_type=RefactoringType.REMOVE_DEAD_CODE,
                file_path=smell.file_path,
                line_start=smell.line_start,
                line_end=smell.line_end,
                original_code=smell.code_snippet,
                refactored_code="# Codigo comentado removido",
                description="Remover codigo comentado",
                impact="Reduz ruido e melhora legibilidade",
                is_safe=True
            ))

    def apply_refactoring(self, refactoring_id: int) -> Dict[str, Any]:
        """
        Aplica uma refatoracao especifica.

        Args:
            refactoring_id: Indice da refatoracao na lista

        Returns:
            Resultado da aplicacao
        """
        if refactoring_id >= len(self.refactorings):
            return {"success": False, "message": "Refatoracao nao encontrada"}

        refactoring = self.refactorings[refactoring_id]

        if not refactoring.is_safe:
            return {
                "success": False,
                "message": "Esta refatoracao requer revisao manual",
                "refactoring": refactoring.to_dict()
            }

        try:
            file_path = self.project_path / refactoring.file_path
            content = file_path.read_text(encoding="utf-8")
            lines = content.split("\n")

            # Aplicar refatoracao simples (remove dead code)
            if refactoring.refactoring_type == RefactoringType.REMOVE_DEAD_CODE:
                # Marcar linhas como removidas
                new_lines = []
                for i, line in enumerate(lines, 1):
                    if refactoring.line_start <= i <= refactoring.line_end:
                        if line.strip().startswith("#") and re.search(r'\b(def|class|if|for|while|import)\b', line):
                            continue  # Remove linha de codigo comentado
                    new_lines.append(line)

                file_path.write_text("\n".join(new_lines), encoding="utf-8")

                return {
                    "success": True,
                    "message": f"Refatoracao aplicada: {refactoring.description}",
                    "file": refactoring.file_path
                }

            return {
                "success": False,
                "message": "Tipo de refatoracao ainda nao implementado para aplicacao automatica"
            }

        except Exception as e:
            return {
                "success": False,
                "message": f"Erro ao aplicar refatoracao: {str(e)}"
            }

    def apply_all_safe_refactorings(self) -> Dict[str, Any]:
        """Aplica todas as refatoracoes seguras"""
        applied = []
        failed = []

        for i, ref in enumerate(self.refactorings):
            if ref.is_safe:
                result = self.apply_refactoring(i)
                if result.get("success"):
                    applied.append(ref.to_dict())
                else:
                    failed.append({"refactoring": ref.to_dict(), "error": result.get("message")})

        return {
            "success": True,
            "applied_count": len(applied),
            "failed_count": len(failed),
            "applied": applied,
            "failed": failed
        }

    # =========================================================================
    # HELPERS
    # =========================================================================

    def _group_smells_by_type(self) -> Dict[str, int]:
        """Agrupa smells por tipo"""
        groups = defaultdict(int)
        for smell in self.smells:
            groups[smell.smell_type.value] += 1
        return dict(groups)

    def _group_smells_by_severity(self) -> Dict[str, int]:
        """Agrupa smells por severidade"""
        groups = defaultdict(int)
        for smell in self.smells:
            groups[smell.severity.value] += 1
        return dict(groups)

    def _generate_summary(self, score: DebtScore) -> Dict[str, Any]:
        """Gera resumo da analise"""
        # Definir status baseado no score
        if score.overall_score >= 80:
            status = "excellent"
            status_message = "Codigo em excelente estado! Poucos problemas encontrados."
            status_color = "green"
        elif score.overall_score >= 60:
            status = "good"
            status_message = "Codigo em bom estado. Algumas melhorias recomendadas."
            status_color = "blue"
        elif score.overall_score >= 40:
            status = "needs_improvement"
            status_message = "Codigo precisa de atencao. Varias melhorias recomendadas."
            status_color = "orange"
        else:
            status = "critical"
            status_message = "Codigo em estado critico. Refatoracao urgente necessaria."
            status_color = "red"

        return {
            "status": status,
            "status_message": status_message,
            "status_color": status_color,
            "top_issues": [s.to_dict() for s in sorted(
                self.smells,
                key=lambda x: (
                    {"critical": 0, "high": 1, "medium": 2, "low": 3}[x.severity.value],
                    -x.estimated_fix_time_minutes
                )
            )[:5]],
            "quick_wins": [s.to_dict() for s in sorted(
                [s for s in self.smells if s.can_auto_fix],
                key=lambda x: x.estimated_fix_time_minutes
            )[:3]],
            "recommendations": self._get_recommendations(score)
        }

    def _get_recommendations(self, score: DebtScore) -> List[str]:
        """Gera recomendacoes baseadas no score"""
        recommendations = []

        if score.duplication_score < 70:
            recommendations.append("Priorize remover codigo duplicado - extraia funcoes reutilizaveis")
        if score.complexity_score < 70:
            recommendations.append("Simplifique condicionais complexos usando early returns e metodos auxiliares")
        if score.maintainability_score < 70:
            recommendations.append("Divida classes e metodos grandes em componentes menores")
        if score.readability_score < 70:
            recommendations.append("Melhore nomes de variaveis e extraia constantes para numeros magicos")
        if score.critical_issues > 0:
            recommendations.append(f"Resolva os {score.critical_issues} problemas criticos com urgencia")

        if not recommendations:
            recommendations.append("Continue mantendo as boas praticas de codigo!")

        return recommendations


# =============================================================================
# FUNCOES UTILITARIAS
# =============================================================================

def analyze_project_debt(project_id: str) -> Dict[str, Any]:
    """
    Analisa technical debt de um projeto.

    Args:
        project_id: ID do projeto

    Returns:
        Resultado da analise com score e smells
    """
    engine = RefactoringEngine(project_id)
    return engine.analyze()


def get_debt_score(project_id: str) -> Dict[str, Any]:
    """
    Retorna apenas o score de debt de um projeto.

    Args:
        project_id: ID do projeto

    Returns:
        Score de debt
    """
    engine = RefactoringEngine(project_id)
    analysis = engine.analyze()

    return {
        "project_id": project_id,
        "debt_score": analysis.get("debt_score", {}),
        "summary": analysis.get("summary", {})
    }


def get_debt_items(project_id: str) -> Dict[str, Any]:
    """
    Retorna itens de debt de um projeto.

    Args:
        project_id: ID do projeto

    Returns:
        Lista de itens de debt
    """
    engine = RefactoringEngine(project_id)
    analysis = engine.analyze()

    return {
        "project_id": project_id,
        "items": analysis.get("smells", []),
        "count": len(analysis.get("smells", [])),
        "by_type": analysis.get("smells_by_type", {}),
        "by_severity": analysis.get("smells_by_severity", {})
    }


def auto_refactor(project_id: str, apply_all: bool = False) -> Dict[str, Any]:
    """
    Executa refatoracao automatica.

    Args:
        project_id: ID do projeto
        apply_all: Se True, aplica todas as refatoracoes seguras

    Returns:
        Resultado da refatoracao
    """
    engine = RefactoringEngine(project_id)
    analysis = engine.analyze()

    if apply_all:
        result = engine.apply_all_safe_refactorings()
        return {
            "project_id": project_id,
            "analysis": analysis.get("summary", {}),
            "refactoring_result": result
        }

    return {
        "project_id": project_id,
        "analysis": analysis.get("summary", {}),
        "available_refactorings": analysis.get("refactorings", []),
        "message": "Use apply_all=True para aplicar refatoracoes automaticas"
    }


def generate_debt_stories(project_id: str) -> Dict[str, Any]:
    """
    Gera stories para resolver technical debt.

    Args:
        project_id: ID do projeto

    Returns:
        Lista de stories sugeridas
    """
    engine = RefactoringEngine(project_id)
    analysis = engine.analyze()

    stories = []
    smells = analysis.get("smells", [])

    # Agrupar por tipo para criar stories
    smell_groups = defaultdict(list)
    for smell in smells:
        smell_groups[smell["type"]].append(smell)

    for smell_type, type_smells in smell_groups.items():
        if len(type_smells) >= 2:
            # Criar story para grupo
            total_time = sum(s.get("fix_time_minutes", 5) for s in type_smells)
            story_points = min(13, max(1, total_time // 30))

            stories.append({
                "title": f"Resolver {len(type_smells)} issues de {smell_type.replace('_', ' ').title()}",
                "persona": "desenvolvedor",
                "action": f"corrigir os {len(type_smells)} problemas de {smell_type.replace('_', ' ')}",
                "benefit": "melhorar a qualidade e manutenibilidade do codigo",
                "story_points": story_points,
                "acceptance_criteria": [
                    f"Todos os {len(type_smells)} smells de {smell_type.replace('_', ' ')} devem ser resolvidos",
                    "Score de debt deve melhorar pelo menos 5 pontos",
                    "Testes existentes devem continuar passando"
                ],
                "files_affected": list(set(s.get("file", "") for s in type_smells)),
                "estimated_hours": round(total_time / 60, 1)
            })

    return {
        "project_id": project_id,
        "debt_score": analysis.get("debt_score", {}),
        "suggested_stories": stories,
        "total_stories": len(stories)
    }


# =============================================================================
# CLI TEST
# =============================================================================

if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1:
        project_id = sys.argv[1]
    else:
        project_id = "test-project"

    print(f"Analisando projeto: {project_id}")
    print("=" * 60)

    result = analyze_project_debt(project_id)

    if result.get("status") == "error":
        print(f"Erro: {result.get('message')}")
    else:
        print(f"Arquivos analisados: {result.get('files_analyzed', 0)}")
        print(f"Smells encontrados: {len(result.get('smells', []))}")
        print()

        score = result.get("debt_score", {})
        print("Debt Score:")
        print(f"  Overall:        {score.get('overall', 0)}/100")
        print(f"  Duplication:    {score.get('duplication', 0)}/100")
        print(f"  Complexity:     {score.get('complexity', 0)}/100")
        print(f"  Maintainability:{score.get('maintainability', 0)}/100")
        print(f"  Readability:    {score.get('readability', 0)}/100")
        print(f"  Fix Hours:      {score.get('fix_hours', 0)}h")
        print()

        summary = result.get("summary", {})
        print(f"Status: {summary.get('status_message', 'N/A')}")
        print()

        print("Top Issues:")
        for issue in summary.get("top_issues", [])[:5]:
            print(f"  - [{issue.get('severity', 'N/A').upper()}] {issue.get('type', '')}: {issue.get('message', '')[:60]}...")

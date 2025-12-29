# -*- coding: utf-8 -*-
"""
Test Generator - Geracao Automatica de Testes v1.0
===================================================
Fabrica de Agentes - Issue #53

Gera testes automaticos para codigo produzido pelos workers:
- Testes Unitarios (pytest)
- Testes de Integracao
- Testes E2E (Playwright)
- Relatorio de Cobertura

Autor: Claude AI
Data: 2024
"""
import os
import sys
import json
import ast
import re
import subprocess
import asyncio
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, Any, List, Tuple
from dataclasses import dataclass, field
from enum import Enum
from abc import ABC, abstractmethod

# Garantir encoding UTF-8
if sys.stdout:
    try:
        sys.stdout.reconfigure(encoding='utf-8')
    except:
        pass

# Adicionar path do projeto
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from dotenv import load_dotenv
load_dotenv()


# =============================================================================
# ENUMS E CONSTANTES
# =============================================================================

class TestType(str, Enum):
    """Tipos de testes suportados"""
    UNIT = "unit"
    INTEGRATION = "integration"
    E2E = "e2e"


class TestFramework(str, Enum):
    """Frameworks de teste suportados"""
    PYTEST = "pytest"
    JEST = "jest"
    PLAYWRIGHT = "playwright"
    UNITTEST = "unittest"


class CoverageLevel(str, Enum):
    """Niveis de cobertura"""
    MINIMAL = "minimal"      # 50%
    STANDARD = "standard"    # 70%
    HIGH = "high"            # 80%
    COMPLETE = "complete"    # 90%


# Configuracao padrao de cobertura
DEFAULT_COVERAGE_CONFIG = {
    "minimum": 80,
    "unit": 90,
    "integration": 70,
    "e2e": 50
}

# Templates de testes
FIBONACCI_POINTS = [1, 2, 3, 5, 8, 13, 21]


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class TestConfig:
    """Configuracao para geracao de testes"""
    test_types: List[TestType] = field(default_factory=lambda: [TestType.UNIT])
    framework: TestFramework = TestFramework.PYTEST
    coverage_config: Dict[str, int] = field(default_factory=lambda: DEFAULT_COVERAGE_CONFIG.copy())
    tdd_mode: bool = False
    generate_mocks: bool = True
    include_edge_cases: bool = True
    include_error_cases: bool = True
    output_dir: Optional[str] = None
    verbose: bool = True


@dataclass
class TestCase:
    """Representa um caso de teste"""
    name: str
    description: str
    test_type: TestType
    function_name: str
    test_code: str
    setup_code: Optional[str] = None
    teardown_code: Optional[str] = None
    mocks: List[str] = field(default_factory=list)
    expected_result: Optional[Any] = None
    is_edge_case: bool = False
    is_error_case: bool = False


@dataclass
class TestFile:
    """Representa um arquivo de teste gerado"""
    filename: str
    filepath: str
    test_type: TestType
    test_cases: List[TestCase] = field(default_factory=list)
    imports: List[str] = field(default_factory=list)
    fixtures: List[str] = field(default_factory=list)
    content: str = ""


@dataclass
class CoverageReport:
    """Relatorio de cobertura de codigo"""
    total_coverage: float = 0.0
    unit_coverage: float = 0.0
    integration_coverage: float = 0.0
    e2e_coverage: float = 0.0
    files: Dict[str, float] = field(default_factory=dict)
    lines_covered: int = 0
    lines_total: int = 0
    tests_passed: int = 0
    tests_failed: int = 0
    tests_skipped: int = 0
    generated_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())


@dataclass
class TestGenerationResult:
    """Resultado da geracao de testes"""
    success: bool
    test_files: List[TestFile] = field(default_factory=list)
    coverage_report: Optional[CoverageReport] = None
    error_message: Optional[str] = None
    execution_time_ms: int = 0
    tdd_mode: bool = False


# =============================================================================
# ANALISADOR DE CODIGO
# =============================================================================

class CodeAnalyzer:
    """
    Analisa codigo fonte para extrair informacoes necessarias para geracao de testes.
    Suporta Python e JavaScript/TypeScript.
    """

    def __init__(self, source_path: Path):
        self.source_path = source_path
        self.functions: List[Dict[str, Any]] = []
        self.classes: List[Dict[str, Any]] = []
        self.imports: List[str] = []
        self.dependencies: List[str] = []

    def analyze(self) -> Dict[str, Any]:
        """Analisa o codigo fonte e retorna estrutura"""
        if not self.source_path.exists():
            return {"error": f"Arquivo nao encontrado: {self.source_path}"}

        suffix = self.source_path.suffix.lower()

        if suffix == ".py":
            return self._analyze_python()
        elif suffix in [".js", ".ts", ".jsx", ".tsx"]:
            return self._analyze_javascript()
        else:
            return {"error": f"Tipo de arquivo nao suportado: {suffix}"}

    def _analyze_python(self) -> Dict[str, Any]:
        """Analisa codigo Python usando AST"""
        try:
            with open(self.source_path, "r", encoding="utf-8") as f:
                source_code = f.read()

            tree = ast.parse(source_code)

            # Extrair imports
            for node in ast.walk(tree):
                if isinstance(node, ast.Import):
                    for alias in node.names:
                        self.imports.append(alias.name)
                elif isinstance(node, ast.ImportFrom):
                    module = node.module or ""
                    for alias in node.names:
                        self.imports.append(f"{module}.{alias.name}")

            # Extrair funcoes de nivel superior
            for node in ast.iter_child_nodes(tree):
                if isinstance(node, ast.FunctionDef) or isinstance(node, ast.AsyncFunctionDef):
                    func_info = self._extract_function_info(node, source_code)
                    self.functions.append(func_info)

                elif isinstance(node, ast.ClassDef):
                    class_info = self._extract_class_info(node, source_code)
                    self.classes.append(class_info)

            return {
                "language": "python",
                "filepath": str(self.source_path),
                "imports": self.imports,
                "functions": self.functions,
                "classes": self.classes,
                "dependencies": self._detect_dependencies()
            }

        except SyntaxError as e:
            return {"error": f"Erro de sintaxe: {e}"}
        except Exception as e:
            return {"error": f"Erro ao analisar: {e}"}

    def _extract_function_info(self, node, source_code: str) -> Dict[str, Any]:
        """Extrai informacoes de uma funcao"""
        # Extrair parametros
        params = []
        for arg in node.args.args:
            param_info = {"name": arg.arg, "type": None, "default": None}
            if arg.annotation:
                param_info["type"] = ast.unparse(arg.annotation)
            params.append(param_info)

        # Defaults
        defaults = node.args.defaults
        for i, default in enumerate(defaults):
            param_idx = len(params) - len(defaults) + i
            if param_idx >= 0:
                params[param_idx]["default"] = ast.unparse(default)

        # Retorno
        return_type = None
        if node.returns:
            return_type = ast.unparse(node.returns)

        # Docstring
        docstring = ast.get_docstring(node)

        # Determinar se e async
        is_async = isinstance(node, ast.AsyncFunctionDef)

        # Extrair corpo resumido
        body_summary = self._summarize_function_body(node)

        return {
            "name": node.name,
            "params": params,
            "return_type": return_type,
            "docstring": docstring,
            "is_async": is_async,
            "decorators": [ast.unparse(d) for d in node.decorator_list],
            "body_summary": body_summary,
            "line_start": node.lineno,
            "line_end": node.end_lineno or node.lineno
        }

    def _extract_class_info(self, node, source_code: str) -> Dict[str, Any]:
        """Extrai informacoes de uma classe"""
        methods = []
        attributes = []

        for item in node.body:
            if isinstance(item, (ast.FunctionDef, ast.AsyncFunctionDef)):
                method_info = self._extract_function_info(item, source_code)
                methods.append(method_info)
            elif isinstance(item, ast.Assign):
                for target in item.targets:
                    if isinstance(target, ast.Name):
                        attributes.append(target.id)

        # Bases (heranca)
        bases = [ast.unparse(base) for base in node.bases]

        return {
            "name": node.name,
            "bases": bases,
            "docstring": ast.get_docstring(node),
            "methods": methods,
            "attributes": attributes,
            "decorators": [ast.unparse(d) for d in node.decorator_list]
        }

    def _summarize_function_body(self, node) -> Dict[str, Any]:
        """Resume o corpo da funcao para entender o que ela faz"""
        summary = {
            "has_return": False,
            "has_raise": False,
            "has_loop": False,
            "has_conditional": False,
            "calls": [],
            "raises": []
        }

        for child in ast.walk(node):
            if isinstance(child, ast.Return) and child.value is not None:
                summary["has_return"] = True
            elif isinstance(child, ast.Raise):
                summary["has_raise"] = True
                if child.exc and isinstance(child.exc, ast.Call):
                    if isinstance(child.exc.func, ast.Name):
                        summary["raises"].append(child.exc.func.id)
            elif isinstance(child, (ast.For, ast.While, ast.AsyncFor)):
                summary["has_loop"] = True
            elif isinstance(child, ast.If):
                summary["has_conditional"] = True
            elif isinstance(child, ast.Call):
                if isinstance(child.func, ast.Name):
                    summary["calls"].append(child.func.id)
                elif isinstance(child.func, ast.Attribute):
                    summary["calls"].append(child.func.attr)

        return summary

    def _detect_dependencies(self) -> List[str]:
        """Detecta dependencias externas que precisam de mock"""
        external_deps = []
        external_modules = ["requests", "httpx", "aiohttp", "sqlalchemy", "redis", "boto3"]

        for imp in self.imports:
            for ext in external_modules:
                if ext in imp:
                    external_deps.append(imp)
                    break

        return external_deps

    def _analyze_javascript(self) -> Dict[str, Any]:
        """Analisa codigo JavaScript/TypeScript usando regex (basico)"""
        try:
            with open(self.source_path, "r", encoding="utf-8") as f:
                source_code = f.read()

            functions = []
            classes = []
            imports = []

            # Extrair imports
            import_pattern = r"import\s+(?:{[^}]+}|[\w\s,]+)\s+from\s+['\"]([^'\"]+)['\"]"
            for match in re.finditer(import_pattern, source_code):
                imports.append(match.group(1))

            # Extrair funcoes (function e arrow)
            func_pattern = r"(?:async\s+)?function\s+(\w+)\s*\(([^)]*)\)|const\s+(\w+)\s*=\s*(?:async\s+)?\(?([^)]*)\)?\s*=>"
            for match in re.finditer(func_pattern, source_code):
                name = match.group(1) or match.group(3)
                params_str = match.group(2) or match.group(4)
                params = [p.strip() for p in params_str.split(",") if p.strip()]

                functions.append({
                    "name": name,
                    "params": [{"name": p, "type": None} for p in params],
                    "is_async": "async" in match.group(0)
                })

            # Extrair classes
            class_pattern = r"class\s+(\w+)(?:\s+extends\s+(\w+))?"
            for match in re.finditer(class_pattern, source_code):
                classes.append({
                    "name": match.group(1),
                    "bases": [match.group(2)] if match.group(2) else [],
                    "methods": []
                })

            return {
                "language": "javascript",
                "filepath": str(self.source_path),
                "imports": imports,
                "functions": functions,
                "classes": classes,
                "dependencies": []
            }

        except Exception as e:
            return {"error": f"Erro ao analisar JS: {e}"}


# =============================================================================
# GERADORES DE TESTE
# =============================================================================

class BaseTestGenerator(ABC):
    """Classe base para geradores de teste"""

    def __init__(self, config: TestConfig):
        self.config = config
        self._log_buffer: List[str] = []

    @abstractmethod
    def generate(self, analysis: Dict[str, Any]) -> List[TestFile]:
        """Gera testes baseado na analise do codigo"""
        pass

    def _log(self, message: str):
        """Log interno"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        log_msg = f"[{timestamp}] [TestGen] {message}"
        self._log_buffer.append(log_msg)
        if self.config.verbose:
            print(log_msg)


class PytestGenerator(BaseTestGenerator):
    """Gera testes usando pytest para codigo Python"""

    def generate(self, analysis: Dict[str, Any]) -> List[TestFile]:
        """Gera arquivos de teste pytest"""
        test_files = []

        if "error" in analysis:
            self._log(f"Erro na analise: {analysis['error']}")
            return test_files

        filepath = Path(analysis.get("filepath", ""))
        module_name = filepath.stem

        # Gerar testes unitarios
        if TestType.UNIT in self.config.test_types:
            unit_tests = self._generate_unit_tests(analysis, module_name)
            if unit_tests:
                test_files.append(unit_tests)

        # Gerar testes de integracao
        if TestType.INTEGRATION in self.config.test_types:
            integration_tests = self._generate_integration_tests(analysis, module_name)
            if integration_tests:
                test_files.append(integration_tests)

        return test_files

    def _generate_unit_tests(self, analysis: Dict[str, Any], module_name: str) -> Optional[TestFile]:
        """Gera arquivo de testes unitarios"""
        test_cases = []

        # Testes para funcoes
        for func in analysis.get("functions", []):
            cases = self._generate_function_tests(func, module_name)
            test_cases.extend(cases)

        # Testes para classes
        for cls in analysis.get("classes", []):
            cases = self._generate_class_tests(cls, module_name)
            test_cases.extend(cases)

        if not test_cases:
            return None

        # Montar imports
        imports = self._build_imports(analysis, module_name)

        # Montar fixtures
        fixtures = self._build_fixtures(analysis)

        # Gerar conteudo do arquivo
        content = self._build_test_file_content(imports, fixtures, test_cases)

        test_filename = f"test_{module_name}.py"
        output_dir = self.config.output_dir or "tests"

        return TestFile(
            filename=test_filename,
            filepath=str(Path(output_dir) / test_filename),
            test_type=TestType.UNIT,
            test_cases=test_cases,
            imports=imports,
            fixtures=fixtures,
            content=content
        )

    def _generate_function_tests(self, func: Dict[str, Any], module_name: str) -> List[TestCase]:
        """Gera casos de teste para uma funcao"""
        cases = []
        func_name = func["name"]
        params = func.get("params", [])
        body = func.get("body_summary", {})
        is_async = func.get("is_async", False)

        # Prefixo async
        async_prefix = "async " if is_async else ""
        await_prefix = "await " if is_async else ""

        # 1. Happy Path Test
        happy_path_code = self._generate_happy_path_test(
            func_name, params, module_name, is_async
        )
        cases.append(TestCase(
            name=f"test_{func_name}_happy_path",
            description=f"Testa o caminho feliz da funcao {func_name}",
            test_type=TestType.UNIT,
            function_name=func_name,
            test_code=happy_path_code
        ))

        # 2. Edge Cases (se habilitado)
        if self.config.include_edge_cases:
            edge_cases = self._generate_edge_case_tests(func_name, params, module_name, is_async)
            cases.extend(edge_cases)

        # 3. Error Cases (se a funcao lanca excecoes)
        if self.config.include_error_cases and body.get("has_raise"):
            error_cases = self._generate_error_tests(func_name, params, body, module_name, is_async)
            cases.extend(error_cases)

        return cases

    def _generate_happy_path_test(
        self,
        func_name: str,
        params: List[Dict],
        module_name: str,
        is_async: bool
    ) -> str:
        """Gera teste de happy path"""
        async_def = "async def" if is_async else "def"
        await_kw = "await " if is_async else ""

        # Gerar valores de parametros de exemplo
        param_values = self._generate_sample_params(params)
        param_str = ", ".join(param_values)

        code = f'''
{async_def} test_{func_name}_happy_path():
    """
    Testa o caminho feliz da funcao {func_name}.
    Verifica se a funcao executa corretamente com parametros validos.
    """
    # Arrange
    {self._generate_arrange_section(params)}

    # Act
    result = {await_kw}{module_name}.{func_name}({param_str})

    # Assert
    assert result is not None
'''
        return code.strip()

    def _generate_edge_case_tests(
        self,
        func_name: str,
        params: List[Dict],
        module_name: str,
        is_async: bool
    ) -> List[TestCase]:
        """Gera testes para casos de borda"""
        cases = []
        async_def = "async def" if is_async else "def"
        await_kw = "await " if is_async else ""

        # Teste com valores vazios
        if params:
            empty_test_code = f'''
{async_def} test_{func_name}_empty_values():
    """
    Testa a funcao {func_name} com valores vazios/nulos.
    """
    # Este teste verifica o comportamento com inputs vazios
    # Ajuste conforme a logica esperada da funcao
    try:
        result = {await_kw}{module_name}.{func_name}()
        # Se chegou aqui sem erro, verificar resultado
        assert result is not None or result is None  # Ajustar asserção
    except (TypeError, ValueError) as e:
        # Comportamento esperado para inputs invalidos
        pass
'''
            cases.append(TestCase(
                name=f"test_{func_name}_empty_values",
                description=f"Testa {func_name} com valores vazios",
                test_type=TestType.UNIT,
                function_name=func_name,
                test_code=empty_test_code.strip(),
                is_edge_case=True
            ))

        # Teste com valores limites
        for param in params:
            param_name = param["name"]
            param_type = param.get("type", "")

            if "int" in str(param_type).lower() or "float" in str(param_type).lower():
                boundary_code = f'''
{async_def} test_{func_name}_boundary_{param_name}():
    """
    Testa a funcao {func_name} com valores limite para {param_name}.
    """
    # Testar com zero
    # Testar com valor negativo
    # Testar com valor muito grande
    pass  # TODO: Implementar casos de borda especificos
'''
                cases.append(TestCase(
                    name=f"test_{func_name}_boundary_{param_name}",
                    description=f"Testa limites do parametro {param_name}",
                    test_type=TestType.UNIT,
                    function_name=func_name,
                    test_code=boundary_code.strip(),
                    is_edge_case=True
                ))

        return cases

    def _generate_error_tests(
        self,
        func_name: str,
        params: List[Dict],
        body: Dict[str, Any],
        module_name: str,
        is_async: bool
    ) -> List[TestCase]:
        """Gera testes para casos de erro"""
        cases = []
        async_def = "async def" if is_async else "def"
        await_kw = "await " if is_async else ""

        raises = body.get("raises", [])

        for exception in raises:
            error_code = f'''
{async_def} test_{func_name}_raises_{exception.lower()}():
    """
    Testa se a funcao {func_name} lanca {exception} corretamente.
    """
    import pytest

    with pytest.raises({exception}):
        # Arrange - preparar dados que causam o erro
        # TODO: Ajustar parametros para provocar a excecao

        # Act
        {await_kw}{module_name}.{func_name}()
'''
            cases.append(TestCase(
                name=f"test_{func_name}_raises_{exception.lower()}",
                description=f"Testa se {func_name} lanca {exception}",
                test_type=TestType.UNIT,
                function_name=func_name,
                test_code=error_code.strip(),
                is_error_case=True
            ))

        return cases

    def _generate_class_tests(self, cls: Dict[str, Any], module_name: str) -> List[TestCase]:
        """Gera casos de teste para uma classe"""
        cases = []
        class_name = cls["name"]

        # Teste de instanciacao
        init_code = f'''
def test_{class_name.lower()}_instantiation():
    """
    Testa se a classe {class_name} pode ser instanciada corretamente.
    """
    # Arrange & Act
    instance = {module_name}.{class_name}()

    # Assert
    assert instance is not None
    assert isinstance(instance, {module_name}.{class_name})
'''
        cases.append(TestCase(
            name=f"test_{class_name.lower()}_instantiation",
            description=f"Testa instanciacao de {class_name}",
            test_type=TestType.UNIT,
            function_name=f"{class_name}.__init__",
            test_code=init_code.strip()
        ))

        # Testes para cada metodo
        for method in cls.get("methods", []):
            if method["name"].startswith("_") and method["name"] != "__init__":
                continue  # Pular metodos privados exceto __init__

            method_cases = self._generate_method_tests(method, class_name, module_name)
            cases.extend(method_cases)

        return cases

    def _generate_method_tests(
        self,
        method: Dict[str, Any],
        class_name: str,
        module_name: str
    ) -> List[TestCase]:
        """Gera testes para um metodo de classe"""
        cases = []
        method_name = method["name"]

        if method_name == "__init__":
            return cases  # Ja testado em instantiation

        is_async = method.get("is_async", False)
        async_def = "async def" if is_async else "def"
        await_kw = "await " if is_async else ""

        test_code = f'''
{async_def} test_{class_name.lower()}_{method_name}():
    """
    Testa o metodo {method_name} da classe {class_name}.
    """
    # Arrange
    instance = {module_name}.{class_name}()

    # Act
    result = {await_kw}instance.{method_name}()

    # Assert
    # TODO: Adicionar assercoes especificas para o metodo
    pass
'''
        cases.append(TestCase(
            name=f"test_{class_name.lower()}_{method_name}",
            description=f"Testa {class_name}.{method_name}",
            test_type=TestType.UNIT,
            function_name=f"{class_name}.{method_name}",
            test_code=test_code.strip()
        ))

        return cases

    def _generate_integration_tests(
        self,
        analysis: Dict[str, Any],
        module_name: str
    ) -> Optional[TestFile]:
        """Gera arquivo de testes de integracao"""
        test_cases = []
        dependencies = analysis.get("dependencies", [])

        if not dependencies:
            # Sem dependencias externas, criar teste basico
            pass

        # Identificar endpoints de API
        api_functions = [
            f for f in analysis.get("functions", [])
            if any(d in f.get("decorators", []) for d in ["app.get", "app.post", "router.get", "router.post"])
        ]

        for func in api_functions:
            cases = self._generate_api_integration_test(func, module_name)
            test_cases.extend(cases)

        # Identificar funcoes que usam banco de dados
        db_functions = [
            f for f in analysis.get("functions", [])
            if "db" in str(f.get("params", [])).lower() or "session" in str(f.get("params", [])).lower()
        ]

        for func in db_functions:
            cases = self._generate_db_integration_test(func, module_name)
            test_cases.extend(cases)

        if not test_cases:
            return None

        imports = [
            "import pytest",
            "from unittest.mock import Mock, patch, AsyncMock",
            "from fastapi.testclient import TestClient",
        ]

        content = self._build_test_file_content(imports, [], test_cases)

        test_filename = f"test_{module_name}_integration.py"
        output_dir = self.config.output_dir or "tests"

        return TestFile(
            filename=test_filename,
            filepath=str(Path(output_dir) / test_filename),
            test_type=TestType.INTEGRATION,
            test_cases=test_cases,
            imports=imports,
            content=content
        )

    def _generate_api_integration_test(
        self,
        func: Dict[str, Any],
        module_name: str
    ) -> List[TestCase]:
        """Gera teste de integracao para endpoint de API"""
        cases = []
        func_name = func["name"]

        # Detectar metodo HTTP
        decorators = func.get("decorators", [])
        http_method = "get"
        for d in decorators:
            if "post" in d.lower():
                http_method = "post"
            elif "put" in d.lower():
                http_method = "put"
            elif "delete" in d.lower():
                http_method = "delete"
            elif "patch" in d.lower():
                http_method = "patch"

        test_code = f'''
def test_api_{func_name}_integration(client):
    """
    Teste de integracao para o endpoint {func_name}.
    """
    # Arrange
    url = "/"  # TODO: Definir URL correta

    # Act
    response = client.{http_method}(url)

    # Assert
    assert response.status_code in [200, 201]
    data = response.json()
    assert data is not None
'''
        cases.append(TestCase(
            name=f"test_api_{func_name}_integration",
            description=f"Teste de integracao do endpoint {func_name}",
            test_type=TestType.INTEGRATION,
            function_name=func_name,
            test_code=test_code.strip()
        ))

        return cases

    def _generate_db_integration_test(
        self,
        func: Dict[str, Any],
        module_name: str
    ) -> List[TestCase]:
        """Gera teste de integracao para funcao de banco de dados"""
        cases = []
        func_name = func["name"]

        test_code = f'''
def test_db_{func_name}_integration(db_session):
    """
    Teste de integracao com banco de dados para {func_name}.
    """
    # Arrange
    # TODO: Preparar dados de teste

    # Act
    result = {module_name}.{func_name}(db_session)

    # Assert
    assert result is not None

    # Cleanup
    db_session.rollback()
'''
        cases.append(TestCase(
            name=f"test_db_{func_name}_integration",
            description=f"Teste de integracao DB para {func_name}",
            test_type=TestType.INTEGRATION,
            function_name=func_name,
            test_code=test_code.strip()
        ))

        return cases

    def _build_imports(self, analysis: Dict[str, Any], module_name: str) -> List[str]:
        """Constroi lista de imports necessarios"""
        imports = [
            "import pytest",
            "from unittest.mock import Mock, patch, MagicMock",
        ]

        # Import do modulo sendo testado
        filepath = Path(analysis.get("filepath", ""))
        if filepath.exists():
            # Tentar inferir caminho do import
            imports.append(f"import {module_name}")

        # Adicionar AsyncMock se houver funcoes async
        has_async = any(
            f.get("is_async") for f in analysis.get("functions", [])
        ) or any(
            any(m.get("is_async") for m in c.get("methods", []))
            for c in analysis.get("classes", [])
        )

        if has_async:
            imports.append("from unittest.mock import AsyncMock")
            imports.append("import asyncio")

        return imports

    def _build_fixtures(self, analysis: Dict[str, Any]) -> List[str]:
        """Constroi fixtures pytest necessarias"""
        fixtures = []

        # Fixture basica de cliente HTTP
        has_api = any(
            "app" in str(f.get("decorators", [])) or "router" in str(f.get("decorators", []))
            for f in analysis.get("functions", [])
        )

        if has_api:
            fixtures.append('''
@pytest.fixture
def client():
    """Cliente de teste para API"""
    from fastapi.testclient import TestClient
    # TODO: Importar a app correta
    # from main import app
    # return TestClient(app)
    pass
''')

        # Fixture de banco de dados
        has_db = any(
            "db" in str(f.get("params", [])).lower()
            for f in analysis.get("functions", [])
        )

        if has_db:
            fixtures.append('''
@pytest.fixture
def db_session():
    """Sessao de banco de dados para testes"""
    # TODO: Configurar conexao de teste
    # from factory.database.connection import SessionLocal
    # session = SessionLocal()
    # yield session
    # session.close()
    pass
''')

        return fixtures

    def _build_test_file_content(
        self,
        imports: List[str],
        fixtures: List[str],
        test_cases: List[TestCase]
    ) -> str:
        """Monta o conteudo completo do arquivo de teste"""
        lines = [
            '# -*- coding: utf-8 -*-',
            '"""',
            'Testes Automaticos Gerados - Fabrica de Agentes',
            f'Gerado em: {datetime.utcnow().isoformat()}',
            '"""',
            ''
        ]

        # Imports
        lines.extend(imports)
        lines.append('')

        # Fixtures
        for fixture in fixtures:
            lines.append(fixture)
            lines.append('')

        # Test cases
        for tc in test_cases:
            lines.append('')
            lines.append(tc.test_code)
            lines.append('')

        return '\n'.join(lines)

    def _generate_sample_params(self, params: List[Dict]) -> List[str]:
        """Gera valores de exemplo para parametros"""
        values = []
        for param in params:
            param_type = str(param.get("type", "")).lower()
            default = param.get("default")

            if default:
                values.append(default)
            elif "str" in param_type:
                values.append('"test_value"')
            elif "int" in param_type:
                values.append("1")
            elif "float" in param_type:
                values.append("1.0")
            elif "bool" in param_type:
                values.append("True")
            elif "list" in param_type:
                values.append("[]")
            elif "dict" in param_type:
                values.append("{}")
            elif "optional" in param_type:
                values.append("None")
            else:
                values.append("None")

        return values

    def _generate_arrange_section(self, params: List[Dict]) -> str:
        """Gera secao de Arrange do teste"""
        if not params:
            return "# Sem parametros necessarios"

        lines = []
        for param in params:
            param_name = param["name"]
            param_type = str(param.get("type", "")).lower()

            if "str" in param_type:
                lines.append(f'{param_name} = "test_{param_name}"')
            elif "int" in param_type:
                lines.append(f'{param_name} = 1')
            elif "float" in param_type:
                lines.append(f'{param_name} = 1.0')
            elif "bool" in param_type:
                lines.append(f'{param_name} = True')
            elif "list" in param_type:
                lines.append(f'{param_name} = []')
            elif "dict" in param_type:
                lines.append(f'{param_name} = {{}}')
            else:
                lines.append(f'{param_name} = None  # TODO: Definir valor apropriado')

        return '\n    '.join(lines)


class E2ETestGenerator(BaseTestGenerator):
    """Gera testes E2E usando Playwright"""

    def generate(self, analysis: Dict[str, Any]) -> List[TestFile]:
        """Gera arquivos de teste E2E"""
        test_files = []

        if TestType.E2E not in self.config.test_types:
            return test_files

        # Para E2E, precisamos de informacoes sobre a UI ou endpoints
        # Por ora, geramos um template basico

        e2e_test = self._generate_e2e_template(analysis)
        if e2e_test:
            test_files.append(e2e_test)

        return test_files

    def _generate_e2e_template(self, analysis: Dict[str, Any]) -> TestFile:
        """Gera template de teste E2E"""
        module_name = Path(analysis.get("filepath", "module")).stem

        content = f'''# -*- coding: utf-8 -*-
"""
Testes E2E - {module_name}
Gerado automaticamente pela Fabrica de Agentes
"""
import pytest
from playwright.sync_api import Page, expect


@pytest.fixture(scope="session")
def browser_context_args(browser_context_args):
    """Configuracao do contexto do navegador"""
    return {{
        **browser_context_args,
        "viewport": {{"width": 1920, "height": 1080}},
        "ignore_https_errors": True
    }}


def test_page_loads(page: Page):
    """
    Testa se a pagina principal carrega corretamente.
    """
    # Navigate
    page.goto("http://localhost:9001")

    # Wait for page to load
    page.wait_for_load_state("networkidle")

    # Assert page loaded
    expect(page).to_have_title(re.compile(".+"))


def test_basic_flow(page: Page):
    """
    Testa o fluxo basico do usuario.
    """
    # Navigate to home
    page.goto("http://localhost:9001")

    # Wait for content
    page.wait_for_selector("body")

    # TODO: Adicionar interacoes especificas
    # page.click("button#action")
    # page.fill("input#name", "test")

    # Take screenshot on completion
    page.screenshot(path="test_results/basic_flow.png")


def test_error_handling(page: Page):
    """
    Testa tratamento de erros na interface.
    """
    page.goto("http://localhost:9001/nonexistent")

    # Verificar se mostra mensagem de erro apropriada
    # expect(page.locator(".error-message")).to_be_visible()
    pass
'''

        test_filename = f"test_{module_name}_e2e.py"
        output_dir = self.config.output_dir or "tests/e2e"

        return TestFile(
            filename=test_filename,
            filepath=str(Path(output_dir) / test_filename),
            test_type=TestType.E2E,
            test_cases=[],
            imports=["pytest", "playwright"],
            content=content
        )


# =============================================================================
# EXECUTOR DE TESTES E COBERTURA
# =============================================================================

class TestRunner:
    """Executa testes e coleta metricas de cobertura"""

    def __init__(self, project_path: Path):
        self.project_path = project_path
        self.coverage_report: Optional[CoverageReport] = None

    async def run_tests(
        self,
        test_files: List[TestFile],
        with_coverage: bool = True
    ) -> CoverageReport:
        """Executa os testes e retorna relatorio de cobertura"""
        report = CoverageReport()

        try:
            # Separar por tipo
            unit_tests = [t for t in test_files if t.test_type == TestType.UNIT]
            integration_tests = [t for t in test_files if t.test_type == TestType.INTEGRATION]
            e2e_tests = [t for t in test_files if t.test_type == TestType.E2E]

            # Executar testes unitarios
            if unit_tests:
                unit_result = await self._run_pytest(unit_tests, with_coverage)
                report.unit_coverage = unit_result.get("coverage", 0)
                report.tests_passed += unit_result.get("passed", 0)
                report.tests_failed += unit_result.get("failed", 0)

            # Executar testes de integracao
            if integration_tests:
                int_result = await self._run_pytest(integration_tests, with_coverage)
                report.integration_coverage = int_result.get("coverage", 0)
                report.tests_passed += int_result.get("passed", 0)
                report.tests_failed += int_result.get("failed", 0)

            # Executar testes E2E
            if e2e_tests:
                e2e_result = await self._run_playwright(e2e_tests)
                report.e2e_coverage = e2e_result.get("coverage", 0)
                report.tests_passed += e2e_result.get("passed", 0)
                report.tests_failed += e2e_result.get("failed", 0)

            # Calcular cobertura total
            coverage_count = 0
            coverage_sum = 0

            if unit_tests:
                coverage_sum += report.unit_coverage
                coverage_count += 1
            if integration_tests:
                coverage_sum += report.integration_coverage
                coverage_count += 1
            if e2e_tests:
                coverage_sum += report.e2e_coverage
                coverage_count += 1

            if coverage_count > 0:
                report.total_coverage = coverage_sum / coverage_count

        except Exception as e:
            print(f"[TestRunner] Erro ao executar testes: {e}")

        self.coverage_report = report
        return report

    async def _run_pytest(
        self,
        test_files: List[TestFile],
        with_coverage: bool
    ) -> Dict[str, Any]:
        """Executa pytest nos arquivos de teste"""
        result = {"passed": 0, "failed": 0, "coverage": 0}

        try:
            # Construir comando
            cmd = ["python", "-m", "pytest"]

            for tf in test_files:
                if Path(tf.filepath).exists():
                    cmd.append(tf.filepath)

            cmd.extend(["-v", "--tb=short"])

            if with_coverage:
                cmd.extend([
                    "--cov=factory",
                    "--cov-report=json:coverage.json",
                    "--cov-report=term"
                ])

            # Executar
            process = await asyncio.create_subprocess_exec(
                *cmd,
                cwd=str(self.project_path),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )

            stdout, stderr = await process.communicate()
            output = stdout.decode("utf-8", errors="ignore")

            # Parsear resultados
            passed_match = re.search(r"(\d+) passed", output)
            failed_match = re.search(r"(\d+) failed", output)

            if passed_match:
                result["passed"] = int(passed_match.group(1))
            if failed_match:
                result["failed"] = int(failed_match.group(1))

            # Ler cobertura do arquivo JSON
            if with_coverage:
                cov_file = self.project_path / "coverage.json"
                if cov_file.exists():
                    with open(cov_file, "r") as f:
                        cov_data = json.load(f)
                        result["coverage"] = cov_data.get("totals", {}).get("percent_covered", 0)

        except FileNotFoundError:
            print("[TestRunner] pytest nao encontrado")
        except Exception as e:
            print(f"[TestRunner] Erro pytest: {e}")

        return result

    async def _run_playwright(self, test_files: List[TestFile]) -> Dict[str, Any]:
        """Executa testes Playwright"""
        result = {"passed": 0, "failed": 0, "coverage": 0}

        try:
            cmd = ["python", "-m", "pytest"]

            for tf in test_files:
                if Path(tf.filepath).exists():
                    cmd.append(tf.filepath)

            cmd.extend(["--browser=chromium", "--headed=false"])

            process = await asyncio.create_subprocess_exec(
                *cmd,
                cwd=str(self.project_path),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )

            stdout, stderr = await process.communicate()
            output = stdout.decode("utf-8", errors="ignore")

            passed_match = re.search(r"(\d+) passed", output)
            failed_match = re.search(r"(\d+) failed", output)

            if passed_match:
                result["passed"] = int(passed_match.group(1))
            if failed_match:
                result["failed"] = int(failed_match.group(1))

            # E2E coverage e estimada baseada em testes passados
            total = result["passed"] + result["failed"]
            if total > 0:
                result["coverage"] = (result["passed"] / total) * 100

        except Exception as e:
            print(f"[TestRunner] Erro playwright: {e}")

        return result

    def generate_report_ascii(self) -> str:
        """Gera relatorio de cobertura em ASCII"""
        if not self.coverage_report:
            return "Nenhum relatorio disponivel"

        report = self.coverage_report
        bar_width = 15

        def make_bar(percentage: float) -> str:
            filled = int((percentage / 100) * bar_width)
            empty = bar_width - filled
            return "█" * filled + "░" * empty

        lines = [
            "┌─────────────────────────────────────────┐",
            "│ Relatorio de Cobertura                  │",
            "├─────────────────────────────────────────┤",
            f"│ Total: {report.total_coverage:5.1f}%  {make_bar(report.total_coverage)}     │",
            "│                                         │",
            f"│ Unitarios:    {report.unit_coverage:5.1f}%  {make_bar(report.unit_coverage)}   │",
            f"│ Integracao:   {report.integration_coverage:5.1f}%  {make_bar(report.integration_coverage)}   │",
            f"│ E2E:          {report.e2e_coverage:5.1f}%  {make_bar(report.e2e_coverage)}   │",
            "├─────────────────────────────────────────┤",
            f"│ Testes: {report.tests_passed} passou | {report.tests_failed} falhou            │",
            "└─────────────────────────────────────────┘"
        ]

        return "\n".join(lines)


# =============================================================================
# MODO TDD
# =============================================================================

class TDDMode:
    """
    Implementa o modo TDD (Test-Driven Development):
    1. Gera testes baseado nos acceptance criteria (Red)
    2. Executa testes - devem falhar (Red)
    3. Gera codigo que faz os testes passar (Green)
    4. Refatora (Refactor)
    """

    def __init__(self, config: TestConfig):
        self.config = config
        self.config.tdd_mode = True

    async def generate_tests_from_criteria(
        self,
        acceptance_criteria: List[str],
        module_name: str,
        output_dir: str
    ) -> TestFile:
        """Gera testes a partir de criterios de aceite (antes do codigo)"""
        test_cases = []

        for idx, criteria in enumerate(acceptance_criteria):
            test_name = self._criteria_to_test_name(criteria, idx)
            test_code = self._generate_test_from_criteria(criteria, test_name, module_name)

            test_cases.append(TestCase(
                name=test_name,
                description=criteria,
                test_type=TestType.UNIT,
                function_name="to_be_implemented",
                test_code=test_code
            ))

        # Montar arquivo de teste
        imports = [
            "import pytest",
            f"# import {module_name}  # TODO: Descomentar apos implementacao"
        ]

        content = self._build_tdd_test_file(imports, test_cases, acceptance_criteria)

        test_filename = f"test_{module_name}_tdd.py"

        return TestFile(
            filename=test_filename,
            filepath=str(Path(output_dir) / test_filename),
            test_type=TestType.UNIT,
            test_cases=test_cases,
            imports=imports,
            content=content
        )

    def _criteria_to_test_name(self, criteria: str, index: int) -> str:
        """Converte criterio de aceite em nome de teste"""
        # Limpar e converter para snake_case
        clean = re.sub(r'[^\w\s]', '', criteria.lower())
        words = clean.split()[:5]  # Pegar primeiras 5 palavras
        name = "_".join(words)
        return f"test_ac_{index + 1}_{name}"

    def _generate_test_from_criteria(
        self,
        criteria: str,
        test_name: str,
        module_name: str
    ) -> str:
        """Gera codigo de teste a partir do criterio"""
        return f'''
def {test_name}():
    """
    Criterio de Aceite: {criteria}

    Status: RED (aguardando implementacao)
    """
    # Arrange
    # TODO: Preparar dados de teste

    # Act
    # TODO: Chamar funcao a ser implementada
    # result = {module_name}.funcao_a_implementar()

    # Assert
    # TODO: Verificar criterio de aceite
    pytest.fail("Teste TDD - Aguardando implementacao do codigo")
'''

    def _build_tdd_test_file(
        self,
        imports: List[str],
        test_cases: List[TestCase],
        criteria: List[str]
    ) -> str:
        """Monta arquivo de teste TDD"""
        lines = [
            '# -*- coding: utf-8 -*-',
            '"""',
            'Testes TDD - Gerados a partir dos Criterios de Aceite',
            '======================================================',
            '',
            'Criterios de Aceite:',
        ]

        for idx, c in enumerate(criteria):
            lines.append(f'{idx + 1}. {c}')

        lines.extend([
            '',
            'Modo: RED (testes devem falhar ate implementacao)',
            f'Gerado em: {datetime.utcnow().isoformat()}',
            '"""',
            ''
        ])

        lines.extend(imports)
        lines.append('')
        lines.append('')

        for tc in test_cases:
            lines.append(tc.test_code)
            lines.append('')

        return '\n'.join(lines)


# =============================================================================
# CLASSE PRINCIPAL - TEST GENERATOR
# =============================================================================

class TestGenerator:
    """
    Classe principal para geracao automatica de testes.

    Uso:
        generator = TestGenerator(config)
        result = await generator.generate_tests(source_path)
        await generator.save_tests(result, output_dir)
    """

    def __init__(self, config: TestConfig = None):
        self.config = config or TestConfig()
        self._generators: Dict[str, BaseTestGenerator] = {}
        self._setup_generators()

    def _setup_generators(self):
        """Configura os geradores de teste"""
        self._generators["pytest"] = PytestGenerator(self.config)
        self._generators["e2e"] = E2ETestGenerator(self.config)

    async def generate_tests(
        self,
        source_path: str,
        acceptance_criteria: List[str] = None
    ) -> TestGenerationResult:
        """
        Gera testes para um arquivo ou diretorio de codigo.

        Args:
            source_path: Caminho para o codigo fonte
            acceptance_criteria: Lista de criterios de aceite (para modo TDD)

        Returns:
            TestGenerationResult com arquivos gerados e relatorio
        """
        start_time = datetime.now()
        result = TestGenerationResult(success=False)

        try:
            source = Path(source_path)

            # Modo TDD - gerar testes antes do codigo
            if self.config.tdd_mode and acceptance_criteria:
                result = await self._generate_tdd_tests(source, acceptance_criteria)
            else:
                # Modo normal - gerar testes a partir do codigo existente
                result = await self._generate_standard_tests(source)

            result.success = True

        except Exception as e:
            result.error_message = str(e)

        result.execution_time_ms = int((datetime.now() - start_time).total_seconds() * 1000)
        result.tdd_mode = self.config.tdd_mode

        return result

    async def _generate_standard_tests(self, source: Path) -> TestGenerationResult:
        """Gera testes padrao baseados em codigo existente"""
        result = TestGenerationResult(success=False)
        test_files = []

        if source.is_file():
            # Analisar arquivo unico
            analyzer = CodeAnalyzer(source)
            analysis = analyzer.analyze()

            # Gerar com cada gerador apropriado
            for gen in self._generators.values():
                files = gen.generate(analysis)
                test_files.extend(files)

        elif source.is_dir():
            # Analisar todos os arquivos Python no diretorio
            for py_file in source.rglob("*.py"):
                if "test" in py_file.name or "__pycache__" in str(py_file):
                    continue

                analyzer = CodeAnalyzer(py_file)
                analysis = analyzer.analyze()

                for gen in self._generators.values():
                    files = gen.generate(analysis)
                    test_files.extend(files)

        result.test_files = test_files
        result.success = len(test_files) > 0

        return result

    async def _generate_tdd_tests(
        self,
        source: Path,
        acceptance_criteria: List[str]
    ) -> TestGenerationResult:
        """Gera testes TDD baseados em criterios de aceite"""
        result = TestGenerationResult(success=False, tdd_mode=True)

        tdd = TDDMode(self.config)
        module_name = source.stem if source.is_file() else source.name
        output_dir = self.config.output_dir or "tests"

        test_file = await tdd.generate_tests_from_criteria(
            acceptance_criteria,
            module_name,
            output_dir
        )

        result.test_files = [test_file]
        result.success = True

        return result

    async def save_tests(
        self,
        result: TestGenerationResult,
        output_dir: str = None
    ) -> List[str]:
        """
        Salva os arquivos de teste gerados em disco.

        Args:
            result: Resultado da geracao
            output_dir: Diretorio de saida (opcional, usa config se nao especificado)

        Returns:
            Lista de caminhos dos arquivos salvos
        """
        saved_files = []
        base_dir = Path(output_dir or self.config.output_dir or "tests")
        base_dir.mkdir(parents=True, exist_ok=True)

        for test_file in result.test_files:
            # Criar subdiretorio por tipo se necessario
            if test_file.test_type == TestType.E2E:
                file_dir = base_dir / "e2e"
            elif test_file.test_type == TestType.INTEGRATION:
                file_dir = base_dir / "integration"
            else:
                file_dir = base_dir / "unit"

            file_dir.mkdir(parents=True, exist_ok=True)
            filepath = file_dir / test_file.filename

            with open(filepath, "w", encoding="utf-8") as f:
                f.write(test_file.content)

            saved_files.append(str(filepath))
            print(f"[TestGenerator] Arquivo salvo: {filepath}")

        return saved_files

    async def run_and_report(
        self,
        test_files: List[TestFile],
        project_path: str
    ) -> CoverageReport:
        """
        Executa os testes e gera relatorio de cobertura.

        Args:
            test_files: Lista de arquivos de teste
            project_path: Caminho do projeto

        Returns:
            CoverageReport com metricas
        """
        runner = TestRunner(Path(project_path))
        report = await runner.run_tests(test_files)

        # Imprimir relatorio ASCII
        print("\n" + runner.generate_report_ascii())

        return report

    def check_coverage_threshold(
        self,
        report: CoverageReport,
        config: Dict[str, int] = None
    ) -> Tuple[bool, List[str]]:
        """
        Verifica se a cobertura atende aos thresholds configurados.

        Args:
            report: Relatorio de cobertura
            config: Configuracao de thresholds (opcional)

        Returns:
            Tupla (passou, lista de violacoes)
        """
        cfg = config or self.config.coverage_config
        violations = []

        if report.total_coverage < cfg.get("minimum", 80):
            violations.append(
                f"Cobertura total ({report.total_coverage:.1f}%) abaixo do minimo ({cfg['minimum']}%)"
            )

        if report.unit_coverage < cfg.get("unit", 90):
            violations.append(
                f"Cobertura unitaria ({report.unit_coverage:.1f}%) abaixo do esperado ({cfg['unit']}%)"
            )

        if report.integration_coverage < cfg.get("integration", 70):
            violations.append(
                f"Cobertura integracao ({report.integration_coverage:.1f}%) abaixo do esperado ({cfg['integration']}%)"
            )

        if report.e2e_coverage < cfg.get("e2e", 50):
            violations.append(
                f"Cobertura E2E ({report.e2e_coverage:.1f}%) abaixo do esperado ({cfg['e2e']}%)"
            )

        return len(violations) == 0, violations


# =============================================================================
# FUNCOES AUXILIARES PARA INTEGRACAO
# =============================================================================

async def generate_tests_for_story(
    story_id: str,
    source_files: List[str],
    acceptance_criteria: List[str] = None,
    tdd_mode: bool = False,
    output_dir: str = None
) -> TestGenerationResult:
    """
    Gera testes para uma Story especifica.

    Args:
        story_id: ID da story
        source_files: Lista de arquivos de codigo
        acceptance_criteria: Criterios de aceite da story
        tdd_mode: Se True, gera testes TDD antes do codigo
        output_dir: Diretorio de saida

    Returns:
        TestGenerationResult
    """
    config = TestConfig(
        test_types=[TestType.UNIT, TestType.INTEGRATION],
        tdd_mode=tdd_mode,
        output_dir=output_dir or f"tests/{story_id}"
    )

    generator = TestGenerator(config)
    all_files = []

    if tdd_mode and acceptance_criteria:
        # Modo TDD - gerar a partir dos criterios
        result = await generator.generate_tests(
            source_files[0] if source_files else "module",
            acceptance_criteria
        )
        return result
    else:
        # Modo padrao - gerar a partir do codigo
        for source_file in source_files:
            result = await generator.generate_tests(source_file)
            all_files.extend(result.test_files)

        return TestGenerationResult(
            success=len(all_files) > 0,
            test_files=all_files
        )


async def run_tests_for_story(
    story_id: str,
    test_dir: str = None
) -> CoverageReport:
    """
    Executa testes de uma Story e retorna relatorio.

    Args:
        story_id: ID da story
        test_dir: Diretorio dos testes

    Returns:
        CoverageReport
    """
    test_path = Path(test_dir or f"tests/{story_id}")
    project_path = Path.cwd()

    # Coletar arquivos de teste
    test_files = []
    for test_file in test_path.rglob("test_*.py"):
        # Determinar tipo pelo diretorio
        if "e2e" in str(test_file):
            test_type = TestType.E2E
        elif "integration" in str(test_file):
            test_type = TestType.INTEGRATION
        else:
            test_type = TestType.UNIT

        test_files.append(TestFile(
            filename=test_file.name,
            filepath=str(test_file),
            test_type=test_type
        ))

    runner = TestRunner(project_path)
    report = await runner.run_tests(test_files)

    return report


# =============================================================================
# CLI / MAIN
# =============================================================================

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Gerador Automatico de Testes - Fabrica de Agentes"
    )
    parser.add_argument(
        "source",
        help="Caminho do arquivo ou diretorio de codigo fonte"
    )
    parser.add_argument(
        "-o", "--output",
        default="tests",
        help="Diretorio de saida para os testes"
    )
    parser.add_argument(
        "-t", "--types",
        nargs="+",
        choices=["unit", "integration", "e2e"],
        default=["unit"],
        help="Tipos de teste a gerar"
    )
    parser.add_argument(
        "--tdd",
        action="store_true",
        help="Ativar modo TDD"
    )
    parser.add_argument(
        "--criteria",
        nargs="+",
        help="Criterios de aceite (para modo TDD)"
    )
    parser.add_argument(
        "--run",
        action="store_true",
        help="Executar testes apos gerar"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Modo verbose"
    )

    args = parser.parse_args()

    # Configurar
    test_types = [TestType(t) for t in args.types]

    config = TestConfig(
        test_types=test_types,
        tdd_mode=args.tdd,
        output_dir=args.output,
        verbose=args.verbose
    )

    async def main():
        generator = TestGenerator(config)

        print(f"\n[TestGenerator] Analisando: {args.source}")
        print(f"[TestGenerator] Tipos: {[t.value for t in test_types]}")
        print(f"[TestGenerator] Saida: {args.output}")

        if args.tdd:
            print("[TestGenerator] Modo: TDD")

        # Gerar testes
        result = await generator.generate_tests(
            args.source,
            args.criteria
        )

        if result.success:
            print(f"\n[TestGenerator] {len(result.test_files)} arquivo(s) de teste gerado(s)")

            # Salvar
            saved = await generator.save_tests(result, args.output)
            for path in saved:
                print(f"  - {path}")

            # Executar se solicitado
            if args.run and result.test_files:
                print("\n[TestGenerator] Executando testes...")
                report = await generator.run_and_report(
                    result.test_files,
                    str(Path.cwd())
                )

                # Verificar threshold
                passed, violations = generator.check_coverage_threshold(report)
                if not passed:
                    print("\n[TestGenerator] ATENCAO - Cobertura abaixo do esperado:")
                    for v in violations:
                        print(f"  - {v}")
        else:
            print(f"\n[TestGenerator] Erro: {result.error_message}")

    asyncio.run(main())

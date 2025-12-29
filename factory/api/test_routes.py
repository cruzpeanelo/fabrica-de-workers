# -*- coding: utf-8 -*-
"""
Test Routes - API para Geracao Automatica de Testes (Issue #53)
================================================================
Fabrica de Agentes

Endpoints para geracao, execucao e cobertura de testes.
"""
import os
import sys
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Optional, List

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel

# Adicionar path do projeto
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

# Database
from factory.database.connection import SessionLocal
from factory.database.repositories import StoryRepository, StoryTaskRepository

# Test Generator
try:
    from factory.core.test_generator import (
        TestGenerator, TestConfig, TestType, TestRunner,
        CoverageReport, generate_tests_for_story, run_tests_for_story
    )
    HAS_TEST_GENERATOR = True
except ImportError:
    HAS_TEST_GENERATOR = False
    print("[TestRoutes] Test Generator not available")


# Router
router = APIRouter(prefix="/api", tags=["Tests"])


# =============================================================================
# SCHEMAS
# =============================================================================

class GenerateTestsRequest(BaseModel):
    """Request para geracao de testes"""
    test_types: Optional[List[str]] = ["unit"]  # unit, integration, e2e
    tdd_mode: Optional[bool] = False
    include_edge_cases: Optional[bool] = True
    include_error_cases: Optional[bool] = True
    coverage_target: Optional[int] = 80


class RunTestsRequest(BaseModel):
    """Request para executar testes"""
    test_dir: Optional[str] = None
    with_coverage: Optional[bool] = True


class CoverageConfig(BaseModel):
    """Configuracao de cobertura minima"""
    minimum: int = 80
    unit: int = 90
    integration: int = 70
    e2e: int = 50


# =============================================================================
# ROUTES - STORY TESTS
# =============================================================================

@router.post("/stories/{story_id}/generate-tests")
async def generate_story_tests(story_id: str, request: Optional[GenerateTestsRequest] = None):
    """
    Gera testes automaticos para uma Story completa.

    Analisa o codigo de todas as tasks da story e gera:
    - Testes unitarios (pytest)
    - Testes de integracao
    - Testes E2E (Playwright)

    Args:
        story_id: ID da story
        request: Configuracoes de geracao

    Returns:
        Lista de arquivos de teste gerados
    """
    db = SessionLocal()
    try:
        # Buscar story com tasks
        story_repo = StoryRepository(db)
        story_data = story_repo.get_with_tasks(story_id)
        if not story_data:
            raise HTTPException(404, "Story not found")

        story = story_data.get("story", story_data)
        tasks = story_data.get("tasks", [])

        # Configurar geracao
        req = request or GenerateTestsRequest()

        # Fallback se TestGenerator nao disponivel
        if not HAS_TEST_GENERATOR:
            results = []
            for task in tasks:
                if task.get("code_output"):
                    safe_title = task.get("title", "task").replace(" ", "_").replace("-", "_")
                    safe_title = ''.join(c for c in safe_title if c.isalnum() or c == '_')

                    test_code = f'''# -*- coding: utf-8 -*-
"""
Testes para: {task.get("title")}
Story: {story_id}
Gerado automaticamente pela Fabrica de Agentes
"""
import pytest


class Test{safe_title}:
    """Testes automaticos para {task.get("title")}"""

    def test_basic_functionality(self):
        """Testa funcionalidade basica"""
        # TODO: Implementar teste real
        assert True

    def test_edge_cases(self):
        """Testa casos de borda"""
        # TODO: Implementar casos de borda
        assert True

    def test_error_handling(self):
        """Testa tratamento de erros"""
        # TODO: Implementar teste de erros
        with pytest.raises(Exception):
            raise Exception("Teste de erro")
'''
                    results.append({
                        "task_id": task.get("task_id"),
                        "test_code": test_code,
                        "test_count": 3,
                        "type": "unit"
                    })

            return {
                "story_id": story_id,
                "tasks_with_tests": len(results),
                "total_tests": sum(r["test_count"] for r in results),
                "results": results,
                "is_template": True,
                "generated_at": datetime.utcnow().isoformat()
            }

        # Usar TestGenerator completo
        try:
            source_files = []
            temp_files = []

            # Criar arquivos temporarios com codigo das tasks
            for task in tasks:
                if task.get("code_output"):
                    with tempfile.NamedTemporaryFile(
                        mode='w',
                        suffix='.py',
                        delete=False,
                        encoding='utf-8'
                    ) as f:
                        f.write(task.get("code_output"))
                        source_files.append(f.name)
                        temp_files.append(f.name)

            # Criterios de aceite para modo TDD
            acceptance_criteria = []
            if isinstance(story, dict):
                acceptance_criteria = story.get("acceptance_criteria", [])
            else:
                acceptance_criteria = story.acceptance_criteria or []

            # Tipos de teste
            test_types = []
            for t in req.test_types:
                if t == "unit":
                    test_types.append(TestType.UNIT)
                elif t == "integration":
                    test_types.append(TestType.INTEGRATION)
                elif t == "e2e":
                    test_types.append(TestType.E2E)

            if not test_types:
                test_types = [TestType.UNIT]

            # Configurar generator
            config = TestConfig(
                test_types=test_types,
                tdd_mode=req.tdd_mode,
                generate_mocks=True,
                include_edge_cases=req.include_edge_cases,
                include_error_cases=req.include_error_cases,
                output_dir=f"tests/{story_id}",
                verbose=False
            )

            generator = TestGenerator(config)

            # Gerar testes
            if req.tdd_mode and acceptance_criteria:
                result = await generator.generate_tests(
                    source_files[0] if source_files else story_id,
                    acceptance_criteria
                )
                all_test_files = result.test_files if result.success else []
            else:
                all_test_files = []
                for src_file in source_files:
                    res = await generator.generate_tests(src_file)
                    if res.success:
                        all_test_files.extend(res.test_files)

            # Limpar arquivos temporarios
            for temp_file in temp_files:
                try:
                    os.unlink(temp_file)
                except:
                    pass

            # Retornar resultado
            return {
                "story_id": story_id,
                "success": len(all_test_files) > 0,
                "test_files": [
                    {
                        "filename": tf.filename,
                        "type": tf.test_type.value,
                        "content": tf.content,
                        "test_count": len(tf.test_cases),
                        "tests": [tc.name for tc in tf.test_cases]
                    }
                    for tf in all_test_files
                ],
                "total_tests": sum(len(tf.test_cases) for tf in all_test_files),
                "total_files": len(all_test_files),
                "tdd_mode": req.tdd_mode,
                "is_template": False,
                "generated_at": datetime.utcnow().isoformat()
            }

        except Exception as e:
            raise HTTPException(500, f"Test generation error: {str(e)}")

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error: {str(e)}")
    finally:
        db.close()


@router.get("/stories/{story_id}/coverage")
async def get_story_coverage(story_id: str):
    """
    Retorna relatorio de cobertura de testes de uma Story.

    Mostra metricas de cobertura para:
    - Testes unitarios
    - Testes de integracao
    - Testes E2E
    """
    db = SessionLocal()
    try:
        story_repo = StoryRepository(db)
        story = story_repo.get_by_id(story_id)
        if not story:
            raise HTTPException(404, "Story not found")

        if not HAS_TEST_GENERATOR:
            return {
                "story_id": story_id,
                "total_coverage": 0,
                "unit_coverage": 0,
                "integration_coverage": 0,
                "e2e_coverage": 0,
                "tests_passed": 0,
                "tests_failed": 0,
                "message": "TestGenerator not available",
                "generated_at": datetime.utcnow().isoformat()
            }

        try:
            report = await run_tests_for_story(story_id, f"tests/{story_id}")

            return {
                "story_id": story_id,
                "total_coverage": report.total_coverage,
                "unit_coverage": report.unit_coverage,
                "integration_coverage": report.integration_coverage,
                "e2e_coverage": report.e2e_coverage,
                "tests_passed": report.tests_passed,
                "tests_failed": report.tests_failed,
                "tests_skipped": report.tests_skipped,
                "files": report.files,
                "lines_covered": report.lines_covered,
                "lines_total": report.lines_total,
                "generated_at": report.generated_at
            }
        except Exception as e:
            return {
                "story_id": story_id,
                "total_coverage": 0,
                "error": str(e),
                "generated_at": datetime.utcnow().isoformat()
            }

    finally:
        db.close()


@router.post("/stories/{story_id}/run-tests")
async def run_story_tests(story_id: str, request: Optional[RunTestsRequest] = None):
    """
    Executa os testes de uma Story e retorna resultados.

    Executa todos os testes gerados e retorna:
    - Numero de testes passando/falhando
    - Cobertura de codigo
    - Detalhes de falhas
    - Relatorio ASCII visual
    """
    db = SessionLocal()
    try:
        story_repo = StoryRepository(db)
        story = story_repo.get_by_id(story_id)
        if not story:
            raise HTTPException(404, "Story not found")

        req = request or RunTestsRequest()

        if not HAS_TEST_GENERATOR:
            return {
                "story_id": story_id,
                "success": False,
                "message": "TestGenerator not available"
            }

        try:
            test_dir = req.test_dir or f"tests/{story_id}"
            report = await run_tests_for_story(story_id, test_dir)

            # Gerar relatorio ASCII
            runner = TestRunner(Path.cwd())
            runner.coverage_report = report
            ascii_report = runner.generate_report_ascii()

            # Verificar threshold
            passed_threshold = report.total_coverage >= 80

            return {
                "story_id": story_id,
                "success": True,
                "coverage": {
                    "total": report.total_coverage,
                    "unit": report.unit_coverage,
                    "integration": report.integration_coverage,
                    "e2e": report.e2e_coverage
                },
                "tests": {
                    "passed": report.tests_passed,
                    "failed": report.tests_failed,
                    "skipped": report.tests_skipped,
                    "total": report.tests_passed + report.tests_failed + report.tests_skipped
                },
                "passed_threshold": passed_threshold,
                "ascii_report": ascii_report,
                "generated_at": report.generated_at
            }

        except Exception as e:
            return {
                "story_id": story_id,
                "success": False,
                "error": str(e)
            }

    finally:
        db.close()


# =============================================================================
# ROUTES - TASK TESTS
# =============================================================================

@router.post("/story-tasks/{task_id}/generate-tests-v2")
async def generate_task_tests_v2(
    task_id: str,
    tdd_mode: bool = Query(False, description="Ativar modo TDD"),
    include_edge_cases: bool = Query(True, description="Incluir casos de borda"),
    include_error_cases: bool = Query(True, description="Incluir testes de erro")
):
    """
    Gera testes automaticos para uma Task especifica (versao melhorada).

    Usa o TestGenerator para analise de codigo e geracao inteligente
    de testes unitarios e de integracao.

    Args:
        task_id: ID da task
        tdd_mode: Se True, gera testes TDD baseados nos criterios de aceite
        include_edge_cases: Incluir testes de casos de borda
        include_error_cases: Incluir testes de tratamento de erro
    """
    db = SessionLocal()
    try:
        task_repo = StoryTaskRepository(db)
        task = task_repo.get_by_id(task_id)
        if not task:
            raise HTTPException(404, "Task not found")

        # Buscar story para criterios
        story_repo = StoryRepository(db)
        story = story_repo.get_by_id(task.story_id)

        # Detectar linguagem
        code = task.code_output or ""
        language = "python"
        framework = "pytest"

        if "function " in code or "const " in code or "let " in code:
            language = "javascript"
            framework = "jest"
        elif "func " in code and "package " in code:
            language = "go"
            framework = "testing"
        elif "public class " in code or "private void " in code:
            language = "java"
            framework = "junit"

        # Fallback
        if not HAS_TEST_GENERATOR or not code:
            result = _generate_basic_test_template(task, language, framework)

            # Salvar na task
            task.generated_tests = result
            task.updated_at = datetime.utcnow()
            db.commit()

            return result

        # Usar TestGenerator
        try:
            config = TestConfig(
                test_types=[TestType.UNIT, TestType.INTEGRATION],
                tdd_mode=tdd_mode,
                generate_mocks=True,
                include_edge_cases=include_edge_cases,
                include_error_cases=include_error_cases,
                verbose=False
            )

            generator = TestGenerator(config)

            # Criar arquivo temporario
            with tempfile.NamedTemporaryFile(
                mode='w',
                suffix='.py',
                delete=False,
                encoding='utf-8'
            ) as f:
                f.write(code)
                temp_file = f.name

            try:
                if tdd_mode and story and story.acceptance_criteria:
                    gen_result = await generator.generate_tests(
                        temp_file,
                        story.acceptance_criteria
                    )
                else:
                    gen_result = await generator.generate_tests(temp_file)
            finally:
                os.unlink(temp_file)

            if gen_result.success and gen_result.test_files:
                test_codes = [tf.content for tf in gen_result.test_files]
                combined_code = "\n\n".join(test_codes)
                test_count = sum(len(tf.test_cases) for tf in gen_result.test_files)

                result = {
                    "language": language,
                    "framework": framework,
                    "test_code": combined_code,
                    "test_count": test_count,
                    "test_files": [
                        {
                            "filename": tf.filename,
                            "type": tf.test_type.value,
                            "tests": len(tf.test_cases)
                        }
                        for tf in gen_result.test_files
                    ],
                    "coverage_estimate": "high" if test_count > 5 else "medium" if test_count > 3 else "low",
                    "generated_at": datetime.utcnow().isoformat(),
                    "is_template": False,
                    "tdd_mode": tdd_mode,
                    "execution_time_ms": gen_result.execution_time_ms
                }
            else:
                result = _generate_basic_test_template(task, language, framework)

        except Exception as e:
            print(f"[TestGenerator] Erro: {e}")
            result = _generate_basic_test_template(task, language, framework)

        # Salvar na task
        task.generated_tests = result
        task.updated_at = datetime.utcnow()
        db.commit()

        return result

    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, f"Error generating tests: {str(e)}")
    finally:
        db.close()


@router.get("/story-tasks/{task_id}/tests")
async def get_task_tests(task_id: str):
    """
    Retorna os testes gerados para uma Task.
    """
    db = SessionLocal()
    try:
        task_repo = StoryTaskRepository(db)
        task = task_repo.get_by_id(task_id)
        if not task:
            raise HTTPException(404, "Task not found")

        generated = task.generated_tests or {}

        return {
            "task_id": task_id,
            "has_tests": bool(generated),
            "tests": generated,
            "test_results": task.test_results or {}
        }

    finally:
        db.close()


# =============================================================================
# HELPERS
# =============================================================================

def _generate_basic_test_template(task, language: str, framework: str) -> dict:
    """Gera template basico de testes (fallback)"""
    safe_title = task.title.replace(" ", "_").replace("-", "_").replace(".", "_")
    safe_title = ''.join(c for c in safe_title if c.isalnum() or c == '_')

    test_templates = {
        "python": f'''# -*- coding: utf-8 -*-
"""
Testes Automaticos - {task.title}
Gerado pela Fabrica de Agentes (Issue #53)
"""
import pytest


class Test{safe_title}:
    """Testes para {task.title}"""

    def test_basic_functionality(self):
        """Testa funcionalidade basica"""
        # TODO: Implementar teste real baseado no codigo
        assert True

    def test_edge_cases(self):
        """Testa casos de borda"""
        # Exemplos de edge cases:
        # - Valores nulos/vazios
        # - Valores muito grandes/pequenos
        # - Strings especiais
        assert True

    def test_error_handling(self):
        """Testa tratamento de erros"""
        # TODO: Testar excecoes esperadas
        with pytest.raises(Exception):
            raise Exception("Teste de erro")


class Test{safe_title}Integration:
    """Testes de integracao"""

    @pytest.fixture
    def setup_data(self):
        """Prepara dados de teste"""
        return {{"test": True}}

    def test_integration_flow(self, setup_data):
        """Testa fluxo de integracao"""
        assert setup_data is not None
''',
        "javascript": f'''/**
 * Testes Automaticos - {task.title}
 * Gerado pela Fabrica de Agentes (Issue #53)
 */

describe('{task.title}', () => {{
    describe('Funcionalidade Basica', () => {{
        test('deve funcionar corretamente', () => {{
            // TODO: Implementar teste real
            expect(true).toBe(true);
        }});
    }});

    describe('Casos de Borda', () => {{
        test('deve lidar com valores nulos', () => {{
            expect(null).toBeNull();
        }});

        test('deve lidar com valores vazios', () => {{
            expect('').toBe('');
        }});
    }});

    describe('Tratamento de Erros', () => {{
        test('deve lancar erro apropriado', () => {{
            expect(() => {{
                throw new Error('Teste');
            }}).toThrow('Teste');
        }});
    }});
}});
'''
    }

    test_code = test_templates.get(language, test_templates["python"])

    return {
        "language": language,
        "framework": framework,
        "test_code": test_code,
        "test_count": 4 if language == "python" else 3,
        "coverage_estimate": "low",
        "generated_at": datetime.utcnow().isoformat(),
        "is_template": True,
        "tdd_mode": False
    }


# =============================================================================
# MAIN (para teste direto)
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI

    app = FastAPI(title="Test Routes - Fabrica de Agentes")
    app.include_router(router)

    uvicorn.run(app, host="0.0.0.0", port=8001)

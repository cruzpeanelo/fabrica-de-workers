# -*- coding: utf-8 -*-
"""
A/B Testing de Codigo Gerado - Issue #71
=========================================

Este modulo permite testar diferentes implementacoes de uma mesma feature,
comparando metricas de performance, qualidade e cobertura de testes.

Workflow:
1. Story criada
2. Gerar 2-3 variantes com abordagens diferentes
3. Executar testes em cada variante
4. Comparar metricas
5. IA recomenda vencedora
6. Usuario escolhe (ou aceita recomendacao)
7. Descartar outras variantes
"""

import os
import re
import json
import time
import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import Dict, List, Optional, Any
from datetime import datetime
from enum import Enum
import uuid


class VariantApproach(str, Enum):
    """Abordagens de implementacao para variantes"""
    SIMPLE = "simple"           # Codigo direto, facil de entender
    OPTIMIZED = "optimized"     # Performance otimizada
    ROBUST = "robust"           # Tratamento de erros robusto
    MODULAR = "modular"         # Alta modularidade
    FUNCTIONAL = "functional"   # Estilo funcional
    OOP = "oop"                 # Orientado a objetos


class ABTestStatus(str, Enum):
    """Status do teste A/B"""
    PENDING = "pending"           # Aguardando geracao de variantes
    GENERATING = "generating"     # Gerando variantes
    TESTING = "testing"           # Executando testes
    COMPLETED = "completed"       # Teste concluido
    WINNER_SELECTED = "winner_selected"  # Vencedor selecionado
    CANCELLED = "cancelled"       # Teste cancelado


class VariantStatus(str, Enum):
    """Status de uma variante"""
    PENDING = "pending"
    GENERATING = "generating"
    GENERATED = "generated"
    TESTING = "testing"
    TESTED = "tested"
    WINNER = "winner"
    DISCARDED = "discarded"
    FAILED = "failed"


class CodeMetrics:
    """Calcula metricas de codigo"""

    @staticmethod
    def count_lines(code: str) -> Dict[str, int]:
        """Conta linhas de codigo"""
        lines = code.split('\n')
        total = len(lines)
        blank = sum(1 for line in lines if not line.strip())
        comments = sum(1 for line in lines if line.strip().startswith('#') or line.strip().startswith('//'))
        code_lines = total - blank - comments
        return {
            "total_lines": total,
            "code_lines": code_lines,
            "blank_lines": blank,
            "comment_lines": comments
        }

    @staticmethod
    def calculate_complexity(code: str) -> Dict[str, Any]:
        """
        Calcula complexidade ciclomatica simplificada.
        Conta estruturas de decisao.
        """
        complexity_keywords = [
            r'\bif\b', r'\belif\b', r'\belse\b',
            r'\bfor\b', r'\bwhile\b',
            r'\btry\b', r'\bexcept\b',
            r'\band\b', r'\bor\b',
            r'\bcase\b', r'\bswitch\b'
        ]

        complexity = 1  # Base complexity
        for pattern in complexity_keywords:
            matches = re.findall(pattern, code)
            complexity += len(matches)

        # Classificar
        if complexity <= 5:
            level = "low"
        elif complexity <= 10:
            level = "medium"
        elif complexity <= 20:
            level = "high"
        else:
            level = "very_high"

        return {
            "cyclomatic_complexity": complexity,
            "complexity_level": level
        }

    @staticmethod
    def analyze_readability(code: str) -> Dict[str, Any]:
        """
        Analisa legibilidade do codigo.
        Baseado em heuristicas simples.
        """
        lines = code.split('\n')

        # Tamanho medio das linhas
        line_lengths = [len(line) for line in lines if line.strip()]
        avg_line_length = sum(line_lengths) / len(line_lengths) if line_lengths else 0

        # Linhas muito longas (>80 caracteres)
        long_lines = sum(1 for l in line_lengths if l > 80)

        # Funcoes/metodos definidos
        function_count = len(re.findall(r'\bdef\s+\w+|function\s+\w+|const\s+\w+\s*=\s*\(', code))

        # Classes definidas
        class_count = len(re.findall(r'\bclass\s+\w+', code))

        # Docstrings/comentarios
        has_docstrings = '"""' in code or "'''" in code or '/**' in code

        # Score de legibilidade (0-100)
        score = 100

        # Penalidades
        if avg_line_length > 80:
            score -= 10
        if avg_line_length > 100:
            score -= 10
        if long_lines > 5:
            score -= 5 * min(long_lines, 10)
        if not has_docstrings and function_count > 2:
            score -= 15

        # Bonus
        if has_docstrings:
            score += 5
        if function_count > 0 and function_count <= 10:
            score += 5

        score = max(0, min(100, score))

        return {
            "readability_score": score,
            "avg_line_length": round(avg_line_length, 1),
            "long_lines_count": long_lines,
            "function_count": function_count,
            "class_count": class_count,
            "has_docstrings": has_docstrings
        }

    @staticmethod
    def full_analysis(code: str) -> Dict[str, Any]:
        """Analise completa do codigo"""
        return {
            **CodeMetrics.count_lines(code),
            **CodeMetrics.calculate_complexity(code),
            **CodeMetrics.analyze_readability(code)
        }


class TestRunner:
    """Executa testes em variantes de codigo"""

    def __init__(self, work_dir: Optional[str] = None):
        self.work_dir = work_dir or tempfile.mkdtemp(prefix="ab_test_")

    def run_python_tests(self, code: str, test_code: str) -> Dict[str, Any]:
        """Executa testes Python"""
        test_dir = Path(self.work_dir) / f"test_{uuid.uuid4().hex[:8]}"
        test_dir.mkdir(parents=True, exist_ok=True)

        try:
            # Salvar codigo
            code_file = test_dir / "implementation.py"
            code_file.write_text(code, encoding="utf-8")

            # Salvar testes
            test_file = test_dir / "test_implementation.py"
            full_test = f"from implementation import *\n\n{test_code}"
            test_file.write_text(full_test, encoding="utf-8")

            # Executar pytest
            start_time = time.time()
            result = subprocess.run(
                ["python", "-m", "pytest", str(test_file), "-v", "--tb=short", "-q"],
                cwd=str(test_dir),
                capture_output=True,
                text=True,
                timeout=60
            )
            execution_time = time.time() - start_time

            # Parsear resultado
            output = result.stdout + result.stderr
            passed = len(re.findall(r'PASSED', output))
            failed = len(re.findall(r'FAILED', output))
            errors = len(re.findall(r'ERROR', output))
            total = passed + failed + errors

            return {
                "success": result.returncode == 0,
                "tests_passed": passed,
                "tests_failed": failed,
                "tests_error": errors,
                "tests_total": total,
                "pass_rate": (passed / total * 100) if total > 0 else 0,
                "execution_time_ms": round(execution_time * 1000),
                "output": output[:2000]  # Limitar output
            }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "tests_passed": 0,
                "tests_failed": 0,
                "tests_error": 1,
                "tests_total": 1,
                "pass_rate": 0,
                "execution_time_ms": 60000,
                "output": "Test execution timed out (60s)"
            }
        except Exception as e:
            return {
                "success": False,
                "tests_passed": 0,
                "tests_failed": 0,
                "tests_error": 1,
                "tests_total": 1,
                "pass_rate": 0,
                "execution_time_ms": 0,
                "output": f"Error running tests: {str(e)}"
            }
        finally:
            # Cleanup
            try:
                shutil.rmtree(test_dir)
            except:
                pass

    def run_performance_test(self, code: str, iterations: int = 100) -> Dict[str, Any]:
        """Executa teste de performance"""
        test_dir = Path(self.work_dir) / f"perf_{uuid.uuid4().hex[:8]}"
        test_dir.mkdir(parents=True, exist_ok=True)

        try:
            # Criar script de benchmark
            benchmark_code = f'''
import time
import statistics

# Codigo a testar
{code}

# Benchmark simples
times = []
for i in range({iterations}):
    start = time.perf_counter()
    # Executar codigo (se houver funcao main ou chamada)
    if 'main' in dir():
        main()
    end = time.perf_counter()
    times.append((end - start) * 1000)  # ms

print(f"MIN:{{min(times):.3f}}")
print(f"MAX:{{max(times):.3f}}")
print(f"AVG:{{statistics.mean(times):.3f}}")
print(f"MEDIAN:{{statistics.median(times):.3f}}")
print(f"STDEV:{{statistics.stdev(times) if len(times) > 1 else 0:.3f}}")
'''
            benchmark_file = test_dir / "benchmark.py"
            benchmark_file.write_text(benchmark_code, encoding="utf-8")

            # Executar
            result = subprocess.run(
                ["python", str(benchmark_file)],
                capture_output=True,
                text=True,
                timeout=120
            )

            if result.returncode == 0:
                output = result.stdout
                metrics = {}
                for line in output.split('\n'):
                    if ':' in line:
                        key, value = line.split(':', 1)
                        metrics[key.lower()] = float(value)
                return {
                    "success": True,
                    "iterations": iterations,
                    **metrics
                }
            else:
                return {
                    "success": False,
                    "error": result.stderr[:500]
                }

        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
        finally:
            try:
                shutil.rmtree(test_dir)
            except:
                pass


class ABTestManager:
    """
    Gerenciador de testes A/B para codigo gerado.

    Permite criar variantes de implementacao, executar testes,
    comparar metricas e selecionar a melhor variante.
    """

    def __init__(self, db_session=None):
        self.db = db_session
        self.test_runner = TestRunner()
        self._variants_cache = {}

    def create_ab_test(
        self,
        story_id: str,
        title: str,
        description: str,
        num_variants: int = 3,
        approaches: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Cria um novo teste A/B para uma story.

        Args:
            story_id: ID da story
            title: Titulo do teste
            description: Descricao do que sera testado
            num_variants: Numero de variantes (2-5)
            approaches: Lista de abordagens especificas

        Returns:
            Dados do teste criado
        """
        num_variants = max(2, min(5, num_variants))

        # Abordagens padrao se nao especificadas
        if not approaches:
            default_approaches = [
                VariantApproach.SIMPLE.value,
                VariantApproach.OPTIMIZED.value,
                VariantApproach.ROBUST.value,
                VariantApproach.MODULAR.value,
                VariantApproach.FUNCTIONAL.value
            ]
            approaches = default_approaches[:num_variants]

        test_id = f"ABT-{uuid.uuid4().hex[:8].upper()}"

        # Criar variantes
        variants = []
        for i, approach in enumerate(approaches[:num_variants]):
            variant = {
                "variant_id": f"{test_id}-V{chr(65 + i)}",  # A, B, C...
                "approach": approach,
                "status": VariantStatus.PENDING.value,
                "code": None,
                "metrics": {},
                "test_results": {},
                "score": 0,
                "created_at": datetime.utcnow().isoformat()
            }
            variants.append(variant)

        ab_test = {
            "test_id": test_id,
            "story_id": story_id,
            "title": title,
            "description": description,
            "status": ABTestStatus.PENDING.value,
            "variants": variants,
            "winner_id": None,
            "recommendation": None,
            "created_at": datetime.utcnow().isoformat(),
            "updated_at": datetime.utcnow().isoformat(),
            "completed_at": None
        }

        self._variants_cache[test_id] = ab_test

        # Salvar no banco se disponivel
        if self.db:
            self._save_to_db(ab_test)

        return ab_test

    def generate_variant_code(
        self,
        test_id: str,
        variant_id: str,
        base_requirements: str,
        tech_stack: str = "python"
    ) -> Dict[str, Any]:
        """
        Gera codigo para uma variante usando Claude AI.

        Args:
            test_id: ID do teste A/B
            variant_id: ID da variante
            base_requirements: Requisitos do codigo
            tech_stack: Stack tecnologica

        Returns:
            Dados da variante com codigo gerado
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        variant = None
        for v in ab_test["variants"]:
            if v["variant_id"] == variant_id:
                variant = v
                break

        if not variant:
            return {"error": "Variant not found"}

        variant["status"] = VariantStatus.GENERATING.value

        # Construir prompt baseado na abordagem
        approach_prompts = {
            VariantApproach.SIMPLE.value: """
                Implemente de forma SIMPLES e DIRETA.
                - Codigo facil de entender
                - Poucos niveis de abstracao
                - Nomes descritivos
                - Evite over-engineering
            """,
            VariantApproach.OPTIMIZED.value: """
                Implemente com foco em PERFORMANCE.
                - Algoritmos eficientes
                - Minimize operacoes custosas
                - Use estruturas de dados apropriadas
                - Considere complexidade O(n)
            """,
            VariantApproach.ROBUST.value: """
                Implemente com foco em ROBUSTEZ.
                - Tratamento completo de erros
                - Validacoes de entrada
                - Logging adequado
                - Recuperacao de falhas
            """,
            VariantApproach.MODULAR.value: """
                Implemente com alta MODULARIDADE.
                - Funcoes pequenas e focadas
                - Alta coesao, baixo acoplamento
                - Facil de testar unitariamente
                - Interfaces bem definidas
            """,
            VariantApproach.FUNCTIONAL.value: """
                Implemente em estilo FUNCIONAL.
                - Funcoes puras quando possivel
                - Imutabilidade
                - Composicao de funcoes
                - Evite efeitos colaterais
            """,
            VariantApproach.OOP.value: """
                Implemente usando ORIENTACAO A OBJETOS.
                - Classes bem estruturadas
                - Encapsulamento
                - Heranca quando apropriado
                - Principios SOLID
            """
        }

        approach_instruction = approach_prompts.get(
            variant["approach"],
            approach_prompts[VariantApproach.SIMPLE.value]
        )

        prompt = f"""
        Gere codigo {tech_stack} para implementar:

        {base_requirements}

        ABORDAGEM ({variant["approach"]}):
        {approach_instruction}

        Requisitos:
        1. Codigo completo e funcional
        2. Inclua docstrings/comentarios
        3. Trate casos de erro basicos
        4. Retorne apenas o codigo, sem explicacoes
        """

        # Tentar usar Claude
        try:
            from factory.ai.claude_integration import get_claude_client
            client = get_claude_client()
            response = client.generate(prompt)
            code = self._extract_code(response)
        except:
            # Fallback: codigo template
            code = self._generate_template_code(variant["approach"], base_requirements, tech_stack)

        variant["code"] = code
        variant["status"] = VariantStatus.GENERATED.value
        variant["generated_at"] = datetime.utcnow().isoformat()

        # Calcular metricas de codigo
        variant["metrics"] = CodeMetrics.full_analysis(code)

        ab_test["updated_at"] = datetime.utcnow().isoformat()

        if self.db:
            self._save_to_db(ab_test)

        return variant

    def generate_all_variants(
        self,
        test_id: str,
        base_requirements: str,
        tech_stack: str = "python"
    ) -> Dict[str, Any]:
        """
        Gera codigo para todas as variantes de um teste.

        Args:
            test_id: ID do teste A/B
            base_requirements: Requisitos do codigo
            tech_stack: Stack tecnologica

        Returns:
            Dados do teste com todas as variantes geradas
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        ab_test["status"] = ABTestStatus.GENERATING.value

        for variant in ab_test["variants"]:
            self.generate_variant_code(
                test_id,
                variant["variant_id"],
                base_requirements,
                tech_stack
            )

        ab_test["status"] = ABTestStatus.TESTING.value
        ab_test["updated_at"] = datetime.utcnow().isoformat()

        if self.db:
            self._save_to_db(ab_test)

        return ab_test

    def run_tests_on_variant(
        self,
        test_id: str,
        variant_id: str,
        test_code: str
    ) -> Dict[str, Any]:
        """
        Executa testes em uma variante especifica.

        Args:
            test_id: ID do teste A/B
            variant_id: ID da variante
            test_code: Codigo de testes

        Returns:
            Resultados dos testes
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        variant = None
        for v in ab_test["variants"]:
            if v["variant_id"] == variant_id:
                variant = v
                break

        if not variant:
            return {"error": "Variant not found"}

        if not variant.get("code"):
            return {"error": "Variant has no code generated"}

        variant["status"] = VariantStatus.TESTING.value

        # Executar testes
        test_results = self.test_runner.run_python_tests(
            variant["code"],
            test_code
        )

        variant["test_results"] = test_results
        variant["status"] = VariantStatus.TESTED.value
        variant["tested_at"] = datetime.utcnow().isoformat()

        # Calcular score
        variant["score"] = self._calculate_variant_score(variant)

        ab_test["updated_at"] = datetime.utcnow().isoformat()

        if self.db:
            self._save_to_db(ab_test)

        return variant

    def run_tests_on_all_variants(
        self,
        test_id: str,
        test_code: str
    ) -> Dict[str, Any]:
        """
        Executa testes em todas as variantes.

        Args:
            test_id: ID do teste A/B
            test_code: Codigo de testes

        Returns:
            Dados do teste com resultados
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        for variant in ab_test["variants"]:
            if variant.get("code"):
                self.run_tests_on_variant(test_id, variant["variant_id"], test_code)

        # Gerar recomendacao
        ab_test["recommendation"] = self._generate_recommendation(ab_test)
        ab_test["status"] = ABTestStatus.COMPLETED.value
        ab_test["updated_at"] = datetime.utcnow().isoformat()
        ab_test["completed_at"] = datetime.utcnow().isoformat()

        if self.db:
            self._save_to_db(ab_test)

        return ab_test

    def compare_variants(self, test_id: str) -> Dict[str, Any]:
        """
        Compara todas as variantes de um teste.

        Args:
            test_id: ID do teste A/B

        Returns:
            Comparacao detalhada das variantes
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        comparison = {
            "test_id": test_id,
            "variants": [],
            "best_by_metric": {},
            "overall_winner": None
        }

        metrics_to_compare = [
            "tests_passed",
            "pass_rate",
            "execution_time_ms",
            "code_lines",
            "cyclomatic_complexity",
            "readability_score",
            "score"
        ]

        # Inicializar tracking de melhores
        for metric in metrics_to_compare:
            comparison["best_by_metric"][metric] = {
                "variant_id": None,
                "value": None
            }

        # Processar cada variante
        for variant in ab_test["variants"]:
            variant_data = {
                "variant_id": variant["variant_id"],
                "approach": variant["approach"],
                "status": variant["status"],
                "score": variant.get("score", 0)
            }

            # Adicionar metricas
            metrics = variant.get("metrics", {})
            test_results = variant.get("test_results", {})

            variant_data["metrics"] = {
                "code_lines": metrics.get("code_lines", 0),
                "cyclomatic_complexity": metrics.get("cyclomatic_complexity", 0),
                "readability_score": metrics.get("readability_score", 0),
                "tests_passed": test_results.get("tests_passed", 0),
                "tests_total": test_results.get("tests_total", 0),
                "pass_rate": test_results.get("pass_rate", 0),
                "execution_time_ms": test_results.get("execution_time_ms", 0)
            }

            comparison["variants"].append(variant_data)

            # Atualizar melhores por metrica
            for metric in metrics_to_compare:
                value = variant_data.get("score") if metric == "score" else variant_data["metrics"].get(metric, 0)
                current_best = comparison["best_by_metric"][metric]["value"]

                # Menor e melhor para: complexity, execution_time, code_lines
                lower_is_better = metric in ["cyclomatic_complexity", "execution_time_ms", "code_lines"]

                if current_best is None:
                    comparison["best_by_metric"][metric] = {
                        "variant_id": variant["variant_id"],
                        "value": value
                    }
                elif lower_is_better and value < current_best:
                    comparison["best_by_metric"][metric] = {
                        "variant_id": variant["variant_id"],
                        "value": value
                    }
                elif not lower_is_better and value > current_best:
                    comparison["best_by_metric"][metric] = {
                        "variant_id": variant["variant_id"],
                        "value": value
                    }

        # Determinar vencedor geral
        if comparison["variants"]:
            sorted_variants = sorted(
                comparison["variants"],
                key=lambda x: x.get("score", 0),
                reverse=True
            )
            comparison["overall_winner"] = sorted_variants[0]["variant_id"]

        return comparison

    def select_winner(
        self,
        test_id: str,
        variant_id: str,
        reason: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Seleciona manualmente o vencedor de um teste A/B.

        Args:
            test_id: ID do teste A/B
            variant_id: ID da variante vencedora
            reason: Justificativa da selecao

        Returns:
            Dados do teste atualizado
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        # Atualizar status das variantes
        for variant in ab_test["variants"]:
            if variant["variant_id"] == variant_id:
                variant["status"] = VariantStatus.WINNER.value
            else:
                variant["status"] = VariantStatus.DISCARDED.value

        ab_test["winner_id"] = variant_id
        ab_test["status"] = ABTestStatus.WINNER_SELECTED.value
        ab_test["selection_reason"] = reason
        ab_test["selected_at"] = datetime.utcnow().isoformat()
        ab_test["updated_at"] = datetime.utcnow().isoformat()

        if self.db:
            self._save_to_db(ab_test)

        return ab_test

    def auto_select_winner(self, test_id: str) -> Dict[str, Any]:
        """
        Seleciona automaticamente o vencedor baseado no score.

        Args:
            test_id: ID do teste A/B

        Returns:
            Dados do teste com vencedor selecionado
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return {"error": "Test not found"}

        # Encontrar variante com maior score
        best_variant = None
        best_score = -1

        for variant in ab_test["variants"]:
            if variant.get("score", 0) > best_score:
                best_score = variant["score"]
                best_variant = variant

        if best_variant:
            return self.select_winner(
                test_id,
                best_variant["variant_id"],
                f"Auto-selected: highest score ({best_score:.1f})"
            )

        return {"error": "No valid variants to select"}

    def get_winner_code(self, test_id: str) -> Optional[str]:
        """
        Retorna o codigo da variante vencedora.

        Args:
            test_id: ID do teste A/B

        Returns:
            Codigo da variante vencedora ou None
        """
        ab_test = self._get_test(test_id)
        if not ab_test or not ab_test.get("winner_id"):
            return None

        for variant in ab_test["variants"]:
            if variant["variant_id"] == ab_test["winner_id"]:
                return variant.get("code")

        return None

    def _get_test(self, test_id: str) -> Optional[Dict]:
        """Busca teste no cache ou banco"""
        if test_id in self._variants_cache:
            return self._variants_cache[test_id]

        if self.db:
            return self._load_from_db(test_id)

        return None

    def _calculate_variant_score(self, variant: Dict) -> float:
        """
        Calcula score total de uma variante.
        Pesos:
        - Testes passando: 40%
        - Legibilidade: 25%
        - Complexidade baixa: 20%
        - Codigo conciso: 15%
        """
        metrics = variant.get("metrics", {})
        test_results = variant.get("test_results", {})

        # Testes (40 pontos max)
        pass_rate = test_results.get("pass_rate", 0)
        test_score = (pass_rate / 100) * 40

        # Legibilidade (25 pontos max)
        readability = metrics.get("readability_score", 50)
        readability_score = (readability / 100) * 25

        # Complexidade (20 pontos max) - menor e melhor
        complexity = metrics.get("cyclomatic_complexity", 10)
        if complexity <= 5:
            complexity_score = 20
        elif complexity <= 10:
            complexity_score = 15
        elif complexity <= 20:
            complexity_score = 10
        else:
            complexity_score = 5

        # Concisao (15 pontos max) - codigo nao muito longo nem muito curto
        code_lines = metrics.get("code_lines", 50)
        if 20 <= code_lines <= 100:
            concision_score = 15
        elif 10 <= code_lines <= 200:
            concision_score = 10
        else:
            concision_score = 5

        total = test_score + readability_score + complexity_score + concision_score
        return round(total, 1)

    def _generate_recommendation(self, ab_test: Dict) -> Dict[str, Any]:
        """Gera recomendacao baseada nas metricas"""
        variants = ab_test.get("variants", [])

        if not variants:
            return {"recommendation": "No variants to compare"}

        # Ordenar por score
        sorted_variants = sorted(
            [v for v in variants if v.get("score", 0) > 0],
            key=lambda x: x.get("score", 0),
            reverse=True
        )

        if not sorted_variants:
            return {"recommendation": "No valid variants with scores"}

        winner = sorted_variants[0]
        second = sorted_variants[1] if len(sorted_variants) > 1 else None

        # Construir recomendacao
        rec = {
            "winner_id": winner["variant_id"],
            "winner_approach": winner["approach"],
            "winner_score": winner.get("score", 0),
            "confidence": "high" if winner.get("score", 0) > 70 else "medium" if winner.get("score", 0) > 50 else "low",
            "reasons": []
        }

        # Adicionar razoes
        test_results = winner.get("test_results", {})
        metrics = winner.get("metrics", {})

        if test_results.get("pass_rate", 0) == 100:
            rec["reasons"].append("100% dos testes passando")
        elif test_results.get("pass_rate", 0) > 80:
            rec["reasons"].append(f"{test_results.get('pass_rate', 0):.0f}% dos testes passando")

        if metrics.get("readability_score", 0) > 80:
            rec["reasons"].append("Alta legibilidade do codigo")

        if metrics.get("cyclomatic_complexity", 99) < 10:
            rec["reasons"].append("Baixa complexidade ciclomatica")

        if second:
            diff = winner.get("score", 0) - second.get("score", 0)
            rec["margin_over_second"] = round(diff, 1)
            if diff > 20:
                rec["reasons"].append(f"Margem significativa sobre {second['variant_id']} ({diff:.1f} pontos)")

        return rec

    def _extract_code(self, response: str) -> str:
        """Extrai codigo de uma resposta do Claude"""
        # Tentar extrair bloco de codigo
        code_blocks = re.findall(r'```(?:python|javascript|typescript)?\n(.*?)```', response, re.DOTALL)
        if code_blocks:
            return code_blocks[0].strip()

        # Fallback: retornar resposta limpa
        return response.strip()

    def _generate_template_code(self, approach: str, requirements: str, tech_stack: str) -> str:
        """Gera codigo template quando Claude nao esta disponivel"""
        if tech_stack == "python":
            if approach == VariantApproach.SIMPLE.value:
                return f'''"""
Implementacao Simples
Requisitos: {requirements[:100]}
"""

def main():
    """Funcao principal."""
    # Implementacao direta
    result = process_data()
    return result

def process_data():
    """Processa dados de forma simples."""
    data = []
    # TODO: Implementar logica
    return data

if __name__ == "__main__":
    main()
'''
            elif approach == VariantApproach.OPTIMIZED.value:
                return f'''"""
Implementacao Otimizada
Requisitos: {requirements[:100]}
"""
from functools import lru_cache
from typing import List, Dict

@lru_cache(maxsize=1000)
def optimized_lookup(key: str) -> Dict:
    """Busca otimizada com cache."""
    return {{"key": key, "value": None}}

def batch_process(items: List) -> List:
    """Processamento em lote para melhor performance."""
    return [process_item(item) for item in items]

def process_item(item):
    """Processa item individual."""
    return item

def main():
    """Funcao principal."""
    data = batch_process([])
    return data

if __name__ == "__main__":
    main()
'''
            elif approach == VariantApproach.ROBUST.value:
                return f'''"""
Implementacao Robusta
Requisitos: {requirements[:100]}
"""
import logging
from typing import Optional, Any

logger = logging.getLogger(__name__)

class ProcessingError(Exception):
    """Erro de processamento."""
    pass

def safe_process(data: Any) -> Optional[Any]:
    """Processa dados com tratamento de erros."""
    try:
        if data is None:
            logger.warning("Dados nulos recebidos")
            return None

        result = validate_and_process(data)
        return result

    except ValueError as e:
        logger.error(f"Erro de validacao: {{e}}")
        raise ProcessingError(f"Dados invalidos: {{e}}")
    except Exception as e:
        logger.exception(f"Erro inesperado: {{e}}")
        raise ProcessingError(f"Erro ao processar: {{e}}")

def validate_and_process(data: Any) -> Any:
    """Valida e processa dados."""
    # Validacao
    if not data:
        raise ValueError("Dados vazios")

    # Processamento
    return data

def main():
    """Funcao principal."""
    try:
        result = safe_process({{}})
        return result
    except ProcessingError as e:
        logger.error(f"Falha no processamento: {{e}}")
        return None

if __name__ == "__main__":
    main()
'''
            else:
                return f'''"""
Implementacao Modular
Requisitos: {requirements[:100]}
"""
from typing import List, Any
from dataclasses import dataclass

@dataclass
class DataItem:
    """Modelo de dados."""
    id: str
    value: Any

class DataProcessor:
    """Processador de dados modular."""

    def __init__(self):
        self.items: List[DataItem] = []

    def add(self, item: DataItem) -> None:
        """Adiciona item."""
        self.items.append(item)

    def process(self) -> List[Any]:
        """Processa todos os itens."""
        return [self._process_item(item) for item in self.items]

    def _process_item(self, item: DataItem) -> Any:
        """Processa um item."""
        return item.value

def main():
    """Funcao principal."""
    processor = DataProcessor()
    processor.add(DataItem("1", "value"))
    return processor.process()

if __name__ == "__main__":
    main()
'''

        return f"// TODO: Implement for {tech_stack}\n// Requirements: {requirements}"

    def _save_to_db(self, ab_test: Dict) -> None:
        """Salva teste no banco de dados"""
        if not self.db:
            return

        try:
            from factory.database.models import ABTest, ABTestVariant

            # Buscar ou criar teste
            test = self.db.query(ABTest).filter(
                ABTest.test_id == ab_test["test_id"]
            ).first()

            if not test:
                test = ABTest(
                    test_id=ab_test["test_id"],
                    story_id=ab_test["story_id"],
                    title=ab_test["title"],
                    description=ab_test["description"]
                )
                self.db.add(test)

            test.status = ab_test["status"]
            test.winner_id = ab_test.get("winner_id")
            test.recommendation = ab_test.get("recommendation")
            test.updated_at = datetime.utcnow()

            if ab_test.get("completed_at"):
                test.completed_at = datetime.fromisoformat(ab_test["completed_at"])

            # Atualizar variantes
            for variant_data in ab_test.get("variants", []):
                variant = self.db.query(ABTestVariant).filter(
                    ABTestVariant.variant_id == variant_data["variant_id"]
                ).first()

                if not variant:
                    variant = ABTestVariant(
                        variant_id=variant_data["variant_id"],
                        test_id=ab_test["test_id"],
                        approach=variant_data["approach"]
                    )
                    self.db.add(variant)

                variant.status = variant_data["status"]
                variant.code = variant_data.get("code")
                variant.metrics = variant_data.get("metrics", {})
                variant.test_results = variant_data.get("test_results", {})
                variant.score = variant_data.get("score", 0)
                variant.updated_at = datetime.utcnow()

            self.db.commit()
        except Exception as e:
            self.db.rollback()
            print(f"Error saving AB test to DB: {e}")

    def allocate_user(self, test_id: str, user_id: str) -> Optional[str]:
        """
        Allocate a user to a variant in an A/B test.

        Uses consistent hashing to ensure the same user always gets the same variant.

        Args:
            test_id: ID of the A/B test
            user_id: ID of the user to allocate

        Returns:
            The variant_id the user is allocated to, or None if test not found
        """
        ab_test = self._get_test(test_id)
        if not ab_test:
            return None

        variants = ab_test.get("variants", [])
        if not variants:
            return None

        # Use consistent hashing based on user_id and test_id
        hash_input = f"{test_id}:{user_id}"
        hash_value = hash(hash_input)
        variant_index = abs(hash_value) % len(variants)

        return variants[variant_index]["variant_id"]

    def _load_from_db(self, test_id: str) -> Optional[Dict]:
        """Carrega teste do banco de dados"""
        if not self.db:
            return None

        try:
            from factory.database.models import ABTest, ABTestVariant

            test = self.db.query(ABTest).filter(
                ABTest.test_id == test_id
            ).first()

            if not test:
                return None

            variants = self.db.query(ABTestVariant).filter(
                ABTestVariant.test_id == test_id
            ).all()

            ab_test = {
                "test_id": test.test_id,
                "story_id": test.story_id,
                "title": test.title,
                "description": test.description,
                "status": test.status,
                "winner_id": test.winner_id,
                "recommendation": test.recommendation,
                "created_at": test.created_at.isoformat() if test.created_at else None,
                "updated_at": test.updated_at.isoformat() if test.updated_at else None,
                "completed_at": test.completed_at.isoformat() if test.completed_at else None,
                "variants": []
            }

            for v in variants:
                ab_test["variants"].append({
                    "variant_id": v.variant_id,
                    "approach": v.approach,
                    "status": v.status,
                    "code": v.code,
                    "metrics": v.metrics or {},
                    "test_results": v.test_results or {},
                    "score": v.score or 0
                })

            # Cachear
            self._variants_cache[test_id] = ab_test

            return ab_test
        except Exception as e:
            print(f"Error loading AB test from DB: {e}")
            return None


# =============================================================================
# FUNCOES UTILITARIAS
# =============================================================================

def create_ab_test(story_id: str, title: str, description: str, num_variants: int = 3, db=None) -> Dict:
    """
    Cria um novo teste A/B para uma story.

    Exemplo:
        >>> test = create_ab_test("STR-0001", "Login Feature", "Implementar login", 3)
        >>> print(test["test_id"])  # ABT-XXXXXXXX
    """
    manager = ABTestManager(db)
    return manager.create_ab_test(story_id, title, description, num_variants)


def run_ab_test(test_id: str, requirements: str, test_code: str, db=None) -> Dict:
    """
    Executa um teste A/B completo: gera variantes, executa testes, compara.

    Exemplo:
        >>> result = run_ab_test("ABT-XXXXXXXX", "Criar funcao de soma", "def test_soma(): assert soma(1,2) == 3")
        >>> print(result["recommendation"]["winner_id"])
    """
    manager = ABTestManager(db)

    # Gerar todas as variantes
    manager.generate_all_variants(test_id, requirements)

    # Executar testes
    manager.run_tests_on_all_variants(test_id, test_code)

    # Retornar comparacao
    return manager.compare_variants(test_id)


def get_ab_test_status(test_id: str, db=None) -> Optional[Dict]:
    """
    Retorna status de um teste A/B.

    Exemplo:
        >>> status = get_ab_test_status("ABT-XXXXXXXX")
        >>> print(status["status"])  # completed
    """
    manager = ABTestManager(db)
    return manager._get_test(test_id)


# =============================================================================
# CLASSES DE MODELO PARA COMPATIBILIDADE
# =============================================================================

class Variant:
    """
    Represents a variant in an A/B test.

    This class provides a structured representation of a test variant
    with its code, metrics, and test results.
    """

    def __init__(
        self,
        variant_id: str,
        approach: str = VariantApproach.SIMPLE.value,
        status: str = VariantStatus.PENDING.value,
        code: Optional[str] = None,
        metrics: Optional[Dict[str, Any]] = None,
        test_results: Optional[Dict[str, Any]] = None,
        score: float = 0.0
    ):
        self.variant_id = variant_id
        self.approach = approach
        self.status = status
        self.code = code
        self.metrics = metrics or {}
        self.test_results = test_results or {}
        self.score = score
        self.created_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()

    def to_dict(self) -> Dict[str, Any]:
        """Convert variant to dictionary representation."""
        return {
            "variant_id": self.variant_id,
            "approach": self.approach,
            "status": self.status,
            "code": self.code,
            "metrics": self.metrics,
            "test_results": self.test_results,
            "score": self.score,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Variant":
        """Create a Variant instance from a dictionary."""
        variant = cls(
            variant_id=data.get("variant_id", f"V-{uuid.uuid4().hex[:8]}"),
            approach=data.get("approach", VariantApproach.SIMPLE.value),
            status=data.get("status", VariantStatus.PENDING.value),
            code=data.get("code"),
            metrics=data.get("metrics", {}),
            test_results=data.get("test_results", {}),
            score=data.get("score", 0.0)
        )
        return variant


class ABTest:
    """
    Represents an A/B test for code variants.

    This class manages the lifecycle of an A/B test, including
    creating variants, running tests, and selecting a winner.
    """

    def __init__(
        self,
        test_id: Optional[str] = None,
        story_id: Optional[str] = None,
        title: str = "",
        description: str = "",
        status: str = ABTestStatus.PENDING.value,
        variants: Optional[List[Variant]] = None,
        winner_id: Optional[str] = None,
        recommendation: Optional[Dict[str, Any]] = None
    ):
        self.test_id = test_id or f"ABT-{uuid.uuid4().hex[:8].upper()}"
        self.story_id = story_id
        self.title = title
        self.description = description
        self.status = status
        self.variants = variants or []
        self.winner_id = winner_id
        self.recommendation = recommendation
        self.created_at = datetime.utcnow()
        self.updated_at = datetime.utcnow()
        self.completed_at: Optional[datetime] = None

    def add_variant(self, variant: Variant) -> None:
        """Add a variant to the test."""
        self.variants.append(variant)
        self.updated_at = datetime.utcnow()

    def get_variant(self, variant_id: str) -> Optional[Variant]:
        """Get a variant by ID."""
        for variant in self.variants:
            if variant.variant_id == variant_id:
                return variant
        return None

    def select_winner(self, variant_id: str) -> bool:
        """Select a winner variant."""
        variant = self.get_variant(variant_id)
        if not variant:
            return False

        self.winner_id = variant_id
        self.status = ABTestStatus.WINNER_SELECTED.value
        variant.status = VariantStatus.WINNER.value

        for v in self.variants:
            if v.variant_id != variant_id:
                v.status = VariantStatus.DISCARDED.value

        self.updated_at = datetime.utcnow()
        return True

    def to_dict(self) -> Dict[str, Any]:
        """Convert ABTest to dictionary representation."""
        return {
            "test_id": self.test_id,
            "story_id": self.story_id,
            "title": self.title,
            "description": self.description,
            "status": self.status,
            "variants": [v.to_dict() if isinstance(v, Variant) else v for v in self.variants],
            "winner_id": self.winner_id,
            "recommendation": self.recommendation,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ABTest":
        """Create an ABTest instance from a dictionary."""
        variants = []
        for v_data in data.get("variants", []):
            if isinstance(v_data, dict):
                variants.append(Variant.from_dict(v_data))
            elif isinstance(v_data, Variant):
                variants.append(v_data)

        test = cls(
            test_id=data.get("test_id"),
            story_id=data.get("story_id"),
            title=data.get("title", ""),
            description=data.get("description", ""),
            status=data.get("status", ABTestStatus.PENDING.value),
            variants=variants,
            winner_id=data.get("winner_id"),
            recommendation=data.get("recommendation")
        )
        return test

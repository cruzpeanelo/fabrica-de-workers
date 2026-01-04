"""
Autonomous Loop - Loop Autonomo de Geracao de Codigo v4.0
Nova Arquitetura MVP - Plataforma E

Integrado com Claude API para geracao e auto-fix de codigo.
Issue #198: Sandbox isolation para workers

Implementa o ciclo:
1. Parse requirements
2. Generate code (usando Claude API)
3. Lint (fix if fails usando Claude)
4. Type check (fix if fails usando Claude)
5. Test (fix if fails usando Claude)
6. Security scan
7. Commit to git
8. Done or fail after 5 attempts
"""
import asyncio
import subprocess
import os
import json
import shutil
import logging
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, Any, List, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from dotenv import load_dotenv
load_dotenv()

PROJECTS_DIR = Path(os.getenv("PROJECTS_DIR", "projects"))


@dataclass
class LoopConfig:
    """Configuracao do loop autonomo"""
    max_attempts: int = 5  # Max tentativas por step
    lint_enabled: bool = True
    type_check_enabled: bool = True
    test_enabled: bool = True
    security_scan_enabled: bool = True
    auto_commit: bool = True
    project_base_dir: Path = PROJECTS_DIR
    # Issue #198: Sandbox configuration
    sandbox_enabled: bool = True  # Enable sandbox isolation
    sandbox_timeout: int = 120  # Sandbox timeout in seconds
    sandbox_memory_limit: str = "512m"  # Memory limit for sandbox
    sandbox_cpu_limit: float = 1.0  # CPU limit (1 core)
    sandbox_network: bool = False  # Allow network in sandbox


@dataclass
class StepResult:
    """Resultado de um step"""
    success: bool
    message: str
    output: str = ""
    errors: List[str] = None
    can_fix: bool = False
    fix_suggestions: List[str] = None


class AutonomousLoop:
    """
    Loop Autonomo de Geracao de Codigo

    Executa o ciclo completo de geracao, validacao e correcao de codigo
    de forma autonoma, com auto-healing em caso de erros.
    """

    def __init__(self, config: LoopConfig = None):
        self.config = config or LoopConfig()
        self._current_job_id: Optional[str] = None
        self._project_path: Optional[Path] = None
        self._sandbox_executor = None
        self._resource_limiter = None
        self._claude_client = None  # Inicializado em run()
        self._model = None
        self._on_step_update = None
        self._init_sandbox()

    def _init_sandbox(self):
        """Issue #198: Initialize sandbox and resource limiter"""
        if self.config.sandbox_enabled:
            try:
                from factory.core.sandbox_executor import (
                    SandboxExecutor, SandboxConfig, get_sandbox_executor
                )
                from factory.core.resource_limiter import (
                    ResourceLimiter, ResourceLimits, get_resource_limiter
                )

                # Configure sandbox
                sandbox_config = SandboxConfig(
                    cpu_limit=self.config.sandbox_cpu_limit,
                    memory_limit=self.config.sandbox_memory_limit,
                    timeout_seconds=self.config.sandbox_timeout,
                    network_mode="bridge" if self.config.sandbox_network else "none"
                )
                self._sandbox_executor = get_sandbox_executor(sandbox_config)

                # Configure resource limiter
                resource_limits = ResourceLimits(
                    max_execution_time=self.config.sandbox_timeout,
                    max_memory_mb=int(self.config.sandbox_memory_limit.replace("m", "")),
                    max_cpu_percent=self.config.sandbox_cpu_limit * 100
                )
                self._resource_limiter = get_resource_limiter(resource_limits)

                logger.info("[AutonomousLoop] Sandbox and resource limiter initialized")
            except ImportError as e:
                logger.warning(f"[AutonomousLoop] Sandbox modules not available: {e}")
                self._sandbox_executor = None
                self._resource_limiter = None

    async def _execute_in_sandbox(
        self,
        code: str,
        language: str = "python",
        files: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Issue #198: Execute code in sandboxed environment

        Args:
            code: Source code to execute
            language: Programming language
            files: Additional files to include

        Returns:
            Dict with success, stdout, stderr, execution_time
        """
        if not self._sandbox_executor:
            # Fallback to direct execution (less secure)
            return await self._execute_direct(code, language)

        try:
            # Start resource tracking
            task_id = f"{self._current_job_id}-{datetime.now().timestamp()}"
            if self._resource_limiter:
                if not self._resource_limiter.start_task(task_id):
                    return {
                        "success": False,
                        "stdout": "",
                        "stderr": "Rate limit exceeded",
                        "execution_time": 0
                    }

            # Execute in sandbox
            result = await self._sandbox_executor.execute(
                code=code,
                language=language,
                files=files
            )

            # End resource tracking
            if self._resource_limiter:
                usage = self._resource_limiter.end_task(task_id)
                logger.info(f"[Sandbox] Task {task_id} resource usage: {usage}")

            return {
                "success": result.success,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "execution_time": result.execution_time,
                "container_id": result.container_id,
                "error": result.error
            }

        except Exception as e:
            logger.error(f"[Sandbox] Execution error: {e}")
            return {
                "success": False,
                "stdout": "",
                "stderr": str(e),
                "execution_time": 0,
                "error": "SANDBOX_ERROR"
            }

    async def _execute_direct(self, code: str, language: str) -> Dict[str, Any]:
        """Fallback direct execution when sandbox unavailable"""
        import tempfile

        try:
            with tempfile.TemporaryDirectory(prefix="direct_exec_") as tmpdir:
                if language in ("python", "python3"):
                    code_file = Path(tmpdir) / "main.py"
                    code_file.write_text(code, encoding="utf-8")
                    cmd = ["python", str(code_file)]
                elif language in ("node", "javascript"):
                    code_file = Path(tmpdir) / "main.js"
                    code_file.write_text(code, encoding="utf-8")
                    cmd = ["node", str(code_file)]
                else:
                    return {
                        "success": False,
                        "stdout": "",
                        "stderr": f"Unsupported language: {language}",
                        "execution_time": 0
                    }

                start = datetime.now()
                proc = await asyncio.create_subprocess_exec(
                    *cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout, stderr = await asyncio.wait_for(
                    proc.communicate(),
                    timeout=self.config.sandbox_timeout
                )
                execution_time = (datetime.now() - start).total_seconds()

                return {
                    "success": proc.returncode == 0,
                    "stdout": stdout.decode("utf-8", errors="replace"),
                    "stderr": stderr.decode("utf-8", errors="replace") + "\n[WARNING: Direct execution - no sandbox]",
                    "execution_time": execution_time
                }
        except asyncio.TimeoutError:
            return {
                "success": False,
                "stdout": "",
                "stderr": f"Execution timed out after {self.config.sandbox_timeout}s",
                "execution_time": self.config.sandbox_timeout,
                "error": "TIMEOUT"
            }
        except Exception as e:
            return {
                "success": False,
                "stdout": "",
                "stderr": str(e),
                "execution_time": 0,
                "error": "EXECUTION_ERROR"
            }

    async def _run_sandboxed_tests(self, test_file: Path) -> StepResult:
        """Issue #198: Run tests in sandboxed environment"""
        if not self._sandbox_executor:
            return await self._run_direct_tests(test_file)

        try:
            # Read test file
            test_code = test_file.read_text(encoding="utf-8")

            # Read main code files
            files = {}
            for py_file in self._project_path.glob("**/*.py"):
                if py_file != test_file and "__pycache__" not in str(py_file):
                    rel_path = py_file.relative_to(self._project_path)
                    files[str(rel_path)] = py_file.read_text(encoding="utf-8")

            # Execute tests in sandbox
            result = await self._execute_in_sandbox(
                code=test_code,
                language="python",
                files=files
            )

            if result["success"]:
                return StepResult(
                    success=True,
                    message="Tests passed (sandboxed)",
                    output=result["stdout"]
                )
            else:
                return StepResult(
                    success=False,
                    message="Tests failed (sandboxed)",
                    output=result["stdout"] + result["stderr"],
                    errors=result["stderr"].split("\n"),
                    can_fix=True
                )

        except Exception as e:
            return StepResult(
                success=False,
                message=f"Sandboxed test execution failed: {e}",
                errors=[str(e)],
                can_fix=True
            )

    async def _run_direct_tests(self, test_file: Path) -> StepResult:
        """Fallback: run tests directly"""
        try:
            result = subprocess.run(
                ["pytest", str(test_file), "-v", "--tb=short"],
                capture_output=True,
                text=True,
                timeout=300,
                cwd=str(self._project_path)
            )
            if result.returncode == 0:
                return StepResult(
                    success=True,
                    message="Tests passed",
                    output=result.stdout
                )
            else:
                return StepResult(
                    success=False,
                    message="Tests failed",
                    output=result.stdout + result.stderr,
                    errors=result.stderr.split("\n"),
                    can_fix=True
                )
        except Exception as e:
            return StepResult(
                success=False,
                message=f"Test execution failed: {e}",
                errors=[str(e)],
                can_fix=True
            )

    # =========================================================================
    # MAIN LOOP
    # =========================================================================

    async def run(
        self,
        job_id: str,
        description: str = None,
        tech_stack: str = None,
        features: list = None,
        claude_client = None,
        model: str = None,
        on_step_update: Callable = None
    ) -> Dict[str, Any]:
        """
        Executa o loop autonomo completo para um job

        Args:
            job_id: ID do job a processar
            description: Descricao do projeto (se nao usar DB)
            tech_stack: Stack tecnologica
            features: Lista de features
            claude_client: Cliente Anthropic (opcional)
            model: Modelo Claude a usar (opcional)
            on_step_update: Callback para atualizacao de status

        Returns:
            Resultado do processamento
        """
        self._current_job_id = job_id
        self._claude_client = claude_client
        self._model = model or "claude-sonnet-4-20250514"
        self._on_step_update = on_step_update

        # Modo standalone (sem DB)
        if description:
            return await self._run_standalone(
                job_id, description, tech_stack, features
            )

        # Modo com DB
        from factory.database.connection import SessionLocal
        from factory.database.repositories import JobRepository, FailureHistoryRepository
        from factory.database.models import JobStatus, JobStep

        db = SessionLocal()
        try:
            job_repo = JobRepository(db)
            failure_repo = FailureHistoryRepository(db)

            # Buscar job
            job = job_repo.get_by_id(job_id)
            if not job:
                return {"success": False, "error": f"Job {job_id} not found"}

            # Criar diretorio do projeto
            project_name = self._generate_project_name(job.description)
            self._project_path = self.config.project_base_dir / project_name
            self._project_path.mkdir(parents=True, exist_ok=True)

            # Atualizar job com output_path
            job_repo.update(job_id, {"output_path": str(self._project_path)})

            # Definir steps do loop
            steps = [
                (JobStep.PARSING, self._step_parse),
                (JobStep.GENERATING, self._step_generate),
            ]

            if self.config.lint_enabled:
                steps.append((JobStep.LINTING, self._step_lint))

            if self.config.type_check_enabled:
                steps.append((JobStep.TYPE_CHECKING, self._step_type_check))

            if self.config.test_enabled:
                steps.append((JobStep.TESTING, self._step_test))

            if self.config.security_scan_enabled:
                steps.append((JobStep.SECURITY_SCAN, self._step_security))

            if self.config.auto_commit:
                steps.append((JobStep.COMMITTING, self._step_commit))

            # Executar steps
            for step_enum, step_fn in steps:
                step_name = step_enum.value

                # Atualizar status
                job_repo.update_status(job_id, JobStatus.RUNNING.value, step_name)

                # Reset attempt counter for this step
                job = job_repo.get_by_id(job_id)
                job.current_attempt = 0
                db.commit()

                # Executar step com retry
                for attempt in range(1, self.config.max_attempts + 1):
                    job_repo.increment_attempt(job_id)

                    self._log(f"Step {step_name} - Attempt {attempt}/{self.config.max_attempts}")

                    result = await step_fn(job)

                    # Logar resultado
                    job_repo.add_step_log(job_id, step_name, result.message, result.success)

                    if result.success:
                        self._log(f"Step {step_name} completed successfully")
                        break

                    # Registrar falha
                    failure_repo.create({
                        "job_id": job_id,
                        "project_id": job.project_id,
                        "step": step_name,
                        "attempt": attempt,
                        "error_message": result.message,
                        "error_type": "step_failure",
                        "step_output": {"output": result.output, "errors": result.errors}
                    })

                    # Verificar se pode corrigir
                    if result.can_fix and attempt < self.config.max_attempts:
                        self._log(f"Attempting auto-fix for {step_name}")
                        # Tentar corrigir (proximo loop tentara novamente)
                        await self._attempt_fix(job, step_name, result)
                    else:
                        # Falha definitiva apos max attempts
                        if attempt >= self.config.max_attempts:
                            job_repo.update_status(job_id, JobStatus.FAILED.value, JobStep.FAILED.value)
                            job_repo.update(job_id, {
                                "error_message": f"Step {step_name} failed after {self.config.max_attempts} attempts: {result.message}"
                            })
                            return {
                                "success": False,
                                "error": f"Step {step_name} failed",
                                "step": step_name,
                                "attempts": attempt
                            }

            # Loop completado com sucesso
            job_repo.update_status(job_id, JobStatus.COMPLETED.value, JobStep.COMPLETED.value)
            job_repo.update(job_id, {
                "result": {
                    "project_path": str(self._project_path),
                    "completed_at": datetime.utcnow().isoformat()
                },
                "progress": 100.0
            })

            return {
                "success": True,
                "project_path": str(self._project_path),
                "job_id": job_id
            }

        finally:
            db.close()

    # =========================================================================
    # STEPS
    # =========================================================================

    async def _step_parse(self, job) -> StepResult:
        """
        Step 1: Parse requirements
        Analisa a descricao e extrai requisitos estruturados
        """
        try:
            description = job.description
            tech_stack = job.tech_stack
            features = job.features or []

            # Criar estrutura de requisitos
            requirements = {
                "description": description,
                "tech_stack": self._parse_tech_stack(tech_stack),
                "features": features,
                "project_structure": self._determine_project_structure(tech_stack)
            }

            # Salvar requirements.json no projeto
            requirements_file = self._project_path / "requirements.json"
            with open(requirements_file, "w", encoding="utf-8") as f:
                json.dump(requirements, f, indent=2)

            return StepResult(
                success=True,
                message="Requirements parsed successfully",
                output=json.dumps(requirements, indent=2)
            )

        except Exception as e:
            return StepResult(
                success=False,
                message=f"Failed to parse requirements: {str(e)}",
                errors=[str(e)]
            )

    async def _step_generate(self, job) -> StepResult:
        """
        Step 2: Generate code
        Gera o codigo base do projeto
        """
        try:
            # Ler requirements
            requirements_file = self._project_path / "requirements.json"
            with open(requirements_file, "r", encoding="utf-8") as f:
                requirements = json.load(f)

            tech_stack = requirements.get("tech_stack", {})
            structure = requirements.get("project_structure", {})

            # Gerar estrutura de pastas
            self._create_project_structure(structure)

            # Gerar arquivos base dependendo do tech_stack
            # Verifica tanto o dict quanto a estrutura do projeto
            tech_stack_str = str(tech_stack).lower()
            structure_type = structure.get("type", "").lower() if isinstance(structure, dict) else ""
            backend = tech_stack.get("backend", "").lower() if isinstance(tech_stack, dict) else ""

            is_python = ("fastapi" in tech_stack_str or "python" in tech_stack_str or
                         "flask" in tech_stack_str or "django" in tech_stack_str or
                         structure_type == "python")
            is_node = ("react" in tech_stack_str or "node" in tech_stack_str or
                       "express" in tech_stack_str or "express" in backend or
                       structure_type == "node")

            if is_python:
                self._generate_python_project(requirements)
            elif is_node:
                self._generate_node_project(requirements)
            else:
                self._generate_generic_project(requirements)

            return StepResult(
                success=True,
                message="Code generated successfully",
                output=f"Project created at {self._project_path}"
            )

        except Exception as e:
            return StepResult(
                success=False,
                message=f"Failed to generate code: {str(e)}",
                errors=[str(e)],
                can_fix=True
            )

    async def _step_lint(self, job) -> StepResult:
        """
        Step 3: Lint code
        Executa linter (ruff para Python, eslint para JS)
        """
        try:
            # Detectar tipo de projeto
            if (self._project_path / "pyproject.toml").exists() or \
               (self._project_path / "requirements.txt").exists():
                # Python - usar ruff
                result = subprocess.run(
                    ["ruff", "check", str(self._project_path), "--fix"],
                    capture_output=True,
                    text=True,
                    timeout=60
                )
            elif (self._project_path / "package.json").exists():
                # Node - usar eslint
                result = subprocess.run(
                    ["npx", "eslint", str(self._project_path), "--fix"],
                    capture_output=True,
                    text=True,
                    timeout=60
                )
            else:
                return StepResult(success=True, message="No linter configured for this project type")

            if result.returncode == 0:
                return StepResult(
                    success=True,
                    message="Lint passed",
                    output=result.stdout
                )
            else:
                return StepResult(
                    success=False,
                    message="Lint errors found",
                    output=result.stdout + result.stderr,
                    errors=result.stderr.split("\n"),
                    can_fix=True
                )

        except FileNotFoundError:
            # Linter nao instalado - skip
            return StepResult(success=True, message="Linter not installed, skipping")
        except Exception as e:
            return StepResult(
                success=False,
                message=f"Lint failed: {str(e)}",
                errors=[str(e)],
                can_fix=True
            )

    async def _step_type_check(self, job) -> StepResult:
        """
        Step 4: Type check
        Executa verificacao de tipos (mypy para Python, tsc para TS)
        """
        try:
            if (self._project_path / "pyproject.toml").exists():
                # Python - usar mypy
                result = subprocess.run(
                    ["mypy", str(self._project_path), "--ignore-missing-imports"],
                    capture_output=True,
                    text=True,
                    timeout=120
                )
            elif (self._project_path / "tsconfig.json").exists():
                # TypeScript - usar tsc
                result = subprocess.run(
                    ["npx", "tsc", "--noEmit"],
                    cwd=str(self._project_path),
                    capture_output=True,
                    text=True,
                    timeout=120
                )
            else:
                return StepResult(success=True, message="No type checker configured")

            if result.returncode == 0:
                return StepResult(
                    success=True,
                    message="Type check passed",
                    output=result.stdout
                )
            else:
                return StepResult(
                    success=False,
                    message="Type errors found",
                    output=result.stdout + result.stderr,
                    errors=result.stderr.split("\n"),
                    can_fix=True
                )

        except FileNotFoundError:
            return StepResult(success=True, message="Type checker not installed, skipping")
        except Exception as e:
            return StepResult(
                success=False,
                message=f"Type check failed: {str(e)}",
                errors=[str(e)],
                can_fix=True
            )

    async def _step_test(self, job) -> StepResult:
        """
        Step 5: Run tests
        Executa testes (pytest para Python, jest para JS)
        """
        try:
            if (self._project_path / "pyproject.toml").exists():
                result = subprocess.run(
                    ["pytest", str(self._project_path), "-v", "--tb=short"],
                    capture_output=True,
                    text=True,
                    timeout=300
                )
            elif (self._project_path / "package.json").exists():
                result = subprocess.run(
                    ["npm", "test"],
                    cwd=str(self._project_path),
                    capture_output=True,
                    text=True,
                    timeout=300
                )
            else:
                return StepResult(success=True, message="No test runner configured")

            if result.returncode == 0:
                return StepResult(
                    success=True,
                    message="Tests passed",
                    output=result.stdout
                )
            else:
                return StepResult(
                    success=False,
                    message="Tests failed",
                    output=result.stdout + result.stderr,
                    errors=result.stderr.split("\n"),
                    can_fix=True
                )

        except FileNotFoundError:
            return StepResult(success=True, message="Test runner not installed, skipping")
        except Exception as e:
            return StepResult(
                success=False,
                message=f"Tests failed: {str(e)}",
                errors=[str(e)],
                can_fix=True
            )

    async def _step_security(self, job) -> StepResult:
        """
        Step 6: Security scan
        Executa scan de seguranca (bandit para Python, npm audit para JS)
        """
        try:
            if (self._project_path / "pyproject.toml").exists():
                result = subprocess.run(
                    ["bandit", "-r", str(self._project_path), "-ll"],
                    capture_output=True,
                    text=True,
                    timeout=120
                )
            elif (self._project_path / "package.json").exists():
                result = subprocess.run(
                    ["npm", "audit", "--audit-level=high"],
                    cwd=str(self._project_path),
                    capture_output=True,
                    text=True,
                    timeout=120
                )
            else:
                return StepResult(success=True, message="No security scanner configured")

            # Bandit retorna 0 mesmo com issues de baixa severidade
            if result.returncode == 0 or "No issues" in result.stdout:
                return StepResult(
                    success=True,
                    message="Security scan passed",
                    output=result.stdout
                )
            else:
                return StepResult(
                    success=False,
                    message="Security issues found",
                    output=result.stdout + result.stderr,
                    errors=result.stderr.split("\n"),
                    can_fix=True
                )

        except FileNotFoundError:
            return StepResult(success=True, message="Security scanner not installed, skipping")
        except Exception as e:
            return StepResult(
                success=False,
                message=f"Security scan failed: {str(e)}",
                errors=[str(e)]
            )

    async def _step_commit(self, job) -> StepResult:
        """
        Step 7: Commit to git
        Inicializa repo git e faz commit inicial
        """
        try:
            # Inicializar git se necessario
            git_dir = self._project_path / ".git"
            if not git_dir.exists():
                subprocess.run(
                    ["git", "init"],
                    cwd=str(self._project_path),
                    capture_output=True,
                    timeout=30
                )

            # Add all files
            subprocess.run(
                ["git", "add", "."],
                cwd=str(self._project_path),
                capture_output=True,
                timeout=30
            )

            # Commit
            result = subprocess.run(
                ["git", "commit", "-m", f"Initial commit - Generated by Agent Factory\n\nJob: {job.job_id}"],
                cwd=str(self._project_path),
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0 or "nothing to commit" in result.stdout.lower():
                return StepResult(
                    success=True,
                    message="Committed to git",
                    output=result.stdout
                )
            else:
                return StepResult(
                    success=False,
                    message="Git commit failed",
                    output=result.stderr,
                    errors=[result.stderr]
                )

        except Exception as e:
            return StepResult(
                success=False,
                message=f"Git commit failed: {str(e)}",
                errors=[str(e)]
            )

    # =========================================================================
    # HELPERS
    # =========================================================================

    def _generate_project_name(self, description: str) -> str:
        """Gera nome de projeto a partir da descricao"""
        # Pegar primeiras palavras e criar slug
        words = description.lower().split()[:3]
        slug = "-".join(words)
        # Remover caracteres especiais
        slug = "".join(c for c in slug if c.isalnum() or c == "-")
        # Adicionar timestamp
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        return f"{slug}-{timestamp}"

    def _parse_tech_stack(self, tech_stack: str) -> Dict[str, str]:
        """Parse tech stack string into structured dict"""
        stack = {}
        if not tech_stack:
            return stack

        tech_stack_lower = tech_stack.lower()

        # Backend
        if "fastapi" in tech_stack_lower:
            stack["backend"] = "fastapi"
        elif "django" in tech_stack_lower:
            stack["backend"] = "django"
        elif "flask" in tech_stack_lower:
            stack["backend"] = "flask"
        elif "express" in tech_stack_lower or "node" in tech_stack_lower:
            stack["backend"] = "express"

        # Frontend
        if "react" in tech_stack_lower:
            stack["frontend"] = "react"
        elif "vue" in tech_stack_lower:
            stack["frontend"] = "vue"
        elif "angular" in tech_stack_lower:
            stack["frontend"] = "angular"

        # Database
        if "postgres" in tech_stack_lower:
            stack["database"] = "postgresql"
        elif "mysql" in tech_stack_lower:
            stack["database"] = "mysql"
        elif "sqlite" in tech_stack_lower:
            stack["database"] = "sqlite"
        elif "mongo" in tech_stack_lower:
            stack["database"] = "mongodb"

        return stack

    def _determine_project_structure(self, tech_stack: str) -> Dict[str, Any]:
        """Determina estrutura de pastas baseado no tech stack"""
        tech_stack_lower = str(tech_stack).lower() if tech_stack else ""

        if "fastapi" in tech_stack_lower or "python" in tech_stack_lower or \
           "flask" in tech_stack_lower or "django" in tech_stack_lower:
            return {
                "type": "python",
                "folders": ["app", "app/api", "app/models", "app/services", "tests"],
                "files": ["main.py", "requirements.txt", "pyproject.toml", "README.md"]
            }
        elif "react" in tech_stack_lower:
            return {
                "type": "react",
                "folders": ["src", "src/components", "src/pages", "public", "tests"],
                "files": ["package.json", "README.md", "tsconfig.json"]
            }
        elif "node" in tech_stack_lower or "express" in tech_stack_lower:
            return {
                "type": "node",
                "folders": ["routes", "middleware", "tests"],
                "files": ["package.json", "index.js", "README.md"]
            }
        else:
            return {
                "type": "generic",
                "folders": ["src", "tests", "docs"],
                "files": ["README.md"]
            }

    def _create_project_structure(self, structure: Dict[str, Any]):
        """Cria estrutura de pastas do projeto"""
        for folder in structure.get("folders", []):
            (self._project_path / folder).mkdir(parents=True, exist_ok=True)

    def _generate_python_project(self, requirements: Dict[str, Any]):
        """Gera projeto Python/FastAPI"""
        description = requirements.get("description", "Generated Project")
        features = requirements.get("features", [])

        # requirements.txt
        reqs = """fastapi>=0.104.0
uvicorn>=0.24.0
sqlalchemy>=2.0.0
pydantic>=2.0.0
python-dotenv>=1.0.0
"""
        (self._project_path / "requirements.txt").write_text(reqs)

        # pyproject.toml
        pyproject = f"""[project]
name = "generated-project"
version = "0.1.0"
description = "{description}"
requires-python = ">=3.10"

[tool.ruff]
line-length = 100

[tool.pytest.ini_options]
testpaths = ["tests"]
"""
        (self._project_path / "pyproject.toml").write_text(pyproject)

        # main.py
        main_py = '''"""
Generated API - Agent Factory MVP
"""
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

app = FastAPI(
    title="Generated API",
    version="0.1.0"
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:9001",
        "http://localhost:9000",
        "http://localhost:8000",
        "http://127.0.0.1:9001",
        "http://127.0.0.1:9000",
        "http://127.0.0.1:8000"
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
async def root():
    return {"message": "API is running", "status": "ok"}


@app.get("/health")
async def health():
    return {"status": "healthy"}


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
'''
        (self._project_path / "main.py").write_text(main_py)

        # README.md
        readme = f"""# Generated Project

{description}

## Features

{chr(10).join(f"- {f}" for f in features)}

## Setup

```bash
pip install -r requirements.txt
python main.py
```

## API Docs

After running, access: http://localhost:8000/docs

---
Generated by Agent Factory MVP
"""
        (self._project_path / "README.md").write_text(readme)

        # tests/__init__.py
        tests_dir = self._project_path / "tests"
        tests_dir.mkdir(exist_ok=True)
        (tests_dir / "__init__.py").write_text("")

        # tests/test_main.py
        test_main = '''"""Tests for main API"""
from fastapi.testclient import TestClient
from main import app

client = TestClient(app)


def test_root():
    response = client.get("/")
    assert response.status_code == 200
    assert response.json()["status"] == "ok"


def test_health():
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json()["status"] == "healthy"
'''
        (tests_dir / "test_main.py").write_text(test_main)

    def _generate_node_project(self, requirements: Dict[str, Any]):
        """Gera projeto Node/React"""
        description = requirements.get("description", "Generated Project")

        # package.json
        package = {
            "name": "generated-project",
            "version": "0.1.0",
            "description": description,
            "scripts": {
                "start": "node index.js",
                "test": "jest"
            },
            "dependencies": {
                "express": "^4.18.0"
            },
            "devDependencies": {
                "jest": "^29.0.0"
            }
        }
        (self._project_path / "package.json").write_text(json.dumps(package, indent=2))

        # index.js
        index_js = '''const express = require("express");
const app = express();
const port = process.env.PORT || 3000;

app.get("/", (req, res) => {
  res.json({ message: "API is running", status: "ok" });
});

app.get("/health", (req, res) => {
  res.json({ status: "healthy" });
});

app.listen(port, () => {
  console.log(`Server running on port ${port}`);
});

module.exports = app;
'''
        (self._project_path / "index.js").write_text(index_js)

        # README.md
        readme = f"""# Generated Project

{description}

## Setup

```bash
npm install
npm start
```

---
Generated by Agent Factory MVP
"""
        (self._project_path / "README.md").write_text(readme)

    def _generate_generic_project(self, requirements: Dict[str, Any]):
        """Gera projeto generico"""
        description = requirements.get("description", "Generated Project")

        readme = f"""# Generated Project

{description}

---
Generated by Agent Factory MVP
"""
        (self._project_path / "README.md").write_text(readme)

    async def _attempt_fix(self, job, step: str, result: StepResult):
        """Tenta corrigir erros automaticamente usando Claude"""
        if not self._claude_client:
            self._log(f"Auto-fix skipped (no Claude client): {result.errors}")
            return

        try:
            # Construir prompt para fix
            fix_prompt = f"""You are a code fixer. The following step failed during code generation:

Step: {step}
Error: {result.message}
Output: {result.output}
Errors: {result.errors}

Project path: {self._project_path}

Please analyze the error and provide a fix. Return ONLY the corrected code files in the following format:

---FILE: path/to/file.py---
<file content>
---END FILE---

For each file that needs to be modified or created.
"""
            response = self._claude_client.messages.create(
                model=self._model,
                max_tokens=8192,
                messages=[{"role": "user", "content": fix_prompt}]
            )

            fix_content = response.content[0].text

            # Aplicar fixes
            self._apply_claude_fixes(fix_content)
            self._log(f"Auto-fix applied for {step}")

        except Exception as e:
            self._log(f"Auto-fix failed: {e}")

    def _apply_claude_fixes(self, content: str):
        """Aplica fixes gerados pelo Claude"""
        # Parse files do content
        import re
        file_pattern = r'---FILE: (.+?)---\n(.*?)---END FILE---'
        matches = re.findall(file_pattern, content, re.DOTALL)

        for file_path, file_content in matches:
            full_path = self._project_path / file_path.strip()
            full_path.parent.mkdir(parents=True, exist_ok=True)
            full_path.write_text(file_content.strip())
            self._log(f"Fixed file: {file_path}")

    async def _run_standalone(
        self,
        job_id: str,
        description: str,
        tech_stack: str,
        features: list
    ) -> Dict[str, Any]:
        """
        Executa loop em modo standalone (sem DB)
        Usado quando chamado diretamente pelo worker
        """
        # Criar diretorio do projeto
        project_name = self._generate_project_name(description)
        self._project_path = self.config.project_base_dir / project_name
        self._project_path.mkdir(parents=True, exist_ok=True)

        # Criar job mock
        class JobMock:
            def __init__(self, desc, stack, feats, jid):
                self.job_id = jid
                self.description = desc
                self.tech_stack = stack
                self.features = feats or []
                self.project_id = None

        job = JobMock(description, tech_stack, features, job_id)

        # Definir steps
        steps = [
            ("parsing", self._step_parse),
            ("generating", self._step_generate_with_claude if self._claude_client else self._step_generate),
        ]

        if self.config.lint_enabled:
            steps.append(("linting", self._step_lint))
        if self.config.type_check_enabled:
            steps.append(("type_checking", self._step_type_check))
        if self.config.test_enabled:
            steps.append(("testing", self._step_test))
        if self.config.security_scan_enabled:
            steps.append(("security_scan", self._step_security))
        if self.config.auto_commit:
            steps.append(("committing", self._step_commit))

        total_steps = len(steps)

        # Executar steps
        for idx, (step_name, step_fn) in enumerate(steps):
            progress = (idx / total_steps) * 100

            if self._on_step_update:
                await self._on_step_update(step_name, progress, f"Running {step_name}")

            # Executar com retry
            for attempt in range(1, self.config.max_attempts + 1):
                self._log(f"Step {step_name} - Attempt {attempt}/{self.config.max_attempts}")

                result = await step_fn(job)

                if result.success:
                    self._log(f"Step {step_name} completed")
                    break

                # Tentar corrigir
                if result.can_fix and attempt < self.config.max_attempts:
                    await self._attempt_fix(job, step_name, result)
                elif attempt >= self.config.max_attempts:
                    return {
                        "success": False,
                        "error": f"Step {step_name} failed: {result.message}",
                        "step": step_name,
                        "output_path": str(self._project_path)
                    }

        return {
            "success": True,
            "output_path": str(self._project_path),
            "job_id": job_id
        }

    async def _step_generate_with_claude(self, job) -> StepResult:
        """
        Gera codigo usando Claude API
        """
        if not self._claude_client:
            return await self._step_generate(job)

        try:
            # Ler requirements
            requirements_file = self._project_path / "requirements.json"
            with open(requirements_file, "r", encoding="utf-8") as f:
                requirements = json.load(f)

            # Construir prompt
            prompt = f"""You are an expert software developer. Generate a complete, working project based on these requirements:

Description: {requirements.get('description', job.description)}
Tech Stack: {requirements.get('tech_stack', {})}
Features: {requirements.get('features', [])}

Generate a complete project with:
1. All necessary source files
2. Configuration files (requirements.txt, package.json, etc.)
3. Basic tests
4. README.md

Return the files in this format:

---FILE: path/to/file.ext---
<complete file content>
---END FILE---

Generate ALL files needed for a working project. Include proper error handling, type hints (for Python), and follow best practices.
"""

            response = self._claude_client.messages.create(
                model=self._model,
                max_tokens=16384,
                messages=[{"role": "user", "content": prompt}]
            )

            content = response.content[0].text

            # Aplicar arquivos gerados
            self._apply_claude_fixes(content)

            return StepResult(
                success=True,
                message="Code generated with Claude",
                output=f"Project created at {self._project_path}"
            )

        except Exception as e:
            return StepResult(
                success=False,
                message=f"Claude generation failed: {str(e)}",
                errors=[str(e)],
                can_fix=False
            )

    def _log(self, message: str):
        """Log interno"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        print(f"[{timestamp}] [Loop:{self._current_job_id}] {message}")


# =============================================================================
# SINGLETON
# =============================================================================

_loop_instance: Optional[AutonomousLoop] = None


def get_autonomous_loop(config: LoopConfig = None) -> AutonomousLoop:
    """Retorna instancia singleton do loop"""
    global _loop_instance
    if _loop_instance is None:
        _loop_instance = AutonomousLoop(config)
    return _loop_instance


# =============================================================================
# CLI TEST
# =============================================================================

if __name__ == "__main__":
    import asyncio

    async def test():
        # Criar job de teste primeiro
        from factory.core.job_queue import get_queue

        queue = get_queue()
        job = queue.enqueue({
            "description": "API REST para lista de tarefas",
            "tech_stack": "python, fastapi, sqlite",
            "features": ["CRUD tarefas", "Auth JWT"],
            "created_by": "test"
        })

        print(f"Job criado: {job['job_id']}")

        # Executar loop
        loop = get_autonomous_loop()
        result = await loop.run(job["job_id"])

        print(f"Resultado: {result}")

    asyncio.run(test())

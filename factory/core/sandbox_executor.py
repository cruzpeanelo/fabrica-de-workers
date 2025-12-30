"""
Sandbox Executor - Isolated Worker Execution
Issue #198: Security sandbox for worker execution

Provides Docker-based isolated execution environment for workers
with resource limits, security hardening, and ephemeral containers.
"""
import os
import uuid
import asyncio
import tempfile
import logging
from typing import Optional, Dict, Any, List
from dataclasses import dataclass, field
from pathlib import Path
from datetime import datetime

logger = logging.getLogger(__name__)


@dataclass
class SandboxConfig:
    """Configuration for sandbox execution"""
    # Resource limits
    cpu_limit: float = 0.5  # CPU cores (0.5 = 50% of one core)
    memory_limit: str = "256m"  # Memory limit (256MB)
    timeout_seconds: int = 60  # Max execution time

    # Network settings
    network_mode: str = "none"  # No network by default

    # Security settings
    read_only_root: bool = True  # Read-only filesystem
    no_new_privileges: bool = True  # Prevent privilege escalation
    user: str = "1000:1000"  # Run as non-root user

    # Seccomp profile (restrict syscalls)
    seccomp_profile: str = "default"  # Docker's default seccomp

    # Allowed capabilities (minimal)
    cap_drop: List[str] = field(default_factory=lambda: ["ALL"])
    cap_add: List[str] = field(default_factory=list)

    # Container settings
    image: str = "python:3.10-slim"
    work_dir: str = "/app"
    tmp_size: str = "64m"  # Size of /tmp


@dataclass
class SandboxResult:
    """Result from sandbox execution"""
    success: bool
    exit_code: int
    stdout: str
    stderr: str
    execution_time: float
    container_id: Optional[str] = None
    error: Optional[str] = None
    resource_usage: Optional[Dict[str, Any]] = None


class SandboxExecutor:
    """
    Executes code in isolated Docker containers with security hardening.

    Features:
    - Ephemeral containers (destroyed after use)
    - Resource limits (CPU, memory, timeout)
    - Security hardening (seccomp, no-new-privileges, read-only fs)
    - Network isolation
    - Non-root execution
    """

    def __init__(self, config: Optional[SandboxConfig] = None):
        self.config = config or SandboxConfig()
        self._docker_available = None
        self._warm_pool: List[str] = []

    async def check_docker(self) -> bool:
        """Check if Docker is available and running"""
        if self._docker_available is not None:
            return self._docker_available

        try:
            proc = await asyncio.create_subprocess_exec(
                "docker", "info",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            await proc.communicate()
            self._docker_available = proc.returncode == 0
        except Exception as e:
            logger.warning(f"Docker not available: {e}")
            self._docker_available = False

        return self._docker_available

    async def execute(
        self,
        code: str,
        language: str = "python",
        files: Optional[Dict[str, str]] = None,
        env_vars: Optional[Dict[str, str]] = None,
        config_override: Optional[SandboxConfig] = None
    ) -> SandboxResult:
        """
        Execute code in an isolated sandbox container.

        Args:
            code: Source code to execute
            language: Programming language (python, node, etc)
            files: Additional files to include {filename: content}
            env_vars: Environment variables (sanitized)
            config_override: Override default config

        Returns:
            SandboxResult with execution outcome
        """
        config = config_override or self.config
        start_time = datetime.now()
        container_id = None

        # Check Docker availability
        if not await self.check_docker():
            return await self._fallback_execute(code, language, config)

        try:
            # Create temp directory with code and files
            with tempfile.TemporaryDirectory(prefix="sandbox_") as tmpdir:
                tmpdir_path = Path(tmpdir)

                # Write main code file
                main_file = self._get_main_filename(language)
                (tmpdir_path / main_file).write_text(code, encoding='utf-8')

                # Write additional files
                if files:
                    for filename, content in files.items():
                        safe_filename = self._sanitize_filename(filename)
                        (tmpdir_path / safe_filename).write_text(content, encoding='utf-8')

                # Build Docker run command
                container_id = f"sandbox_{uuid.uuid4().hex[:12]}"
                cmd = self._build_docker_command(
                    container_id, tmpdir_path, main_file, language,
                    env_vars, config
                )

                logger.info(f"[Sandbox] Starting container {container_id}")

                # Execute with timeout
                try:
                    proc = await asyncio.create_subprocess_shell(
                        cmd,
                        stdout=asyncio.subprocess.PIPE,
                        stderr=asyncio.subprocess.PIPE
                    )

                    stdout, stderr = await asyncio.wait_for(
                        proc.communicate(),
                        timeout=config.timeout_seconds
                    )

                    execution_time = (datetime.now() - start_time).total_seconds()

                    return SandboxResult(
                        success=proc.returncode == 0,
                        exit_code=proc.returncode or 0,
                        stdout=stdout.decode('utf-8', errors='replace'),
                        stderr=stderr.decode('utf-8', errors='replace'),
                        execution_time=execution_time,
                        container_id=container_id
                    )

                except asyncio.TimeoutError:
                    await self._kill_container(container_id)
                    execution_time = (datetime.now() - start_time).total_seconds()

                    return SandboxResult(
                        success=False,
                        exit_code=-1,
                        stdout="",
                        stderr=f"Execution timed out after {config.timeout_seconds}s",
                        execution_time=execution_time,
                        container_id=container_id,
                        error="TIMEOUT"
                    )

        except Exception as e:
            execution_time = (datetime.now() - start_time).total_seconds()
            logger.error(f"[Sandbox] Execution error: {e}")

            return SandboxResult(
                success=False,
                exit_code=-1,
                stdout="",
                stderr=str(e),
                execution_time=execution_time,
                container_id=container_id,
                error="EXECUTION_ERROR"
            )
        finally:
            if container_id:
                await self._cleanup_container(container_id)

    def _build_docker_command(
        self,
        container_id: str,
        tmpdir: Path,
        main_file: str,
        language: str,
        env_vars: Optional[Dict[str, str]],
        config: SandboxConfig
    ) -> str:
        """Build the docker run command with all security options"""

        image = self._get_image_for_language(language)
        run_cmd = self._get_run_command(language, main_file)

        cmd_parts = [
            "docker", "run",
            "--name", container_id,
            "--rm",
            f"--cpus={config.cpu_limit}",
            f"--memory={config.memory_limit}",
            "--memory-swap", config.memory_limit,
            "--pids-limit", "50",
            "--security-opt", "no-new-privileges:true",
            "--cap-drop", "ALL",
            "-u", config.user,
            f"--network={config.network_mode}",
            "--tmpfs", f"/tmp:rw,noexec,nosuid,size={config.tmp_size}",
            "-v", f"{tmpdir}:{config.work_dir}:ro",
            "-w", config.work_dir,
        ]

        if config.read_only_root:
            cmd_parts.append("--read-only")

        if env_vars:
            for key, value in env_vars.items():
                safe_key = self._sanitize_env_key(key)
                if not self._is_sensitive_var(safe_key):
                    cmd_parts.extend(["-e", f"{safe_key}={value}"])

        cmd_parts.extend([image, *run_cmd.split()])

        return " ".join(filter(None, cmd_parts))

    def _get_image_for_language(self, language: str) -> str:
        """Get Docker image for language"""
        images = {
            "python": "python:3.10-slim",
            "python3": "python:3.10-slim",
            "node": "node:18-slim",
            "javascript": "node:18-slim",
            "bash": "alpine:3.18",
            "shell": "alpine:3.18",
        }
        return images.get(language.lower(), self.config.image)

    def _get_main_filename(self, language: str) -> str:
        """Get main filename for language"""
        extensions = {
            "python": "main.py",
            "python3": "main.py",
            "node": "main.js",
            "javascript": "main.js",
            "bash": "main.sh",
            "shell": "main.sh",
        }
        return extensions.get(language.lower(), "main.py")

    def _get_run_command(self, language: str, main_file: str) -> str:
        """Get command to run the code"""
        commands = {
            "python": f"python {main_file}",
            "python3": f"python {main_file}",
            "node": f"node {main_file}",
            "javascript": f"node {main_file}",
            "bash": f"sh {main_file}",
            "shell": f"sh {main_file}",
        }
        return commands.get(language.lower(), f"python {main_file}")

    def _sanitize_filename(self, filename: str) -> str:
        """Sanitize filename to prevent path traversal"""
        safe_name = Path(filename).name
        safe_name = "".join(c for c in safe_name if c.isalnum() or c in "._-")
        return safe_name or "file.txt"

    def _sanitize_env_key(self, key: str) -> str:
        """Sanitize environment variable key"""
        return "".join(c for c in key if c.isalnum() or c == "_").upper()

    def _is_sensitive_var(self, key: str) -> bool:
        """Check if environment variable is sensitive"""
        sensitive_patterns = [
            "PASSWORD", "SECRET", "KEY", "TOKEN", "CREDENTIAL",
            "AWS_", "AZURE_", "GCP_", "API_KEY", "AUTH", "PRIVATE"
        ]
        return any(pattern in key.upper() for pattern in sensitive_patterns)

    async def _kill_container(self, container_id: str):
        """Force kill a container"""
        try:
            proc = await asyncio.create_subprocess_exec(
                "docker", "kill", container_id,
                stdout=asyncio.subprocess.DEVNULL,
                stderr=asyncio.subprocess.DEVNULL
            )
            await proc.wait()
        except Exception as e:
            logger.warning(f"[Sandbox] Failed to kill container {container_id}: {e}")

    async def _cleanup_container(self, container_id: str):
        """Cleanup container"""
        try:
            proc = await asyncio.create_subprocess_exec(
                "docker", "rm", "-f", container_id,
                stdout=asyncio.subprocess.DEVNULL,
                stderr=asyncio.subprocess.DEVNULL
            )
            await proc.wait()
        except Exception:
            pass

    async def _fallback_execute(
        self,
        code: str,
        language: str,
        config: SandboxConfig
    ) -> SandboxResult:
        """Fallback execution when Docker is not available (less secure)"""
        logger.warning("[Sandbox] Docker not available, using fallback (less secure)")

        start_time = datetime.now()

        try:
            with tempfile.TemporaryDirectory(prefix="sandbox_fallback_") as tmpdir:
                tmpdir_path = Path(tmpdir)
                main_file = self._get_main_filename(language)
                (tmpdir_path / main_file).write_text(code, encoding='utf-8')

                run_cmd = self._get_run_command(language, main_file)

                proc = await asyncio.create_subprocess_shell(
                    run_cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                    cwd=str(tmpdir_path)
                )

                try:
                    stdout, stderr = await asyncio.wait_for(
                        proc.communicate(),
                        timeout=config.timeout_seconds
                    )

                    execution_time = (datetime.now() - start_time).total_seconds()

                    return SandboxResult(
                        success=proc.returncode == 0,
                        exit_code=proc.returncode or 0,
                        stdout=stdout.decode('utf-8', errors='replace'),
                        stderr=stderr.decode('utf-8', errors='replace') + "\n[WARNING: Executed without Docker isolation]",
                        execution_time=execution_time,
                        error="FALLBACK_MODE"
                    )
                except asyncio.TimeoutError:
                    proc.kill()
                    return SandboxResult(
                        success=False,
                        exit_code=-1,
                        stdout="",
                        stderr=f"Execution timed out after {config.timeout_seconds}s",
                        execution_time=config.timeout_seconds,
                        error="TIMEOUT"
                    )

        except Exception as e:
            return SandboxResult(
                success=False,
                exit_code=-1,
                stdout="",
                stderr=str(e),
                execution_time=(datetime.now() - start_time).total_seconds(),
                error="FALLBACK_ERROR"
            )


# Singleton instance
_sandbox_executor: Optional[SandboxExecutor] = None


def get_sandbox_executor(config: Optional[SandboxConfig] = None) -> SandboxExecutor:
    """Get or create sandbox executor singleton"""
    global _sandbox_executor
    if _sandbox_executor is None:
        _sandbox_executor = SandboxExecutor(config)
    return _sandbox_executor


async def execute_in_sandbox(
    code: str,
    language: str = "python",
    files: Optional[Dict[str, str]] = None,
    timeout: int = 60
) -> SandboxResult:
    """
    Convenience function to execute code in sandbox.

    Args:
        code: Source code to execute
        language: Programming language
        files: Additional files
        timeout: Execution timeout in seconds

    Returns:
        SandboxResult with execution outcome
    """
    config = SandboxConfig(timeout_seconds=timeout)
    executor = get_sandbox_executor()
    return await executor.execute(code, language, files, config_override=config)

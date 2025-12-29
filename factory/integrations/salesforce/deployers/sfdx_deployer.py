# -*- coding: utf-8 -*-
"""
Salesforce DX Deployer
======================
Deployer usando Salesforce DX CLI (sf/sfdx).

Funcionalidades:
- Deploy via SFDX CLI
- Suporte a Scratch Orgs
- Suporte a Source Tracking
- Push e Pull de metadados
- Deploy incremental

Exemplo de uso:
    from factory.integrations.salesforce.deployers import SFDXDeployer

    deployer = SFDXDeployer()

    # Deploy de projeto
    result = await deployer.deploy_source("force-app")

    # Push para scratch org
    result = await deployer.push()

    # Pull de scratch org
    result = await deployer.pull()
"""

import asyncio
import json
import logging
import os
import subprocess
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class SFDXStatus(str, Enum):
    """Status do SFDX"""
    SUCCESS = "Succeeded"
    PARTIAL_SUCCESS = "PartialSuccess"
    FAILED = "Failed"
    IN_PROGRESS = "InProgress"


@dataclass
class SFDXResult:
    """Resultado de comando SFDX"""
    success: bool
    status: str
    message: str = ""
    data: Dict[str, Any] = field(default_factory=dict)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    deployed_components: int = 0
    failed_components: int = 0
    stdout: str = ""
    stderr: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "status": self.status,
            "message": self.message,
            "deployed": self.deployed_components,
            "failed": self.failed_components,
            "errors": self.errors,
            "warnings": self.warnings
        }


@dataclass
class OrgInfo:
    """Informacoes de uma org Salesforce"""
    alias: str
    username: str
    org_id: str
    instance_url: str
    access_token: str
    is_scratch: bool = False
    is_dev_hub: bool = False
    is_default: bool = False
    expiration_date: Optional[str] = None
    connected_status: str = "Connected"


class SFDXDeployer:
    """
    Deployer usando Salesforce DX

    Permite operacoes de deploy usando o CLI do Salesforce DX
    (comandos sf ou sfdx).
    """

    def __init__(
        self,
        target_org: Optional[str] = None,
        project_path: Optional[str] = None,
        use_sf_cli: bool = True
    ):
        """
        Inicializa o deployer

        Args:
            target_org: Alias ou username da org alvo
            project_path: Caminho do projeto SFDX
            use_sf_cli: Usar 'sf' ao inves de 'sfdx'
        """
        self.target_org = target_org
        self.project_path = project_path or os.getcwd()
        self.cli_command = "sf" if use_sf_cli else "sfdx"
        self._cli_available = None

    async def check_cli(self) -> bool:
        """
        Verifica se o CLI esta disponivel

        Returns:
            True se o CLI esta instalado
        """
        if self._cli_available is not None:
            return self._cli_available

        try:
            result = await self._run_command([self.cli_command, "--version"])
            self._cli_available = result.success
            return self._cli_available
        except Exception:
            self._cli_available = False
            return False

    async def get_org_list(self) -> List[OrgInfo]:
        """
        Lista orgs conectadas

        Returns:
            Lista de OrgInfo
        """
        result = await self._run_command([
            self.cli_command, "org", "list", "--json"
        ])

        if not result.success:
            return []

        orgs = []
        data = result.data

        # Scratch orgs
        for org in data.get("result", {}).get("scratchOrgs", []):
            orgs.append(OrgInfo(
                alias=org.get("alias", ""),
                username=org.get("username", ""),
                org_id=org.get("orgId", ""),
                instance_url=org.get("instanceUrl", ""),
                access_token=org.get("accessToken", ""),
                is_scratch=True,
                is_default=org.get("isDefaultUsername", False),
                expiration_date=org.get("expirationDate"),
                connected_status=org.get("connectedStatus", "Unknown")
            ))

        # Non-scratch orgs
        for org in data.get("result", {}).get("nonScratchOrgs", []):
            orgs.append(OrgInfo(
                alias=org.get("alias", ""),
                username=org.get("username", ""),
                org_id=org.get("orgId", ""),
                instance_url=org.get("instanceUrl", ""),
                access_token=org.get("accessToken", ""),
                is_scratch=False,
                is_dev_hub=org.get("isDevHub", False),
                is_default=org.get("isDefaultUsername", False),
                connected_status=org.get("connectedStatus", "Unknown")
            ))

        return orgs

    async def deploy_source(
        self,
        source_path: str,
        target_org: Optional[str] = None,
        test_level: str = "NoTestRun",
        specified_tests: Optional[List[str]] = None,
        check_only: bool = False,
        ignore_conflicts: bool = False,
        ignore_warnings: bool = False,
        wait_minutes: int = 33
    ) -> SFDXResult:
        """
        Faz deploy de source para org

        Args:
            source_path: Caminho do source (ex: force-app)
            target_org: Org alvo (alias ou username)
            test_level: NoTestRun, RunSpecifiedTests, RunLocalTests, RunAllTestsInOrg
            specified_tests: Testes para RunSpecifiedTests
            check_only: Apenas validar
            ignore_conflicts: Ignorar conflitos
            ignore_warnings: Ignorar warnings
            wait_minutes: Tempo de espera

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        cmd = [
            self.cli_command, "project", "deploy", "start",
            "--source-dir", source_path,
            "--test-level", test_level,
            "--wait", str(wait_minutes),
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        if specified_tests:
            cmd.extend(["--tests", ",".join(specified_tests)])

        if check_only:
            cmd.append("--dry-run")

        if ignore_conflicts:
            cmd.append("--ignore-conflicts")

        if ignore_warnings:
            cmd.append("--ignore-warnings")

        return await self._run_command(cmd)

    async def deploy_metadata(
        self,
        metadata_type: str,
        metadata_names: List[str],
        target_org: Optional[str] = None,
        test_level: str = "NoTestRun"
    ) -> SFDXResult:
        """
        Faz deploy de metadados especificos

        Args:
            metadata_type: Tipo de metadata (ApexClass, CustomObject, etc)
            metadata_names: Nomes dos metadados
            target_org: Org alvo
            test_level: Nivel de testes

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        metadata_list = ",".join([f"{metadata_type}:{name}" for name in metadata_names])

        cmd = [
            self.cli_command, "project", "deploy", "start",
            "--metadata", metadata_list,
            "--test-level", test_level,
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        return await self._run_command(cmd)

    async def push(
        self,
        target_org: Optional[str] = None,
        force: bool = False,
        ignore_conflicts: bool = False,
        wait_minutes: int = 33
    ) -> SFDXResult:
        """
        Push de mudancas para scratch org

        Args:
            target_org: Scratch org alvo
            force: Forcar push mesmo com conflitos
            ignore_conflicts: Ignorar conflitos
            wait_minutes: Tempo de espera

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        cmd = [
            self.cli_command, "project", "deploy", "start",
            "--wait", str(wait_minutes),
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        if force:
            cmd.append("--force-overwrite")

        if ignore_conflicts:
            cmd.append("--ignore-conflicts")

        return await self._run_command(cmd)

    async def pull(
        self,
        target_org: Optional[str] = None,
        force: bool = False,
        ignore_conflicts: bool = False
    ) -> SFDXResult:
        """
        Pull de mudancas da scratch org

        Args:
            target_org: Scratch org fonte
            force: Forcar pull mesmo com conflitos
            ignore_conflicts: Ignorar conflitos

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        cmd = [
            self.cli_command, "project", "retrieve", "start",
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        if force:
            cmd.append("--force-overwrite")

        if ignore_conflicts:
            cmd.append("--ignore-conflicts")

        return await self._run_command(cmd)

    async def retrieve(
        self,
        metadata_type: str,
        metadata_names: List[str],
        target_org: Optional[str] = None,
        output_dir: Optional[str] = None
    ) -> SFDXResult:
        """
        Retrieve metadados especificos

        Args:
            metadata_type: Tipo de metadata
            metadata_names: Nomes dos metadados
            target_org: Org fonte
            output_dir: Diretorio de saida

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        metadata_list = ",".join([f"{metadata_type}:{name}" for name in metadata_names])

        cmd = [
            self.cli_command, "project", "retrieve", "start",
            "--metadata", metadata_list,
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        if output_dir:
            cmd.extend(["--output-dir", output_dir])

        return await self._run_command(cmd)

    async def create_scratch_org(
        self,
        definition_file: str,
        alias: str,
        duration_days: int = 7,
        dev_hub: Optional[str] = None,
        set_default: bool = True
    ) -> SFDXResult:
        """
        Cria scratch org

        Args:
            definition_file: Arquivo de definicao (ex: config/project-scratch-def.json)
            alias: Alias da scratch org
            duration_days: Duracao em dias
            dev_hub: Dev Hub para usar
            set_default: Definir como org padrao

        Returns:
            SFDXResult
        """
        cmd = [
            self.cli_command, "org", "create", "scratch",
            "--definition-file", definition_file,
            "--alias", alias,
            "--duration-days", str(duration_days),
            "--json"
        ]

        if dev_hub:
            cmd.extend(["--target-dev-hub", dev_hub])

        if set_default:
            cmd.append("--set-default")

        return await self._run_command(cmd)

    async def delete_scratch_org(
        self,
        target_org: str,
        no_prompt: bool = True
    ) -> SFDXResult:
        """
        Deleta scratch org

        Args:
            target_org: Alias ou username da scratch org
            no_prompt: Nao pedir confirmacao

        Returns:
            SFDXResult
        """
        cmd = [
            self.cli_command, "org", "delete", "scratch",
            "--target-org", target_org,
            "--json"
        ]

        if no_prompt:
            cmd.append("--no-prompt")

        return await self._run_command(cmd)

    async def run_tests(
        self,
        target_org: Optional[str] = None,
        class_names: Optional[List[str]] = None,
        suite_names: Optional[List[str]] = None,
        test_level: str = "RunLocalTests",
        code_coverage: bool = True,
        output_dir: Optional[str] = None
    ) -> SFDXResult:
        """
        Executa testes Apex

        Args:
            target_org: Org alvo
            class_names: Classes de teste especificas
            suite_names: Test suites
            test_level: Nivel de teste
            code_coverage: Incluir coverage
            output_dir: Diretorio para resultados

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        cmd = [
            self.cli_command, "apex", "run", "test",
            "--test-level", test_level,
            "--wait", "10",
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        if class_names:
            cmd.extend(["--class-names", ",".join(class_names)])

        if suite_names:
            cmd.extend(["--suite-names", ",".join(suite_names)])

        if code_coverage:
            cmd.append("--code-coverage")

        if output_dir:
            cmd.extend(["--output-dir", output_dir])

        return await self._run_command(cmd)

    async def execute_anonymous(
        self,
        apex_code: str,
        target_org: Optional[str] = None
    ) -> SFDXResult:
        """
        Executa codigo Apex anonimo

        Args:
            apex_code: Codigo Apex
            target_org: Org alvo

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        # Criar arquivo temporario
        import tempfile
        with tempfile.NamedTemporaryFile(mode='w', suffix='.apex', delete=False) as f:
            f.write(apex_code)
            temp_file = f.name

        try:
            cmd = [
                self.cli_command, "apex", "run",
                "--file", temp_file,
                "--json"
            ]

            if target:
                cmd.extend(["--target-org", target])

            return await self._run_command(cmd)

        finally:
            os.unlink(temp_file)

    async def generate_password(
        self,
        target_org: Optional[str] = None
    ) -> SFDXResult:
        """
        Gera senha para usuario da org

        Args:
            target_org: Org alvo

        Returns:
            SFDXResult com senha gerada
        """
        target = target_org or self.target_org

        cmd = [
            self.cli_command, "org", "generate", "password",
            "--json"
        ]

        if target:
            cmd.extend(["--target-org", target])

        return await self._run_command(cmd)

    async def open_org(
        self,
        target_org: Optional[str] = None,
        path: Optional[str] = None
    ) -> SFDXResult:
        """
        Abre a org no navegador

        Args:
            target_org: Org alvo
            path: Caminho especifico (ex: /lightning/setup/SetupOneHome/home)

        Returns:
            SFDXResult
        """
        target = target_org or self.target_org

        cmd = [self.cli_command, "org", "open", "--json"]

        if target:
            cmd.extend(["--target-org", target])

        if path:
            cmd.extend(["--path", path])

        return await self._run_command(cmd)

    async def display_org(
        self,
        target_org: Optional[str] = None
    ) -> SFDXResult:
        """
        Exibe informacoes da org

        Args:
            target_org: Org alvo

        Returns:
            SFDXResult com informacoes da org
        """
        target = target_org or self.target_org

        cmd = [self.cli_command, "org", "display", "--json"]

        if target:
            cmd.extend(["--target-org", target])

        return await self._run_command(cmd)

    async def _run_command(
        self,
        cmd: List[str],
        cwd: Optional[str] = None
    ) -> SFDXResult:
        """Executa comando SFDX"""
        work_dir = cwd or self.project_path

        logger.debug(f"Executando: {' '.join(cmd)}")

        try:
            process = await asyncio.create_subprocess_exec(
                *cmd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd=work_dir
            )

            stdout, stderr = await process.communicate()
            stdout_str = stdout.decode('utf-8', errors='ignore')
            stderr_str = stderr.decode('utf-8', errors='ignore')

            # Tentar parsear JSON
            try:
                data = json.loads(stdout_str)
                success = data.get("status", 1) == 0
                result_data = data.get("result", {})
                message = data.get("message", "")

                # Extrair erros e warnings
                errors = []
                warnings = []

                if isinstance(result_data, dict):
                    # Deploy result
                    for failure in result_data.get("files", {}).get("failures", []):
                        errors.append(failure.get("error", str(failure)))

                    for warning in result_data.get("files", {}).get("warnings", []):
                        warnings.append(str(warning))

                return SFDXResult(
                    success=success,
                    status=SFDXStatus.SUCCESS.value if success else SFDXStatus.FAILED.value,
                    message=message,
                    data=data,
                    errors=errors,
                    warnings=warnings,
                    stdout=stdout_str,
                    stderr=stderr_str
                )

            except json.JSONDecodeError:
                # Comando nao retornou JSON
                success = process.returncode == 0

                return SFDXResult(
                    success=success,
                    status=SFDXStatus.SUCCESS.value if success else SFDXStatus.FAILED.value,
                    message=stdout_str or stderr_str,
                    stdout=stdout_str,
                    stderr=stderr_str,
                    errors=[stderr_str] if not success and stderr_str else []
                )

        except FileNotFoundError:
            return SFDXResult(
                success=False,
                status=SFDXStatus.FAILED.value,
                message=f"CLI '{self.cli_command}' nao encontrado. Instale o Salesforce CLI.",
                errors=[f"CLI '{self.cli_command}' nao encontrado"]
            )

        except Exception as e:
            logger.error(f"Erro ao executar comando: {e}")
            return SFDXResult(
                success=False,
                status=SFDXStatus.FAILED.value,
                message=str(e),
                errors=[str(e)]
            )

# -*- coding: utf-8 -*-
"""
Salesforce Metadata Deployer
============================
Deployer usando Metadata API do Salesforce.

Funcionalidades:
- Deploy de classes Apex
- Deploy de triggers
- Deploy de LWC
- Deploy de objetos e campos
- Deploy de flows
- Validacao antes do deploy
- Rollback em caso de erro

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient
    from factory.integrations.salesforce.deployers import MetadataDeployer

    sf = SalesforceClient(config)
    await sf.connect()

    deployer = MetadataDeployer(sf)

    # Deploy de uma classe
    result = await deployer.deploy_apex_class("MinhaClasse", apex_code)

    # Deploy de multiplos componentes
    result = await deployer.deploy_components([
        {"type": "ApexClass", "name": "Classe1", "content": code1},
        {"type": "ApexTrigger", "name": "Trigger1", "content": code2},
    ])
"""

import asyncio
import base64
import io
import logging
import os
import tempfile
import zipfile
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)


class DeployStatus(str, Enum):
    """Status do deploy"""
    PENDING = "Pending"
    IN_PROGRESS = "InProgress"
    SUCCEEDED = "Succeeded"
    CANCELED = "Canceled"
    FAILED = "Failed"


class TestLevel(str, Enum):
    """Nivel de testes no deploy"""
    NO_TEST_RUN = "NoTestRun"
    RUN_SPECIFIED_TESTS = "RunSpecifiedTests"
    RUN_LOCAL_TESTS = "RunLocalTests"
    RUN_ALL_TESTS_IN_ORG = "RunAllTestsInOrg"


@dataclass
class ComponentResult:
    """Resultado de deploy de um componente"""
    name: str
    type: str
    success: bool
    created: bool = False
    changed: bool = False
    deleted: bool = False
    error_message: Optional[str] = None
    line_number: Optional[int] = None
    column_number: Optional[int] = None


@dataclass
class DeployResult:
    """Resultado completo do deploy"""
    id: str
    success: bool
    status: str
    done: bool
    start_time: Optional[datetime] = None
    complete_time: Optional[datetime] = None
    component_results: List[ComponentResult] = field(default_factory=list)
    test_results: List[Dict[str, Any]] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    @property
    def components_deployed(self) -> int:
        return sum(1 for c in self.component_results if c.success)

    @property
    def components_failed(self) -> int:
        return sum(1 for c in self.component_results if not c.success)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "success": self.success,
            "status": self.status,
            "done": self.done,
            "components_deployed": self.components_deployed,
            "components_failed": self.components_failed,
            "errors": self.errors,
            "warnings": self.warnings
        }


class MetadataDeployer:
    """
    Deployer usando Metadata API

    Permite fazer deploy de componentes Salesforce
    usando a Metadata API.
    """

    def __init__(self, sf_client, api_version: str = "59.0"):
        """
        Inicializa o deployer

        Args:
            sf_client: SalesforceClient autenticado
            api_version: Versao da API
        """
        self.sf = sf_client
        self.api_version = api_version
        self._metadata = None

    @property
    def metadata(self):
        """Lazy loading do MetadataClient"""
        if self._metadata is None:
            from ..metadata_client import MetadataClient
            self._metadata = MetadataClient(self.sf)
        return self._metadata

    async def deploy_apex_class(
        self,
        class_name: str,
        body: str,
        test_level: TestLevel = TestLevel.NO_TEST_RUN,
        specified_tests: Optional[List[str]] = None
    ) -> DeployResult:
        """
        Faz deploy de uma classe Apex

        Args:
            class_name: Nome da classe
            body: Codigo Apex
            test_level: Nivel de testes
            specified_tests: Testes especificos para executar

        Returns:
            DeployResult
        """
        # Criar meta.xml
        meta_xml = self._generate_apex_meta_xml()

        components = [
            {
                "type": "ApexClass",
                "name": class_name,
                "content": body,
                "meta_content": meta_xml
            }
        ]

        return await self.deploy_components(
            components,
            test_level=test_level,
            specified_tests=specified_tests
        )

    async def deploy_apex_trigger(
        self,
        trigger_name: str,
        body: str,
        test_level: TestLevel = TestLevel.NO_TEST_RUN,
        specified_tests: Optional[List[str]] = None
    ) -> DeployResult:
        """
        Faz deploy de um trigger

        Args:
            trigger_name: Nome do trigger
            body: Codigo do trigger
            test_level: Nivel de testes
            specified_tests: Testes especificos

        Returns:
            DeployResult
        """
        meta_xml = self._generate_trigger_meta_xml()

        components = [
            {
                "type": "ApexTrigger",
                "name": trigger_name,
                "content": body,
                "meta_content": meta_xml
            }
        ]

        return await self.deploy_components(
            components,
            test_level=test_level,
            specified_tests=specified_tests
        )

    async def deploy_lwc(
        self,
        component_name: str,
        files: Dict[str, str]
    ) -> DeployResult:
        """
        Faz deploy de um Lightning Web Component

        Args:
            component_name: Nome do componente
            files: Dict com arquivos {nome: conteudo}

        Returns:
            DeployResult
        """
        components = [
            {
                "type": "LightningComponentBundle",
                "name": component_name,
                "files": files
            }
        ]

        return await self.deploy_components(components)

    async def deploy_custom_object(
        self,
        object_name: str,
        object_xml: str
    ) -> DeployResult:
        """
        Faz deploy de um objeto customizado

        Args:
            object_name: Nome do objeto
            object_xml: XML do objeto

        Returns:
            DeployResult
        """
        components = [
            {
                "type": "CustomObject",
                "name": object_name,
                "content": object_xml
            }
        ]

        return await self.deploy_components(components)

    async def deploy_custom_field(
        self,
        object_name: str,
        field_name: str,
        field_xml: str
    ) -> DeployResult:
        """
        Faz deploy de um campo customizado

        Args:
            object_name: Nome do objeto
            field_name: Nome do campo
            field_xml: XML do campo

        Returns:
            DeployResult
        """
        components = [
            {
                "type": "CustomField",
                "name": f"{object_name}.{field_name}",
                "content": field_xml
            }
        ]

        return await self.deploy_components(components)

    async def deploy_flow(
        self,
        flow_name: str,
        flow_xml: str
    ) -> DeployResult:
        """
        Faz deploy de um Flow

        Args:
            flow_name: Nome do flow
            flow_xml: XML do flow

        Returns:
            DeployResult
        """
        components = [
            {
                "type": "Flow",
                "name": flow_name,
                "content": flow_xml
            }
        ]

        return await self.deploy_components(components)

    async def deploy_components(
        self,
        components: List[Dict[str, Any]],
        test_level: TestLevel = TestLevel.NO_TEST_RUN,
        specified_tests: Optional[List[str]] = None,
        check_only: bool = False,
        ignore_warnings: bool = False,
        rollback_on_error: bool = True
    ) -> DeployResult:
        """
        Faz deploy de multiplos componentes

        Args:
            components: Lista de componentes
            test_level: Nivel de testes
            specified_tests: Testes especificos
            check_only: Apenas validar, nao fazer deploy
            ignore_warnings: Ignorar warnings
            rollback_on_error: Reverter em caso de erro

        Returns:
            DeployResult
        """
        logger.info(f"Iniciando deploy de {len(components)} componente(s)")

        try:
            # Criar arquivo ZIP
            zip_bytes = self._create_deploy_package(components)

            # Opcoes de deploy
            options = {
                "checkOnly": check_only,
                "ignoreWarnings": ignore_warnings,
                "rollbackOnError": rollback_on_error,
                "testLevel": test_level.value
            }

            if test_level == TestLevel.RUN_SPECIFIED_TESTS and specified_tests:
                options["runTests"] = specified_tests

            # Fazer deploy
            result = await self.metadata.deploy_and_wait(
                zip_bytes,
                options=options,
                poll_interval=5,
                timeout=600
            )

            # Converter resultado
            return self._convert_deploy_result(result)

        except Exception as e:
            logger.error(f"Erro no deploy: {e}")
            return DeployResult(
                id="",
                success=False,
                status=DeployStatus.FAILED.value,
                done=True,
                errors=[str(e)]
            )

    async def validate(
        self,
        components: List[Dict[str, Any]],
        test_level: TestLevel = TestLevel.RUN_LOCAL_TESTS
    ) -> DeployResult:
        """
        Valida componentes sem fazer deploy

        Args:
            components: Lista de componentes
            test_level: Nivel de testes

        Returns:
            DeployResult
        """
        return await self.deploy_components(
            components,
            test_level=test_level,
            check_only=True
        )

    async def quick_deploy(self, validation_id: str) -> DeployResult:
        """
        Faz quick deploy de uma validacao bem sucedida

        Args:
            validation_id: ID da validacao

        Returns:
            DeployResult
        """
        logger.info(f"Executando quick deploy: {validation_id}")

        try:
            # Quick deploy via Metadata API
            result = await self.metadata.deploy_recent_validation(validation_id)

            return self._convert_deploy_result(result)

        except Exception as e:
            logger.error(f"Erro no quick deploy: {e}")
            return DeployResult(
                id=validation_id,
                success=False,
                status=DeployStatus.FAILED.value,
                done=True,
                errors=[str(e)]
            )

    async def cancel_deploy(self, deploy_id: str) -> bool:
        """
        Cancela um deploy em andamento

        Args:
            deploy_id: ID do deploy

        Returns:
            True se cancelado
        """
        try:
            await self.metadata.abort_job(deploy_id)
            return True
        except Exception as e:
            logger.error(f"Erro ao cancelar deploy: {e}")
            return False

    def _create_deploy_package(
        self,
        components: List[Dict[str, Any]]
    ) -> bytes:
        """Cria pacote ZIP para deploy"""
        zip_buffer = io.BytesIO()

        with zipfile.ZipFile(zip_buffer, 'w', zipfile.ZIP_DEFLATED) as zf:
            package_types = {}

            for comp in components:
                comp_type = comp.get("type", "")
                comp_name = comp.get("name", "")
                content = comp.get("content", "")
                meta_content = comp.get("meta_content", "")
                files = comp.get("files", {})

                # Determinar pasta e extensao
                folder, extension = self._get_type_info(comp_type)

                if files:
                    # LWC ou Aura - multiplos arquivos
                    for filename, file_content in files.items():
                        path = f"{folder}/{comp_name}/{filename}"
                        if isinstance(file_content, str):
                            file_content = file_content.encode('utf-8')
                        zf.writestr(path, file_content)

                    # Adicionar ao package.xml
                    if comp_type not in package_types:
                        package_types[comp_type] = []
                    package_types[comp_type].append(comp_name)

                else:
                    # Componente simples
                    if content:
                        path = f"{folder}/{comp_name}{extension}"
                        if isinstance(content, str):
                            content = content.encode('utf-8')
                        zf.writestr(path, content)

                        # Meta.xml se necessario
                        if meta_content:
                            meta_path = f"{folder}/{comp_name}{extension}-meta.xml"
                            if isinstance(meta_content, str):
                                meta_content = meta_content.encode('utf-8')
                            zf.writestr(meta_path, meta_content)

                        # Adicionar ao package.xml
                        if comp_type not in package_types:
                            package_types[comp_type] = []
                        package_types[comp_type].append(comp_name)

            # Gerar package.xml
            package_xml = self._generate_package_xml(package_types)
            zf.writestr("package.xml", package_xml.encode('utf-8'))

        return zip_buffer.getvalue()

    def _get_type_info(self, comp_type: str) -> Tuple[str, str]:
        """Retorna pasta e extensao para um tipo de componente"""
        type_mapping = {
            "ApexClass": ("classes", ".cls"),
            "ApexTrigger": ("triggers", ".trigger"),
            "ApexPage": ("pages", ".page"),
            "ApexComponent": ("components", ".component"),
            "LightningComponentBundle": ("lwc", ""),
            "AuraDefinitionBundle": ("aura", ""),
            "StaticResource": ("staticresources", ".resource"),
            "CustomObject": ("objects", ".object"),
            "CustomField": ("objects", ".object"),
            "Flow": ("flows", ".flow"),
            "Layout": ("layouts", ".layout"),
            "Profile": ("profiles", ".profile"),
            "PermissionSet": ("permissionsets", ".permissionset"),
            "CustomTab": ("tabs", ".tab"),
            "CustomLabel": ("labels", ".labels"),
            "RemoteSiteSetting": ("remoteSiteSettings", ".remoteSite"),
            "NamedCredential": ("namedCredentials", ".namedCredential")
        }

        return type_mapping.get(comp_type, ("", ""))

    def _generate_package_xml(self, package_types: Dict[str, List[str]]) -> str:
        """Gera package.xml"""
        lines = [
            '<?xml version="1.0" encoding="UTF-8"?>',
            '<Package xmlns="http://soap.sforce.com/2006/04/metadata">'
        ]

        for type_name, members in package_types.items():
            lines.append("    <types>")
            for member in members:
                lines.append(f"        <members>{member}</members>")
            lines.append(f"        <name>{type_name}</name>")
            lines.append("    </types>")

        lines.append(f"    <version>{self.api_version}</version>")
        lines.append("</Package>")

        return "\n".join(lines)

    def _generate_apex_meta_xml(self) -> str:
        """Gera meta.xml para classe Apex"""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>{self.api_version}</apiVersion>
    <status>Active</status>
</ApexClass>"""

    def _generate_trigger_meta_xml(self) -> str:
        """Gera meta.xml para trigger"""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<ApexTrigger xmlns="http://soap.sforce.com/2006/04/metadata">
    <apiVersion>{self.api_version}</apiVersion>
    <status>Active</status>
</ApexTrigger>"""

    def _convert_deploy_result(self, result) -> DeployResult:
        """Converte resultado do MetadataClient para DeployResult"""
        component_results = []

        # Sucessos
        for success in result.component_successes:
            component_results.append(ComponentResult(
                name=success.get("fullName", ""),
                type=success.get("componentType", ""),
                success=True,
                created=success.get("created", False),
                changed=success.get("changed", False),
                deleted=success.get("deleted", False)
            ))

        # Falhas
        for failure in result.component_failures:
            component_results.append(ComponentResult(
                name=failure.get("fullName", ""),
                type=failure.get("componentType", ""),
                success=False,
                error_message=failure.get("problem", ""),
                line_number=int(failure.get("lineNumber", 0)) if failure.get("lineNumber") else None,
                column_number=int(failure.get("columnNumber", 0)) if failure.get("columnNumber") else None
            ))

        # Erros gerais
        errors = [result.error_message] if result.error_message else []
        errors.extend([f.get("problem", "") for f in result.component_failures])

        return DeployResult(
            id=result.id,
            success=result.success,
            status=result.status,
            done=result.done,
            component_results=component_results,
            errors=[e for e in errors if e]
        )

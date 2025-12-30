# -*- coding: utf-8 -*-
"""
Salesforce Deploy Skill
=======================
Skill para deploy de componentes no Salesforce.

Funcionalidades:
- Deploy de classes Apex
- Deploy de triggers
- Deploy de LWC
- Deploy de objetos e campos
- Deploy de flows
- Validacao

Suporta isolamento multi-tenant atraves do tenant_id do SalesforceClient (Issue #314).

Uso pelos agentes:
    from factory.integrations.salesforce import SalesforceClient, SalesforceConfig
    from factory.integrations.salesforce.skills import SalesforceDeploySkill

    config = SalesforceConfig(
        tenant_id="TENANT-001",
        username="user@empresa.com",
        password="senha123",
        security_token="token"
    )
    sf_client = SalesforceClient(config)
    await sf_client.connect()

    skill = SalesforceDeploySkill(sf_client)

    # Deploy de classe
    result = await skill.deploy_apex_class("MinhaClasse", apex_code)

    # Validar antes de deploy
    result = await skill.validate_components([...])
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from ..client import SalesforceClient

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado de uma skill"""
    success: bool
    data: Any = None
    message: str = ""
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "errors": self.errors,
            "warnings": self.warnings
        }


class SalesforceDeploySkill:
    """
    Skill de deploy para Salesforce

    Fornece funcionalidades de deploy de componentes
    para agentes especializados.
    """

    def __init__(self, sf_client):
        """
        Inicializa a skill

        Args:
            sf_client: SalesforceClient autenticado
        """
        self.sf = sf_client
        self._metadata_deployer = None
        self._sfdx_deployer = None
        self._apex_generator = None
        self._lwc_generator = None
        self._object_generator = None

    @property
    def metadata_deployer(self):
        if self._metadata_deployer is None:
            from ..deployers import MetadataDeployer
            self._metadata_deployer = MetadataDeployer(self.sf)
        return self._metadata_deployer

    @property
    def apex_generator(self):
        if self._apex_generator is None:
            from ..generators import ApexGenerator
            self._apex_generator = ApexGenerator()
        return self._apex_generator

    @property
    def lwc_generator(self):
        if self._lwc_generator is None:
            from ..generators import LWCGenerator
            self._lwc_generator = LWCGenerator()
        return self._lwc_generator

    @property
    def object_generator(self):
        if self._object_generator is None:
            from ..generators import ObjectGenerator
            self._object_generator = ObjectGenerator()
        return self._object_generator

    # ==================== APEX ====================

    async def deploy_apex_class(
        self,
        class_name: str,
        body: str,
        run_tests: bool = False,
        test_classes: Optional[List[str]] = None
    ) -> SkillResult:
        """
        Faz deploy de uma classe Apex

        Args:
            class_name: Nome da classe
            body: Codigo Apex
            run_tests: Executar testes
            test_classes: Classes de teste especificas

        Returns:
            SkillResult
        """
        try:
            from ..deployers.metadata_deployer import TestLevel

            test_level = TestLevel.NO_TEST_RUN
            if run_tests:
                if test_classes:
                    test_level = TestLevel.RUN_SPECIFIED_TESTS
                else:
                    test_level = TestLevel.RUN_LOCAL_TESTS

            result = await self.metadata_deployer.deploy_apex_class(
                class_name,
                body,
                test_level=test_level,
                specified_tests=test_classes
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Classe {class_name} {'deployada' if result.success else 'falhou'}",
                errors=result.errors,
                warnings=result.warnings
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def deploy_apex_trigger(
        self,
        trigger_name: str,
        body: str,
        run_tests: bool = False
    ) -> SkillResult:
        """
        Faz deploy de um trigger

        Args:
            trigger_name: Nome do trigger
            body: Codigo do trigger
            run_tests: Executar testes

        Returns:
            SkillResult
        """
        try:
            from ..deployers.metadata_deployer import TestLevel

            test_level = TestLevel.RUN_LOCAL_TESTS if run_tests else TestLevel.NO_TEST_RUN

            result = await self.metadata_deployer.deploy_apex_trigger(
                trigger_name,
                body,
                test_level=test_level
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Trigger {trigger_name} {'deployado' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def deploy_service_layer(
        self,
        sobject: str,
        include_selector: bool = True,
        include_domain: bool = True,
        include_trigger: bool = True,
        include_tests: bool = True
    ) -> SkillResult:
        """
        Faz deploy de uma camada service completa

        Gera e faz deploy de:
        - Service class
        - Selector class
        - Domain class
        - Trigger com handler
        - Classes de teste

        Args:
            sobject: Objeto principal
            include_*: O que incluir

        Returns:
            SkillResult
        """
        try:
            components = []
            service_name = f"{sobject}Service"
            selector_name = f"{sobject}Selector"
            domain_name = f"{sobject}Domain"

            # Service
            service_code = self.apex_generator.generate_service_class(service_name, sobject)
            components.append({
                "type": "ApexClass",
                "name": service_name,
                "content": service_code,
                "meta_content": self.apex_generator.generate_meta_xml(service_name)
            })

            # Selector
            if include_selector:
                selector_code = self.apex_generator.generate_selector_class(selector_name, sobject)
                components.append({
                    "type": "ApexClass",
                    "name": selector_name,
                    "content": selector_code,
                    "meta_content": self.apex_generator.generate_meta_xml(selector_name)
                })

            # Domain
            if include_domain:
                domain_code = self.apex_generator.generate_domain_class(domain_name, sobject)
                components.append({
                    "type": "ApexClass",
                    "name": domain_name,
                    "content": domain_code,
                    "meta_content": self.apex_generator.generate_meta_xml(domain_name)
                })

            # Trigger
            if include_trigger:
                trigger_code, handler_code = self.apex_generator.generate_trigger_with_handler(
                    sobject,
                    ["before insert", "after insert", "before update", "after update"]
                )

                trigger_name = f"{sobject}Trigger"
                handler_name = f"{sobject}TriggerHandler"

                components.append({
                    "type": "ApexTrigger",
                    "name": trigger_name,
                    "content": trigger_code,
                    "meta_content": self.apex_generator.generate_trigger_meta_xml(trigger_name)
                })

                components.append({
                    "type": "ApexClass",
                    "name": handler_name,
                    "content": handler_code,
                    "meta_content": self.apex_generator.generate_meta_xml(handler_name)
                })

            # Testes
            if include_tests:
                service_test = self.apex_generator.generate_test_class(
                    f"{service_name}Test",
                    service_name,
                    sobject=sobject
                )
                components.append({
                    "type": "ApexClass",
                    "name": f"{service_name}Test",
                    "content": service_test,
                    "meta_content": self.apex_generator.generate_meta_xml(f"{service_name}Test")
                })

            # Deploy
            result = await self.metadata_deployer.deploy_components(components)

            return SkillResult(
                success=result.success,
                data={
                    "components_deployed": result.components_deployed,
                    "components_failed": result.components_failed,
                    "components": [c["name"] for c in components]
                },
                message=f"Service layer para {sobject}: {result.components_deployed} componentes deployados",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== LWC ====================

    async def deploy_lwc(
        self,
        component_name: str,
        files: Dict[str, str]
    ) -> SkillResult:
        """
        Faz deploy de um componente LWC

        Args:
            component_name: Nome do componente
            files: Arquivos do componente

        Returns:
            SkillResult
        """
        try:
            result = await self.metadata_deployer.deploy_lwc(
                component_name,
                files
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Componente LWC {component_name} {'deployado' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_and_deploy_lwc(
        self,
        component_name: str,
        label: str,
        component_type: str = "basic",
        sobject: Optional[str] = None,
        fields: Optional[List[str]] = None
    ) -> SkillResult:
        """
        Gera e faz deploy de um componente LWC

        Args:
            component_name: Nome do componente
            label: Label do componente
            component_type: Tipo (basic, form, list)
            sobject: Objeto para form/list
            fields: Campos para form

        Returns:
            SkillResult
        """
        try:
            # Gerar componente
            if component_type == "form" and sobject:
                files = self.lwc_generator.generate_form_component(
                    component_name,
                    sobject,
                    fields or ["Name"],
                    label
                )
            elif component_type == "list" and sobject:
                files = self.lwc_generator.generate_list_component(
                    component_name,
                    sobject,
                    label=label
                )
            else:
                files = self.lwc_generator.generate_component(
                    component_name,
                    label
                )

            # Deploy
            return await self.deploy_lwc(component_name, files)

        except Exception as e:
            logger.error(f"Erro ao gerar e deployar LWC: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== OBJECTS ====================

    async def deploy_custom_object(
        self,
        object_name: str,
        label: str,
        fields: Optional[List[Dict[str, Any]]] = None
    ) -> SkillResult:
        """
        Faz deploy de um objeto customizado

        Args:
            object_name: Nome do objeto
            label: Label do objeto
            fields: Lista de campos

        Returns:
            SkillResult
        """
        try:
            # Gerar XML do objeto
            object_xml = self.object_generator.generate_custom_object(
                object_name,
                label,
                fields=fields
            )

            # Deploy
            result = await self.metadata_deployer.deploy_custom_object(
                object_name,
                object_xml
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Objeto {object_name} {'deployado' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def deploy_custom_field(
        self,
        object_name: str,
        field_name: str,
        field_type: str,
        label: str,
        **kwargs
    ) -> SkillResult:
        """
        Faz deploy de um campo customizado

        Args:
            object_name: Nome do objeto
            field_name: Nome do campo
            field_type: Tipo do campo
            label: Label do campo
            **kwargs: Outros atributos do campo

        Returns:
            SkillResult
        """
        try:
            field_def = {
                "name": field_name,
                "type": field_type,
                "label": label,
                **kwargs
            }

            field_xml = self.object_generator.generate_custom_field(
                object_name,
                field_def
            )

            result = await self.metadata_deployer.deploy_custom_field(
                object_name,
                field_name,
                field_xml
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Campo {object_name}.{field_name} {'deployado' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== FLOWS ====================

    async def deploy_flow(
        self,
        flow_name: str,
        flow_xml: str
    ) -> SkillResult:
        """
        Faz deploy de um Flow

        Args:
            flow_name: Nome do flow
            flow_xml: XML do flow

        Returns:
            SkillResult
        """
        try:
            result = await self.metadata_deployer.deploy_flow(
                flow_name,
                flow_xml
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Flow {flow_name} {'deployado' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro ao fazer deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== VALIDACAO ====================

    async def validate_components(
        self,
        components: List[Dict[str, Any]],
        run_tests: bool = True
    ) -> SkillResult:
        """
        Valida componentes sem fazer deploy

        Args:
            components: Lista de componentes
            run_tests: Executar testes

        Returns:
            SkillResult
        """
        try:
            from ..deployers.metadata_deployer import TestLevel

            test_level = TestLevel.RUN_LOCAL_TESTS if run_tests else TestLevel.NO_TEST_RUN

            result = await self.metadata_deployer.validate(
                components,
                test_level=test_level
            )

            return SkillResult(
                success=result.success,
                data={
                    "validation_id": result.id,
                    "components_valid": result.components_deployed,
                    "components_invalid": result.components_failed,
                    "test_results": result.test_results
                },
                message=f"Validacao {'passou' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro na validacao: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def quick_deploy(self, validation_id: str) -> SkillResult:
        """
        Faz quick deploy de uma validacao

        Args:
            validation_id: ID da validacao

        Returns:
            SkillResult
        """
        try:
            result = await self.metadata_deployer.quick_deploy(validation_id)

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Quick deploy {'concluido' if result.success else 'falhou'}",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro no quick deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== MULTIPLOS COMPONENTES ====================

    async def deploy_multiple(
        self,
        components: List[Dict[str, Any]],
        run_tests: bool = False,
        check_only: bool = False
    ) -> SkillResult:
        """
        Faz deploy de multiplos componentes

        Args:
            components: Lista de componentes
            run_tests: Executar testes
            check_only: Apenas validar

        Returns:
            SkillResult
        """
        try:
            from ..deployers.metadata_deployer import TestLevel

            test_level = TestLevel.RUN_LOCAL_TESTS if run_tests else TestLevel.NO_TEST_RUN

            result = await self.metadata_deployer.deploy_components(
                components,
                test_level=test_level,
                check_only=check_only
            )

            return SkillResult(
                success=result.success,
                data=result.to_dict(),
                message=f"Deploy de {len(components)} componentes: "
                        f"{result.components_deployed} OK, {result.components_failed} falhas",
                errors=result.errors
            )

        except Exception as e:
            logger.error(f"Erro no deploy: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

# -*- coding: utf-8 -*-
"""
SAP CPI iFlow Skill
===================

Skill para criação e modificação de iFlows SAP CPI.

Funcionalidades:
- Criar novos iFlows
- Atualizar iFlows existentes
- Gerar scripts Groovy
- Gerar mapeamentos
- Configurar adaptadores
"""

import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from ..sap_cpi import SAPCPIIntegration, get_sap_cpi_integration
from ..generators import IFlowGenerator, MappingGenerator, GroovyGenerator
from ..generators.iflow_generator import IFlowDefinition, ProcessStep, AdapterConfig, AdapterType, StepType
from ..generators.mapping_generator import MappingDefinition, StructureDefinition, MappingRule, FieldDefinition, DataType
from ..generators.groovy_generator import ScriptDefinition, ScriptPurpose

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado de execução de skill"""
    success: bool
    data: Any = None
    message: str = ""
    error: str = ""

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "error": self.error
        }


class CPIIFlowSkill:
    """
    Skill para criação e modificação de iFlows SAP CPI.

    Esta skill permite que agentes IA criem e modifiquem
    iFlows, scripts e mapeamentos.

    Exemplo:
    ```python
    skill = CPIIFlowSkill()

    # Gera iFlow HTTP simples
    result = await skill.generate_http_iflow(
        iflow_id="CustomerReplication",
        name="Customer Replication",
        source_path="/api/customer",
        target_url="https://erp.example.com/api/customer"
    )

    if result.success:
        # Resultado contém o ZIP do iFlow
        zip_content = result.data["content"]

        # Pode fazer deploy
        deploy_result = await skill.create_and_deploy(
            package_id="MyPackage",
            iflow_id="CustomerReplication",
            ...
        )
    ```
    """

    def __init__(self, integration: Optional[SAPCPIIntegration] = None):
        """
        Inicializa a skill.

        Args:
            integration: Integração SAP CPI (usa global se não fornecida)
        """
        self.integration = integration or get_sap_cpi_integration()
        self.iflow_generator = IFlowGenerator()
        self.mapping_generator = MappingGenerator()
        self.groovy_generator = GroovyGenerator()

    async def connect(self) -> SkillResult:
        """Conecta ao SAP CPI"""
        try:
            if await self.integration.connect():
                return SkillResult(
                    success=True,
                    message="Conectado ao SAP CPI"
                )
            else:
                return SkillResult(
                    success=False,
                    error=self.integration.last_error or "Falha na conexão"
                )
        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao conectar: {str(e)}"
            )

    def generate_http_iflow(
        self,
        iflow_id: str,
        name: str,
        source_path: str,
        target_url: str,
        description: str = "",
        include_logging: bool = True,
        include_error_handling: bool = True
    ) -> SkillResult:
        """
        Gera um iFlow HTTP para HTTP simples.

        Args:
            iflow_id: ID do iFlow
            name: Nome do iFlow
            source_path: Path do endpoint de entrada
            target_url: URL do sistema destino
            description: Descrição
            include_logging: Incluir script de logging
            include_error_handling: Incluir tratamento de erros

        Returns:
            SkillResult com ZIP do iFlow gerado
        """
        try:
            # Cria definição do iFlow
            definition = IFlowGenerator.create_http_to_http_iflow(
                iflow_id=iflow_id,
                name=name,
                source_path=source_path,
                target_url=target_url,
                description=description
            )

            definition.exception_handling = include_error_handling

            # Scripts a incluir
            scripts = {}

            if include_logging:
                logging_script = self.groovy_generator.generate_logging_script(
                    name="logging",
                    log_headers=True,
                    log_properties=True
                )
                scripts["logging.groovy"] = logging_script

                definition.processing_steps.insert(0, ProcessStep(
                    step_type=StepType.GROOVY_SCRIPT,
                    name="Log Request",
                    script_path="script/logging.groovy"
                ))

            if include_error_handling:
                error_script = self.groovy_generator.generate_error_handler_script(
                    name="error_handler",
                    error_response_format="json"
                )
                scripts["error_handler.groovy"] = error_script

            # Gera ZIP
            zip_content = self.iflow_generator.generate(
                definition,
                include_scripts=scripts
            )

            return SkillResult(
                success=True,
                data={
                    "content": zip_content,
                    "iflow_id": iflow_id,
                    "size_bytes": len(zip_content)
                },
                message=f"iFlow {iflow_id} gerado com sucesso ({len(zip_content)} bytes)"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar iFlow: {str(e)}"
            )

    def generate_sftp_to_sap_iflow(
        self,
        iflow_id: str,
        name: str,
        sftp_directory: str,
        file_pattern: str,
        rfc_function: str,
        description: str = ""
    ) -> SkillResult:
        """
        Gera um iFlow SFTP para SAP (RFC/BAPI).

        Args:
            iflow_id: ID do iFlow
            name: Nome do iFlow
            sftp_directory: Diretório SFTP para polling
            file_pattern: Padrão de arquivo (ex: *.xml)
            rfc_function: Nome da função RFC/BAPI
            description: Descrição

        Returns:
            SkillResult com ZIP do iFlow gerado
        """
        try:
            definition = IFlowGenerator.create_sftp_to_sap_iflow(
                iflow_id=iflow_id,
                name=name,
                sftp_directory=sftp_directory,
                file_pattern=file_pattern,
                rfc_function=rfc_function,
                description=description
            )

            zip_content = self.iflow_generator.generate(definition)

            return SkillResult(
                success=True,
                data={
                    "content": zip_content,
                    "iflow_id": iflow_id,
                    "size_bytes": len(zip_content)
                },
                message=f"iFlow {iflow_id} gerado com sucesso"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar iFlow: {str(e)}"
            )

    def generate_custom_iflow(
        self,
        iflow_id: str,
        name: str,
        sender_adapter_type: str,
        sender_address: str,
        receiver_adapter_type: str,
        receiver_address: str,
        processing_steps: List[Dict[str, Any]],
        description: str = "",
        externalized_params: Dict[str, str] = None
    ) -> SkillResult:
        """
        Gera um iFlow customizado com configuração completa.

        Args:
            iflow_id: ID do iFlow
            name: Nome do iFlow
            sender_adapter_type: Tipo do adaptador de entrada (HTTP, SFTP, etc)
            sender_address: Endereço/path do sender
            receiver_adapter_type: Tipo do adaptador de saída
            receiver_address: Endereço do receiver
            processing_steps: Lista de steps de processamento
            description: Descrição
            externalized_params: Parâmetros externalizados

        Returns:
            SkillResult com ZIP do iFlow gerado
        """
        try:
            # Converte tipos de adaptador
            sender_type = AdapterType(sender_adapter_type)
            receiver_type = AdapterType(receiver_adapter_type)

            # Converte steps
            steps = []
            for step_data in processing_steps:
                step = ProcessStep(
                    step_type=StepType(step_data.get("type", "ContentModifier")),
                    name=step_data.get("name", ""),
                    description=step_data.get("description", ""),
                    properties=step_data.get("properties", {}),
                    script_path=step_data.get("script_path", ""),
                    mapping_path=step_data.get("mapping_path", "")
                )
                steps.append(step)

            definition = IFlowDefinition(
                id=iflow_id,
                name=name,
                description=description,
                sender_adapter=AdapterConfig(
                    adapter_type=sender_type,
                    address=sender_address
                ),
                receiver_adapter=AdapterConfig(
                    adapter_type=receiver_type,
                    address=receiver_address
                ),
                processing_steps=steps,
                externalized_params=externalized_params or {}
            )

            zip_content = self.iflow_generator.generate(definition)

            return SkillResult(
                success=True,
                data={
                    "content": zip_content,
                    "iflow_id": iflow_id,
                    "size_bytes": len(zip_content)
                },
                message=f"iFlow customizado {iflow_id} gerado"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar iFlow: {str(e)}"
            )

    def generate_groovy_script(
        self,
        name: str,
        purpose: str,
        body: str,
        description: str = ""
    ) -> SkillResult:
        """
        Gera um script Groovy para CPI.

        Args:
            name: Nome do script
            purpose: Propósito (processor, validator, transformer, etc)
            body: Corpo do script
            description: Descrição

        Returns:
            SkillResult com código Groovy
        """
        try:
            script_purpose = ScriptPurpose(purpose)

            script = self.groovy_generator.generate_processor_script(
                name=name,
                body=body,
                description=description
            )

            return SkillResult(
                success=True,
                data={
                    "content": script,
                    "name": name,
                    "purpose": purpose
                },
                message=f"Script {name} gerado"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar script: {str(e)}"
            )

    def generate_validation_script(
        self,
        name: str,
        required_fields: List[str],
        field_validators: Dict[str, str] = None,
        message_format: str = "xml"
    ) -> SkillResult:
        """
        Gera um script de validação.

        Args:
            name: Nome do script
            required_fields: Lista de campos obrigatórios
            field_validators: Validadores customizados {campo: expressão}
            message_format: Formato da mensagem (xml/json)

        Returns:
            SkillResult com código Groovy
        """
        try:
            script = self.groovy_generator.generate_validation_script(
                name=name,
                required_fields=required_fields,
                field_validators=field_validators,
                message_format=message_format
            )

            return SkillResult(
                success=True,
                data={
                    "content": script,
                    "name": name,
                    "fields_validated": len(required_fields)
                },
                message=f"Script de validação {name} gerado"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar script: {str(e)}"
            )

    def generate_transformation_script(
        self,
        name: str,
        source_format: str,
        target_format: str,
        field_mappings: Dict[str, str]
    ) -> SkillResult:
        """
        Gera um script de transformação.

        Args:
            name: Nome do script
            source_format: Formato fonte (xml/json)
            target_format: Formato destino (xml/json)
            field_mappings: Mapeamento de campos {destino: fonte}

        Returns:
            SkillResult com código Groovy
        """
        try:
            script = self.groovy_generator.generate_transformation_script(
                name=name,
                source_format=source_format,
                target_format=target_format,
                field_mappings=field_mappings
            )

            return SkillResult(
                success=True,
                data={
                    "content": script,
                    "name": name,
                    "transformation": f"{source_format} -> {target_format}",
                    "fields_mapped": len(field_mappings)
                },
                message=f"Script de transformação {name} gerado"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar script: {str(e)}"
            )

    def generate_mapping(
        self,
        mapping_id: str,
        name: str,
        source_fields: List[Dict[str, Any]],
        target_fields: List[Dict[str, Any]],
        auto_map: bool = True
    ) -> SkillResult:
        """
        Gera um Message Mapping.

        Args:
            mapping_id: ID do mapeamento
            name: Nome do mapeamento
            source_fields: Lista de campos fonte [{name, path, type}]
            target_fields: Lista de campos destino [{name, path, type}]
            auto_map: Mapear automaticamente campos com nomes similares

        Returns:
            SkillResult com XML do mapeamento
        """
        try:
            xml_content = self.mapping_generator.generate_from_schemas(
                mapping_id=mapping_id,
                name=name,
                source_fields=source_fields,
                target_fields=target_fields,
                auto_map=auto_map
            )

            return SkillResult(
                success=True,
                data={
                    "content": xml_content,
                    "mapping_id": mapping_id,
                    "source_fields": len(source_fields),
                    "target_fields": len(target_fields)
                },
                message=f"Mapping {mapping_id} gerado"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao gerar mapping: {str(e)}"
            )

    async def create_iflow(
        self,
        package_id: str,
        iflow_id: str,
        iflow_name: str,
        iflow_content: bytes,
        description: str = ""
    ) -> SkillResult:
        """
        Cria um novo iFlow no CPI.

        Args:
            package_id: ID do package destino
            iflow_id: ID do novo iFlow
            iflow_name: Nome do iFlow
            iflow_content: Conteúdo ZIP do iFlow
            description: Descrição

        Returns:
            SkillResult indicando sucesso ou erro
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            iflow = await self.integration.iflow_manager.create_iflow(
                package_id=package_id,
                iflow_id=iflow_id,
                iflow_name=iflow_name,
                iflow_content=iflow_content,
                description=description
            )

            if iflow is None:
                return SkillResult(
                    success=False,
                    error=self.integration.client.last_error or "Falha ao criar iFlow"
                )

            return SkillResult(
                success=True,
                data=iflow.to_dict(),
                message=f"iFlow {iflow_id} criado com sucesso"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao criar iFlow: {str(e)}"
            )

    async def update_iflow(
        self,
        iflow_id: str,
        version: str,
        iflow_content: bytes
    ) -> SkillResult:
        """
        Atualiza um iFlow existente.

        Args:
            iflow_id: ID do iFlow
            version: Versão atual
            iflow_content: Novo conteúdo ZIP

        Returns:
            SkillResult indicando sucesso ou erro
        """
        try:
            if not self.integration.is_connected:
                return SkillResult(
                    success=False,
                    error="Não conectado ao SAP CPI"
                )

            success = await self.integration.iflow_manager.update_iflow(
                iflow_id=iflow_id,
                version=version,
                iflow_content=iflow_content
            )

            if not success:
                return SkillResult(
                    success=False,
                    error=self.integration.client.last_error or "Falha ao atualizar iFlow"
                )

            return SkillResult(
                success=True,
                message=f"iFlow {iflow_id} atualizado com sucesso"
            )

        except Exception as e:
            return SkillResult(
                success=False,
                error=f"Erro ao atualizar iFlow: {str(e)}"
            )

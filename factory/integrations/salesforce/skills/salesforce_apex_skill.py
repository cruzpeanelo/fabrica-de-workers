# -*- coding: utf-8 -*-
"""
Salesforce Apex Skill
=====================
Skill para desenvolvimento e gerenciamento de codigo Apex.

Funcionalidades:
- Geracao de codigo Apex
- Execucao de codigo anonimo
- Execucao de testes
- Analise de codigo

Uso pelos agentes:
    from factory.integrations.salesforce.skills import SalesforceApexSkill

    skill = SalesforceApexSkill(sf_client)

    # Gerar classe service
    result = await skill.generate_service("AccountService", "Account")

    # Executar testes
    result = await skill.run_tests(["AccountServiceTest"])
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado de uma skill"""
    success: bool
    data: Any = None
    message: str = ""
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "errors": self.errors
        }


class SalesforceApexSkill:
    """
    Skill de desenvolvimento Apex

    Fornece funcionalidades de geracao e execucao
    de codigo Apex para agentes especializados.
    """

    def __init__(self, sf_client):
        """
        Inicializa a skill

        Args:
            sf_client: SalesforceClient autenticado
        """
        self.sf = sf_client
        self._tooling = None
        self._apex_generator = None

    @property
    def tooling(self):
        if self._tooling is None:
            from ..tooling_client import ToolingClient
            self._tooling = ToolingClient(self.sf)
        return self._tooling

    @property
    def apex_generator(self):
        if self._apex_generator is None:
            from ..generators import ApexGenerator
            self._apex_generator = ApexGenerator()
        return self._apex_generator

    # ==================== GERACAO DE CODIGO ====================

    async def generate_service(
        self,
        class_name: str,
        sobject: str,
        include_crud: bool = True
    ) -> SkillResult:
        """
        Gera classe Service

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            include_crud: Incluir metodos CRUD

        Returns:
            SkillResult com codigo gerado
        """
        try:
            code = self.apex_generator.generate_service_class(
                class_name,
                sobject,
                include_crud=include_crud
            )

            meta_xml = self.apex_generator.generate_meta_xml(class_name)

            return SkillResult(
                success=True,
                data={
                    "class_name": class_name,
                    "code": code,
                    "meta_xml": meta_xml
                },
                message=f"Classe Service {class_name} gerada"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar service: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_selector(
        self,
        class_name: str,
        sobject: str,
        fields: Optional[List[str]] = None
    ) -> SkillResult:
        """
        Gera classe Selector

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            fields: Campos a selecionar

        Returns:
            SkillResult com codigo gerado
        """
        try:
            code = self.apex_generator.generate_selector_class(
                class_name,
                sobject,
                fields=fields
            )

            meta_xml = self.apex_generator.generate_meta_xml(class_name)

            return SkillResult(
                success=True,
                data={
                    "class_name": class_name,
                    "code": code,
                    "meta_xml": meta_xml
                },
                message=f"Classe Selector {class_name} gerada"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar selector: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_domain(
        self,
        class_name: str,
        sobject: str,
        validations: Optional[List[Dict[str, Any]]] = None
    ) -> SkillResult:
        """
        Gera classe Domain

        Args:
            class_name: Nome da classe
            sobject: Objeto principal
            validations: Regras de validacao

        Returns:
            SkillResult com codigo gerado
        """
        try:
            code = self.apex_generator.generate_domain_class(
                class_name,
                sobject,
                validations=validations
            )

            meta_xml = self.apex_generator.generate_meta_xml(class_name)

            return SkillResult(
                success=True,
                data={
                    "class_name": class_name,
                    "code": code,
                    "meta_xml": meta_xml
                },
                message=f"Classe Domain {class_name} gerada"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar domain: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_trigger_with_handler(
        self,
        sobject: str,
        events: List[str]
    ) -> SkillResult:
        """
        Gera trigger e handler

        Args:
            sobject: Objeto do trigger
            events: Eventos do trigger

        Returns:
            SkillResult com codigo gerado
        """
        try:
            trigger_code, handler_code = self.apex_generator.generate_trigger_with_handler(
                sobject,
                events
            )

            trigger_name = f"{sobject}Trigger"
            handler_name = f"{sobject}TriggerHandler"

            return SkillResult(
                success=True,
                data={
                    "trigger": {
                        "name": trigger_name,
                        "code": trigger_code,
                        "meta_xml": self.apex_generator.generate_trigger_meta_xml(trigger_name)
                    },
                    "handler": {
                        "name": handler_name,
                        "code": handler_code,
                        "meta_xml": self.apex_generator.generate_meta_xml(handler_name)
                    }
                },
                message=f"Trigger e handler para {sobject} gerados"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar trigger: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_test_class(
        self,
        class_name: str,
        target_class: str,
        sobject: Optional[str] = None
    ) -> SkillResult:
        """
        Gera classe de teste

        Args:
            class_name: Nome da classe de teste
            target_class: Classe sendo testada
            sobject: Objeto para test data

        Returns:
            SkillResult com codigo gerado
        """
        try:
            code = self.apex_generator.generate_test_class(
                class_name,
                target_class,
                sobject=sobject
            )

            meta_xml = self.apex_generator.generate_meta_xml(class_name)

            return SkillResult(
                success=True,
                data={
                    "class_name": class_name,
                    "code": code,
                    "meta_xml": meta_xml
                },
                message=f"Classe de teste {class_name} gerada"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar teste: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_batch(
        self,
        class_name: str,
        sobject: str,
        query: Optional[str] = None
    ) -> SkillResult:
        """
        Gera classe Batch

        Args:
            class_name: Nome da classe
            sobject: Objeto processado
            query: Query para selecionar registros

        Returns:
            SkillResult com codigo gerado
        """
        try:
            code = self.apex_generator.generate_batch_class(
                class_name,
                sobject,
                query=query
            )

            meta_xml = self.apex_generator.generate_meta_xml(class_name)

            return SkillResult(
                success=True,
                data={
                    "class_name": class_name,
                    "code": code,
                    "meta_xml": meta_xml
                },
                message=f"Classe Batch {class_name} gerada"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar batch: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def generate_rest_resource(
        self,
        class_name: str,
        url_mapping: str,
        sobject: Optional[str] = None
    ) -> SkillResult:
        """
        Gera classe REST Resource

        Args:
            class_name: Nome da classe
            url_mapping: URL mapping
            sobject: Objeto principal

        Returns:
            SkillResult com codigo gerado
        """
        try:
            code = self.apex_generator.generate_rest_resource(
                class_name,
                url_mapping,
                sobject=sobject
            )

            meta_xml = self.apex_generator.generate_meta_xml(class_name)

            return SkillResult(
                success=True,
                data={
                    "class_name": class_name,
                    "code": code,
                    "meta_xml": meta_xml
                },
                message=f"REST Resource {class_name} gerada"
            )

        except Exception as e:
            logger.error(f"Erro ao gerar REST resource: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== EXECUCAO ====================

    async def execute_anonymous(self, apex_code: str) -> SkillResult:
        """
        Executa codigo Apex anonimo

        Args:
            apex_code: Codigo Apex

        Returns:
            SkillResult com resultado da execucao
        """
        try:
            result = await self.tooling.execute_anonymous(apex_code)

            return SkillResult(
                success=result.success,
                data={
                    "compiled": result.compiled,
                    "logs": result.logs
                },
                message="Codigo executado com sucesso" if result.success else result.exception_message or result.compile_problem,
                errors=[result.exception_message or result.compile_problem] if not result.success else []
            )

        except Exception as e:
            logger.error(f"Erro ao executar codigo: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def run_tests(
        self,
        class_names: List[str],
        code_coverage: bool = True
    ) -> SkillResult:
        """
        Executa testes Apex

        Args:
            class_names: Nomes das classes de teste
            code_coverage: Incluir coverage

        Returns:
            SkillResult com resultados dos testes
        """
        try:
            result = await self.tooling.run_tests_and_wait(class_names)

            return SkillResult(
                success=result.success,
                data={
                    "tests_run": result.num_tests_run,
                    "failures": result.num_failures,
                    "total_time": result.total_time,
                    "test_failures": result.test_failures,
                    "test_successes": result.test_successes,
                    "code_coverage": result.code_coverage
                },
                message=f"Testes executados: {result.num_tests_run}, "
                        f"Falhas: {result.num_failures}"
            )

        except Exception as e:
            logger.error(f"Erro ao executar testes: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_code_coverage(
        self,
        class_names: Optional[List[str]] = None
    ) -> SkillResult:
        """
        Obtem coverage de codigo

        Args:
            class_names: Filtrar por classes

        Returns:
            SkillResult com coverage
        """
        try:
            coverage = await self.tooling.get_code_coverage(class_names)

            return SkillResult(
                success=True,
                data=coverage,
                message=f"Coverage obtido para {len(coverage)} classe(s)"
            )

        except Exception as e:
            logger.error(f"Erro ao obter coverage: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== DEBUG ====================

    async def enable_debug_logs(
        self,
        duration_minutes: int = 30
    ) -> SkillResult:
        """
        Habilita debug logs

        Args:
            duration_minutes: Duracao em minutos

        Returns:
            SkillResult com ID do trace flag
        """
        try:
            trace_flag_id = await self.tooling.create_trace_flag(
                duration_minutes=duration_minutes
            )

            return SkillResult(
                success=True,
                data={"trace_flag_id": trace_flag_id},
                message=f"Debug logs habilitados por {duration_minutes} minutos"
            )

        except Exception as e:
            logger.error(f"Erro ao habilitar logs: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_debug_logs(self, limit: int = 10) -> SkillResult:
        """
        Lista debug logs

        Args:
            limit: Numero maximo de logs

        Returns:
            SkillResult com logs
        """
        try:
            logs = await self.tooling.get_debug_logs(limit=limit)

            return SkillResult(
                success=True,
                data=logs,
                message=f"Obtidos {len(logs)} debug logs"
            )

        except Exception as e:
            logger.error(f"Erro ao obter logs: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_debug_log_body(self, log_id: str) -> SkillResult:
        """
        Obtem conteudo de um debug log

        Args:
            log_id: ID do log

        Returns:
            SkillResult com conteudo do log
        """
        try:
            body = await self.tooling.get_debug_log_body(log_id)

            return SkillResult(
                success=True,
                data=body,
                message="Conteudo do log obtido"
            )

        except Exception as e:
            logger.error(f"Erro ao obter log: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== CRUD DE CLASSES ====================

    async def create_class(
        self,
        class_name: str,
        body: str
    ) -> SkillResult:
        """
        Cria classe Apex

        Args:
            class_name: Nome da classe
            body: Codigo Apex

        Returns:
            SkillResult com ID da classe
        """
        try:
            result = await self.tooling.create_apex_class(class_name, body)

            return SkillResult(
                success=result.get("success", False),
                data=result,
                message=f"Classe {class_name} criada" if result.get("success") else "Erro ao criar classe",
                errors=result.get("errors", [])
            )

        except Exception as e:
            logger.error(f"Erro ao criar classe: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def update_class(
        self,
        class_name: str,
        body: str
    ) -> SkillResult:
        """
        Atualiza classe Apex

        Args:
            class_name: Nome da classe
            body: Novo codigo

        Returns:
            SkillResult
        """
        try:
            # Buscar ID da classe
            apex_class = await self.tooling.get_apex_class_by_name(class_name)

            if not apex_class:
                return SkillResult(
                    success=False,
                    message=f"Classe {class_name} nao encontrada"
                )

            await self.tooling.update_apex_class(apex_class.id, body)

            return SkillResult(
                success=True,
                data={"id": apex_class.id},
                message=f"Classe {class_name} atualizada"
            )

        except Exception as e:
            logger.error(f"Erro ao atualizar classe: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def delete_class(self, class_name: str) -> SkillResult:
        """
        Deleta classe Apex

        Args:
            class_name: Nome da classe

        Returns:
            SkillResult
        """
        try:
            apex_class = await self.tooling.get_apex_class_by_name(class_name)

            if not apex_class:
                return SkillResult(
                    success=False,
                    message=f"Classe {class_name} nao encontrada"
                )

            await self.tooling.delete_apex_class(apex_class.id)

            return SkillResult(
                success=True,
                message=f"Classe {class_name} deletada"
            )

        except Exception as e:
            logger.error(f"Erro ao deletar classe: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

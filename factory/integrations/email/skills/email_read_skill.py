# -*- coding: utf-8 -*-
"""
Email Read Skill
================
Skill para leitura e processamento de emails, usada por agentes Claude.

Esta skill permite que agentes leiam e processem emails para
extrair requisitos e criar projetos automaticamente.

Uso:
    skill = EmailReadSkill(graph_client)

    # Buscar emails nao lidos
    result = await skill.execute({
        "action": "get_unread",
        "folder": "Inbox"
    })

    # Buscar solicitacoes de projeto
    result = await skill.execute({
        "action": "get_project_requests"
    })

    # Processar email especifico
    result = await skill.execute({
        "action": "process_email",
        "email_id": "xxx"
    })
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class ReadAction(str, Enum):
    """Acoes de leitura disponiveis"""
    GET_UNREAD = "get_unread"
    GET_MESSAGES = "get_messages"
    GET_MESSAGE = "get_message"
    GET_PROJECT_REQUESTS = "get_project_requests"
    GET_REQUIREMENT_EMAILS = "get_requirement_emails"
    PROCESS_EMAIL = "process_email"
    PROCESS_ATTACHMENTS = "process_attachments"
    EXTRACT_REQUIREMENTS = "extract_requirements"
    MARK_AS_READ = "mark_as_read"


@dataclass
class SkillResult:
    """Resultado da execucao da skill"""
    success: bool
    action: str
    message: str
    data: Optional[Dict[str, Any]] = None


class EmailReadSkill:
    """
    Skill de leitura de emails para agentes.

    Fornece uma interface simplificada para agentes lerem
    e processarem emails durante a execucao de tarefas.

    Exemplo para agente:
        # No contexto do agente Claude
        tools = [email_read_skill.get_tool_definition()]

        # Agente pode chamar:
        # read_email(action="get_project_requests")
    """

    SKILL_NAME = "email_read"
    SKILL_DESCRIPTION = """
    Skill para leitura e processamento de emails.

    Acoes disponiveis:
    - get_unread: Buscar emails nao lidos
    - get_messages: Buscar mensagens com filtros
    - get_message: Buscar mensagem especifica
    - get_project_requests: Buscar solicitacoes de projeto
    - get_requirement_emails: Buscar emails com requisitos
    - process_email: Processar email e extrair dados
    - process_attachments: Processar anexos de email
    - extract_requirements: Extrair requisitos de email
    - mark_as_read: Marcar email como lido
    """

    def __init__(self, email_client):
        """
        Inicializa a skill.

        Args:
            email_client: Cliente de email (Graph)
        """
        self.client = email_client
        self._inbox_reader = None
        self._attachment_processor = None

    def _get_inbox_reader(self):
        """Obtem leitor de inbox (lazy loading)"""
        if self._inbox_reader is None:
            from ..readers.inbox_reader import InboxReader
            self._inbox_reader = InboxReader(self.client)
        return self._inbox_reader

    def _get_attachment_processor(self):
        """Obtem processador de anexos (lazy loading)"""
        if self._attachment_processor is None:
            from ..readers.attachment_processor import AttachmentProcessor
            self._attachment_processor = AttachmentProcessor(self.client)
        return self._attachment_processor

    def get_tool_definition(self) -> Dict[str, Any]:
        """
        Retorna definicao da ferramenta para uso com Claude.

        Returns:
            Dict com schema da ferramenta
        """
        return {
            "name": "read_email",
            "description": self.SKILL_DESCRIPTION,
            "input_schema": {
                "type": "object",
                "properties": {
                    "action": {
                        "type": "string",
                        "enum": [e.value for e in ReadAction],
                        "description": "Acao a executar"
                    },
                    "folder": {
                        "type": "string",
                        "description": "Pasta de email (Inbox, SentItems, etc.)",
                        "default": "Inbox"
                    },
                    "email_id": {
                        "type": "string",
                        "description": "ID do email (para acoes especificas)"
                    },
                    "max_results": {
                        "type": "integer",
                        "description": "Numero maximo de resultados",
                        "default": 10
                    },
                    "days_back": {
                        "type": "integer",
                        "description": "Buscar emails dos ultimos N dias",
                        "default": 7
                    },
                    "unread_only": {
                        "type": "boolean",
                        "description": "Apenas emails nao lidos",
                        "default": True
                    },
                    "subject_contains": {
                        "type": "string",
                        "description": "Filtrar por assunto"
                    },
                    "from_address": {
                        "type": "string",
                        "description": "Filtrar por remetente"
                    },
                    "project_id": {
                        "type": "string",
                        "description": "ID do projeto (para filtrar requisitos)"
                    },
                    "download_attachments": {
                        "type": "boolean",
                        "description": "Baixar anexos",
                        "default": True
                    },
                    "extract_text": {
                        "type": "boolean",
                        "description": "Extrair texto de anexos",
                        "default": True
                    }
                },
                "required": ["action"]
            }
        }

    async def execute(self, params: Dict[str, Any]) -> SkillResult:
        """
        Executa a skill com os parametros fornecidos.

        Args:
            params: Parametros da acao

        Returns:
            SkillResult com resultado da execucao
        """
        action = params.get("action", "")

        try:
            action_enum = ReadAction(action)
        except ValueError:
            return SkillResult(
                success=False,
                action=action,
                message=f"Acao desconhecida: {action}"
            )

        try:
            # Roteia para o metodo correto
            if action_enum == ReadAction.GET_UNREAD:
                return await self._action_get_unread(params)

            elif action_enum == ReadAction.GET_MESSAGES:
                return await self._action_get_messages(params)

            elif action_enum == ReadAction.GET_MESSAGE:
                return await self._action_get_message(params)

            elif action_enum == ReadAction.GET_PROJECT_REQUESTS:
                return await self._action_get_project_requests(params)

            elif action_enum == ReadAction.GET_REQUIREMENT_EMAILS:
                return await self._action_get_requirement_emails(params)

            elif action_enum == ReadAction.PROCESS_EMAIL:
                return await self._action_process_email(params)

            elif action_enum == ReadAction.PROCESS_ATTACHMENTS:
                return await self._action_process_attachments(params)

            elif action_enum == ReadAction.EXTRACT_REQUIREMENTS:
                return await self._action_extract_requirements(params)

            elif action_enum == ReadAction.MARK_AS_READ:
                return await self._action_mark_as_read(params)

            else:
                return SkillResult(
                    success=False,
                    action=action,
                    message=f"Acao nao implementada: {action}"
                )

        except Exception as e:
            logger.exception(f"Erro na skill de leitura: {e}")
            return SkillResult(
                success=False,
                action=action,
                message=f"Erro ao executar: {str(e)}"
            )

    async def _action_get_unread(self, params: Dict[str, Any]) -> SkillResult:
        """Busca emails nao lidos"""
        folder = params.get("folder", "Inbox")
        max_results = params.get("max_results", 10)

        reader = self._get_inbox_reader()
        emails = await reader.get_unread_emails(folder, max_results)

        return SkillResult(
            success=True,
            action="get_unread",
            message=f"Encontrados {len(emails)} emails nao lidos",
            data={
                "count": len(emails),
                "emails": [self._email_to_dict(e) for e in emails]
            }
        )

    async def _action_get_messages(self, params: Dict[str, Any]) -> SkillResult:
        """Busca mensagens com filtros"""
        from ..readers.inbox_reader import EmailFilter

        filter_config = EmailFilter(
            folder=params.get("folder", "Inbox"),
            unread_only=params.get("unread_only", False),
            max_results=params.get("max_results", 20),
            subject_contains=params.get("subject_contains")
        )

        if params.get("from_address"):
            filter_config.from_addresses = [params["from_address"]]

        reader = self._get_inbox_reader()
        emails = await reader.get_emails(filter_config)

        return SkillResult(
            success=True,
            action="get_messages",
            message=f"Encontradas {len(emails)} mensagens",
            data={
                "count": len(emails),
                "emails": [self._email_to_dict(e) for e in emails]
            }
        )

    async def _action_get_message(self, params: Dict[str, Any]) -> SkillResult:
        """Busca mensagem especifica"""
        email_id = params.get("email_id", "")

        if not email_id:
            return SkillResult(
                success=False,
                action="get_message",
                message="email_id e obrigatorio"
            )

        message = await self.client.get_message(email_id)

        if message:
            from ..readers.inbox_reader import ProcessedEmail
            email = ProcessedEmail.from_graph_message(message)

            return SkillResult(
                success=True,
                action="get_message",
                message="Mensagem encontrada",
                data=self._email_to_dict(email, include_body=True)
            )
        else:
            return SkillResult(
                success=False,
                action="get_message",
                message="Mensagem nao encontrada"
            )

    async def _action_get_project_requests(self, params: Dict[str, Any]) -> SkillResult:
        """Busca solicitacoes de projeto"""
        days_back = params.get("days_back", 7)
        max_results = params.get("max_results", 10)

        reader = self._get_inbox_reader()
        emails = await reader.get_project_requests(days_back, max_results)

        return SkillResult(
            success=True,
            action="get_project_requests",
            message=f"Encontradas {len(emails)} solicitacoes de projeto",
            data={
                "count": len(emails),
                "emails": [self._email_to_dict(e) for e in emails]
            }
        )

    async def _action_get_requirement_emails(self, params: Dict[str, Any]) -> SkillResult:
        """Busca emails com requisitos"""
        project_id = params.get("project_id")
        days_back = params.get("days_back", 7)

        reader = self._get_inbox_reader()
        emails = await reader.get_requirement_emails(project_id, days_back)

        return SkillResult(
            success=True,
            action="get_requirement_emails",
            message=f"Encontrados {len(emails)} emails com requisitos",
            data={
                "count": len(emails),
                "emails": [self._email_to_dict(e) for e in emails]
            }
        )

    async def _action_process_email(self, params: Dict[str, Any]) -> SkillResult:
        """Processa email e extrai dados de projeto"""
        email_id = params.get("email_id", "")

        if not email_id:
            return SkillResult(
                success=False,
                action="process_email",
                message="email_id e obrigatorio"
            )

        # Busca mensagem
        message = await self.client.get_message(email_id)
        if not message:
            return SkillResult(
                success=False,
                action="process_email",
                message="Mensagem nao encontrada"
            )

        from ..readers.inbox_reader import ProcessedEmail
        email = ProcessedEmail.from_graph_message(message)

        # Extrai requisitos
        reader = self._get_inbox_reader()
        project_data = await reader.extract_project_requirements(email)

        # Processa anexos se solicitado
        attachments_data = None
        if params.get("download_attachments", True) and email.has_attachments:
            processor = self._get_attachment_processor()
            attachments_data = await processor.process_email_attachments(
                email_id,
                extract_text=params.get("extract_text", True)
            )

            # Adiciona requisitos dos anexos
            if attachments_data.get("requirements"):
                existing_reqs = set(project_data.get("requirements", []))
                for req in attachments_data["requirements"]:
                    if req not in existing_reqs:
                        project_data.setdefault("requirements", []).append(req)

            # Adiciona user stories dos anexos
            if attachments_data.get("user_stories"):
                project_data["user_stories"] = attachments_data["user_stories"]

        return SkillResult(
            success=True,
            action="process_email",
            message="Email processado com sucesso",
            data={
                "email": self._email_to_dict(email),
                "project_data": project_data,
                "attachments": attachments_data
            }
        )

    async def _action_process_attachments(self, params: Dict[str, Any]) -> SkillResult:
        """Processa anexos de um email"""
        email_id = params.get("email_id", "")

        if not email_id:
            return SkillResult(
                success=False,
                action="process_attachments",
                message="email_id e obrigatorio"
            )

        processor = self._get_attachment_processor()
        result = await processor.process_email_attachments(
            email_id,
            extract_text=params.get("extract_text", True),
            save_files=params.get("save_files", False)
        )

        return SkillResult(
            success=True,
            action="process_attachments",
            message=f"Processados {len(result.get('attachments', []))} anexos",
            data=result
        )

    async def _action_extract_requirements(self, params: Dict[str, Any]) -> SkillResult:
        """Extrai requisitos de texto"""
        text = params.get("text", "")
        email_id = params.get("email_id", "")

        requirements = []
        user_stories = []

        processor = self._get_attachment_processor()

        if text:
            requirements = processor.extract_requirements(text)
            user_stories = processor.extract_user_stories(text)

        elif email_id:
            # Busca email e extrai
            message = await self.client.get_message(email_id)
            if message:
                from ..readers.inbox_reader import ProcessedEmail
                email = ProcessedEmail.from_graph_message(message)

                # Extrai do corpo
                body_text = processor._strip_html(email.body) if hasattr(processor, '_strip_html') else email.body
                requirements = processor.extract_requirements(body_text)
                user_stories = processor.extract_user_stories(body_text)

        return SkillResult(
            success=True,
            action="extract_requirements",
            message=f"Extraidos {len(requirements)} requisitos e {len(user_stories)} user stories",
            data={
                "requirements": requirements,
                "user_stories": user_stories
            }
        )

    async def _action_mark_as_read(self, params: Dict[str, Any]) -> SkillResult:
        """Marca email como lido"""
        email_id = params.get("email_id", "")

        if not email_id:
            return SkillResult(
                success=False,
                action="mark_as_read",
                message="email_id e obrigatorio"
            )

        success = await self.client.mark_as_read(email_id, True)

        return SkillResult(
            success=success,
            action="mark_as_read",
            message="Email marcado como lido" if success else "Falha ao marcar"
        )

    def _email_to_dict(
        self,
        email,
        include_body: bool = False
    ) -> Dict[str, Any]:
        """Converte ProcessedEmail para dict"""
        data = {
            "id": email.id,
            "subject": email.subject,
            "from_address": email.from_address,
            "from_name": email.from_name,
            "to": email.to_addresses,
            "received_at": email.received_at.isoformat() if email.received_at else None,
            "is_read": email.is_read,
            "has_attachments": email.has_attachments,
            "importance": email.importance,
            "category": email.category.value if hasattr(email.category, 'value') else str(email.category),
            "is_project_request": email.is_project_request,
            "preview": email.body_preview[:200] if email.body_preview else ""
        }

        if include_body:
            data["body"] = email.body

        if email.extracted_data:
            data["extracted_data"] = email.extracted_data

        return data


# =============================================================================
# FUNCAO HELPER
# =============================================================================

async def execute_email_read_skill(
    params: Dict[str, Any],
    email_client=None
) -> SkillResult:
    """
    Funcao helper para executar skill de leitura.

    Args:
        params: Parametros da acao
        email_client: Cliente de email (opcional)

    Returns:
        SkillResult
    """
    if email_client is None:
        from ..graph_mail_client import get_graph_client
        email_client = get_graph_client()

    skill = EmailReadSkill(email_client)
    return await skill.execute(params)

# -*- coding: utf-8 -*-
"""
Email Send Skill
================
Skill para envio de emails, usada por agentes Claude.

Esta skill permite que agentes enviem emails automaticamente
durante o processamento de tarefas.

Uso:
    skill = EmailSendSkill(graph_client)

    # Enviar email simples
    result = await skill.execute({
        "action": "send",
        "to": ["usuario@empresa.com"],
        "subject": "Teste",
        "body": "Conteudo do email"
    })

    # Enviar notificacao de projeto
    result = await skill.execute({
        "action": "notify_project_completed",
        "project_id": "PROJ-001",
        "project_name": "Meu Projeto"
    })
"""

import logging
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union
from enum import Enum

logger = logging.getLogger(__name__)


class EmailAction(str, Enum):
    """Acoes disponiveis"""
    SEND = "send"
    SEND_WITH_TEMPLATE = "send_with_template"
    NOTIFY_PROJECT_CREATED = "notify_project_created"
    NOTIFY_PROJECT_COMPLETED = "notify_project_completed"
    NOTIFY_STORY_UPDATED = "notify_story_updated"
    NOTIFY_ERROR = "notify_error"
    SCHEDULE_MEETING = "schedule_meeting"
    SEND_DAILY_REPORT = "send_daily_report"


@dataclass
class SkillResult:
    """Resultado da execucao da skill"""
    success: bool
    action: str
    message: str
    data: Optional[Dict[str, Any]] = None


class EmailSendSkill:
    """
    Skill de envio de emails para agentes.

    Fornece uma interface simplificada para agentes enviarem
    emails e notificacoes durante a execucao de tarefas.

    Exemplo para agente:
        # No contexto do agente Claude
        tools = [email_send_skill.get_tool_definition()]

        # Agente pode chamar:
        # send_email(action="send", to=["x@y.com"], subject="Teste", body="Ola")
    """

    SKILL_NAME = "email_send"
    SKILL_DESCRIPTION = """
    Skill para envio de emails e notificacoes.

    Acoes disponiveis:
    - send: Enviar email simples
    - send_with_template: Enviar email com template HTML
    - notify_project_created: Notificar criacao de projeto
    - notify_project_completed: Notificar conclusao de projeto
    - notify_story_updated: Notificar atualizacao de story
    - notify_error: Enviar alerta de erro
    - schedule_meeting: Agendar reuniao
    - send_daily_report: Enviar relatorio diario
    """

    def __init__(self, email_client):
        """
        Inicializa a skill.

        Args:
            email_client: Cliente de email (Graph ou SMTP)
        """
        self.client = email_client
        self._notification_sender = None
        self._report_sender = None

    def _get_notification_sender(self):
        """Obtem sender de notificacoes (lazy loading)"""
        if self._notification_sender is None:
            from ..senders.notification_sender import NotificationSender
            self._notification_sender = NotificationSender(self.client)
        return self._notification_sender

    def _get_report_sender(self):
        """Obtem sender de relatorios (lazy loading)"""
        if self._report_sender is None:
            from ..senders.report_sender import ReportSender
            self._report_sender = ReportSender(self.client)
        return self._report_sender

    def get_tool_definition(self) -> Dict[str, Any]:
        """
        Retorna definicao da ferramenta para uso com Claude.

        Returns:
            Dict com schema da ferramenta
        """
        return {
            "name": "send_email",
            "description": self.SKILL_DESCRIPTION,
            "input_schema": {
                "type": "object",
                "properties": {
                    "action": {
                        "type": "string",
                        "enum": [e.value for e in EmailAction],
                        "description": "Acao a executar"
                    },
                    "to": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Lista de destinatarios"
                    },
                    "subject": {
                        "type": "string",
                        "description": "Assunto do email"
                    },
                    "body": {
                        "type": "string",
                        "description": "Corpo do email (HTML ou texto)"
                    },
                    "template_name": {
                        "type": "string",
                        "description": "Nome do template (para send_with_template)"
                    },
                    "template_vars": {
                        "type": "object",
                        "description": "Variaveis do template"
                    },
                    "project_id": {
                        "type": "string",
                        "description": "ID do projeto"
                    },
                    "project_name": {
                        "type": "string",
                        "description": "Nome do projeto"
                    },
                    "story_id": {
                        "type": "string",
                        "description": "ID da story"
                    },
                    "story_data": {
                        "type": "object",
                        "description": "Dados da story"
                    },
                    "error_type": {
                        "type": "string",
                        "description": "Tipo do erro"
                    },
                    "error_message": {
                        "type": "string",
                        "description": "Mensagem de erro"
                    },
                    "meeting_subject": {
                        "type": "string",
                        "description": "Assunto da reuniao"
                    },
                    "meeting_datetime": {
                        "type": "string",
                        "description": "Data/hora da reuniao (ISO format)"
                    },
                    "meeting_duration": {
                        "type": "integer",
                        "description": "Duracao em minutos"
                    },
                    "attendees": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "Lista de participantes"
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
            action_enum = EmailAction(action)
        except ValueError:
            return SkillResult(
                success=False,
                action=action,
                message=f"Acao desconhecida: {action}"
            )

        try:
            # Roteia para o metodo correto
            if action_enum == EmailAction.SEND:
                return await self._action_send(params)

            elif action_enum == EmailAction.SEND_WITH_TEMPLATE:
                return await self._action_send_with_template(params)

            elif action_enum == EmailAction.NOTIFY_PROJECT_CREATED:
                return await self._action_notify_project_created(params)

            elif action_enum == EmailAction.NOTIFY_PROJECT_COMPLETED:
                return await self._action_notify_project_completed(params)

            elif action_enum == EmailAction.NOTIFY_STORY_UPDATED:
                return await self._action_notify_story_updated(params)

            elif action_enum == EmailAction.NOTIFY_ERROR:
                return await self._action_notify_error(params)

            elif action_enum == EmailAction.SCHEDULE_MEETING:
                return await self._action_schedule_meeting(params)

            elif action_enum == EmailAction.SEND_DAILY_REPORT:
                return await self._action_send_daily_report(params)

            else:
                return SkillResult(
                    success=False,
                    action=action,
                    message=f"Acao nao implementada: {action}"
                )

        except Exception as e:
            logger.exception(f"Erro na skill de email: {e}")
            return SkillResult(
                success=False,
                action=action,
                message=f"Erro ao executar: {str(e)}"
            )

    async def _action_send(self, params: Dict[str, Any]) -> SkillResult:
        """Envia email simples"""
        to = params.get("to", [])
        subject = params.get("subject", "")
        body = params.get("body", "")

        if not to:
            return SkillResult(
                success=False,
                action="send",
                message="Destinatario(s) nao informado(s)"
            )

        if not subject or not body:
            return SkillResult(
                success=False,
                action="send",
                message="Assunto e corpo sao obrigatorios"
            )

        success = await self.client.send_email(
            to=to,
            subject=subject,
            body=body
        )

        return SkillResult(
            success=success,
            action="send",
            message="Email enviado com sucesso" if success else "Falha ao enviar email",
            data={"to": to, "subject": subject}
        )

    async def _action_send_with_template(self, params: Dict[str, Any]) -> SkillResult:
        """Envia email com template"""
        to = params.get("to", [])
        template_name = params.get("template_name", "")
        template_vars = params.get("template_vars", {})
        subject = params.get("subject")

        if not to or not template_name:
            return SkillResult(
                success=False,
                action="send_with_template",
                message="Destinatario(s) e template sao obrigatorios"
            )

        success = await self.client.send_email_with_template(
            to=to,
            template_name=template_name,
            template_vars=template_vars,
            subject=subject
        )

        return SkillResult(
            success=success,
            action="send_with_template",
            message="Email enviado com sucesso" if success else "Falha ao enviar email",
            data={"to": to, "template": template_name}
        )

    async def _action_notify_project_created(self, params: Dict[str, Any]) -> SkillResult:
        """Notifica criacao de projeto"""
        project_data = {
            "project_id": params.get("project_id", ""),
            "name": params.get("project_name", ""),
            "description": params.get("description", ""),
            "type": params.get("project_type", "web-app"),
            "created_by": params.get("created_by", "Sistema")
        }

        sender = self._get_notification_sender()
        success = await sender.notify_project_created(
            project_data,
            params.get("to")
        )

        return SkillResult(
            success=success,
            action="notify_project_created",
            message="Notificacao enviada" if success else "Falha ao notificar",
            data={"project_id": project_data["project_id"]}
        )

    async def _action_notify_project_completed(self, params: Dict[str, Any]) -> SkillResult:
        """Notifica conclusao de projeto"""
        project_data = {
            "project_id": params.get("project_id", ""),
            "name": params.get("project_name", "")
        }

        execution_data = params.get("execution_data", {})

        sender = self._get_notification_sender()
        success = await sender.notify_project_completed(
            project_data,
            execution_data,
            params.get("to")
        )

        return SkillResult(
            success=success,
            action="notify_project_completed",
            message="Notificacao enviada" if success else "Falha ao notificar",
            data={"project_id": project_data["project_id"]}
        )

    async def _action_notify_story_updated(self, params: Dict[str, Any]) -> SkillResult:
        """Notifica atualizacao de story"""
        story_data = params.get("story_data", {})
        if not story_data:
            story_data = {
                "story_id": params.get("story_id", ""),
                "title": params.get("story_title", ""),
                "project_name": params.get("project_name", ""),
                "status": params.get("new_status", "")
            }

        sender = self._get_notification_sender()
        success = await sender.notify_story_updated(
            story_data,
            params.get("old_status"),
            params.get("new_status"),
            params.get("to")
        )

        return SkillResult(
            success=success,
            action="notify_story_updated",
            message="Notificacao enviada" if success else "Falha ao notificar",
            data={"story_id": story_data.get("story_id")}
        )

    async def _action_notify_error(self, params: Dict[str, Any]) -> SkillResult:
        """Notifica erro"""
        error_type = params.get("error_type", "Erro")
        error_message = params.get("error_message", "")

        if not error_message:
            return SkillResult(
                success=False,
                action="notify_error",
                message="Mensagem de erro e obrigatoria"
            )

        sender = self._get_notification_sender()
        success = await sender.notify_error(
            error_type=error_type,
            error_message=error_message,
            severity=params.get("severity", "high"),
            context=params.get("context"),
            stack_trace=params.get("stack_trace"),
            recipients=params.get("to")
        )

        return SkillResult(
            success=success,
            action="notify_error",
            message="Alerta enviado" if success else "Falha ao enviar alerta"
        )

    async def _action_schedule_meeting(self, params: Dict[str, Any]) -> SkillResult:
        """Agenda reuniao"""
        subject = params.get("meeting_subject", "Reuniao")
        datetime_str = params.get("meeting_datetime", "")
        duration = params.get("meeting_duration", 30)
        attendees = params.get("attendees", [])

        if not attendees:
            return SkillResult(
                success=False,
                action="schedule_meeting",
                message="Participantes sao obrigatorios"
            )

        # Parse datetime
        if datetime_str:
            try:
                start = datetime.fromisoformat(datetime_str)
            except ValueError:
                return SkillResult(
                    success=False,
                    action="schedule_meeting",
                    message="Formato de data invalido. Use ISO format."
                )
        else:
            # Default: amanha as 10h
            start = datetime.now().replace(hour=10, minute=0, second=0, microsecond=0)
            start += timedelta(days=1)

        event = await self.client.schedule_meeting(
            subject=subject,
            start=start,
            duration_minutes=duration,
            attendees=attendees,
            body=params.get("body", ""),
            reminder_minutes=params.get("reminder", 15)
        )

        if event:
            return SkillResult(
                success=True,
                action="schedule_meeting",
                message="Reuniao agendada com sucesso",
                data={
                    "event_id": event.get("id"),
                    "web_link": event.get("webLink"),
                    "online_meeting_url": event.get("onlineMeeting", {}).get("joinUrl")
                }
            )
        else:
            return SkillResult(
                success=False,
                action="schedule_meeting",
                message="Falha ao agendar reuniao"
            )

    async def _action_send_daily_report(self, params: Dict[str, Any]) -> SkillResult:
        """Envia relatorio diario"""
        sender = self._get_report_sender()
        success = await sender.send_daily_report(
            data=params.get("report_data"),
            recipients=params.get("to")
        )

        return SkillResult(
            success=success,
            action="send_daily_report",
            message="Relatorio enviado" if success else "Falha ao enviar relatorio"
        )


# =============================================================================
# FUNCAO HELPER
# =============================================================================

async def execute_email_skill(
    params: Dict[str, Any],
    email_client=None
) -> SkillResult:
    """
    Funcao helper para executar skill de email.

    Args:
        params: Parametros da acao
        email_client: Cliente de email (opcional)

    Returns:
        SkillResult
    """
    if email_client is None:
        from ..graph_mail_client import get_graph_client
        email_client = get_graph_client()

    skill = EmailSendSkill(email_client)
    return await skill.execute(params)

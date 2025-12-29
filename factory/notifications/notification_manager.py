# -*- coding: utf-8 -*-
"""
Gerenciador Central de Notificacoes
Fabrica de Agentes v6.0

Coordena o envio de notificacoes atraves de multiplos canais:
- Avalia regras para determinar destinatarios e canais
- Renderiza templates
- Gerencia filas e retries
- Registra logs

Uso:
    from factory.notifications import notification_manager

    await notification_manager.notify(
        event_type="project_completed",
        data={"project_name": "Sistema X", "duration": "2h"},
        recipients=["user@email.com"]
    )
"""

import asyncio
import logging
import uuid
from datetime import datetime
from typing import Dict, List, Any, Optional, Type
from dataclasses import dataclass

from .channels.base_channel import BaseChannel, NotificationMessage, ChannelResponse
from .channels.email_channel import EmailChannel
from .channels.slack_channel import SlackChannel
from .channels.teams_channel import TeamsChannel
from .rules.rule_engine import RuleEngine
from .rules.default_rules import DEFAULT_RULES, DEFAULT_QUIET_HOURS

logger = logging.getLogger(__name__)


@dataclass
class NotificationResult:
    """Resultado do envio de uma notificacao"""
    notification_id: str
    event_type: str
    success: bool
    channels_sent: List[str]
    channels_failed: List[str]
    recipients_count: int
    responses: List[ChannelResponse]
    timestamp: datetime


class NotificationManager:
    """
    Gerenciador Central de Notificacoes.

    Responsabilidades:
    - Registrar e gerenciar canais
    - Avaliar regras de notificacao
    - Renderizar templates
    - Enviar para multiplos canais
    - Registrar logs e metricas
    """

    # Mapa de tipos de canal para classes
    CHANNEL_CLASSES: Dict[str, Type[BaseChannel]] = {
        "email": EmailChannel,
        "slack": SlackChannel,
        "teams": TeamsChannel,
    }

    def __init__(self):
        """Inicializa o gerenciador"""
        self.channels: Dict[str, BaseChannel] = {}
        self.channel_configs: Dict[str, Dict[str, Any]] = {}
        self.rule_engine = RuleEngine(DEFAULT_RULES)
        self.templates: Dict[str, Dict[str, str]] = {}
        self.enabled = True

        # Configurar quiet hours padrao
        if DEFAULT_QUIET_HOURS.get("enabled"):
            self.rule_engine.set_quiet_hours(
                start=DEFAULT_QUIET_HOURS["start"],
                end=DEFAULT_QUIET_HOURS["end"],
                timezone=DEFAULT_QUIET_HOURS["timezone"],
                exceptions=DEFAULT_QUIET_HOURS.get("exceptions", [])
            )

        # Callbacks para eventos
        self._on_send_callbacks: List[callable] = []
        self._on_error_callbacks: List[callable] = []

        logger.info("NotificationManager inicializado")

    # =========================================================================
    # CONFIGURACAO DE CANAIS
    # =========================================================================

    def register_channel(
        self,
        channel_type: str,
        config: Dict[str, Any],
        channel_id: Optional[str] = None
    ) -> bool:
        """
        Registra um canal de notificacao.

        Args:
            channel_type: Tipo do canal (email, slack, teams)
            config: Configuracao do canal
            channel_id: ID unico (opcional, gera automaticamente)

        Returns:
            True se registrado com sucesso
        """
        if channel_type not in self.CHANNEL_CLASSES:
            logger.error(f"Tipo de canal desconhecido: {channel_type}")
            return False

        channel_id = channel_id or f"{channel_type}-{uuid.uuid4().hex[:8]}"

        try:
            channel_class = self.CHANNEL_CLASSES[channel_type]
            channel = channel_class(config)

            self.channels[channel_id] = channel
            self.channel_configs[channel_id] = {
                "type": channel_type,
                "config": config,
                "enabled": True,
                "registered_at": datetime.utcnow().isoformat()
            }

            logger.info(f"Canal registrado: {channel_id} ({channel_type})")
            return True

        except ValueError as e:
            logger.error(f"Erro ao registrar canal {channel_type}: {e}")
            return False

    def unregister_channel(self, channel_id: str) -> bool:
        """Remove um canal registrado"""
        if channel_id in self.channels:
            del self.channels[channel_id]
            del self.channel_configs[channel_id]
            logger.info(f"Canal removido: {channel_id}")
            return True
        return False

    def get_channel(self, channel_id: str) -> Optional[BaseChannel]:
        """Retorna um canal por ID"""
        return self.channels.get(channel_id)

    def get_channel_by_type(self, channel_type: str) -> Optional[BaseChannel]:
        """Retorna primeiro canal de um tipo"""
        for channel_id, info in self.channel_configs.items():
            if info["type"] == channel_type and info.get("enabled", True):
                return self.channels.get(channel_id)
        return None

    def list_channels(self) -> List[Dict[str, Any]]:
        """Lista todos os canais registrados"""
        return [
            {
                "channel_id": channel_id,
                **{k: v for k, v in info.items() if k != "config"}
            }
            for channel_id, info in self.channel_configs.items()
        ]

    async def test_channel(self, channel_id: str) -> bool:
        """Testa conexao de um canal"""
        channel = self.channels.get(channel_id)
        if not channel:
            return False
        return await channel.test_connection()

    # =========================================================================
    # CONFIGURACAO DE REGRAS
    # =========================================================================

    def add_rule(self, rule: Dict[str, Any]) -> None:
        """Adiciona uma regra de notificacao"""
        self.rule_engine.add_rule(rule)

    def remove_rule(self, rule_id: str) -> bool:
        """Remove uma regra"""
        return self.rule_engine.remove_rule(rule_id)

    def set_rules(self, rules: List[Dict[str, Any]]) -> None:
        """Define todas as regras"""
        self.rule_engine.set_rules(rules)

    def set_quiet_hours(
        self,
        start: str,
        end: str,
        timezone: str = "America/Sao_Paulo",
        exceptions: List[str] = None
    ) -> None:
        """Configura horario de nao perturbe"""
        self.rule_engine.set_quiet_hours(start, end, timezone, exceptions)

    # =========================================================================
    # TEMPLATES
    # =========================================================================

    def register_template(
        self,
        template_id: str,
        event_type: str,
        channel_type: str,
        subject: str,
        body: str,
        body_html: Optional[str] = None
    ) -> None:
        """
        Registra um template de notificacao.

        Args:
            template_id: ID unico do template
            event_type: Tipo de evento
            channel_type: Tipo de canal
            subject: Template do assunto (suporta {variaveis})
            body: Template do corpo
            body_html: Template HTML (opcional)
        """
        key = f"{event_type}:{channel_type}"
        self.templates[key] = {
            "id": template_id,
            "subject": subject,
            "body": body,
            "body_html": body_html
        }

    def render_template(
        self,
        event_type: str,
        channel_type: str,
        data: Dict[str, Any]
    ) -> Dict[str, str]:
        """
        Renderiza um template com dados.

        Args:
            event_type: Tipo do evento
            channel_type: Tipo do canal
            data: Dados para substituicao

        Returns:
            {"subject": str, "body": str, "body_html": str}
        """
        key = f"{event_type}:{channel_type}"
        template = self.templates.get(key)

        if not template:
            # Template padrao
            return {
                "subject": f"[{event_type}] Notificacao da Fabrica de Agentes",
                "body": self._default_body(event_type, data),
                "body_html": None
            }

        try:
            rendered = {
                "subject": template["subject"].format(**data),
                "body": template["body"].format(**data),
                "body_html": None
            }

            if template.get("body_html"):
                rendered["body_html"] = template["body_html"].format(**data)

            return rendered

        except KeyError as e:
            logger.warning(f"Variavel ausente no template: {e}")
            return {
                "subject": template["subject"],
                "body": template["body"],
                "body_html": template.get("body_html")
            }

    def _default_body(self, event_type: str, data: Dict[str, Any]) -> str:
        """Gera corpo padrao para eventos sem template"""
        lines = [f"Evento: {event_type}", ""]

        for key, value in data.items():
            if not key.startswith("_"):
                label = key.replace("_", " ").title()
                lines.append(f"{label}: {value}")

        return "\n".join(lines)

    # =========================================================================
    # ENVIO DE NOTIFICACOES
    # =========================================================================

    async def notify(
        self,
        event_type: str,
        data: Dict[str, Any],
        recipients: Optional[List[str]] = None,
        channels: Optional[List[str]] = None,
        priority: str = "normal",
        context: Optional[Dict[str, Any]] = None
    ) -> NotificationResult:
        """
        Envia notificacao para os canais apropriados.

        Este e o metodo principal para enviar notificacoes.

        Args:
            event_type: Tipo do evento (project_completed, error, etc)
            data: Dados do evento para templates
            recipients: Lista de destinatarios (opcional, usa regras)
            channels: Lista de canais (opcional, usa regras)
            priority: Prioridade (low, normal, high, urgent)
            context: Contexto adicional (project_id, user_id, etc)

        Returns:
            NotificationResult com detalhes do envio
        """
        notification_id = f"NTF-{uuid.uuid4().hex[:12].upper()}"
        context = context or {}
        responses = []
        channels_sent = []
        channels_failed = []

        if not self.enabled:
            logger.warning("NotificationManager desabilitado, notificacao ignorada")
            return NotificationResult(
                notification_id=notification_id,
                event_type=event_type,
                success=False,
                channels_sent=[],
                channels_failed=[],
                recipients_count=0,
                responses=[],
                timestamp=datetime.utcnow()
            )

        try:
            # Avaliar regras
            rule_matches = self.rule_engine.evaluate(event_type, data, context)

            # Se nenhuma regra combinou e nao foram especificados canais/destinatarios
            if not rule_matches and not channels and not recipients:
                logger.debug(f"Nenhuma regra combinou para evento {event_type}")
                return NotificationResult(
                    notification_id=notification_id,
                    event_type=event_type,
                    success=True,
                    channels_sent=[],
                    channels_failed=[],
                    recipients_count=0,
                    responses=[],
                    timestamp=datetime.utcnow()
                )

            # Coletar canais e destinatarios das regras ou dos parametros
            if channels:
                target_channels = channels
            else:
                target_channels = []
                for match in rule_matches:
                    target_channels.extend(match.channels)
                target_channels = list(set(target_channels))

            if recipients:
                target_recipients = recipients
            else:
                target_recipients = []
                for match in rule_matches:
                    target_recipients.extend(match.recipients)
                target_recipients = list(set(target_recipients))

            # Resolver destinatarios especiais (owner, team, admin)
            resolved_recipients = self._resolve_recipients(target_recipients, context)

            if not resolved_recipients:
                logger.warning(f"Sem destinatarios para evento {event_type}")
                return NotificationResult(
                    notification_id=notification_id,
                    event_type=event_type,
                    success=True,
                    channels_sent=[],
                    channels_failed=[],
                    recipients_count=0,
                    responses=[],
                    timestamp=datetime.utcnow()
                )

            # Usar prioridade mais alta das regras
            if rule_matches:
                priorities = {"urgent": 4, "high": 3, "normal": 2, "low": 1}
                highest_priority = max(
                    rule_matches,
                    key=lambda m: priorities.get(m.priority, 2)
                ).priority
                priority = highest_priority if priorities.get(highest_priority, 0) > priorities.get(priority, 0) else priority

            # Enviar para cada canal
            for channel_type in target_channels:
                channel = self.get_channel_by_type(channel_type)

                if not channel:
                    logger.warning(f"Canal {channel_type} nao configurado")
                    channels_failed.append(channel_type)
                    continue

                # Renderizar template
                template_content = self.render_template(event_type, channel_type, data)

                # Criar mensagem
                message = NotificationMessage(
                    notification_id=notification_id,
                    event_type=event_type,
                    subject=template_content["subject"],
                    body=template_content["body"],
                    body_html=template_content.get("body_html"),
                    recipients=resolved_recipients,
                    priority=priority,
                    data=data,
                    timestamp=datetime.utcnow()
                )

                # Adicionar acoes do contexto
                if context.get("actions"):
                    message.actions = context["actions"]

                # Enviar
                try:
                    response = await channel.send(message)
                    responses.append(response)

                    if response.success:
                        channels_sent.append(channel_type)
                        logger.info(
                            f"Notificacao {notification_id} enviada via {channel_type}"
                        )
                    else:
                        channels_failed.append(channel_type)
                        logger.error(
                            f"Falha ao enviar {notification_id} via {channel_type}: "
                            f"{response.error_message}"
                        )

                except Exception as e:
                    logger.error(f"Erro ao enviar via {channel_type}: {e}")
                    channels_failed.append(channel_type)
                    responses.append(ChannelResponse(
                        success=False,
                        channel_type=channel_type,
                        notification_id=notification_id,
                        error_message=str(e),
                        error_code="EXCEPTION"
                    ))

            # Resultado final
            result = NotificationResult(
                notification_id=notification_id,
                event_type=event_type,
                success=len(channels_sent) > 0,
                channels_sent=channels_sent,
                channels_failed=channels_failed,
                recipients_count=len(resolved_recipients),
                responses=responses,
                timestamp=datetime.utcnow()
            )

            # Callbacks
            for callback in self._on_send_callbacks:
                try:
                    callback(result)
                except Exception as e:
                    logger.error(f"Erro em callback on_send: {e}")

            return result

        except Exception as e:
            logger.error(f"Erro ao processar notificacao: {e}")

            for callback in self._on_error_callbacks:
                try:
                    callback(notification_id, event_type, str(e))
                except:
                    pass

            return NotificationResult(
                notification_id=notification_id,
                event_type=event_type,
                success=False,
                channels_sent=[],
                channels_failed=channels or [],
                recipients_count=0,
                responses=[],
                timestamp=datetime.utcnow()
            )

    async def notify_sync(self, *args, **kwargs) -> NotificationResult:
        """Versao sincrona do notify (para uso em codigo sync)"""
        loop = asyncio.get_event_loop()
        return loop.run_until_complete(self.notify(*args, **kwargs))

    def _resolve_recipients(
        self,
        recipients: List[str],
        context: Dict[str, Any]
    ) -> List[str]:
        """
        Resolve destinatarios especiais.

        Valores especiais:
        - "owner": Dono do projeto/recurso
        - "team": Membros da equipe
        - "admin": Administradores
        - "assignee": Responsavel pela tarefa
        - "reporter": Quem reportou
        - "approvers": Aprovadores

        Args:
            recipients: Lista de destinatarios (pode ter valores especiais)
            context: Contexto com informacoes de usuario

        Returns:
            Lista de emails/IDs resolvidos
        """
        resolved = []

        for recipient in recipients:
            if recipient == "owner":
                owner = context.get("owner_email") or context.get("owner")
                if owner:
                    resolved.append(owner)

            elif recipient == "team":
                team = context.get("team_emails") or context.get("team", [])
                if isinstance(team, list):
                    resolved.extend(team)
                elif team:
                    resolved.append(team)

            elif recipient == "admin":
                admins = context.get("admin_emails") or context.get("admins", [])
                if isinstance(admins, list):
                    resolved.extend(admins)
                elif admins:
                    resolved.append(admins)

            elif recipient == "assignee":
                assignee = context.get("assignee_email") or context.get("assignee")
                if assignee:
                    resolved.append(assignee)

            elif recipient == "reporter":
                reporter = context.get("reporter_email") or context.get("reporter")
                if reporter:
                    resolved.append(reporter)

            elif recipient == "approvers":
                approvers = context.get("approver_emails") or context.get("approvers", [])
                if isinstance(approvers, list):
                    resolved.extend(approvers)
                elif approvers:
                    resolved.append(approvers)

            elif "@" in recipient or recipient.startswith("#"):
                # Email ou canal (Slack)
                resolved.append(recipient)

            else:
                # Assumir que e um identificador de usuario
                resolved.append(recipient)

        # Remover duplicatas mantendo ordem
        seen = set()
        unique = []
        for r in resolved:
            if r and r not in seen:
                seen.add(r)
                unique.append(r)

        return unique

    # =========================================================================
    # CALLBACKS E EVENTOS
    # =========================================================================

    def on_send(self, callback: callable) -> None:
        """Registra callback para quando notificacao for enviada"""
        self._on_send_callbacks.append(callback)

    def on_error(self, callback: callable) -> None:
        """Registra callback para erros"""
        self._on_error_callbacks.append(callback)

    # =========================================================================
    # UTILIDADES
    # =========================================================================

    def enable(self) -> None:
        """Habilita o gerenciador"""
        self.enabled = True
        logger.info("NotificationManager habilitado")

    def disable(self) -> None:
        """Desabilita o gerenciador (nao envia notificacoes)"""
        self.enabled = False
        logger.info("NotificationManager desabilitado")

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do gerenciador"""
        return {
            "enabled": self.enabled,
            "channels_count": len(self.channels),
            "channels": list(self.channel_configs.keys()),
            "rules_count": len(self.rule_engine.rules),
            "templates_count": len(self.templates)
        }


# =============================================================================
# INSTANCIA GLOBAL (Singleton)
# =============================================================================

_instance: Optional[NotificationManager] = None


def get_notification_manager() -> NotificationManager:
    """
    Retorna instancia global do NotificationManager.

    Returns:
        NotificationManager singleton
    """
    global _instance
    if _instance is None:
        _instance = NotificationManager()
    return _instance


# Alias para facilitar imports
notification_manager = get_notification_manager()

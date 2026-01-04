# -*- coding: utf-8 -*-
"""
Canal de Email (SMTP)
Plataforma E v6.0

Implementa envio de notificacoes por email usando SMTP.
Suporta:
- Conexao SMTP com TLS/SSL
- Templates HTML
- Anexos
- Multiplos destinatarios
"""

import smtplib
import ssl
import time
import logging
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.base import MIMEBase
from email import encoders
from typing import Dict, List, Any, Optional
from datetime import datetime

from .base_channel import BaseChannel, NotificationMessage, ChannelResponse, MessageFormat

logger = logging.getLogger(__name__)


class EmailChannel(BaseChannel):
    """
    Canal de notificacao via Email (SMTP).

    Configuracao necessaria:
    - smtp_host: Servidor SMTP (ex: smtp.gmail.com)
    - smtp_port: Porta SMTP (ex: 587 para TLS, 465 para SSL)
    - username: Usuario para autenticacao
    - password: Senha ou app password
    - from_email: Email de origem
    - from_name: Nome do remetente (opcional)
    - use_tls: Usar TLS (padrao: True)
    - use_ssl: Usar SSL direto (padrao: False)
    """

    def _get_channel_type(self) -> str:
        return "email"

    def _get_required_config_keys(self) -> List[str]:
        return ["smtp_host", "smtp_port", "username", "password", "from_email"]

    async def send(self, message: NotificationMessage) -> ChannelResponse:
        """
        Envia email para os destinatarios.

        Args:
            message: Mensagem de notificacao

        Returns:
            ChannelResponse com resultado do envio
        """
        start_time = time.time()

        try:
            # Construir email
            email_msg = self.format_message(message)

            # Conectar ao servidor SMTP
            server = self._create_smtp_connection()

            try:
                # Autenticar
                server.login(
                    self.config["username"],
                    self.config["password"]
                )

                # Enviar para cada destinatario
                failed_recipients = []
                for recipient in message.recipients:
                    try:
                        email_msg["To"] = recipient
                        server.sendmail(
                            self.config["from_email"],
                            recipient,
                            email_msg.as_string()
                        )
                        logger.info(f"Email enviado para {recipient}")
                    except Exception as e:
                        logger.error(f"Falha ao enviar para {recipient}: {e}")
                        failed_recipients.append(recipient)

                latency_ms = int((time.time() - start_time) * 1000)

                if failed_recipients:
                    if len(failed_recipients) == len(message.recipients):
                        return ChannelResponse(
                            success=False,
                            channel_type=self.channel_type,
                            notification_id=message.notification_id,
                            error_message=f"Falha ao enviar para todos os destinatarios: {failed_recipients}",
                            error_code="SEND_FAILED",
                            latency_ms=latency_ms
                        )
                    else:
                        return ChannelResponse(
                            success=True,
                            channel_type=self.channel_type,
                            notification_id=message.notification_id,
                            provider_id=f"partial-{message.notification_id}",
                            raw_response={"failed_recipients": failed_recipients},
                            latency_ms=latency_ms
                        )

                return ChannelResponse(
                    success=True,
                    channel_type=self.channel_type,
                    notification_id=message.notification_id,
                    provider_id=f"email-{message.notification_id}",
                    latency_ms=latency_ms
                )

            finally:
                server.quit()

        except smtplib.SMTPAuthenticationError as e:
            logger.error(f"Erro de autenticacao SMTP: {e}")
            return ChannelResponse(
                success=False,
                channel_type=self.channel_type,
                notification_id=message.notification_id,
                error_message=f"Erro de autenticacao SMTP: {str(e)}",
                error_code="AUTH_ERROR",
                latency_ms=int((time.time() - start_time) * 1000)
            )

        except smtplib.SMTPConnectError as e:
            logger.error(f"Erro de conexao SMTP: {e}")
            return ChannelResponse(
                success=False,
                channel_type=self.channel_type,
                notification_id=message.notification_id,
                error_message=f"Erro de conexao SMTP: {str(e)}",
                error_code="CONNECTION_ERROR",
                latency_ms=int((time.time() - start_time) * 1000)
            )

        except Exception as e:
            logger.error(f"Erro ao enviar email: {e}")
            return ChannelResponse(
                success=False,
                channel_type=self.channel_type,
                notification_id=message.notification_id,
                error_message=str(e),
                error_code="UNKNOWN_ERROR",
                latency_ms=int((time.time() - start_time) * 1000)
            )

    async def test_connection(self) -> bool:
        """Testa conexao com servidor SMTP"""
        try:
            server = self._create_smtp_connection()
            server.login(
                self.config["username"],
                self.config["password"]
            )
            server.quit()
            logger.info("Conexao SMTP testada com sucesso")
            return True
        except Exception as e:
            logger.error(f"Falha no teste de conexao SMTP: {e}")
            return False

    def format_message(self, message: NotificationMessage) -> MIMEMultipart:
        """
        Converte NotificationMessage para email MIME.

        Args:
            message: Mensagem padronizada

        Returns:
            MIMEMultipart pronto para envio
        """
        email_msg = MIMEMultipart("alternative")

        # Headers
        from_name = self.config.get("from_name", "Plataforma E")
        email_msg["From"] = f"{from_name} <{self.config['from_email']}>"
        email_msg["Subject"] = self._format_subject(message)
        email_msg["X-Priority"] = self._get_priority_header(message.priority)
        email_msg["X-Notification-ID"] = message.notification_id
        email_msg["X-Event-Type"] = message.event_type

        # Corpo texto
        text_body = self._format_text_body(message)
        email_msg.attach(MIMEText(text_body, "plain", "utf-8"))

        # Corpo HTML
        html_body = message.body_html or self._format_html_body(message)
        email_msg.attach(MIMEText(html_body, "html", "utf-8"))

        # Anexos
        for attachment in message.attachments:
            self._add_attachment(email_msg, attachment)

        return email_msg

    def _create_smtp_connection(self) -> smtplib.SMTP:
        """Cria conexao SMTP"""
        host = self.config["smtp_host"]
        port = int(self.config["smtp_port"])
        use_ssl = self.config.get("use_ssl", False)
        use_tls = self.config.get("use_tls", True)

        context = ssl.create_default_context()

        if use_ssl:
            server = smtplib.SMTP_SSL(host, port, context=context)
        else:
            server = smtplib.SMTP(host, port)
            if use_tls:
                server.starttls(context=context)

        return server

    def _format_subject(self, message: NotificationMessage) -> str:
        """Formata o assunto do email"""
        priority_prefix = ""
        if message.priority == "urgent":
            priority_prefix = "[URGENTE] "
        elif message.priority == "high":
            priority_prefix = "[IMPORTANTE] "

        emoji = self.get_event_emoji(message.event_type)
        return f"{priority_prefix}{emoji} {message.subject}"

    def _get_priority_header(self, priority: str) -> str:
        """Retorna header de prioridade para email"""
        priorities = {
            "urgent": "1",
            "high": "2",
            "normal": "3",
            "low": "5"
        }
        return priorities.get(priority, "3")

    def _format_text_body(self, message: NotificationMessage) -> str:
        """Formata corpo em texto simples"""
        lines = [
            message.body,
            "",
            "---",
            f"Notificacao enviada por Plataforma E",
            f"Tipo: {message.event_type}",
            f"Data: {message.timestamp.strftime('%d/%m/%Y %H:%M')}",
            "",
            "Este e um email automatico. Nao responda."
        ]

        # Adicionar acoes como links
        if message.actions:
            lines.insert(-2, "")
            lines.insert(-2, "Acoes disponiveis:")
            for action in message.actions:
                if "url" in action:
                    lines.insert(-2, f"- {action.get('text', 'Link')}: {action['url']}")

        return "\n".join(lines)

    def _format_html_body(self, message: NotificationMessage) -> str:
        """Formata corpo em HTML"""
        priority_color = self.get_priority_color(message.priority)
        emoji = self.get_event_emoji(message.event_type)

        # Botoes de acao
        actions_html = ""
        if message.actions:
            buttons = []
            for action in message.actions:
                if "url" in action:
                    style = action.get("style", "primary")
                    btn_color = "#FF6C00" if style == "primary" else "#003B4A"
                    buttons.append(
                        f'''<a href="{action['url']}"
                            style="display: inline-block; padding: 10px 20px;
                            background-color: {btn_color}; color: white;
                            text-decoration: none; border-radius: 4px;
                            margin-right: 10px; margin-top: 10px;">
                            {action.get('text', 'Clique aqui')}
                        </a>'''
                    )
            if buttons:
                actions_html = f'''
                <div style="margin-top: 20px;">
                    {"".join(buttons)}
                </div>
                '''

        # Dados adicionais
        data_html = ""
        if message.data:
            data_items = []
            for key, value in message.data.items():
                if key not in ["_internal", "raw"]:
                    label = key.replace("_", " ").title()
                    data_items.append(
                        f'<tr><td style="padding: 8px; color: #6B7280;">{label}:</td>'
                        f'<td style="padding: 8px; font-weight: 500;">{value}</td></tr>'
                    )
            if data_items:
                data_html = f'''
                <table style="margin-top: 20px; border-collapse: collapse; width: 100%;">
                    {"".join(data_items)}
                </table>
                '''

        html = f'''
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
             background-color: #F3F4F6; margin: 0; padding: 20px;">
    <div style="max-width: 600px; margin: 0 auto; background-color: white;
                border-radius: 8px; overflow: hidden; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">

        <!-- Header -->
        <div style="background-color: #003B4A; padding: 20px; text-align: center;">
            <h1 style="color: white; margin: 0; font-size: 20px;">
                {emoji} Plataforma E
            </h1>
        </div>

        <!-- Barra de prioridade -->
        <div style="height: 4px; background-color: {priority_color};"></div>

        <!-- Conteudo -->
        <div style="padding: 30px;">
            <h2 style="color: #1F2937; margin-top: 0; margin-bottom: 20px;">
                {message.subject}
            </h2>

            <div style="color: #4B5563; line-height: 1.6;">
                {message.body.replace(chr(10), '<br>')}
            </div>

            {data_html}

            {actions_html}
        </div>

        <!-- Footer -->
        <div style="background-color: #F9FAFB; padding: 20px; border-top: 1px solid #E5E7EB;">
            <p style="margin: 0; color: #9CA3AF; font-size: 12px; text-align: center;">
                Notificacao automatica - {message.event_type}<br>
                {message.timestamp.strftime('%d/%m/%Y as %H:%M')}
            </p>
            <p style="margin: 10px 0 0 0; color: #9CA3AF; font-size: 12px; text-align: center;">
                <a href="#" style="color: #9CA3AF;">Gerenciar preferencias de notificacao</a>
            </p>
        </div>
    </div>
</body>
</html>
        '''

        return html

    def _add_attachment(self, email_msg: MIMEMultipart, attachment: Dict[str, Any]) -> None:
        """Adiciona anexo ao email"""
        try:
            filename = attachment.get("filename", "attachment")
            content = attachment.get("content")
            content_type = attachment.get("content_type", "application/octet-stream")

            if isinstance(content, str):
                content = content.encode("utf-8")

            part = MIMEBase(*content_type.split("/"))
            part.set_payload(content)
            encoders.encode_base64(part)

            part.add_header(
                "Content-Disposition",
                f"attachment; filename={filename}"
            )

            email_msg.attach(part)

        except Exception as e:
            logger.error(f"Erro ao adicionar anexo {attachment.get('filename')}: {e}")

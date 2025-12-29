# -*- coding: utf-8 -*-
"""
SMTP Email Client (Fallback)
============================
Cliente SMTP como alternativa ao Microsoft Graph para envio de emails.

Use este cliente quando:
- Microsoft Graph nao estiver disponivel
- Precisar de compatibilidade com servidores SMTP genericos
- Office 365 via SMTP direto

Configuracao via variaveis de ambiente:
- SMTP_SERVER: Servidor SMTP (smtp.office365.com)
- SMTP_PORT: Porta (587 para TLS, 465 para SSL)
- SMTP_USERNAME: Usuario (geralmente email)
- SMTP_PASSWORD: Senha ou app password
- SMTP_USE_TLS: true/false
- SMTP_USE_SSL: true/false
- SMTP_SENDER_EMAIL: Email remetente
- SMTP_SENDER_NAME: Nome do remetente
"""

import os
import ssl
import logging
import asyncio
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.base import MIMEBase
from email.utils import formataddr, formatdate
from email import encoders
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Union
from pathlib import Path

from ..base import IntegrationStatus

logger = logging.getLogger(__name__)


@dataclass
class SmtpConfig:
    """Configuracao para servidor SMTP"""
    server: str = "smtp.office365.com"
    port: int = 587
    username: str = ""
    password: str = ""
    use_tls: bool = True
    use_ssl: bool = False
    sender_email: str = ""
    sender_name: str = "Fabrica de Agentes"
    timeout: int = 30
    enabled: bool = False

    @classmethod
    def from_env(cls) -> "SmtpConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            server=os.getenv("SMTP_SERVER", "smtp.office365.com"),
            port=int(os.getenv("SMTP_PORT", "587")),
            username=os.getenv("SMTP_USERNAME", ""),
            password=os.getenv("SMTP_PASSWORD", ""),
            use_tls=os.getenv("SMTP_USE_TLS", "true").lower() == "true",
            use_ssl=os.getenv("SMTP_USE_SSL", "false").lower() == "true",
            sender_email=os.getenv("SMTP_SENDER_EMAIL", ""),
            sender_name=os.getenv("SMTP_SENDER_NAME", "Fabrica de Agentes"),
            timeout=int(os.getenv("SMTP_TIMEOUT", "30")),
            enabled=os.getenv("SMTP_ENABLED", "false").lower() == "true"
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(
            self.server and
            self.port and
            self.username and
            self.password and
            self.sender_email
        )


@dataclass
class SmtpAttachment:
    """Representa um anexo para email SMTP"""
    filename: str
    content: bytes
    content_type: str = "application/octet-stream"

    @classmethod
    def from_file(cls, file_path: Union[str, Path]) -> "SmtpAttachment":
        """Cria anexo a partir de arquivo"""
        import mimetypes

        path = Path(file_path)
        if not path.exists():
            raise FileNotFoundError(f"Arquivo nao encontrado: {file_path}")

        content_type = mimetypes.guess_type(path.name)[0] or "application/octet-stream"

        with open(path, "rb") as f:
            content = f.read()

        return cls(
            filename=path.name,
            content=content,
            content_type=content_type
        )


class SmtpClient:
    """
    Cliente SMTP para envio de emails.

    Suporta:
    - TLS e SSL
    - Autenticacao SMTP
    - Anexos
    - HTML e texto plano

    Exemplo de uso:
        config = SmtpConfig.from_env()
        client = SmtpClient(config)

        if await client.connect():
            await client.send_email(
                to=["usuario@empresa.com"],
                subject="Teste",
                body="<h1>Ola!</h1>"
            )
    """

    def __init__(self, config: SmtpConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._connection: Optional[smtplib.SMTP] = None
        self._last_error: Optional[str] = None

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self.status == IntegrationStatus.CONNECTED

    @property
    def last_error(self) -> Optional[str]:
        """Retorna ultimo erro"""
        return self._last_error

    async def connect(self) -> bool:
        """
        Conecta ao servidor SMTP.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuracao SMTP invalida"
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao SMTP: {self.config.server}:{self.config.port}")

        try:
            # Executa conexao em thread separada para nao bloquear
            loop = asyncio.get_event_loop()
            result = await loop.run_in_executor(None, self._connect_sync)
            return result

        except Exception as e:
            self._last_error = f"Erro ao conectar: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.exception("Erro ao conectar ao SMTP")
            return False

    def _connect_sync(self) -> bool:
        """Conexao sincrona ao SMTP (executada em thread)"""
        try:
            if self.config.use_ssl:
                # Conexao SSL direta
                context = ssl.create_default_context()
                self._connection = smtplib.SMTP_SSL(
                    self.config.server,
                    self.config.port,
                    timeout=self.config.timeout,
                    context=context
                )
            else:
                # Conexao normal (com TLS opcional)
                self._connection = smtplib.SMTP(
                    self.config.server,
                    self.config.port,
                    timeout=self.config.timeout
                )

                if self.config.use_tls:
                    context = ssl.create_default_context()
                    self._connection.starttls(context=context)

            # Autentica
            self._connection.login(self.config.username, self.config.password)

            self.status = IntegrationStatus.CONNECTED
            logger.info("Conectado ao SMTP com sucesso")
            return True

        except smtplib.SMTPAuthenticationError as e:
            self._last_error = f"Falha na autenticacao: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

        except smtplib.SMTPException as e:
            self._last_error = f"Erro SMTP: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

        except Exception as e:
            self._last_error = f"Erro de conexao: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

    async def disconnect(self) -> bool:
        """
        Desconecta do servidor SMTP.

        Returns:
            bool: True se desconectado com sucesso
        """
        try:
            if self._connection:
                self._connection.quit()
        except Exception:
            pass

        self._connection = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do SMTP")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao SMTP.

        Returns:
            bool: True se a conexao esta funcionando
        """
        if not self._connection:
            return False

        try:
            loop = asyncio.get_event_loop()
            status = await loop.run_in_executor(None, self._connection.noop)
            return status[0] == 250
        except Exception:
            return False

    async def send_email(
        self,
        to: Union[str, List[str]],
        subject: str,
        body: str,
        body_type: str = "html",
        cc: Optional[List[str]] = None,
        bcc: Optional[List[str]] = None,
        attachments: Optional[List[SmtpAttachment]] = None,
        reply_to: Optional[str] = None
    ) -> bool:
        """
        Envia um email via SMTP.

        Args:
            to: Destinatario(s)
            subject: Assunto
            body: Corpo do email
            body_type: "html" ou "plain"
            cc: Copia
            bcc: Copia oculta
            attachments: Lista de anexos
            reply_to: Endereco para resposta

        Returns:
            bool: True se enviado com sucesso
        """
        # Reconecta se necessario
        if not await self.test_connection():
            if not await self.connect():
                return False

        # Normaliza destinatarios
        if isinstance(to, str):
            to = [to]

        try:
            # Cria mensagem
            if attachments:
                msg = MIMEMultipart()
                msg.attach(MIMEText(body, body_type, "utf-8"))
            else:
                msg = MIMEMultipart("alternative")
                msg.attach(MIMEText(body, body_type, "utf-8"))

            # Headers
            msg["Subject"] = subject
            msg["From"] = formataddr((self.config.sender_name, self.config.sender_email))
            msg["To"] = ", ".join(to)
            msg["Date"] = formatdate(localtime=True)

            if cc:
                msg["Cc"] = ", ".join(cc)

            if reply_to:
                msg["Reply-To"] = reply_to

            # Adiciona anexos
            if attachments:
                for attachment in attachments:
                    part = MIMEBase(*attachment.content_type.split("/", 1))
                    part.set_payload(attachment.content)
                    encoders.encode_base64(part)
                    part.add_header(
                        "Content-Disposition",
                        "attachment",
                        filename=attachment.filename
                    )
                    msg.attach(part)

            # Lista completa de destinatarios
            all_recipients = list(to)
            if cc:
                all_recipients.extend(cc)
            if bcc:
                all_recipients.extend(bcc)

            # Envia em thread separada
            loop = asyncio.get_event_loop()
            await loop.run_in_executor(
                None,
                lambda: self._connection.sendmail(
                    self.config.sender_email,
                    all_recipients,
                    msg.as_string()
                )
            )

            logger.info(f"Email enviado para {to}: {subject}")
            return True

        except smtplib.SMTPException as e:
            self._last_error = f"Erro ao enviar: {str(e)}"
            logger.error(self._last_error)
            return False

        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"
            logger.exception("Erro ao enviar email")
            return False

    async def send_email_with_template(
        self,
        to: Union[str, List[str]],
        template_name: str,
        template_vars: Dict[str, Any],
        subject: Optional[str] = None,
        **kwargs
    ) -> bool:
        """
        Envia email usando template HTML.

        Args:
            to: Destinatario(s)
            template_name: Nome do template (sem extensao)
            template_vars: Variaveis para substituir no template
            subject: Assunto (opcional)
            **kwargs: Argumentos adicionais para send_email

        Returns:
            bool: True se enviado com sucesso
        """
        try:
            from jinja2 import Environment, FileSystemLoader

            # Carrega template
            templates_dir = Path(__file__).parent / "templates"
            env = Environment(loader=FileSystemLoader(templates_dir))

            template = env.get_template(f"{template_name}.html")
            body = template.render(**template_vars)

            # Usa assunto do template_vars ou parametro
            email_subject = subject or template_vars.get("subject", "Fabrica de Agentes")

            return await self.send_email(
                to=to,
                subject=email_subject,
                body=body,
                body_type="html",
                **kwargs
            )

        except Exception as e:
            self._last_error = f"Erro ao renderizar template: {str(e)}"
            logger.exception("Erro ao enviar email com template")
            return False

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        return {
            "system": "smtp",
            "status": self.status.value,
            "connected": self.is_connected,
            "server": self.config.server,
            "port": self.config.port,
            "sender_email": self.config.sender_email if self.is_connected else None,
            "last_error": self._last_error
        }


# =============================================================================
# INSTANCIA GLOBAL (SINGLETON)
# =============================================================================

_smtp_instance: Optional[SmtpClient] = None


def get_smtp_client() -> SmtpClient:
    """Retorna instancia global do cliente SMTP"""
    global _smtp_instance
    if _smtp_instance is None:
        config = SmtpConfig.from_env()
        _smtp_instance = SmtpClient(config)
    return _smtp_instance


async def init_smtp_client() -> Optional[SmtpClient]:
    """Inicializa e conecta o cliente SMTP se configurado"""
    client = get_smtp_client()
    if client.config.is_valid() and client.config.enabled:
        if await client.connect():
            return client
    return None

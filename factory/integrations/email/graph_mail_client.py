# -*- coding: utf-8 -*-
"""
Microsoft Graph Mail Client
============================
Cliente para integracao com Microsoft Graph API para Email e Calendario.

Funcionalidades:
- Autenticacao OAuth 2.0 via MSAL
- Envio de emails com anexos
- Leitura de caixa de entrada
- Gerenciamento de calendario (reunioes)
- Templates HTML personalizados

Configuracao via variaveis de ambiente:
- AZURE_TENANT_ID: ID do tenant Azure AD
- AZURE_CLIENT_ID: ID da aplicacao registrada
- AZURE_CLIENT_SECRET: Secret da aplicacao
- GRAPH_SENDER_EMAIL: Email do remetente

Permissoes necessarias no Azure AD:
- Mail.Send
- Mail.Read
- Mail.ReadWrite
- Calendars.ReadWrite
- User.Read
"""

import os
import logging
import base64
import mimetypes
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union
from pathlib import Path
from enum import Enum

try:
    import msal
    MSAL_AVAILABLE = True
except ImportError:
    MSAL_AVAILABLE = False
    msal = None

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    aiohttp = None

from ..base import IntegrationStatus

logger = logging.getLogger(__name__)


class EmailPriority(str, Enum):
    """Prioridade do email"""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"


class EmailImportance(str, Enum):
    """Importancia do email para Microsoft Graph"""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"


@dataclass
class EmailAttachment:
    """Representa um anexo de email"""
    name: str
    content: bytes
    content_type: Optional[str] = None
    is_inline: bool = False
    content_id: Optional[str] = None

    def __post_init__(self):
        if not self.content_type:
            self.content_type = mimetypes.guess_type(self.name)[0] or "application/octet-stream"

    def to_graph_format(self) -> Dict[str, Any]:
        """Converte para formato Microsoft Graph API"""
        attachment_data = {
            "@odata.type": "#microsoft.graph.fileAttachment",
            "name": self.name,
            "contentType": self.content_type,
            "contentBytes": base64.b64encode(self.content).decode("utf-8")
        }

        if self.is_inline and self.content_id:
            attachment_data["isInline"] = True
            attachment_data["contentId"] = self.content_id

        return attachment_data

    @classmethod
    def from_file(cls, file_path: Union[str, Path]) -> "EmailAttachment":
        """Cria anexo a partir de um arquivo"""
        path = Path(file_path)
        if not path.exists():
            raise FileNotFoundError(f"Arquivo nao encontrado: {file_path}")

        with open(path, "rb") as f:
            content = f.read()

        return cls(
            name=path.name,
            content=content,
            content_type=mimetypes.guess_type(path.name)[0]
        )


@dataclass
class EmailMessage:
    """Representa uma mensagem de email"""
    to: List[str]
    subject: str
    body: str
    body_type: str = "HTML"  # HTML ou Text
    cc: List[str] = field(default_factory=list)
    bcc: List[str] = field(default_factory=list)
    attachments: List[EmailAttachment] = field(default_factory=list)
    importance: EmailImportance = EmailImportance.NORMAL
    reply_to: Optional[str] = None
    save_to_sent_items: bool = True

    def to_graph_format(self, sender_email: str) -> Dict[str, Any]:
        """Converte para formato Microsoft Graph API"""
        # Destinatarios
        to_recipients = [{"emailAddress": {"address": email}} for email in self.to]
        cc_recipients = [{"emailAddress": {"address": email}} for email in self.cc]
        bcc_recipients = [{"emailAddress": {"address": email}} for email in self.bcc]

        message = {
            "message": {
                "subject": self.subject,
                "body": {
                    "contentType": self.body_type,
                    "content": self.body
                },
                "toRecipients": to_recipients,
                "from": {
                    "emailAddress": {"address": sender_email}
                },
                "importance": self.importance.value
            },
            "saveToSentItems": self.save_to_sent_items
        }

        if cc_recipients:
            message["message"]["ccRecipients"] = cc_recipients

        if bcc_recipients:
            message["message"]["bccRecipients"] = bcc_recipients

        if self.reply_to:
            message["message"]["replyTo"] = [{"emailAddress": {"address": self.reply_to}}]

        if self.attachments:
            message["message"]["attachments"] = [
                att.to_graph_format() for att in self.attachments
            ]

        return message


@dataclass
class CalendarEvent:
    """Representa um evento de calendario"""
    subject: str
    start: datetime
    end: datetime
    attendees: List[str] = field(default_factory=list)
    body: str = ""
    location: str = ""
    is_online_meeting: bool = True
    reminder_minutes: int = 15
    is_all_day: bool = False
    recurrence: Optional[Dict] = None

    def to_graph_format(self) -> Dict[str, Any]:
        """Converte para formato Microsoft Graph API"""
        attendee_list = [
            {
                "emailAddress": {"address": email},
                "type": "required"
            }
            for email in self.attendees
        ]

        event = {
            "subject": self.subject,
            "body": {
                "contentType": "HTML",
                "content": self.body
            },
            "start": {
                "dateTime": self.start.isoformat(),
                "timeZone": "America/Sao_Paulo"
            },
            "end": {
                "dateTime": self.end.isoformat(),
                "timeZone": "America/Sao_Paulo"
            },
            "attendees": attendee_list,
            "isOnlineMeeting": self.is_online_meeting,
            "reminderMinutesBeforeStart": self.reminder_minutes,
            "isAllDay": self.is_all_day
        }

        if self.location:
            event["location"] = {"displayName": self.location}

        if self.is_online_meeting:
            event["onlineMeetingProvider"] = "teamsForBusiness"

        if self.recurrence:
            event["recurrence"] = self.recurrence

        return event


@dataclass
class EmailConfig:
    """Configuracao para Microsoft Graph Mail"""
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    sender_email: str = ""
    default_recipients: List[str] = field(default_factory=list)
    admin_recipients: List[str] = field(default_factory=list)
    monitored_folder: str = "Inbox"
    enabled: bool = False
    scopes: List[str] = field(default_factory=lambda: [
        "https://graph.microsoft.com/.default"
    ])

    @classmethod
    def from_env(cls) -> "EmailConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        default_recipients = os.getenv("GRAPH_DEFAULT_RECIPIENTS", "")
        admin_recipients = os.getenv("GRAPH_ADMIN_RECIPIENTS", "")

        return cls(
            tenant_id=os.getenv("AZURE_TENANT_ID", ""),
            client_id=os.getenv("AZURE_CLIENT_ID", ""),
            client_secret=os.getenv("AZURE_CLIENT_SECRET", ""),
            sender_email=os.getenv("GRAPH_SENDER_EMAIL", ""),
            default_recipients=[r.strip() for r in default_recipients.split(",") if r.strip()],
            admin_recipients=[r.strip() for r in admin_recipients.split(",") if r.strip()],
            monitored_folder=os.getenv("GRAPH_MONITORED_FOLDER", "Inbox"),
            enabled=os.getenv("GRAPH_ENABLED", "false").lower() == "true"
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(
            self.tenant_id and
            self.client_id and
            self.client_secret and
            self.sender_email
        )


class MicrosoftGraphClient:
    """
    Cliente para Microsoft Graph API.

    Suporta:
    - Autenticacao OAuth 2.0 via client credentials
    - Envio de emails
    - Leitura de caixa de entrada
    - Gerenciamento de calendario

    Exemplo de uso:
        config = EmailConfig.from_env()
        client = MicrosoftGraphClient(config)

        if await client.connect():
            await client.send_email(
                to=["usuario@empresa.com"],
                subject="Teste",
                body="<h1>Ola!</h1>"
            )
    """

    GRAPH_BASE_URL = "https://graph.microsoft.com/v1.0"
    AUTHORITY_BASE = "https://login.microsoftonline.com"

    def __init__(self, config: EmailConfig):
        self.config = config
        self.status = IntegrationStatus.DISCONNECTED
        self._access_token: Optional[str] = None
        self._token_expires: Optional[datetime] = None
        self._msal_app: Optional[Any] = None
        self._session: Optional[Any] = None
        self._last_error: Optional[str] = None

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado e com token valido"""
        if self.status != IntegrationStatus.CONNECTED:
            return False
        if not self._access_token:
            return False
        if self._token_expires and datetime.utcnow() >= self._token_expires:
            return False
        return True

    @property
    def last_error(self) -> Optional[str]:
        """Retorna ultimo erro"""
        return self._last_error

    def _check_dependencies(self) -> bool:
        """Verifica se dependencias estao instaladas"""
        if not MSAL_AVAILABLE:
            self._last_error = "Biblioteca 'msal' nao instalada. Execute: pip install msal"
            return False
        if not AIOHTTP_AVAILABLE:
            self._last_error = "Biblioteca 'aiohttp' nao instalada. Execute: pip install aiohttp"
            return False
        return True

    async def connect(self) -> bool:
        """
        Conecta ao Microsoft Graph via OAuth 2.0.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self._check_dependencies():
            self.status = IntegrationStatus.ERROR
            logger.error(self._last_error)
            return False

        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique tenant_id, client_id, client_secret e sender_email."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info("Conectando ao Microsoft Graph...")

        try:
            # Cria app MSAL para client credentials flow
            authority = f"{self.AUTHORITY_BASE}/{self.config.tenant_id}"

            self._msal_app = msal.ConfidentialClientApplication(
                client_id=self.config.client_id,
                client_credential=self.config.client_secret,
                authority=authority
            )

            # Adquire token
            result = self._msal_app.acquire_token_for_client(
                scopes=self.config.scopes
            )

            if "access_token" in result:
                self._access_token = result["access_token"]
                # Token expira em ~1 hora, renovamos um pouco antes
                expires_in = result.get("expires_in", 3600)
                self._token_expires = datetime.utcnow() + timedelta(seconds=expires_in - 300)

                self.status = IntegrationStatus.CONNECTED
                logger.info("Conectado ao Microsoft Graph com sucesso")
                return True
            else:
                self._last_error = result.get("error_description", "Erro desconhecido na autenticacao")
                self.status = IntegrationStatus.ERROR
                logger.error(f"Falha na autenticacao: {self._last_error}")
                return False

        except Exception as e:
            self._last_error = f"Erro ao conectar: {str(e)}"
            self.status = IntegrationStatus.ERROR
            logger.exception("Erro ao conectar ao Microsoft Graph")
            return False

    async def _ensure_token(self) -> bool:
        """Garante que o token esta valido, renovando se necessario"""
        if self._access_token and self._token_expires:
            if datetime.utcnow() < self._token_expires:
                return True

        # Token expirado ou inexistente, reconecta
        return await self.connect()

    async def _ensure_session(self):
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json"
        }

    async def disconnect(self) -> bool:
        """
        Desconecta do Microsoft Graph.

        Returns:
            bool: True se desconectado com sucesso
        """
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._access_token = None
        self._token_expires = None
        self._msal_app = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Microsoft Graph")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com Microsoft Graph.

        Returns:
            bool: True se a conexao esta funcionando
        """
        if not await self._ensure_token():
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/me"

            async with session.get(url, headers=self._get_headers()) as response:
                return response.status == 200

        except Exception as e:
            logger.error(f"Erro ao testar conexao: {e}")
            return False

    async def send_email(
        self,
        to: Union[str, List[str]],
        subject: str,
        body: str,
        body_type: str = "HTML",
        cc: Optional[List[str]] = None,
        bcc: Optional[List[str]] = None,
        attachments: Optional[List[EmailAttachment]] = None,
        importance: EmailImportance = EmailImportance.NORMAL,
        save_to_sent: bool = True
    ) -> bool:
        """
        Envia um email via Microsoft Graph.

        Args:
            to: Destinatario(s)
            subject: Assunto
            body: Corpo do email (HTML ou texto)
            body_type: "HTML" ou "Text"
            cc: Copia
            bcc: Copia oculta
            attachments: Lista de anexos
            importance: Importancia do email
            save_to_sent: Salvar em itens enviados

        Returns:
            bool: True se enviado com sucesso
        """
        if not await self._ensure_token():
            return False

        # Normaliza destinatarios
        if isinstance(to, str):
            to = [to]

        message = EmailMessage(
            to=to,
            subject=subject,
            body=body,
            body_type=body_type,
            cc=cc or [],
            bcc=bcc or [],
            attachments=attachments or [],
            importance=importance,
            save_to_sent_items=save_to_sent
        )

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/sendMail"

            payload = message.to_graph_format(self.config.sender_email)

            async with session.post(
                url,
                headers=self._get_headers(),
                json=payload
            ) as response:
                if response.status == 202:
                    logger.info(f"Email enviado para {to}: {subject}")
                    return True
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"
                    logger.error(f"Falha ao enviar email: {self._last_error}")
                    return False

        except Exception as e:
            self._last_error = f"Erro ao enviar email: {str(e)}"
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
            subject: Assunto (opcional, pode estar no template)
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
            email_subject = subject or template_vars.get("subject", "Plataforma E")

            return await self.send_email(
                to=to,
                subject=email_subject,
                body=body,
                **kwargs
            )

        except Exception as e:
            self._last_error = f"Erro ao renderizar template: {str(e)}"
            logger.exception("Erro ao enviar email com template")
            return False

    async def get_messages(
        self,
        folder: str = "Inbox",
        filter_query: Optional[str] = None,
        top: int = 10,
        skip: int = 0,
        order_by: str = "receivedDateTime desc",
        select: Optional[List[str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista mensagens de uma pasta.

        Args:
            folder: Pasta (Inbox, SentItems, Drafts, etc.)
            filter_query: Filtro OData (ex: "isRead eq false")
            top: Numero maximo de mensagens
            skip: Offset para paginacao
            order_by: Ordenacao
            select: Campos para retornar

        Returns:
            List[Dict]: Lista de mensagens
        """
        if not await self._ensure_token():
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/mailFolders/{folder}/messages"

            params = {
                "$top": top,
                "$skip": skip,
                "$orderby": order_by
            }

            if filter_query:
                params["$filter"] = filter_query

            if select:
                params["$select"] = ",".join(select)

            async with session.get(
                url,
                headers=self._get_headers(),
                params=params
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("value", [])
                else:
                    error_text = await response.text()
                    logger.error(f"Erro ao listar mensagens: {error_text}")
                    return []

        except Exception as e:
            logger.exception(f"Erro ao listar mensagens: {e}")
            return []

    async def get_message(self, message_id: str) -> Optional[Dict[str, Any]]:
        """
        Busca uma mensagem especifica.

        Args:
            message_id: ID da mensagem

        Returns:
            Dict ou None
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/messages/{message_id}"

            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    return await response.json()
                return None

        except Exception as e:
            logger.exception(f"Erro ao buscar mensagem: {e}")
            return None

    async def get_message_attachments(self, message_id: str) -> List[Dict[str, Any]]:
        """
        Lista anexos de uma mensagem.

        Args:
            message_id: ID da mensagem

        Returns:
            List[Dict]: Lista de anexos
        """
        if not await self._ensure_token():
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/messages/{message_id}/attachments"

            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("value", [])
                return []

        except Exception as e:
            logger.exception(f"Erro ao listar anexos: {e}")
            return []

    async def download_attachment(
        self,
        message_id: str,
        attachment_id: str
    ) -> Optional[bytes]:
        """
        Baixa conteudo de um anexo.

        Args:
            message_id: ID da mensagem
            attachment_id: ID do anexo

        Returns:
            bytes ou None
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/messages/{message_id}/attachments/{attachment_id}"

            async with session.get(url, headers=self._get_headers()) as response:
                if response.status == 200:
                    data = await response.json()
                    content_bytes = data.get("contentBytes")
                    if content_bytes:
                        return base64.b64decode(content_bytes)
                return None

        except Exception as e:
            logger.exception(f"Erro ao baixar anexo: {e}")
            return None

    async def mark_as_read(self, message_id: str, is_read: bool = True) -> bool:
        """
        Marca mensagem como lida/nao lida.

        Args:
            message_id: ID da mensagem
            is_read: True para marcar como lida

        Returns:
            bool: True se atualizado
        """
        if not await self._ensure_token():
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/messages/{message_id}"

            async with session.patch(
                url,
                headers=self._get_headers(),
                json={"isRead": is_read}
            ) as response:
                return response.status == 200

        except Exception as e:
            logger.exception(f"Erro ao marcar mensagem: {e}")
            return False

    async def reply_to_message(
        self,
        message_id: str,
        body: str,
        reply_all: bool = False
    ) -> bool:
        """
        Responde a uma mensagem.

        Args:
            message_id: ID da mensagem original
            body: Corpo da resposta (HTML)
            reply_all: Responder a todos

        Returns:
            bool: True se enviado
        """
        if not await self._ensure_token():
            return False

        try:
            session = await self._ensure_session()
            action = "replyAll" if reply_all else "reply"
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/messages/{message_id}/{action}"

            payload = {
                "message": {
                    "body": {
                        "contentType": "HTML",
                        "content": body
                    }
                }
            }

            async with session.post(
                url,
                headers=self._get_headers(),
                json=payload
            ) as response:
                return response.status == 202

        except Exception as e:
            logger.exception(f"Erro ao responder mensagem: {e}")
            return False

    # =========================================================================
    # CALENDARIO
    # =========================================================================

    async def create_calendar_event(self, event: CalendarEvent) -> Optional[Dict[str, Any]]:
        """
        Cria um evento no calendario.

        Args:
            event: Evento a criar

        Returns:
            Dict com evento criado ou None
        """
        if not await self._ensure_token():
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/calendar/events"

            async with session.post(
                url,
                headers=self._get_headers(),
                json=event.to_graph_format()
            ) as response:
                if response.status == 201:
                    event_data = await response.json()
                    logger.info(f"Evento criado: {event.subject}")
                    return event_data
                else:
                    error_text = await response.text()
                    logger.error(f"Erro ao criar evento: {error_text}")
                    return None

        except Exception as e:
            logger.exception(f"Erro ao criar evento: {e}")
            return None

    async def get_calendar_events(
        self,
        start_datetime: Optional[datetime] = None,
        end_datetime: Optional[datetime] = None,
        top: int = 10
    ) -> List[Dict[str, Any]]:
        """
        Lista eventos do calendario.

        Args:
            start_datetime: Data/hora inicial
            end_datetime: Data/hora final
            top: Numero maximo de eventos

        Returns:
            List[Dict]: Lista de eventos
        """
        if not await self._ensure_token():
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/calendar/events"

            params = {"$top": top, "$orderby": "start/dateTime"}

            if start_datetime and end_datetime:
                params["$filter"] = (
                    f"start/dateTime ge '{start_datetime.isoformat()}' and "
                    f"end/dateTime le '{end_datetime.isoformat()}'"
                )

            async with session.get(
                url,
                headers=self._get_headers(),
                params=params
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("value", [])
                return []

        except Exception as e:
            logger.exception(f"Erro ao listar eventos: {e}")
            return []

    async def delete_calendar_event(self, event_id: str) -> bool:
        """
        Deleta um evento do calendario.

        Args:
            event_id: ID do evento

        Returns:
            bool: True se deletado
        """
        if not await self._ensure_token():
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.GRAPH_BASE_URL}/users/{self.config.sender_email}/calendar/events/{event_id}"

            async with session.delete(url, headers=self._get_headers()) as response:
                return response.status == 204

        except Exception as e:
            logger.exception(f"Erro ao deletar evento: {e}")
            return False

    async def schedule_meeting(
        self,
        subject: str,
        start: datetime,
        duration_minutes: int,
        attendees: List[str],
        body: str = "",
        location: str = "",
        reminder_minutes: int = 15
    ) -> Optional[Dict[str, Any]]:
        """
        Agenda uma reuniao (metodo de conveniencia).

        Args:
            subject: Assunto da reuniao
            start: Data/hora de inicio
            duration_minutes: Duracao em minutos
            attendees: Lista de participantes
            body: Descricao
            location: Local
            reminder_minutes: Lembrete em minutos

        Returns:
            Dict com evento criado ou None
        """
        event = CalendarEvent(
            subject=subject,
            start=start,
            end=start + timedelta(minutes=duration_minutes),
            attendees=attendees,
            body=body,
            location=location,
            is_online_meeting=True,
            reminder_minutes=reminder_minutes
        )

        return await self.create_calendar_event(event)

    def get_status(self) -> Dict[str, Any]:
        """Retorna status da integracao"""
        return {
            "system": "microsoft_graph",
            "status": self.status.value,
            "connected": self.is_connected,
            "sender_email": self.config.sender_email if self.is_connected else None,
            "last_error": self._last_error,
            "token_expires": self._token_expires.isoformat() if self._token_expires else None
        }


# =============================================================================
# INSTANCIA GLOBAL (SINGLETON)
# =============================================================================

_graph_instance: Optional[MicrosoftGraphClient] = None


def get_graph_client() -> MicrosoftGraphClient:
    """Retorna instancia global do cliente Microsoft Graph"""
    global _graph_instance
    if _graph_instance is None:
        config = EmailConfig.from_env()
        _graph_instance = MicrosoftGraphClient(config)
    return _graph_instance


async def init_graph_client() -> Optional[MicrosoftGraphClient]:
    """Inicializa e conecta o cliente Microsoft Graph se configurado"""
    client = get_graph_client()
    if client.config.is_valid() and client.config.enabled:
        if await client.connect():
            return client
    return None

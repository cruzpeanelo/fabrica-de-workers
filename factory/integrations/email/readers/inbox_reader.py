# -*- coding: utf-8 -*-
"""
Inbox Reader
============
Leitor de caixa de entrada para monitoramento de emails.

Funcionalidades:
- Monitorar pasta especifica
- Filtrar emails por assunto, remetente, etc.
- Extrair requisitos de projetos
- Criar projetos automaticamente a partir de emails
- Responder automaticamente

Uso:
    from factory.integrations.email.readers import InboxReader

    reader = InboxReader(graph_client)
    emails = await reader.get_unread_emails()

    for email in emails:
        if email.is_project_request:
            project_data = await reader.extract_project_requirements(email)
            # Criar projeto...
"""

import re
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Pattern
from enum import Enum

logger = logging.getLogger(__name__)


class EmailCategory(str, Enum):
    """Categoria do email"""
    PROJECT_REQUEST = "project_request"
    REQUIREMENT = "requirement"
    FEEDBACK = "feedback"
    QUESTION = "question"
    NOTIFICATION = "notification"
    OTHER = "other"


@dataclass
class EmailFilter:
    """Filtros para busca de emails"""
    unread_only: bool = True
    from_addresses: List[str] = field(default_factory=list)
    subject_contains: Optional[str] = None
    subject_pattern: Optional[str] = None
    received_after: Optional[datetime] = None
    received_before: Optional[datetime] = None
    has_attachments: Optional[bool] = None
    folder: str = "Inbox"
    max_results: int = 20

    def to_odata_filter(self) -> str:
        """Converte filtros para OData filter query"""
        filters = []

        if self.unread_only:
            filters.append("isRead eq false")

        if self.from_addresses:
            from_filters = []
            for addr in self.from_addresses:
                from_filters.append(f"from/emailAddress/address eq '{addr}'")
            if from_filters:
                filters.append(f"({' or '.join(from_filters)})")

        if self.subject_contains:
            # Escapa aspas simples
            subject = self.subject_contains.replace("'", "''")
            filters.append(f"contains(subject, '{subject}')")

        if self.received_after:
            filters.append(f"receivedDateTime ge {self.received_after.isoformat()}Z")

        if self.received_before:
            filters.append(f"receivedDateTime le {self.received_before.isoformat()}Z")

        if self.has_attachments is not None:
            filters.append(f"hasAttachments eq {str(self.has_attachments).lower()}")

        return " and ".join(filters) if filters else ""


@dataclass
class ProcessedEmail:
    """Email processado com metadados extraidos"""
    id: str
    subject: str
    body: str
    body_preview: str
    from_address: str
    from_name: str
    to_addresses: List[str]
    cc_addresses: List[str]
    received_at: datetime
    has_attachments: bool
    attachment_ids: List[str] = field(default_factory=list)
    is_read: bool = False
    importance: str = "normal"
    category: EmailCategory = EmailCategory.OTHER
    extracted_data: Dict[str, Any] = field(default_factory=dict)
    raw_data: Dict[str, Any] = field(default_factory=dict)

    @property
    def is_project_request(self) -> bool:
        """Verifica se e uma solicitacao de projeto"""
        return self.category == EmailCategory.PROJECT_REQUEST

    @property
    def is_requirement(self) -> bool:
        """Verifica se contem requisitos"""
        return self.category == EmailCategory.REQUIREMENT

    @classmethod
    def from_graph_message(cls, message: Dict[str, Any]) -> "ProcessedEmail":
        """Cria a partir de mensagem do Microsoft Graph"""
        body = message.get("body", {})
        from_data = message.get("from", {}).get("emailAddress", {})

        to_addresses = [
            r.get("emailAddress", {}).get("address", "")
            for r in message.get("toRecipients", [])
        ]
        cc_addresses = [
            r.get("emailAddress", {}).get("address", "")
            for r in message.get("ccRecipients", [])
        ]

        # Parse data de recebimento
        received_str = message.get("receivedDateTime", "")
        try:
            received_at = datetime.fromisoformat(received_str.replace("Z", "+00:00"))
        except Exception:
            received_at = datetime.utcnow()

        return cls(
            id=message.get("id", ""),
            subject=message.get("subject", ""),
            body=body.get("content", ""),
            body_preview=message.get("bodyPreview", ""),
            from_address=from_data.get("address", ""),
            from_name=from_data.get("name", ""),
            to_addresses=to_addresses,
            cc_addresses=cc_addresses,
            received_at=received_at,
            has_attachments=message.get("hasAttachments", False),
            is_read=message.get("isRead", False),
            importance=message.get("importance", "normal"),
            raw_data=message
        )


class InboxReader:
    """
    Leitor de caixa de entrada via Microsoft Graph.

    Monitora emails e extrai informacoes para criacao automatica de projetos.

    Exemplo:
        client = MicrosoftGraphClient(config)
        reader = InboxReader(client)

        # Buscar emails nao lidos
        emails = await reader.get_unread_emails()

        # Buscar solicitacoes de projeto
        requests = await reader.get_project_requests()

        for email in requests:
            project_data = await reader.extract_project_requirements(email)
            print(f"Novo projeto: {project_data['name']}")
    """

    # Padroes para identificar tipo de email
    PROJECT_PATTERNS = [
        r"\[PROJETO\]",
        r"\[PROJECT\]",
        r"\[NOVO PROJETO\]",
        r"\[NEW PROJECT\]",
        r"solicita[cç][aã]o de projeto",
        r"novo projeto",
        r"criar projeto"
    ]

    REQUIREMENT_PATTERNS = [
        r"\[REQUISITO\]",
        r"\[REQUIREMENT\]",
        r"\[REQ\]",
        r"requisitos?",
        r"especifica[cç][aã]o"
    ]

    def __init__(self, graph_client):
        """
        Inicializa o leitor.

        Args:
            graph_client: Cliente Microsoft Graph conectado
        """
        self.client = graph_client
        self._project_regex: Optional[Pattern] = None
        self._requirement_regex: Optional[Pattern] = None
        self._compile_patterns()

    def _compile_patterns(self):
        """Compila padroes regex"""
        self._project_regex = re.compile(
            "|".join(self.PROJECT_PATTERNS),
            re.IGNORECASE
        )
        self._requirement_regex = re.compile(
            "|".join(self.REQUIREMENT_PATTERNS),
            re.IGNORECASE
        )

    def _categorize_email(self, email: ProcessedEmail) -> EmailCategory:
        """
        Categoriza um email baseado em seu conteudo.

        Args:
            email: Email a categorizar

        Returns:
            EmailCategory
        """
        text = f"{email.subject} {email.body_preview}"

        if self._project_regex.search(text):
            return EmailCategory.PROJECT_REQUEST

        if self._requirement_regex.search(text):
            return EmailCategory.REQUIREMENT

        # Outros padroes
        if re.search(r"feedback|opiniao|sugestao", text, re.IGNORECASE):
            return EmailCategory.FEEDBACK

        if re.search(r"\?|pergunta|duvida|question", text, re.IGNORECASE):
            return EmailCategory.QUESTION

        return EmailCategory.OTHER

    async def get_emails(
        self,
        filter_config: Optional[EmailFilter] = None
    ) -> List[ProcessedEmail]:
        """
        Busca emails com filtros opcionais.

        Args:
            filter_config: Configuracao de filtros

        Returns:
            List[ProcessedEmail]: Lista de emails processados
        """
        if filter_config is None:
            filter_config = EmailFilter()

        try:
            messages = await self.client.get_messages(
                folder=filter_config.folder,
                filter_query=filter_config.to_odata_filter() or None,
                top=filter_config.max_results,
                select=[
                    "id", "subject", "body", "bodyPreview",
                    "from", "toRecipients", "ccRecipients",
                    "receivedDateTime", "hasAttachments",
                    "isRead", "importance"
                ]
            )

            emails = []
            for msg in messages:
                email = ProcessedEmail.from_graph_message(msg)
                email.category = self._categorize_email(email)

                # Aplica filtro de pattern se especificado
                if filter_config.subject_pattern:
                    if not re.search(filter_config.subject_pattern, email.subject, re.IGNORECASE):
                        continue

                emails.append(email)

            return emails

        except Exception as e:
            logger.exception(f"Erro ao buscar emails: {e}")
            return []

    async def get_unread_emails(
        self,
        folder: str = "Inbox",
        max_results: int = 20
    ) -> List[ProcessedEmail]:
        """
        Busca emails nao lidos.

        Args:
            folder: Pasta a buscar
            max_results: Maximo de resultados

        Returns:
            List[ProcessedEmail]
        """
        filter_config = EmailFilter(
            unread_only=True,
            folder=folder,
            max_results=max_results
        )
        return await self.get_emails(filter_config)

    async def get_project_requests(
        self,
        days_back: int = 7,
        max_results: int = 10
    ) -> List[ProcessedEmail]:
        """
        Busca emails que sao solicitacoes de projeto.

        Args:
            days_back: Buscar emails dos ultimos N dias
            max_results: Maximo de resultados

        Returns:
            List[ProcessedEmail]: Emails de solicitacao de projeto
        """
        filter_config = EmailFilter(
            unread_only=True,
            received_after=datetime.utcnow() - timedelta(days=days_back),
            max_results=max_results
        )

        emails = await self.get_emails(filter_config)
        return [e for e in emails if e.is_project_request]

    async def get_requirement_emails(
        self,
        project_id: Optional[str] = None,
        days_back: int = 7
    ) -> List[ProcessedEmail]:
        """
        Busca emails com requisitos.

        Args:
            project_id: Filtrar por ID de projeto no assunto
            days_back: Buscar emails dos ultimos N dias

        Returns:
            List[ProcessedEmail]
        """
        filter_config = EmailFilter(
            unread_only=True,
            received_after=datetime.utcnow() - timedelta(days=days_back)
        )

        if project_id:
            filter_config.subject_contains = project_id

        emails = await self.get_emails(filter_config)
        return [e for e in emails if e.is_requirement]

    async def extract_project_requirements(
        self,
        email: ProcessedEmail
    ) -> Dict[str, Any]:
        """
        Extrai informacoes de projeto de um email.

        Args:
            email: Email a processar

        Returns:
            Dict com dados do projeto extraidos
        """
        # Remove tags HTML do corpo
        body_text = self._strip_html(email.body)

        # Extrai nome do projeto do assunto
        project_name = self._extract_project_name(email.subject)

        # Extrai requisitos do corpo
        requirements = self._extract_requirements(body_text)

        # Extrai tipo de projeto
        project_type = self._detect_project_type(body_text)

        # Extrai prioridade
        priority = self._extract_priority(email.subject, body_text, email.importance)

        project_data = {
            "name": project_name,
            "description": body_text[:500] if body_text else "",
            "type": project_type,
            "requirements": requirements,
            "priority": priority,
            "source_email_id": email.id,
            "requester_email": email.from_address,
            "requester_name": email.from_name,
            "requested_at": email.received_at.isoformat(),
            "has_attachments": email.has_attachments
        }

        email.extracted_data = project_data
        return project_data

    def _strip_html(self, html: str) -> str:
        """Remove tags HTML"""
        try:
            from bs4 import BeautifulSoup
            soup = BeautifulSoup(html, "html.parser")
            return soup.get_text(separator="\n", strip=True)
        except ImportError:
            # Fallback simples
            import re
            text = re.sub(r"<[^>]+>", "", html)
            return text.strip()

    def _extract_project_name(self, subject: str) -> str:
        """Extrai nome do projeto do assunto"""
        # Remove prefixos comuns
        name = re.sub(
            r"^\s*(\[PROJETO\]|\[PROJECT\]|\[NOVO PROJETO\]|RE:|FW:|ENC:)\s*",
            "",
            subject,
            flags=re.IGNORECASE
        )
        return name.strip()[:100]

    def _extract_requirements(self, text: str) -> List[str]:
        """Extrai lista de requisitos do texto"""
        requirements = []

        # Procura por listas numeradas ou com bullets
        lines = text.split("\n")

        in_requirements_section = False
        for line in lines:
            line = line.strip()

            # Detecta inicio de secao de requisitos
            if re.match(r"^(requisitos?|requirements?|funcionalidades?|features?):", line, re.IGNORECASE):
                in_requirements_section = True
                continue

            # Detecta fim de secao
            if in_requirements_section and re.match(r"^[A-Z].*:", line):
                in_requirements_section = False

            # Extrai items de lista
            if line:
                # Listas com numeros, bullets, tracos
                match = re.match(r"^[\d\.\-\*\+•]+\s*(.+)", line)
                if match:
                    req = match.group(1).strip()
                    if len(req) > 5:
                        requirements.append(req)
                elif in_requirements_section and len(line) > 10:
                    requirements.append(line)

        return requirements[:20]  # Limita a 20 requisitos

    def _detect_project_type(self, text: str) -> str:
        """Detecta tipo de projeto baseado no texto"""
        text_lower = text.lower()

        if any(kw in text_lower for kw in ["api", "rest", "endpoint", "backend"]):
            return "api-service"

        if any(kw in text_lower for kw in ["web", "frontend", "react", "angular", "vue", "dashboard"]):
            return "web-app"

        if any(kw in text_lower for kw in ["dados", "analise", "relatorio", "bi", "data"]):
            return "data-analysis"

        if any(kw in text_lower for kw in ["automacao", "script", "bot", "automation"]):
            return "automation"

        if any(kw in text_lower for kw in ["integracao", "integration", "conectar"]):
            return "integration"

        return "web-app"  # Default

    def _extract_priority(
        self,
        subject: str,
        body: str,
        email_importance: str
    ) -> str:
        """Extrai prioridade do email"""
        text = f"{subject} {body}".lower()

        if email_importance == "high":
            return "high"

        if any(kw in text for kw in ["urgente", "urgent", "asap", "critico", "critical"]):
            return "urgent"

        if any(kw in text for kw in ["alta prioridade", "high priority", "importante"]):
            return "high"

        if any(kw in text for kw in ["baixa prioridade", "low priority", "quando puder"]):
            return "low"

        return "medium"

    async def mark_as_processed(
        self,
        email: ProcessedEmail,
        mark_read: bool = True
    ) -> bool:
        """
        Marca email como processado.

        Args:
            email: Email a marcar
            mark_read: Marcar como lido

        Returns:
            bool: True se marcado com sucesso
        """
        if mark_read:
            return await self.client.mark_as_read(email.id, True)
        return True

    async def send_confirmation_reply(
        self,
        email: ProcessedEmail,
        project_id: str,
        project_url: Optional[str] = None
    ) -> bool:
        """
        Envia resposta de confirmacao ao remetente.

        Args:
            email: Email original
            project_id: ID do projeto criado
            project_url: URL para acessar o projeto

        Returns:
            bool: True se enviado
        """
        body = f"""
        <h2>Projeto Recebido!</h2>

        <p>Ola {email.from_name or 'Usuario'},</p>

        <p>Sua solicitacao de projeto foi recebida e registrada com sucesso.</p>

        <p><strong>ID do Projeto:</strong> {project_id}</p>

        <p>O desenvolvimento autonomo sera iniciado em breve. Voce recebera
        atualizacoes sobre o progresso.</p>

        {"<p><a href='" + project_url + "'>Clique aqui para acompanhar o projeto</a></p>" if project_url else ""}

        <p>Atenciosamente,<br>
        <strong>Plataforma E</strong><br>
        Sistema de Desenvolvimento Autonomo</p>
        """

        return await self.client.reply_to_message(email.id, body)

    async def watch_for_projects(
        self,
        callback,
        check_interval: int = 60,
        folder: str = "Inbox"
    ):
        """
        Monitora continuamente por novos emails de projeto.

        Args:
            callback: Funcao async a chamar para cada novo email
            check_interval: Intervalo entre verificacoes (segundos)
            folder: Pasta a monitorar
        """
        import asyncio

        logger.info(f"Iniciando monitoramento de emails em '{folder}'")

        processed_ids = set()

        while True:
            try:
                requests = await self.get_project_requests()

                for email in requests:
                    if email.id not in processed_ids:
                        logger.info(f"Novo email de projeto: {email.subject}")
                        try:
                            await callback(email)
                            processed_ids.add(email.id)
                        except Exception as e:
                            logger.error(f"Erro no callback: {e}")

                # Limpa IDs antigos para evitar crescimento infinito
                if len(processed_ids) > 1000:
                    processed_ids.clear()

            except Exception as e:
                logger.error(f"Erro no monitoramento: {e}")

            await asyncio.sleep(check_interval)

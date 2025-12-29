# -*- coding: utf-8 -*-
"""
Microsoft Graph API Client
==========================
Cliente para interacao com a API Microsoft Graph.
Permite enviar mensagens, gerenciar equipes e acessar recursos do Teams.

Documentacao: https://docs.microsoft.com/graph/api/overview
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

import aiohttp

logger = logging.getLogger(__name__)


class GraphScope(str, Enum):
    """Escopos de permissao do Microsoft Graph"""
    CHANNEL_MESSAGE_SEND = "ChannelMessage.Send"
    CHAT_MESSAGE_SEND = "Chat.Message"
    CHAT_READ_WRITE = "Chat.ReadWrite"
    TEAM_READ_ALL = "Team.ReadBasic.All"
    USER_READ = "User.Read"
    DEFAULT = "https://graph.microsoft.com/.default"


@dataclass
class GraphConfig:
    """Configuracao do cliente Microsoft Graph"""
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    scopes: List[str] = field(default_factory=lambda: [GraphScope.DEFAULT.value])
    base_url: str = "https://graph.microsoft.com/v1.0"
    token_url_template: str = "https://login.microsoftonline.com/{tenant_id}/oauth2/v2.0/token"
    timeout_seconds: int = 30
    max_retries: int = 3
    retry_delay_seconds: float = 1.0


@dataclass
class AccessToken:
    """Token de acesso OAuth2"""
    token: str
    expires_at: datetime
    token_type: str = "Bearer"

    @property
    def is_expired(self) -> bool:
        """Verifica se o token expirou"""
        # Considera expirado 5 minutos antes para margem de seguranca
        return datetime.utcnow() >= (self.expires_at - timedelta(minutes=5))


class GraphClient:
    """
    Cliente para Microsoft Graph API.

    Fornece metodos para:
    - Autenticacao OAuth2 com Azure AD
    - Envio de mensagens em canais do Teams
    - Envio de mensagens diretas
    - Gerenciamento de equipes e canais
    - Upload de arquivos

    Exemplo:
        config = GraphConfig(
            tenant_id="seu-tenant-id",
            client_id="seu-client-id",
            client_secret="seu-client-secret"
        )
        client = GraphClient(config)
        await client.authenticate()
        await client.send_channel_message(team_id, channel_id, "Mensagem")
    """

    def __init__(self, config: GraphConfig):
        """
        Inicializa o cliente Graph.

        Args:
            config: Configuracao do cliente
        """
        self.config = config
        self._token: Optional[AccessToken] = None
        self._session: Optional[aiohttp.ClientSession] = None

    async def _get_session(self) -> aiohttp.ClientSession:
        """Obtem ou cria sessao HTTP"""
        if self._session is None or self._session.closed:
            timeout = aiohttp.ClientTimeout(total=self.config.timeout_seconds)
            self._session = aiohttp.ClientSession(timeout=timeout)
        return self._session

    async def close(self) -> None:
        """Fecha a sessao HTTP"""
        if self._session and not self._session.closed:
            await self._session.close()

    async def authenticate(self) -> bool:
        """
        Autentica com Azure AD usando Client Credentials Flow.

        Returns:
            True se autenticacao bem sucedida, False caso contrario
        """
        try:
            token_url = self.config.token_url_template.format(
                tenant_id=self.config.tenant_id
            )

            data = {
                "client_id": self.config.client_id,
                "client_secret": self.config.client_secret,
                "scope": " ".join(self.config.scopes),
                "grant_type": "client_credentials"
            }

            session = await self._get_session()
            async with session.post(token_url, data=data) as response:
                if response.status == 200:
                    result = await response.json()
                    expires_in = result.get("expires_in", 3600)
                    self._token = AccessToken(
                        token=result["access_token"],
                        expires_at=datetime.utcnow() + timedelta(seconds=expires_in),
                        token_type=result.get("token_type", "Bearer")
                    )
                    logger.info("Autenticacao com Microsoft Graph bem sucedida")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro na autenticacao: {response.status} - {error}")
                    return False

        except Exception as e:
            logger.error(f"Erro ao autenticar com Microsoft Graph: {e}")
            return False

    async def _ensure_authenticated(self) -> bool:
        """Garante que existe um token valido"""
        if self._token is None or self._token.is_expired:
            return await self.authenticate()
        return True

    async def _get_headers(self) -> Dict[str, str]:
        """Retorna headers de autorizacao"""
        await self._ensure_authenticated()
        return {
            "Authorization": f"{self._token.token_type} {self._token.token}",
            "Content-Type": "application/json"
        }

    async def _request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Faz uma requisicao para a API Graph.

        Args:
            method: Metodo HTTP (GET, POST, etc)
            endpoint: Endpoint da API
            data: Dados do corpo da requisicao
            params: Parametros de query string

        Returns:
            Resposta JSON ou None em caso de erro
        """
        url = f"{self.config.base_url}{endpoint}"
        headers = await self._get_headers()
        session = await self._get_session()

        for attempt in range(self.config.max_retries):
            try:
                async with session.request(
                    method,
                    url,
                    json=data,
                    params=params,
                    headers=headers
                ) as response:
                    if response.status == 204:
                        return {"success": True}

                    if response.status in [200, 201]:
                        return await response.json()

                    if response.status == 429:
                        # Rate limiting - espera e tenta novamente
                        retry_after = int(response.headers.get("Retry-After", 5))
                        logger.warning(f"Rate limit atingido. Aguardando {retry_after}s...")
                        await asyncio.sleep(retry_after)
                        continue

                    error = await response.text()
                    logger.error(f"Erro na requisicao: {response.status} - {error}")
                    return None

            except asyncio.TimeoutError:
                logger.warning(f"Timeout na tentativa {attempt + 1}/{self.config.max_retries}")
                if attempt < self.config.max_retries - 1:
                    await asyncio.sleep(self.config.retry_delay_seconds)
                continue

            except Exception as e:
                logger.error(f"Erro na requisicao: {e}")
                return None

        return None

    # =========================================================================
    # Teams e Canais
    # =========================================================================

    async def list_teams(self) -> List[Dict[str, Any]]:
        """
        Lista todas as equipes acessiveis.

        Returns:
            Lista de equipes
        """
        result = await self._request("GET", "/teams")
        if result and "value" in result:
            return result["value"]
        return []

    async def get_team(self, team_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de uma equipe.

        Args:
            team_id: ID da equipe

        Returns:
            Dados da equipe ou None
        """
        return await self._request("GET", f"/teams/{team_id}")

    async def list_channels(self, team_id: str) -> List[Dict[str, Any]]:
        """
        Lista canais de uma equipe.

        Args:
            team_id: ID da equipe

        Returns:
            Lista de canais
        """
        result = await self._request("GET", f"/teams/{team_id}/channels")
        if result and "value" in result:
            return result["value"]
        return []

    async def get_channel(self, team_id: str, channel_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de um canal.

        Args:
            team_id: ID da equipe
            channel_id: ID do canal

        Returns:
            Dados do canal ou None
        """
        return await self._request("GET", f"/teams/{team_id}/channels/{channel_id}")

    async def send_channel_message(
        self,
        team_id: str,
        channel_id: str,
        content: str,
        content_type: str = "html"
    ) -> Optional[Dict[str, Any]]:
        """
        Envia mensagem em um canal.

        Args:
            team_id: ID da equipe
            channel_id: ID do canal
            content: Conteudo da mensagem
            content_type: Tipo do conteudo (text ou html)

        Returns:
            Dados da mensagem enviada ou None
        """
        data = {
            "body": {
                "contentType": content_type,
                "content": content
            }
        }
        return await self._request(
            "POST",
            f"/teams/{team_id}/channels/{channel_id}/messages",
            data=data
        )

    async def send_channel_card(
        self,
        team_id: str,
        channel_id: str,
        card: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Envia um Card Adaptativo em um canal.

        Args:
            team_id: ID da equipe
            channel_id: ID do canal
            card: Card Adaptativo em formato JSON

        Returns:
            Dados da mensagem enviada ou None
        """
        data = {
            "body": {
                "contentType": "html",
                "content": ""
            },
            "attachments": [
                {
                    "contentType": "application/vnd.microsoft.card.adaptive",
                    "content": card
                }
            ]
        }
        return await self._request(
            "POST",
            f"/teams/{team_id}/channels/{channel_id}/messages",
            data=data
        )

    # =========================================================================
    # Chat (Mensagens Diretas)
    # =========================================================================

    async def list_chats(self) -> List[Dict[str, Any]]:
        """
        Lista chats do usuario/app.

        Returns:
            Lista de chats
        """
        result = await self._request("GET", "/chats")
        if result and "value" in result:
            return result["value"]
        return []

    async def create_chat(self, user_ids: List[str]) -> Optional[Dict[str, Any]]:
        """
        Cria um novo chat com usuarios.

        Args:
            user_ids: Lista de IDs de usuarios

        Returns:
            Dados do chat criado ou None
        """
        members = []
        for user_id in user_ids:
            members.append({
                "@odata.type": "#microsoft.graph.aadUserConversationMember",
                "roles": ["owner"],
                "user@odata.bind": f"https://graph.microsoft.com/v1.0/users('{user_id}')"
            })

        data = {
            "chatType": "oneOnOne" if len(user_ids) <= 2 else "group",
            "members": members
        }
        return await self._request("POST", "/chats", data=data)

    async def send_chat_message(
        self,
        chat_id: str,
        content: str,
        content_type: str = "html"
    ) -> Optional[Dict[str, Any]]:
        """
        Envia mensagem em um chat.

        Args:
            chat_id: ID do chat
            content: Conteudo da mensagem
            content_type: Tipo do conteudo (text ou html)

        Returns:
            Dados da mensagem enviada ou None
        """
        data = {
            "body": {
                "contentType": content_type,
                "content": content
            }
        }
        return await self._request("POST", f"/chats/{chat_id}/messages", data=data)

    async def send_chat_card(
        self,
        chat_id: str,
        card: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Envia um Card Adaptativo em um chat.

        Args:
            chat_id: ID do chat
            card: Card Adaptativo em formato JSON

        Returns:
            Dados da mensagem enviada ou None
        """
        data = {
            "body": {
                "contentType": "html",
                "content": ""
            },
            "attachments": [
                {
                    "contentType": "application/vnd.microsoft.card.adaptive",
                    "content": card
                }
            ]
        }
        return await self._request("POST", f"/chats/{chat_id}/messages", data=data)

    # =========================================================================
    # Usuarios
    # =========================================================================

    async def get_user(self, user_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de um usuario.

        Args:
            user_id: ID ou email do usuario

        Returns:
            Dados do usuario ou None
        """
        return await self._request("GET", f"/users/{user_id}")

    async def search_users(self, query: str) -> List[Dict[str, Any]]:
        """
        Pesquisa usuarios.

        Args:
            query: Termo de busca

        Returns:
            Lista de usuarios encontrados
        """
        params = {
            "$filter": f"startswith(displayName, '{query}') or startswith(mail, '{query}')",
            "$top": "10"
        }
        result = await self._request("GET", "/users", params=params)
        if result and "value" in result:
            return result["value"]
        return []

    # =========================================================================
    # Presenca
    # =========================================================================

    async def get_presence(self, user_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem status de presenca de um usuario.

        Args:
            user_id: ID do usuario

        Returns:
            Dados de presenca ou None
        """
        return await self._request("GET", f"/users/{user_id}/presence")

    # =========================================================================
    # Arquivos (OneDrive/SharePoint)
    # =========================================================================

    async def upload_file_to_channel(
        self,
        team_id: str,
        channel_id: str,
        filename: str,
        content: bytes,
        folder: str = "General"
    ) -> Optional[Dict[str, Any]]:
        """
        Faz upload de arquivo para um canal.

        Args:
            team_id: ID da equipe
            channel_id: ID do canal
            filename: Nome do arquivo
            content: Conteudo do arquivo em bytes
            folder: Pasta de destino

        Returns:
            Dados do arquivo ou None
        """
        # Primeiro obtem o drive do canal
        channel = await self.get_channel(team_id, channel_id)
        if not channel:
            return None

        # Upload via Graph
        endpoint = f"/teams/{team_id}/channels/{channel_id}/filesFolder"
        folder_info = await self._request("GET", endpoint)

        if folder_info and "id" in folder_info:
            drive_id = folder_info.get("parentReference", {}).get("driveId")
            if drive_id:
                upload_endpoint = f"/drives/{drive_id}/items/root:/{folder}/{filename}:/content"

                session = await self._get_session()
                headers = await self._get_headers()
                headers["Content-Type"] = "application/octet-stream"

                async with session.put(
                    f"{self.config.base_url}{upload_endpoint}",
                    data=content,
                    headers=headers
                ) as response:
                    if response.status in [200, 201]:
                        return await response.json()
                    else:
                        error = await response.text()
                        logger.error(f"Erro no upload: {response.status} - {error}")

        return None

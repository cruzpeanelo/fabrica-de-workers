# -*- coding: utf-8 -*-
"""
SharePoint Graph Client
=======================
Cliente base para interacao com SharePoint via Microsoft Graph API.

Terminal 5 - Issue #298
"""

import asyncio
import logging
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

import aiohttp

from .config import SharePointConfig

logger = logging.getLogger(__name__)


@dataclass
class AccessToken:
    """Token de acesso OAuth2"""
    token: str
    expires_at: datetime
    token_type: str = "Bearer"

    @property
    def is_expired(self) -> bool:
        """Verifica se o token expirou (com margem de 5 minutos)"""
        return datetime.utcnow() >= (self.expires_at - timedelta(minutes=5))


class SharePointGraphClient:
    """
    Cliente base para Microsoft Graph API - SharePoint.

    Fornece metodos para:
    - Autenticacao OAuth2 com Azure AD
    - Operacoes basicas de sites
    - Gerenciamento de sessoes HTTP
    - Rate limiting e retry automatico

    Exemplo:
        config = SharePointConfig(
            tenant_id="seu-tenant-id",
            client_id="seu-client-id",
            client_secret="seu-client-secret"
        )
        client = SharePointGraphClient(config)
        await client.authenticate()
        sites = await client.list_sites()
    """

    def __init__(self, config: SharePointConfig):
        """
        Inicializa o cliente SharePoint.

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
            self._session = None

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
                    logger.info("Autenticacao com SharePoint bem sucedida")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro na autenticacao SharePoint: {response.status} - {error}")
                    return False

        except Exception as e:
            logger.error(f"Erro ao autenticar com SharePoint: {e}")
            return False

    async def _ensure_authenticated(self) -> bool:
        """Garante que existe um token valido"""
        if self._token is None or self._token.is_expired:
            return await self.authenticate()
        return True

    async def _get_headers(self) -> Dict[str, str]:
        """Retorna headers de autorizacao"""
        await self._ensure_authenticated()
        if not self._token:
            raise RuntimeError("Nao foi possivel obter token de autenticacao")
        return {
            "Authorization": f"{self._token.token_type} {self._token.token}",
            "Content-Type": "application/json"
        }

    async def _request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None,
        headers_override: Optional[Dict] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Faz uma requisicao para a API Graph.

        Args:
            method: Metodo HTTP (GET, POST, PUT, DELETE, PATCH)
            endpoint: Endpoint da API
            data: Dados do corpo da requisicao
            params: Parametros de query string
            headers_override: Headers customizados

        Returns:
            Resposta JSON ou None em caso de erro
        """
        url = f"{self.config.base_url}{endpoint}"
        headers = await self._get_headers()
        if headers_override:
            headers.update(headers_override)

        session = await self._get_session()

        for attempt in range(self.config.max_retries):
            try:
                async with session.request(
                    method,
                    url,
                    json=data if method != "PUT" else None,
                    data=data if method == "PUT" and not isinstance(data, dict) else None,
                    params=params,
                    headers=headers
                ) as response:
                    if response.status == 204:
                        return {"success": True}

                    if response.status in [200, 201, 202]:
                        content_type = response.headers.get("Content-Type", "")
                        if "application/json" in content_type:
                            return await response.json()
                        return {"success": True, "content": await response.text()}

                    if response.status == 429:
                        # Rate limiting
                        retry_after = int(response.headers.get("Retry-After", 10))
                        logger.warning(f"Rate limit SharePoint. Aguardando {retry_after}s...")
                        await asyncio.sleep(retry_after)
                        continue

                    if response.status == 503:
                        # Service unavailable - retry
                        logger.warning(f"SharePoint indisponivel. Tentativa {attempt + 1}...")
                        await asyncio.sleep(self.config.retry_delay_seconds * (attempt + 1))
                        continue

                    if response.status == 404:
                        logger.warning(f"Recurso nao encontrado: {endpoint}")
                        return None

                    error = await response.text()
                    logger.error(f"Erro na requisicao SharePoint: {response.status} - {error}")
                    return None

            except asyncio.TimeoutError:
                logger.warning(f"Timeout na tentativa {attempt + 1}/{self.config.max_retries}")
                if attempt < self.config.max_retries - 1:
                    await asyncio.sleep(self.config.retry_delay_seconds)
                continue

            except aiohttp.ClientError as e:
                logger.error(f"Erro de conexao: {e}")
                if attempt < self.config.max_retries - 1:
                    await asyncio.sleep(self.config.retry_delay_seconds)
                continue

            except Exception as e:
                logger.error(f"Erro inesperado na requisicao: {e}")
                return None

        logger.error(f"Todas as tentativas falharam para {endpoint}")
        return None

    # =========================================================================
    # Sites
    # =========================================================================

    async def list_sites(self, search: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Lista sites SharePoint acessiveis.

        Args:
            search: Termo de busca opcional

        Returns:
            Lista de sites
        """
        params = {}
        if search:
            params["search"] = search

        result = await self._request("GET", "/sites", params=params)
        if result and "value" in result:
            return result["value"]
        return []

    async def get_site(self, site_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de um site.

        Args:
            site_id: ID do site (pode ser hostname:path ou ID)

        Returns:
            Dados do site ou None
        """
        return await self._request("GET", f"/sites/{site_id}")

    async def get_site_by_url(self, hostname: str, path: str) -> Optional[Dict[str, Any]]:
        """
        Obtem site por URL.

        Args:
            hostname: Hostname do SharePoint (ex: contoso.sharepoint.com)
            path: Caminho do site (ex: /sites/MeuSite)

        Returns:
            Dados do site ou None
        """
        site_identifier = f"{hostname}:{path}"
        return await self._request("GET", f"/sites/{site_identifier}")

    async def get_root_site(self) -> Optional[Dict[str, Any]]:
        """
        Obtem o site raiz do tenant.

        Returns:
            Dados do site raiz ou None
        """
        return await self._request("GET", "/sites/root")

    async def list_subsites(self, site_id: str) -> List[Dict[str, Any]]:
        """
        Lista subsites de um site.

        Args:
            site_id: ID do site pai

        Returns:
            Lista de subsites
        """
        result = await self._request("GET", f"/sites/{site_id}/sites")
        if result and "value" in result:
            return result["value"]
        return []

    # =========================================================================
    # Drives (Document Libraries)
    # =========================================================================

    async def list_drives(self, site_id: str) -> List[Dict[str, Any]]:
        """
        Lista bibliotecas de documentos de um site.

        Args:
            site_id: ID do site

        Returns:
            Lista de drives (bibliotecas)
        """
        result = await self._request("GET", f"/sites/{site_id}/drives")
        if result and "value" in result:
            return result["value"]
        return []

    async def get_drive(self, site_id: str, drive_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de uma biblioteca de documentos.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca

        Returns:
            Dados da biblioteca ou None
        """
        return await self._request("GET", f"/sites/{site_id}/drives/{drive_id}")

    async def get_default_drive(self, site_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem a biblioteca de documentos padrao de um site.

        Args:
            site_id: ID do site

        Returns:
            Dados da biblioteca padrao ou None
        """
        return await self._request("GET", f"/sites/{site_id}/drive")

    # =========================================================================
    # Permissions (para isolamento de tenant)
    # =========================================================================

    async def list_permissions(self, site_id: str) -> List[Dict[str, Any]]:
        """
        Lista permissoes de um site.

        Args:
            site_id: ID do site

        Returns:
            Lista de permissoes
        """
        result = await self._request("GET", f"/sites/{site_id}/permissions")
        if result and "value" in result:
            return result["value"]
        return []

    async def check_user_access(self, site_id: str, user_id: str) -> bool:
        """
        Verifica se um usuario tem acesso a um site.

        Args:
            site_id: ID do site
            user_id: ID do usuario

        Returns:
            True se tem acesso, False caso contrario
        """
        try:
            permissions = await self.list_permissions(site_id)
            for perm in permissions:
                granted_to = perm.get("grantedTo", {})
                if granted_to.get("user", {}).get("id") == user_id:
                    return True
            return False
        except Exception:
            return False

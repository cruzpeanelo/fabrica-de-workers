# -*- coding: utf-8 -*-
"""
SharePoint Site Client
======================
Cliente para operacoes em sites SharePoint.

Terminal 5 - Issue #298
"""

import logging
from typing import Any, Dict, List, Optional
from datetime import datetime

from .config import SharePointConfig
from .graph_client import SharePointGraphClient

logger = logging.getLogger(__name__)


class SiteClient(SharePointGraphClient):
    """
    Cliente para operacoes em sites SharePoint.

    Estende o SharePointGraphClient com funcionalidades especificas de sites:
    - Navegacao em sites e subsites
    - Gerenciamento de colunas de site
    - Content types
    - Webhooks de site

    Exemplo:
        config = SharePointConfig(...)
        client = SiteClient(config)
        await client.authenticate()

        # Obter site por URL
        site = await client.get_site_by_path("contoso.sharepoint.com", "/sites/projeto")

        # Listar bibliotecas
        libraries = await client.list_document_libraries(site["id"])
    """

    def __init__(self, config: SharePointConfig):
        """
        Inicializa o cliente de sites.

        Args:
            config: Configuracao do SharePoint
        """
        super().__init__(config)
        self._current_site_id: Optional[str] = None

    async def set_current_site(self, site_id: str) -> bool:
        """
        Define o site atual para operacoes.

        Args:
            site_id: ID do site

        Returns:
            True se site existe e foi definido
        """
        site = await self.get_site(site_id)
        if site:
            self._current_site_id = site_id
            logger.info(f"Site atual definido: {site.get('displayName', site_id)}")
            return True
        return False

    async def get_site_by_path(self, hostname: str, path: str) -> Optional[Dict[str, Any]]:
        """
        Obtem site por hostname e caminho.

        Args:
            hostname: Hostname do SharePoint (ex: contoso.sharepoint.com)
            path: Caminho do site (ex: /sites/MeuSite)

        Returns:
            Dados do site ou None
        """
        return await self.get_site_by_url(hostname, path)

    # =========================================================================
    # Informacoes do Site
    # =========================================================================

    async def get_site_info(self, site_id: Optional[str] = None) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes detalhadas de um site.

        Args:
            site_id: ID do site (usa o atual se nao especificado)

        Returns:
            Informacoes do site ou None
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            logger.error("Nenhum site definido")
            return None

        site = await self.get_site(site_id)
        if not site:
            return None

        # Adiciona informacoes extras
        drives = await self.list_drives(site_id)
        lists = await self.list_lists(site_id)

        return {
            **site,
            "drives": drives,
            "lists": lists,
            "drive_count": len(drives),
            "list_count": len(lists)
        }

    async def list_lists(self, site_id: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Lista todas as listas de um site.

        Args:
            site_id: ID do site

        Returns:
            Lista de listas SharePoint
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return []

        result = await self._request("GET", f"/sites/{site_id}/lists")
        if result and "value" in result:
            return result["value"]
        return []

    async def list_document_libraries(self, site_id: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Lista apenas bibliotecas de documentos (exclui listas).

        Args:
            site_id: ID do site

        Returns:
            Lista de bibliotecas de documentos
        """
        lists = await self.list_lists(site_id)
        # Filtra apenas bibliotecas de documentos
        return [
            lst for lst in lists
            if lst.get("list", {}).get("template") == "documentLibrary"
        ]

    # =========================================================================
    # Colunas de Site
    # =========================================================================

    async def list_site_columns(self, site_id: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Lista colunas de site.

        Args:
            site_id: ID do site

        Returns:
            Lista de colunas
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return []

        result = await self._request("GET", f"/sites/{site_id}/columns")
        if result and "value" in result:
            return result["value"]
        return []

    async def create_site_column(
        self,
        name: str,
        column_type: str,
        description: str = "",
        site_id: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Cria uma nova coluna de site.

        Args:
            name: Nome da coluna
            column_type: Tipo (text, number, dateTime, boolean, etc)
            description: Descricao da coluna
            site_id: ID do site

        Returns:
            Dados da coluna criada ou None
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return None

        data = {
            "name": name,
            "description": description,
            column_type: {}  # Estrutura varia por tipo
        }

        return await self._request("POST", f"/sites/{site_id}/columns", data=data)

    # =========================================================================
    # Content Types
    # =========================================================================

    async def list_content_types(self, site_id: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Lista content types de um site.

        Args:
            site_id: ID do site

        Returns:
            Lista de content types
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return []

        result = await self._request("GET", f"/sites/{site_id}/contentTypes")
        if result and "value" in result:
            return result["value"]
        return []

    async def get_content_type(
        self,
        content_type_id: str,
        site_id: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem um content type especifico.

        Args:
            content_type_id: ID do content type
            site_id: ID do site

        Returns:
            Dados do content type ou None
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return None

        return await self._request(
            "GET",
            f"/sites/{site_id}/contentTypes/{content_type_id}"
        )

    # =========================================================================
    # Webhooks (Subscriptions)
    # =========================================================================

    async def create_subscription(
        self,
        resource: str,
        notification_url: str,
        expiration_datetime: datetime,
        change_type: str = "created,updated,deleted",
        site_id: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Cria uma subscription (webhook) para um recurso.

        Args:
            resource: Recurso para monitorar (ex: /sites/{id}/lists/{id}/items)
            notification_url: URL para receber notificacoes
            expiration_datetime: Data de expiracao da subscription
            change_type: Tipos de mudanca para monitorar
            site_id: ID do site

        Returns:
            Dados da subscription ou None
        """
        data = {
            "changeType": change_type,
            "notificationUrl": notification_url,
            "resource": resource,
            "expirationDateTime": expiration_datetime.isoformat() + "Z",
            "clientState": f"tenant_{self.config.integration_tenant_id}"  # Para validacao
        }

        return await self._request("POST", "/subscriptions", data=data)

    async def list_subscriptions(self) -> List[Dict[str, Any]]:
        """
        Lista todas as subscriptions ativas.

        Returns:
            Lista de subscriptions
        """
        result = await self._request("GET", "/subscriptions")
        if result and "value" in result:
            return result["value"]
        return []

    async def delete_subscription(self, subscription_id: str) -> bool:
        """
        Remove uma subscription.

        Args:
            subscription_id: ID da subscription

        Returns:
            True se removida com sucesso
        """
        result = await self._request("DELETE", f"/subscriptions/{subscription_id}")
        return result is not None

    # =========================================================================
    # Atividade do Site
    # =========================================================================

    async def get_site_analytics(
        self,
        site_id: Optional[str] = None,
        interval: str = "lastSevenDays"
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem analytics de um site.

        Args:
            site_id: ID do site
            interval: Intervalo (lastSevenDays, allTime, etc)

        Returns:
            Dados de analytics ou None
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return None

        return await self._request(
            "GET",
            f"/sites/{site_id}/analytics/{interval}"
        )

    async def list_recent_activities(
        self,
        site_id: Optional[str] = None,
        limit: int = 50
    ) -> List[Dict[str, Any]]:
        """
        Lista atividades recentes no site.

        Args:
            site_id: ID do site
            limit: Numero maximo de atividades

        Returns:
            Lista de atividades
        """
        site_id = site_id or self._current_site_id
        if not site_id:
            return []

        # Obtem atividades via drive principal
        drive = await self.get_default_drive(site_id)
        if not drive:
            return []

        params = {"$top": str(limit)}
        result = await self._request(
            "GET",
            f"/drives/{drive['id']}/activities",
            params=params
        )

        if result and "value" in result:
            return result["value"]
        return []

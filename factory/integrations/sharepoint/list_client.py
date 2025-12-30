# -*- coding: utf-8 -*-
"""
SharePoint List Client
======================
Cliente para operacoes em listas SharePoint.

Terminal 5 - Issue #298
"""

import logging
from typing import Any, Dict, List, Optional

from .config import SharePointConfig
from .graph_client import SharePointGraphClient

logger = logging.getLogger(__name__)


class ListClient(SharePointGraphClient):
    """
    Cliente para operacoes em listas SharePoint.

    Permite:
    - CRUD de listas
    - CRUD de itens de lista
    - Gerenciamento de colunas
    - Filtragem e ordenacao

    Exemplo:
        config = SharePointConfig(...)
        client = ListClient(config)
        await client.authenticate()

        # Listar itens
        items = await client.list_items(site_id, list_id)

        # Criar item
        new_item = await client.create_item(site_id, list_id, {"Title": "Novo Item"})
    """

    def __init__(self, config: SharePointConfig):
        """
        Inicializa o cliente de listas.

        Args:
            config: Configuracao do SharePoint
        """
        super().__init__(config)

    # =========================================================================
    # Operacoes de Lista
    # =========================================================================

    async def get_list(self, site_id: str, list_id: str) -> Optional[Dict[str, Any]]:
        """
        Obtem uma lista especifica.

        Args:
            site_id: ID do site
            list_id: ID ou nome da lista

        Returns:
            Dados da lista ou None
        """
        return await self._request("GET", f"/sites/{site_id}/lists/{list_id}")

    async def get_list_by_name(self, site_id: str, list_name: str) -> Optional[Dict[str, Any]]:
        """
        Obtem uma lista pelo nome.

        Args:
            site_id: ID do site
            list_name: Nome da lista

        Returns:
            Dados da lista ou None
        """
        # Busca lista por nome
        result = await self._request(
            "GET",
            f"/sites/{site_id}/lists",
            params={"$filter": f"displayName eq '{list_name}'"}
        )
        if result and "value" in result and len(result["value"]) > 0:
            return result["value"][0]
        return None

    async def create_list(
        self,
        site_id: str,
        name: str,
        template: str = "genericList",
        description: str = ""
    ) -> Optional[Dict[str, Any]]:
        """
        Cria uma nova lista.

        Args:
            site_id: ID do site
            name: Nome da lista
            template: Template da lista (genericList, documentLibrary, etc)
            description: Descricao

        Returns:
            Dados da lista criada ou None
        """
        data = {
            "displayName": name,
            "description": description,
            "list": {
                "template": template
            }
        }
        return await self._request("POST", f"/sites/{site_id}/lists", data=data)

    async def delete_list(self, site_id: str, list_id: str) -> bool:
        """
        Remove uma lista.

        Args:
            site_id: ID do site
            list_id: ID da lista

        Returns:
            True se removida com sucesso
        """
        result = await self._request("DELETE", f"/sites/{site_id}/lists/{list_id}")
        return result is not None

    # =========================================================================
    # Colunas de Lista
    # =========================================================================

    async def list_columns(self, site_id: str, list_id: str) -> List[Dict[str, Any]]:
        """
        Lista colunas de uma lista.

        Args:
            site_id: ID do site
            list_id: ID da lista

        Returns:
            Lista de colunas
        """
        result = await self._request(
            "GET",
            f"/sites/{site_id}/lists/{list_id}/columns"
        )
        if result and "value" in result:
            return result["value"]
        return []

    async def create_column(
        self,
        site_id: str,
        list_id: str,
        name: str,
        column_definition: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Cria uma coluna na lista.

        Args:
            site_id: ID do site
            list_id: ID da lista
            name: Nome da coluna
            column_definition: Definicao da coluna

        Returns:
            Dados da coluna criada ou None
        """
        data = {
            "name": name,
            **column_definition
        }
        return await self._request(
            "POST",
            f"/sites/{site_id}/lists/{list_id}/columns",
            data=data
        )

    # =========================================================================
    # Itens de Lista
    # =========================================================================

    async def list_items(
        self,
        site_id: str,
        list_id: str,
        select: Optional[List[str]] = None,
        filter_query: Optional[str] = None,
        order_by: Optional[str] = None,
        top: int = 100,
        skip: int = 0,
        expand: Optional[List[str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista itens de uma lista com opcoes de filtragem.

        Args:
            site_id: ID do site
            list_id: ID da lista
            select: Campos a retornar
            filter_query: Filtro OData
            order_by: Ordenacao
            top: Limite de resultados
            skip: Pular N resultados
            expand: Campos para expandir

        Returns:
            Lista de itens
        """
        params = {"$top": str(top)}

        if skip > 0:
            params["$skip"] = str(skip)
        if select:
            params["$select"] = ",".join(select)
        if filter_query:
            params["$filter"] = filter_query
        if order_by:
            params["$orderby"] = order_by
        if expand:
            params["$expand"] = ",".join(expand)

        result = await self._request(
            "GET",
            f"/sites/{site_id}/lists/{list_id}/items",
            params=params
        )

        if result and "value" in result:
            return result["value"]
        return []

    async def get_item(
        self,
        site_id: str,
        list_id: str,
        item_id: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem um item especifico.

        Args:
            site_id: ID do site
            list_id: ID da lista
            item_id: ID do item

        Returns:
            Dados do item ou None
        """
        return await self._request(
            "GET",
            f"/sites/{site_id}/lists/{list_id}/items/{item_id}",
            params={"$expand": "fields"}
        )

    async def create_item(
        self,
        site_id: str,
        list_id: str,
        fields: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Cria um novo item na lista.

        Args:
            site_id: ID do site
            list_id: ID da lista
            fields: Campos do item

        Returns:
            Dados do item criado ou None
        """
        data = {"fields": fields}
        return await self._request(
            "POST",
            f"/sites/{site_id}/lists/{list_id}/items",
            data=data
        )

    async def update_item(
        self,
        site_id: str,
        list_id: str,
        item_id: str,
        fields: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """
        Atualiza um item existente.

        Args:
            site_id: ID do site
            list_id: ID da lista
            item_id: ID do item
            fields: Campos a atualizar

        Returns:
            Dados do item atualizado ou None
        """
        return await self._request(
            "PATCH",
            f"/sites/{site_id}/lists/{list_id}/items/{item_id}/fields",
            data=fields
        )

    async def delete_item(
        self,
        site_id: str,
        list_id: str,
        item_id: str
    ) -> bool:
        """
        Remove um item da lista.

        Args:
            site_id: ID do site
            list_id: ID da lista
            item_id: ID do item

        Returns:
            True se removido com sucesso
        """
        result = await self._request(
            "DELETE",
            f"/sites/{site_id}/lists/{list_id}/items/{item_id}"
        )
        return result is not None

    # =========================================================================
    # Operacoes em Lote (Batch)
    # =========================================================================

    async def batch_create_items(
        self,
        site_id: str,
        list_id: str,
        items: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """
        Cria multiplos itens em lote.

        Args:
            site_id: ID do site
            list_id: ID da lista
            items: Lista de campos para cada item

        Returns:
            Lista de itens criados
        """
        # Graph API suporta batch requests
        # Por simplicidade, fazemos requests individuais
        # TODO: Implementar batch real com $batch endpoint
        results = []
        for item_fields in items:
            result = await self.create_item(site_id, list_id, item_fields)
            if result:
                results.append(result)
        return results

    async def batch_update_items(
        self,
        site_id: str,
        list_id: str,
        updates: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """
        Atualiza multiplos itens em lote.

        Args:
            site_id: ID do site
            list_id: ID da lista
            updates: Lista de dicts com 'id' e 'fields'

        Returns:
            Lista de itens atualizados
        """
        results = []
        for update in updates:
            item_id = update.get("id")
            fields = update.get("fields", {})
            if item_id:
                result = await self.update_item(site_id, list_id, item_id, fields)
                if result:
                    results.append(result)
        return results

    async def batch_delete_items(
        self,
        site_id: str,
        list_id: str,
        item_ids: List[str]
    ) -> int:
        """
        Remove multiplos itens em lote.

        Args:
            site_id: ID do site
            list_id: ID da lista
            item_ids: Lista de IDs a remover

        Returns:
            Numero de itens removidos
        """
        deleted = 0
        for item_id in item_ids:
            if await self.delete_item(site_id, list_id, item_id):
                deleted += 1
        return deleted

    # =========================================================================
    # Busca
    # =========================================================================

    async def search_items(
        self,
        site_id: str,
        list_id: str,
        query: str,
        fields: Optional[List[str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Busca itens por texto.

        Args:
            site_id: ID do site
            list_id: ID da lista
            query: Termo de busca
            fields: Campos para buscar (usa Title por padrao)

        Returns:
            Lista de itens encontrados
        """
        search_fields = fields or ["Title"]
        filter_conditions = []

        for field in search_fields:
            filter_conditions.append(f"contains(fields/{field}, '{query}')")

        filter_query = " or ".join(filter_conditions)

        return await self.list_items(
            site_id,
            list_id,
            filter_query=filter_query
        )

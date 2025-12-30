# -*- coding: utf-8 -*-
"""
SharePoint Document Client
==========================
Cliente para operacoes em documentos SharePoint.

Terminal 5 - Issue #298
"""

import asyncio
import logging
import os
from typing import Any, Dict, List, Optional, BinaryIO
from datetime import datetime

import aiohttp

from .config import SharePointConfig
from .graph_client import SharePointGraphClient

logger = logging.getLogger(__name__)


class DocumentClient(SharePointGraphClient):
    """
    Cliente para operacoes em documentos SharePoint.

    Permite:
    - Upload/download de arquivos
    - Navegacao em pastas
    - Gerenciamento de versoes
    - Compartilhamento de arquivos
    - Sincronizacao bidirecional

    Exemplo:
        config = SharePointConfig(...)
        client = DocumentClient(config)
        await client.authenticate()

        # Upload de arquivo
        result = await client.upload_file(
            site_id, drive_id,
            "/Documentos/relatorio.pdf",
            file_content
        )

        # Download de arquivo
        content = await client.download_file(site_id, drive_id, item_id)
    """

    def __init__(self, config: SharePointConfig):
        """
        Inicializa o cliente de documentos.

        Args:
            config: Configuracao do SharePoint
        """
        super().__init__(config)

    # =========================================================================
    # Navegacao em Pastas
    # =========================================================================

    async def list_folder_contents(
        self,
        site_id: str,
        drive_id: str,
        folder_path: str = "/",
        include_children: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Lista conteudo de uma pasta.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            folder_path: Caminho da pasta (/ para raiz)
            include_children: Incluir subpastas recursivamente

        Returns:
            Lista de itens (arquivos e pastas)
        """
        if folder_path == "/" or folder_path == "":
            endpoint = f"/sites/{site_id}/drives/{drive_id}/root/children"
        else:
            # Remove barra inicial se presente
            path = folder_path.lstrip("/")
            endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}:/children"

        result = await self._request("GET", endpoint)
        if result and "value" in result:
            return result["value"]
        return []

    async def get_folder(
        self,
        site_id: str,
        drive_id: str,
        folder_path: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de uma pasta.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            folder_path: Caminho da pasta

        Returns:
            Dados da pasta ou None
        """
        if folder_path == "/" or folder_path == "":
            endpoint = f"/sites/{site_id}/drives/{drive_id}/root"
        else:
            path = folder_path.lstrip("/")
            endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}"

        return await self._request("GET", endpoint)

    async def create_folder(
        self,
        site_id: str,
        drive_id: str,
        folder_path: str,
        folder_name: str
    ) -> Optional[Dict[str, Any]]:
        """
        Cria uma nova pasta.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            folder_path: Caminho da pasta pai
            folder_name: Nome da nova pasta

        Returns:
            Dados da pasta criada ou None
        """
        if folder_path == "/" or folder_path == "":
            endpoint = f"/sites/{site_id}/drives/{drive_id}/root/children"
        else:
            path = folder_path.lstrip("/")
            endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}:/children"

        data = {
            "name": folder_name,
            "folder": {},
            "@microsoft.graph.conflictBehavior": "rename"
        }

        return await self._request("POST", endpoint, data=data)

    async def delete_folder(
        self,
        site_id: str,
        drive_id: str,
        folder_path: str
    ) -> bool:
        """
        Remove uma pasta e todo seu conteudo.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            folder_path: Caminho da pasta

        Returns:
            True se removida com sucesso
        """
        path = folder_path.lstrip("/")
        endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}"
        result = await self._request("DELETE", endpoint)
        return result is not None

    # =========================================================================
    # Operacoes de Arquivo
    # =========================================================================

    async def get_file_info(
        self,
        site_id: str,
        drive_id: str,
        file_path: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de um arquivo.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            file_path: Caminho do arquivo

        Returns:
            Dados do arquivo ou None
        """
        path = file_path.lstrip("/")
        endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}"
        return await self._request("GET", endpoint)

    async def get_file_by_id(
        self,
        drive_id: str,
        item_id: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem informacoes de um arquivo por ID.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do item

        Returns:
            Dados do arquivo ou None
        """
        return await self._request("GET", f"/drives/{drive_id}/items/{item_id}")

    async def upload_file(
        self,
        site_id: str,
        drive_id: str,
        file_path: str,
        content: bytes,
        conflict_behavior: str = "replace"
    ) -> Optional[Dict[str, Any]]:
        """
        Faz upload de um arquivo (arquivos pequenos, ate 4MB).

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            file_path: Caminho de destino
            content: Conteudo do arquivo em bytes
            conflict_behavior: Comportamento em conflito (replace, rename, fail)

        Returns:
            Dados do arquivo ou None
        """
        if len(content) > 4 * 1024 * 1024:  # 4MB
            return await self.upload_large_file(site_id, drive_id, file_path, content)

        path = file_path.lstrip("/")
        endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}:/content"

        headers = await self._get_headers()
        headers["Content-Type"] = "application/octet-stream"

        session = await self._get_session()

        try:
            async with session.put(
                f"{self.config.base_url}{endpoint}",
                data=content,
                headers=headers,
                params={"@microsoft.graph.conflictBehavior": conflict_behavior}
            ) as response:
                if response.status in [200, 201]:
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro no upload: {response.status} - {error}")
                    return None
        except Exception as e:
            logger.error(f"Erro ao fazer upload: {e}")
            return None

    async def upload_large_file(
        self,
        site_id: str,
        drive_id: str,
        file_path: str,
        content: bytes
    ) -> Optional[Dict[str, Any]]:
        """
        Faz upload de arquivo grande usando sessao de upload.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            file_path: Caminho de destino
            content: Conteudo do arquivo em bytes

        Returns:
            Dados do arquivo ou None
        """
        path = file_path.lstrip("/")
        file_size = len(content)

        # Criar sessao de upload
        create_session_endpoint = f"/sites/{site_id}/drives/{drive_id}/root:/{path}:/createUploadSession"
        session_data = {
            "item": {
                "@microsoft.graph.conflictBehavior": "replace"
            }
        }

        session_result = await self._request("POST", create_session_endpoint, data=session_data)
        if not session_result or "uploadUrl" not in session_result:
            logger.error("Falha ao criar sessao de upload")
            return None

        upload_url = session_result["uploadUrl"]
        chunk_size = self.config.chunk_size
        session = await self._get_session()

        # Upload em chunks
        offset = 0
        while offset < file_size:
            chunk = content[offset:offset + chunk_size]
            chunk_end = offset + len(chunk) - 1

            headers = {
                "Content-Length": str(len(chunk)),
                "Content-Range": f"bytes {offset}-{chunk_end}/{file_size}"
            }

            try:
                async with session.put(upload_url, data=chunk, headers=headers) as response:
                    if response.status == 202:
                        # Chunk aceito, continua
                        offset += len(chunk)
                        logger.debug(f"Upload progress: {offset}/{file_size}")
                    elif response.status in [200, 201]:
                        # Upload completo
                        return await response.json()
                    else:
                        error = await response.text()
                        logger.error(f"Erro no upload chunk: {response.status} - {error}")
                        return None
            except Exception as e:
                logger.error(f"Erro no upload chunk: {e}")
                return None

        return None

    async def download_file(
        self,
        site_id: str,
        drive_id: str,
        item_id: str
    ) -> Optional[bytes]:
        """
        Faz download de um arquivo.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            item_id: ID do arquivo

        Returns:
            Conteudo do arquivo ou None
        """
        endpoint = f"/drives/{drive_id}/items/{item_id}/content"
        headers = await self._get_headers()
        session = await self._get_session()

        try:
            async with session.get(
                f"{self.config.base_url}{endpoint}",
                headers=headers,
                allow_redirects=True
            ) as response:
                if response.status == 200:
                    return await response.read()
                elif response.status == 302:
                    # Redirect para download URL
                    download_url = response.headers.get("Location")
                    if download_url:
                        async with session.get(download_url) as download_response:
                            if download_response.status == 200:
                                return await download_response.read()
                else:
                    logger.error(f"Erro no download: {response.status}")
                    return None
        except Exception as e:
            logger.error(f"Erro ao fazer download: {e}")
            return None

    async def download_file_by_path(
        self,
        site_id: str,
        drive_id: str,
        file_path: str
    ) -> Optional[bytes]:
        """
        Faz download de um arquivo pelo caminho.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            file_path: Caminho do arquivo

        Returns:
            Conteudo do arquivo ou None
        """
        # Primeiro obtem o ID do arquivo
        file_info = await self.get_file_info(site_id, drive_id, file_path)
        if not file_info or "id" not in file_info:
            return None

        return await self.download_file(site_id, drive_id, file_info["id"])

    async def delete_file(
        self,
        site_id: str,
        drive_id: str,
        item_id: str
    ) -> bool:
        """
        Remove um arquivo.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            item_id: ID do arquivo

        Returns:
            True se removido com sucesso
        """
        result = await self._request("DELETE", f"/drives/{drive_id}/items/{item_id}")
        return result is not None

    async def move_file(
        self,
        drive_id: str,
        item_id: str,
        new_parent_id: str,
        new_name: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Move um arquivo para outra pasta.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo
            new_parent_id: ID da nova pasta pai
            new_name: Novo nome (opcional)

        Returns:
            Dados do arquivo movido ou None
        """
        data = {
            "parentReference": {
                "id": new_parent_id
            }
        }
        if new_name:
            data["name"] = new_name

        return await self._request("PATCH", f"/drives/{drive_id}/items/{item_id}", data=data)

    async def copy_file(
        self,
        drive_id: str,
        item_id: str,
        new_parent_id: str,
        new_name: Optional[str] = None
    ) -> Optional[str]:
        """
        Copia um arquivo para outra pasta.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo
            new_parent_id: ID da pasta destino
            new_name: Novo nome (opcional)

        Returns:
            URL para monitorar a operacao ou None
        """
        data = {
            "parentReference": {
                "driveId": drive_id,
                "id": new_parent_id
            }
        }
        if new_name:
            data["name"] = new_name

        result = await self._request(
            "POST",
            f"/drives/{drive_id}/items/{item_id}/copy",
            data=data
        )
        # Copy retorna Location header com URL de monitoramento
        return result.get("Location") if result else None

    # =========================================================================
    # Versoes
    # =========================================================================

    async def list_versions(
        self,
        drive_id: str,
        item_id: str
    ) -> List[Dict[str, Any]]:
        """
        Lista versoes de um arquivo.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo

        Returns:
            Lista de versoes
        """
        result = await self._request("GET", f"/drives/{drive_id}/items/{item_id}/versions")
        if result and "value" in result:
            return result["value"]
        return []

    async def get_version(
        self,
        drive_id: str,
        item_id: str,
        version_id: str
    ) -> Optional[Dict[str, Any]]:
        """
        Obtem uma versao especifica.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo
            version_id: ID da versao

        Returns:
            Dados da versao ou None
        """
        return await self._request(
            "GET",
            f"/drives/{drive_id}/items/{item_id}/versions/{version_id}"
        )

    async def restore_version(
        self,
        drive_id: str,
        item_id: str,
        version_id: str
    ) -> bool:
        """
        Restaura uma versao anterior.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo
            version_id: ID da versao

        Returns:
            True se restaurado com sucesso
        """
        result = await self._request(
            "POST",
            f"/drives/{drive_id}/items/{item_id}/versions/{version_id}/restoreVersion"
        )
        return result is not None

    # =========================================================================
    # Compartilhamento
    # =========================================================================

    async def create_sharing_link(
        self,
        drive_id: str,
        item_id: str,
        link_type: str = "view",
        scope: str = "organization",
        password: Optional[str] = None,
        expiration: Optional[datetime] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Cria um link de compartilhamento.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo
            link_type: Tipo do link (view, edit, embed)
            scope: Escopo (anonymous, organization, users)
            password: Senha para o link (opcional)
            expiration: Data de expiracao (opcional)

        Returns:
            Dados do link ou None
        """
        data = {
            "type": link_type,
            "scope": scope
        }
        if password:
            data["password"] = password
        if expiration:
            data["expirationDateTime"] = expiration.isoformat() + "Z"

        return await self._request(
            "POST",
            f"/drives/{drive_id}/items/{item_id}/createLink",
            data=data
        )

    async def list_permissions(
        self,
        drive_id: str,
        item_id: str
    ) -> List[Dict[str, Any]]:
        """
        Lista permissoes de um arquivo.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo

        Returns:
            Lista de permissoes
        """
        result = await self._request(
            "GET",
            f"/drives/{drive_id}/items/{item_id}/permissions"
        )
        if result and "value" in result:
            return result["value"]
        return []

    # =========================================================================
    # Busca
    # =========================================================================

    async def search_files(
        self,
        site_id: str,
        drive_id: str,
        query: str,
        top: int = 25
    ) -> List[Dict[str, Any]]:
        """
        Busca arquivos por texto.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            query: Termo de busca

        Returns:
            Lista de arquivos encontrados
        """
        endpoint = f"/sites/{site_id}/drives/{drive_id}/root/search(q='{query}')"
        result = await self._request("GET", endpoint, params={"$top": str(top)})

        if result and "value" in result:
            return result["value"]
        return []

    # =========================================================================
    # Delta (Sync)
    # =========================================================================

    async def get_delta(
        self,
        drive_id: str,
        delta_token: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Obtem mudancas desde o ultimo sync.

        Args:
            drive_id: ID da biblioteca
            delta_token: Token do ultimo delta (None para sync inicial)

        Returns:
            Dict com 'value' (mudancas) e 'deltaLink' (proximo token)
        """
        if delta_token:
            # Usa o delta link completo
            endpoint = delta_token
            result = await self._request("GET", endpoint)
        else:
            endpoint = f"/drives/{drive_id}/root/delta"
            result = await self._request("GET", endpoint)

        return result or {"value": [], "deltaLink": None}

    async def sync_folder(
        self,
        drive_id: str,
        local_path: str,
        remote_folder: str = "/",
        delta_token: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Sincroniza uma pasta local com o SharePoint.

        Args:
            drive_id: ID da biblioteca
            local_path: Caminho local
            remote_folder: Pasta remota
            delta_token: Token de sync anterior

        Returns:
            Dict com resultado do sync e novo delta_token
        """
        delta = await self.get_delta(drive_id, delta_token)
        changes = delta.get("value", [])
        new_token = delta.get("@odata.deltaLink") or delta.get("deltaLink")

        sync_result = {
            "downloaded": [],
            "uploaded": [],
            "deleted": [],
            "errors": [],
            "delta_token": new_token
        }

        for change in changes:
            item_name = change.get("name", "")
            item_path = change.get("parentReference", {}).get("path", "") + "/" + item_name

            if change.get("deleted"):
                # Item deletado no SharePoint
                local_file = os.path.join(local_path, item_path.lstrip("/"))
                if os.path.exists(local_file):
                    try:
                        os.remove(local_file)
                        sync_result["deleted"].append(item_path)
                    except Exception as e:
                        sync_result["errors"].append(f"Erro ao deletar {item_path}: {e}")
            elif "file" in change:
                # Arquivo novo ou modificado
                sync_result["downloaded"].append(item_path)
                # TODO: Implementar download real

        return sync_result

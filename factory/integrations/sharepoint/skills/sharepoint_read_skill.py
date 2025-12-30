# -*- coding: utf-8 -*-
"""
SharePoint Read Skill
=====================
Skill para leitura de dados do SharePoint.

Terminal 5 - Issue #298
"""

import logging
from typing import Any, Dict, List, Optional
from dataclasses import dataclass, field

from ..config import SharePointConfig
from ..site_client import SiteClient
from ..list_client import ListClient
from ..document_client import DocumentClient

logger = logging.getLogger(__name__)


@dataclass
class SharePointReadResult:
    """Resultado de uma operacao de leitura"""
    success: bool
    data: Any = None
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class SharePointReadSkill:
    """
    Skill para leitura de dados do SharePoint.

    Permite ler:
    - Sites e suas informacoes
    - Listas e itens
    - Documentos e metadados
    - Estrutura de pastas

    Isolamento por tenant garantido atraves do config.integration_tenant_id.

    Exemplo:
        config = SharePointConfig(
            tenant_id="...",
            client_id="...",
            client_secret="...",
            integration_tenant_id="TENANT-001"
        )
        skill = SharePointReadSkill(config)

        # Ler sites
        result = await skill.read_sites()

        # Ler documentos de uma pasta
        result = await skill.read_documents(site_id, drive_id, "/Projetos")
    """

    def __init__(self, config: SharePointConfig):
        """
        Inicializa o skill de leitura.

        Args:
            config: Configuracao do SharePoint
        """
        self.config = config
        self._site_client: Optional[SiteClient] = None
        self._list_client: Optional[ListClient] = None
        self._document_client: Optional[DocumentClient] = None
        self._authenticated = False

    async def _ensure_clients(self) -> bool:
        """Inicializa e autentica os clientes se necessario"""
        if self._authenticated:
            return True

        try:
            self._site_client = SiteClient(self.config)
            self._list_client = ListClient(self.config)
            self._document_client = DocumentClient(self.config)

            # Autentica apenas um (compartilham a mesma config)
            if await self._site_client.authenticate():
                self._authenticated = True
                # Copia o token para os outros clientes
                self._list_client._token = self._site_client._token
                self._document_client._token = self._site_client._token
                return True

            return False
        except Exception as e:
            logger.error(f"Erro ao inicializar clientes: {e}")
            return False

    async def close(self) -> None:
        """Fecha todas as conexoes"""
        if self._site_client:
            await self._site_client.close()
        if self._list_client:
            await self._list_client.close()
        if self._document_client:
            await self._document_client.close()

    # =========================================================================
    # Leitura de Sites
    # =========================================================================

    async def read_sites(self, search: Optional[str] = None) -> SharePointReadResult:
        """
        Le sites disponiveis.

        Args:
            search: Termo de busca opcional

        Returns:
            Resultado com lista de sites
        """
        if not await self._ensure_clients():
            return SharePointReadResult(
                success=False,
                error="Falha na autenticacao"
            )

        try:
            sites = await self._site_client.list_sites(search)
            return SharePointReadResult(
                success=True,
                data=sites,
                metadata={"count": len(sites), "tenant_id": self.config.integration_tenant_id}
            )
        except Exception as e:
            logger.error(f"Erro ao ler sites: {e}")
            return SharePointReadResult(success=False, error=str(e))

    async def read_site_info(self, site_id: str) -> SharePointReadResult:
        """
        Le informacoes detalhadas de um site.

        Args:
            site_id: ID do site

        Returns:
            Resultado com informacoes do site
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            site = await self._site_client.get_site_info(site_id)
            if site:
                return SharePointReadResult(
                    success=True,
                    data=site,
                    metadata={"tenant_id": self.config.integration_tenant_id}
                )
            return SharePointReadResult(success=False, error="Site nao encontrado")
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    # =========================================================================
    # Leitura de Listas
    # =========================================================================

    async def read_lists(self, site_id: str) -> SharePointReadResult:
        """
        Le listas de um site.

        Args:
            site_id: ID do site

        Returns:
            Resultado com lista de listas
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            # Usa site_client que ja tem o metodo
            lists = await self._site_client.list_lists(site_id)
            return SharePointReadResult(
                success=True,
                data=lists,
                metadata={"count": len(lists), "site_id": site_id}
            )
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    async def read_list_items(
        self,
        site_id: str,
        list_id: str,
        filter_query: Optional[str] = None,
        top: int = 100
    ) -> SharePointReadResult:
        """
        Le itens de uma lista.

        Args:
            site_id: ID do site
            list_id: ID da lista
            filter_query: Filtro OData opcional
            top: Limite de resultados

        Returns:
            Resultado com itens da lista
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            items = await self._list_client.list_items(
                site_id, list_id,
                filter_query=filter_query,
                top=top
            )
            return SharePointReadResult(
                success=True,
                data=items,
                metadata={
                    "count": len(items),
                    "site_id": site_id,
                    "list_id": list_id
                }
            )
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    # =========================================================================
    # Leitura de Documentos
    # =========================================================================

    async def read_document_libraries(self, site_id: str) -> SharePointReadResult:
        """
        Le bibliotecas de documentos de um site.

        Args:
            site_id: ID do site

        Returns:
            Resultado com bibliotecas
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            libraries = await self._site_client.list_document_libraries(site_id)
            return SharePointReadResult(
                success=True,
                data=libraries,
                metadata={"count": len(libraries), "site_id": site_id}
            )
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    async def read_documents(
        self,
        site_id: str,
        drive_id: str,
        folder_path: str = "/"
    ) -> SharePointReadResult:
        """
        Le documentos de uma pasta.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            folder_path: Caminho da pasta

        Returns:
            Resultado com lista de documentos
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            documents = await self._document_client.list_folder_contents(
                site_id, drive_id, folder_path
            )

            # Separa arquivos e pastas
            files = [d for d in documents if "file" in d]
            folders = [d for d in documents if "folder" in d]

            return SharePointReadResult(
                success=True,
                data={
                    "files": files,
                    "folders": folders,
                    "all": documents
                },
                metadata={
                    "file_count": len(files),
                    "folder_count": len(folders),
                    "path": folder_path
                }
            )
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    async def read_document_content(
        self,
        site_id: str,
        drive_id: str,
        file_path: str
    ) -> SharePointReadResult:
        """
        Le o conteudo de um documento.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            file_path: Caminho do arquivo

        Returns:
            Resultado com conteudo do arquivo
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            content = await self._document_client.download_file_by_path(
                site_id, drive_id, file_path
            )

            if content:
                # Obtem metadados do arquivo
                file_info = await self._document_client.get_file_info(
                    site_id, drive_id, file_path
                )

                return SharePointReadResult(
                    success=True,
                    data=content,
                    metadata={
                        "size": len(content),
                        "file_info": file_info,
                        "path": file_path
                    }
                )
            return SharePointReadResult(success=False, error="Arquivo nao encontrado")
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    async def search_documents(
        self,
        site_id: str,
        drive_id: str,
        query: str
    ) -> SharePointReadResult:
        """
        Busca documentos por texto.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            query: Termo de busca

        Returns:
            Resultado com documentos encontrados
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            results = await self._document_client.search_files(
                site_id, drive_id, query
            )
            return SharePointReadResult(
                success=True,
                data=results,
                metadata={"query": query, "count": len(results)}
            )
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    # =========================================================================
    # Leitura de Metadados
    # =========================================================================

    async def read_file_metadata(
        self,
        site_id: str,
        drive_id: str,
        file_path: str
    ) -> SharePointReadResult:
        """
        Le metadados de um arquivo.

        Args:
            site_id: ID do site
            drive_id: ID da biblioteca
            file_path: Caminho do arquivo

        Returns:
            Resultado com metadados
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            file_info = await self._document_client.get_file_info(
                site_id, drive_id, file_path
            )

            if file_info:
                # Adiciona versoes
                versions = await self._document_client.list_versions(
                    drive_id, file_info["id"]
                )

                return SharePointReadResult(
                    success=True,
                    data={
                        "info": file_info,
                        "versions": versions
                    },
                    metadata={"version_count": len(versions)}
                )
            return SharePointReadResult(success=False, error="Arquivo nao encontrado")
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

    async def read_permissions(
        self,
        drive_id: str,
        item_id: str
    ) -> SharePointReadResult:
        """
        Le permissoes de um arquivo.

        Args:
            drive_id: ID da biblioteca
            item_id: ID do arquivo

        Returns:
            Resultado com permissoes
        """
        if not await self._ensure_clients():
            return SharePointReadResult(success=False, error="Falha na autenticacao")

        try:
            permissions = await self._document_client.list_permissions(drive_id, item_id)
            return SharePointReadResult(
                success=True,
                data=permissions,
                metadata={"count": len(permissions)}
            )
        except Exception as e:
            return SharePointReadResult(success=False, error=str(e))

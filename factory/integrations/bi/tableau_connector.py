# -*- coding: utf-8 -*-
"""
Tableau Connector
=================
Conector nativo para Tableau Server e Tableau Online.

Funcionalidades:
- Publicar datasources
- Refresh de extracts
- Acesso a workbooks e views
- Embed com token trusted
- Hyper file generation

Configuracao via variaveis de ambiente:
- TABLEAU_SERVER: URL do servidor Tableau
- TABLEAU_SITE_ID: ID do site
- TABLEAU_TOKEN_NAME: Nome do Personal Access Token
- TABLEAU_TOKEN_SECRET: Secret do token
- TABLEAU_API_VERSION: Versao da API (default: 3.19)

Issue #117 - Conectores Nativos para Power BI, Tableau e Excel
"""

import os
import logging
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, BinaryIO
from enum import Enum
import json

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False

logger = logging.getLogger(__name__)


class TableauContentType(str, Enum):
    """Tipos de conteudo Tableau"""
    WORKBOOK = "workbook"
    DATASOURCE = "datasource"
    VIEW = "view"
    PROJECT = "project"


class RefreshMode(str, Enum):
    """Modos de refresh"""
    FULL = "full"
    INCREMENTAL = "incremental"


@dataclass
class TableauConfig:
    """Configuracao do Tableau Connector"""
    server: str = ""
    site_id: str = ""
    token_name: str = ""
    token_secret: str = ""
    api_version: str = "3.19"
    username: str = ""
    password: str = ""
    timeout: int = 60

    @classmethod
    def from_env(cls) -> "TableauConfig":
        return cls(
            server=os.getenv("TABLEAU_SERVER", ""),
            site_id=os.getenv("TABLEAU_SITE_ID", ""),
            token_name=os.getenv("TABLEAU_TOKEN_NAME", ""),
            token_secret=os.getenv("TABLEAU_TOKEN_SECRET", ""),
            api_version=os.getenv("TABLEAU_API_VERSION", "3.19"),
            username=os.getenv("TABLEAU_USERNAME", ""),
            password=os.getenv("TABLEAU_PASSWORD", ""),
            timeout=int(os.getenv("TABLEAU_TIMEOUT", "60"))
        )

    def is_valid(self) -> bool:
        if not self.server:
            return False
        # PAT ou username/password
        return bool(self.token_name and self.token_secret) or bool(self.username and self.password)


@dataclass
class TableauDatasource:
    """Definicao de datasource Tableau"""
    name: str
    project_id: str
    connection_type: str = "web"  # web, file, database
    description: str = ""


class TableauConnector:
    """
    Conector para Tableau Server/Online.

    Permite:
    - Autenticacao via PAT ou username/password
    - Gerenciar workbooks e datasources
    - Refresh de extracts
    - Download/upload de conteudo
    - Trusted tickets para embed

    Exemplo:
    ```python
    config = TableauConfig.from_env()
    connector = TableauConnector(config)

    # Conectar
    await connector.connect()

    # Listar workbooks
    workbooks = await connector.list_workbooks()

    # Refresh de datasource
    await connector.refresh_datasource(datasource_id)

    # Gerar trusted ticket
    ticket = await connector.get_trusted_ticket(username)
    embed_url = f"https://tableau.server.com/trusted/{ticket}/views/workbook/view"
    ```
    """

    def __init__(self, config: Optional[TableauConfig] = None, tenant_id: str = ""):
        self.config = config or TableauConfig.from_env()
        self.tenant_id = tenant_id
        self._auth_token: Optional[str] = None
        self._site_id: Optional[str] = None
        self._user_id: Optional[str] = None
        self._session: Optional[aiohttp.ClientSession] = None

    @property
    def api_url(self) -> str:
        return f"{self.config.server.rstrip('/')}/api/{self.config.api_version}"

    async def _ensure_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    def _get_headers(self) -> Dict[str, str]:
        headers = {
            "Content-Type": "application/json",
            "Accept": "application/json"
        }
        if self._auth_token:
            headers["X-Tableau-Auth"] = self._auth_token
        return headers

    async def connect(self) -> bool:
        """Autentica no Tableau"""
        if not self.config.is_valid():
            logger.error("Configuracao Tableau invalida")
            return False

        try:
            session = await self._ensure_session()

            # Construir payload de autenticacao
            if self.config.token_name and self.config.token_secret:
                # Personal Access Token
                credentials = {
                    "credentials": {
                        "personalAccessTokenName": self.config.token_name,
                        "personalAccessTokenSecret": self.config.token_secret,
                        "site": {"contentUrl": self.config.site_id}
                    }
                }
            else:
                # Username/Password
                credentials = {
                    "credentials": {
                        "name": self.config.username,
                        "password": self.config.password,
                        "site": {"contentUrl": self.config.site_id}
                    }
                }

            url = f"{self.api_url}/auth/signin"

            async with session.post(
                url,
                json=credentials,
                headers={"Content-Type": "application/json"}
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    creds = data.get("credentials", {})
                    self._auth_token = creds.get("token")
                    self._site_id = creds.get("site", {}).get("id")
                    self._user_id = creds.get("user", {}).get("id")
                    logger.info(f"Conectado ao Tableau: {self.config.server}")
                    return True
                else:
                    error = await response.text()
                    logger.error(f"Erro ao autenticar: {error}")
                    return False

        except Exception as e:
            logger.error(f"Erro ao conectar ao Tableau: {e}")
            return False

    async def disconnect(self):
        """Desconecta do Tableau"""
        if self._auth_token:
            try:
                session = await self._ensure_session()
                url = f"{self.api_url}/auth/signout"
                await session.post(url, headers=self._get_headers())
            except Exception:
                pass

        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._auth_token = None
        self._site_id = None

    # ====================
    # Projects
    # ====================

    async def list_projects(self) -> List[Dict]:
        """Lista projetos do site"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/projects"

        async with session.get(url, headers=self._get_headers()) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("projects", {}).get("project", [])
            return []

    async def get_project(self, project_id: str) -> Optional[Dict]:
        """Busca projeto por ID"""
        projects = await self.list_projects()
        for project in projects:
            if project.get("id") == project_id:
                return project
        return None

    async def create_project(
        self,
        name: str,
        description: str = "",
        parent_project_id: Optional[str] = None
    ) -> Optional[str]:
        """Cria novo projeto"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/projects"

        payload = {
            "project": {
                "name": name,
                "description": description
            }
        }

        if parent_project_id:
            payload["project"]["parentProjectId"] = parent_project_id

        async with session.post(url, headers=self._get_headers(), json=payload) as response:
            if response.status == 201:
                data = await response.json()
                return data.get("project", {}).get("id")
            return None

    # ====================
    # Workbooks
    # ====================

    async def list_workbooks(
        self,
        filter_name: Optional[str] = None,
        page_size: int = 100,
        page_number: int = 1
    ) -> List[Dict]:
        """Lista workbooks do site"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/workbooks"

        params = {
            "pageSize": page_size,
            "pageNumber": page_number
        }

        if filter_name:
            params["filter"] = f"name:eq:{filter_name}"

        async with session.get(url, headers=self._get_headers(), params=params) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("workbooks", {}).get("workbook", [])
            return []

    async def get_workbook(self, workbook_id: str) -> Optional[Dict]:
        """Busca workbook por ID"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/workbooks/{workbook_id}"

        async with session.get(url, headers=self._get_headers()) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("workbook")
            return None

    async def delete_workbook(self, workbook_id: str) -> bool:
        """Deleta workbook"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/workbooks/{workbook_id}"

        async with session.delete(url, headers=self._get_headers()) as response:
            return response.status == 204

    async def get_workbook_views(self, workbook_id: str) -> List[Dict]:
        """Lista views de um workbook"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/workbooks/{workbook_id}/views"

        async with session.get(url, headers=self._get_headers()) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("views", {}).get("view", [])
            return []

    async def get_view_image(
        self,
        view_id: str,
        resolution: str = "high",
        max_age: int = 1
    ) -> Optional[bytes]:
        """
        Retorna imagem PNG de uma view.

        Args:
            view_id: ID da view
            resolution: low, medium, high
            max_age: Idade maxima em minutos

        Returns:
            Bytes da imagem ou None
        """
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/views/{view_id}/image"

        params = {"resolution": resolution, "maxAge": max_age}

        async with session.get(url, headers=self._get_headers(), params=params) as response:
            if response.status == 200:
                return await response.read()
            return None

    # ====================
    # Datasources
    # ====================

    async def list_datasources(
        self,
        page_size: int = 100,
        page_number: int = 1
    ) -> List[Dict]:
        """Lista datasources do site"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/datasources"

        params = {"pageSize": page_size, "pageNumber": page_number}

        async with session.get(url, headers=self._get_headers(), params=params) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("datasources", {}).get("datasource", [])
            return []

    async def get_datasource(self, datasource_id: str) -> Optional[Dict]:
        """Busca datasource por ID"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/datasources/{datasource_id}"

        async with session.get(url, headers=self._get_headers()) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("datasource")
            return None

    async def refresh_datasource(self, datasource_id: str) -> Optional[str]:
        """
        Dispara refresh de datasource.

        Args:
            datasource_id: ID do datasource

        Returns:
            Job ID ou None
        """
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/datasources/{datasource_id}/refresh"

        async with session.post(url, headers=self._get_headers(), json={}) as response:
            if response.status == 202:
                data = await response.json()
                job_id = data.get("job", {}).get("id")
                logger.info(f"Refresh iniciado: {job_id}")
                return job_id
            else:
                error = await response.text()
                logger.error(f"Erro ao refresh: {error}")
                return None

    async def delete_datasource(self, datasource_id: str) -> bool:
        """Deleta datasource"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/datasources/{datasource_id}"

        async with session.delete(url, headers=self._get_headers()) as response:
            return response.status == 204

    # ====================
    # Jobs
    # ====================

    async def get_job(self, job_id: str) -> Optional[Dict]:
        """Busca status de job"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/jobs/{job_id}"

        async with session.get(url, headers=self._get_headers()) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("job")
            return None

    async def wait_for_job(
        self,
        job_id: str,
        timeout_seconds: int = 300,
        poll_interval: int = 5
    ) -> bool:
        """
        Aguarda conclusao de job.

        Args:
            job_id: ID do job
            timeout_seconds: Timeout em segundos
            poll_interval: Intervalo de verificacao

        Returns:
            True se job concluido com sucesso
        """
        import asyncio

        start_time = datetime.utcnow()

        while (datetime.utcnow() - start_time).seconds < timeout_seconds:
            job = await self.get_job(job_id)
            if not job:
                return False

            status = job.get("finishCode")
            if status == 0:  # Success
                return True
            elif status == 1:  # Failed
                logger.error(f"Job falhou: {job.get('notes')}")
                return False
            elif status == 2:  # Cancelled
                logger.warning("Job cancelado")
                return False

            await asyncio.sleep(poll_interval)

        logger.error("Timeout aguardando job")
        return False

    # ====================
    # Trusted Tickets
    # ====================

    async def get_trusted_ticket(
        self,
        username: str,
        target_site: Optional[str] = None
    ) -> Optional[str]:
        """
        Gera trusted ticket para embed.

        Args:
            username: Nome do usuario
            target_site: Site de destino (opcional)

        Returns:
            Ticket ou None
        """
        session = await self._ensure_session()

        # Trusted tickets usam endpoint diferente
        url = f"{self.config.server.rstrip('/')}/trusted"

        data = {
            "username": username
        }

        if target_site:
            data["target_site"] = target_site

        async with session.post(url, data=data) as response:
            if response.status == 200:
                ticket = await response.text()
                if ticket and ticket != "-1":
                    return ticket
            return None

    # ====================
    # Users
    # ====================

    async def list_users(
        self,
        page_size: int = 100,
        page_number: int = 1
    ) -> List[Dict]:
        """Lista usuarios do site"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/users"

        params = {"pageSize": page_size, "pageNumber": page_number}

        async with session.get(url, headers=self._get_headers(), params=params) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("users", {}).get("user", [])
            return []

    async def get_user(self, user_id: str) -> Optional[Dict]:
        """Busca usuario por ID"""
        session = await self._ensure_session()
        url = f"{self.api_url}/sites/{self._site_id}/users/{user_id}"

        async with session.get(url, headers=self._get_headers()) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("user")
            return None

    # ====================
    # Factory Agents Integration
    # ====================

    async def sync_stories_to_hyper(
        self,
        stories: List[Dict],
        output_path: str
    ) -> bool:
        """
        Exporta stories para arquivo Hyper (Tableau).

        Args:
            stories: Lista de stories
            output_path: Caminho do arquivo de saida

        Returns:
            True se exportado com sucesso

        Nota: Requer tableauhyperapi instalado
        """
        try:
            from tableauhyperapi import (
                HyperProcess, Telemetry, Connection, CreateMode,
                TableDefinition, SqlType, Inserter, TableName
            )
        except ImportError:
            logger.error("tableauhyperapi nao instalado. Use: pip install tableauhyperapi")
            return False

        try:
            # Iniciar Hyper
            with HyperProcess(telemetry=Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU) as hyper:
                with Connection(
                    hyper.endpoint,
                    output_path,
                    CreateMode.CREATE_AND_REPLACE
                ) as connection:
                    # Definir schema
                    table_def = TableDefinition(
                        TableName("Extract", "Stories"),
                        [
                            TableDefinition.Column("story_id", SqlType.text()),
                            TableDefinition.Column("title", SqlType.text()),
                            TableDefinition.Column("status", SqlType.text()),
                            TableDefinition.Column("priority", SqlType.text()),
                            TableDefinition.Column("story_points", SqlType.int()),
                            TableDefinition.Column("assignee", SqlType.text()),
                            TableDefinition.Column("sprint_id", SqlType.text()),
                            TableDefinition.Column("created_at", SqlType.timestamp()),
                            TableDefinition.Column("tenant_id", SqlType.text())
                        ]
                    )

                    connection.catalog.create_schema("Extract")
                    connection.catalog.create_table(table_def)

                    # Inserir dados
                    with Inserter(connection, table_def) as inserter:
                        for story in stories:
                            created_at = story.get("created_at")
                            if isinstance(created_at, str):
                                created_at = datetime.fromisoformat(created_at.replace("Z", ""))

                            inserter.add_row([
                                story.get("story_id", ""),
                                story.get("title", "")[:1000],
                                story.get("status", ""),
                                story.get("priority", ""),
                                story.get("story_points", 0) or 0,
                                story.get("assignee", "") or "",
                                story.get("sprint_id", "") or "",
                                created_at,
                                self.tenant_id
                            ])

                        inserter.execute()

            logger.info(f"Hyper file criado: {output_path}")
            return True

        except Exception as e:
            logger.error(f"Erro ao criar Hyper file: {e}")
            return False


# Singleton
_tableau_instance: Optional[TableauConnector] = None


def get_tableau_connector(tenant_id: str = "") -> TableauConnector:
    """Retorna instancia global"""
    global _tableau_instance
    if _tableau_instance is None:
        _tableau_instance = TableauConnector(tenant_id=tenant_id)
    return _tableau_instance

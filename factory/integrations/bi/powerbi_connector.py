# -*- coding: utf-8 -*-
"""
Power BI Connector
==================
Conector nativo para Microsoft Power BI.

Funcionalidades:
- Push datasets para Power BI
- Refresh de datasets
- Streaming data
- Embedded reports
- Autenticacao via Azure AD

Configuracao via variaveis de ambiente:
- POWERBI_CLIENT_ID: Client ID do Azure AD
- POWERBI_CLIENT_SECRET: Client Secret
- POWERBI_TENANT_ID: Tenant ID do Azure
- POWERBI_WORKSPACE_ID: ID do workspace

Issue #117 - Conectores Nativos para Power BI, Tableau e Excel
"""

import os
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False

logger = logging.getLogger(__name__)


class PowerBIDataType(str, Enum):
    """Tipos de dados Power BI"""
    STRING = "String"
    INT64 = "Int64"
    DOUBLE = "Double"
    BOOLEAN = "Boolean"
    DATETIME = "DateTime"
    DATETIMEZONE = "DateTimeZone"


class RefreshType(str, Enum):
    """Tipos de refresh"""
    FULL = "Full"
    INCREMENTAL = "Incremental"
    ON_DEMAND = "OnDemand"


@dataclass
class PowerBIColumn:
    """Definicao de coluna Power BI"""
    name: str
    data_type: PowerBIDataType

    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "dataType": self.data_type.value
        }


@dataclass
class PowerBITable:
    """Definicao de tabela Power BI"""
    name: str
    columns: List[PowerBIColumn]

    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "columns": [col.to_dict() for col in self.columns]
        }


@dataclass
class PowerBIDataset:
    """Definicao de dataset Power BI"""
    name: str
    tables: List[PowerBITable]
    default_mode: str = "Push"

    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "defaultMode": self.default_mode,
            "tables": [table.to_dict() for table in self.tables]
        }


@dataclass
class PowerBIConfig:
    """Configuracao do Power BI Connector"""
    client_id: str = ""
    client_secret: str = ""
    tenant_id: str = ""
    workspace_id: str = ""
    authority: str = "https://login.microsoftonline.com"
    resource: str = "https://analysis.windows.net/powerbi/api"
    api_url: str = "https://api.powerbi.com/v1.0/myorg"
    timeout: int = 60

    @classmethod
    def from_env(cls) -> "PowerBIConfig":
        return cls(
            client_id=os.getenv("POWERBI_CLIENT_ID", ""),
            client_secret=os.getenv("POWERBI_CLIENT_SECRET", ""),
            tenant_id=os.getenv("POWERBI_TENANT_ID", ""),
            workspace_id=os.getenv("POWERBI_WORKSPACE_ID", ""),
            timeout=int(os.getenv("POWERBI_TIMEOUT", "60"))
        )

    def is_valid(self) -> bool:
        return bool(self.client_id and self.client_secret and self.tenant_id)


class PowerBIConnector:
    """
    Conector para Microsoft Power BI.

    Permite:
    - Criar e gerenciar datasets
    - Push de dados em tempo real
    - Refresh de datasets
    - Acesso a reports e dashboards

    Exemplo:
    ```python
    config = PowerBIConfig.from_env()
    connector = PowerBIConnector(config)

    # Conectar
    await connector.connect()

    # Criar dataset
    dataset = PowerBIDataset(
        name="FactoryAgents",
        tables=[
            PowerBITable(
                name="Stories",
                columns=[
                    PowerBIColumn("story_id", PowerBIDataType.STRING),
                    PowerBIColumn("title", PowerBIDataType.STRING),
                    PowerBIColumn("status", PowerBIDataType.STRING),
                    PowerBIColumn("story_points", PowerBIDataType.INT64),
                    PowerBIColumn("created_at", PowerBIDataType.DATETIME)
                ]
            )
        ]
    )
    dataset_id = await connector.create_dataset(dataset)

    # Push de dados
    rows = [
        {"story_id": "STR-001", "title": "Story 1", "status": "done", "story_points": 5},
        {"story_id": "STR-002", "title": "Story 2", "status": "in_progress", "story_points": 3}
    ]
    await connector.push_rows(dataset_id, "Stories", rows)
    ```
    """

    def __init__(self, config: Optional[PowerBIConfig] = None, tenant_id: str = ""):
        self.config = config or PowerBIConfig.from_env()
        self.tenant_id = tenant_id  # Tenant da fabrica de agentes
        self._access_token: Optional[str] = None
        self._token_expires_at: Optional[datetime] = None
        self._session: Optional[aiohttp.ClientSession] = None

    @property
    def is_token_valid(self) -> bool:
        if not self._access_token or not self._token_expires_at:
            return False
        return datetime.utcnow() < (self._token_expires_at - timedelta(seconds=60))

    async def _ensure_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    async def connect(self) -> bool:
        """Autentica com Azure AD e obtem token"""
        if not self.config.is_valid():
            logger.error("Configuracao Power BI invalida")
            return False

        try:
            await self._fetch_token()
            logger.info("Conectado ao Power BI")
            return True
        except Exception as e:
            logger.error(f"Erro ao conectar ao Power BI: {e}")
            return False

    async def _fetch_token(self):
        """Busca token OAuth do Azure AD"""
        session = await self._ensure_session()

        token_url = f"{self.config.authority}/{self.config.tenant_id}/oauth2/token"

        data = {
            "grant_type": "client_credentials",
            "client_id": self.config.client_id,
            "client_secret": self.config.client_secret,
            "resource": self.config.resource
        }

        async with session.post(token_url, data=data) as response:
            if response.status != 200:
                error = await response.text()
                raise Exception(f"Falha ao obter token: {error}")

            token_data = await response.json()
            self._access_token = token_data.get("access_token")
            expires_in = int(token_data.get("expires_in", 3600))
            self._token_expires_at = datetime.utcnow() + timedelta(seconds=expires_in)

    async def _get_headers(self) -> Dict[str, str]:
        """Retorna headers com token valido"""
        if not self.is_token_valid:
            await self._fetch_token()

        return {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json"
        }

    def _get_base_url(self) -> str:
        """Retorna URL base (com ou sem workspace)"""
        if self.config.workspace_id:
            return f"{self.config.api_url}/groups/{self.config.workspace_id}"
        return self.config.api_url

    async def disconnect(self):
        """Desconecta"""
        if self._session and not self._session.closed:
            await self._session.close()
        self._session = None
        self._access_token = None

    # ====================
    # Datasets
    # ====================

    async def list_datasets(self) -> List[Dict]:
        """Lista datasets do workspace"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("value", [])
            return []

    async def get_dataset(self, dataset_id: str) -> Optional[Dict]:
        """Busca dataset por ID"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                return await response.json()
            return None

    async def create_dataset(self, dataset: PowerBIDataset) -> Optional[str]:
        """
        Cria novo dataset.

        Args:
            dataset: Definicao do dataset

        Returns:
            ID do dataset criado ou None
        """
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets?defaultRetentionPolicy=basicFIFO"

        async with session.post(url, headers=headers, json=dataset.to_dict()) as response:
            if response.status in (200, 201):
                data = await response.json()
                dataset_id = data.get("id")
                logger.info(f"Dataset criado: {dataset_id}")
                return dataset_id
            else:
                error = await response.text()
                logger.error(f"Erro ao criar dataset: {error}")
                return None

    async def delete_dataset(self, dataset_id: str) -> bool:
        """Deleta dataset"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}"

        async with session.delete(url, headers=headers) as response:
            return response.status == 200

    async def refresh_dataset(self, dataset_id: str) -> bool:
        """
        Dispara refresh do dataset.

        Args:
            dataset_id: ID do dataset

        Returns:
            True se refresh iniciado
        """
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}/refreshes"

        async with session.post(url, headers=headers) as response:
            if response.status == 202:
                logger.info(f"Refresh iniciado para dataset {dataset_id}")
                return True
            else:
                error = await response.text()
                logger.error(f"Erro ao iniciar refresh: {error}")
                return False

    async def get_refresh_history(self, dataset_id: str, top: int = 10) -> List[Dict]:
        """Retorna historico de refresh"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}/refreshes?$top={top}"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("value", [])
            return []

    # ====================
    # Tables & Rows
    # ====================

    async def get_tables(self, dataset_id: str) -> List[Dict]:
        """Lista tabelas do dataset"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}/tables"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("value", [])
            return []

    async def push_rows(
        self,
        dataset_id: str,
        table_name: str,
        rows: List[Dict]
    ) -> bool:
        """
        Push de linhas para tabela.

        Args:
            dataset_id: ID do dataset
            table_name: Nome da tabela
            rows: Lista de dicionarios com dados

        Returns:
            True se enviado com sucesso
        """
        if not rows:
            return True

        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}/tables/{table_name}/rows"

        # Power BI aceita max 10000 rows por request
        batch_size = 10000
        success = True

        for i in range(0, len(rows), batch_size):
            batch = rows[i:i + batch_size]
            payload = {"rows": batch}

            async with session.post(url, headers=headers, json=payload) as response:
                if response.status != 200:
                    error = await response.text()
                    logger.error(f"Erro ao push rows: {error}")
                    success = False
                else:
                    logger.debug(f"Push {len(batch)} rows para {table_name}")

        return success

    async def delete_rows(self, dataset_id: str, table_name: str) -> bool:
        """Deleta todas as linhas da tabela"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}/tables/{table_name}/rows"

        async with session.delete(url, headers=headers) as response:
            return response.status == 200

    async def update_table_schema(
        self,
        dataset_id: str,
        table: PowerBITable
    ) -> bool:
        """Atualiza schema da tabela"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets/{dataset_id}/tables/{table.name}"

        async with session.put(url, headers=headers, json=table.to_dict()) as response:
            return response.status == 200

    # ====================
    # Reports & Dashboards
    # ====================

    async def list_reports(self) -> List[Dict]:
        """Lista reports do workspace"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/reports"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("value", [])
            return []

    async def get_report(self, report_id: str) -> Optional[Dict]:
        """Busca report por ID"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/reports/{report_id}"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                return await response.json()
            return None

    async def get_embed_token(
        self,
        report_id: str,
        access_level: str = "View"
    ) -> Optional[Dict]:
        """
        Gera token de embed para report.

        Args:
            report_id: ID do report
            access_level: View, Edit, Create

        Returns:
            Dict com token e URL de embed
        """
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/reports/{report_id}/GenerateToken"
        payload = {"accessLevel": access_level}

        async with session.post(url, headers=headers, json=payload) as response:
            if response.status == 200:
                return await response.json()
            return None

    async def list_dashboards(self) -> List[Dict]:
        """Lista dashboards do workspace"""
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/dashboards"

        async with session.get(url, headers=headers) as response:
            if response.status == 200:
                data = await response.json()
                return data.get("value", [])
            return []

    # ====================
    # Streaming Data
    # ====================

    async def create_streaming_dataset(
        self,
        name: str,
        columns: List[PowerBIColumn],
        push_enabled: bool = True,
        historic_enabled: bool = True
    ) -> Optional[str]:
        """
        Cria dataset de streaming em tempo real.

        Args:
            name: Nome do dataset
            columns: Definicao das colunas
            push_enabled: Permitir push via API
            historic_enabled: Manter historico

        Returns:
            ID do dataset ou None
        """
        session = await self._ensure_session()
        headers = await self._get_headers()

        url = f"{self._get_base_url()}/datasets"

        payload = {
            "name": name,
            "defaultMode": "Streaming",
            "tables": [{
                "name": name,
                "columns": [col.to_dict() for col in columns]
            }]
        }

        async with session.post(url, headers=headers, json=payload) as response:
            if response.status in (200, 201):
                data = await response.json()
                return data.get("id")
            return None

    async def push_streaming_data(
        self,
        push_url: str,
        rows: List[Dict]
    ) -> bool:
        """
        Push para dataset de streaming.

        Args:
            push_url: URL de push do dataset
            rows: Dados para enviar

        Returns:
            True se sucesso
        """
        session = await self._ensure_session()

        async with session.post(push_url, json=rows) as response:
            return response.status == 200

    # ====================
    # Factory Agents Sync
    # ====================

    async def sync_stories(
        self,
        stories: List[Dict],
        dataset_name: str = "FactoryAgentsStories"
    ) -> bool:
        """
        Sincroniza stories da fabrica de agentes para Power BI.

        Args:
            stories: Lista de stories
            dataset_name: Nome do dataset

        Returns:
            True se sincronizado
        """
        # Verificar se dataset existe
        datasets = await self.list_datasets()
        dataset_id = None

        for ds in datasets:
            if ds.get("name") == dataset_name:
                dataset_id = ds.get("id")
                break

        # Criar dataset se nao existe
        if not dataset_id:
            dataset = PowerBIDataset(
                name=dataset_name,
                tables=[
                    PowerBITable(
                        name="Stories",
                        columns=[
                            PowerBIColumn("story_id", PowerBIDataType.STRING),
                            PowerBIColumn("title", PowerBIDataType.STRING),
                            PowerBIColumn("status", PowerBIDataType.STRING),
                            PowerBIColumn("priority", PowerBIDataType.STRING),
                            PowerBIColumn("story_points", PowerBIDataType.INT64),
                            PowerBIColumn("assignee", PowerBIDataType.STRING),
                            PowerBIColumn("sprint_id", PowerBIDataType.STRING),
                            PowerBIColumn("epic_id", PowerBIDataType.STRING),
                            PowerBIColumn("created_at", PowerBIDataType.DATETIME),
                            PowerBIColumn("updated_at", PowerBIDataType.DATETIME),
                            PowerBIColumn("completed_at", PowerBIDataType.DATETIME),
                            PowerBIColumn("tenant_id", PowerBIDataType.STRING)
                        ]
                    )
                ]
            )
            dataset_id = await self.create_dataset(dataset)

            if not dataset_id:
                return False

        # Preparar dados
        rows = []
        for story in stories:
            rows.append({
                "story_id": story.get("story_id", ""),
                "title": story.get("title", "")[:255],
                "status": story.get("status", ""),
                "priority": story.get("priority", ""),
                "story_points": story.get("story_points", 0),
                "assignee": story.get("assignee", ""),
                "sprint_id": story.get("sprint_id", ""),
                "epic_id": story.get("epic_id", ""),
                "created_at": story.get("created_at"),
                "updated_at": story.get("updated_at"),
                "completed_at": story.get("completed_at"),
                "tenant_id": self.tenant_id
            })

        # Limpar dados antigos e fazer push
        await self.delete_rows(dataset_id, "Stories")
        return await self.push_rows(dataset_id, "Stories", rows)


# Singleton
_powerbi_instance: Optional[PowerBIConnector] = None


def get_powerbi_connector(tenant_id: str = "") -> PowerBIConnector:
    """Retorna instancia global"""
    global _powerbi_instance
    if _powerbi_instance is None:
        _powerbi_instance = PowerBIConnector(tenant_id=tenant_id)
    return _powerbi_instance

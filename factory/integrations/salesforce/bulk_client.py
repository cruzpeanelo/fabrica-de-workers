# -*- coding: utf-8 -*-
"""
Salesforce Bulk API 2.0 Client
==============================
Cliente para operacoes em massa no Salesforce.

A Bulk API 2.0 e otimizada para:
- Insert, update, upsert e delete de grandes volumes
- Consultas SOQL em datasets grandes
- Processamento assincrono com monitoramento

Suporta isolamento multi-tenant atraves do tenant_id do SalesforceClient (Issue #314).

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient, SalesforceConfig
    from factory.integrations.salesforce.bulk_client import BulkClient

    config = SalesforceConfig(
        tenant_id="TENANT-001",
        username="user@empresa.com",
        password="senha123",
        security_token="token"
    )
    sf_client = SalesforceClient(config)
    await sf_client.connect()

    bulk = BulkClient(sf_client)

    # Insert em massa
    accounts = [
        {"Name": "Empresa 1", "Industry": "Technology"},
        {"Name": "Empresa 2", "Industry": "Finance"},
        # ... milhares de registros
    ]
    result = await bulk.insert("Account", accounts)

    # Query em massa
    async for batch in bulk.query_all("SELECT Id, Name FROM Account"):
        for record in batch:
            print(record["Name"])
"""

import asyncio
import csv
import io
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, AsyncGenerator, Dict, List, Optional, Union, TYPE_CHECKING

if TYPE_CHECKING:
    from .client import SalesforceClient

logger = logging.getLogger(__name__)


class BulkOperation(str, Enum):
    """Tipo de operacao bulk"""
    INSERT = "insert"
    UPDATE = "update"
    UPSERT = "upsert"
    DELETE = "delete"
    HARD_DELETE = "hardDelete"


class BulkJobState(str, Enum):
    """Estado do job bulk"""
    OPEN = "Open"
    UPLOAD_COMPLETE = "UploadComplete"
    IN_PROGRESS = "InProgress"
    JOB_COMPLETE = "JobComplete"
    ABORTED = "Aborted"
    FAILED = "Failed"


class ContentType(str, Enum):
    """Tipo de conteudo para dados"""
    CSV = "CSV"
    JSON = "JSON"


@dataclass
class BulkJobInfo:
    """Informacoes de um job bulk"""
    id: str
    operation: str
    object: str
    state: str
    created_by_id: str
    created_date: Optional[datetime] = None
    system_modstamp: Optional[datetime] = None
    content_type: str = "CSV"
    api_version: str = "59.0"
    line_ending: str = "LF"
    column_delimiter: str = "COMMA"
    concurrency_mode: str = "Parallel"
    external_id_field: Optional[str] = None
    number_records_processed: int = 0
    number_records_failed: int = 0
    retries: int = 0
    total_processing_time: int = 0
    api_active_processing_time: int = 0
    apex_processing_time: int = 0
    job_type: str = "V2Ingest"

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "BulkJobInfo":
        return cls(
            id=data.get("id", ""),
            operation=data.get("operation", ""),
            object=data.get("object", ""),
            state=data.get("state", ""),
            created_by_id=data.get("createdById", ""),
            content_type=data.get("contentType", "CSV"),
            api_version=str(data.get("apiVersion", "59.0")),
            external_id_field=data.get("externalIdFieldName"),
            number_records_processed=data.get("numberRecordsProcessed", 0),
            number_records_failed=data.get("numberRecordsFailed", 0),
            retries=data.get("retries", 0),
            total_processing_time=data.get("totalProcessingTime", 0)
        )


@dataclass
class BulkResult:
    """Resultado de uma operacao bulk"""
    job_id: str
    success: bool
    records_processed: int
    records_failed: int
    successful_results: List[Dict[str, Any]] = field(default_factory=list)
    failed_results: List[Dict[str, Any]] = field(default_factory=list)
    unprocessed_records: List[Dict[str, Any]] = field(default_factory=list)
    processing_time_ms: int = 0


class BulkClient:
    """
    Cliente para Bulk API 2.0 do Salesforce

    Permite operacoes em massa com alta performance
    para grandes volumes de dados.

    Herda o contexto de tenant do SalesforceClient para isolamento multi-tenant.
    """

    def __init__(self, sf_client: "SalesforceClient", batch_size: int = 10000):
        """
        Inicializa o cliente Bulk

        Args:
            sf_client: SalesforceClient autenticado (deve ter tenant_id configurado)
            batch_size: Tamanho maximo de cada batch
        """
        self.sf = sf_client
        self.batch_size = batch_size
        self._max_csv_file_size = 100 * 1024 * 1024  # 100MB

    @property
    def tenant_id(self) -> str:
        """ID do tenant para isolamento (herdado do SalesforceClient)"""
        return self.sf.tenant_id

    @property
    def bulk_url(self) -> str:
        """URL base da Bulk API"""
        return self.sf.config.bulk_url

    def _get_headers(self, content_type: str = "application/json") -> Dict[str, str]:
        """Headers para requisicoes bulk"""
        return {
            "Authorization": f"Bearer {self.sf.config.access_token}",
            "Content-Type": content_type,
            "Accept": "application/json"
        }

    async def _request(
        self,
        method: str,
        url: str,
        data: Optional[Any] = None,
        headers: Optional[Dict] = None,
        is_json: bool = True
    ) -> Any:
        """Faz requisicao para Bulk API"""
        if headers is None:
            headers = self._get_headers()

        session = await self.sf._get_session()

        if is_json and data:
            data = json.dumps(data)

        async with session.request(
            method,
            url,
            data=data,
            headers=headers
        ) as response:
            response_text = await response.text()

            if response.status >= 400:
                try:
                    error = json.loads(response_text)
                    error_msg = error.get("message", response_text)
                except json.JSONDecodeError:
                    error_msg = response_text
                raise Exception(f"Bulk API error: {error_msg}")

            if not response_text:
                return None

            try:
                return json.loads(response_text)
            except json.JSONDecodeError:
                return response_text

    # ==================== JOB MANAGEMENT ====================

    async def create_job(
        self,
        sobject: str,
        operation: Union[str, BulkOperation],
        external_id_field: Optional[str] = None,
        content_type: ContentType = ContentType.CSV,
        line_ending: str = "LF",
        column_delimiter: str = "COMMA"
    ) -> BulkJobInfo:
        """
        Cria um novo job bulk

        Args:
            sobject: Nome do objeto Salesforce
            operation: Tipo de operacao (insert, update, etc.)
            external_id_field: Campo para upsert
            content_type: Formato dos dados (CSV ou JSON)
            line_ending: Tipo de quebra de linha
            column_delimiter: Delimitador de colunas

        Returns:
            BulkJobInfo com detalhes do job
        """
        if isinstance(operation, BulkOperation):
            operation = operation.value

        data = {
            "object": sobject,
            "operation": operation,
            "contentType": content_type.value,
            "lineEnding": line_ending,
            "columnDelimiter": column_delimiter
        }

        if external_id_field and operation == "upsert":
            data["externalIdFieldName"] = external_id_field

        result = await self._request("POST", self.bulk_url, data=data)
        return BulkJobInfo.from_dict(result)

    async def get_job(self, job_id: str) -> BulkJobInfo:
        """
        Obtem informacoes de um job

        Args:
            job_id: ID do job

        Returns:
            BulkJobInfo atualizado
        """
        url = f"{self.bulk_url}/{job_id}"
        result = await self._request("GET", url)
        return BulkJobInfo.from_dict(result)

    async def close_job(self, job_id: str) -> BulkJobInfo:
        """
        Fecha um job para iniciar processamento

        Args:
            job_id: ID do job

        Returns:
            BulkJobInfo atualizado
        """
        url = f"{self.bulk_url}/{job_id}"
        data = {"state": "UploadComplete"}
        result = await self._request("PATCH", url, data=data)
        return BulkJobInfo.from_dict(result)

    async def abort_job(self, job_id: str) -> BulkJobInfo:
        """
        Aborta um job em execucao

        Args:
            job_id: ID do job

        Returns:
            BulkJobInfo atualizado
        """
        url = f"{self.bulk_url}/{job_id}"
        data = {"state": "Aborted"}
        result = await self._request("PATCH", url, data=data)
        return BulkJobInfo.from_dict(result)

    async def delete_job(self, job_id: str) -> bool:
        """
        Deleta um job

        Args:
            job_id: ID do job

        Returns:
            True se deletado
        """
        url = f"{self.bulk_url}/{job_id}"
        await self._request("DELETE", url)
        return True

    async def list_jobs(
        self,
        is_pk_chunking_enabled: Optional[bool] = None,
        job_type: str = "V2Ingest",
        query_locator: Optional[str] = None
    ) -> List[BulkJobInfo]:
        """
        Lista jobs bulk

        Args:
            is_pk_chunking_enabled: Filtrar por PK chunking
            job_type: Tipo de job
            query_locator: Locator para paginacao

        Returns:
            Lista de BulkJobInfo
        """
        params = {"jobType": job_type}
        if is_pk_chunking_enabled is not None:
            params["isPkChunkingEnabled"] = str(is_pk_chunking_enabled).lower()
        if query_locator:
            params["queryLocator"] = query_locator

        url = f"{self.bulk_url}?" + "&".join(f"{k}={v}" for k, v in params.items())
        result = await self._request("GET", url)

        jobs = []
        for record in result.get("records", []):
            jobs.append(BulkJobInfo.from_dict(record))

        return jobs

    # ==================== DATA UPLOAD ====================

    async def upload_data(
        self,
        job_id: str,
        records: List[Dict[str, Any]],
        content_type: ContentType = ContentType.CSV
    ) -> bool:
        """
        Faz upload de dados para um job

        Args:
            job_id: ID do job
            records: Lista de registros
            content_type: Formato dos dados

        Returns:
            True se upload bem sucedido
        """
        url = f"{self.bulk_url}/{job_id}/batches"

        if content_type == ContentType.CSV:
            data = self._records_to_csv(records)
            headers = self._get_headers("text/csv")
        else:
            data = json.dumps(records)
            headers = self._get_headers("application/json")

        await self._request("PUT", url, data=data, headers=headers, is_json=False)
        return True

    def _records_to_csv(self, records: List[Dict[str, Any]]) -> str:
        """Converte lista de dicts para CSV"""
        if not records:
            return ""

        output = io.StringIO()
        fieldnames = list(records[0].keys())

        writer = csv.DictWriter(output, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(records)

        return output.getvalue()

    # ==================== RESULTS ====================

    async def get_successful_results(
        self,
        job_id: str
    ) -> List[Dict[str, Any]]:
        """
        Obtem resultados de sucesso

        Args:
            job_id: ID do job

        Returns:
            Lista de registros processados com sucesso
        """
        url = f"{self.bulk_url}/{job_id}/successfulResults"
        headers = self._get_headers("text/csv")
        headers["Accept"] = "text/csv"

        result = await self._request("GET", url, headers=headers)
        return self._csv_to_records(result)

    async def get_failed_results(
        self,
        job_id: str
    ) -> List[Dict[str, Any]]:
        """
        Obtem resultados de falha

        Args:
            job_id: ID do job

        Returns:
            Lista de registros que falharam com mensagens de erro
        """
        url = f"{self.bulk_url}/{job_id}/failedResults"
        headers = self._get_headers("text/csv")
        headers["Accept"] = "text/csv"

        result = await self._request("GET", url, headers=headers)
        return self._csv_to_records(result)

    async def get_unprocessed_records(
        self,
        job_id: str
    ) -> List[Dict[str, Any]]:
        """
        Obtem registros nao processados

        Args:
            job_id: ID do job

        Returns:
            Lista de registros nao processados
        """
        url = f"{self.bulk_url}/{job_id}/unprocessedrecords"
        headers = self._get_headers("text/csv")
        headers["Accept"] = "text/csv"

        result = await self._request("GET", url, headers=headers)
        return self._csv_to_records(result)

    def _csv_to_records(self, csv_text: str) -> List[Dict[str, Any]]:
        """Converte CSV para lista de dicts"""
        if not csv_text:
            return []

        reader = csv.DictReader(io.StringIO(csv_text))
        return list(reader)

    # ==================== HIGH-LEVEL OPERATIONS ====================

    async def insert(
        self,
        sobject: str,
        records: List[Dict[str, Any]],
        wait: bool = True,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> BulkResult:
        """
        Insert em massa

        Args:
            sobject: Nome do objeto
            records: Lista de registros para inserir
            wait: Aguardar conclusao do job
            poll_interval: Intervalo de polling
            timeout: Timeout maximo

        Returns:
            BulkResult com detalhes
        """
        return await self._execute_operation(
            sobject,
            BulkOperation.INSERT,
            records,
            wait=wait,
            poll_interval=poll_interval,
            timeout=timeout
        )

    async def update(
        self,
        sobject: str,
        records: List[Dict[str, Any]],
        wait: bool = True,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> BulkResult:
        """
        Update em massa

        Args:
            sobject: Nome do objeto
            records: Lista de registros (devem conter Id)
            wait: Aguardar conclusao
            poll_interval: Intervalo de polling
            timeout: Timeout maximo

        Returns:
            BulkResult com detalhes
        """
        return await self._execute_operation(
            sobject,
            BulkOperation.UPDATE,
            records,
            wait=wait,
            poll_interval=poll_interval,
            timeout=timeout
        )

    async def upsert(
        self,
        sobject: str,
        records: List[Dict[str, Any]],
        external_id_field: str,
        wait: bool = True,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> BulkResult:
        """
        Upsert em massa

        Args:
            sobject: Nome do objeto
            records: Lista de registros
            external_id_field: Campo external ID
            wait: Aguardar conclusao
            poll_interval: Intervalo de polling
            timeout: Timeout maximo

        Returns:
            BulkResult com detalhes
        """
        return await self._execute_operation(
            sobject,
            BulkOperation.UPSERT,
            records,
            external_id_field=external_id_field,
            wait=wait,
            poll_interval=poll_interval,
            timeout=timeout
        )

    async def delete(
        self,
        sobject: str,
        record_ids: List[str],
        hard_delete: bool = False,
        wait: bool = True,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> BulkResult:
        """
        Delete em massa

        Args:
            sobject: Nome do objeto
            record_ids: Lista de IDs para deletar
            hard_delete: Hard delete (sem lixeira)
            wait: Aguardar conclusao
            poll_interval: Intervalo de polling
            timeout: Timeout maximo

        Returns:
            BulkResult com detalhes
        """
        operation = BulkOperation.HARD_DELETE if hard_delete else BulkOperation.DELETE
        records = [{"Id": id} for id in record_ids]

        return await self._execute_operation(
            sobject,
            operation,
            records,
            wait=wait,
            poll_interval=poll_interval,
            timeout=timeout
        )

    async def _execute_operation(
        self,
        sobject: str,
        operation: BulkOperation,
        records: List[Dict[str, Any]],
        external_id_field: Optional[str] = None,
        wait: bool = True,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> BulkResult:
        """Executa operacao bulk generica"""
        # Criar job
        job = await self.create_job(
            sobject,
            operation,
            external_id_field=external_id_field
        )
        logger.info(f"Job criado: {job.id}")

        try:
            # Upload em batches
            for i in range(0, len(records), self.batch_size):
                batch = records[i:i + self.batch_size]
                await self.upload_data(job.id, batch)
                logger.debug(f"Batch {i // self.batch_size + 1} enviado")

            # Fechar job
            await self.close_job(job.id)
            logger.info(f"Job fechado, iniciando processamento")

            if not wait:
                return BulkResult(
                    job_id=job.id,
                    success=True,
                    records_processed=0,
                    records_failed=0
                )

            # Aguardar conclusao
            job = await self._wait_for_job(job.id, poll_interval, timeout)

            # Coletar resultados
            successful = await self.get_successful_results(job.id)
            failed = await self.get_failed_results(job.id)
            unprocessed = await self.get_unprocessed_records(job.id)

            return BulkResult(
                job_id=job.id,
                success=job.number_records_failed == 0,
                records_processed=job.number_records_processed,
                records_failed=job.number_records_failed,
                successful_results=successful,
                failed_results=failed,
                unprocessed_records=unprocessed,
                processing_time_ms=job.total_processing_time
            )

        except Exception as e:
            logger.error(f"Erro na operacao bulk: {e}")
            try:
                await self.abort_job(job.id)
            except Exception:
                pass
            raise

    async def _wait_for_job(
        self,
        job_id: str,
        poll_interval: int,
        timeout: int
    ) -> BulkJobInfo:
        """Aguarda conclusao de um job"""
        start_time = asyncio.get_event_loop().time()

        while True:
            job = await self.get_job(job_id)

            if job.state in (
                BulkJobState.JOB_COMPLETE.value,
                BulkJobState.FAILED.value,
                BulkJobState.ABORTED.value
            ):
                return job

            elapsed = asyncio.get_event_loop().time() - start_time
            if elapsed > timeout:
                raise TimeoutError(f"Job timeout apos {timeout}s")

            logger.debug(f"Job status: {job.state}, processados: {job.number_records_processed}")
            await asyncio.sleep(poll_interval)

    # ==================== BULK QUERY ====================

    async def create_query_job(
        self,
        soql: str,
        content_type: ContentType = ContentType.CSV,
        column_delimiter: str = "COMMA",
        line_ending: str = "LF"
    ) -> str:
        """
        Cria job de query bulk

        Args:
            soql: Query SOQL
            content_type: Formato do resultado
            column_delimiter: Delimitador de colunas
            line_ending: Tipo de quebra de linha

        Returns:
            ID do job
        """
        url = f"{self.sf.config.instance_url}/services/data/v{self.sf.config.api_version}/jobs/query"

        data = {
            "operation": "query",
            "query": soql,
            "contentType": content_type.value,
            "columnDelimiter": column_delimiter,
            "lineEnding": line_ending
        }

        result = await self._request("POST", url, data=data)
        return result.get("id")

    async def get_query_job(self, job_id: str) -> Dict[str, Any]:
        """
        Obtem status de job de query

        Args:
            job_id: ID do job

        Returns:
            Informacoes do job
        """
        url = f"{self.sf.config.instance_url}/services/data/v{self.sf.config.api_version}/jobs/query/{job_id}"
        return await self._request("GET", url)

    async def get_query_results(
        self,
        job_id: str,
        locator: Optional[str] = None,
        max_records: int = 50000
    ) -> tuple:
        """
        Obtem resultados de query bulk

        Args:
            job_id: ID do job
            locator: Locator para paginacao
            max_records: Maximo de registros

        Returns:
            Tuple (records, next_locator)
        """
        url = f"{self.sf.config.instance_url}/services/data/v{self.sf.config.api_version}/jobs/query/{job_id}/results"

        params = {"maxRecords": max_records}
        if locator:
            params["locator"] = locator

        url += "?" + "&".join(f"{k}={v}" for k, v in params.items())

        headers = self._get_headers("text/csv")
        headers["Accept"] = "text/csv"

        session = await self.sf._get_session()
        async with session.get(url, headers=headers) as response:
            next_locator = response.headers.get("Sforce-Locator")
            csv_text = await response.text()

            records = self._csv_to_records(csv_text)
            return records, next_locator if next_locator != "null" else None

    async def query(
        self,
        soql: str,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> List[Dict[str, Any]]:
        """
        Executa query bulk e retorna todos os resultados

        Args:
            soql: Query SOQL
            poll_interval: Intervalo de polling
            timeout: Timeout maximo

        Returns:
            Lista com todos os registros
        """
        all_records = []

        async for batch in self.query_iter(soql, poll_interval, timeout):
            all_records.extend(batch)

        return all_records

    async def query_iter(
        self,
        soql: str,
        poll_interval: int = 5,
        timeout: int = 600
    ) -> AsyncGenerator[List[Dict[str, Any]], None]:
        """
        Executa query bulk com iterator

        Args:
            soql: Query SOQL
            poll_interval: Intervalo de polling
            timeout: Timeout maximo

        Yields:
            Batches de registros
        """
        # Criar job de query
        job_id = await self.create_query_job(soql)
        logger.info(f"Query job criado: {job_id}")

        # Aguardar conclusao
        start_time = asyncio.get_event_loop().time()

        while True:
            job = await self.get_query_job(job_id)
            state = job.get("state")

            if state == "JobComplete":
                break
            elif state in ("Failed", "Aborted"):
                raise Exception(f"Query job falhou: {job.get('errorMessage')}")

            elapsed = asyncio.get_event_loop().time() - start_time
            if elapsed > timeout:
                raise TimeoutError(f"Query timeout apos {timeout}s")

            await asyncio.sleep(poll_interval)

        # Buscar resultados paginados
        locator = None
        while True:
            records, locator = await self.get_query_results(job_id, locator)

            if records:
                yield records

            if not locator:
                break

        logger.info(f"Query concluida: {job.get('numberOfRecordsProcessed')} registros")

    # ==================== UTILITIES ====================

    async def count_records(self, sobject: str, where: Optional[str] = None) -> int:
        """
        Conta registros de um objeto via bulk query

        Args:
            sobject: Nome do objeto
            where: Clausula WHERE opcional

        Returns:
            Numero de registros
        """
        soql = f"SELECT COUNT() FROM {sobject}"
        if where:
            soql += f" WHERE {where}"

        job_id = await self.create_query_job(soql)

        # Aguardar
        while True:
            job = await self.get_query_job(job_id)
            if job.get("state") == "JobComplete":
                return job.get("numberOfRecordsProcessed", 0)
            elif job.get("state") in ("Failed", "Aborted"):
                raise Exception(f"Query falhou: {job.get('errorMessage')}")
            await asyncio.sleep(2)

    async def export_object(
        self,
        sobject: str,
        fields: Optional[List[str]] = None,
        where: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Exporta todos os registros de um objeto

        Args:
            sobject: Nome do objeto
            fields: Lista de campos (padrao: todos)
            where: Clausula WHERE opcional

        Returns:
            Lista com todos os registros
        """
        if fields:
            field_list = ", ".join(fields)
        else:
            # Buscar todos os campos
            describe = await self.sf.describe(sobject)
            field_list = ", ".join(f["name"] for f in describe.fields if f.get("type") != "address")

        soql = f"SELECT {field_list} FROM {sobject}"
        if where:
            soql += f" WHERE {where}"

        return await self.query(soql)

# -*- coding: utf-8 -*-
"""
Salesforce Tooling API Client
=============================
Cliente para Tooling API do Salesforce.

A Tooling API permite:
- Ler e modificar codigo Apex
- Compilar classes e triggers
- Executar testes Apex
- Analisar coverage de codigo
- Gerenciar debug logs
- Executar codigo Apex anonimo

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient
    from factory.integrations.salesforce.tooling_client import ToolingClient

    sf_client = SalesforceClient(config)
    await sf_client.connect()

    tooling = ToolingClient(sf_client)

    # Listar classes Apex
    classes = await tooling.list_apex_classes()

    # Ler codigo de uma classe
    code = await tooling.get_apex_class_body("MinhaClasse")

    # Executar codigo anonimo
    result = await tooling.execute_anonymous("System.debug('Ola mundo!');")

    # Executar testes
    test_result = await tooling.run_tests(["MinhaClasseTest"])
"""

import asyncio
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)


class ApexLogLevel(str, Enum):
    """Niveis de log Apex"""
    NONE = "NONE"
    ERROR = "ERROR"
    WARN = "WARN"
    INFO = "INFO"
    DEBUG = "DEBUG"
    FINE = "FINE"
    FINER = "FINER"
    FINEST = "FINEST"


@dataclass
class ApexExecutionResult:
    """Resultado de execucao de Apex"""
    success: bool
    compiled: bool
    compile_problem: Optional[str] = None
    exception_message: Optional[str] = None
    exception_stack_trace: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None
    logs: Optional[str] = None


@dataclass
class ApexTestResult:
    """Resultado de testes Apex"""
    success: bool
    num_tests_run: int
    num_failures: int
    total_time: float
    test_failures: List[Dict[str, Any]] = field(default_factory=list)
    test_successes: List[Dict[str, Any]] = field(default_factory=list)
    code_coverage: List[Dict[str, Any]] = field(default_factory=list)
    code_coverage_warnings: List[str] = field(default_factory=list)


@dataclass
class ApexClass:
    """Representacao de uma classe Apex"""
    id: str
    name: str
    body: str
    api_version: str
    status: str
    is_valid: bool
    length_without_comments: int
    namespace_prefix: Optional[str] = None
    created_date: Optional[datetime] = None
    last_modified_date: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApexClass":
        return cls(
            id=data.get("Id", ""),
            name=data.get("Name", ""),
            body=data.get("Body", ""),
            api_version=str(data.get("ApiVersion", "")),
            status=data.get("Status", ""),
            is_valid=data.get("IsValid", False),
            length_without_comments=data.get("LengthWithoutComments", 0),
            namespace_prefix=data.get("NamespacePrefix")
        )


@dataclass
class ApexTrigger:
    """Representacao de um trigger Apex"""
    id: str
    name: str
    body: str
    table_enum_or_id: str
    api_version: str
    status: str
    is_valid: bool
    usage_before_insert: bool = False
    usage_after_insert: bool = False
    usage_before_update: bool = False
    usage_after_update: bool = False
    usage_before_delete: bool = False
    usage_after_delete: bool = False
    usage_after_undelete: bool = False

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ApexTrigger":
        return cls(
            id=data.get("Id", ""),
            name=data.get("Name", ""),
            body=data.get("Body", ""),
            table_enum_or_id=data.get("TableEnumOrId", ""),
            api_version=str(data.get("ApiVersion", "")),
            status=data.get("Status", ""),
            is_valid=data.get("IsValid", False),
            usage_before_insert=data.get("UsageBeforeInsert", False),
            usage_after_insert=data.get("UsageAfterInsert", False),
            usage_before_update=data.get("UsageBeforeUpdate", False),
            usage_after_update=data.get("UsageAfterUpdate", False),
            usage_before_delete=data.get("UsageBeforeDelete", False),
            usage_after_delete=data.get("UsageAfterDelete", False),
            usage_after_undelete=data.get("UsageAfterUndelete", False)
        )


class ToolingClient:
    """
    Cliente para Tooling API do Salesforce

    Fornece acesso a recursos de desenvolvimento como
    classes Apex, triggers, logs e execucao de testes.
    """

    def __init__(self, sf_client):
        """
        Inicializa o cliente Tooling

        Args:
            sf_client: SalesforceClient autenticado
        """
        self.sf = sf_client

    @property
    def tooling_url(self) -> str:
        """URL da Tooling API"""
        return self.sf.config.tooling_url

    async def _request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None
    ) -> Any:
        """
        Faz requisicao para Tooling API

        Args:
            method: Metodo HTTP
            endpoint: Endpoint relativo
            data: Dados do body
            params: Query params

        Returns:
            Resposta JSON
        """
        url = f"{self.tooling_url}{endpoint}"

        session = await self.sf._get_session()

        async with session.request(
            method,
            url,
            json=data,
            params=params,
            headers=self.sf.headers
        ) as response:
            response_text = await response.text()

            if response.status >= 400:
                self.sf._handle_error_response(response.status, response_text)

            if not response_text:
                return None

            return json.loads(response_text)

    async def query(self, soql: str) -> List[Dict[str, Any]]:
        """
        Executa query SOQL na Tooling API

        Args:
            soql: Query SOQL

        Returns:
            Lista de registros
        """
        params = {"q": soql}
        result = await self._request("GET", "/query", params=params)
        return result.get("records", [])

    # ==================== APEX CLASSES ====================

    async def list_apex_classes(
        self,
        name_filter: Optional[str] = None,
        namespace: Optional[str] = None
    ) -> List[ApexClass]:
        """
        Lista classes Apex

        Args:
            name_filter: Filtro por nome (suporta LIKE)
            namespace: Filtrar por namespace

        Returns:
            Lista de ApexClass
        """
        soql = """
            SELECT Id, Name, Body, ApiVersion, Status, IsValid,
                   LengthWithoutComments, NamespacePrefix,
                   CreatedDate, LastModifiedDate
            FROM ApexClass
        """

        conditions = []
        if name_filter:
            conditions.append(f"Name LIKE '{name_filter}'")
        if namespace:
            conditions.append(f"NamespacePrefix = '{namespace}'")
        elif namespace is None:
            conditions.append("NamespacePrefix = null")

        if conditions:
            soql += " WHERE " + " AND ".join(conditions)

        soql += " ORDER BY Name"

        records = await self.query(soql)
        return [ApexClass.from_dict(r) for r in records]

    async def get_apex_class(self, class_id: str) -> ApexClass:
        """
        Obtem uma classe Apex por ID

        Args:
            class_id: ID da classe

        Returns:
            ApexClass
        """
        endpoint = f"/sobjects/ApexClass/{class_id}"
        data = await self._request("GET", endpoint)
        return ApexClass.from_dict(data)

    async def get_apex_class_by_name(self, name: str) -> Optional[ApexClass]:
        """
        Obtem uma classe Apex por nome

        Args:
            name: Nome da classe

        Returns:
            ApexClass ou None se nao encontrada
        """
        soql = f"""
            SELECT Id, Name, Body, ApiVersion, Status, IsValid,
                   LengthWithoutComments, NamespacePrefix
            FROM ApexClass
            WHERE Name = '{name}' AND NamespacePrefix = null
        """
        records = await self.query(soql)
        if records:
            return ApexClass.from_dict(records[0])
        return None

    async def get_apex_class_body(self, name: str) -> Optional[str]:
        """
        Obtem apenas o corpo de uma classe

        Args:
            name: Nome da classe

        Returns:
            Codigo da classe ou None
        """
        apex_class = await self.get_apex_class_by_name(name)
        return apex_class.body if apex_class else None

    async def create_apex_class(
        self,
        name: str,
        body: str,
        api_version: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria nova classe Apex

        Args:
            name: Nome da classe
            body: Codigo Apex
            api_version: Versao da API

        Returns:
            Resultado da criacao
        """
        data = {
            "Name": name,
            "Body": body,
            "ApiVersion": api_version or self.sf.config.api_version
        }

        return await self._request("POST", "/sobjects/ApexClass", data=data)

    async def update_apex_class(
        self,
        class_id: str,
        body: str,
        api_version: Optional[str] = None
    ) -> bool:
        """
        Atualiza classe Apex existente

        Args:
            class_id: ID da classe
            body: Novo codigo
            api_version: Nova versao da API

        Returns:
            True se sucesso
        """
        data = {"Body": body}
        if api_version:
            data["ApiVersion"] = api_version

        endpoint = f"/sobjects/ApexClass/{class_id}"
        await self._request("PATCH", endpoint, data=data)
        return True

    async def delete_apex_class(self, class_id: str) -> bool:
        """
        Deleta classe Apex

        Args:
            class_id: ID da classe

        Returns:
            True se sucesso
        """
        endpoint = f"/sobjects/ApexClass/{class_id}"
        await self._request("DELETE", endpoint)
        return True

    # ==================== APEX TRIGGERS ====================

    async def list_apex_triggers(
        self,
        sobject: Optional[str] = None,
        name_filter: Optional[str] = None
    ) -> List[ApexTrigger]:
        """
        Lista triggers Apex

        Args:
            sobject: Filtrar por objeto
            name_filter: Filtro por nome

        Returns:
            Lista de ApexTrigger
        """
        soql = """
            SELECT Id, Name, Body, TableEnumOrId, ApiVersion, Status, IsValid,
                   UsageBeforeInsert, UsageAfterInsert,
                   UsageBeforeUpdate, UsageAfterUpdate,
                   UsageBeforeDelete, UsageAfterDelete,
                   UsageAfterUndelete
            FROM ApexTrigger
            WHERE NamespacePrefix = null
        """

        if sobject:
            soql += f" AND TableEnumOrId = '{sobject}'"
        if name_filter:
            soql += f" AND Name LIKE '{name_filter}'"

        soql += " ORDER BY Name"

        records = await self.query(soql)
        return [ApexTrigger.from_dict(r) for r in records]

    async def get_apex_trigger(self, trigger_id: str) -> ApexTrigger:
        """
        Obtem um trigger por ID

        Args:
            trigger_id: ID do trigger

        Returns:
            ApexTrigger
        """
        endpoint = f"/sobjects/ApexTrigger/{trigger_id}"
        data = await self._request("GET", endpoint)
        return ApexTrigger.from_dict(data)

    async def get_apex_trigger_by_name(self, name: str) -> Optional[ApexTrigger]:
        """
        Obtem um trigger por nome

        Args:
            name: Nome do trigger

        Returns:
            ApexTrigger ou None
        """
        soql = f"""
            SELECT Id, Name, Body, TableEnumOrId, ApiVersion, Status, IsValid,
                   UsageBeforeInsert, UsageAfterInsert,
                   UsageBeforeUpdate, UsageAfterUpdate,
                   UsageBeforeDelete, UsageAfterDelete,
                   UsageAfterUndelete
            FROM ApexTrigger
            WHERE Name = '{name}' AND NamespacePrefix = null
        """
        records = await self.query(soql)
        if records:
            return ApexTrigger.from_dict(records[0])
        return None

    async def create_apex_trigger(
        self,
        name: str,
        body: str,
        sobject: str,
        api_version: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Cria novo trigger Apex

        Args:
            name: Nome do trigger
            body: Codigo Apex
            sobject: Objeto do trigger
            api_version: Versao da API

        Returns:
            Resultado da criacao
        """
        data = {
            "Name": name,
            "Body": body,
            "TableEnumOrId": sobject,
            "ApiVersion": api_version or self.sf.config.api_version
        }

        return await self._request("POST", "/sobjects/ApexTrigger", data=data)

    async def update_apex_trigger(
        self,
        trigger_id: str,
        body: str,
        api_version: Optional[str] = None
    ) -> bool:
        """
        Atualiza trigger existente

        Args:
            trigger_id: ID do trigger
            body: Novo codigo
            api_version: Nova versao da API

        Returns:
            True se sucesso
        """
        data = {"Body": body}
        if api_version:
            data["ApiVersion"] = api_version

        endpoint = f"/sobjects/ApexTrigger/{trigger_id}"
        await self._request("PATCH", endpoint, data=data)
        return True

    async def delete_apex_trigger(self, trigger_id: str) -> bool:
        """
        Deleta trigger Apex

        Args:
            trigger_id: ID do trigger

        Returns:
            True se sucesso
        """
        endpoint = f"/sobjects/ApexTrigger/{trigger_id}"
        await self._request("DELETE", endpoint)
        return True

    # ==================== EXECUTE ANONYMOUS ====================

    async def execute_anonymous(
        self,
        apex_code: str,
        log_level: ApexLogLevel = ApexLogLevel.DEBUG
    ) -> ApexExecutionResult:
        """
        Executa codigo Apex anonimo

        Args:
            apex_code: Codigo Apex para executar
            log_level: Nivel de log

        Returns:
            ApexExecutionResult

        Exemplo:
            result = await tooling.execute_anonymous('''
                Account acc = new Account(Name = 'Test');
                insert acc;
                System.debug('Conta criada: ' + acc.Id);
            ''')

            if result.success:
                print("Executado com sucesso!")
            else:
                print(f"Erro: {result.exception_message}")
        """
        from urllib.parse import quote

        endpoint = f"/executeAnonymous?anonymousBody={quote(apex_code)}"
        data = await self._request("GET", endpoint)

        return ApexExecutionResult(
            success=data.get("success", False),
            compiled=data.get("compiled", False),
            compile_problem=data.get("compileProblem"),
            exception_message=data.get("exceptionMessage"),
            exception_stack_trace=data.get("exceptionStackTrace"),
            line=data.get("line"),
            column=data.get("column")
        )

    # ==================== APEX TESTS ====================

    async def run_tests(
        self,
        class_names: Optional[List[str]] = None,
        class_ids: Optional[List[str]] = None,
        suite_names: Optional[List[str]] = None,
        max_failed_tests: int = -1,
        test_level: str = "RunSpecifiedTests"
    ) -> str:
        """
        Inicia execucao de testes Apex

        Args:
            class_names: Nomes das classes de teste
            class_ids: IDs das classes de teste
            suite_names: Nomes dos test suites
            max_failed_tests: Maximo de falhas antes de parar (-1 = ilimitado)
            test_level: Nivel de teste

        Returns:
            ID do job de teste
        """
        data = {
            "testLevel": test_level,
            "maxFailedTests": max_failed_tests
        }

        if class_names:
            data["classNames"] = ",".join(class_names)
        elif class_ids:
            data["classIds"] = ",".join(class_ids)
        elif suite_names:
            data["suiteNames"] = ",".join(suite_names)

        result = await self._request("POST", "/runTestsAsynchronous", data=data)
        return result

    async def get_test_result(self, test_run_id: str) -> ApexTestResult:
        """
        Obtem resultado de execucao de testes

        Args:
            test_run_id: ID do job de teste

        Returns:
            ApexTestResult
        """
        # Verificar status do job
        soql = f"""
            SELECT Id, Status, ClassesCompleted, ClassesEnqueued,
                   MethodsCompleted, MethodsEnqueued, MethodsFailed,
                   StartTime, EndTime, TestTime
            FROM ApexTestRunResult
            WHERE AsyncApexJobId = '{test_run_id}'
        """
        job_records = await self.query(soql)

        if not job_records:
            return ApexTestResult(
                success=False,
                num_tests_run=0,
                num_failures=0,
                total_time=0.0
            )

        job = job_records[0]

        # Buscar resultados detalhados
        soql_results = f"""
            SELECT Id, ApexClassId, MethodName, Outcome, Message,
                   StackTrace, RunTime, TestTimestamp
            FROM ApexTestResult
            WHERE AsyncApexJobId = '{test_run_id}'
        """
        test_records = await self.query(soql_results)

        # Separar sucessos e falhas
        successes = []
        failures = []

        for record in test_records:
            if record.get("Outcome") == "Pass":
                successes.append({
                    "methodName": record.get("MethodName"),
                    "className": record.get("ApexClassId"),
                    "time": record.get("RunTime", 0)
                })
            else:
                failures.append({
                    "methodName": record.get("MethodName"),
                    "className": record.get("ApexClassId"),
                    "message": record.get("Message"),
                    "stackTrace": record.get("StackTrace"),
                    "outcome": record.get("Outcome")
                })

        return ApexTestResult(
            success=len(failures) == 0,
            num_tests_run=len(test_records),
            num_failures=len(failures),
            total_time=job.get("TestTime", 0),
            test_successes=successes,
            test_failures=failures
        )

    async def run_tests_and_wait(
        self,
        class_names: List[str],
        poll_interval: int = 5,
        timeout: int = 600
    ) -> ApexTestResult:
        """
        Executa testes e aguarda conclusao

        Args:
            class_names: Nomes das classes de teste
            poll_interval: Intervalo de polling em segundos
            timeout: Timeout maximo em segundos

        Returns:
            ApexTestResult final
        """
        test_run_id = await self.run_tests(class_names=class_names)
        logger.info(f"Testes iniciados: {test_run_id}")

        start_time = asyncio.get_event_loop().time()

        while True:
            # Verificar status
            soql = f"""
                SELECT Status FROM ApexTestRunResult
                WHERE AsyncApexJobId = '{test_run_id}'
            """
            records = await self.query(soql)

            if records:
                status = records[0].get("Status")
                if status in ("Completed", "Failed", "Aborted"):
                    return await self.get_test_result(test_run_id)

            elapsed = asyncio.get_event_loop().time() - start_time
            if elapsed > timeout:
                raise TimeoutError(f"Testes timeout apos {timeout}s")

            await asyncio.sleep(poll_interval)

    async def get_code_coverage(
        self,
        class_names: Optional[List[str]] = None
    ) -> List[Dict[str, Any]]:
        """
        Obtem coverage de codigo

        Args:
            class_names: Filtrar por classes especificas

        Returns:
            Lista com coverage por classe
        """
        soql = """
            SELECT ApexClassOrTriggerId, ApexClassOrTrigger.Name,
                   NumLinesCovered, NumLinesUncovered
            FROM ApexCodeCoverageAggregate
        """

        if class_names:
            names_list = "', '".join(class_names)
            soql += f" WHERE ApexClassOrTrigger.Name IN ('{names_list}')"

        records = await self.query(soql)

        coverage = []
        for record in records:
            covered = record.get("NumLinesCovered", 0)
            uncovered = record.get("NumLinesUncovered", 0)
            total = covered + uncovered
            percentage = (covered / total * 100) if total > 0 else 0

            coverage.append({
                "id": record.get("ApexClassOrTriggerId"),
                "name": record.get("ApexClassOrTrigger", {}).get("Name"),
                "linesCovered": covered,
                "linesUncovered": uncovered,
                "totalLines": total,
                "coveragePercentage": round(percentage, 2)
            })

        return coverage

    # ==================== DEBUG LOGS ====================

    async def create_trace_flag(
        self,
        user_id: Optional[str] = None,
        apex_code: ApexLogLevel = ApexLogLevel.DEBUG,
        apex_profiling: ApexLogLevel = ApexLogLevel.INFO,
        callout: ApexLogLevel = ApexLogLevel.INFO,
        database: ApexLogLevel = ApexLogLevel.INFO,
        system: ApexLogLevel = ApexLogLevel.DEBUG,
        validation: ApexLogLevel = ApexLogLevel.INFO,
        visualforce: ApexLogLevel = ApexLogLevel.INFO,
        workflow: ApexLogLevel = ApexLogLevel.INFO,
        duration_minutes: int = 30
    ) -> str:
        """
        Cria trace flag para habilitar debug logs

        Args:
            user_id: ID do usuario (padrao: usuario autenticado)
            apex_code: Nivel de log para Apex
            ...: Niveis para outras categorias
            duration_minutes: Duracao em minutos

        Returns:
            ID do trace flag
        """
        if not user_id:
            user_id = self.sf.config.user_id

        # Primeiro, criar ou obter debug level
        debug_level_name = f"FabricaAgentes_{datetime.now().strftime('%Y%m%d%H%M%S')}"

        debug_level_data = {
            "DeveloperName": debug_level_name,
            "MasterLabel": debug_level_name,
            "ApexCode": apex_code.value,
            "ApexProfiling": apex_profiling.value,
            "Callout": callout.value,
            "Database": database.value,
            "System": system.value,
            "Validation": validation.value,
            "Visualforce": visualforce.value,
            "Workflow": workflow.value
        }

        debug_level_result = await self._request(
            "POST",
            "/sobjects/DebugLevel",
            data=debug_level_data
        )
        debug_level_id = debug_level_result.get("id")

        # Criar trace flag
        from datetime import timedelta

        expiration = datetime.utcnow() + timedelta(minutes=duration_minutes)

        trace_flag_data = {
            "TracedEntityId": user_id,
            "DebugLevelId": debug_level_id,
            "LogType": "USER_DEBUG",
            "ExpirationDate": expiration.isoformat() + "Z"
        }

        result = await self._request(
            "POST",
            "/sobjects/TraceFlag",
            data=trace_flag_data
        )

        return result.get("id")

    async def get_debug_logs(
        self,
        user_id: Optional[str] = None,
        limit: int = 20
    ) -> List[Dict[str, Any]]:
        """
        Lista debug logs

        Args:
            user_id: Filtrar por usuario
            limit: Numero maximo de logs

        Returns:
            Lista de logs
        """
        soql = f"""
            SELECT Id, LogUserId, LogUser.Name, Operation,
                   Application, Status, DurationMilliseconds,
                   LogLength, StartTime
            FROM ApexLog
        """

        if user_id:
            soql += f" WHERE LogUserId = '{user_id}'"

        soql += f" ORDER BY StartTime DESC LIMIT {limit}"

        return await self.query(soql)

    async def get_debug_log_body(self, log_id: str) -> str:
        """
        Obtem conteudo de um debug log

        Args:
            log_id: ID do log

        Returns:
            Conteudo do log
        """
        endpoint = f"/sobjects/ApexLog/{log_id}/Body"
        url = f"{self.tooling_url}{endpoint}"

        session = await self.sf._get_session()
        async with session.get(url, headers=self.sf.headers) as response:
            return await response.text()

    async def delete_debug_logs(
        self,
        log_ids: Optional[List[str]] = None,
        user_id: Optional[str] = None,
        delete_all: bool = False
    ) -> int:
        """
        Deleta debug logs

        Args:
            log_ids: IDs especificos para deletar
            user_id: Deletar todos de um usuario
            delete_all: Deletar todos os logs

        Returns:
            Numero de logs deletados
        """
        if log_ids:
            ids_to_delete = log_ids
        elif user_id:
            logs = await self.get_debug_logs(user_id=user_id, limit=100)
            ids_to_delete = [log["Id"] for log in logs]
        elif delete_all:
            logs = await self.get_debug_logs(limit=100)
            ids_to_delete = [log["Id"] for log in logs]
        else:
            return 0

        count = 0
        for log_id in ids_to_delete:
            try:
                endpoint = f"/sobjects/ApexLog/{log_id}"
                await self._request("DELETE", endpoint)
                count += 1
            except Exception as e:
                logger.warning(f"Erro ao deletar log {log_id}: {e}")

        return count

    # ==================== SYMBOL TABLE ====================

    async def get_symbol_table(self, class_id: str) -> Dict[str, Any]:
        """
        Obtem symbol table de uma classe

        A symbol table contem informacoes sobre metodos,
        variaveis e referencias da classe.

        Args:
            class_id: ID da classe

        Returns:
            Symbol table da classe
        """
        soql = f"""
            SELECT Id, Name, SymbolTable
            FROM ApexClass
            WHERE Id = '{class_id}'
        """
        records = await self.query(soql)

        if records and records[0].get("SymbolTable"):
            return json.loads(records[0]["SymbolTable"])

        return {}

    async def get_class_dependencies(self, class_id: str) -> Dict[str, Any]:
        """
        Obtem dependencias de uma classe

        Args:
            class_id: ID da classe

        Returns:
            Dict com dependencias (referencias a outras classes)
        """
        symbol_table = await self.get_symbol_table(class_id)

        dependencies = {
            "references": [],
            "referenced_by": []
        }

        # Extrair referencias externas
        external_refs = symbol_table.get("externalReferences", [])
        for ref in external_refs:
            dependencies["references"].append({
                "name": ref.get("name"),
                "namespace": ref.get("namespace"),
                "methods": ref.get("methods", []),
                "variables": ref.get("variables", [])
            })

        return dependencies

    # ==================== COMPLETIONS ====================

    async def get_completions(
        self,
        apex_code: str,
        line: int,
        column: int
    ) -> List[Dict[str, Any]]:
        """
        Obtem sugestoes de autocompletar

        Args:
            apex_code: Codigo Apex atual
            line: Linha do cursor (0-indexed)
            column: Coluna do cursor (0-indexed)

        Returns:
            Lista de sugestoes
        """
        data = {
            "line": line,
            "column": column,
            "body": apex_code
        }

        result = await self._request("POST", "/completions", data=data)
        return result.get("completions", [])

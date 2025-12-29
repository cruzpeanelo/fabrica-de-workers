# -*- coding: utf-8 -*-
"""
SAP RFC Client
==============
Cliente para conexao RFC com SAP ECC usando PyRFC.

Este modulo fornece:
- Conexao RFC com SAP via SAP NW RFC SDK
- Execucao de Function Modules e BAPIs
- Leitura de tabelas SAP
- Tratamento de erros RFC

Requisitos:
-----------
1. SAP NW RFC SDK instalado:
   - Download: https://support.sap.com/en/product/connectors/nwrfcsdk.html
   - Extrair para C:\\nwrfcsdk (Windows) ou /usr/local/sap/nwrfcsdk (Linux)
   - Adicionar ao PATH do sistema

2. PyRFC instalado:
   pip install pyrfc

Exemplo de uso:
---------------
```python
from factory.integrations.sap_ecc.rfc_client import SAPRFCClient, SAPRFCConfig

config = SAPRFCConfig(
    ashost="sap-server.company.com",
    sysnr="00",
    client="100",
    user="RFC_USER",
    passwd="xxx",
    lang="PT"
)

async with SAPRFCClient(config) as client:
    # Chamar BAPI
    result = await client.call_function(
        "BAPI_MATERIAL_GETLIST",
        {"MAXROWS": 100}
    )

    # Ler tabela
    data = await client.read_table(
        "MARA",
        fields=["MATNR", "MTART", "MATKL"],
        options=[{"TEXT": "MATNR LIKE 'A%'"}]
    )
```
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple, Union
from contextlib import asynccontextmanager

logger = logging.getLogger(__name__)


class RFCConnectionError(Exception):
    """Erro de conexao RFC"""
    def __init__(self, message: str, details: Optional[Dict] = None):
        super().__init__(message)
        self.details = details or {}


class RFCExecutionError(Exception):
    """Erro de execucao de funcao RFC"""
    def __init__(self, message: str, function_name: str = "",
                 return_messages: Optional[List[Dict]] = None):
        super().__init__(message)
        self.function_name = function_name
        self.return_messages = return_messages or []


class RFCConnectionStatus(str, Enum):
    """Status da conexao RFC"""
    DISCONNECTED = "disconnected"
    CONNECTING = "connecting"
    CONNECTED = "connected"
    ERROR = "error"


@dataclass
class SAPRFCConfig:
    """
    Configuracao para conexao RFC com SAP.

    Atributos:
        ashost: Hostname ou IP do servidor SAP
        sysnr: Numero do sistema (00-99)
        client: Mandante SAP (000-999)
        user: Usuario RFC
        passwd: Senha do usuario
        lang: Idioma (PT, EN, DE, etc.)
        pool_size: Tamanho do pool de conexoes
        timeout: Timeout em segundos para operacoes
        trace: Nivel de trace (0-3)
        mshost: Host do Message Server (para load balancing)
        msserv: Porta do Message Server
        group: Grupo de logon (para load balancing)
    """
    ashost: str = ""
    sysnr: str = "00"
    client: str = "100"
    user: str = ""
    passwd: str = ""
    lang: str = "PT"
    pool_size: int = 5
    timeout: int = 30
    trace: int = 0
    # Load Balancing (opcional)
    mshost: Optional[str] = None
    msserv: Optional[str] = None
    group: Optional[str] = None
    # SNC (Secure Network Communication) - opcional
    snc_partnername: Optional[str] = None
    snc_mode: Optional[str] = None
    snc_myname: Optional[str] = None
    snc_qop: Optional[str] = None
    snc_lib: Optional[str] = None

    def to_connection_params(self) -> Dict[str, str]:
        """Converte para parametros de conexao PyRFC"""
        params = {
            "user": self.user,
            "passwd": self.passwd,
            "client": self.client,
            "lang": self.lang
        }

        # Conexao direta ou load balancing
        if self.mshost:
            params["mshost"] = self.mshost
            if self.msserv:
                params["msserv"] = self.msserv
            if self.group:
                params["group"] = self.group
        else:
            params["ashost"] = self.ashost
            params["sysnr"] = self.sysnr

        # SNC (se configurado)
        if self.snc_mode:
            params["snc_mode"] = self.snc_mode
            if self.snc_partnername:
                params["snc_partnername"] = self.snc_partnername
            if self.snc_myname:
                params["snc_myname"] = self.snc_myname
            if self.snc_qop:
                params["snc_qop"] = self.snc_qop
            if self.snc_lib:
                params["snc_lib"] = self.snc_lib

        return params

    def validate(self) -> Tuple[bool, List[str]]:
        """Valida a configuracao"""
        errors = []

        if not self.user:
            errors.append("Usuario RFC nao informado")
        if not self.passwd:
            errors.append("Senha RFC nao informada")
        if not self.client:
            errors.append("Mandante nao informado")

        # Deve ter conexao direta ou load balancing
        if not self.ashost and not self.mshost:
            errors.append("Servidor nao informado (ashost ou mshost)")

        return len(errors) == 0, errors


@dataclass
class RFCResult:
    """Resultado de uma chamada RFC"""
    success: bool
    data: Dict[str, Any] = field(default_factory=dict)
    tables: Dict[str, List[Dict]] = field(default_factory=dict)
    return_messages: List[Dict] = field(default_factory=list)
    execution_time_ms: float = 0.0
    error: Optional[str] = None

    def has_errors(self) -> bool:
        """Verifica se ha mensagens de erro no retorno"""
        for msg in self.return_messages:
            if msg.get("TYPE", "") in ("E", "A"):
                return True
        return False

    def get_error_messages(self) -> List[str]:
        """Retorna lista de mensagens de erro"""
        errors = []
        for msg in self.return_messages:
            if msg.get("TYPE", "") in ("E", "A"):
                errors.append(f"{msg.get('ID', '')}-{msg.get('NUMBER', '')}: {msg.get('MESSAGE', '')}")
        return errors


class SAPRFCClient:
    """
    Cliente RFC para SAP ECC.

    Fornece metodos para:
    - Conectar e desconectar do SAP
    - Executar Function Modules e BAPIs
    - Ler tabelas SAP
    - Verificar autorizacoes

    Exemplo:
    --------
    ```python
    config = SAPRFCConfig(
        ashost="sapserver.company.com",
        sysnr="00",
        client="100",
        user="RFCUSER",
        passwd="password"
    )

    client = SAPRFCClient(config)
    await client.connect()

    result = await client.call_function("RFC_READ_TABLE", {...})

    await client.disconnect()
    ```
    """

    def __init__(self, config: SAPRFCConfig):
        """
        Inicializa o cliente RFC.

        Args:
            config: Configuracao de conexao RFC
        """
        self.config = config
        self._connection = None
        self._status = RFCConnectionStatus.DISCONNECTED
        self._last_error: Optional[str] = None
        self._connected_at: Optional[datetime] = None
        self._call_count = 0
        self._pyrfc_available = False

        # Tentar importar PyRFC
        try:
            import pyrfc
            self._pyrfc = pyrfc
            self._pyrfc_available = True
        except ImportError:
            logger.warning(
                "PyRFC nao esta instalado. Instale com: pip install pyrfc. "
                "Requer SAP NW RFC SDK instalado no sistema."
            )
            self._pyrfc = None

    @property
    def status(self) -> RFCConnectionStatus:
        """Status atual da conexao"""
        return self._status

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self._status == RFCConnectionStatus.CONNECTED and self._connection is not None

    @property
    def last_error(self) -> Optional[str]:
        """Ultimo erro ocorrido"""
        return self._last_error

    async def connect(self) -> bool:
        """
        Estabelece conexao RFC com o SAP.

        Returns:
            True se conectou com sucesso

        Raises:
            RFCConnectionError: Se falhar ao conectar
        """
        if self.is_connected:
            return True

        if not self._pyrfc_available:
            raise RFCConnectionError(
                "PyRFC nao esta disponivel. Instale o SAP NW RFC SDK e PyRFC."
            )

        # Validar configuracao
        is_valid, errors = self.config.validate()
        if not is_valid:
            raise RFCConnectionError(
                f"Configuracao invalida: {', '.join(errors)}"
            )

        self._status = RFCConnectionStatus.CONNECTING

        try:
            # Executar conexao em thread separada (PyRFC e bloqueante)
            loop = asyncio.get_event_loop()
            connection_params = self.config.to_connection_params()

            self._connection = await loop.run_in_executor(
                None,
                lambda: self._pyrfc.Connection(**connection_params)
            )

            self._status = RFCConnectionStatus.CONNECTED
            self._connected_at = datetime.now()
            self._last_error = None

            logger.info(
                f"Conectado ao SAP: {self.config.ashost or self.config.mshost} "
                f"mandante {self.config.client}"
            )

            return True

        except Exception as e:
            self._status = RFCConnectionStatus.ERROR
            self._last_error = str(e)
            self._connection = None

            logger.error(f"Erro ao conectar ao SAP: {e}")
            raise RFCConnectionError(str(e))

    async def disconnect(self) -> bool:
        """
        Encerra conexao RFC.

        Returns:
            True se desconectou com sucesso
        """
        if not self.is_connected:
            return True

        try:
            loop = asyncio.get_event_loop()
            await loop.run_in_executor(
                None,
                self._connection.close
            )

            self._connection = None
            self._status = RFCConnectionStatus.DISCONNECTED

            logger.info("Desconectado do SAP")
            return True

        except Exception as e:
            logger.error(f"Erro ao desconectar do SAP: {e}")
            self._connection = None
            self._status = RFCConnectionStatus.DISCONNECTED
            return False

    async def test_connection(self) -> bool:
        """
        Testa a conexao RFC.

        Returns:
            True se a conexao esta funcionando
        """
        try:
            result = await self.call_function("RFC_PING")
            return result.success
        except Exception as e:
            logger.error(f"Erro ao testar conexao: {e}")
            return False

    async def call_function(
        self,
        function_name: str,
        parameters: Optional[Dict[str, Any]] = None,
        commit: bool = False
    ) -> RFCResult:
        """
        Executa uma Function Module RFC.

        Args:
            function_name: Nome da funcao (ex: BAPI_MATERIAL_GETLIST)
            parameters: Parametros de entrada da funcao
            commit: Se True, executa BAPI_TRANSACTION_COMMIT apos a funcao

        Returns:
            RFCResult com o resultado da execucao

        Raises:
            RFCConnectionError: Se nao estiver conectado
            RFCExecutionError: Se ocorrer erro na execucao
        """
        if not self.is_connected:
            raise RFCConnectionError("Nao conectado ao SAP")

        parameters = parameters or {}
        start_time = datetime.now()

        try:
            loop = asyncio.get_event_loop()

            # Executar funcao RFC
            result = await loop.run_in_executor(
                None,
                lambda: self._connection.call(function_name, **parameters)
            )

            execution_time = (datetime.now() - start_time).total_seconds() * 1000
            self._call_count += 1

            # Processar resultado
            rfc_result = self._process_result(result, execution_time)

            # Commit se solicitado e sem erros
            if commit and not rfc_result.has_errors():
                await self.call_function("BAPI_TRANSACTION_COMMIT", {"WAIT": "X"})

            logger.debug(
                f"RFC {function_name} executada em {execution_time:.0f}ms"
            )

            return rfc_result

        except Exception as e:
            execution_time = (datetime.now() - start_time).total_seconds() * 1000

            logger.error(f"Erro ao executar {function_name}: {e}")

            return RFCResult(
                success=False,
                error=str(e),
                execution_time_ms=execution_time
            )

    async def read_table(
        self,
        table_name: str,
        fields: Optional[List[str]] = None,
        options: Optional[List[Dict[str, str]]] = None,
        max_rows: int = 0,
        skip_rows: int = 0,
        delimiter: str = "|"
    ) -> List[Dict[str, Any]]:
        """
        Le dados de uma tabela SAP usando RFC_READ_TABLE.

        Args:
            table_name: Nome da tabela (ex: MARA, VBAK, etc.)
            fields: Lista de campos a retornar (None = todos)
            options: Clausulas WHERE (ex: [{"TEXT": "MATNR LIKE 'A%'"}])
            max_rows: Maximo de linhas (0 = todas)
            skip_rows: Linhas a pular
            delimiter: Delimitador de campos

        Returns:
            Lista de dicionarios com os dados

        Exemplo:
        --------
        ```python
        # Ler materiais que comecam com 'A'
        data = await client.read_table(
            "MARA",
            fields=["MATNR", "MTART", "MATKL"],
            options=[{"TEXT": "MATNR LIKE 'A%'"}],
            max_rows=100
        )
        ```
        """
        # Preparar campos
        field_list = []
        if fields:
            field_list = [{"FIELDNAME": f} for f in fields]

        # Preparar opcoes (WHERE)
        option_list = options or []

        # Chamar RFC_READ_TABLE
        params = {
            "QUERY_TABLE": table_name.upper(),
            "DELIMITER": delimiter,
            "ROWSKIPS": skip_rows,
            "ROWCOUNT": max_rows,
            "FIELDS": field_list,
            "OPTIONS": option_list
        }

        result = await self.call_function("RFC_READ_TABLE", params)

        if not result.success:
            logger.error(f"Erro ao ler tabela {table_name}: {result.error}")
            return []

        # Processar resultado
        data = result.data
        field_info = data.get("FIELDS", [])
        raw_data = data.get("DATA", [])

        # Montar lista de campos com posicoes
        field_positions = []
        for f in field_info:
            field_positions.append({
                "name": f["FIELDNAME"],
                "offset": int(f.get("OFFSET", 0)),
                "length": int(f.get("LENGTH", 0)),
                "type": f.get("TYPE", "")
            })

        # Converter dados
        records = []
        for row in raw_data:
            wa = row.get("WA", "")
            if delimiter:
                values = wa.split(delimiter)
                record = {}
                for i, field in enumerate(field_positions):
                    if i < len(values):
                        record[field["name"]] = values[i].strip()
                records.append(record)
            else:
                # Sem delimitador - usar posicoes fixas
                record = {}
                for field in field_positions:
                    start = field["offset"]
                    end = start + field["length"]
                    record[field["name"]] = wa[start:end].strip()
                records.append(record)

        logger.debug(f"Lidas {len(records)} linhas da tabela {table_name}")
        return records

    async def call_bapi(
        self,
        bapi_name: str,
        parameters: Optional[Dict[str, Any]] = None,
        auto_commit: bool = True
    ) -> RFCResult:
        """
        Executa uma BAPI com tratamento automatico de commit/rollback.

        Args:
            bapi_name: Nome da BAPI (ex: BAPI_MATERIAL_SAVEDATA)
            parameters: Parametros de entrada
            auto_commit: Se True, faz commit automatico em caso de sucesso

        Returns:
            RFCResult com o resultado
        """
        result = await self.call_function(bapi_name, parameters)

        if auto_commit:
            if result.success and not result.has_errors():
                await self.call_function(
                    "BAPI_TRANSACTION_COMMIT",
                    {"WAIT": "X"}
                )
            else:
                await self.call_function("BAPI_TRANSACTION_ROLLBACK")

        return result

    async def get_function_interface(
        self,
        function_name: str
    ) -> Dict[str, Any]:
        """
        Obtem a interface (parametros) de uma Function Module.

        Args:
            function_name: Nome da funcao

        Returns:
            Dicionario com import, export, changing, tables e exceptions
        """
        result = await self.call_function(
            "RFC_GET_FUNCTION_INTERFACE",
            {"FUNCNAME": function_name}
        )

        if not result.success:
            return {}

        data = result.data
        return {
            "import": data.get("PARAMS_IMPORT", []),
            "export": data.get("PARAMS_EXPORT", []),
            "changing": data.get("PARAMS_CHANGING", []),
            "tables": data.get("PARAMS_TABLES", []),
            "exceptions": data.get("PARAMS_EXCEPTIONS", [])
        }

    async def check_authorization(
        self,
        auth_object: str,
        auth_fields: Dict[str, str]
    ) -> bool:
        """
        Verifica autorizacao do usuario.

        Args:
            auth_object: Objeto de autorizacao (ex: S_TCODE)
            auth_fields: Campos e valores a verificar

        Returns:
            True se autorizado
        """
        # Montar tabela de campos
        auth_values = [
            {"FIELD": k, "VALUE": v}
            for k, v in auth_fields.items()
        ]

        result = await self.call_function(
            "AUTHORITY_CHECK",
            {
                "OBJECT": auth_object,
                "AUTHORITY_FIELDS": auth_values
            }
        )

        return result.success and not result.has_errors()

    async def get_system_info(self) -> Dict[str, Any]:
        """
        Obtem informacoes do sistema SAP.

        Returns:
            Dicionario com informacoes do sistema
        """
        result = await self.call_function("RFC_SYSTEM_INFO")

        if not result.success:
            return {}

        rfcsi = result.data.get("RFCSI_EXPORT", {})
        return {
            "system_id": rfcsi.get("RFCSYSID", ""),
            "database": rfcsi.get("RFCDBHOST", ""),
            "host": rfcsi.get("RFCHOST", ""),
            "instance": rfcsi.get("RFCINSTANZ", ""),
            "ip_address": rfcsi.get("RFCIPADDR", ""),
            "kernel_release": rfcsi.get("RFCKERNRL", ""),
            "codepage": rfcsi.get("RFCSI_CODEPAGE", ""),
            "unicode": rfcsi.get("RFCUNICODE", "") == "X"
        }

    def _process_result(
        self,
        raw_result: Dict[str, Any],
        execution_time_ms: float
    ) -> RFCResult:
        """Processa o resultado bruto de uma chamada RFC"""
        data = {}
        tables = {}
        return_messages = []

        for key, value in raw_result.items():
            # Tabela RETURN padrao das BAPIs
            if key.upper() == "RETURN":
                if isinstance(value, list):
                    return_messages = value
                elif isinstance(value, dict):
                    return_messages = [value]
            # Identificar tabelas (listas de dicionarios)
            elif isinstance(value, list) and len(value) > 0 and isinstance(value[0], dict):
                tables[key] = value
            else:
                data[key] = value

        return RFCResult(
            success=True,
            data=data,
            tables=tables,
            return_messages=return_messages,
            execution_time_ms=execution_time_ms
        )

    def get_statistics(self) -> Dict[str, Any]:
        """Retorna estatisticas do cliente"""
        return {
            "status": self._status.value,
            "connected": self.is_connected,
            "connected_at": self._connected_at.isoformat() if self._connected_at else None,
            "call_count": self._call_count,
            "last_error": self._last_error,
            "server": self.config.ashost or self.config.mshost,
            "client": self.config.client
        }

    async def __aenter__(self):
        """Context manager - entrada"""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Context manager - saida"""
        await self.disconnect()
        return False


# Pool de conexoes RFC
class SAPRFCPool:
    """
    Pool de conexoes RFC para melhor performance.

    Gerencia multiplas conexoes RFC reutilizaveis.
    """

    def __init__(self, config: SAPRFCConfig, pool_size: int = 5):
        """
        Inicializa o pool.

        Args:
            config: Configuracao RFC
            pool_size: Tamanho do pool
        """
        self.config = config
        self.pool_size = pool_size
        self._pool: List[SAPRFCClient] = []
        self._available: List[SAPRFCClient] = []
        self._lock = asyncio.Lock()

    async def get_connection(self) -> SAPRFCClient:
        """
        Obtem uma conexao do pool.

        Returns:
            Cliente RFC conectado
        """
        async with self._lock:
            # Tentar reutilizar conexao disponivel
            if self._available:
                client = self._available.pop()
                if client.is_connected:
                    return client

            # Criar nova conexao se pool nao esta cheio
            if len(self._pool) < self.pool_size:
                client = SAPRFCClient(self.config)
                await client.connect()
                self._pool.append(client)
                return client

            # Aguardar conexao disponivel
            while not self._available:
                await asyncio.sleep(0.1)

            return self._available.pop()

    async def release_connection(self, client: SAPRFCClient):
        """
        Devolve uma conexao ao pool.

        Args:
            client: Cliente a devolver
        """
        async with self._lock:
            if client in self._pool and client.is_connected:
                self._available.append(client)

    async def close_all(self):
        """Fecha todas as conexoes do pool"""
        async with self._lock:
            for client in self._pool:
                await client.disconnect()
            self._pool.clear()
            self._available.clear()

    @asynccontextmanager
    async def connection(self):
        """
        Context manager para obter conexao do pool.

        Exemplo:
        --------
        ```python
        async with pool.connection() as client:
            result = await client.call_function("RFC_PING")
        ```
        """
        client = await self.get_connection()
        try:
            yield client
        finally:
            await self.release_connection(client)

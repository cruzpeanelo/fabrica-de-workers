# -*- coding: utf-8 -*-
"""
Salesforce REST API Client
==========================
Cliente principal para interacao com Salesforce REST API.

Funcionalidades:
- Autenticacao via Username/Password ou OAuth 2.0
- Operacoes CRUD em objetos Salesforce
- Consultas SOQL
- Busca SOSL
- Gerenciamento de sessao com refresh automatico
- Isolamento multi-tenant com cache por tenant (Issue #314)

Exemplo de uso:
    from factory.integrations.salesforce import SalesforceClient, SalesforceConfig

    config = SalesforceConfig(
        tenant_id="TENANT-001",
        username="user@empresa.com",
        password="senha123",
        security_token="token"
    )
    client = SalesforceClient(config)

    await client.connect()

    # Consulta SOQL
    accounts = await client.query("SELECT Id, Name FROM Account LIMIT 10")

    # Criar registro
    result = await client.create("Account", {"Name": "Nova Empresa"})

    # Atualizar registro
    await client.update("Account", result["id"], {"Name": "Empresa Atualizada"})

    # Deletar registro
    await client.delete("Account", result["id"])
"""

import asyncio
import aiohttp
import json
import logging
import time
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union, ClassVar
from urllib.parse import urlencode, quote

from .config import SalesforceConfig, SalesforceOAuthConfig, AuthenticationType

logger = logging.getLogger(__name__)


# ==================== PER-TENANT TOKEN CACHE ====================

@dataclass
class CachedToken:
    """Token em cache com metadata de expiracao"""
    access_token: str
    instance_url: str
    user_id: Optional[str]
    organization_id: Optional[str]
    cached_at: datetime
    expires_at: datetime

    @property
    def is_expired(self) -> bool:
        """Verifica se o token expirou"""
        return datetime.utcnow() >= self.expires_at


class TenantTokenCache:
    """
    Cache de tokens por tenant para isolamento multi-tenant.

    Garante que cada tenant tenha seu proprio token em cache,
    evitando vazamento de credenciais entre tenants.
    """
    _instance: ClassVar[Optional["TenantTokenCache"]] = None
    _cache: ClassVar[Dict[str, CachedToken]] = {}
    _default_ttl_seconds: ClassVar[int] = 7200  # 2 horas

    @classmethod
    def get_instance(cls) -> "TenantTokenCache":
        """Singleton para compartilhar cache entre instancias"""
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    def get_token(self, tenant_id: str) -> Optional[CachedToken]:
        """
        Recupera token do cache para um tenant.

        Args:
            tenant_id: ID do tenant

        Returns:
            CachedToken ou None se nao encontrado/expirado
        """
        cached = self._cache.get(tenant_id)
        if cached and not cached.is_expired:
            return cached
        elif cached:
            # Remove token expirado
            del self._cache[tenant_id]
        return None

    def set_token(
        self,
        tenant_id: str,
        access_token: str,
        instance_url: str,
        user_id: Optional[str] = None,
        organization_id: Optional[str] = None,
        ttl_seconds: Optional[int] = None
    ):
        """
        Armazena token no cache para um tenant.

        Args:
            tenant_id: ID do tenant
            access_token: Token de acesso
            instance_url: URL da instancia Salesforce
            user_id: ID do usuario
            organization_id: ID da organizacao
            ttl_seconds: Tempo de vida em segundos
        """
        ttl = ttl_seconds or self._default_ttl_seconds
        self._cache[tenant_id] = CachedToken(
            access_token=access_token,
            instance_url=instance_url,
            user_id=user_id,
            organization_id=organization_id,
            cached_at=datetime.utcnow(),
            expires_at=datetime.utcnow() + timedelta(seconds=ttl)
        )
        logger.debug(f"Token cacheado para tenant {tenant_id}")

    def invalidate_token(self, tenant_id: str):
        """
        Remove token do cache para um tenant.

        Args:
            tenant_id: ID do tenant
        """
        if tenant_id in self._cache:
            del self._cache[tenant_id]
            logger.debug(f"Token invalidado para tenant {tenant_id}")

    def clear_all(self):
        """Remove todos os tokens do cache"""
        self._cache.clear()
        logger.info("Cache de tokens limpo")

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do cache"""
        active_tokens = sum(1 for t in self._cache.values() if not t.is_expired)
        return {
            "total_cached": len(self._cache),
            "active_tokens": active_tokens,
            "expired_tokens": len(self._cache) - active_tokens,
            "tenants": list(self._cache.keys())
        }


# ==================== AUDIT LOGGING ====================

class AuditLogger:
    """
    Logger de auditoria para operacoes Salesforce por tenant.

    Registra todas as operacoes com contexto de tenant para
    rastreabilidade e compliance.
    """
    _instance: ClassVar[Optional["AuditLogger"]] = None
    _logs: ClassVar[List[Dict[str, Any]]] = []
    _max_logs: ClassVar[int] = 10000

    @classmethod
    def get_instance(cls) -> "AuditLogger":
        """Singleton para compartilhar logs entre instancias"""
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    def log_operation(
        self,
        tenant_id: str,
        operation: str,
        resource: str,
        success: bool,
        details: Optional[Dict[str, Any]] = None,
        error: Optional[str] = None,
        user_id: Optional[str] = None,
        duration_ms: Optional[int] = None
    ):
        """
        Registra uma operacao para auditoria.

        Args:
            tenant_id: ID do tenant
            operation: Tipo de operacao (QUERY, CREATE, UPDATE, DELETE, etc)
            resource: Recurso acessado (objeto, endpoint)
            success: Se a operacao foi bem sucedida
            details: Detalhes adicionais
            error: Mensagem de erro se houver
            user_id: ID do usuario Salesforce
            duration_ms: Duracao da operacao em milissegundos
        """
        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "tenant_id": tenant_id,
            "operation": operation,
            "resource": resource,
            "success": success,
            "user_id": user_id,
            "duration_ms": duration_ms,
            "details": details,
            "error": error
        }

        self._logs.append(log_entry)

        # Manter apenas os ultimos N registros
        if len(self._logs) > self._max_logs:
            self._logs = self._logs[-self._max_logs:]

        # Log tambem no logger padrao
        if success:
            logger.info(
                f"[AUDIT] tenant={tenant_id} op={operation} resource={resource}"
            )
        else:
            logger.warning(
                f"[AUDIT] tenant={tenant_id} op={operation} resource={resource} error={error}"
            )

    def get_logs(
        self,
        tenant_id: Optional[str] = None,
        operation: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Recupera logs de auditoria.

        Args:
            tenant_id: Filtrar por tenant
            operation: Filtrar por operacao
            limit: Limite de registros

        Returns:
            Lista de logs
        """
        logs = self._logs

        if tenant_id:
            logs = [l for l in logs if l.get("tenant_id") == tenant_id]
        if operation:
            logs = [l for l in logs if l.get("operation") == operation]

        return logs[-limit:]

    def clear_logs(self, tenant_id: Optional[str] = None):
        """Limpa logs de auditoria"""
        if tenant_id:
            self._logs = [l for l in self._logs if l.get("tenant_id") != tenant_id]
        else:
            self._logs.clear()


class SalesforceError(Exception):
    """Excecao base para erros do Salesforce"""

    def __init__(self, message: str, error_code: Optional[str] = None,
                 fields: Optional[List[str]] = None):
        super().__init__(message)
        self.error_code = error_code
        self.fields = fields or []

    def __str__(self):
        msg = super().__str__()
        if self.error_code:
            msg = f"[{self.error_code}] {msg}"
        if self.fields:
            msg = f"{msg} (campos: {', '.join(self.fields)})"
        return msg


class AuthenticationError(SalesforceError):
    """Erro de autenticacao"""
    pass


class QueryError(SalesforceError):
    """Erro em consulta SOQL/SOSL"""
    pass


class DMLError(SalesforceError):
    """Erro em operacao DML (insert, update, delete)"""
    pass


class RateLimitError(SalesforceError):
    """Erro de limite de requisicoes"""
    pass


@dataclass
class QueryResult:
    """Resultado de uma consulta SOQL"""
    records: List[Dict[str, Any]]
    total_size: int
    done: bool
    next_records_url: Optional[str] = None

    @classmethod
    def from_response(cls, data: Dict[str, Any]) -> "QueryResult":
        """Cria QueryResult a partir da resposta da API"""
        return cls(
            records=data.get("records", []),
            total_size=data.get("totalSize", 0),
            done=data.get("done", True),
            next_records_url=data.get("nextRecordsUrl")
        )


@dataclass
class SObjectDescribe:
    """Descricao de um objeto Salesforce"""
    name: str
    label: str
    label_plural: str
    key_prefix: str
    fields: List[Dict[str, Any]]
    child_relationships: List[Dict[str, Any]]
    record_type_infos: List[Dict[str, Any]]
    is_custom: bool
    is_createable: bool
    is_updateable: bool
    is_deletable: bool
    is_queryable: bool
    raw_data: Dict[str, Any] = field(default_factory=dict)

    @classmethod
    def from_response(cls, data: Dict[str, Any]) -> "SObjectDescribe":
        """Cria SObjectDescribe a partir da resposta da API"""
        return cls(
            name=data.get("name", ""),
            label=data.get("label", ""),
            label_plural=data.get("labelPlural", ""),
            key_prefix=data.get("keyPrefix", ""),
            fields=data.get("fields", []),
            child_relationships=data.get("childRelationships", []),
            record_type_infos=data.get("recordTypeInfos", []),
            is_custom=data.get("custom", False),
            is_createable=data.get("createable", False),
            is_updateable=data.get("updateable", False),
            is_deletable=data.get("deletable", False),
            is_queryable=data.get("queryable", False),
            raw_data=data
        )


class SalesforceClient:
    """
    Cliente REST API do Salesforce

    Fornece interface completa para interacao com Salesforce REST API,
    incluindo autenticacao, CRUD, consultas SOQL e muito mais.

    Suporta isolamento multi-tenant com cache de tokens por tenant
    e audit logging de todas as operacoes.
    """

    def __init__(self, config: SalesforceConfig):
        """
        Inicializa o cliente Salesforce

        Args:
            config: Configuracao de conexao (deve incluir tenant_id)
        """
        self.config = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._connected = False
        self._limits: Dict[str, Any] = {}

        # Tenant isolation (Issue #314)
        self._token_cache = TenantTokenCache.get_instance()
        self._audit_logger = AuditLogger.get_instance()

    @property
    def tenant_id(self) -> str:
        """ID do tenant para isolamento"""
        return self.config.tenant_id

    @property
    def is_connected(self) -> bool:
        """Verifica se esta conectado"""
        return self._connected and self.config.access_token is not None

    @property
    def headers(self) -> Dict[str, str]:
        """Headers padrao para requisicoes"""
        headers = {
            "Content-Type": "application/json",
            "Accept": "application/json"
        }
        if self.config.access_token:
            headers["Authorization"] = f"Bearer {self.config.access_token}"
        headers.update(self.config.custom_headers)
        return headers

    async def __aenter__(self):
        """Context manager entry"""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        await self.close()

    async def _get_session(self) -> aiohttp.ClientSession:
        """Obtem ou cria sessao HTTP"""
        if self._session is None or self._session.closed:
            connector = aiohttp.TCPConnector(
                ssl=self.config.verify_ssl,
                limit=100
            )
            timeout = aiohttp.ClientTimeout(total=self.config.timeout)
            self._session = aiohttp.ClientSession(
                connector=connector,
                timeout=timeout
            )
        return self._session

    async def close(self):
        """Fecha a sessao HTTP"""
        if self._session and not self._session.closed:
            await self._session.close()
        self._session = None
        self._connected = False

    # ==================== AUTENTICACAO ====================

    async def connect(self) -> bool:
        """
        Estabelece conexao com Salesforce

        Tenta autenticar usando o metodo configurado
        (Username/Password ou OAuth).

        Utiliza cache de token por tenant para evitar autenticacoes
        desnecessarias e melhorar performance.

        Returns:
            True se conectou com sucesso

        Raises:
            AuthenticationError: Se falhar na autenticacao
        """
        start_time = time.time()

        try:
            self.config.validate()

            # Verificar se existe token em cache para este tenant
            cached_token = self._token_cache.get_token(self.tenant_id)
            if cached_token:
                self.config.access_token = cached_token.access_token
                self.config.instance_url = cached_token.instance_url
                self.config.user_id = cached_token.user_id
                self.config.organization_id = cached_token.organization_id
                self._connected = True

                self._audit_logger.log_operation(
                    tenant_id=self.tenant_id,
                    operation="CONNECT_CACHED",
                    resource="session",
                    success=True,
                    user_id=self.config.user_id,
                    duration_ms=int((time.time() - start_time) * 1000)
                )

                logger.info(f"Usando token em cache para tenant {self.tenant_id}")
                return True

            # Autenticar com Salesforce
            if self.config.auth_type == AuthenticationType.OAUTH_WEB_SERVER:
                await self._authenticate_oauth()
            else:
                await self._authenticate_soap()

            # Armazenar token no cache por tenant
            self._token_cache.set_token(
                tenant_id=self.tenant_id,
                access_token=self.config.access_token,
                instance_url=self.config.instance_url,
                user_id=self.config.user_id,
                organization_id=self.config.organization_id
            )

            self._connected = True

            self._audit_logger.log_operation(
                tenant_id=self.tenant_id,
                operation="CONNECT",
                resource="session",
                success=True,
                user_id=self.config.user_id,
                duration_ms=int((time.time() - start_time) * 1000)
            )

            logger.info(f"Conectado ao Salesforce para tenant {self.tenant_id}: {self.config.instance_url}")
            return True

        except Exception as e:
            logger.error(f"Falha na autenticacao para tenant {self.tenant_id}: {e}")
            self._connected = False

            self._audit_logger.log_operation(
                tenant_id=self.tenant_id,
                operation="CONNECT",
                resource="session",
                success=False,
                error=str(e),
                duration_ms=int((time.time() - start_time) * 1000)
            )

            raise AuthenticationError(str(e))

    async def _authenticate_soap(self):
        """Autentica via SOAP (Username/Password + Security Token)"""
        soap_url = self.config.soap_url

        # Montar envelope SOAP
        soap_envelope = f"""<?xml version="1.0" encoding="utf-8"?>
        <env:Envelope xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                      xmlns:env="http://schemas.xmlsoap.org/soap/envelope/">
            <env:Body>
                <n1:login xmlns:n1="urn:partner.soap.sforce.com">
                    <n1:username>{self.config.username}</n1:username>
                    <n1:password>{self.config.password}{self.config.security_token}</n1:password>
                </n1:login>
            </env:Body>
        </env:Envelope>"""

        headers = {
            "Content-Type": "text/xml; charset=UTF-8",
            "SOAPAction": "login"
        }

        session = await self._get_session()
        async with session.post(soap_url, data=soap_envelope, headers=headers) as response:
            response_text = await response.text()

            if response.status != 200:
                # Extrair mensagem de erro do SOAP
                error_msg = self._parse_soap_error(response_text)
                raise AuthenticationError(error_msg or f"HTTP {response.status}")

            # Parsear resposta SOAP
            self._parse_login_response(response_text)

    def _parse_soap_error(self, response_text: str) -> Optional[str]:
        """Extrai mensagem de erro de resposta SOAP"""
        try:
            root = ET.fromstring(response_text)
            # Procurar por faultstring
            for elem in root.iter():
                if "faultstring" in elem.tag.lower():
                    return elem.text
        except Exception:
            pass
        return None

    def _parse_login_response(self, response_text: str):
        """Parseia resposta de login SOAP"""
        try:
            root = ET.fromstring(response_text)

            # Namespaces comuns do Salesforce
            ns = {
                'sf': 'urn:partner.soap.sforce.com',
                'soapenv': 'http://schemas.xmlsoap.org/soap/envelope/'
            }

            # Tentar encontrar elementos com ou sem namespace
            session_id = None
            server_url = None
            user_id = None
            org_id = None

            for elem in root.iter():
                tag_name = elem.tag.split('}')[-1].lower()

                if tag_name == 'sessionid' and elem.text:
                    session_id = elem.text
                elif tag_name == 'serverurl' and elem.text:
                    server_url = elem.text
                elif tag_name == 'userid' and elem.text:
                    user_id = elem.text
                elif tag_name == 'organizationid' and elem.text:
                    org_id = elem.text

            if not session_id:
                raise AuthenticationError("Session ID nao encontrado na resposta")

            # Extrair instance URL do server URL
            # server_url e algo como: https://na1.salesforce.com/services/Soap/u/59.0/00D...
            if server_url:
                parts = server_url.split('/services/')
                instance_url = parts[0]
            else:
                instance_url = self.config.login_url

            self.config.access_token = session_id
            self.config.session_id = session_id
            self.config.instance_url = instance_url
            self.config.user_id = user_id
            self.config.organization_id = org_id

        except ET.ParseError as e:
            raise AuthenticationError(f"Erro ao parsear resposta SOAP: {e}")

    async def _authenticate_oauth(self):
        """Autentica via OAuth 2.0"""
        oauth = self.config.oauth_config

        if oauth.refresh_token:
            # Tentar refresh primeiro
            try:
                await self._refresh_oauth_token()
                return
            except Exception:
                logger.warning("Refresh token falhou, tentando nova autenticacao")

        # Se nao tem refresh token, precisa do authorization code
        if not oauth.access_token:
            raise AuthenticationError(
                "OAuth requer autorizacao previa. "
                "Use oauth_config.get_authorization_url() para obter o codigo."
            )

    async def exchange_oauth_code(self, code: str) -> Dict[str, Any]:
        """
        Troca codigo de autorizacao por tokens OAuth

        Args:
            code: Codigo recebido no callback OAuth

        Returns:
            Dict com access_token, refresh_token, instance_url
        """
        oauth = self.config.oauth_config
        if not oauth:
            raise AuthenticationError("OAuth nao configurado")

        data = {
            "grant_type": "authorization_code",
            "code": code,
            "client_id": oauth.client_id,
            "client_secret": oauth.client_secret,
            "redirect_uri": oauth.redirect_uri
        }

        session = await self._get_session()
        async with session.post(oauth.token_url, data=data) as response:
            result = await response.json()

            if response.status != 200:
                raise AuthenticationError(
                    result.get("error_description", result.get("error", "OAuth falhou"))
                )

            # Armazenar tokens
            oauth.access_token = result["access_token"]
            oauth.refresh_token = result.get("refresh_token")
            oauth.instance_url = result["instance_url"]

            self.config.access_token = oauth.access_token
            self.config.instance_url = oauth.instance_url

            return result

    async def _refresh_oauth_token(self):
        """Renova access token usando refresh token"""
        oauth = self.config.oauth_config
        if not oauth or not oauth.refresh_token:
            raise AuthenticationError("Refresh token nao disponivel")

        data = {
            "grant_type": "refresh_token",
            "refresh_token": oauth.refresh_token,
            "client_id": oauth.client_id,
            "client_secret": oauth.client_secret
        }

        session = await self._get_session()
        async with session.post(oauth.token_url, data=data) as response:
            result = await response.json()

            if response.status != 200:
                raise AuthenticationError(
                    result.get("error_description", "Refresh falhou")
                )

            oauth.access_token = result["access_token"]
            oauth.instance_url = result["instance_url"]

            self.config.access_token = oauth.access_token
            self.config.instance_url = oauth.instance_url

    async def test_connection(self) -> bool:
        """
        Testa a conexao com Salesforce

        Returns:
            True se a conexao esta funcionando
        """
        try:
            if not self.is_connected:
                await self.connect()

            # Fazer uma requisicao simples
            await self.get_limits()
            return True

        except Exception as e:
            logger.error(f"Teste de conexao falhou: {e}")
            return False

    # ==================== OPERACOES REST ====================

    async def _request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict] = None,
        params: Optional[Dict] = None,
        headers: Optional[Dict] = None,
        audit_operation: Optional[str] = None,
        audit_resource: Optional[str] = None
    ) -> Any:
        """
        Faz requisicao para a API REST

        Args:
            method: Metodo HTTP (GET, POST, PATCH, DELETE)
            endpoint: Endpoint da API (ex: /sobjects/Account)
            data: Dados para enviar no body
            params: Parametros de query string
            headers: Headers adicionais
            audit_operation: Tipo de operacao para auditoria
            audit_resource: Recurso para auditoria

        Returns:
            Resposta da API (JSON)

        Raises:
            SalesforceError: Em caso de erro
        """
        if not self.is_connected:
            raise SalesforceError("Nao conectado. Chame connect() primeiro.")

        start_time = time.time()
        url = f"{self.config.rest_url}{endpoint}"
        request_headers = self.headers
        if headers:
            request_headers.update(headers)

        session = await self._get_session()

        for attempt in range(self.config.max_retries):
            try:
                async with session.request(
                    method,
                    url,
                    json=data,
                    params=params,
                    headers=request_headers
                ) as response:
                    # Atualizar limites da API
                    self._update_limits(response.headers)

                    # Rate limit
                    if response.status == 429:
                        retry_after = int(response.headers.get("Retry-After", 60))
                        raise RateLimitError(
                            f"Rate limit excedido. Aguarde {retry_after}s"
                        )

                    # Sessao expirada - invalidar cache e reconectar
                    if response.status == 401:
                        self._token_cache.invalidate_token(self.tenant_id)
                        await self.connect()  # Reconectar
                        continue

                    response_text = await response.text()

                    if response.status >= 400:
                        self._handle_error_response(response.status, response_text)

                    # Log de auditoria para operacao bem sucedida
                    if audit_operation:
                        self._audit_logger.log_operation(
                            tenant_id=self.tenant_id,
                            operation=audit_operation,
                            resource=audit_resource or endpoint,
                            success=True,
                            user_id=self.config.user_id,
                            duration_ms=int((time.time() - start_time) * 1000)
                        )

                    # Resposta vazia (ex: DELETE bem sucedido)
                    if not response_text:
                        return None

                    return json.loads(response_text)

            except (aiohttp.ClientError, asyncio.TimeoutError) as e:
                if attempt < self.config.max_retries - 1:
                    await asyncio.sleep(2 ** attempt)
                    continue

                # Log de auditoria para falha
                if audit_operation:
                    self._audit_logger.log_operation(
                        tenant_id=self.tenant_id,
                        operation=audit_operation,
                        resource=audit_resource or endpoint,
                        success=False,
                        error=str(e),
                        user_id=self.config.user_id,
                        duration_ms=int((time.time() - start_time) * 1000)
                    )

                raise SalesforceError(f"Erro de conexao: {e}")

        raise SalesforceError("Maximo de tentativas excedido")

    def _handle_error_response(self, status: int, response_text: str):
        """Processa resposta de erro"""
        try:
            errors = json.loads(response_text)
            if isinstance(errors, list) and errors:
                error = errors[0]
                raise DMLError(
                    error.get("message", response_text),
                    error.get("errorCode"),
                    error.get("fields", [])
                )
            elif isinstance(errors, dict):
                raise SalesforceError(
                    errors.get("message", response_text),
                    errors.get("errorCode")
                )
        except json.JSONDecodeError:
            pass

        raise SalesforceError(f"HTTP {status}: {response_text}")

    def _update_limits(self, headers: Dict):
        """Atualiza informacoes de limites da API"""
        api_usage = headers.get("Sforce-Limit-Info")
        if api_usage:
            # Formato: api-usage=25/15000
            try:
                usage, limit = api_usage.split("=")[1].split("/")
                self._limits = {
                    "api_calls_used": int(usage),
                    "api_calls_limit": int(limit),
                    "api_calls_remaining": int(limit) - int(usage)
                }
            except Exception:
                pass

    # ==================== SOQL QUERIES ====================

    async def query(self, soql: str, include_deleted: bool = False) -> QueryResult:
        """
        Executa consulta SOQL

        Args:
            soql: Query SOQL
            include_deleted: Incluir registros deletados (queryAll)

        Returns:
            QueryResult com os registros

        Exemplo:
            result = await client.query("SELECT Id, Name FROM Account LIMIT 10")
            for record in result.records:
                print(record["Name"])
        """
        endpoint = "/queryAll" if include_deleted else "/query"
        params = {"q": soql}

        try:
            data = await self._request(
                "GET", endpoint, params=params,
                audit_operation="QUERY",
                audit_resource=soql[:100]  # Primeiros 100 chars da query
            )
            return QueryResult.from_response(data)
        except Exception as e:
            raise QueryError(str(e))

    async def query_all(self, soql: str) -> List[Dict[str, Any]]:
        """
        Executa consulta SOQL e retorna todos os registros

        Automaticamente busca proximas paginas se houver mais registros.

        Args:
            soql: Query SOQL

        Returns:
            Lista com todos os registros
        """
        result = await self.query(soql)
        all_records = result.records

        while not result.done and result.next_records_url:
            data = await self._request("GET", result.next_records_url)
            result = QueryResult.from_response(data)
            all_records.extend(result.records)

        return all_records

    async def query_more(self, next_records_url: str) -> QueryResult:
        """
        Busca proxima pagina de resultados

        Args:
            next_records_url: URL retornada em QueryResult.next_records_url

        Returns:
            QueryResult com proximos registros
        """
        # A URL ja inclui o caminho completo, remover o prefixo
        endpoint = next_records_url.replace(f"/services/data/v{self.config.api_version}", "")
        data = await self._request("GET", endpoint)
        return QueryResult.from_response(data)

    async def search(self, sosl: str) -> List[Dict[str, Any]]:
        """
        Executa busca SOSL

        Args:
            sosl: Query SOSL

        Returns:
            Lista de resultados

        Exemplo:
            results = await client.search("FIND {Acme} IN ALL FIELDS RETURNING Account(Id, Name)")
        """
        params = {"q": sosl}
        data = await self._request("GET", "/search", params=params)
        return data.get("searchRecords", [])

    # ==================== CRUD OPERATIONS ====================

    async def create(self, sobject: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Cria um novo registro

        Args:
            sobject: Nome do objeto (ex: "Account")
            data: Dados do registro

        Returns:
            Dict com id e success

        Exemplo:
            result = await client.create("Account", {"Name": "Nova Empresa"})
            print(f"Criado: {result['id']}")
        """
        endpoint = f"/sobjects/{sobject}"
        result = await self._request(
            "POST", endpoint, data=data,
            audit_operation="CREATE",
            audit_resource=sobject
        )
        return result

    async def get(
        self,
        sobject: str,
        record_id: str,
        fields: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Obtem um registro por ID

        Args:
            sobject: Nome do objeto
            record_id: ID do registro
            fields: Lista de campos a retornar (opcional)

        Returns:
            Dados do registro
        """
        endpoint = f"/sobjects/{sobject}/{record_id}"
        params = None
        if fields:
            params = {"fields": ",".join(fields)}
        return await self._request("GET", endpoint, params=params)

    async def update(
        self,
        sobject: str,
        record_id: str,
        data: Dict[str, Any]
    ) -> bool:
        """
        Atualiza um registro

        Args:
            sobject: Nome do objeto
            record_id: ID do registro
            data: Dados para atualizar

        Returns:
            True se sucesso
        """
        endpoint = f"/sobjects/{sobject}/{record_id}"
        await self._request(
            "PATCH", endpoint, data=data,
            audit_operation="UPDATE",
            audit_resource=f"{sobject}/{record_id}"
        )
        return True

    async def upsert(
        self,
        sobject: str,
        external_id_field: str,
        external_id: str,
        data: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Insere ou atualiza registro baseado em external ID

        Args:
            sobject: Nome do objeto
            external_id_field: Nome do campo external ID
            external_id: Valor do external ID
            data: Dados do registro

        Returns:
            Dict com id, success, created
        """
        endpoint = f"/sobjects/{sobject}/{external_id_field}/{quote(external_id)}"
        result = await self._request("PATCH", endpoint, data=data)
        return result or {"success": True, "created": False}

    async def delete(self, sobject: str, record_id: str) -> bool:
        """
        Deleta um registro

        Args:
            sobject: Nome do objeto
            record_id: ID do registro

        Returns:
            True se sucesso
        """
        endpoint = f"/sobjects/{sobject}/{record_id}"
        await self._request(
            "DELETE", endpoint,
            audit_operation="DELETE",
            audit_resource=f"{sobject}/{record_id}"
        )
        return True

    # ==================== SOBJECT METADATA ====================

    async def describe_global(self) -> Dict[str, Any]:
        """
        Lista todos os objetos disponiveis

        Returns:
            Dict com lista de objetos e suas informacoes basicas
        """
        return await self._request("GET", "/sobjects")

    async def describe(self, sobject: str) -> SObjectDescribe:
        """
        Obtem descricao detalhada de um objeto

        Args:
            sobject: Nome do objeto

        Returns:
            SObjectDescribe com campos, relacionamentos, etc.
        """
        endpoint = f"/sobjects/{sobject}/describe"
        data = await self._request("GET", endpoint)
        return SObjectDescribe.from_response(data)

    async def get_fields(self, sobject: str) -> List[Dict[str, Any]]:
        """
        Lista campos de um objeto

        Args:
            sobject: Nome do objeto

        Returns:
            Lista de campos com detalhes
        """
        describe = await self.describe(sobject)
        return describe.fields

    async def get_record_types(self, sobject: str) -> List[Dict[str, Any]]:
        """
        Lista record types de um objeto

        Args:
            sobject: Nome do objeto

        Returns:
            Lista de record types
        """
        describe = await self.describe(sobject)
        return describe.record_type_infos

    # ==================== COMPOSITE ====================

    async def composite(
        self,
        requests: List[Dict[str, Any]],
        all_or_none: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Executa multiplas operacoes em uma unica requisicao

        Args:
            requests: Lista de sub-requisicoes
            all_or_none: Reverter tudo se alguma falhar

        Returns:
            Lista de resultados

        Exemplo:
            requests = [
                {"method": "POST", "url": "/services/data/v59.0/sobjects/Account",
                 "referenceId": "newAccount", "body": {"Name": "Empresa 1"}},
                {"method": "POST", "url": "/services/data/v59.0/sobjects/Contact",
                 "referenceId": "newContact", "body": {"LastName": "Silva",
                 "AccountId": "@{newAccount.id}"}}
            ]
            results = await client.composite(requests)
        """
        data = {
            "compositeRequest": requests,
            "allOrNone": all_or_none
        }
        result = await self._request("POST", "/composite", data=data)
        return result.get("compositeResponse", [])

    async def composite_batch(
        self,
        requests: List[Dict[str, Any]],
        halt_on_error: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Executa batch de operacoes independentes

        Args:
            requests: Lista de sub-requisicoes
            halt_on_error: Parar no primeiro erro

        Returns:
            Lista de resultados
        """
        data = {
            "batchRequests": requests,
            "haltOnError": halt_on_error
        }
        result = await self._request("POST", "/composite/batch", data=data)
        return result.get("results", [])

    # ==================== UTILITY ====================

    async def get_limits(self) -> Dict[str, Any]:
        """
        Obtem limites da organizacao

        Returns:
            Dict com limites de API, storage, etc.
        """
        return await self._request("GET", "/limits")

    async def get_versions(self) -> List[Dict[str, Any]]:
        """Lista versoes da API disponiveis"""
        session = await self._get_session()
        url = f"{self.config.instance_url}/services/data"
        async with session.get(url, headers=self.headers) as response:
            return await response.json()

    async def get_identity(self) -> Dict[str, Any]:
        """Obtem informacoes do usuario autenticado"""
        session = await self._get_session()
        url = f"{self.config.instance_url}/services/oauth2/userinfo"
        async with session.get(url, headers=self.headers) as response:
            return await response.json()

    def get_api_usage(self) -> Dict[str, Any]:
        """
        Retorna uso atual da API

        Returns:
            Dict com api_calls_used, api_calls_limit, api_calls_remaining
        """
        return self._limits.copy()

    # ==================== RECORDS HELPERS ====================

    async def get_by_external_id(
        self,
        sobject: str,
        field: str,
        value: str
    ) -> Dict[str, Any]:
        """
        Busca registro por External ID

        Args:
            sobject: Nome do objeto
            field: Campo External ID
            value: Valor a buscar

        Returns:
            Dados do registro
        """
        endpoint = f"/sobjects/{sobject}/{field}/{quote(value)}"
        return await self._request("GET", endpoint)

    async def get_deleted(
        self,
        sobject: str,
        start: datetime,
        end: datetime
    ) -> Dict[str, Any]:
        """
        Lista registros deletados em um periodo

        Args:
            sobject: Nome do objeto
            start: Data/hora inicial
            end: Data/hora final

        Returns:
            Dict com deletedRecords e earliestDateAvailable
        """
        endpoint = f"/sobjects/{sobject}/deleted"
        params = {
            "start": start.isoformat(),
            "end": end.isoformat()
        }
        return await self._request("GET", endpoint, params=params)

    async def get_updated(
        self,
        sobject: str,
        start: datetime,
        end: datetime
    ) -> Dict[str, Any]:
        """
        Lista registros atualizados em um periodo

        Args:
            sobject: Nome do objeto
            start: Data/hora inicial
            end: Data/hora final

        Returns:
            Dict com ids e latestDateCovered
        """
        endpoint = f"/sobjects/{sobject}/updated"
        params = {
            "start": start.isoformat(),
            "end": end.isoformat()
        }
        return await self._request("GET", endpoint, params=params)

    async def count(self, sobject: str, where: Optional[str] = None) -> int:
        """
        Conta registros de um objeto

        Args:
            sobject: Nome do objeto
            where: Clausula WHERE opcional

        Returns:
            Numero de registros
        """
        soql = f"SELECT COUNT() FROM {sobject}"
        if where:
            soql += f" WHERE {where}"

        result = await self.query(soql)
        return result.total_size

    # ==================== TENANT ISOLATION (Issue #314) ====================

    def get_audit_logs(
        self,
        operation: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Recupera logs de auditoria para este tenant.

        Args:
            operation: Filtrar por tipo de operacao
            limit: Numero maximo de registros

        Returns:
            Lista de logs de auditoria
        """
        return self._audit_logger.get_logs(
            tenant_id=self.tenant_id,
            operation=operation,
            limit=limit
        )

    def get_token_cache_stats(self) -> Dict[str, Any]:
        """
        Retorna estatisticas do cache de tokens.

        Returns:
            Estatisticas do cache
        """
        return self._token_cache.get_stats()

    def invalidate_cached_token(self):
        """
        Invalida o token em cache para este tenant.

        Util quando as credenciais foram alteradas ou o token
        precisa ser renovado.
        """
        self._token_cache.invalidate_token(self.tenant_id)
        self._connected = False
        logger.info(f"Token invalidado para tenant {self.tenant_id}")

    def clear_audit_logs(self):
        """
        Limpa logs de auditoria para este tenant.
        """
        self._audit_logger.clear_logs(self.tenant_id)
        logger.info(f"Logs de auditoria limpos para tenant {self.tenant_id}")

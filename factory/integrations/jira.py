# -*- coding: utf-8 -*-
"""
Jira Integration Module
=======================
Integracao com Jira via API REST com isolamento por tenant.

Funcionalidades:
- Conectar/desconectar via API REST
- Sincronizar issues <-> stories bidirecionalmente
- Mapear status Jira -> Kanban interno
- Webhook para atualizacoes em tempo real
- Isolamento por tenant com credenciais seguras
- Audit logging com contexto de tenant

Terminal 5 - Issue #314: Tenant isolation for Jira integration.

Configuracao via variaveis de ambiente:
- JIRA_URL: URL base do Jira (ex: https://empresa.atlassian.net)
- JIRA_EMAIL: Email do usuario
- JIRA_API_TOKEN: Token de API
- JIRA_PROJECT_KEY: Chave do projeto padrao (ex: PROJ)
- JIRA_TENANT_ID: ID do tenant
"""

import os
import logging
import aiohttp
import base64
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple
from enum import Enum

from .base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult,
    map_status_to_internal,
    map_status_to_external,
    map_priority_to_internal,
    map_priority_to_external
)

# Import from the new jira config module
from .jira.config import JiraConfig

# Import SecretsManager for secure credential storage
try:
    from .secrets import SecretsManager
    from .secrets.azure_keyvault import SecretType
    SECRETS_AVAILABLE = True
except ImportError:
    SECRETS_AVAILABLE = False

logger = logging.getLogger(__name__)


class JiraIssueType(str, Enum):
    """Tipos de issue no Jira"""
    STORY = "Story"
    TASK = "Task"
    BUG = "Bug"
    EPIC = "Epic"
    SUBTASK = "Sub-task"


# Per-tenant session cache
_tenant_sessions: Dict[str, aiohttp.ClientSession] = {}


@dataclass
class JiraAuditEntry:
    """Entrada de auditoria para operacoes Jira"""
    timestamp: datetime
    tenant_id: str
    operation: str
    resource: str
    success: bool
    details: Dict[str, Any] = field(default_factory=dict)
    error: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "timestamp": self.timestamp.isoformat(),
            "tenant_id": self.tenant_id,
            "operation": self.operation,
            "resource": self.resource,
            "success": self.success,
            "details": self.details,
            "error": self.error
        }


class JiraIntegration(IntegrationBase):
    """
    Integracao com Jira via API REST v3 com isolamento por tenant.

    Cada instancia da integracao e isolada por tenant_id, permitindo
    que multiplos clientes usem a integracao simultaneamente com
    credenciais separadas.

    Exemplo de uso:
    ```python
    config = JiraConfig(
        tenant_id="TENANT-001",
        base_url="https://empresa.atlassian.net",
        email="user@empresa.com",
        api_token="..."
    )
    jira = JiraIntegration(config)

    if await jira.connect():
        result = await jira.sync_from_external("PROJ")
        print(f"Sincronizadas {result.items_synced} issues")
    ```

    Ou usando SecretsManager:
    ```python
    config = JiraConfig(tenant_id="TENANT-001", base_url="...")
    jira = JiraIntegration(config, secrets_manager=secrets_manager)
    # Credenciais serao buscadas automaticamente
    ```
    """

    API_VERSION = "3"

    # Class-level audit log (shared across instances)
    _audit_log: List[JiraAuditEntry] = []
    _max_audit_entries: int = 1000

    def __init__(
        self,
        config: JiraConfig,
        secrets_manager: Optional["SecretsManager"] = None
    ):
        """
        Inicializa a integracao Jira.

        Args:
            config: Configuracao do Jira com tenant_id
            secrets_manager: Opcional - gerenciador de secrets para credenciais
        """
        super().__init__(config)
        self.config: JiraConfig = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_info: Optional[Dict] = None
        self._secrets_manager = secrets_manager

        # Validar tenant_id
        if not self.config.tenant_id:
            logger.warning("JiraIntegration criada sem tenant_id - isolamento nao garantido")

    @property
    def tenant_id(self) -> str:
        """ID do tenant associado a esta integracao"""
        return self.config.tenant_id

    @property
    def base_url(self) -> str:
        """URL base da API"""
        url = self.config.base_url.rstrip("/")
        return f"{url}/rest/api/{self.API_VERSION}"

    @property
    def auth_header(self) -> str:
        """Header de autenticacao Basic Auth"""
        credentials = f"{self.config.email}:{self.config.api_token}"
        encoded = base64.b64encode(credentials.encode()).decode()
        return f"Basic {encoded}"

    async def get_credentials(self) -> Tuple[str, str]:
        """
        Obtem credenciais do SecretsManager ou da configuracao.

        Returns:
            Tuple[email, token]
        """
        if self._secrets_manager and SECRETS_AVAILABLE:
            try:
                email_secret = await self._secrets_manager.get_integration_secret(
                    tenant_id=self.config.tenant_id,
                    integration="jira",
                    secret_type=SecretType.PASSWORD,
                    suffix="email"
                )
                token_secret = await self._secrets_manager.get_integration_secret(
                    tenant_id=self.config.tenant_id,
                    integration="jira",
                    secret_type=SecretType.API_KEY,
                    suffix="token"
                )

                if email_secret and token_secret:
                    self._log_audit("GET_CREDENTIALS", "secrets_manager", True)
                    return email_secret.value, token_secret.value

                logger.warning(
                    f"Credenciais nao encontradas no SecretsManager para tenant {self.config.tenant_id}"
                )
            except Exception as e:
                logger.error(f"Erro ao buscar credenciais do SecretsManager: {e}")
                self._log_audit("GET_CREDENTIALS", "secrets_manager", False, error=str(e))

        # Fallback para configuracao
        return self.config.email, self.config.api_token

    def _get_headers(self) -> Dict[str, str]:
        """
        Retorna headers para requisicoes.

        Inclui X-Tenant-ID para rastreamento.
        """
        headers = {
            "Authorization": self.auth_header,
            "Content-Type": "application/json",
            "Accept": "application/json"
        }

        # Adiciona tenant_id para rastreamento
        if self.config.tenant_id:
            headers["X-Tenant-ID"] = self.config.tenant_id

        return headers

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """
        Garante que existe uma sessao HTTP ativa para o tenant.

        Usa cache de sessoes por tenant para eficiencia.
        """
        global _tenant_sessions

        tenant_key = self.config.tenant_id or "default"

        # Verifica se ja existe sessao para este tenant
        if tenant_key in _tenant_sessions:
            session = _tenant_sessions[tenant_key]
            if not session.closed:
                return session
            # Sessao fechada, remover do cache
            del _tenant_sessions[tenant_key]

        # Cria nova sessao para o tenant
        session = aiohttp.ClientSession(headers=self._get_headers())
        _tenant_sessions[tenant_key] = session

        logger.debug(f"Nova sessao criada para tenant: {tenant_key}")
        return session

    def _log_audit(
        self,
        operation: str,
        resource: str,
        success: bool,
        details: Optional[Dict] = None,
        error: Optional[str] = None
    ):
        """
        Registra entrada de auditoria com contexto de tenant.

        Args:
            operation: Operacao realizada (CONNECT, GET, CREATE, etc)
            resource: Recurso afetado (issue key, project, etc)
            success: Se a operacao foi bem sucedida
            details: Detalhes adicionais
            error: Mensagem de erro se aplicavel
        """
        entry = JiraAuditEntry(
            timestamp=datetime.utcnow(),
            tenant_id=self.config.tenant_id,
            operation=operation,
            resource=resource,
            success=success,
            details=details or {},
            error=error
        )

        JiraIntegration._audit_log.append(entry)

        # Manter apenas as ultimas N entradas
        if len(JiraIntegration._audit_log) > JiraIntegration._max_audit_entries:
            JiraIntegration._audit_log = JiraIntegration._audit_log[-JiraIntegration._max_audit_entries:]

        # Log to standard logger as well
        if success:
            logger.info(
                f"[Tenant:{self.config.tenant_id}] {operation} {resource} - SUCCESS"
            )
        else:
            logger.warning(
                f"[Tenant:{self.config.tenant_id}] {operation} {resource} - FAILED: {error}"
            )

    @classmethod
    def get_audit_log(
        cls,
        tenant_id: Optional[str] = None,
        operation: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retorna log de auditoria filtrado.

        Args:
            tenant_id: Filtrar por tenant
            operation: Filtrar por operacao
            limit: Limite de entradas

        Returns:
            Lista de entradas de auditoria
        """
        logs = cls._audit_log

        if tenant_id:
            logs = [l for l in logs if l.tenant_id == tenant_id]

        if operation:
            logs = [l for l in logs if l.operation == operation]

        return [l.to_dict() for l in logs[-limit:]]

    @classmethod
    async def cleanup_tenant_session(cls, tenant_id: str):
        """
        Limpa sessao de um tenant especifico.

        Args:
            tenant_id: ID do tenant
        """
        global _tenant_sessions
        if tenant_id in _tenant_sessions:
            session = _tenant_sessions[tenant_id]
            if not session.closed:
                await session.close()
            del _tenant_sessions[tenant_id]
            logger.info(f"Sessao do tenant {tenant_id} encerrada")

    @classmethod
    async def cleanup_all_sessions(cls):
        """Limpa todas as sessoes de tenant."""
        global _tenant_sessions
        for tenant_id, session in list(_tenant_sessions.items()):
            if not session.closed:
                await session.close()
        _tenant_sessions.clear()
        logger.info("Todas as sessoes de tenant encerradas")

    async def connect(self) -> bool:
        """
        Conecta ao Jira e valida credenciais.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique tenant_id, URL, email e token."
            self.status = IntegrationStatus.ERROR
            self._log_audit("CONNECT", self.config.base_url, False, error=self._last_error)
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"[Tenant:{self.tenant_id}] Conectando ao Jira: {self.config.base_url}")

        try:
            # Try to get credentials from SecretsManager if available
            if self._secrets_manager:
                email, token = await self.get_credentials()
                if email and token:
                    self.config.email = email
                    self.config.api_token = token

            session = await self._ensure_session()

            # Testa conexao buscando informacoes do usuario
            async with session.get(f"{self.base_url}/myself") as response:
                if response.status == 200:
                    self._user_info = await response.json()
                    self.status = IntegrationStatus.CONNECTED
                    self._log_audit(
                        "CONNECT",
                        self.config.base_url,
                        True,
                        details={"user": self._user_info.get("displayName")}
                    )
                    logger.info(
                        f"[Tenant:{self.tenant_id}] Conectado ao Jira como: "
                        f"{self._user_info.get('displayName')}"
                    )
                    return True
                elif response.status == 401:
                    self._last_error = "Credenciais invalidas"
                elif response.status == 403:
                    self._last_error = "Acesso negado. Verifique permissoes do token."
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        self._log_audit("CONNECT", self.config.base_url, False, error=self._last_error)
        logger.error(f"[Tenant:{self.tenant_id}] Falha ao conectar ao Jira: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """
        Desconecta do Jira e limpa sessao do tenant.

        Returns:
            bool: True se desconectado com sucesso
        """
        # Cleanup tenant-specific session
        await self.cleanup_tenant_session(self.config.tenant_id or "default")

        self._session = None
        self._user_info = None
        self.status = IntegrationStatus.DISCONNECTED

        self._log_audit("DISCONNECT", self.config.base_url, True)
        logger.info(f"[Tenant:{self.tenant_id}] Desconectado do Jira")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o Jira.

        Returns:
            bool: True se a conexao esta funcionando
        """
        try:
            session = await self._ensure_session()
            async with session.get(f"{self.base_url}/myself") as response:
                return response.status == 200
        except Exception:
            return False

    async def get_projects(self) -> List[Dict]:
        """
        Lista projetos disponiveis no Jira.

        Returns:
            List[Dict]: Lista de projetos
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            async with session.get(f"{self.base_url}/project") as response:
                if response.status == 200:
                    return await response.json()
        except Exception as e:
            logger.error(f"Erro ao listar projetos: {e}")

        return []

    async def get_issue(self, issue_key: str) -> Optional[Dict]:
        """
        Busca uma issue especifica.

        Args:
            issue_key: Chave da issue (ex: PROJ-123)

        Returns:
            Dict ou None se nao encontrada
        """
        if not self.is_connected:
            self._log_audit("GET_ISSUE", issue_key, False, error="Not connected")
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}"
            params = {
                "expand": "renderedFields,transitions"
            }

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    self._log_audit("GET_ISSUE", issue_key, True)
                    return await response.json()
                elif response.status == 404:
                    self._log_audit("GET_ISSUE", issue_key, False, error="Not found")
                    logger.warning(f"[Tenant:{self.tenant_id}] Issue nao encontrada: {issue_key}")
        except Exception as e:
            self._log_audit("GET_ISSUE", issue_key, False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao buscar issue {issue_key}: {e}")

        return None

    async def search_issues(
        self,
        project_key: Optional[str] = None,
        jql: Optional[str] = None,
        max_results: int = 50,
        start_at: int = 0
    ) -> List[Dict]:
        """
        Busca issues usando JQL.

        Args:
            project_key: Chave do projeto (opcional se jql fornecido)
            jql: Query JQL customizada
            max_results: Maximo de resultados
            start_at: Offset para paginacao

        Returns:
            List[Dict]: Lista de issues
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()

            if not jql:
                project = project_key or self.config.project_key
                jql = f"project = {project} ORDER BY updated DESC"

            params = {
                "jql": jql,
                "maxResults": max_results,
                "startAt": start_at,
                "expand": "renderedFields"
            }

            async with session.get(f"{self.base_url}/search", params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("issues", [])
        except Exception as e:
            logger.error(f"Erro na busca JQL: {e}")

        return []

    async def create_issue(self, issue_data: Dict) -> Optional[Dict]:
        """
        Cria uma nova issue no Jira.

        Args:
            issue_data: Dados da issue no formato Jira

        Returns:
            Dict com a issue criada ou None
        """
        if not self.is_connected:
            self._log_audit("CREATE_ISSUE", "new", False, error="Not connected")
            return None

        try:
            session = await self._ensure_session()

            async with session.post(f"{self.base_url}/issue", json=issue_data) as response:
                if response.status == 201:
                    result = await response.json()
                    self._log_audit(
                        "CREATE_ISSUE",
                        result.get("key", "unknown"),
                        True,
                        details={"id": result.get("id")}
                    )
                    return result
                else:
                    error = await response.text()
                    self._log_audit("CREATE_ISSUE", "new", False, error=error)
                    logger.error(f"[Tenant:{self.tenant_id}] Erro ao criar issue: {error}")
        except Exception as e:
            self._log_audit("CREATE_ISSUE", "new", False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao criar issue: {e}")

        return None

    async def update_issue(self, issue_key: str, update_data: Dict) -> bool:
        """
        Atualiza uma issue existente.

        Args:
            issue_key: Chave da issue
            update_data: Dados para atualizar

        Returns:
            bool: True se atualizada com sucesso
        """
        if not self.is_connected:
            self._log_audit("UPDATE_ISSUE", issue_key, False, error="Not connected")
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}"

            async with session.put(url, json=update_data) as response:
                success = response.status == 204
                self._log_audit("UPDATE_ISSUE", issue_key, success)
                return success
        except Exception as e:
            self._log_audit("UPDATE_ISSUE", issue_key, False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao atualizar issue {issue_key}: {e}")

        return False

    async def transition_issue(self, issue_key: str, transition_id: str) -> bool:
        """
        Muda o status de uma issue usando transicao.

        Args:
            issue_key: Chave da issue
            transition_id: ID da transicao

        Returns:
            bool: True se transicao realizada
        """
        if not self.is_connected:
            self._log_audit("TRANSITION_ISSUE", issue_key, False, error="Not connected")
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}/transitions"
            data = {"transition": {"id": transition_id}}

            async with session.post(url, json=data) as response:
                success = response.status == 204
                self._log_audit(
                    "TRANSITION_ISSUE",
                    issue_key,
                    success,
                    details={"transition_id": transition_id}
                )
                return success
        except Exception as e:
            self._log_audit("TRANSITION_ISSUE", issue_key, False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro na transicao de {issue_key}: {e}")

        return False

    async def get_transitions(self, issue_key: str) -> List[Dict]:
        """
        Lista transicoes disponiveis para uma issue.

        Args:
            issue_key: Chave da issue

        Returns:
            List[Dict]: Lista de transicoes
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}/transitions"

            async with session.get(url) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("transitions", [])
        except Exception as e:
            logger.error(f"Erro ao listar transicoes: {e}")

        return []

    # =========================================================================
    # Jira Agile API (Sprints, Boards, Epics)
    # Issue #311 - Terminal A
    # =========================================================================

    @property
    def agile_base_url(self) -> str:
        """URL base da API Agile"""
        url = self.config.base_url.rstrip("/")
        return f"{url}/rest/agile/1.0"

    async def get_boards(
        self,
        project_key: Optional[str] = None,
        board_type: Optional[str] = None,
        max_results: int = 50
    ) -> List[Dict]:
        """
        Lista boards do Jira Software.

        Args:
            project_key: Filtrar por projeto
            board_type: Filtrar por tipo (scrum/kanban)
            max_results: Maximo de resultados

        Returns:
            List[Dict]: Lista de boards
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board"
            params = {"maxResults": max_results}

            if project_key:
                params["projectKeyOrId"] = project_key
            if board_type:
                params["type"] = board_type

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    self._log_audit("GET_BOARDS", "boards", True)
                    return data.get("values", [])
                else:
                    error = await response.text()
                    self._log_audit("GET_BOARDS", "boards", False, error=error)
                    logger.error(f"[Tenant:{self.tenant_id}] Erro ao listar boards: {error}")
        except Exception as e:
            self._log_audit("GET_BOARDS", "boards", False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao listar boards: {e}")

        return []

    async def get_board(self, board_id: int) -> Optional[Dict]:
        """
        Busca informacoes de um board especifico.

        Args:
            board_id: ID do board

        Returns:
            Dict ou None se nao encontrado
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    self._log_audit("GET_BOARD", str(board_id), True)
                    return await response.json()
                elif response.status == 404:
                    self._log_audit("GET_BOARD", str(board_id), False, error="Not found")
                    logger.warning(f"[Tenant:{self.tenant_id}] Board nao encontrado: {board_id}")
                else:
                    error = await response.text()
                    self._log_audit("GET_BOARD", str(board_id), False, error=error)
        except Exception as e:
            self._log_audit("GET_BOARD", str(board_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao buscar board {board_id}: {e}")

        return None

    async def get_sprints(
        self,
        board_id: int,
        state: Optional[str] = None,
        max_results: int = 50
    ) -> List[Dict]:
        """
        Lista sprints de um board.

        Args:
            board_id: ID do board
            state: Filtrar por estado (future/active/closed)
            max_results: Maximo de resultados

        Returns:
            List[Dict]: Lista de sprints
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/sprint"
            params = {"maxResults": max_results}

            if state:
                params["state"] = state

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    self._log_audit("GET_SPRINTS", str(board_id), True)
                    return data.get("values", [])
                else:
                    error = await response.text()
                    self._log_audit("GET_SPRINTS", str(board_id), False, error=error)
                    logger.error(f"[Tenant:{self.tenant_id}] Erro ao listar sprints: {error}")
        except Exception as e:
            self._log_audit("GET_SPRINTS", str(board_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao listar sprints: {e}")

        return []

    async def get_sprint(self, sprint_id: int) -> Optional[Dict]:
        """
        Busca informacoes de um sprint especifico.

        Args:
            sprint_id: ID do sprint

        Returns:
            Dict ou None se nao encontrado
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    self._log_audit("GET_SPRINT", str(sprint_id), True)
                    return await response.json()
                elif response.status == 404:
                    self._log_audit("GET_SPRINT", str(sprint_id), False, error="Not found")
                    logger.warning(f"[Tenant:{self.tenant_id}] Sprint nao encontrado: {sprint_id}")
                else:
                    error = await response.text()
                    self._log_audit("GET_SPRINT", str(sprint_id), False, error=error)
        except Exception as e:
            self._log_audit("GET_SPRINT", str(sprint_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao buscar sprint {sprint_id}: {e}")

        return None

    async def get_epics(
        self,
        board_id: Optional[int] = None,
        project_key: Optional[str] = None,
        max_results: int = 50
    ) -> List[Dict]:
        """
        Lista epics.

        Args:
            board_id: ID do board (para API Agile)
            project_key: Chave do projeto (para busca JQL)
            max_results: Maximo de resultados

        Returns:
            List[Dict]: Lista de epics
        """
        if not self.is_connected:
            return []

        try:
            if board_id:
                # Usa API Agile
                session = await self._ensure_session()
                url = f"{self.agile_base_url}/board/{board_id}/epic"
                params = {"maxResults": max_results}

                async with session.get(url, params=params) as response:
                    if response.status == 200:
                        data = await response.json()
                        self._log_audit("GET_EPICS", str(board_id), True)
                        return data.get("values", [])
                    else:
                        error = await response.text()
                        self._log_audit("GET_EPICS", str(board_id), False, error=error)
            else:
                # Usa JQL
                project = project_key or self.config.project_key
                jql = f"project = {project} AND issuetype = Epic ORDER BY created DESC"
                issues = await self.search_issues(jql=jql, max_results=max_results)
                self._log_audit("GET_EPICS", project or "all", True)
                return issues

        except Exception as e:
            self._log_audit("GET_EPICS", str(board_id or project_key), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao listar epics: {e}")

        return []

    async def get_epic(self, epic_key: str) -> Optional[Dict]:
        """
        Busca informacoes de um epic especifico.

        Args:
            epic_key: Chave do epic (ex: PROJ-123)

        Returns:
            Dict ou None se nao encontrado
        """
        if not self.is_connected:
            return None

        # Epics sao issues, entao usa get_issue
        return await self.get_issue(epic_key)

    async def get_backlog(self, board_id: int, max_results: int = 100) -> List[Dict]:
        """
        Lista issues do backlog de um board.

        Args:
            board_id: ID do board
            max_results: Maximo de resultados

        Returns:
            List[Dict]: Lista de issues do backlog
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/backlog"
            params = {"maxResults": max_results}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    self._log_audit("GET_BACKLOG", str(board_id), True)
                    return data.get("issues", [])
                else:
                    error = await response.text()
                    self._log_audit("GET_BACKLOG", str(board_id), False, error=error)
                    logger.error(f"[Tenant:{self.tenant_id}] Erro ao buscar backlog: {error}")
        except Exception as e:
            self._log_audit("GET_BACKLOG", str(board_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao buscar backlog: {e}")

        return []

    async def move_to_sprint(self, sprint_id: int, issue_keys: List[str]) -> bool:
        """
        Move issues para um sprint.

        Args:
            sprint_id: ID do sprint destino
            issue_keys: Lista de chaves de issues

        Returns:
            bool: True se movidas com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"
            data = {"issues": issue_keys}

            async with session.post(url, json=data) as response:
                success = response.status in [200, 204]
                self._log_audit(
                    "MOVE_TO_SPRINT",
                    str(sprint_id),
                    success,
                    details={"issues": issue_keys}
                )
                if not success:
                    error = await response.text()
                    logger.error(f"[Tenant:{self.tenant_id}] Erro ao mover issues: {error}")
                return success
        except Exception as e:
            self._log_audit("MOVE_TO_SPRINT", str(sprint_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao mover issues para sprint: {e}")

        return False

    async def move_to_backlog(self, issue_keys: List[str]) -> bool:
        """
        Move issues para o backlog (remove do sprint).

        Args:
            issue_keys: Lista de chaves de issues

        Returns:
            bool: True se movidas com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/backlog/issue"
            data = {"issues": issue_keys}

            async with session.post(url, json=data) as response:
                success = response.status in [200, 204]
                self._log_audit(
                    "MOVE_TO_BACKLOG",
                    "backlog",
                    success,
                    details={"issues": issue_keys}
                )
                return success
        except Exception as e:
            self._log_audit("MOVE_TO_BACKLOG", "backlog", False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao mover issues para backlog: {e}")

        return False

    async def get_velocity(self, board_id: int, num_sprints: int = 5) -> Dict:
        """
        Calcula velocity baseado em sprints anteriores.

        Args:
            board_id: ID do board
            num_sprints: Numero de sprints para calcular

        Returns:
            Dict com velocity media e dados por sprint
        """
        if not self.is_connected:
            return {"error": "Not connected"}

        try:
            # Lista sprints fechados
            sprints = await self.get_sprints(
                board_id=board_id,
                state="closed",
                max_results=num_sprints
            )

            velocity_data = {
                "board_id": board_id,
                "sprints_analyzed": len(sprints),
                "sprint_velocities": [],
                "average_velocity": 0,
                "total_points": 0
            }

            session = await self._ensure_session()

            for sprint in sprints:
                sprint_id = sprint.get("id")
                sprint_name = sprint.get("name")

                # Busca issues do sprint
                url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"

                async with session.get(url) as response:
                    if response.status == 200:
                        data = await response.json()
                        issues = data.get("issues", [])

                        # Soma story points das issues completas
                        sprint_points = 0
                        for issue in issues:
                            fields = issue.get("fields", {})
                            if fields.get("resolution"):
                                points = fields.get("customfield_10016") or \
                                         fields.get("customfield_10024") or 0
                                sprint_points += int(points) if points else 0

                        velocity_data["sprint_velocities"].append({
                            "sprint_id": sprint_id,
                            "sprint_name": sprint_name,
                            "points": sprint_points
                        })
                        velocity_data["total_points"] += sprint_points

            if velocity_data["sprints_analyzed"] > 0:
                velocity_data["average_velocity"] = round(
                    velocity_data["total_points"] / velocity_data["sprints_analyzed"], 1
                )

            self._log_audit("GET_VELOCITY", str(board_id), True)
            return velocity_data

        except Exception as e:
            self._log_audit("GET_VELOCITY", str(board_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao calcular velocity: {e}")
            return {"error": str(e)}

    async def create_sprint(
        self,
        board_id: int,
        name: str,
        goal: Optional[str] = None,
        start_date: Optional[str] = None,
        end_date: Optional[str] = None
    ) -> Optional[Dict]:
        """
        Cria um novo sprint.

        Args:
            board_id: ID do board
            name: Nome do sprint
            goal: Objetivo do sprint
            start_date: Data de inicio (ISO format)
            end_date: Data de fim (ISO format)

        Returns:
            Dict com sprint criado ou None
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint"

            data = {
                "name": name,
                "originBoardId": board_id
            }

            if goal:
                data["goal"] = goal
            if start_date:
                data["startDate"] = start_date
            if end_date:
                data["endDate"] = end_date

            async with session.post(url, json=data) as response:
                if response.status == 201:
                    sprint = await response.json()
                    self._log_audit(
                        "CREATE_SPRINT",
                        sprint.get("name", name),
                        True,
                        details={"sprint_id": sprint.get("id")}
                    )
                    return sprint
                else:
                    error = await response.text()
                    self._log_audit("CREATE_SPRINT", name, False, error=error)
                    logger.error(f"[Tenant:{self.tenant_id}] Erro ao criar sprint: {error}")
        except Exception as e:
            self._log_audit("CREATE_SPRINT", name, False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao criar sprint: {e}")

        return None

    async def start_sprint(
        self,
        sprint_id: int,
        start_date: Optional[str] = None,
        end_date: Optional[str] = None
    ) -> bool:
        """
        Inicia um sprint.

        Args:
            sprint_id: ID do sprint
            start_date: Data de inicio
            end_date: Data de fim

        Returns:
            bool: True se iniciado com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            data = {"state": "active"}
            if start_date:
                data["startDate"] = start_date
            if end_date:
                data["endDate"] = end_date

            async with session.post(url, json=data) as response:
                success = response.status in [200, 204]
                self._log_audit("START_SPRINT", str(sprint_id), success)
                return success
        except Exception as e:
            self._log_audit("START_SPRINT", str(sprint_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao iniciar sprint {sprint_id}: {e}")

        return False

    async def complete_sprint(self, sprint_id: int) -> bool:
        """
        Completa um sprint.

        Args:
            sprint_id: ID do sprint

        Returns:
            bool: True se completado com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"
            data = {"state": "closed"}

            async with session.post(url, json=data) as response:
                success = response.status in [200, 204]
                self._log_audit("COMPLETE_SPRINT", str(sprint_id), success)
                return success
        except Exception as e:
            self._log_audit("COMPLETE_SPRINT", str(sprint_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao completar sprint {sprint_id}: {e}")

        return False

    async def get_sprint_issues(self, sprint_id: int, max_results: int = 100) -> List[Dict]:
        """
        Lista issues de um sprint.

        Args:
            sprint_id: ID do sprint
            max_results: Maximo de resultados

        Returns:
            List[Dict]: Lista de issues
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"
            params = {"maxResults": max_results}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    self._log_audit("GET_SPRINT_ISSUES", str(sprint_id), True)
                    return data.get("issues", [])
                else:
                    error = await response.text()
                    self._log_audit("GET_SPRINT_ISSUES", str(sprint_id), False, error=error)
        except Exception as e:
            self._log_audit("GET_SPRINT_ISSUES", str(sprint_id), False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao listar issues do sprint: {e}")

        return []

    def _jira_issue_to_story(self, issue: Dict) -> Dict:
        """
        Converte issue Jira para formato Story interno.

        Args:
            issue: Issue no formato Jira

        Returns:
            Dict: Story no formato interno
        """
        fields = issue.get("fields", {})

        # Extrai informacoes basicas
        summary = fields.get("summary", "")
        description = fields.get("description", "")

        # Se descricao e um objeto (Atlassian Document Format), extrai texto
        if isinstance(description, dict):
            description = self._extract_text_from_adf(description)

        # Status
        status_obj = fields.get("status", {})
        jira_status = status_obj.get("name", "To Do")
        internal_status = map_status_to_internal(jira_status, "jira")

        # Prioridade
        priority_obj = fields.get("priority", {})
        jira_priority = priority_obj.get("name", "Medium")
        internal_priority = map_priority_to_internal(jira_priority, "jira")

        # Story Points (campo customizado comum)
        story_points = fields.get("customfield_10016") or fields.get("customfield_10024") or 0

        # Assignee
        assignee_obj = fields.get("assignee", {})
        assignee = assignee_obj.get("displayName") if assignee_obj else None

        # Epic
        epic_link = fields.get("customfield_10014") or fields.get("parent", {}).get("key")

        # Labels como tags
        labels = fields.get("labels", [])

        # Acceptance criteria (pode estar em campo customizado ou na descricao)
        acceptance_criteria = []
        if "acceptance" in description.lower():
            # Tenta extrair criterios de aceite da descricao
            lines = description.split("\n")
            in_ac_section = False
            for line in lines:
                if "acceptance" in line.lower() and "criteria" in line.lower():
                    in_ac_section = True
                    continue
                if in_ac_section:
                    if line.strip().startswith(("-", "*", "+")):
                        acceptance_criteria.append(line.strip().lstrip("-*+ "))
                    elif line.strip() and not line.strip().startswith("#"):
                        break

        return {
            "external_id": issue.get("key"),
            "external_system": "jira",
            "external_url": f"{self.config.base_url}/browse/{issue.get('key')}",
            "tenant_id": self.config.tenant_id,  # Include tenant_id for isolation
            "title": summary,
            "description": description,
            "status": internal_status,
            "priority": internal_priority,
            "story_points": int(story_points) if story_points else 0,
            "assignee": assignee,
            "epic_id": epic_link,
            "tags": labels,
            "acceptance_criteria": acceptance_criteria,
            "category": self._issue_type_to_category(fields.get("issuetype", {}).get("name", "Story")),
            "created_at": fields.get("created"),
            "updated_at": fields.get("updated"),
            "external_data": {
                "issue_type": fields.get("issuetype", {}).get("name"),
                "project_key": fields.get("project", {}).get("key"),
                "reporter": fields.get("reporter", {}).get("displayName") if fields.get("reporter") else None,
                "components": [c.get("name") for c in fields.get("components", [])],
                "fix_versions": [v.get("name") for v in fields.get("fixVersions", [])],
                "tenant_id": self.config.tenant_id  # Also in external_data for reference
            }
        }

    def _story_to_jira_issue(self, story: Dict) -> Dict:
        """
        Converte Story interno para formato Jira.

        Args:
            story: Story no formato interno

        Returns:
            Dict: Issue no formato Jira
        """
        # Monta descricao com formato Atlassian Document
        description_parts = []

        if story.get("description"):
            description_parts.append(story["description"])

        # Adiciona narrativa Agile se disponivel
        if story.get("persona") or story.get("action") or story.get("benefit"):
            narrative = f"\n\n*User Story:*\nComo um {story.get('persona', '[persona]')}, "
            narrative += f"eu quero {story.get('action', '[acao]')}, "
            narrative += f"para que {story.get('benefit', '[beneficio]')}."
            description_parts.append(narrative)

        # Adiciona criterios de aceite
        if story.get("acceptance_criteria"):
            ac_text = "\n\n*Criterios de Aceite:*\n"
            for ac in story["acceptance_criteria"]:
                ac_text += f"* {ac}\n"
            description_parts.append(ac_text)

        # Adiciona Definition of Done
        if story.get("definition_of_done"):
            dod_text = "\n\n*Definition of Done:*\n"
            for dod in story["definition_of_done"]:
                dod_text += f"* {dod}\n"
            description_parts.append(dod_text)

        description = "".join(description_parts)

        # Monta campos da issue
        issue_data = {
            "fields": {
                "project": {"key": self.config.project_key},
                "summary": story.get("title", "")[:255],
                "description": description,
                "issuetype": {"name": self._category_to_issue_type(story.get("category", "feature"))},
                "labels": story.get("tags", [])
            }
        }

        # Adiciona prioridade se disponivel
        if story.get("priority"):
            jira_priority = map_priority_to_external(story["priority"], "jira")
            issue_data["fields"]["priority"] = {"name": jira_priority}

        # Story points (campo customizado - ajuste conforme sua instancia)
        if story.get("story_points"):
            # Tenta os campos customizados mais comuns
            issue_data["fields"]["customfield_10016"] = story["story_points"]

        return issue_data

    def _extract_text_from_adf(self, adf: Dict) -> str:
        """
        Extrai texto de Atlassian Document Format.

        Args:
            adf: Documento no formato ADF

        Returns:
            str: Texto extraido
        """
        if not isinstance(adf, dict):
            return str(adf) if adf else ""

        content = adf.get("content", [])
        text_parts = []

        for node in content:
            if node.get("type") == "paragraph":
                para_text = self._extract_text_from_adf_content(node.get("content", []))
                text_parts.append(para_text)
            elif node.get("type") == "bulletList":
                for item in node.get("content", []):
                    item_text = self._extract_text_from_adf_content(item.get("content", []))
                    text_parts.append(f"* {item_text}")
            elif node.get("type") == "heading":
                heading_text = self._extract_text_from_adf_content(node.get("content", []))
                level = node.get("attrs", {}).get("level", 1)
                text_parts.append(f"{'#' * level} {heading_text}")

        return "\n".join(text_parts)

    def _extract_text_from_adf_content(self, content: List[Dict]) -> str:
        """Extrai texto de uma lista de nodes ADF"""
        text_parts = []
        for node in content:
            if node.get("type") == "text":
                text_parts.append(node.get("text", ""))
            elif node.get("content"):
                text_parts.append(self._extract_text_from_adf_content(node["content"]))
        return "".join(text_parts)

    def _issue_type_to_category(self, issue_type: str) -> str:
        """Converte tipo de issue Jira para categoria interna"""
        mapping = {
            "Story": "feature",
            "Bug": "bug",
            "Task": "feature",
            "Technical Debt": "tech_debt",
            "Spike": "spike",
            "Improvement": "improvement",
            "Epic": "feature"
        }
        return mapping.get(issue_type, "feature")

    def _category_to_issue_type(self, category: str) -> str:
        """Converte categoria interna para tipo de issue Jira"""
        mapping = {
            "feature": "Story",
            "bug": "Bug",
            "tech_debt": "Task",
            "spike": "Task",
            "improvement": "Improvement"
        }
        return mapping.get(category, self.config.default_issue_type)

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """
        Sincroniza stories locais para o Jira.

        Args:
            stories: Lista de stories para sincronizar

        Returns:
            SyncResult: Resultado da sincronizacao
        """
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow()
        )

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado ao Jira")
            return result

        self.status = IntegrationStatus.SYNCING

        for story in stories:
            try:
                external_id = story.get("external_id")

                if external_id and story.get("external_system") == "jira":
                    # Atualiza issue existente
                    issue_data = self._story_to_jira_issue(story)
                    if await self.update_issue(external_id, issue_data):
                        result.items_updated += 1
                    else:
                        result.items_failed += 1
                        result.errors.append(f"Falha ao atualizar {external_id}")
                else:
                    # Cria nova issue
                    issue_data = self._story_to_jira_issue(story)
                    created = await self.create_issue(issue_data)
                    if created:
                        result.items_created += 1
                        result.details[story.get("story_id", "unknown")] = created.get("key")
                    else:
                        result.items_failed += 1
                        result.errors.append(f"Falha ao criar issue para {story.get('story_id')}")

                result.items_synced += 1

            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Erro em {story.get('story_id')}: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def sync_from_external(self, project_id: str) -> SyncResult:
        """
        Sincroniza issues do Jira para stories locais.

        Args:
            project_id: ID do projeto local para associar as stories

        Returns:
            SyncResult: Resultado com stories importadas em details["stories"]
        """
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow(),
            details={"stories": []}
        )

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado ao Jira")
            return result

        self.status = IntegrationStatus.SYNCING

        try:
            # Busca issues do projeto
            issues = await self.search_issues(max_results=100)

            for issue in issues:
                try:
                    story_data = self._jira_issue_to_story(issue)
                    story_data["project_id"] = project_id
                    result.details["stories"].append(story_data)
                    result.items_synced += 1
                except Exception as e:
                    result.items_failed += 1
                    result.errors.append(f"Erro ao converter {issue.get('key')}: {str(e)}")

        except Exception as e:
            result.success = False
            result.errors.append(f"Erro na sincronizacao: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def handle_webhook(
        self,
        payload: Dict,
        expected_tenant_id: Optional[str] = None
    ) -> bool:
        """
        Processa webhook do Jira com validacao de tenant.

        Tipos de eventos suportados:
        - jira:issue_created
        - jira:issue_updated
        - jira:issue_deleted

        Args:
            payload: Payload do webhook
            expected_tenant_id: Tenant ID esperado para validacao

        Returns:
            bool: True se processado com sucesso
        """
        try:
            webhook_event = payload.get("webhookEvent", "")
            issue_data = payload.get("issue", {})

            # Validate tenant_id if provided
            if expected_tenant_id and expected_tenant_id != self.config.tenant_id:
                self._log_audit(
                    "WEBHOOK",
                    webhook_event,
                    False,
                    error=f"Tenant mismatch: expected {expected_tenant_id}, got {self.config.tenant_id}"
                )
                logger.warning(
                    f"[Tenant:{self.tenant_id}] Webhook rejeitado: tenant_id nao corresponde"
                )
                return False

            if not issue_data:
                self._log_audit("WEBHOOK", webhook_event, False, error="No issue data")
                logger.warning(f"[Tenant:{self.tenant_id}] Webhook sem dados de issue")
                return False

            issue_key = issue_data.get("key")

            if webhook_event == "jira:issue_created":
                self._log_audit(
                    "WEBHOOK",
                    issue_key,
                    True,
                    details={"event": "issue_created"}
                )
                logger.info(f"[Tenant:{self.tenant_id}] Webhook: Issue criada {issue_key}")
                # Converte para story e retorna para processamento
                story_data = self._jira_issue_to_story(issue_data)
                # Aqui seria chamado o callback para criar a story localmente
                return True

            elif webhook_event == "jira:issue_updated":
                changelog = payload.get("changelog", {})
                items = changelog.get("items", [])

                # Verifica mudancas de status
                status_change = None
                for item in items:
                    if item.get("field") == "status":
                        status_change = {
                            "from": item.get("fromString"),
                            "to": item.get("toString")
                        }
                        logger.info(
                            f"[Tenant:{self.tenant_id}] Status alterado: "
                            f"{status_change['from']} -> {status_change['to']}"
                        )

                self._log_audit(
                    "WEBHOOK",
                    issue_key,
                    True,
                    details={"event": "issue_updated", "status_change": status_change}
                )
                logger.info(f"[Tenant:{self.tenant_id}] Webhook: Issue atualizada {issue_key}")
                return True

            elif webhook_event == "jira:issue_deleted":
                self._log_audit(
                    "WEBHOOK",
                    issue_key,
                    True,
                    details={"event": "issue_deleted"}
                )
                logger.info(f"[Tenant:{self.tenant_id}] Webhook: Issue deletada {issue_key}")
                return True

            logger.debug(f"[Tenant:{self.tenant_id}] Webhook ignorado: {webhook_event}")
            return True

        except Exception as e:
            self._log_audit("WEBHOOK", "unknown", False, error=str(e))
            logger.error(f"[Tenant:{self.tenant_id}] Erro ao processar webhook: {e}")
            return False

    def get_status(self) -> Dict[str, Any]:
        """Retorna status detalhado da integracao com contexto de tenant"""
        status = super().get_status()
        status.update({
            "system": "jira",
            "tenant_id": self.config.tenant_id,
            "url": self.config.base_url,
            "project_key": self.config.project_key,
            "user": self._user_info.get("displayName") if self._user_info else None,
            "user_email": self._user_info.get("emailAddress") if self._user_info else None,
            "secrets_manager_available": self._secrets_manager is not None
        })
        return status


# Per-tenant instance registry
_jira_instances: Dict[str, JiraIntegration] = {}


def get_jira_integration(tenant_id: Optional[str] = None) -> JiraIntegration:
    """
    Retorna instancia da integracao Jira para um tenant.

    Args:
        tenant_id: ID do tenant (opcional - usa variaveis de ambiente se nao fornecido)

    Returns:
        JiraIntegration para o tenant especificado
    """
    global _jira_instances

    # Get tenant_id from env if not provided
    effective_tenant_id = tenant_id or os.getenv("JIRA_TENANT_ID", "default")

    if effective_tenant_id not in _jira_instances:
        config = JiraConfig.from_env(effective_tenant_id)
        _jira_instances[effective_tenant_id] = JiraIntegration(config)

    return _jira_instances[effective_tenant_id]


def get_jira_integration_for_tenant(
    tenant_id: str,
    config: Optional[JiraConfig] = None,
    secrets_manager: Optional["SecretsManager"] = None
) -> JiraIntegration:
    """
    Cria ou retorna instancia da integracao Jira para um tenant especifico.

    Args:
        tenant_id: ID do tenant (obrigatorio)
        config: Configuracao customizada (opcional)
        secrets_manager: Gerenciador de secrets (opcional)

    Returns:
        JiraIntegration configurada para o tenant
    """
    global _jira_instances

    if tenant_id in _jira_instances:
        return _jira_instances[tenant_id]

    if config is None:
        config = JiraConfig.from_env(tenant_id)
    else:
        config.tenant_id = tenant_id

    integration = JiraIntegration(config, secrets_manager)
    _jira_instances[tenant_id] = integration
    return integration


async def init_jira_integration(
    tenant_id: Optional[str] = None,
    secrets_manager: Optional["SecretsManager"] = None
) -> Optional[JiraIntegration]:
    """
    Inicializa e conecta a integracao Jira para um tenant.

    Args:
        tenant_id: ID do tenant (opcional)
        secrets_manager: Gerenciador de secrets (opcional)

    Returns:
        JiraIntegration conectada ou None
    """
    effective_tenant_id = tenant_id or os.getenv("JIRA_TENANT_ID", "default")

    jira = get_jira_integration_for_tenant(
        effective_tenant_id,
        secrets_manager=secrets_manager
    )

    if jira.config.is_valid() and jira.config.enabled:
        if await jira.connect():
            return jira

    return None


async def cleanup_jira_integrations():
    """Limpa todas as instancias e sessoes de integracao Jira."""
    global _jira_instances

    for tenant_id, integration in list(_jira_instances.items()):
        try:
            await integration.disconnect()
        except Exception as e:
            logger.error(f"Erro ao desconectar tenant {tenant_id}: {e}")

    _jira_instances.clear()
    await JiraIntegration.cleanup_all_sessions()
    logger.info("Todas as integracoes Jira foram limpas")

# -*- coding: utf-8 -*-
"""
GitHub Integration Module - Extended Version
=============================================
Integracao completa com GitHub incluindo webhooks avancados e sync de codigo.

Funcionalidades:
- CRUD de Issues com sincronizacao bidirecional
- Webhooks para push, PR, issues, releases
- Sync automatico de codigo
- Suporte a GitHub Actions
- Isolamento por tenant
- Rate limiting

Configuracao via variaveis de ambiente:
- GITHUB_TOKEN: Personal Access Token
- GITHUB_OWNER: Owner do repositorio
- GITHUB_REPO: Nome do repositorio
- GITHUB_WEBHOOK_SECRET: Secret para validacao de webhooks

Issue #102 - GitHub/GitLab Integration Completa
"""

import os
import hmac
import hashlib
import logging
import asyncio
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Set
from enum import Enum

try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False

from .base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult
)

logger = logging.getLogger(__name__)


class GitHubEventType(str, Enum):
    """Tipos de eventos GitHub webhook"""
    PUSH = "push"
    PULL_REQUEST = "pull_request"
    PULL_REQUEST_REVIEW = "pull_request_review"
    ISSUES = "issues"
    ISSUE_COMMENT = "issue_comment"
    CREATE = "create"
    DELETE = "delete"
    RELEASE = "release"
    WORKFLOW_RUN = "workflow_run"
    WORKFLOW_JOB = "workflow_job"
    CHECK_RUN = "check_run"
    CHECK_SUITE = "check_suite"
    DEPLOYMENT = "deployment"
    DEPLOYMENT_STATUS = "deployment_status"
    REPOSITORY = "repository"
    STAR = "star"
    WATCH = "watch"
    FORK = "fork"
    PING = "ping"


class PullRequestAction(str, Enum):
    """Acoes de Pull Request"""
    OPENED = "opened"
    CLOSED = "closed"
    MERGED = "merged"
    SYNCHRONIZE = "synchronize"
    REOPENED = "reopened"
    EDITED = "edited"
    REVIEW_REQUESTED = "review_requested"
    REVIEW_REQUEST_REMOVED = "review_request_removed"
    LABELED = "labeled"
    UNLABELED = "unlabeled"
    READY_FOR_REVIEW = "ready_for_review"
    CONVERTED_TO_DRAFT = "converted_to_draft"


class IssueAction(str, Enum):
    """Acoes de Issue"""
    OPENED = "opened"
    EDITED = "edited"
    DELETED = "deleted"
    CLOSED = "closed"
    REOPENED = "reopened"
    LABELED = "labeled"
    UNLABELED = "unlabeled"
    ASSIGNED = "assigned"
    UNASSIGNED = "unassigned"
    MILESTONED = "milestoned"
    DEMILESTONED = "demilestoned"


@dataclass
class WebhookEvent:
    """Evento de webhook processado"""
    event_type: GitHubEventType
    action: Optional[str]
    payload: Dict
    delivery_id: str
    timestamp: datetime
    repository: Dict
    sender: Dict

    @property
    def repo_full_name(self) -> str:
        return self.repository.get("full_name", "")

    @property
    def sender_login(self) -> str:
        return self.sender.get("login", "")


@dataclass
class GitHubIntegrationConfig(IntegrationConfig):
    """Configuracao estendida para GitHub"""
    token: str = ""
    owner: str = ""
    repo: str = ""
    webhook_secret: str = ""
    # Sincronizacao
    sync_issues: bool = True
    sync_pull_requests: bool = True
    sync_code: bool = False
    sync_releases: bool = True
    # Labels
    default_labels: List[str] = field(default_factory=list)
    create_labels: bool = True
    status_labels: Dict[str, str] = field(default_factory=lambda: {
        "backlog": "status: backlog",
        "ready": "status: ready",
        "in_progress": "status: in progress",
        "review": "status: review",
        "testing": "status: testing",
        "done": "status: done"
    })
    # Branches
    default_branch: str = "main"
    feature_branch_prefix: str = "feature/"
    # Webhooks
    enabled_events: Set[GitHubEventType] = field(default_factory=lambda: {
        GitHubEventType.PUSH,
        GitHubEventType.PULL_REQUEST,
        GitHubEventType.ISSUES,
        GitHubEventType.ISSUE_COMMENT
    })

    @classmethod
    def from_env(cls) -> "GitHubIntegrationConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("GITHUB_ENABLED", "false").lower() == "true",
            token=os.getenv("GITHUB_TOKEN", ""),
            owner=os.getenv("GITHUB_OWNER", ""),
            repo=os.getenv("GITHUB_REPO", ""),
            webhook_secret=os.getenv("GITHUB_WEBHOOK_SECRET", ""),
            sync_issues=os.getenv("GITHUB_SYNC_ISSUES", "true").lower() == "true",
            sync_pull_requests=os.getenv("GITHUB_SYNC_PRS", "true").lower() == "true",
            sync_code=os.getenv("GITHUB_SYNC_CODE", "false").lower() == "true",
            sync_releases=os.getenv("GITHUB_SYNC_RELEASES", "true").lower() == "true",
            default_branch=os.getenv("GITHUB_DEFAULT_BRANCH", "main"),
            feature_branch_prefix=os.getenv("GITHUB_FEATURE_PREFIX", "feature/"),
            auto_sync=os.getenv("GITHUB_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("GITHUB_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        return bool(self.token and self.owner and self.repo)


class WebhookHandler:
    """
    Handler para webhooks do GitHub.

    Permite registrar callbacks para eventos especificos.

    Exemplo:
    ```python
    handler = WebhookHandler(secret="webhook_secret")

    @handler.on(GitHubEventType.PUSH)
    async def on_push(event: WebhookEvent):
        print(f"Push em {event.repo_full_name}")

    @handler.on(GitHubEventType.PULL_REQUEST, action="opened")
    async def on_pr_opened(event: WebhookEvent):
        print(f"Nova PR: {event.payload['pull_request']['title']}")

    # Processar webhook
    await handler.process(headers, body)
    ```
    """

    def __init__(self, secret: str = ""):
        self.secret = secret
        self._handlers: Dict[str, List[Callable]] = {}

    def on(
        self,
        event_type: GitHubEventType,
        action: Optional[str] = None
    ) -> Callable:
        """
        Decorator para registrar handler de evento.

        Args:
            event_type: Tipo do evento
            action: Acao especifica (opcional)

        Returns:
            Decorator function
        """
        def decorator(func: Callable) -> Callable:
            key = f"{event_type.value}:{action or '*'}"
            if key not in self._handlers:
                self._handlers[key] = []
            self._handlers[key].append(func)
            return func
        return decorator

    def register(
        self,
        event_type: GitHubEventType,
        handler: Callable,
        action: Optional[str] = None
    ):
        """Registra handler programaticamente"""
        key = f"{event_type.value}:{action or '*'}"
        if key not in self._handlers:
            self._handlers[key] = []
        self._handlers[key].append(handler)

    def verify_signature(self, payload: bytes, signature: str) -> bool:
        """
        Verifica assinatura HMAC do webhook.

        Args:
            payload: Body do request em bytes
            signature: Header X-Hub-Signature-256

        Returns:
            True se valido
        """
        if not self.secret:
            logger.warning("Webhook secret nao configurado, pulando verificacao")
            return True

        if not signature:
            return False

        # Formato: sha256=xxxx
        if signature.startswith("sha256="):
            signature = signature[7:]

        expected = hmac.new(
            self.secret.encode(),
            payload,
            hashlib.sha256
        ).hexdigest()

        return hmac.compare_digest(signature, expected)

    async def process(
        self,
        headers: Dict[str, str],
        body: bytes
    ) -> Optional[WebhookEvent]:
        """
        Processa webhook do GitHub.

        Args:
            headers: Headers do request
            body: Body em bytes

        Returns:
            WebhookEvent processado ou None
        """
        # Verificar assinatura
        signature = headers.get("X-Hub-Signature-256", headers.get("x-hub-signature-256", ""))
        if not self.verify_signature(body, signature):
            logger.error("Assinatura de webhook invalida")
            return None

        # Parse evento
        event_name = headers.get("X-GitHub-Event", headers.get("x-github-event", ""))
        delivery_id = headers.get("X-GitHub-Delivery", headers.get("x-github-delivery", ""))

        try:
            import json
            payload = json.loads(body.decode())
        except Exception as e:
            logger.error(f"Erro ao fazer parse do webhook: {e}")
            return None

        try:
            event_type = GitHubEventType(event_name)
        except ValueError:
            logger.warning(f"Evento desconhecido: {event_name}")
            return None

        action = payload.get("action")

        event = WebhookEvent(
            event_type=event_type,
            action=action,
            payload=payload,
            delivery_id=delivery_id,
            timestamp=datetime.utcnow(),
            repository=payload.get("repository", {}),
            sender=payload.get("sender", {})
        )

        # Executar handlers
        await self._dispatch(event)

        return event

    async def _dispatch(self, event: WebhookEvent):
        """Despacha evento para handlers registrados"""
        # Handler especifico (evento:acao)
        specific_key = f"{event.event_type.value}:{event.action}"
        if specific_key in self._handlers:
            for handler in self._handlers[specific_key]:
                try:
                    if asyncio.iscoroutinefunction(handler):
                        await handler(event)
                    else:
                        handler(event)
                except Exception as e:
                    logger.error(f"Erro no handler {handler.__name__}: {e}")

        # Handler generico (evento:*)
        generic_key = f"{event.event_type.value}:*"
        if generic_key in self._handlers:
            for handler in self._handlers[generic_key]:
                try:
                    if asyncio.iscoroutinefunction(handler):
                        await handler(event)
                    else:
                        handler(event)
                except Exception as e:
                    logger.error(f"Erro no handler {handler.__name__}: {e}")


class GitHubFullIntegration(IntegrationBase):
    """
    Integracao completa com GitHub.

    Adiciona sobre a integracao basica:
    - Webhooks avancados para todos os eventos
    - Sync automatico de codigo
    - Suporte a Pull Requests
    - Suporte a Releases
    - Actions/Workflows
    - Rate limiting

    Exemplo:
    ```python
    config = GitHubIntegrationConfig.from_env()
    github = GitHubFullIntegration(config)

    # Registrar handlers de webhook
    @github.webhook_handler.on(GitHubEventType.PUSH)
    async def on_push(event):
        print(f"Push: {len(event.payload['commits'])} commits")

    # Conectar
    await github.connect()

    # Sincronizar issues
    issues = await github.list_issues()

    # Criar PR
    pr = await github.create_pull_request(
        title="Nova feature",
        head="feature/nova",
        base="main",
        body="Descricao da PR"
    )
    ```
    """

    API_VERSION = "2022-11-28"
    BASE_URL = "https://api.github.com"

    def __init__(self, config: GitHubIntegrationConfig):
        super().__init__(config)
        self.config: GitHubIntegrationConfig = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_info: Optional[Dict] = None
        self._repo_info: Optional[Dict] = None
        self._rate_limit: Dict = {}
        self.webhook_handler = WebhookHandler(config.webhook_secret)

    @property
    def repo_url(self) -> str:
        return f"{self.BASE_URL}/repos/{self.config.owner}/{self.config.repo}"

    def _get_headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.config.token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": self.API_VERSION
        }

    async def _ensure_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(headers=self._get_headers())
        return self._session

    async def connect(self) -> bool:
        """Conecta ao GitHub"""
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida"
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao GitHub: {self.config.owner}/{self.config.repo}")

        try:
            session = await self._ensure_session()

            async with session.get(f"{self.BASE_URL}/user") as response:
                if response.status == 200:
                    self._user_info = await response.json()
                    self._update_rate_limit(response.headers)
                elif response.status == 401:
                    self._last_error = "Token invalido"
                    self.status = IntegrationStatus.ERROR
                    return False
                else:
                    self._last_error = f"Erro: {response.status}"
                    self.status = IntegrationStatus.ERROR
                    return False

            async with session.get(self.repo_url) as response:
                if response.status == 200:
                    self._repo_info = await response.json()
                    self._update_rate_limit(response.headers)
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(f"Conectado ao GitHub como {self._user_info.get('login')}")
                    return True
                elif response.status == 404:
                    self._last_error = "Repositorio nao encontrado"
                else:
                    self._last_error = f"Erro ao acessar repo: {response.status}"

        except Exception as e:
            self._last_error = str(e)

        self.status = IntegrationStatus.ERROR
        return False

    async def disconnect(self) -> bool:
        if self._session and not self._session.closed:
            await self._session.close()
        self._session = None
        self._user_info = None
        self._repo_info = None
        self.status = IntegrationStatus.DISCONNECTED
        return True

    async def test_connection(self) -> bool:
        try:
            session = await self._ensure_session()
            async with session.get(self.repo_url) as response:
                return response.status == 200
        except Exception:
            return False

    def _update_rate_limit(self, headers):
        """Atualiza informacoes de rate limit"""
        self._rate_limit = {
            "limit": int(headers.get("X-RateLimit-Limit", 0)),
            "remaining": int(headers.get("X-RateLimit-Remaining", 0)),
            "reset": int(headers.get("X-RateLimit-Reset", 0)),
            "used": int(headers.get("X-RateLimit-Used", 0))
        }

    # ====================
    # Issues
    # ====================

    async def list_issues(
        self,
        state: str = "all",
        labels: Optional[List[str]] = None,
        assignee: Optional[str] = None,
        milestone: Optional[str] = None,
        sort: str = "updated",
        direction: str = "desc",
        per_page: int = 100,
        page: int = 1
    ) -> List[Dict]:
        """Lista issues do repositorio"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        params = {
            "state": state,
            "sort": sort,
            "direction": direction,
            "per_page": min(per_page, 100),
            "page": page
        }

        if labels:
            params["labels"] = ",".join(labels)
        if assignee:
            params["assignee"] = assignee
        if milestone:
            params["milestone"] = milestone

        async with session.get(f"{self.repo_url}/issues", params=params) as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                issues = await response.json()
                return [i for i in issues if "pull_request" not in i]
            return []

    async def get_issue(self, issue_number: int) -> Optional[Dict]:
        """Busca issue especifica"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/issues/{issue_number}") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return None

    async def create_issue(
        self,
        title: str,
        body: Optional[str] = None,
        labels: Optional[List[str]] = None,
        assignees: Optional[List[str]] = None,
        milestone: Optional[int] = None
    ) -> Optional[Dict]:
        """Cria nova issue"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        payload = {"title": title}

        if body:
            payload["body"] = body
        if labels:
            payload["labels"] = labels
        if assignees:
            payload["assignees"] = assignees
        if milestone:
            payload["milestone"] = milestone

        async with session.post(f"{self.repo_url}/issues", json=payload) as response:
            self._update_rate_limit(response.headers)
            if response.status == 201:
                return await response.json()
            return None

    async def update_issue(
        self,
        issue_number: int,
        title: Optional[str] = None,
        body: Optional[str] = None,
        state: Optional[str] = None,
        labels: Optional[List[str]] = None,
        assignees: Optional[List[str]] = None
    ) -> bool:
        """Atualiza issue"""
        if not self.is_connected:
            return False

        session = await self._ensure_session()
        payload = {}

        if title:
            payload["title"] = title
        if body is not None:
            payload["body"] = body
        if state:
            payload["state"] = state
        if labels is not None:
            payload["labels"] = labels
        if assignees is not None:
            payload["assignees"] = assignees

        if not payload:
            return True

        async with session.patch(f"{self.repo_url}/issues/{issue_number}", json=payload) as response:
            self._update_rate_limit(response.headers)
            return response.status == 200

    async def close_issue(self, issue_number: int) -> bool:
        """Fecha issue"""
        return await self.update_issue(issue_number, state="closed")

    async def reopen_issue(self, issue_number: int) -> bool:
        """Reabre issue"""
        return await self.update_issue(issue_number, state="open")

    # ====================
    # Pull Requests
    # ====================

    async def list_pull_requests(
        self,
        state: str = "all",
        head: Optional[str] = None,
        base: Optional[str] = None,
        sort: str = "updated",
        direction: str = "desc",
        per_page: int = 100,
        page: int = 1
    ) -> List[Dict]:
        """Lista Pull Requests"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        params = {
            "state": state,
            "sort": sort,
            "direction": direction,
            "per_page": min(per_page, 100),
            "page": page
        }

        if head:
            params["head"] = head
        if base:
            params["base"] = base

        async with session.get(f"{self.repo_url}/pulls", params=params) as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return []

    async def get_pull_request(self, pr_number: int) -> Optional[Dict]:
        """Busca Pull Request especifica"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/pulls/{pr_number}") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return None

    async def create_pull_request(
        self,
        title: str,
        head: str,
        base: str,
        body: Optional[str] = None,
        draft: bool = False,
        maintainer_can_modify: bool = True
    ) -> Optional[Dict]:
        """
        Cria Pull Request.

        Args:
            title: Titulo da PR
            head: Branch de origem
            base: Branch de destino
            body: Descricao
            draft: Se e draft
            maintainer_can_modify: Se mantenedores podem modificar

        Returns:
            Dict com PR criada
        """
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        payload = {
            "title": title,
            "head": head,
            "base": base,
            "draft": draft,
            "maintainer_can_modify": maintainer_can_modify
        }

        if body:
            payload["body"] = body

        async with session.post(f"{self.repo_url}/pulls", json=payload) as response:
            self._update_rate_limit(response.headers)
            if response.status == 201:
                return await response.json()
            else:
                error = await response.text()
                logger.error(f"Erro ao criar PR: {error}")
            return None

    async def merge_pull_request(
        self,
        pr_number: int,
        commit_title: Optional[str] = None,
        commit_message: Optional[str] = None,
        merge_method: str = "merge"
    ) -> bool:
        """
        Faz merge de Pull Request.

        Args:
            pr_number: Numero da PR
            commit_title: Titulo do commit de merge
            commit_message: Mensagem do commit
            merge_method: merge, squash ou rebase

        Returns:
            True se merge ok
        """
        if not self.is_connected:
            return False

        session = await self._ensure_session()
        payload = {"merge_method": merge_method}

        if commit_title:
            payload["commit_title"] = commit_title
        if commit_message:
            payload["commit_message"] = commit_message

        async with session.put(f"{self.repo_url}/pulls/{pr_number}/merge", json=payload) as response:
            self._update_rate_limit(response.headers)
            return response.status == 200

    async def update_pull_request(
        self,
        pr_number: int,
        title: Optional[str] = None,
        body: Optional[str] = None,
        state: Optional[str] = None,
        base: Optional[str] = None
    ) -> bool:
        """Atualiza Pull Request"""
        if not self.is_connected:
            return False

        session = await self._ensure_session()
        payload = {}

        if title:
            payload["title"] = title
        if body is not None:
            payload["body"] = body
        if state:
            payload["state"] = state
        if base:
            payload["base"] = base

        if not payload:
            return True

        async with session.patch(f"{self.repo_url}/pulls/{pr_number}", json=payload) as response:
            self._update_rate_limit(response.headers)
            return response.status == 200

    async def request_reviewers(
        self,
        pr_number: int,
        reviewers: List[str],
        team_reviewers: Optional[List[str]] = None
    ) -> bool:
        """Solicita revisores para PR"""
        if not self.is_connected:
            return False

        session = await self._ensure_session()
        payload = {"reviewers": reviewers}

        if team_reviewers:
            payload["team_reviewers"] = team_reviewers

        async with session.post(
            f"{self.repo_url}/pulls/{pr_number}/requested_reviewers",
            json=payload
        ) as response:
            self._update_rate_limit(response.headers)
            return response.status == 201

    # ====================
    # Branches
    # ====================

    async def list_branches(self, per_page: int = 100, page: int = 1) -> List[Dict]:
        """Lista branches do repositorio"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        params = {"per_page": min(per_page, 100), "page": page}

        async with session.get(f"{self.repo_url}/branches", params=params) as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return []

    async def get_branch(self, branch: str) -> Optional[Dict]:
        """Busca branch especifica"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/branches/{branch}") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return None

    async def create_branch(self, branch_name: str, from_branch: Optional[str] = None) -> bool:
        """
        Cria nova branch.

        Args:
            branch_name: Nome da nova branch
            from_branch: Branch de origem (default: main)

        Returns:
            True se criada
        """
        if not self.is_connected:
            return False

        from_branch = from_branch or self.config.default_branch

        # Buscar SHA da branch de origem
        source = await self.get_branch(from_branch)
        if not source:
            logger.error(f"Branch de origem nao encontrada: {from_branch}")
            return False

        sha = source.get("commit", {}).get("sha")

        session = await self._ensure_session()
        payload = {
            "ref": f"refs/heads/{branch_name}",
            "sha": sha
        }

        async with session.post(f"{self.repo_url}/git/refs", json=payload) as response:
            self._update_rate_limit(response.headers)
            if response.status == 201:
                return True
            elif response.status == 422:
                # Branch ja existe
                logger.warning(f"Branch ja existe: {branch_name}")
                return True
            return False

    async def delete_branch(self, branch_name: str) -> bool:
        """Deleta branch"""
        if not self.is_connected:
            return False

        session = await self._ensure_session()
        async with session.delete(f"{self.repo_url}/git/refs/heads/{branch_name}") as response:
            self._update_rate_limit(response.headers)
            return response.status == 204

    # ====================
    # Commits
    # ====================

    async def list_commits(
        self,
        sha: Optional[str] = None,
        path: Optional[str] = None,
        author: Optional[str] = None,
        since: Optional[datetime] = None,
        until: Optional[datetime] = None,
        per_page: int = 100,
        page: int = 1
    ) -> List[Dict]:
        """Lista commits"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        params = {"per_page": min(per_page, 100), "page": page}

        if sha:
            params["sha"] = sha
        if path:
            params["path"] = path
        if author:
            params["author"] = author
        if since:
            params["since"] = since.isoformat()
        if until:
            params["until"] = until.isoformat()

        async with session.get(f"{self.repo_url}/commits", params=params) as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return []

    async def get_commit(self, sha: str) -> Optional[Dict]:
        """Busca commit especifico"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/commits/{sha}") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return None

    async def compare_commits(self, base: str, head: str) -> Optional[Dict]:
        """Compara dois commits ou branches"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/compare/{base}...{head}") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return None

    # ====================
    # Releases
    # ====================

    async def list_releases(self, per_page: int = 100, page: int = 1) -> List[Dict]:
        """Lista releases"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        params = {"per_page": min(per_page, 100), "page": page}

        async with session.get(f"{self.repo_url}/releases", params=params) as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return []

    async def get_latest_release(self) -> Optional[Dict]:
        """Busca release mais recente"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/releases/latest") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                return await response.json()
            return None

    async def create_release(
        self,
        tag_name: str,
        name: str,
        body: Optional[str] = None,
        target_commitish: Optional[str] = None,
        draft: bool = False,
        prerelease: bool = False,
        generate_release_notes: bool = True
    ) -> Optional[Dict]:
        """Cria nova release"""
        if not self.is_connected:
            return None

        session = await self._ensure_session()
        payload = {
            "tag_name": tag_name,
            "name": name,
            "draft": draft,
            "prerelease": prerelease,
            "generate_release_notes": generate_release_notes
        }

        if body:
            payload["body"] = body
        if target_commitish:
            payload["target_commitish"] = target_commitish

        async with session.post(f"{self.repo_url}/releases", json=payload) as response:
            self._update_rate_limit(response.headers)
            if response.status == 201:
                return await response.json()
            return None

    # ====================
    # Workflows
    # ====================

    async def list_workflows(self) -> List[Dict]:
        """Lista workflows (GitHub Actions)"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        async with session.get(f"{self.repo_url}/actions/workflows") as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                data = await response.json()
                return data.get("workflows", [])
            return []

    async def list_workflow_runs(
        self,
        workflow_id: Optional[int] = None,
        status: Optional[str] = None,
        branch: Optional[str] = None,
        per_page: int = 30,
        page: int = 1
    ) -> List[Dict]:
        """Lista execucoes de workflow"""
        if not self.is_connected:
            return []

        session = await self._ensure_session()
        params = {"per_page": min(per_page, 100), "page": page}

        if status:
            params["status"] = status
        if branch:
            params["branch"] = branch

        url = f"{self.repo_url}/actions/runs"
        if workflow_id:
            url = f"{self.repo_url}/actions/workflows/{workflow_id}/runs"

        async with session.get(url, params=params) as response:
            self._update_rate_limit(response.headers)
            if response.status == 200:
                data = await response.json()
                return data.get("workflow_runs", [])
            return []

    async def trigger_workflow(
        self,
        workflow_id: int,
        ref: str,
        inputs: Optional[Dict] = None
    ) -> bool:
        """
        Dispara execucao de workflow.

        Args:
            workflow_id: ID do workflow
            ref: Branch ou tag
            inputs: Inputs do workflow

        Returns:
            True se disparado
        """
        if not self.is_connected:
            return False

        session = await self._ensure_session()
        payload = {"ref": ref}

        if inputs:
            payload["inputs"] = inputs

        async with session.post(
            f"{self.repo_url}/actions/workflows/{workflow_id}/dispatches",
            json=payload
        ) as response:
            self._update_rate_limit(response.headers)
            return response.status == 204

    # ====================
    # Sync Methods
    # ====================

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Sincroniza stories para GitHub Issues"""
        result = SyncResult(success=True, started_at=datetime.utcnow())

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado")
            return result

        self.status = IntegrationStatus.SYNCING

        for story in stories:
            try:
                external_id = story.get("external_id")

                if external_id and story.get("external_system") == "github":
                    # Atualizar
                    success = await self.update_issue(
                        int(external_id),
                        title=story.get("title"),
                        body=story.get("description"),
                        labels=story.get("tags")
                    )
                    if success:
                        result.items_updated += 1
                    else:
                        result.items_failed += 1
                else:
                    # Criar
                    created = await self.create_issue(
                        title=story.get("title", ""),
                        body=story.get("description"),
                        labels=story.get("tags")
                    )
                    if created:
                        result.items_created += 1
                        result.details[story.get("story_id", "unknown")] = created.get("number")
                    else:
                        result.items_failed += 1

                result.items_synced += 1

            except Exception as e:
                result.items_failed += 1
                result.errors.append(f"Erro em {story.get('story_id')}: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def sync_from_external(self, project_id: str) -> SyncResult:
        """Sincroniza GitHub Issues para stories"""
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow(),
            details={"stories": []}
        )

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado")
            return result

        self.status = IntegrationStatus.SYNCING

        try:
            page = 1
            all_issues = []

            while True:
                issues = await self.list_issues(state="all", per_page=100, page=page)
                if not issues:
                    break

                all_issues.extend(issues)
                page += 1

                if page > 10:
                    result.warnings.append("Limite de 1000 issues atingido")
                    break

            for issue in all_issues:
                try:
                    story_data = self._issue_to_story(issue)
                    story_data["project_id"] = project_id
                    result.details["stories"].append(story_data)
                    result.items_synced += 1
                except Exception as e:
                    result.items_failed += 1
                    result.errors.append(f"Erro na issue {issue.get('number')}: {str(e)}")

        except Exception as e:
            result.success = False
            result.errors.append(str(e))

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    def _issue_to_story(self, issue: Dict) -> Dict:
        """Converte Issue para Story"""
        labels = [label.get("name", "") for label in issue.get("labels", [])]
        assignees = [a.get("login") for a in issue.get("assignees", [])]

        status = "done" if issue.get("state") == "closed" else "backlog"

        return {
            "external_id": str(issue.get("number")),
            "external_system": "github",
            "external_url": issue.get("html_url", ""),
            "title": issue.get("title", ""),
            "description": issue.get("body", "") or "",
            "status": status,
            "priority": "medium",
            "assignee": assignees[0] if assignees else None,
            "tags": labels,
            "created_at": issue.get("created_at"),
            "updated_at": issue.get("updated_at")
        }

    async def handle_webhook(self, payload: Dict) -> bool:
        """Processa webhook (metodo de compatibilidade)"""
        try:
            event_type = GitHubEventType(payload.get("_event_type", "issues"))
        except ValueError:
            event_type = GitHubEventType.ISSUES

        event = WebhookEvent(
            event_type=event_type,
            action=payload.get("action"),
            payload=payload,
            delivery_id="",
            timestamp=datetime.utcnow(),
            repository=payload.get("repository", {}),
            sender=payload.get("sender", {})
        )

        await self.webhook_handler._dispatch(event)
        return True

    def get_status(self) -> Dict[str, Any]:
        status = super().get_status()
        status.update({
            "system": "github",
            "owner": self.config.owner,
            "repo": self.config.repo,
            "user": self._user_info.get("login") if self._user_info else None,
            "rate_limit": self._rate_limit
        })
        return status


# Singleton
_github_full_instance: Optional[GitHubFullIntegration] = None


def get_github_full_integration() -> GitHubFullIntegration:
    """Retorna instancia global"""
    global _github_full_instance
    if _github_full_instance is None:
        config = GitHubIntegrationConfig.from_env()
        _github_full_instance = GitHubFullIntegration(config)
    return _github_full_instance


async def init_github_full_integration() -> Optional[GitHubFullIntegration]:
    """Inicializa e conecta"""
    github = get_github_full_integration()
    if github.config.is_valid() and github.config.enabled:
        if await github.connect():
            return github
    return None

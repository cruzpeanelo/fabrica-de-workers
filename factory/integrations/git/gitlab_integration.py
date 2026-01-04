# -*- coding: utf-8 -*-
"""
GitLab Integration Module
=========================
Integracao completa com GitLab via API REST.

Funcionalidades:
- Sincronizar Issues <-> Stories bidirecionalmente
- Criar MRs quando story for completada
- Webhooks para atualizacoes em tempo real
- Suporte a pipelines CI/CD
- Isolamento por tenant

Configuracao via variaveis de ambiente:
- GITLAB_TOKEN: Personal Access Token ou Project Access Token
- GITLAB_URL: URL do GitLab (padrao: https://gitlab.com)
- GITLAB_PROJECT_ID: ID do projeto
"""

import os
import logging
import aiohttp
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

from .base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult
)

logger = logging.getLogger(__name__)


class GitLabIssueState(str, Enum):
    """Estados de Issue no GitLab"""
    OPENED = "opened"
    CLOSED = "closed"
    ALL = "all"


class GitLabMRState(str, Enum):
    """Estados de Merge Request"""
    OPENED = "opened"
    MERGED = "merged"
    CLOSED = "closed"
    ALL = "all"


# Mapeamento de labels para status interno
LABEL_STATUS_MAPPING = {
    "backlog": "backlog",
    "to do": "ready",
    "ready": "ready",
    "doing": "in_progress",
    "in progress": "in_progress",
    "review": "review",
    "code review": "review",
    "testing": "testing",
    "qa": "testing",
    "done": "done",
    "closed": "done"
}

# Mapeamento de prioridade
PRIORITY_LABELS = {
    "priority::critical": "urgent",
    "priority::high": "high",
    "priority::medium": "medium",
    "priority::low": "low",
    "p1": "urgent",
    "p2": "high",
    "p3": "medium",
    "p4": "low"
}


@dataclass
class GitLabConfig(IntegrationConfig):
    """Configuracao especifica para GitLab"""
    token: str = ""
    url: str = "https://gitlab.com"
    project_id: str = ""
    default_labels: List[str] = field(default_factory=list)
    sync_comments: bool = True
    sync_assignees: bool = True
    create_labels: bool = True
    create_mr_on_complete: bool = True
    source_branch_prefix: str = "feature/"
    target_branch: str = "main"
    status_labels: Dict[str, str] = field(default_factory=lambda: {
        "backlog": "status::backlog",
        "ready": "status::ready",
        "in_progress": "status::doing",
        "review": "status::review",
        "testing": "status::testing",
        "done": "status::done"
    })
    webhook_secret: str = ""

    @classmethod
    def from_env(cls) -> "GitLabConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("GITLAB_ENABLED", "false").lower() == "true",
            token=os.getenv("GITLAB_TOKEN", ""),
            url=os.getenv("GITLAB_URL", "https://gitlab.com"),
            project_id=os.getenv("GITLAB_PROJECT_ID", ""),
            sync_comments=os.getenv("GITLAB_SYNC_COMMENTS", "true").lower() == "true",
            sync_assignees=os.getenv("GITLAB_SYNC_ASSIGNEES", "true").lower() == "true",
            create_labels=os.getenv("GITLAB_CREATE_LABELS", "true").lower() == "true",
            create_mr_on_complete=os.getenv("GITLAB_CREATE_MR", "true").lower() == "true",
            source_branch_prefix=os.getenv("GITLAB_BRANCH_PREFIX", "feature/"),
            target_branch=os.getenv("GITLAB_TARGET_BRANCH", "main"),
            webhook_secret=os.getenv("GITLAB_WEBHOOK_SECRET", ""),
            auto_sync=os.getenv("GITLAB_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("GITLAB_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.token and self.project_id)


class GitLabIntegration(IntegrationBase):
    """
    Integracao completa com GitLab via API REST.

    Exemplo de uso:
    ```python
    config = GitLabConfig.from_env()
    gitlab = GitLabIntegration(config)

    if await gitlab.connect():
        # Sincronizar issues
        result = await gitlab.sync_from_external("MEU-PROJETO")
        print(f"Sincronizadas {result.items_synced} issues")

        # Criar MR para story
        mr = await gitlab.create_mr_for_story(story)
    ```
    """

    API_VERSION = "v4"

    def __init__(self, config: Optional[GitLabConfig] = None, token: str = "", project_id: str = ""):
        """
        Inicializa a integracao GitLab.

        Args:
            config: Configuracao completa (opcional)
            token: Token de acesso (alternativa a config)
            project_id: ID do projeto (alternativa a config)
        """
        if config is None:
            config = GitLabConfig(
                enabled=True,
                token=token,
                project_id=project_id
            )

        super().__init__(config)
        self.config: GitLabConfig = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_info: Optional[Dict] = None
        self._project_info: Optional[Dict] = None

    @property
    def api_url(self) -> str:
        """URL base da API"""
        return f"{self.config.url.rstrip('/')}/api/{self.API_VERSION}"

    @property
    def project_url(self) -> str:
        """URL do projeto na API"""
        return f"{self.api_url}/projects/{self.config.project_id}"

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "PRIVATE-TOKEN": self.config.token,
            "Content-Type": "application/json"
        }

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(headers=self._get_headers())
        return self._session

    async def connect(self) -> bool:
        """Conecta ao GitLab e valida credenciais"""
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique token e project_id."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao GitLab: {self.config.url}, projeto {self.config.project_id}")

        try:
            session = await self._ensure_session()

            # Validar token buscando usuario
            async with session.get(f"{self.api_url}/user") as response:
                if response.status == 200:
                    self._user_info = await response.json()
                elif response.status == 401:
                    self._last_error = "Token invalido ou expirado"
                    self.status = IntegrationStatus.ERROR
                    return False
                else:
                    self._last_error = f"Erro ao autenticar: {response.status}"
                    self.status = IntegrationStatus.ERROR
                    return False

            # Verificar acesso ao projeto
            async with session.get(self.project_url) as response:
                if response.status == 200:
                    self._project_info = await response.json()
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(
                        f"Conectado ao GitLab - Projeto: {self._project_info.get('path_with_namespace')} "
                        f"como {self._user_info.get('username')}"
                    )
                    return True
                elif response.status == 404:
                    self._last_error = f"Projeto {self.config.project_id} nao encontrado"
                elif response.status == 403:
                    self._last_error = "Acesso negado ao projeto"
                else:
                    self._last_error = f"Erro ao acessar projeto: {response.status}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        logger.error(f"Falha ao conectar ao GitLab: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """Desconecta do GitLab"""
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._user_info = None
        self._project_info = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do GitLab")
        return True

    async def test_connection(self) -> bool:
        """Testa a conexao com o GitLab"""
        try:
            session = await self._ensure_session()
            async with session.get(self.project_url) as response:
                return response.status == 200
        except Exception:
            return False

    async def get_issue(self, issue_iid: int) -> Optional[Dict]:
        """Busca uma issue especifica"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/issues/{issue_iid}"

            async with session.get(url) as response:
                if response.status == 200:
                    return await response.json()
                elif response.status == 404:
                    logger.warning(f"Issue nao encontrada: {issue_iid}")
        except Exception as e:
            logger.error(f"Erro ao buscar issue {issue_iid}: {e}")

        return None

    async def list_issues(
        self,
        state: str = "all",
        labels: Optional[List[str]] = None,
        milestone: Optional[str] = None,
        assignee_id: Optional[int] = None,
        sort: str = "updated_at",
        order: str = "desc",
        per_page: int = 100,
        page: int = 1
    ) -> List[Dict]:
        """Lista issues do projeto"""
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/issues"

            params = {
                "state": state,
                "sort": sort,
                "order_by": order,
                "per_page": min(per_page, 100),
                "page": page
            }

            if labels:
                params["labels"] = ",".join(labels)
            if milestone:
                params["milestone"] = milestone
            if assignee_id:
                params["assignee_id"] = assignee_id

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar issues: {error}")

        except Exception as e:
            logger.error(f"Erro ao listar issues: {e}")

        return []

    async def create_issue(
        self,
        title: str,
        description: Optional[str] = None,
        labels: Optional[List[str]] = None,
        assignee_ids: Optional[List[int]] = None,
        milestone_id: Optional[int] = None,
        weight: Optional[int] = None
    ) -> Optional[Dict]:
        """Cria uma nova issue"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/issues"

            payload: Dict[str, Any] = {"title": title}

            if description:
                payload["description"] = description
            if labels:
                payload["labels"] = ",".join(labels)
            if assignee_ids:
                payload["assignee_ids"] = assignee_ids
            if milestone_id:
                payload["milestone_id"] = milestone_id
            if weight:
                payload["weight"] = weight

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar issue: {error}")

        except Exception as e:
            logger.error(f"Erro ao criar issue: {e}")

        return None

    async def update_issue(
        self,
        issue_iid: int,
        title: Optional[str] = None,
        description: Optional[str] = None,
        state_event: Optional[str] = None,
        labels: Optional[List[str]] = None,
        assignee_ids: Optional[List[int]] = None
    ) -> bool:
        """Atualiza uma issue existente"""
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/issues/{issue_iid}"

            payload: Dict[str, Any] = {}

            if title is not None:
                payload["title"] = title
            if description is not None:
                payload["description"] = description
            if state_event is not None:
                payload["state_event"] = state_event
            if labels is not None:
                payload["labels"] = ",".join(labels)
            if assignee_ids is not None:
                payload["assignee_ids"] = assignee_ids

            if not payload:
                return True

            async with session.put(url, json=payload) as response:
                return response.status == 200

        except Exception as e:
            logger.error(f"Erro ao atualizar issue {issue_iid}: {e}")

        return False

    async def add_issue_note(
        self,
        issue_iid: int,
        body: str
    ) -> Optional[Dict]:
        """Adiciona comentario a uma issue"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/issues/{issue_iid}/notes"

            payload = {"body": body}

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()

        except Exception as e:
            logger.error(f"Erro ao adicionar comentario: {e}")

        return None

    async def get_labels(self) -> List[Dict]:
        """Lista labels do projeto"""
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/labels"

            async with session.get(url) as response:
                if response.status == 200:
                    return await response.json()

        except Exception as e:
            logger.error(f"Erro ao listar labels: {e}")

        return []

    async def create_label(
        self,
        name: str,
        color: str = "#428BCA",
        description: Optional[str] = None
    ) -> Optional[Dict]:
        """Cria uma nova label"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/labels"

            payload = {
                "name": name,
                "color": color
            }
            if description:
                payload["description"] = description

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()
                elif response.status == 409:
                    # Label ja existe
                    return {"name": name, "color": color}

        except Exception as e:
            logger.error(f"Erro ao criar label: {e}")

        return None

    async def get_milestones(self, state: str = "all") -> List[Dict]:
        """Lista milestones do projeto"""
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/milestones"
            params = {"state": state}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.json()

        except Exception as e:
            logger.error(f"Erro ao listar milestones: {e}")

        return []

    async def create_branch(
        self,
        branch_name: str,
        ref: str = "main"
    ) -> Optional[Dict]:
        """Cria uma nova branch"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/repository/branches"

            payload = {
                "branch": branch_name,
                "ref": ref
            }

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()
                elif response.status == 400:
                    # Branch ja existe
                    return {"name": branch_name}

        except Exception as e:
            logger.error(f"Erro ao criar branch: {e}")

        return None

    async def create_merge_request(
        self,
        source_branch: str,
        target_branch: str,
        title: str,
        description: Optional[str] = None,
        assignee_id: Optional[int] = None,
        labels: Optional[List[str]] = None,
        remove_source_branch: bool = True
    ) -> Optional[Dict]:
        """Cria um Merge Request"""
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/merge_requests"

            payload = {
                "source_branch": source_branch,
                "target_branch": target_branch,
                "title": title,
                "remove_source_branch": remove_source_branch
            }

            if description:
                payload["description"] = description
            if assignee_id:
                payload["assignee_id"] = assignee_id
            if labels:
                payload["labels"] = ",".join(labels)

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar MR: {error}")

        except Exception as e:
            logger.error(f"Erro ao criar MR: {e}")

        return None

    async def create_mr_for_story(self, story: Dict) -> Optional[Dict]:
        """
        Cria MR quando story for completada.

        Args:
            story: Dados da story completada

        Returns:
            Dict com MR criado ou None
        """
        if not self.config.create_mr_on_complete:
            return None

        story_id = story.get("story_id", "unknown")
        title = story.get("title", "Story")

        # Criar branch
        branch_name = f"{self.config.source_branch_prefix}{story_id.lower().replace('-', '_')}"
        await self.create_branch(branch_name, self.config.target_branch)

        # Criar MR
        mr_title = f"[{story_id}] {title}"
        mr_description = self._generate_mr_description(story)

        return await self.create_merge_request(
            source_branch=branch_name,
            target_branch=self.config.target_branch,
            title=mr_title,
            description=mr_description,
            labels=story.get("tags", [])
        )

    def _generate_mr_description(self, story: Dict) -> str:
        """Gera descricao do MR baseada na story"""
        parts = [f"## Story: {story.get('title', '')}"]

        if story.get("description"):
            parts.append(f"\n{story['description']}")

        if story.get("acceptance_criteria"):
            parts.append("\n## Criterios de Aceite")
            for ac in story["acceptance_criteria"]:
                parts.append(f"- [ ] {ac}")

        parts.append(f"\n\n---\nGerado pela Plataforma E")

        return "\n".join(parts)

    def _issue_to_story(self, issue: Dict) -> Dict:
        """Converte GitLab Issue para formato Story"""
        title = issue.get("title", "")
        description = issue.get("description", "") or ""
        labels = issue.get("labels", [])

        # Determinar status
        internal_status = "backlog"
        if issue.get("state") == "closed":
            internal_status = "done"
        else:
            for label in labels:
                label_lower = label.lower()
                if label_lower in LABEL_STATUS_MAPPING:
                    internal_status = LABEL_STATUS_MAPPING[label_lower]
                    break

        # Determinar prioridade
        internal_priority = "medium"
        for label in labels:
            label_lower = label.lower()
            if label_lower in PRIORITY_LABELS:
                internal_priority = PRIORITY_LABELS[label_lower]
                break

        # Weight como story points
        story_points = issue.get("weight", 0) or 0

        # Assignee
        assignees = issue.get("assignees", [])
        assignee = assignees[0].get("username") if assignees else None

        # Milestone como sprint
        milestone = issue.get("milestone", {})
        sprint_id = milestone.get("iid") if milestone else None

        return {
            "external_id": str(issue.get("iid")),
            "external_system": "gitlab",
            "external_url": issue.get("web_url", ""),
            "title": title,
            "description": description,
            "status": internal_status,
            "priority": internal_priority,
            "story_points": story_points,
            "assignee": assignee,
            "sprint_id": str(sprint_id) if sprint_id else None,
            "tags": [l for l in labels if l.lower() not in LABEL_STATUS_MAPPING and l.lower() not in PRIORITY_LABELS],
            "created_at": issue.get("created_at"),
            "updated_at": issue.get("updated_at"),
            "external_data": {
                "iid": issue.get("iid"),
                "state": issue.get("state"),
                "all_labels": labels,
                "milestone_title": milestone.get("title") if milestone else None,
                "author": issue.get("author", {}).get("username"),
                "due_date": issue.get("due_date"),
                "time_estimate": issue.get("time_stats", {}).get("time_estimate"),
                "total_time_spent": issue.get("time_stats", {}).get("total_time_spent")
            }
        }

    def _story_to_issue(self, story: Dict) -> Dict:
        """Converte Story para formato GitLab Issue"""
        # Montar description
        body_parts = []

        if story.get("description"):
            body_parts.append(story["description"])

        if story.get("persona") or story.get("action") or story.get("benefit"):
            narrative = "\n\n## User Story\n"
            narrative += f"**Como** {story.get('persona', '[persona]')}, "
            narrative += f"**eu quero** {story.get('action', '[acao]')}, "
            narrative += f"**para que** {story.get('benefit', '[beneficio]')}."
            body_parts.append(narrative)

        if story.get("acceptance_criteria"):
            ac_text = "\n\n## Criterios de Aceite\n"
            for ac in story["acceptance_criteria"]:
                ac_text += f"- [ ] {ac}\n"
            body_parts.append(ac_text)

        description = "".join(body_parts)

        # Labels
        labels = list(story.get("tags", []))

        status = story.get("status", "backlog")
        if status in self.config.status_labels:
            labels.append(self.config.status_labels[status])

        priority = story.get("priority", "medium")
        priority_labels = {
            "urgent": "priority::critical",
            "high": "priority::high",
            "medium": "priority::medium",
            "low": "priority::low"
        }
        if priority in priority_labels:
            labels.append(priority_labels[priority])

        labels.extend(self.config.default_labels)
        labels = list(set(labels))

        return {
            "title": story.get("title", "")[:255],
            "description": description,
            "labels": labels,
            "weight": story.get("story_points", 0)
        }

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """Sincroniza stories locais para o GitLab"""
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow()
        )

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado ao GitLab")
            return result

        self.status = IntegrationStatus.SYNCING

        for story in stories:
            try:
                external_id = story.get("external_id")
                issue_data = self._story_to_issue(story)

                if external_id and story.get("external_system") == "gitlab":
                    # Atualizar issue existente
                    if await self.update_issue(
                        int(external_id),
                        title=issue_data.get("title"),
                        description=issue_data.get("description"),
                        labels=issue_data.get("labels")
                    ):
                        result.items_updated += 1
                    else:
                        result.items_failed += 1
                else:
                    # Criar nova issue
                    created = await self.create_issue(
                        title=issue_data.get("title", ""),
                        description=issue_data.get("description"),
                        labels=issue_data.get("labels"),
                        weight=issue_data.get("weight")
                    )

                    if created:
                        result.items_created += 1
                        result.details[story.get("story_id", "unknown")] = created.get("iid")
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
        """Sincroniza Issues do GitLab para stories locais"""
        result = SyncResult(
            success=True,
            started_at=datetime.utcnow(),
            details={"stories": []}
        )

        if not self.is_connected:
            result.success = False
            result.errors.append("Nao conectado ao GitLab")
            return result

        self.status = IntegrationStatus.SYNCING

        try:
            page = 1
            all_issues = []

            while True:
                issues = await self.list_issues(
                    state="all",
                    per_page=100,
                    page=page
                )

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
                    result.errors.append(f"Erro na issue {issue.get('iid')}: {str(e)}")

        except Exception as e:
            result.success = False
            result.errors.append(f"Erro na sincronizacao: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def handle_webhook(self, payload: Dict) -> bool:
        """Processa webhook do GitLab"""
        try:
            object_kind = payload.get("object_kind", "")

            if object_kind == "issue":
                action = payload.get("object_attributes", {}).get("action", "")
                issue = payload.get("object_attributes", {})
                iid = issue.get("iid")

                if action == "open":
                    logger.info(f"Webhook: Issue criada #{iid}")
                elif action == "update":
                    logger.info(f"Webhook: Issue atualizada #{iid}")
                elif action == "close":
                    logger.info(f"Webhook: Issue fechada #{iid}")
                elif action == "reopen":
                    logger.info(f"Webhook: Issue reaberta #{iid}")

                return True

            elif object_kind == "merge_request":
                action = payload.get("object_attributes", {}).get("action", "")
                mr = payload.get("object_attributes", {})
                iid = mr.get("iid")

                if action == "merge":
                    logger.info(f"Webhook: MR merged #{iid}")
                elif action == "open":
                    logger.info(f"Webhook: MR criado #{iid}")

                return True

            elif object_kind == "note":
                note = payload.get("object_attributes", {})
                noteable_type = note.get("noteable_type")
                logger.info(f"Webhook: Comentario em {noteable_type}")
                return True

            elif object_kind == "pipeline":
                status = payload.get("object_attributes", {}).get("status")
                logger.info(f"Webhook: Pipeline {status}")
                return True

            logger.debug(f"Webhook ignorado: {object_kind}")
            return True

        except Exception as e:
            logger.error(f"Erro ao processar webhook: {e}")
            return False

    def get_status(self) -> Dict[str, Any]:
        """Retorna status detalhado da integracao"""
        status = super().get_status()
        status.update({
            "system": "gitlab",
            "url": self.config.url,
            "project_id": self.config.project_id,
            "user": self._user_info.get("username") if self._user_info else None,
            "project_name": self._project_info.get("name") if self._project_info else None,
            "project_url": self._project_info.get("web_url") if self._project_info else None
        })
        return status


# Instancia global (singleton)
_gitlab_instance: Optional[GitLabIntegration] = None


def get_gitlab_integration() -> GitLabIntegration:
    """Retorna instancia global da integracao GitLab"""
    global _gitlab_instance
    if _gitlab_instance is None:
        config = GitLabConfig.from_env()
        _gitlab_instance = GitLabIntegration(config)
    return _gitlab_instance


async def init_gitlab_integration() -> Optional[GitLabIntegration]:
    """Inicializa e conecta a integracao GitLab se configurada"""
    gitlab = get_gitlab_integration()
    if gitlab.config.is_valid() and gitlab.config.enabled:
        if await gitlab.connect():
            return gitlab
    return None

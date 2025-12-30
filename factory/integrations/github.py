# -*- coding: utf-8 -*-
"""
GitHub Issues Integration Module
=================================
Integracao com GitHub Issues via API REST.

Funcionalidades:
- Conectar/desconectar via API REST e Personal Access Token
- Sincronizar Issues <-> Stories bidirecionalmente
- Mapear labels <-> tags
- Mapear milestones <-> sprints
- Mapear projects <-> epics
- Webhook para atualizacoes em tempo real

Configuracao via variaveis de ambiente:
- GITHUB_TOKEN: Personal Access Token
- GITHUB_OWNER: Owner do repositorio (usuario ou organizacao)
- GITHUB_REPO: Nome do repositorio
- GITHUB_PROJECT_NUMBER: Numero do projeto (opcional)
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


class GitHubIssueState(str, Enum):
    """Estados de Issue no GitHub"""
    OPEN = "open"
    CLOSED = "closed"


# Mapeamento de status GitHub -> interno
STATUS_MAPPING_TO_INTERNAL = {
    "open": "backlog",
    "closed": "done"
}

# Labels especiais que indicam status (customizavel)
LABEL_STATUS_MAPPING = {
    "backlog": "backlog",
    "ready": "ready",
    "to do": "ready",
    "in progress": "in_progress",
    "doing": "in_progress",
    "in review": "review",
    "review": "review",
    "testing": "testing",
    "qa": "testing",
    "done": "done",
    "completed": "done"
}

# Mapeamento de prioridade via labels
PRIORITY_LABELS = {
    "priority: critical": "urgent",
    "priority: urgent": "urgent",
    "priority: high": "high",
    "priority: medium": "medium",
    "priority: low": "low",
    "p0": "urgent",
    "p1": "high",
    "p2": "medium",
    "p3": "low"
}


@dataclass
class GitHubConfig(IntegrationConfig):
    """Configuracao especifica para GitHub Issues"""
    token: str = ""
    owner: str = ""
    repo: str = ""
    project_number: Optional[int] = None
    default_labels: List[str] = field(default_factory=list)
    sync_comments: bool = True
    sync_assignees: bool = True
    create_labels: bool = True
    status_labels: Dict[str, str] = field(default_factory=lambda: {
        "backlog": "status: backlog",
        "ready": "status: ready",
        "in_progress": "status: in progress",
        "review": "status: review",
        "testing": "status: testing",
        "done": "status: done"
    })
    webhook_secret: str = ""

    @classmethod
    def from_env(cls) -> "GitHubConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("GITHUB_ENABLED", "false").lower() == "true",
            token=os.getenv("GITHUB_TOKEN", ""),
            owner=os.getenv("GITHUB_OWNER", ""),
            repo=os.getenv("GITHUB_REPO", ""),
            project_number=int(os.getenv("GITHUB_PROJECT_NUMBER", "0")) or None,
            sync_comments=os.getenv("GITHUB_SYNC_COMMENTS", "true").lower() == "true",
            sync_assignees=os.getenv("GITHUB_SYNC_ASSIGNEES", "true").lower() == "true",
            create_labels=os.getenv("GITHUB_CREATE_LABELS", "true").lower() == "true",
            webhook_secret=os.getenv("GITHUB_WEBHOOK_SECRET", ""),
            auto_sync=os.getenv("GITHUB_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("GITHUB_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.token and self.owner and self.repo)


class GitHubIntegration(IntegrationBase):
    """
    Integracao com GitHub Issues via API REST.

    Exemplo de uso:
    ```python
    config = GitHubConfig.from_env()
    github = GitHubIntegration(config)

    if await github.connect():
        result = await github.sync_from_external("MEU-PROJETO")
        print(f"Sincronizadas {result.items_synced} issues")
    ```
    """

    API_VERSION = "2022-11-28"
    BASE_URL = "https://api.github.com"

    def __init__(self, config: GitHubConfig):
        super().__init__(config)
        self.config: GitHubConfig = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_info: Optional[Dict] = None
        self._repo_info: Optional[Dict] = None

    @property
    def repo_url(self) -> str:
        """URL do repositorio na API"""
        return f"{self.BASE_URL}/repos/{self.config.owner}/{self.config.repo}"

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "Authorization": f"Bearer {self.config.token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": self.API_VERSION
        }

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(headers=self._get_headers())
        return self._session

    async def connect(self) -> bool:
        """
        Conecta ao GitHub e valida credenciais.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique token, owner e repo."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao GitHub: {self.config.owner}/{self.config.repo}")

        try:
            session = await self._ensure_session()

            # Valida token buscando usuario autenticado
            async with session.get(f"{self.BASE_URL}/user") as response:
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

            # Verifica acesso ao repositorio
            async with session.get(self.repo_url) as response:
                if response.status == 200:
                    self._repo_info = await response.json()
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(
                        f"Conectado ao GitHub - Repo: {self._repo_info.get('full_name')} "
                        f"como {self._user_info.get('login')}"
                    )
                    return True
                elif response.status == 404:
                    self._last_error = f"Repositorio {self.config.owner}/{self.config.repo} nao encontrado"
                elif response.status == 403:
                    self._last_error = "Acesso negado ao repositorio. Verifique permissoes do token."
                else:
                    self._last_error = f"Erro ao acessar repositorio: {response.status}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        logger.error(f"Falha ao conectar ao GitHub: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """
        Desconecta do GitHub.

        Returns:
            bool: True se desconectado com sucesso
        """
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._user_info = None
        self._repo_info = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do GitHub")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o GitHub.

        Returns:
            bool: True se a conexao esta funcionando
        """
        try:
            session = await self._ensure_session()
            async with session.get(self.repo_url) as response:
                return response.status == 200
        except Exception:
            return False

    async def get_issue(self, issue_number: int) -> Optional[Dict]:
        """
        Busca uma issue especifica.

        Args:
            issue_number: Numero da issue

        Returns:
            Dict ou None se nao encontrada
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/issues/{issue_number}"

            async with session.get(url) as response:
                if response.status == 200:
                    return await response.json()
                elif response.status == 404:
                    logger.warning(f"Issue nao encontrada: {issue_number}")
        except Exception as e:
            logger.error(f"Erro ao buscar issue {issue_number}: {e}")

        return None

    async def list_issues(
        self,
        state: str = "all",
        labels: Optional[List[str]] = None,
        milestone: Optional[str] = None,
        assignee: Optional[str] = None,
        sort: str = "updated",
        direction: str = "desc",
        per_page: int = 100,
        page: int = 1
    ) -> List[Dict]:
        """
        Lista issues do repositorio.

        Args:
            state: open, closed ou all
            labels: Lista de labels para filtrar
            milestone: Milestone para filtrar
            assignee: Assignee para filtrar
            sort: created, updated ou comments
            direction: asc ou desc
            per_page: Issues por pagina (max 100)
            page: Numero da pagina

        Returns:
            List[Dict]: Lista de issues
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/issues"

            params = {
                "state": state,
                "sort": sort,
                "direction": direction,
                "per_page": min(per_page, 100),
                "page": page
            }

            if labels:
                params["labels"] = ",".join(labels)

            if milestone:
                params["milestone"] = milestone

            if assignee:
                params["assignee"] = assignee

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    issues = await response.json()
                    # Filtra Pull Requests (que tambem aparecem como issues)
                    return [i for i in issues if "pull_request" not in i]
                else:
                    error = await response.text()
                    logger.error(f"Erro ao listar issues: {error}")

        except Exception as e:
            logger.error(f"Erro ao listar issues: {e}")

        return []

    async def create_issue(
        self,
        title: str,
        body: Optional[str] = None,
        labels: Optional[List[str]] = None,
        assignees: Optional[List[str]] = None,
        milestone: Optional[int] = None
    ) -> Optional[Dict]:
        """
        Cria uma nova issue.

        Args:
            title: Titulo da issue
            body: Corpo da issue (Markdown)
            labels: Lista de labels
            assignees: Lista de assignees
            milestone: Numero do milestone

        Returns:
            Dict com a issue criada ou None
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/issues"

            payload: Dict[str, Any] = {"title": title}

            if body:
                payload["body"] = body

            if labels:
                payload["labels"] = labels

            if assignees:
                payload["assignees"] = assignees

            if milestone:
                payload["milestone"] = milestone

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
        issue_number: int,
        title: Optional[str] = None,
        body: Optional[str] = None,
        state: Optional[str] = None,
        labels: Optional[List[str]] = None,
        assignees: Optional[List[str]] = None,
        milestone: Optional[int] = None
    ) -> bool:
        """
        Atualiza uma issue existente.

        Args:
            issue_number: Numero da issue
            title: Novo titulo
            body: Novo corpo
            state: Novo estado (open/closed)
            labels: Novas labels (substitui todas)
            assignees: Novos assignees
            milestone: Novo milestone

        Returns:
            bool: True se atualizada com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/issues/{issue_number}"

            payload: Dict[str, Any] = {}

            if title is not None:
                payload["title"] = title

            if body is not None:
                payload["body"] = body

            if state is not None:
                payload["state"] = state

            if labels is not None:
                payload["labels"] = labels

            if assignees is not None:
                payload["assignees"] = assignees

            if milestone is not None:
                payload["milestone"] = milestone

            if not payload:
                return True  # Nada para atualizar

            async with session.patch(url, json=payload) as response:
                return response.status == 200

        except Exception as e:
            logger.error(f"Erro ao atualizar issue {issue_number}: {e}")

        return False

    async def add_issue_comment(
        self,
        issue_number: int,
        body: str
    ) -> Optional[Dict]:
        """
        Adiciona comentario a uma issue.

        Args:
            issue_number: Numero da issue
            body: Corpo do comentario

        Returns:
            Dict com o comentario criado ou None
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/issues/{issue_number}/comments"

            payload = {"body": body}

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro ao adicionar comentario: {error}")

        except Exception as e:
            logger.error(f"Erro ao adicionar comentario: {e}")

        return None

    async def get_issue_comments(self, issue_number: int) -> List[Dict]:
        """
        Lista comentarios de uma issue.

        Args:
            issue_number: Numero da issue

        Returns:
            List[Dict]: Lista de comentarios
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/issues/{issue_number}/comments"

            async with session.get(url) as response:
                if response.status == 200:
                    return await response.json()

        except Exception as e:
            logger.error(f"Erro ao listar comentarios: {e}")

        return []

    async def get_labels(self) -> List[Dict]:
        """
        Lista labels do repositorio.

        Returns:
            List[Dict]: Lista de labels
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/labels"

            async with session.get(url) as response:
                if response.status == 200:
                    return await response.json()

        except Exception as e:
            logger.error(f"Erro ao listar labels: {e}")

        return []

    async def create_label(
        self,
        name: str,
        color: str = "ededed",
        description: Optional[str] = None
    ) -> Optional[Dict]:
        """
        Cria uma nova label.

        Args:
            name: Nome da label
            color: Cor em hexadecimal (sem #)
            description: Descricao da label

        Returns:
            Dict com a label criada ou None
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/labels"

            payload = {
                "name": name,
                "color": color
            }

            if description:
                payload["description"] = description

            async with session.post(url, json=payload) as response:
                if response.status == 201:
                    return await response.json()
                elif response.status == 422:
                    # Label ja existe
                    logger.debug(f"Label ja existe: {name}")
                    return {"name": name, "color": color}

        except Exception as e:
            logger.error(f"Erro ao criar label: {e}")

        return None

    async def get_milestones(self, state: str = "all") -> List[Dict]:
        """
        Lista milestones do repositorio.

        Args:
            state: open, closed ou all

        Returns:
            List[Dict]: Lista de milestones
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.repo_url}/milestones"
            params = {"state": state}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.json()

        except Exception as e:
            logger.error(f"Erro ao listar milestones: {e}")

        return []

    def _issue_to_story(self, issue: Dict) -> Dict:
        """
        Converte GitHub Issue para formato Story interno.

        Args:
            issue: Issue no formato GitHub

        Returns:
            Dict: Story no formato interno
        """
        # Extrai informacoes basicas
        title = issue.get("title", "")
        body = issue.get("body", "") or ""

        # Labels
        labels = [label.get("name", "") for label in issue.get("labels", [])]

        # Determina status baseado em labels ou estado
        internal_status = "backlog"
        if issue.get("state") == "closed":
            internal_status = "done"
        else:
            # Verifica labels de status
            for label in labels:
                label_lower = label.lower()
                if label_lower in LABEL_STATUS_MAPPING:
                    internal_status = LABEL_STATUS_MAPPING[label_lower]
                    break

        # Determina prioridade baseado em labels
        internal_priority = "medium"
        for label in labels:
            label_lower = label.lower()
            if label_lower in PRIORITY_LABELS:
                internal_priority = PRIORITY_LABELS[label_lower]
                break

        # Assignees
        assignees = [a.get("login") for a in issue.get("assignees", [])]
        assignee = assignees[0] if assignees else None

        # Milestone como Sprint
        milestone = issue.get("milestone", {})
        sprint_id = milestone.get("number") if milestone else None
        sprint_name = milestone.get("title") if milestone else None

        # Story points via labels (ex: "points: 5")
        story_points = 0
        for label in labels:
            if label.lower().startswith("points:"):
                try:
                    story_points = int(label.split(":")[1].strip())
                except (ValueError, IndexError):
                    pass
            elif label.lower().startswith("sp:"):
                try:
                    story_points = int(label.split(":")[1].strip())
                except (ValueError, IndexError):
                    pass

        # Filtra labels de sistema (status, priority, points)
        tags = [
            label for label in labels
            if not any([
                label.lower() in LABEL_STATUS_MAPPING,
                label.lower() in PRIORITY_LABELS,
                label.lower().startswith("points:"),
                label.lower().startswith("sp:")
            ])
        ]

        # URL da issue
        issue_url = issue.get("html_url", "")

        # Categoria baseada em labels
        category = "feature"
        for label in labels:
            label_lower = label.lower()
            if label_lower in ["bug", "bugfix", "fix"]:
                category = "bug"
                break
            elif label_lower in ["enhancement", "improvement"]:
                category = "improvement"
                break
            elif label_lower in ["tech-debt", "refactor"]:
                category = "tech_debt"
                break
            elif label_lower in ["spike", "research"]:
                category = "spike"
                break

        # Extrai criterios de aceite do body se seguir formato
        acceptance_criteria = []
        if body:
            lines = body.split("\n")
            in_ac_section = False
            for line in lines:
                line_stripped = line.strip()
                if any(x in line_stripped.lower() for x in ["acceptance criteria", "criterios de aceite"]):
                    in_ac_section = True
                    continue
                if in_ac_section:
                    if line_stripped.startswith(("- [ ]", "- [x]", "* [ ]", "* [x]", "- ", "* ")):
                        # Remove checkbox e markers
                        clean_line = line_stripped.lstrip("- *[]x ").strip()
                        if clean_line:
                            acceptance_criteria.append(clean_line)
                    elif line_stripped.startswith("#"):
                        # Nova secao, sai do modo AC
                        in_ac_section = False

        return {
            "external_id": str(issue.get("number")),
            "external_system": "github",
            "external_url": issue_url,
            "title": title,
            "description": body,
            "status": internal_status,
            "priority": internal_priority,
            "story_points": story_points,
            "assignee": assignee,
            "sprint_id": str(sprint_id) if sprint_id else None,
            "tags": tags,
            "acceptance_criteria": acceptance_criteria,
            "category": category,
            "created_at": issue.get("created_at"),
            "updated_at": issue.get("updated_at"),
            "external_data": {
                "number": issue.get("number"),
                "state": issue.get("state"),
                "all_labels": labels,
                "all_assignees": assignees,
                "milestone_title": sprint_name,
                "comments_count": issue.get("comments", 0),
                "user": issue.get("user", {}).get("login"),
                "locked": issue.get("locked", False),
                "reactions": issue.get("reactions", {})
            }
        }

    def _story_to_issue(self, story: Dict) -> Dict:
        """
        Converte Story interno para formato GitHub Issue.

        Args:
            story: Story no formato interno

        Returns:
            Dict: Dados para criar/atualizar issue
        """
        # Monta body com formato Markdown
        body_parts = []

        # Descricao
        if story.get("description"):
            body_parts.append(story["description"])

        # Narrativa Agile
        if story.get("persona") or story.get("action") or story.get("benefit"):
            narrative = "\n\n## User Story\n"
            narrative += f"**Como** {story.get('persona', '[persona]')}, "
            narrative += f"**eu quero** {story.get('action', '[acao]')}, "
            narrative += f"**para que** {story.get('benefit', '[beneficio]')}."
            body_parts.append(narrative)

        # Criterios de aceite
        if story.get("acceptance_criteria"):
            ac_text = "\n\n## Criterios de Aceite\n"
            for ac in story["acceptance_criteria"]:
                ac_text += f"- [ ] {ac}\n"
            body_parts.append(ac_text)

        # Definition of Done
        if story.get("definition_of_done"):
            dod_text = "\n\n## Definition of Done\n"
            for dod in story["definition_of_done"]:
                dod_text += f"- [ ] {dod}\n"
            body_parts.append(dod_text)

        body = "".join(body_parts)

        # Labels
        labels = list(story.get("tags", []))

        # Adiciona label de status
        status = story.get("status", "backlog")
        if status in self.config.status_labels:
            labels.append(self.config.status_labels[status])

        # Adiciona label de prioridade
        priority = story.get("priority", "medium")
        priority_labels = {
            "urgent": "priority: critical",
            "high": "priority: high",
            "medium": "priority: medium",
            "low": "priority: low"
        }
        if priority in priority_labels:
            labels.append(priority_labels[priority])

        # Adiciona label de story points
        if story.get("story_points"):
            labels.append(f"points: {story['story_points']}")

        # Adiciona label de categoria
        category = story.get("category", "feature")
        category_labels = {
            "feature": "feature",
            "bug": "bug",
            "tech_debt": "tech-debt",
            "spike": "spike",
            "improvement": "enhancement"
        }
        if category in category_labels:
            labels.append(category_labels[category])

        # Adiciona labels padrao
        labels.extend(self.config.default_labels)

        # Remove duplicatas
        labels = list(set(labels))

        issue_data = {
            "title": story.get("title", "")[:255],
            "body": body,
            "labels": labels
        }

        # Assignee
        if story.get("assignee"):
            issue_data["assignees"] = [story["assignee"]]

        return issue_data

    async def _ensure_labels_exist(self, labels: List[str]) -> None:
        """Garante que as labels necessarias existem no repositorio"""
        if not self.config.create_labels:
            return

        existing_labels = await self.get_labels()
        existing_names = {label.get("name", "").lower() for label in existing_labels}

        # Cores por categoria de label
        label_colors = {
            "priority": "d73a4a",    # Vermelho
            "status": "0e8a16",      # Verde
            "points": "1d76db",      # Azul
            "feature": "a2eeef",     # Cyan
            "bug": "d73a4a",         # Vermelho
            "enhancement": "a2eeef", # Cyan
            "tech-debt": "fbca04",   # Amarelo
            "spike": "5319e7"        # Roxo
        }

        for label in labels:
            if label.lower() not in existing_names:
                # Determina cor baseado no prefixo
                color = "ededed"  # Cinza padrao
                for prefix, c in label_colors.items():
                    if label.lower().startswith(prefix) or label.lower() == prefix:
                        color = c
                        break

                await self.create_label(label, color)

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """
        Sincroniza stories locais para o GitHub.

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
            result.errors.append("Nao conectado ao GitHub")
            return result

        self.status = IntegrationStatus.SYNCING

        # Coleta todas as labels necessarias
        all_labels = set()
        for story in stories:
            issue_data = self._story_to_issue(story)
            all_labels.update(issue_data.get("labels", []))

        # Garante que labels existem
        await self._ensure_labels_exist(list(all_labels))

        for story in stories:
            try:
                external_id = story.get("external_id")
                issue_data = self._story_to_issue(story)

                if external_id and story.get("external_system") == "github":
                    # Atualiza issue existente
                    if await self.update_issue(
                        int(external_id),
                        title=issue_data.get("title"),
                        body=issue_data.get("body"),
                        labels=issue_data.get("labels"),
                        assignees=issue_data.get("assignees")
                    ):
                        result.items_updated += 1
                    else:
                        result.items_failed += 1
                        result.errors.append(f"Falha ao atualizar issue {external_id}")
                else:
                    # Cria nova issue
                    created = await self.create_issue(
                        title=issue_data.get("title", ""),
                        body=issue_data.get("body"),
                        labels=issue_data.get("labels"),
                        assignees=issue_data.get("assignees")
                    )

                    if created:
                        result.items_created += 1
                        result.details[story.get("story_id", "unknown")] = created.get("number")
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
        Sincroniza Issues do GitHub para stories locais.

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
            result.errors.append("Nao conectado ao GitHub")
            return result

        self.status = IntegrationStatus.SYNCING

        try:
            # Busca todas as issues (abertas e fechadas)
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

                # Limite de seguranca
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
                    result.errors.append(
                        f"Erro ao converter issue {issue.get('number')}: {str(e)}"
                    )

        except Exception as e:
            result.success = False
            result.errors.append(f"Erro na sincronizacao: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def handle_webhook(self, payload: Dict, project_id: str = None) -> Dict[str, Any]:
        """
        Processa webhook do GitHub e sincroniza com stories locais.

        Issue #162: Implementa callbacks que atualizam dados locais.

        Eventos suportados:
        - issues (opened, edited, deleted, closed, reopened, labeled, unlabeled)
        - issue_comment (created, edited, deleted)
        - pull_request (opened, closed, merged)
        - push (commits)

        Args:
            payload: Payload do webhook
            project_id: ID do projeto local para sincronização

        Returns:
            Dict com resultado do processamento
        """
        try:
            from factory.database.connection import SessionLocal
            from factory.database.models import Story, StoryTask, StoryStatus

            action = payload.get("action", "")
            event_type = payload.get("event", "issues")
            result = {"success": True, "action": action, "changes": []}

            # Issue events
            issue = payload.get("issue", {})
            if issue:
                issue_number = issue.get("number")
                story_data = self._issue_to_story(issue)

                db = SessionLocal()
                try:
                    # Issue #162: Buscar story pelo external_id ou criar nova
                    external_id = f"github:{self.config.owner}/{self.config.repo}#{issue_number}"

                    # Buscar story existente
                    story = db.query(Story).filter(
                        Story.quotas.contains({"external_id": external_id})
                    ).first()

                    if not story and project_id:
                        # Buscar por title match como fallback
                        story = db.query(Story).filter(
                            Story.project_id == project_id,
                            Story.title == story_data["title"]
                        ).first()

                    if action == "opened":
                        logger.info(f"[Webhook] Issue criada #{issue_number}")
                        if not story and project_id:
                            # Criar nova story
                            count = db.query(Story).count()
                            new_story = Story(
                                story_id=f"STR-{count + 1:04d}",
                                project_id=project_id,
                                title=story_data["title"],
                                description=story_data.get("description"),
                                status=story_data.get("status", "backlog"),
                                priority=story_data.get("priority", "medium"),
                                story_points=story_data.get("story_points", 0),
                                assignee=story_data.get("assignee"),
                                tags=story_data.get("tags", []),
                            )
                            db.add(new_story)
                            db.commit()
                            result["changes"].append({"type": "story_created", "story_id": new_story.story_id})
                            logger.info(f"[Webhook] Story criada: {new_story.story_id}")

                    elif action in ["edited", "labeled", "unlabeled", "assigned", "unassigned"]:
                        logger.info(f"[Webhook] Issue atualizada #{issue_number} ({action})")
                        if story:
                            # Atualizar campos da story
                            story.title = story_data["title"]
                            if story_data.get("description"):
                                story.description = story_data["description"]
                            story.status = story_data.get("status", story.status)
                            story.priority = story_data.get("priority", story.priority)
                            story.assignee = story_data.get("assignee")
                            story.tags = story_data.get("tags", story.tags)
                            story.updated_at = datetime.utcnow()
                            db.commit()
                            result["changes"].append({"type": "story_updated", "story_id": story.story_id})
                            logger.info(f"[Webhook] Story atualizada: {story.story_id}")

                    elif action == "closed":
                        logger.info(f"[Webhook] Issue fechada #{issue_number}")
                        if story:
                            story.status = "done"
                            story.completed_at = datetime.utcnow()
                            story.updated_at = datetime.utcnow()
                            db.commit()
                            result["changes"].append({"type": "story_closed", "story_id": story.story_id})
                            logger.info(f"[Webhook] Story fechada: {story.story_id}")

                    elif action == "reopened":
                        logger.info(f"[Webhook] Issue reaberta #{issue_number}")
                        if story:
                            story.status = "in_progress"
                            story.completed_at = None
                            story.updated_at = datetime.utcnow()
                            db.commit()
                            result["changes"].append({"type": "story_reopened", "story_id": story.story_id})
                            logger.info(f"[Webhook] Story reaberta: {story.story_id}")

                    elif action == "deleted":
                        logger.info(f"[Webhook] Issue deletada #{issue_number}")
                        if story:
                            story.is_deleted = True
                            story.deleted_at = datetime.utcnow()
                            db.commit()
                            result["changes"].append({"type": "story_deleted", "story_id": story.story_id})

                finally:
                    db.close()

            # Pull Request events - Issue #162
            pr = payload.get("pull_request", {})
            if pr:
                pr_number = pr.get("number")
                pr_title = pr.get("title", "")
                pr_state = pr.get("state", "")
                pr_merged = pr.get("merged", False)

                logger.info(f"[Webhook] PR #{pr_number}: {action}")

                db = SessionLocal()
                try:
                    # Tentar encontrar task relacionada ao PR
                    task = db.query(StoryTask).filter(
                        StoryTask.title.contains(f"PR #{pr_number}")
                    ).first()

                    # Ou buscar por branch name mencionando story
                    branch_name = pr.get("head", {}).get("ref", "")
                    if not task and branch_name:
                        # Ex: feature/STR-0001-login
                        import re
                        match = re.search(r'(STR-\d+)', branch_name, re.IGNORECASE)
                        if match:
                            story_id = match.group(1).upper()
                            story = db.query(Story).filter(Story.story_id == story_id).first()
                            if story:
                                # Atualizar status da story baseado no PR
                                if action == "opened":
                                    if story.status in ["backlog", "ready", "in_progress"]:
                                        story.status = "review"
                                        story.updated_at = datetime.utcnow()
                                        db.commit()
                                        result["changes"].append({"type": "story_to_review", "story_id": story.story_id})

                                elif action == "closed":
                                    if pr_merged:
                                        # PR merged = move to testing or done
                                        story.status = "testing"
                                        story.updated_at = datetime.utcnow()
                                        db.commit()
                                        result["changes"].append({"type": "story_to_testing", "story_id": story.story_id})
                finally:
                    db.close()

            # Push events - Issue #162
            commits = payload.get("commits", [])
            if commits:
                ref = payload.get("ref", "")
                branch = ref.replace("refs/heads/", "") if ref.startswith("refs/heads/") else ref

                logger.info(f"[Webhook] Push: {len(commits)} commits to {branch}")

                # Verificar se commits mencionam stories
                db = SessionLocal()
                try:
                    for commit in commits:
                        message = commit.get("message", "")
                        # Procurar por referências a stories (ex: "STR-0001")
                        import re
                        story_refs = re.findall(r'(STR-\d+)', message, re.IGNORECASE)
                        for story_ref in story_refs:
                            story = db.query(Story).filter(Story.story_id == story_ref.upper()).first()
                            if story and story.status == "backlog":
                                story.status = "in_progress"
                                story.started_at = datetime.utcnow()
                                story.updated_at = datetime.utcnow()
                                db.commit()
                                result["changes"].append({
                                    "type": "story_started",
                                    "story_id": story.story_id,
                                    "commit": commit.get("id", "")[:7]
                                })
                finally:
                    db.close()

            # Issue comment events
            comment = payload.get("comment", {})
            if comment and action in ["created", "edited", "deleted"]:
                issue_number = issue.get("number") if issue else None
                logger.info(f"[Webhook] Comentário {action} na issue #{issue_number}")
                result["changes"].append({"type": f"comment_{action}", "issue": issue_number})

            logger.debug(f"[Webhook] Processado: {result}")
            return result

        except Exception as e:
            logger.error(f"[Webhook] Erro ao processar: {e}")
            return {"success": False, "error": str(e)}

    def get_status(self) -> Dict[str, Any]:
        """Retorna status detalhado da integracao"""
        status = super().get_status()
        status.update({
            "system": "github",
            "owner": self.config.owner,
            "repo": self.config.repo,
            "full_name": f"{self.config.owner}/{self.config.repo}",
            "user": self._user_info.get("login") if self._user_info else None,
            "repo_private": self._repo_info.get("private") if self._repo_info else None,
            "repo_url": self._repo_info.get("html_url") if self._repo_info else None
        })
        return status


# Instancia global (singleton)
_github_instance: Optional[GitHubIntegration] = None


def get_github_integration() -> GitHubIntegration:
    """Retorna instancia global da integracao GitHub"""
    global _github_instance
    if _github_instance is None:
        config = GitHubConfig.from_env()
        _github_instance = GitHubIntegration(config)
    return _github_instance


async def init_github_integration() -> Optional[GitHubIntegration]:
    """Inicializa e conecta a integracao GitHub se configurada"""
    github = get_github_integration()
    if github.config.is_valid() and github.config.enabled:
        if await github.connect():
            return github
    return None

# -*- coding: utf-8 -*-
"""
Jira Agile Skill
================
Skill para operacoes Agile no Jira.

Funcionalidades:
- Gerenciamento de Sprints
- Gerenciamento de Boards (Kanban/Scrum)
- Gerenciamento de Epics
- Backlog operations
- Velocity e metricas

Uso pelos agentes:
    from factory.integrations.jira.skills import JiraAgileSkill

    skill = JiraAgileSkill(jira_client)

    # Listar sprints de um board
    result = await skill.list_sprints(board_id=123)

    # Criar sprint
    result = await skill.create_sprint(
        board_id=123,
        name="Sprint 10",
        start_date="2024-01-15",
        end_date="2024-01-29"
    )

    # Mover issue para sprint
    result = await skill.move_to_sprint(issue_keys=["PROJ-123"], sprint_id=456)

Autor: Plataforma E
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class SprintState(str, Enum):
    """Estados de um Sprint"""
    FUTURE = "future"
    ACTIVE = "active"
    CLOSED = "closed"


class BoardType(str, Enum):
    """Tipos de Board"""
    SCRUM = "scrum"
    KANBAN = "kanban"


@dataclass
class SkillResult:
    """Resultado de uma operacao de skill"""
    success: bool
    data: Any = None
    message: str = ""
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "message": self.message,
            "errors": self.errors
        }


@dataclass
class SprintInfo:
    """Informacoes de um Sprint"""
    id: int
    name: str
    state: SprintState
    start_date: Optional[str] = None
    end_date: Optional[str] = None
    complete_date: Optional[str] = None
    goal: Optional[str] = None
    board_id: Optional[int] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "name": self.name,
            "state": self.state.value,
            "start_date": self.start_date,
            "end_date": self.end_date,
            "complete_date": self.complete_date,
            "goal": self.goal,
            "board_id": self.board_id
        }


@dataclass
class BoardInfo:
    """Informacoes de um Board"""
    id: int
    name: str
    board_type: BoardType
    project_key: Optional[str] = None
    project_name: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "name": self.name,
            "board_type": self.board_type.value,
            "project_key": self.project_key,
            "project_name": self.project_name
        }


@dataclass
class EpicInfo:
    """Informacoes de um Epic"""
    key: str
    name: str
    summary: str
    status: str
    done: bool = False
    issue_count: int = 0
    done_count: int = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "key": self.key,
            "name": self.name,
            "summary": self.summary,
            "status": self.status,
            "done": self.done,
            "issue_count": self.issue_count,
            "done_count": self.done_count,
            "progress": round(self.done_count / self.issue_count * 100, 1) if self.issue_count > 0 else 0
        }


class JiraAgileSkill:
    """
    Skill para operacoes Agile no Jira

    Fornece metodos de alto nivel para gerenciar sprints,
    boards, epics e outras funcionalidades Agile do Jira.

    Esta skill usa a API Agile do Jira (Jira Software).

    Exemplo de uso:
    ```python
    from factory.integrations.jira import get_jira_integration

    jira = get_jira_integration()
    await jira.connect()

    skill = JiraAgileSkill(jira)

    # Gerenciar sprints
    sprints = await skill.list_sprints(board_id=123, state="active")
    await skill.start_sprint(sprint_id=456)
    await skill.complete_sprint(sprint_id=456)

    # Gerenciar epics
    epics = await skill.list_epics(project_key="PROJ")
    await skill.add_to_epic(epic_key="PROJ-10", issue_keys=["PROJ-20", "PROJ-21"])
    ```
    """

    # API Agile base
    AGILE_API_VERSION = "1.0"

    # Schema Claude-compatible para tools
    TOOL_SCHEMA = {
        "name": "jira_agile",
        "description": "Agile operations in Jira including sprints, boards, and epics",
        "input_schema": {
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "enum": [
                        "list_boards",
                        "get_board",
                        "list_sprints",
                        "get_sprint",
                        "create_sprint",
                        "start_sprint",
                        "complete_sprint",
                        "move_to_sprint",
                        "move_to_backlog",
                        "list_epics",
                        "get_epic",
                        "create_epic",
                        "add_to_epic",
                        "remove_from_epic",
                        "get_velocity",
                        "get_sprint_report",
                        "get_backlog"
                    ],
                    "description": "The agile action to perform"
                },
                "board_id": {
                    "type": "integer",
                    "description": "Board ID"
                },
                "sprint_id": {
                    "type": "integer",
                    "description": "Sprint ID"
                },
                "epic_key": {
                    "type": "string",
                    "description": "Epic issue key"
                },
                "project_key": {
                    "type": "string",
                    "description": "Project key"
                },
                "issue_keys": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "List of issue keys"
                },
                "name": {
                    "type": "string",
                    "description": "Name for sprint or epic"
                },
                "goal": {
                    "type": "string",
                    "description": "Sprint goal"
                },
                "start_date": {
                    "type": "string",
                    "description": "Start date (YYYY-MM-DD)"
                },
                "end_date": {
                    "type": "string",
                    "description": "End date (YYYY-MM-DD)"
                },
                "state": {
                    "type": "string",
                    "enum": ["future", "active", "closed"],
                    "description": "Sprint state filter"
                },
                "max_results": {
                    "type": "integer",
                    "default": 50
                }
            },
            "required": ["action"]
        }
    }

    # Acoes disponiveis
    AVAILABLE_ACTIONS = [
        "list_boards",
        "get_board",
        "list_sprints",
        "get_sprint",
        "create_sprint",
        "start_sprint",
        "complete_sprint",
        "move_to_sprint",
        "move_to_backlog",
        "list_epics",
        "get_epic",
        "create_epic",
        "add_to_epic",
        "remove_from_epic",
        "get_velocity",
        "get_sprint_report",
        "get_backlog"
    ]

    def __init__(self, jira_client):
        """
        Inicializa a skill

        Args:
            jira_client: JiraIntegration autenticado
        """
        self.jira = jira_client

    @property
    def agile_base_url(self) -> str:
        """URL base da API Agile"""
        url = self.jira.config.url.rstrip("/")
        return f"{url}/rest/agile/{self.AGILE_API_VERSION}"

    async def execute(self, action: str, **params) -> SkillResult:
        """
        Executa uma acao da skill

        Args:
            action: Nome da acao
            **params: Parametros da acao

        Returns:
            SkillResult com o resultado
        """
        action_map = {
            "list_boards": self.list_boards,
            "get_board": self.get_board,
            "list_sprints": self.list_sprints,
            "get_sprint": self.get_sprint,
            "create_sprint": self.create_sprint,
            "start_sprint": self.start_sprint,
            "complete_sprint": self.complete_sprint,
            "move_to_sprint": self.move_to_sprint,
            "move_to_backlog": self.move_to_backlog,
            "list_epics": self.list_epics,
            "get_epic": self.get_epic,
            "create_epic": self.create_epic,
            "add_to_epic": self.add_to_epic,
            "remove_from_epic": self.remove_from_epic,
            "get_velocity": self.get_velocity,
            "get_sprint_report": self.get_sprint_report,
            "get_backlog": self.get_backlog
        }

        if action not in action_map:
            return SkillResult(
                success=False,
                message=f"Acao desconhecida: {action}",
                errors=[f"Acoes disponiveis: {', '.join(self.AVAILABLE_ACTIONS)}"]
            )

        try:
            handler = action_map[action]
            result = await handler(**params)
            return result
        except Exception as e:
            logger.error(f"Erro ao executar acao {action}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao executar {action}",
                errors=[str(e)]
            )

    # ==================== BOARDS ====================

    async def list_boards(
        self,
        project_key: Optional[str] = None,
        board_type: Optional[str] = None,
        name_filter: Optional[str] = None,
        max_results: int = 50,
        **kwargs
    ) -> SkillResult:
        """
        Lista boards do Jira Software

        Args:
            project_key: Filtrar por projeto
            board_type: Filtrar por tipo (scrum/kanban)
            name_filter: Filtrar por nome
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de boards
        """
        try:
            session = await self.jira._ensure_session()

            params = {"maxResults": max_results}

            if project_key:
                params["projectKeyOrId"] = project_key
            if board_type:
                params["type"] = board_type
            if name_filter:
                params["name"] = name_filter

            url = f"{self.agile_base_url}/board"

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    boards = data.get("values", [])

                    board_list = []
                    for b in boards:
                        board_info = BoardInfo(
                            id=b.get("id"),
                            name=b.get("name"),
                            board_type=BoardType(b.get("type", "scrum").lower()),
                            project_key=b.get("location", {}).get("projectKey"),
                            project_name=b.get("location", {}).get("displayName")
                        )
                        board_list.append(board_info.to_dict())

                    return SkillResult(
                        success=True,
                        data=board_list,
                        message=f"Encontrados {len(board_list)} boards"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao listar boards: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao listar boards: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_board(self, board_id: int, **kwargs) -> SkillResult:
        """
        Obtem detalhes de um board

        Args:
            board_id: ID do board

        Returns:
            SkillResult com dados do board
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    b = await response.json()

                    board_info = {
                        "id": b.get("id"),
                        "name": b.get("name"),
                        "board_type": b.get("type", "scrum").lower(),
                        "project_key": b.get("location", {}).get("projectKey"),
                        "project_name": b.get("location", {}).get("displayName"),
                        "self": b.get("self")
                    }

                    return SkillResult(
                        success=True,
                        data=board_info,
                        message=f"Board {board_id} encontrado"
                    )
                elif response.status == 404:
                    return SkillResult(
                        success=False,
                        message=f"Board {board_id} nao encontrado"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao buscar board: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar board {board_id}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== SPRINTS ====================

    async def list_sprints(
        self,
        board_id: int,
        state: Optional[str] = None,
        max_results: int = 50,
        **kwargs
    ) -> SkillResult:
        """
        Lista sprints de um board

        Args:
            board_id: ID do board
            state: Filtrar por estado (future/active/closed)
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de sprints
        """
        try:
            session = await self.jira._ensure_session()

            params = {"maxResults": max_results}
            if state:
                params["state"] = state

            url = f"{self.agile_base_url}/board/{board_id}/sprint"

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    sprints = data.get("values", [])

                    sprint_list = []
                    for s in sprints:
                        sprint_info = SprintInfo(
                            id=s.get("id"),
                            name=s.get("name"),
                            state=SprintState(s.get("state", "future").lower()),
                            start_date=s.get("startDate"),
                            end_date=s.get("endDate"),
                            complete_date=s.get("completeDate"),
                            goal=s.get("goal"),
                            board_id=board_id
                        )
                        sprint_list.append(sprint_info.to_dict())

                    return SkillResult(
                        success=True,
                        data=sprint_list,
                        message=f"Encontrados {len(sprint_list)} sprints"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao listar sprints: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao listar sprints: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_sprint(self, sprint_id: int, **kwargs) -> SkillResult:
        """
        Obtem detalhes de um sprint

        Args:
            sprint_id: ID do sprint

        Returns:
            SkillResult com dados do sprint
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            async with session.get(url) as response:
                if response.status == 200:
                    s = await response.json()

                    sprint_info = SprintInfo(
                        id=s.get("id"),
                        name=s.get("name"),
                        state=SprintState(s.get("state", "future").lower()),
                        start_date=s.get("startDate"),
                        end_date=s.get("endDate"),
                        complete_date=s.get("completeDate"),
                        goal=s.get("goal"),
                        board_id=s.get("originBoardId")
                    )

                    return SkillResult(
                        success=True,
                        data=sprint_info.to_dict(),
                        message=f"Sprint {sprint_id} encontrado"
                    )
                elif response.status == 404:
                    return SkillResult(
                        success=False,
                        message=f"Sprint {sprint_id} nao encontrado"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao buscar sprint: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar sprint {sprint_id}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def create_sprint(
        self,
        board_id: int,
        name: str,
        goal: Optional[str] = None,
        start_date: Optional[str] = None,
        end_date: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Cria um novo sprint

        Args:
            board_id: ID do board
            name: Nome do sprint
            goal: Objetivo do sprint
            start_date: Data de inicio (ISO format)
            end_date: Data de fim (ISO format)

        Returns:
            SkillResult com sprint criado
        """
        try:
            session = await self.jira._ensure_session()
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

                    return SkillResult(
                        success=True,
                        data={
                            "id": sprint.get("id"),
                            "name": sprint.get("name"),
                            "state": sprint.get("state"),
                            "goal": sprint.get("goal")
                        },
                        message=f"Sprint '{name}' criado com sucesso"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao criar sprint: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao criar sprint: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def start_sprint(
        self,
        sprint_id: int,
        start_date: Optional[str] = None,
        end_date: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Inicia um sprint

        Args:
            sprint_id: ID do sprint
            start_date: Data de inicio
            end_date: Data de fim

        Returns:
            SkillResult
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            data = {"state": "active"}

            if start_date:
                data["startDate"] = start_date
            if end_date:
                data["endDate"] = end_date

            async with session.post(url, json=data) as response:
                if response.status in [200, 204]:
                    return SkillResult(
                        success=True,
                        data={"sprint_id": sprint_id, "state": "active"},
                        message=f"Sprint {sprint_id} iniciado"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao iniciar sprint: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao iniciar sprint {sprint_id}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def complete_sprint(
        self,
        sprint_id: int,
        move_incomplete_to: Optional[int] = None,
        **kwargs
    ) -> SkillResult:
        """
        Completa um sprint

        Args:
            sprint_id: ID do sprint
            move_incomplete_to: Sprint para mover issues incompletas

        Returns:
            SkillResult
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}"

            data = {"state": "closed"}

            async with session.post(url, json=data) as response:
                if response.status in [200, 204]:
                    return SkillResult(
                        success=True,
                        data={"sprint_id": sprint_id, "state": "closed"},
                        message=f"Sprint {sprint_id} completado"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao completar sprint: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao completar sprint {sprint_id}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def move_to_sprint(
        self,
        sprint_id: int,
        issue_keys: List[str],
        **kwargs
    ) -> SkillResult:
        """
        Move issues para um sprint

        Args:
            sprint_id: ID do sprint destino
            issue_keys: Lista de chaves de issues

        Returns:
            SkillResult
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"

            data = {"issues": issue_keys}

            async with session.post(url, json=data) as response:
                if response.status in [200, 204]:
                    return SkillResult(
                        success=True,
                        data={"sprint_id": sprint_id, "moved_issues": issue_keys},
                        message=f"Movidas {len(issue_keys)} issues para sprint {sprint_id}"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao mover issues: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao mover issues para sprint: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def move_to_backlog(
        self,
        issue_keys: List[str],
        **kwargs
    ) -> SkillResult:
        """
        Move issues para o backlog (remove do sprint)

        Args:
            issue_keys: Lista de chaves de issues

        Returns:
            SkillResult
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/backlog/issue"

            data = {"issues": issue_keys}

            async with session.post(url, json=data) as response:
                if response.status in [200, 204]:
                    return SkillResult(
                        success=True,
                        data={"moved_issues": issue_keys},
                        message=f"Movidas {len(issue_keys)} issues para o backlog"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao mover issues: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao mover issues para backlog: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== EPICS ====================

    async def list_epics(
        self,
        board_id: Optional[int] = None,
        project_key: Optional[str] = None,
        done: Optional[bool] = None,
        max_results: int = 50,
        **kwargs
    ) -> SkillResult:
        """
        Lista epics

        Args:
            board_id: ID do board (para API Agile)
            project_key: Chave do projeto (para busca JQL)
            done: Filtrar por completos/incompletos
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de epics
        """
        try:
            if board_id:
                # Usa API Agile
                session = await self.jira._ensure_session()
                url = f"{self.agile_base_url}/board/{board_id}/epic"

                params = {"maxResults": max_results}
                if done is not None:
                    params["done"] = str(done).lower()

                async with session.get(url, params=params) as response:
                    if response.status == 200:
                        data = await response.json()
                        epics = data.get("values", [])

                        epic_list = []
                        for e in epics:
                            epic_info = EpicInfo(
                                key=e.get("key"),
                                name=e.get("name", ""),
                                summary=e.get("summary", ""),
                                status=e.get("done", False) and "Done" or "In Progress",
                                done=e.get("done", False)
                            )
                            epic_list.append(epic_info.to_dict())

                        return SkillResult(
                            success=True,
                            data=epic_list,
                            message=f"Encontrados {len(epic_list)} epics"
                        )
            else:
                # Usa JQL
                project = project_key or self.jira.config.project_key
                jql = f"project = {project} AND issuetype = Epic"

                if done is True:
                    jql += " AND resolution IS NOT EMPTY"
                elif done is False:
                    jql += " AND resolution IS EMPTY"

                jql += " ORDER BY created DESC"

                issues = await self.jira.search_issues(jql=jql, max_results=max_results)

                epic_list = []
                for issue in issues:
                    fields = issue.get("fields", {})
                    epic_info = EpicInfo(
                        key=issue.get("key"),
                        name=fields.get("customfield_10011", fields.get("summary", "")),
                        summary=fields.get("summary", ""),
                        status=fields.get("status", {}).get("name", "Unknown"),
                        done=fields.get("resolution") is not None
                    )
                    epic_list.append(epic_info.to_dict())

                return SkillResult(
                    success=True,
                    data=epic_list,
                    message=f"Encontrados {len(epic_list)} epics"
                )

        except Exception as e:
            logger.error(f"Erro ao listar epics: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_epic(
        self,
        epic_key: str,
        include_issues: bool = False,
        **kwargs
    ) -> SkillResult:
        """
        Obtem detalhes de um epic

        Args:
            epic_key: Chave do epic
            include_issues: Incluir issues do epic

        Returns:
            SkillResult com dados do epic
        """
        try:
            # Busca issue do epic
            issue = await self.jira.get_issue(epic_key)

            if not issue:
                return SkillResult(
                    success=False,
                    message=f"Epic {epic_key} nao encontrado"
                )

            fields = issue.get("fields", {})

            epic_data = {
                "key": epic_key,
                "name": fields.get("customfield_10011", fields.get("summary", "")),
                "summary": fields.get("summary"),
                "description": fields.get("description"),
                "status": fields.get("status", {}).get("name"),
                "priority": fields.get("priority", {}).get("name"),
                "assignee": fields.get("assignee", {}).get("displayName") if fields.get("assignee") else None,
                "created": fields.get("created"),
                "updated": fields.get("updated"),
                "done": fields.get("resolution") is not None
            }

            if include_issues:
                # Busca issues do epic
                jql = f"'Epic Link' = {epic_key} OR parent = {epic_key}"
                epic_issues = await self.jira.search_issues(jql=jql, max_results=100)

                epic_data["issues"] = [
                    {
                        "key": i.get("key"),
                        "summary": i.get("fields", {}).get("summary"),
                        "status": i.get("fields", {}).get("status", {}).get("name"),
                        "issue_type": i.get("fields", {}).get("issuetype", {}).get("name")
                    }
                    for i in epic_issues
                ]
                epic_data["issue_count"] = len(epic_issues)
                epic_data["done_count"] = sum(
                    1 for i in epic_issues
                    if i.get("fields", {}).get("resolution") is not None
                )

            return SkillResult(
                success=True,
                data=epic_data,
                message=f"Epic {epic_key} encontrado"
            )

        except Exception as e:
            logger.error(f"Erro ao buscar epic {epic_key}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def create_epic(
        self,
        name: str,
        summary: str,
        project_key: Optional[str] = None,
        description: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Cria um novo epic

        Args:
            name: Nome do epic
            summary: Resumo do epic
            project_key: Chave do projeto
            description: Descricao detalhada

        Returns:
            SkillResult com epic criado
        """
        try:
            project = project_key or self.jira.config.project_key

            issue_data = {
                "fields": {
                    "project": {"key": project},
                    "summary": summary,
                    "issuetype": {"name": "Epic"},
                    "customfield_10011": name  # Epic Name
                }
            }

            if description:
                issue_data["fields"]["description"] = description

            created = await self.jira.create_issue(issue_data)

            if created:
                return SkillResult(
                    success=True,
                    data={
                        "key": created.get("key"),
                        "id": created.get("id"),
                        "name": name,
                        "summary": summary
                    },
                    message=f"Epic '{name}' criado: {created.get('key')}"
                )
            else:
                return SkillResult(
                    success=False,
                    message="Falha ao criar epic"
                )

        except Exception as e:
            logger.error(f"Erro ao criar epic: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def add_to_epic(
        self,
        epic_key: str,
        issue_keys: List[str],
        **kwargs
    ) -> SkillResult:
        """
        Adiciona issues a um epic

        Args:
            epic_key: Chave do epic
            issue_keys: Lista de chaves de issues

        Returns:
            SkillResult
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/epic/{epic_key}/issue"

            data = {"issues": issue_keys}

            async with session.post(url, json=data) as response:
                if response.status in [200, 204]:
                    return SkillResult(
                        success=True,
                        data={"epic_key": epic_key, "added_issues": issue_keys},
                        message=f"Adicionadas {len(issue_keys)} issues ao epic {epic_key}"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao adicionar issues ao epic: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao adicionar issues ao epic: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def remove_from_epic(
        self,
        issue_keys: List[str],
        **kwargs
    ) -> SkillResult:
        """
        Remove issues de seus epics

        Args:
            issue_keys: Lista de chaves de issues

        Returns:
            SkillResult
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/epic/none/issue"

            data = {"issues": issue_keys}

            async with session.post(url, json=data) as response:
                if response.status in [200, 204]:
                    return SkillResult(
                        success=True,
                        data={"removed_issues": issue_keys},
                        message=f"Removidas {len(issue_keys)} issues de seus epics"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao remover issues dos epics: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao remover issues dos epics: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== REPORTS ====================

    async def get_velocity(
        self,
        board_id: int,
        num_sprints: int = 5,
        **kwargs
    ) -> SkillResult:
        """
        Calcula velocity baseado em sprints anteriores

        Args:
            board_id: ID do board
            num_sprints: Numero de sprints para calcular

        Returns:
            SkillResult com velocity
        """
        try:
            # Lista sprints fechados
            sprints_result = await self.list_sprints(
                board_id=board_id,
                state="closed",
                max_results=num_sprints
            )

            if not sprints_result.success:
                return sprints_result

            sprints = sprints_result.data

            velocity_data = {
                "board_id": board_id,
                "sprints_analyzed": len(sprints),
                "sprint_velocities": [],
                "average_velocity": 0,
                "total_points": 0
            }

            for sprint in sprints:
                sprint_id = sprint.get("id")

                # Busca issues do sprint
                session = await self.jira._ensure_session()
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
                                points = fields.get("customfield_10016") or fields.get("customfield_10024") or 0
                                sprint_points += int(points) if points else 0

                        velocity_data["sprint_velocities"].append({
                            "sprint_id": sprint_id,
                            "sprint_name": sprint.get("name"),
                            "points": sprint_points
                        })
                        velocity_data["total_points"] += sprint_points

            if velocity_data["sprints_analyzed"] > 0:
                velocity_data["average_velocity"] = round(
                    velocity_data["total_points"] / velocity_data["sprints_analyzed"], 1
                )

            return SkillResult(
                success=True,
                data=velocity_data,
                message=f"Velocity media: {velocity_data['average_velocity']} pontos/sprint"
            )

        except Exception as e:
            logger.error(f"Erro ao calcular velocity: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_sprint_report(
        self,
        sprint_id: int,
        **kwargs
    ) -> SkillResult:
        """
        Gera relatorio de um sprint

        Args:
            sprint_id: ID do sprint

        Returns:
            SkillResult com relatorio
        """
        try:
            # Busca info do sprint
            sprint_result = await self.get_sprint(sprint_id)
            if not sprint_result.success:
                return sprint_result

            sprint = sprint_result.data

            # Busca issues do sprint
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/sprint/{sprint_id}/issue"

            async with session.get(url) as response:
                if response.status == 200:
                    data = await response.json()
                    issues = data.get("issues", [])

                    report = {
                        "sprint": sprint,
                        "total_issues": len(issues),
                        "completed": 0,
                        "incomplete": 0,
                        "total_points": 0,
                        "completed_points": 0,
                        "issues_by_type": {},
                        "issues_by_status": {},
                        "issues": []
                    }

                    for issue in issues:
                        fields = issue.get("fields", {})

                        # Tipo
                        issue_type = fields.get("issuetype", {}).get("name", "Unknown")
                        report["issues_by_type"][issue_type] = report["issues_by_type"].get(issue_type, 0) + 1

                        # Status
                        status = fields.get("status", {}).get("name", "Unknown")
                        report["issues_by_status"][status] = report["issues_by_status"].get(status, 0) + 1

                        # Points
                        points = fields.get("customfield_10016") or fields.get("customfield_10024") or 0
                        points = int(points) if points else 0
                        report["total_points"] += points

                        # Completo?
                        if fields.get("resolution"):
                            report["completed"] += 1
                            report["completed_points"] += points
                        else:
                            report["incomplete"] += 1

                        report["issues"].append({
                            "key": issue.get("key"),
                            "summary": fields.get("summary"),
                            "type": issue_type,
                            "status": status,
                            "points": points,
                            "completed": fields.get("resolution") is not None
                        })

                    report["completion_rate"] = round(
                        report["completed"] / report["total_issues"] * 100, 1
                    ) if report["total_issues"] > 0 else 0

                    return SkillResult(
                        success=True,
                        data=report,
                        message=f"Sprint {sprint['name']}: {report['completed']}/{report['total_issues']} completas ({report['completion_rate']}%)"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao gerar relatorio: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao gerar relatorio do sprint: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_backlog(
        self,
        board_id: int,
        max_results: int = 100,
        **kwargs
    ) -> SkillResult:
        """
        Lista issues do backlog

        Args:
            board_id: ID do board
            max_results: Maximo de resultados

        Returns:
            SkillResult com issues do backlog
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.agile_base_url}/board/{board_id}/backlog"

            params = {"maxResults": max_results}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    issues = data.get("issues", [])

                    backlog = []
                    for issue in issues:
                        fields = issue.get("fields", {})
                        backlog.append({
                            "key": issue.get("key"),
                            "summary": fields.get("summary"),
                            "type": fields.get("issuetype", {}).get("name"),
                            "priority": fields.get("priority", {}).get("name"),
                            "status": fields.get("status", {}).get("name"),
                            "story_points": fields.get("customfield_10016") or fields.get("customfield_10024"),
                            "epic": fields.get("customfield_10014")
                        })

                    return SkillResult(
                        success=True,
                        data=backlog,
                        message=f"Backlog: {len(backlog)} issues"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao buscar backlog: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar backlog: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

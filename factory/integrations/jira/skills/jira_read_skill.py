# -*- coding: utf-8 -*-
"""
Jira Read Skill
===============
Skill para leitura de dados do Jira.

Funcionalidades:
- Leitura de issues (stories, tasks, bugs, epics)
- Leitura de projetos e usuarios
- Busca por JQL
- Analise de issues

Uso pelos agentes:
    from factory.integrations.jira.skills import JiraReadSkill

    skill = JiraReadSkill(jira_client)

    # Buscar issue especifica
    result = await skill.get_issue("PROJ-123")

    # Buscar issues por JQL
    result = await skill.search_issues(jql="project = PROJ AND status = 'In Progress'")

    # Listar projetos
    result = await skill.list_projects()

Autor: Plataforma E
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class JiraIssueType(str, Enum):
    """Tipos de issue no Jira"""
    STORY = "Story"
    TASK = "Task"
    BUG = "Bug"
    EPIC = "Epic"
    SUBTASK = "Sub-task"
    IMPROVEMENT = "Improvement"
    SPIKE = "Spike"


class JiraIssueStatus(str, Enum):
    """Status comuns de issues no Jira"""
    TO_DO = "To Do"
    IN_PROGRESS = "In Progress"
    IN_REVIEW = "In Review"
    DONE = "Done"
    BLOCKED = "Blocked"


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
class JiraReadResult:
    """Resultado de uma operacao de leitura do Jira"""
    success: bool
    data: List[Dict] = field(default_factory=list)
    count: int = 0
    total: int = 0
    message: str = ""
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "data": self.data,
            "count": self.count,
            "total": self.total,
            "message": self.message,
            "errors": self.errors
        }


class JiraReadSkill:
    """
    Skill de leitura de dados do Jira

    Fornece metodos de alto nivel para leitura de issues,
    projetos e usuarios do Jira.

    Esta skill e utilizada por agentes para obter informacoes
    do Jira de forma estruturada.

    Exemplo de uso:
    ```python
    from factory.integrations.jira import get_jira_integration

    jira = get_jira_integration()
    await jira.connect()

    skill = JiraReadSkill(jira)

    # Buscar todas as stories de um projeto
    result = await skill.search_issues(
        project_key="PROJ",
        issue_type=JiraIssueType.STORY,
        max_results=50
    )

    # Buscar issue especifica com detalhes
    issue = await skill.get_issue("PROJ-123", include_transitions=True)

    # Listar usuarios atribuiveis
    users = await skill.list_assignable_users("PROJ")
    ```
    """

    # Schema Claude-compatible para tools
    TOOL_SCHEMA = {
        "name": "jira_read",
        "description": "Read data from Jira including issues, projects, and users",
        "input_schema": {
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "enum": [
                        "get_issue",
                        "search_issues",
                        "list_projects",
                        "get_project",
                        "list_users",
                        "get_user",
                        "get_transitions",
                        "get_comments"
                    ],
                    "description": "The action to perform"
                },
                "issue_key": {
                    "type": "string",
                    "description": "Issue key (e.g., PROJ-123)"
                },
                "project_key": {
                    "type": "string",
                    "description": "Project key (e.g., PROJ)"
                },
                "jql": {
                    "type": "string",
                    "description": "JQL query for searching issues"
                },
                "issue_type": {
                    "type": "string",
                    "enum": ["Story", "Task", "Bug", "Epic", "Sub-task"],
                    "description": "Filter by issue type"
                },
                "status": {
                    "type": "string",
                    "description": "Filter by status"
                },
                "assignee": {
                    "type": "string",
                    "description": "Filter by assignee username"
                },
                "max_results": {
                    "type": "integer",
                    "default": 50,
                    "description": "Maximum number of results"
                },
                "start_at": {
                    "type": "integer",
                    "default": 0,
                    "description": "Offset for pagination"
                }
            },
            "required": ["action"]
        }
    }

    # Acoes disponiveis
    AVAILABLE_ACTIONS = [
        "get_issue",
        "search_issues",
        "list_projects",
        "get_project",
        "list_users",
        "get_user",
        "get_transitions",
        "get_comments"
    ]

    def __init__(self, jira_client):
        """
        Inicializa a skill

        Args:
            jira_client: JiraIntegration autenticado
        """
        self.jira = jira_client

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
            "get_issue": self.get_issue,
            "search_issues": self.search_issues,
            "list_projects": self.list_projects,
            "get_project": self.get_project,
            "list_users": self.list_assignable_users,
            "get_user": self.get_user,
            "get_transitions": self.get_transitions,
            "get_comments": self.get_comments
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

    # ==================== ISSUES ====================

    async def get_issue(
        self,
        issue_key: str,
        include_transitions: bool = False,
        include_comments: bool = False,
        **kwargs
    ) -> SkillResult:
        """
        Busca uma issue especifica por chave

        Args:
            issue_key: Chave da issue (ex: PROJ-123)
            include_transitions: Incluir transicoes disponiveis
            include_comments: Incluir comentarios

        Returns:
            SkillResult com dados da issue
        """
        try:
            issue = await self.jira.get_issue(issue_key)

            if not issue:
                return SkillResult(
                    success=False,
                    message=f"Issue {issue_key} nao encontrada"
                )

            # Simplifica dados para retorno
            issue_data = self._simplify_issue(issue)

            # Adiciona transicoes se solicitado
            if include_transitions:
                transitions = await self.jira.get_transitions(issue_key)
                issue_data["available_transitions"] = [
                    {"id": t.get("id"), "name": t.get("name")}
                    for t in transitions
                ]

            # Adiciona comentarios se solicitado
            if include_comments:
                comments_result = await self.get_comments(issue_key=issue_key)
                if comments_result.success:
                    issue_data["comments"] = comments_result.data

            return SkillResult(
                success=True,
                data=issue_data,
                message=f"Issue {issue_key} encontrada"
            )

        except Exception as e:
            logger.error(f"Erro ao buscar issue {issue_key}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao buscar issue: {str(e)}",
                errors=[str(e)]
            )

    async def search_issues(
        self,
        project_key: Optional[str] = None,
        jql: Optional[str] = None,
        issue_type: Optional[str] = None,
        status: Optional[str] = None,
        assignee: Optional[str] = None,
        labels: Optional[List[str]] = None,
        max_results: int = 50,
        start_at: int = 0,
        order_by: str = "updated DESC",
        **kwargs
    ) -> JiraReadResult:
        """
        Busca issues usando JQL ou filtros

        Args:
            project_key: Chave do projeto
            jql: Query JQL customizada (ignora outros filtros se fornecida)
            issue_type: Filtrar por tipo de issue
            status: Filtrar por status
            assignee: Filtrar por assignee
            labels: Filtrar por labels
            max_results: Maximo de resultados
            start_at: Offset para paginacao
            order_by: Ordenacao

        Returns:
            JiraReadResult com lista de issues
        """
        try:
            # Constroi JQL se nao fornecida
            if not jql:
                jql_parts = []

                if project_key:
                    jql_parts.append(f"project = {project_key}")
                elif self.jira.config.project_key:
                    jql_parts.append(f"project = {self.jira.config.project_key}")

                if issue_type:
                    jql_parts.append(f"issuetype = '{issue_type}'")

                if status:
                    jql_parts.append(f"status = '{status}'")

                if assignee:
                    jql_parts.append(f"assignee = '{assignee}'")

                if labels:
                    for label in labels:
                        jql_parts.append(f"labels = '{label}'")

                jql = " AND ".join(jql_parts) if jql_parts else None

                if jql and order_by:
                    jql += f" ORDER BY {order_by}"

            issues = await self.jira.search_issues(
                project_key=project_key,
                jql=jql,
                max_results=max_results,
                start_at=start_at
            )

            # Simplifica cada issue
            simplified_issues = [self._simplify_issue(issue) for issue in issues]

            return JiraReadResult(
                success=True,
                data=simplified_issues,
                count=len(simplified_issues),
                total=len(simplified_issues),  # API pode retornar total real
                message=f"Encontradas {len(simplified_issues)} issues"
            )

        except Exception as e:
            logger.error(f"Erro na busca de issues: {e}")
            return JiraReadResult(
                success=False,
                message=f"Erro na busca: {str(e)}",
                errors=[str(e)]
            )

    async def get_transitions(
        self,
        issue_key: str,
        **kwargs
    ) -> SkillResult:
        """
        Lista transicoes disponiveis para uma issue

        Args:
            issue_key: Chave da issue

        Returns:
            SkillResult com lista de transicoes
        """
        try:
            transitions = await self.jira.get_transitions(issue_key)

            simplified = [
                {
                    "id": t.get("id"),
                    "name": t.get("name"),
                    "to_status": t.get("to", {}).get("name")
                }
                for t in transitions
            ]

            return SkillResult(
                success=True,
                data=simplified,
                message=f"Encontradas {len(simplified)} transicoes para {issue_key}"
            )

        except Exception as e:
            logger.error(f"Erro ao buscar transicoes: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_comments(
        self,
        issue_key: str,
        **kwargs
    ) -> SkillResult:
        """
        Lista comentarios de uma issue

        Args:
            issue_key: Chave da issue

        Returns:
            SkillResult com lista de comentarios
        """
        try:
            issue = await self.jira.get_issue(issue_key)

            if not issue:
                return SkillResult(
                    success=False,
                    message=f"Issue {issue_key} nao encontrada"
                )

            fields = issue.get("fields", {})
            comment_data = fields.get("comment", {})
            comments = comment_data.get("comments", [])

            simplified = []
            for c in comments:
                author = c.get("author", {})
                simplified.append({
                    "id": c.get("id"),
                    "author": author.get("displayName"),
                    "author_email": author.get("emailAddress"),
                    "body": self._extract_comment_body(c.get("body")),
                    "created": c.get("created"),
                    "updated": c.get("updated")
                })

            return SkillResult(
                success=True,
                data=simplified,
                message=f"Encontrados {len(simplified)} comentarios"
            )

        except Exception as e:
            logger.error(f"Erro ao buscar comentarios: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== PROJETOS ====================

    async def list_projects(self, **kwargs) -> SkillResult:
        """
        Lista todos os projetos disponiveis

        Returns:
            SkillResult com lista de projetos
        """
        try:
            projects = await self.jira.get_projects()

            simplified = [
                {
                    "key": p.get("key"),
                    "name": p.get("name"),
                    "description": p.get("description", ""),
                    "lead": p.get("lead", {}).get("displayName"),
                    "project_type": p.get("projectTypeKey"),
                    "avatar_url": p.get("avatarUrls", {}).get("48x48")
                }
                for p in projects
            ]

            return SkillResult(
                success=True,
                data=simplified,
                message=f"Encontrados {len(simplified)} projetos"
            )

        except Exception as e:
            logger.error(f"Erro ao listar projetos: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_project(
        self,
        project_key: str,
        **kwargs
    ) -> SkillResult:
        """
        Obtem detalhes de um projeto

        Args:
            project_key: Chave do projeto

        Returns:
            SkillResult com dados do projeto
        """
        try:
            session = await self.jira._ensure_session()
            url = f"{self.jira.base_url}/project/{project_key}"

            async with session.get(url) as response:
                if response.status == 200:
                    project = await response.json()

                    simplified = {
                        "key": project.get("key"),
                        "id": project.get("id"),
                        "name": project.get("name"),
                        "description": project.get("description", ""),
                        "lead": project.get("lead", {}).get("displayName"),
                        "project_type": project.get("projectTypeKey"),
                        "issue_types": [
                            {"id": it.get("id"), "name": it.get("name")}
                            for it in project.get("issueTypes", [])
                        ],
                        "components": [
                            {"id": c.get("id"), "name": c.get("name")}
                            for c in project.get("components", [])
                        ],
                        "versions": [
                            {
                                "id": v.get("id"),
                                "name": v.get("name"),
                                "released": v.get("released", False)
                            }
                            for v in project.get("versions", [])
                        ]
                    }

                    return SkillResult(
                        success=True,
                        data=simplified,
                        message=f"Projeto {project_key} encontrado"
                    )
                elif response.status == 404:
                    return SkillResult(
                        success=False,
                        message=f"Projeto {project_key} nao encontrado"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao buscar projeto: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar projeto {project_key}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== USUARIOS ====================

    async def list_assignable_users(
        self,
        project_key: Optional[str] = None,
        issue_key: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Lista usuarios que podem ser assignados a issues

        Args:
            project_key: Chave do projeto
            issue_key: Chave de uma issue especifica

        Returns:
            SkillResult com lista de usuarios
        """
        try:
            session = await self.jira._ensure_session()

            if issue_key:
                url = f"{self.jira.base_url}/user/assignable/search"
                params = {"issueKey": issue_key}
            elif project_key:
                url = f"{self.jira.base_url}/user/assignable/search"
                params = {"project": project_key}
            else:
                project = self.jira.config.project_key
                if project:
                    url = f"{self.jira.base_url}/user/assignable/search"
                    params = {"project": project}
                else:
                    return SkillResult(
                        success=False,
                        message="Necessario fornecer project_key ou issue_key"
                    )

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    users = await response.json()

                    simplified = [
                        {
                            "account_id": u.get("accountId"),
                            "display_name": u.get("displayName"),
                            "email": u.get("emailAddress"),
                            "active": u.get("active", True),
                            "avatar_url": u.get("avatarUrls", {}).get("48x48")
                        }
                        for u in users
                    ]

                    return SkillResult(
                        success=True,
                        data=simplified,
                        message=f"Encontrados {len(simplified)} usuarios"
                    )
                else:
                    error = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao listar usuarios: {error}"
                    )

        except Exception as e:
            logger.error(f"Erro ao listar usuarios: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def get_user(
        self,
        account_id: Optional[str] = None,
        email: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Busca um usuario por ID ou email

        Args:
            account_id: Account ID do usuario
            email: Email do usuario

        Returns:
            SkillResult com dados do usuario
        """
        try:
            session = await self.jira._ensure_session()

            if account_id:
                url = f"{self.jira.base_url}/user"
                params = {"accountId": account_id}
            elif email:
                url = f"{self.jira.base_url}/user/search"
                params = {"query": email}
            else:
                return SkillResult(
                    success=False,
                    message="Necessario fornecer account_id ou email"
                )

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()

                    # Se foi busca por email, retorna lista
                    if email and isinstance(data, list):
                        if not data:
                            return SkillResult(
                                success=False,
                                message=f"Usuario com email {email} nao encontrado"
                            )
                        data = data[0]

                    simplified = {
                        "account_id": data.get("accountId"),
                        "display_name": data.get("displayName"),
                        "email": data.get("emailAddress"),
                        "active": data.get("active", True),
                        "timezone": data.get("timeZone"),
                        "locale": data.get("locale"),
                        "avatar_url": data.get("avatarUrls", {}).get("48x48")
                    }

                    return SkillResult(
                        success=True,
                        data=simplified,
                        message="Usuario encontrado"
                    )
                else:
                    return SkillResult(
                        success=False,
                        message="Usuario nao encontrado"
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar usuario: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== HELPERS ====================

    def _simplify_issue(self, issue: Dict) -> Dict:
        """
        Simplifica dados de uma issue para retorno

        Args:
            issue: Issue no formato Jira

        Returns:
            Dict simplificado
        """
        fields = issue.get("fields", {})

        # Extrai descricao (pode ser ADF)
        description = fields.get("description")
        if isinstance(description, dict):
            description = self.jira._extract_text_from_adf(description)

        # Status
        status_obj = fields.get("status", {})
        status = status_obj.get("name", "Unknown")
        status_category = status_obj.get("statusCategory", {}).get("key")

        # Prioridade
        priority_obj = fields.get("priority", {})
        priority = priority_obj.get("name", "Medium")

        # Assignee
        assignee_obj = fields.get("assignee", {})
        assignee = assignee_obj.get("displayName") if assignee_obj else None
        assignee_email = assignee_obj.get("emailAddress") if assignee_obj else None

        # Reporter
        reporter_obj = fields.get("reporter", {})
        reporter = reporter_obj.get("displayName") if reporter_obj else None

        # Tipo de issue
        issuetype_obj = fields.get("issuetype", {})
        issue_type = issuetype_obj.get("name", "Task")

        # Epic
        epic_key = fields.get("customfield_10014") or fields.get("parent", {}).get("key")

        # Story points
        story_points = fields.get("customfield_10016") or fields.get("customfield_10024")

        # Sprint (pode estar em campo customizado)
        sprint_data = fields.get("customfield_10020") or fields.get("customfield_10007")
        sprint = None
        if sprint_data and isinstance(sprint_data, list) and sprint_data:
            sprint_info = sprint_data[-1]  # Ultimo sprint
            if isinstance(sprint_info, dict):
                sprint = sprint_info.get("name")
            elif isinstance(sprint_info, str):
                # Parse do formato texto do sprint
                import re
                match = re.search(r'name=([^,\]]+)', sprint_info)
                if match:
                    sprint = match.group(1)

        return {
            "key": issue.get("key"),
            "id": issue.get("id"),
            "summary": fields.get("summary"),
            "description": description,
            "status": status,
            "status_category": status_category,
            "issue_type": issue_type,
            "priority": priority,
            "assignee": assignee,
            "assignee_email": assignee_email,
            "reporter": reporter,
            "labels": fields.get("labels", []),
            "components": [c.get("name") for c in fields.get("components", [])],
            "fix_versions": [v.get("name") for v in fields.get("fixVersions", [])],
            "epic_key": epic_key,
            "story_points": story_points,
            "sprint": sprint,
            "created": fields.get("created"),
            "updated": fields.get("updated"),
            "resolution_date": fields.get("resolutiondate"),
            "url": f"{self.jira.config.url}/browse/{issue.get('key')}"
        }

    def _extract_comment_body(self, body: Any) -> str:
        """
        Extrai texto do corpo do comentario (pode ser ADF)

        Args:
            body: Corpo do comentario

        Returns:
            str: Texto do comentario
        """
        if isinstance(body, str):
            return body
        elif isinstance(body, dict):
            return self.jira._extract_text_from_adf(body)
        return str(body) if body else ""

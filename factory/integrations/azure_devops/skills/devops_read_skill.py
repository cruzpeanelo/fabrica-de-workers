# -*- coding: utf-8 -*-
"""
Azure DevOps Read Skill
=======================
Skill para leitura de work items, repositorios e projetos do Azure DevOps.

Funcionalidades:
- Busca de work items por ID
- Pesquisa de work items com WIQL
- Listagem de projetos
- Informacoes de projeto

Uso pelos agentes:
    from factory.integrations.azure_devops.skills import DevOpsReadSkill

    skill = DevOpsReadSkill(azure_client)

    # Buscar work item
    result = await skill.get_work_item(123)

    # Pesquisar work items
    result = await skill.search_work_items(
        work_item_type="User Story",
        state="Active"
    )
"""

import logging
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


@dataclass
class SkillResult:
    """Resultado padrao de uma skill"""
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


class DevOpsReadSkill:
    """
    Skill de leitura para Azure DevOps.

    Fornece funcionalidades de leitura de work items,
    projetos e repositorios para agentes.
    """

    skill_name = "devops_read"
    skill_description = "Leitura de work items, projetos e repositorios do Azure DevOps"

    # Acoes disponiveis para Claude
    available_actions = [
        "get_work_item",
        "search_work_items",
        "get_project",
        "list_projects"
    ]

    # Schema de ferramenta compativel com Claude
    tool_schema = {
        "name": "devops_read",
        "description": "Leitura de dados do Azure DevOps (work items, projetos, repositorios)",
        "input_schema": {
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "description": "Acao a executar",
                    "enum": ["get_work_item", "search_work_items", "get_project", "list_projects"]
                },
                "params": {
                    "type": "object",
                    "description": "Parametros da acao",
                    "properties": {
                        "work_item_id": {
                            "type": "integer",
                            "description": "ID do work item (para get_work_item)"
                        },
                        "work_item_type": {
                            "type": "string",
                            "description": "Tipo do work item (User Story, Bug, Task, etc)"
                        },
                        "state": {
                            "type": "string",
                            "description": "Estado do work item (New, Active, Resolved, Closed)"
                        },
                        "wiql": {
                            "type": "string",
                            "description": "Query WIQL customizada"
                        },
                        "max_results": {
                            "type": "integer",
                            "description": "Maximo de resultados (padrao: 50)"
                        },
                        "project_name": {
                            "type": "string",
                            "description": "Nome do projeto"
                        }
                    }
                }
            },
            "required": ["action"]
        }
    }

    def __init__(self, azure_client):
        """
        Inicializa a skill.

        Args:
            azure_client: AzureDevOpsIntegration autenticado
        """
        self.azure = azure_client

    # ==================== WORK ITEMS ====================

    async def get_work_item(self, work_item_id: int) -> SkillResult:
        """
        Busca um work item especifico por ID.

        Args:
            work_item_id: ID do work item

        Returns:
            SkillResult com dados do work item
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            work_item = await self.azure.get_work_item(work_item_id)

            if work_item:
                # Extrai campos relevantes
                fields = work_item.get("fields", {})
                simplified = {
                    "id": work_item.get("id"),
                    "rev": work_item.get("rev"),
                    "url": work_item.get("url"),
                    "title": fields.get("System.Title"),
                    "description": fields.get("System.Description"),
                    "work_item_type": fields.get("System.WorkItemType"),
                    "state": fields.get("System.State"),
                    "reason": fields.get("System.Reason"),
                    "priority": fields.get("Microsoft.VSTS.Common.Priority"),
                    "story_points": fields.get("Microsoft.VSTS.Scheduling.StoryPoints"),
                    "assigned_to": self._get_identity_name(fields.get("System.AssignedTo")),
                    "created_by": self._get_identity_name(fields.get("System.CreatedBy")),
                    "created_date": fields.get("System.CreatedDate"),
                    "changed_date": fields.get("System.ChangedDate"),
                    "area_path": fields.get("System.AreaPath"),
                    "iteration_path": fields.get("System.IterationPath"),
                    "tags": fields.get("System.Tags", "").split("; ") if fields.get("System.Tags") else [],
                    "acceptance_criteria": fields.get("Microsoft.VSTS.Common.AcceptanceCriteria")
                }

                return SkillResult(
                    success=True,
                    data=simplified,
                    message=f"Work item {work_item_id} encontrado: {simplified['title']}"
                )
            else:
                return SkillResult(
                    success=False,
                    message=f"Work item {work_item_id} nao encontrado",
                    errors=[f"Work item {work_item_id} nao existe ou sem permissao"]
                )

        except Exception as e:
            logger.error(f"Erro ao buscar work item {work_item_id}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao buscar work item: {str(e)}",
                errors=[str(e)]
            )

    async def search_work_items(
        self,
        work_item_type: Optional[str] = None,
        state: Optional[str] = None,
        wiql: Optional[str] = None,
        max_results: int = 50
    ) -> SkillResult:
        """
        Pesquisa work items usando filtros ou WIQL.

        Args:
            work_item_type: Filtrar por tipo (User Story, Bug, etc)
            state: Filtrar por estado (New, Active, Resolved, Closed)
            wiql: Query WIQL customizada (sobrescreve outros filtros)
            max_results: Maximo de resultados

        Returns:
            SkillResult com lista de work items
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            work_items = await self.azure.query_work_items(
                wiql=wiql,
                work_item_type=work_item_type,
                state=state,
                max_results=max_results
            )

            # Simplifica resposta
            simplified = []
            for wi in work_items:
                fields = wi.get("fields", {})
                simplified.append({
                    "id": wi.get("id"),
                    "title": fields.get("System.Title"),
                    "work_item_type": fields.get("System.WorkItemType"),
                    "state": fields.get("System.State"),
                    "priority": fields.get("Microsoft.VSTS.Common.Priority"),
                    "assigned_to": self._get_identity_name(fields.get("System.AssignedTo")),
                    "story_points": fields.get("Microsoft.VSTS.Scheduling.StoryPoints"),
                    "changed_date": fields.get("System.ChangedDate")
                })

            filters_desc = []
            if work_item_type:
                filters_desc.append(f"tipo={work_item_type}")
            if state:
                filters_desc.append(f"estado={state}")
            if wiql:
                filters_desc.append("WIQL customizado")

            filter_msg = f" ({', '.join(filters_desc)})" if filters_desc else ""

            return SkillResult(
                success=True,
                data=simplified,
                message=f"Encontrados {len(simplified)} work items{filter_msg}"
            )

        except Exception as e:
            logger.error(f"Erro ao pesquisar work items: {e}")
            return SkillResult(
                success=False,
                message=f"Erro na pesquisa: {str(e)}",
                errors=[str(e)]
            )

    # ==================== PROJETOS ====================

    async def get_project(self, project_name: Optional[str] = None) -> SkillResult:
        """
        Obtem informacoes de um projeto.

        Args:
            project_name: Nome do projeto (usa projeto atual se nao informado)

        Returns:
            SkillResult com dados do projeto
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            # Usa projeto configurado se nao informado
            target_project = project_name or self.azure.config.project

            session = await self.azure._ensure_session()
            url = f"{self.azure.base_url}/_apis/projects/{target_project}"
            params = {"api-version": self.azure.API_VERSION}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    project = await response.json()

                    simplified = {
                        "id": project.get("id"),
                        "name": project.get("name"),
                        "description": project.get("description"),
                        "url": project.get("url"),
                        "state": project.get("state"),
                        "visibility": project.get("visibility"),
                        "revision": project.get("revision"),
                        "last_update_time": project.get("lastUpdateTime")
                    }

                    return SkillResult(
                        success=True,
                        data=simplified,
                        message=f"Projeto {target_project} encontrado"
                    )
                elif response.status == 404:
                    return SkillResult(
                        success=False,
                        message=f"Projeto {target_project} nao encontrado",
                        errors=["Projeto nao existe"]
                    )
                else:
                    error_text = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao buscar projeto: {response.status}",
                        errors=[error_text]
                    )

        except Exception as e:
            logger.error(f"Erro ao buscar projeto {project_name}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao buscar projeto: {str(e)}",
                errors=[str(e)]
            )

    async def list_projects(self) -> SkillResult:
        """
        Lista todos os projetos da organizacao.

        Returns:
            SkillResult com lista de projetos
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            session = await self.azure._ensure_session()
            url = f"{self.azure.base_url}/_apis/projects"
            params = {"api-version": self.azure.API_VERSION}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    projects = data.get("value", [])

                    simplified = []
                    for p in projects:
                        simplified.append({
                            "id": p.get("id"),
                            "name": p.get("name"),
                            "description": p.get("description"),
                            "state": p.get("state"),
                            "visibility": p.get("visibility"),
                            "last_update_time": p.get("lastUpdateTime")
                        })

                    return SkillResult(
                        success=True,
                        data=simplified,
                        message=f"Encontrados {len(simplified)} projetos"
                    )
                else:
                    error_text = await response.text()
                    return SkillResult(
                        success=False,
                        message=f"Erro ao listar projetos: {response.status}",
                        errors=[error_text]
                    )

        except Exception as e:
            logger.error(f"Erro ao listar projetos: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao listar projetos: {str(e)}",
                errors=[str(e)]
            )

    # ==================== HELPERS ====================

    def _get_identity_name(self, identity: Any) -> Optional[str]:
        """Extrai nome de uma identidade Azure DevOps"""
        if isinstance(identity, dict):
            return identity.get("displayName")
        return None

    # ==================== SKILL INTERFACE ====================

    async def execute(
        self,
        action: str,
        params: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Executa uma acao do skill.

        Args:
            action: Acao a executar (get_work_item, search_work_items, etc)
            params: Parametros da acao

        Returns:
            Resultado da acao como dicionario
        """
        params = params or {}

        actions = {
            "get_work_item": self._action_get_work_item,
            "search_work_items": self._action_search_work_items,
            "get_project": self._action_get_project,
            "list_projects": self._action_list_projects
        }

        handler = actions.get(action)
        if not handler:
            return {
                "success": False,
                "error": f"Acao desconhecida: {action}. Acoes disponiveis: {', '.join(self.available_actions)}"
            }

        try:
            result = await handler(params)
            return result.to_dict()
        except Exception as e:
            logger.error(f"Erro ao executar skill {action}: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    async def _action_get_work_item(self, params: Dict) -> SkillResult:
        """Acao para buscar work item"""
        work_item_id = params.get("work_item_id")
        if not work_item_id:
            return SkillResult(
                success=False,
                message="Parametro work_item_id obrigatorio",
                errors=["work_item_id nao informado"]
            )
        return await self.get_work_item(int(work_item_id))

    async def _action_search_work_items(self, params: Dict) -> SkillResult:
        """Acao para pesquisar work items"""
        return await self.search_work_items(
            work_item_type=params.get("work_item_type"),
            state=params.get("state"),
            wiql=params.get("wiql"),
            max_results=params.get("max_results", 50)
        )

    async def _action_get_project(self, params: Dict) -> SkillResult:
        """Acao para buscar projeto"""
        return await self.get_project(
            project_name=params.get("project_name")
        )

    async def _action_list_projects(self, params: Dict) -> SkillResult:
        """Acao para listar projetos"""
        return await self.list_projects()

# -*- coding: utf-8 -*-
"""
Jira Sync Skill
===============
Skill para sincronizacao bidirecional entre Jira e Stories.

Funcionalidades:
- Sincronizar issues Jira -> Stories locais
- Sincronizar Stories locais -> Jira
- Mapeamento de status e prioridades
- Sincronizacao de comentarios
- Deteccao de conflitos

Uso pelos agentes:
    from factory.integrations.jira.skills import JiraSyncSkill

    skill = JiraSyncSkill(jira_client)

    # Importar issues do Jira para stories locais
    result = await skill.import_from_jira(project_key="PROJ")

    # Exportar story para Jira
    result = await skill.export_to_jira(story)

    # Sincronizar bidirecional
    result = await skill.sync_bidirectional(project_key="PROJ", project_id="local-123")

Autor: Fabrica de Agentes
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class SyncDirection(str, Enum):
    """Direcao da sincronizacao"""
    JIRA_TO_LOCAL = "jira_to_local"
    LOCAL_TO_JIRA = "local_to_jira"
    BIDIRECTIONAL = "bidirectional"


class SyncStrategy(str, Enum):
    """Estrategia para resolucao de conflitos"""
    JIRA_WINS = "jira_wins"
    LOCAL_WINS = "local_wins"
    NEWEST_WINS = "newest_wins"
    MANUAL = "manual"


class SyncStatus(str, Enum):
    """Status de um item sincronizado"""
    SYNCED = "synced"
    PENDING = "pending"
    CONFLICT = "conflict"
    ERROR = "error"
    NEW = "new"


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
class SyncItemResult:
    """Resultado de sincronizacao de um item"""
    local_id: Optional[str] = None
    jira_key: Optional[str] = None
    status: SyncStatus = SyncStatus.PENDING
    direction: Optional[SyncDirection] = None
    message: str = ""
    changes: Dict[str, Any] = field(default_factory=dict)


@dataclass
class SyncResult:
    """Resultado completo de uma sincronizacao"""
    success: bool
    direction: SyncDirection
    total_items: int = 0
    synced: int = 0
    created: int = 0
    updated: int = 0
    conflicts: int = 0
    errors: int = 0
    items: List[SyncItemResult] = field(default_factory=list)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    error_messages: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "success": self.success,
            "direction": self.direction.value,
            "total_items": self.total_items,
            "synced": self.synced,
            "created": self.created,
            "updated": self.updated,
            "conflicts": self.conflicts,
            "errors": self.errors,
            "items": [
                {
                    "local_id": item.local_id,
                    "jira_key": item.jira_key,
                    "status": item.status.value,
                    "message": item.message
                }
                for item in self.items
            ],
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "error_messages": self.error_messages
        }


class JiraSyncSkill:
    """
    Skill de sincronizacao bidirecional Jira <-> Stories

    Permite importar issues do Jira como stories locais,
    exportar stories para o Jira, e manter sincronizacao
    bidirecional com deteccao de conflitos.

    Exemplo de uso:
    ```python
    from factory.integrations.jira import get_jira_integration

    jira = get_jira_integration()
    await jira.connect()

    skill = JiraSyncSkill(jira)

    # Importar todas as stories de um projeto
    result = await skill.import_from_jira(
        project_key="PROJ",
        issue_types=["Story", "Bug"],
        status_filter=["To Do", "In Progress"]
    )

    # Exportar story para Jira
    story = {"title": "Nova feature", "description": "..."}
    result = await skill.export_to_jira(story)
    ```
    """

    # Schema Claude-compatible para tools
    TOOL_SCHEMA = {
        "name": "jira_sync",
        "description": "Bidirectional synchronization between Jira issues and local stories",
        "input_schema": {
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "enum": [
                        "import_from_jira",
                        "export_to_jira",
                        "sync_bidirectional",
                        "sync_single_issue",
                        "sync_single_story",
                        "check_sync_status",
                        "resolve_conflict"
                    ],
                    "description": "The sync action to perform"
                },
                "project_key": {
                    "type": "string",
                    "description": "Jira project key"
                },
                "project_id": {
                    "type": "string",
                    "description": "Local project ID"
                },
                "issue_key": {
                    "type": "string",
                    "description": "Jira issue key for single sync"
                },
                "story_id": {
                    "type": "string",
                    "description": "Local story ID for single sync"
                },
                "story": {
                    "type": "object",
                    "description": "Story data to export"
                },
                "strategy": {
                    "type": "string",
                    "enum": ["jira_wins", "local_wins", "newest_wins", "manual"],
                    "default": "newest_wins",
                    "description": "Conflict resolution strategy"
                },
                "issue_types": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Issue types to sync"
                },
                "max_results": {
                    "type": "integer",
                    "default": 100,
                    "description": "Maximum issues to sync"
                }
            },
            "required": ["action"]
        }
    }

    # Acoes disponiveis
    AVAILABLE_ACTIONS = [
        "import_from_jira",
        "export_to_jira",
        "sync_bidirectional",
        "sync_single_issue",
        "sync_single_story",
        "check_sync_status",
        "resolve_conflict"
    ]

    # Mapeamento de status Jira -> interno
    STATUS_MAP_TO_INTERNAL = {
        "To Do": "backlog",
        "Open": "backlog",
        "Backlog": "backlog",
        "Ready": "ready",
        "Ready for Dev": "ready",
        "In Progress": "in_progress",
        "In Development": "in_progress",
        "In Review": "review",
        "Code Review": "review",
        "Testing": "testing",
        "QA": "testing",
        "Done": "done",
        "Closed": "done",
        "Resolved": "done"
    }

    # Mapeamento de status interno -> Jira
    STATUS_MAP_TO_JIRA = {
        "backlog": "To Do",
        "ready": "Ready",
        "in_progress": "In Progress",
        "review": "In Review",
        "testing": "Testing",
        "done": "Done"
    }

    # Mapeamento de prioridade Jira -> interno
    PRIORITY_MAP_TO_INTERNAL = {
        "Highest": "urgent",
        "High": "high",
        "Medium": "medium",
        "Low": "low",
        "Lowest": "low"
    }

    # Mapeamento de prioridade interno -> Jira
    PRIORITY_MAP_TO_JIRA = {
        "urgent": "Highest",
        "high": "High",
        "medium": "Medium",
        "low": "Low"
    }

    def __init__(self, jira_client, story_repository=None):
        """
        Inicializa a skill

        Args:
            jira_client: JiraIntegration autenticado
            story_repository: Repositorio de stories (opcional)
        """
        self.jira = jira_client
        self.story_repo = story_repository
        self._sync_map: Dict[str, str] = {}  # local_id -> jira_key

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
            "import_from_jira": self.import_from_jira,
            "export_to_jira": self.export_to_jira,
            "sync_bidirectional": self.sync_bidirectional,
            "sync_single_issue": self.sync_single_issue,
            "sync_single_story": self.sync_single_story,
            "check_sync_status": self.check_sync_status,
            "resolve_conflict": self.resolve_conflict
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

            if isinstance(result, SyncResult):
                return SkillResult(
                    success=result.success,
                    data=result.to_dict(),
                    message=f"Sync: {result.synced} items, {result.created} created, {result.updated} updated"
                )
            return result

        except Exception as e:
            logger.error(f"Erro ao executar acao {action}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao executar {action}",
                errors=[str(e)]
            )

    # ==================== IMPORT ====================

    async def import_from_jira(
        self,
        project_key: Optional[str] = None,
        jql: Optional[str] = None,
        issue_types: Optional[List[str]] = None,
        status_filter: Optional[List[str]] = None,
        max_results: int = 100,
        project_id: Optional[str] = None,
        **kwargs
    ) -> SyncResult:
        """
        Importa issues do Jira como stories locais

        Args:
            project_key: Chave do projeto Jira
            jql: Query JQL customizada
            issue_types: Tipos de issue para importar
            status_filter: Filtrar por status
            max_results: Maximo de issues
            project_id: ID do projeto local para associar

        Returns:
            SyncResult com resultado da importacao
        """
        result = SyncResult(
            success=True,
            direction=SyncDirection.JIRA_TO_LOCAL,
            started_at=datetime.utcnow()
        )

        try:
            # Constroi JQL
            if not jql:
                jql_parts = []

                if project_key:
                    jql_parts.append(f"project = {project_key}")
                elif self.jira.config.project_key:
                    jql_parts.append(f"project = {self.jira.config.project_key}")

                if issue_types:
                    types_str = ", ".join([f"'{t}'" for t in issue_types])
                    jql_parts.append(f"issuetype IN ({types_str})")

                if status_filter:
                    status_str = ", ".join([f"'{s}'" for s in status_filter])
                    jql_parts.append(f"status IN ({status_str})")

                jql = " AND ".join(jql_parts) + " ORDER BY updated DESC"

            # Busca issues
            issues = await self.jira.search_issues(jql=jql, max_results=max_results)
            result.total_items = len(issues)

            for issue in issues:
                try:
                    # Converte issue para story
                    story_data = self._jira_issue_to_story(issue)

                    if project_id:
                        story_data["project_id"] = project_id

                    item_result = SyncItemResult(
                        jira_key=issue.get("key"),
                        direction=SyncDirection.JIRA_TO_LOCAL
                    )

                    # Verifica se ja existe story sincronizada
                    existing_id = self._find_local_by_jira_key(issue.get("key"))

                    if existing_id:
                        # Atualizar existente
                        story_data["story_id"] = existing_id
                        item_result.local_id = existing_id
                        item_result.status = SyncStatus.SYNCED
                        item_result.message = "Updated"
                        result.updated += 1
                    else:
                        # Criar novo
                        item_result.status = SyncStatus.NEW
                        item_result.message = "Created"
                        result.created += 1

                    item_result.changes = story_data
                    result.items.append(item_result)
                    result.synced += 1

                except Exception as e:
                    logger.error(f"Erro ao importar {issue.get('key')}: {e}")
                    result.items.append(SyncItemResult(
                        jira_key=issue.get("key"),
                        status=SyncStatus.ERROR,
                        message=str(e)
                    ))
                    result.errors += 1
                    result.error_messages.append(f"{issue.get('key')}: {str(e)}")

        except Exception as e:
            logger.error(f"Erro na importacao: {e}")
            result.success = False
            result.error_messages.append(str(e))

        result.completed_at = datetime.utcnow()
        return result

    # ==================== EXPORT ====================

    async def export_to_jira(
        self,
        story: Optional[Dict] = None,
        stories: Optional[List[Dict]] = None,
        update_if_exists: bool = True,
        **kwargs
    ) -> SyncResult:
        """
        Exporta stories para o Jira

        Args:
            story: Story individual para exportar
            stories: Lista de stories para exportar
            update_if_exists: Atualizar se ja existir no Jira

        Returns:
            SyncResult com resultado da exportacao
        """
        result = SyncResult(
            success=True,
            direction=SyncDirection.LOCAL_TO_JIRA,
            started_at=datetime.utcnow()
        )

        # Normaliza para lista
        if story:
            stories = [story]
        elif not stories:
            return SyncResult(
                success=False,
                direction=SyncDirection.LOCAL_TO_JIRA,
                error_messages=["Nenhuma story fornecida"]
            )

        result.total_items = len(stories)

        for story_data in stories:
            try:
                item_result = SyncItemResult(
                    local_id=story_data.get("story_id"),
                    direction=SyncDirection.LOCAL_TO_JIRA
                )

                # Verifica se ja existe no Jira
                jira_key = story_data.get("external_id")

                if not jira_key and story_data.get("external_system") == "jira":
                    jira_key = self._find_jira_by_local_id(story_data.get("story_id"))

                if jira_key and update_if_exists:
                    # Atualizar issue existente
                    issue_data = self._story_to_jira_issue(story_data)
                    success = await self.jira.update_issue(jira_key, issue_data)

                    if success:
                        item_result.jira_key = jira_key
                        item_result.status = SyncStatus.SYNCED
                        item_result.message = "Updated"
                        result.updated += 1
                        result.synced += 1
                    else:
                        item_result.status = SyncStatus.ERROR
                        item_result.message = "Failed to update"
                        result.errors += 1

                else:
                    # Criar nova issue
                    issue_data = self._story_to_jira_issue(story_data)
                    created = await self.jira.create_issue(issue_data)

                    if created:
                        jira_key = created.get("key")
                        item_result.jira_key = jira_key
                        item_result.status = SyncStatus.NEW
                        item_result.message = "Created"
                        result.created += 1
                        result.synced += 1

                        # Registra mapeamento
                        if story_data.get("story_id"):
                            self._sync_map[story_data["story_id"]] = jira_key
                    else:
                        item_result.status = SyncStatus.ERROR
                        item_result.message = "Failed to create"
                        result.errors += 1

                result.items.append(item_result)

            except Exception as e:
                logger.error(f"Erro ao exportar story: {e}")
                result.items.append(SyncItemResult(
                    local_id=story_data.get("story_id"),
                    status=SyncStatus.ERROR,
                    message=str(e)
                ))
                result.errors += 1
                result.error_messages.append(str(e))

        result.completed_at = datetime.utcnow()
        return result

    # ==================== BIDIRECTIONAL ====================

    async def sync_bidirectional(
        self,
        project_key: Optional[str] = None,
        project_id: Optional[str] = None,
        strategy: SyncStrategy = SyncStrategy.NEWEST_WINS,
        max_results: int = 100,
        **kwargs
    ) -> SyncResult:
        """
        Sincroniza bidirecionalmente entre Jira e stories locais

        Args:
            project_key: Chave do projeto Jira
            project_id: ID do projeto local
            strategy: Estrategia de resolucao de conflitos
            max_results: Maximo de items

        Returns:
            SyncResult com resultado da sincronizacao
        """
        result = SyncResult(
            success=True,
            direction=SyncDirection.BIDIRECTIONAL,
            started_at=datetime.utcnow()
        )

        try:
            # Importa do Jira
            import_result = await self.import_from_jira(
                project_key=project_key,
                project_id=project_id,
                max_results=max_results
            )

            # Consolida resultados
            result.synced = import_result.synced
            result.created = import_result.created
            result.updated = import_result.updated
            result.errors = import_result.errors
            result.items.extend(import_result.items)
            result.error_messages.extend(import_result.error_messages)

            # Buscar stories locais que precisam ser exportadas - Issue #335
            if self.story_repo and project_id:
                try:
                    # Busca stories do projeto que não têm referência Jira ou foram modificadas
                    local_stories = await self._get_stories_for_export(project_id, project_key)

                    for story in local_stories:
                        export_result = await self.export_to_jira(
                            story_data=story,
                            project_key=project_key
                        )

                        if export_result.success:
                            result.synced += 1
                            if export_result.data.get("created"):
                                result.created += 1
                            else:
                                result.updated += 1
                            result.items.append(export_result.data)
                        else:
                            result.errors += 1
                            result.error_messages.extend(export_result.errors or [])

                except Exception as e:
                    logger.warning(f"Erro ao exportar stories locais: {e}")
                    result.error_messages.append(f"Falha na exportacao: {str(e)}")

            result.total_items = len(result.items)

        except Exception as e:
            logger.error(f"Erro na sincronizacao bidirecional: {e}")
            result.success = False
            result.error_messages.append(str(e))

        result.completed_at = datetime.utcnow()
        return result

    # ==================== SINGLE ITEM SYNC ====================

    async def sync_single_issue(
        self,
        issue_key: str,
        project_id: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Sincroniza uma unica issue do Jira

        Args:
            issue_key: Chave da issue Jira
            project_id: ID do projeto local

        Returns:
            SkillResult com story sincronizada
        """
        try:
            issue = await self.jira.get_issue(issue_key)

            if not issue:
                return SkillResult(
                    success=False,
                    message=f"Issue {issue_key} nao encontrada"
                )

            story_data = self._jira_issue_to_story(issue)

            if project_id:
                story_data["project_id"] = project_id

            return SkillResult(
                success=True,
                data=story_data,
                message=f"Issue {issue_key} sincronizada"
            )

        except Exception as e:
            logger.error(f"Erro ao sincronizar issue {issue_key}: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def sync_single_story(
        self,
        story: Dict,
        create_if_not_exists: bool = True,
        **kwargs
    ) -> SkillResult:
        """
        Sincroniza uma unica story para o Jira

        Args:
            story: Dados da story
            create_if_not_exists: Criar se nao existir no Jira

        Returns:
            SkillResult com resultado
        """
        try:
            jira_key = story.get("external_id")

            if jira_key:
                # Atualizar existente
                issue_data = self._story_to_jira_issue(story)
                success = await self.jira.update_issue(jira_key, issue_data)

                if success:
                    return SkillResult(
                        success=True,
                        data={"jira_key": jira_key, "action": "updated"},
                        message=f"Issue {jira_key} atualizada"
                    )
                else:
                    return SkillResult(
                        success=False,
                        message=f"Falha ao atualizar {jira_key}"
                    )
            elif create_if_not_exists:
                # Criar nova
                issue_data = self._story_to_jira_issue(story)
                created = await self.jira.create_issue(issue_data)

                if created:
                    return SkillResult(
                        success=True,
                        data={
                            "jira_key": created.get("key"),
                            "jira_id": created.get("id"),
                            "action": "created"
                        },
                        message=f"Issue {created.get('key')} criada"
                    )
                else:
                    return SkillResult(
                        success=False,
                        message="Falha ao criar issue"
                    )
            else:
                return SkillResult(
                    success=False,
                    message="Story nao tem external_id e create_if_not_exists=False"
                )

        except Exception as e:
            logger.error(f"Erro ao sincronizar story: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== STATUS ====================

    async def check_sync_status(
        self,
        issue_key: Optional[str] = None,
        story_id: Optional[str] = None,
        **kwargs
    ) -> SkillResult:
        """
        Verifica status de sincronizacao de um item

        Args:
            issue_key: Chave da issue Jira
            story_id: ID da story local

        Returns:
            SkillResult com status
        """
        try:
            status_data = {
                "synced": False,
                "jira_key": issue_key,
                "story_id": story_id,
                "last_sync": None
            }

            if issue_key and story_id:
                # Verifica se estao mapeados
                mapped_key = self._sync_map.get(story_id)
                status_data["synced"] = mapped_key == issue_key
            elif issue_key:
                # Busca story local
                local_id = self._find_local_by_jira_key(issue_key)
                if local_id:
                    status_data["synced"] = True
                    status_data["story_id"] = local_id
            elif story_id:
                # Busca issue Jira
                jira_key = self._sync_map.get(story_id)
                if jira_key:
                    status_data["synced"] = True
                    status_data["jira_key"] = jira_key

            return SkillResult(
                success=True,
                data=status_data,
                message="Synced" if status_data["synced"] else "Not synced"
            )

        except Exception as e:
            logger.error(f"Erro ao verificar status: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    async def resolve_conflict(
        self,
        issue_key: str,
        story_id: str,
        strategy: SyncStrategy = SyncStrategy.NEWEST_WINS,
        **kwargs
    ) -> SkillResult:
        """
        Resolve conflito de sincronizacao

        Args:
            issue_key: Chave da issue Jira
            story_id: ID da story local
            strategy: Estrategia de resolucao

        Returns:
            SkillResult com resolucao
        """
        try:
            # Busca dados de ambos
            jira_issue = await self.jira.get_issue(issue_key)

            if not jira_issue:
                return SkillResult(
                    success=False,
                    message=f"Issue {issue_key} nao encontrada"
                )

            # Buscar story local do repositorio - Issue #335
            local_story = None
            if self.story_repo and story_id:
                try:
                    local_story = await self.story_repo.get_by_id(story_id)
                except Exception as e:
                    logger.warning(f"Erro ao buscar story local {story_id}: {e}")

            # Determina vencedor baseado na estrategia
            jira_data = self._jira_issue_to_story(jira_issue)
            winner = "jira"  # default

            if strategy == SyncStrategy.JIRA_WINS:
                winner = "jira"
            elif strategy == SyncStrategy.LOCAL_WINS:
                winner = "local"
            elif strategy == SyncStrategy.NEWEST_WINS and local_story:
                # Compara timestamps
                jira_updated = jira_issue.get("fields", {}).get("updated")
                local_updated = getattr(local_story, "updated_at", None)
                if local_updated and jira_updated:
                    from dateutil.parser import parse as parse_date
                    jira_time = parse_date(jira_updated)
                    if local_updated.replace(tzinfo=None) > jira_time.replace(tzinfo=None):
                        winner = "local"

            resolution = {
                "winner": winner,
                "strategy": strategy.value,
                "issue_key": issue_key,
                "story_id": story_id
            }

            if winner == "jira":
                resolution["data"] = jira_data
                resolution["data"]["story_id"] = story_id
            elif winner == "local" and local_story:
                resolution["data"] = self._story_to_dict(local_story)
                resolution["data"]["jira_key"] = issue_key

            if strategy == SyncStrategy.MANUAL:
                resolution["requires_manual_resolution"] = True
                resolution["jira_data"] = jira_data
                if local_story:
                    resolution["local_data"] = self._story_to_dict(local_story)

            return SkillResult(
                success=True,
                data=resolution,
                message=f"Conflito resolvido usando estrategia {strategy.value}"
            )

        except Exception as e:
            logger.error(f"Erro ao resolver conflito: {e}")
            return SkillResult(
                success=False,
                errors=[str(e)]
            )

    # ==================== CONVERSAO ====================

    def _jira_issue_to_story(self, issue: Dict) -> Dict:
        """
        Converte issue Jira para formato Story interno

        Args:
            issue: Issue no formato Jira

        Returns:
            Dict: Story no formato interno
        """
        fields = issue.get("fields", {})

        # Extrai descricao
        description = fields.get("description", "")
        if isinstance(description, dict):
            description = self.jira._extract_text_from_adf(description)

        # Status
        status_obj = fields.get("status", {})
        jira_status = status_obj.get("name", "To Do")
        internal_status = self.STATUS_MAP_TO_INTERNAL.get(jira_status, "backlog")

        # Prioridade
        priority_obj = fields.get("priority", {})
        jira_priority = priority_obj.get("name", "Medium")
        internal_priority = self.PRIORITY_MAP_TO_INTERNAL.get(jira_priority, "medium")

        # Story Points
        story_points = fields.get("customfield_10016") or fields.get("customfield_10024") or 0

        # Assignee
        assignee_obj = fields.get("assignee", {})
        assignee = assignee_obj.get("displayName") if assignee_obj else None

        # Epic
        epic_link = fields.get("customfield_10014") or fields.get("parent", {}).get("key")

        # Extrai persona/action/benefit da descricao se possivel
        persona, action, benefit = self._parse_user_story(description)

        # Criterios de aceite
        acceptance_criteria = self._extract_acceptance_criteria(description)

        return {
            "external_id": issue.get("key"),
            "external_system": "jira",
            "external_url": f"{self.jira.config.url}/browse/{issue.get('key')}",
            "title": fields.get("summary", ""),
            "description": description,
            "persona": persona,
            "action": action,
            "benefit": benefit,
            "status": internal_status,
            "priority": internal_priority,
            "story_points": int(story_points) if story_points else 0,
            "assignee": assignee,
            "epic_id": epic_link,
            "tags": fields.get("labels", []),
            "acceptance_criteria": acceptance_criteria,
            "created_at": fields.get("created"),
            "updated_at": fields.get("updated"),
            "jira_data": {
                "issue_type": fields.get("issuetype", {}).get("name"),
                "project_key": fields.get("project", {}).get("key"),
                "reporter": fields.get("reporter", {}).get("displayName") if fields.get("reporter") else None,
                "components": [c.get("name") for c in fields.get("components", [])],
                "fix_versions": [v.get("name") for v in fields.get("fixVersions", [])]
            }
        }

    def _story_to_jira_issue(self, story: Dict) -> Dict:
        """
        Converte Story interno para formato Jira

        Args:
            story: Story no formato interno

        Returns:
            Dict: Issue no formato Jira
        """
        # Monta descricao
        description_parts = []

        if story.get("description"):
            description_parts.append(story["description"])

        # Adiciona narrativa Agile
        if story.get("persona") or story.get("action") or story.get("benefit"):
            narrative = f"\n\n*User Story:*\nComo um {story.get('persona', '[persona]')}, "
            narrative += f"eu quero {story.get('action', '[acao]')}, "
            narrative += f"para que {story.get('benefit', '[beneficio]')}."
            description_parts.append(narrative)

        # Criterios de aceite
        if story.get("acceptance_criteria"):
            ac_text = "\n\n*Criterios de Aceite:*\n"
            for ac in story["acceptance_criteria"]:
                ac_text += f"* {ac}\n"
            description_parts.append(ac_text)

        # Definition of Done
        if story.get("definition_of_done"):
            dod_text = "\n\n*Definition of Done:*\n"
            for dod in story["definition_of_done"]:
                dod_text += f"* {dod}\n"
            description_parts.append(dod_text)

        description = "".join(description_parts)

        # Monta issue
        issue_data = {
            "fields": {
                "project": {"key": self.jira.config.project_key},
                "summary": story.get("title", "")[:255],
                "description": description,
                "issuetype": {"name": self._get_issue_type(story)},
                "labels": story.get("tags", [])
            }
        }

        # Prioridade
        if story.get("priority"):
            jira_priority = self.PRIORITY_MAP_TO_JIRA.get(story["priority"], "Medium")
            issue_data["fields"]["priority"] = {"name": jira_priority}

        # Story points
        if story.get("story_points"):
            issue_data["fields"]["customfield_10016"] = story["story_points"]

        return issue_data

    def _parse_user_story(self, description: str) -> tuple:
        """
        Tenta extrair persona/action/benefit da descricao

        Args:
            description: Descricao da issue

        Returns:
            Tuple (persona, action, benefit)
        """
        import re

        persona = None
        action = None
        benefit = None

        # Padroes comuns
        patterns = [
            r"[Cc]omo\s+(?:um|uma)\s+(.+?),?\s+[Ee]u\s+quero\s+(.+?),?\s+[Pp]ara\s+que\s+(.+?)(?:\.|$)",
            r"[Aa]s\s+(?:a|an)\s+(.+?),?\s+[Ii]\s+want\s+(?:to\s+)?(.+?),?\s+[Ss]o\s+that\s+(.+?)(?:\.|$)"
        ]

        for pattern in patterns:
            match = re.search(pattern, description, re.IGNORECASE | re.DOTALL)
            if match:
                persona = match.group(1).strip()
                action = match.group(2).strip()
                benefit = match.group(3).strip()
                break

        return (persona, action, benefit)

    def _extract_acceptance_criteria(self, description: str) -> List[str]:
        """
        Extrai criterios de aceite da descricao

        Args:
            description: Descricao da issue

        Returns:
            Lista de criterios
        """
        criteria = []

        lines = description.split("\n")
        in_ac_section = False

        for line in lines:
            lower_line = line.lower()
            if "acceptance" in lower_line and ("criteria" in lower_line or "criterios" in lower_line):
                in_ac_section = True
                continue
            elif "definition" in lower_line and "done" in lower_line:
                in_ac_section = False
                continue

            if in_ac_section:
                stripped = line.strip()
                if stripped.startswith(("-", "*", "+")):
                    criteria.append(stripped.lstrip("-*+ "))
                elif stripped.startswith(("Given", "When", "Then", "And")):
                    criteria.append(stripped)

        return criteria

    def _get_issue_type(self, story: Dict) -> str:
        """
        Determina tipo de issue baseado na story

        Args:
            story: Dados da story

        Returns:
            Nome do tipo de issue
        """
        category = story.get("category", "feature")

        mapping = {
            "feature": "Story",
            "bug": "Bug",
            "tech_debt": "Task",
            "spike": "Task",
            "improvement": "Improvement"
        }

        return mapping.get(category, self.jira.config.default_issue_type)

    def _find_local_by_jira_key(self, jira_key: str) -> Optional[str]:
        """
        Busca ID local por chave Jira

        Args:
            jira_key: Chave da issue Jira

        Returns:
            ID local ou None
        """
        for local_id, key in self._sync_map.items():
            if key == jira_key:
                return local_id
        return None

    def _find_jira_by_local_id(self, local_id: str) -> Optional[str]:
        """
        Busca chave Jira por ID local

        Args:
            local_id: ID local da story

        Returns:
            Chave Jira ou None
        """
        return self._sync_map.get(local_id)

    async def _get_stories_for_export(self, project_id: str, project_key: str) -> List[Dict]:
        """
        Busca stories locais que precisam ser exportadas para o Jira - Issue #335

        Retorna stories que:
        1. Não têm referência Jira (external_references sem jira)
        2. Foram modificadas após última sincronização

        Args:
            project_id: ID do projeto local
            project_key: Chave do projeto Jira

        Returns:
            Lista de stories para exportar (como dicts)
        """
        stories_to_export = []

        if not self.story_repo:
            return stories_to_export

        try:
            # Busca todas as stories do projeto
            all_stories = await self.story_repo.get_by_project(project_id)

            for story in all_stories:
                # Verifica se já tem referência Jira
                ext_refs = getattr(story, "external_references", None) or {}
                jira_ref = ext_refs.get("jira", {})

                # Se não tem chave Jira, precisa exportar
                if not jira_ref.get("key"):
                    stories_to_export.append(self._story_to_dict(story))
                    continue

                # Se tem chave, verifica se foi modificada após última sync
                synced_at = jira_ref.get("synced_at")
                updated_at = getattr(story, "updated_at", None)

                if synced_at and updated_at:
                    from dateutil.parser import parse as parse_date
                    sync_time = parse_date(synced_at) if isinstance(synced_at, str) else synced_at
                    if updated_at > sync_time.replace(tzinfo=None):
                        stories_to_export.append(self._story_to_dict(story))

        except Exception as e:
            logger.error(f"Erro ao buscar stories para exportacao: {e}")

        return stories_to_export

    def _story_to_dict(self, story) -> Dict:
        """
        Converte objeto Story (SQLAlchemy) para dicionário - Issue #335

        Args:
            story: Objeto Story do ORM

        Returns:
            Dict com dados da story
        """
        return {
            "story_id": getattr(story, "story_id", None),
            "tenant_id": getattr(story, "tenant_id", None),
            "project_id": getattr(story, "project_id", None),
            "title": getattr(story, "title", ""),
            "description": getattr(story, "description", ""),
            "persona": getattr(story, "persona", ""),
            "action": getattr(story, "action", ""),
            "benefit": getattr(story, "benefit", ""),
            "status": getattr(story, "status", "backlog"),
            "priority": getattr(story, "priority", "medium"),
            "story_points": getattr(story, "story_points", 0),
            "assignee": getattr(story, "assignee", None),
            "epic_id": getattr(story, "epic_id", None),
            "sprint_id": getattr(story, "sprint_id", None),
            "tags": getattr(story, "tags", []),
            "acceptance_criteria": getattr(story, "acceptance_criteria", []),
            "definition_of_done": getattr(story, "definition_of_done", []),
            "external_references": getattr(story, "external_references", {}),
            "created_at": str(getattr(story, "created_at", "")) if getattr(story, "created_at", None) else None,
            "updated_at": str(getattr(story, "updated_at", "")) if getattr(story, "updated_at", None) else None,
        }

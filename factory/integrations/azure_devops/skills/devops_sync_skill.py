# -*- coding: utf-8 -*-
"""
Azure DevOps Sync Skill
=======================
Skill para sincronizacao bidirecional entre stories locais e work items do Azure DevOps.

Funcionalidades:
- Sincronizar uma story com work item
- Sincronizar todas as stories de um projeto
- Status de sincronizacao

Uso pelos agentes:
    from factory.integrations.azure_devops.skills import DevOpsSyncSkill

    skill = DevOpsSyncSkill(azure_client)

    # Sincronizar story especifica
    result = await skill.sync_story(story_id="STR-0001")

    # Sincronizar todas as stories
    result = await skill.sync_all_stories(project_id="PROJ-001")

    # Status de sincronizacao
    result = await skill.get_sync_status()
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)


class SyncDirection(str, Enum):
    """Direcao da sincronizacao"""
    TO_AZURE = "to_azure"        # Story local -> Azure DevOps
    FROM_AZURE = "from_azure"    # Azure DevOps -> Story local
    BIDIRECTIONAL = "bidirectional"  # Ambas direcoes


class SyncStatus(str, Enum):
    """Status da sincronizacao"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


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


@dataclass
class SyncResult:
    """Resultado detalhado de sincronizacao"""
    story_id: str
    status: SyncStatus
    direction: SyncDirection
    work_item_id: Optional[int] = None
    message: str = ""
    error: Optional[str] = None
    synced_at: Optional[datetime] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "story_id": self.story_id,
            "status": self.status.value,
            "direction": self.direction.value,
            "work_item_id": self.work_item_id,
            "message": self.message,
            "error": self.error,
            "synced_at": self.synced_at.isoformat() if self.synced_at else None
        }


class DevOpsSyncSkill:
    """
    Skill para sincronizacao com Azure DevOps.

    Fornece funcionalidades para manter stories locais
    sincronizadas com work items do Azure DevOps.
    """

    skill_name = "devops_sync"
    skill_description = "Sincronizacao bidirecional entre stories e work items do Azure DevOps"

    # Acoes disponiveis para Claude
    available_actions = [
        "sync_story",
        "sync_all_stories",
        "get_sync_status"
    ]

    # Schema de ferramenta compativel com Claude
    tool_schema = {
        "name": "devops_sync",
        "description": "Sincronizacao bidirecional entre stories locais e work items do Azure DevOps",
        "input_schema": {
            "type": "object",
            "properties": {
                "action": {
                    "type": "string",
                    "description": "Acao a executar",
                    "enum": ["sync_story", "sync_all_stories", "get_sync_status"]
                },
                "params": {
                    "type": "object",
                    "description": "Parametros da acao",
                    "properties": {
                        "story_id": {
                            "type": "string",
                            "description": "ID da story local (para sync_story)"
                        },
                        "story": {
                            "type": "object",
                            "description": "Dados da story para sincronizar"
                        },
                        "project_id": {
                            "type": "string",
                            "description": "ID do projeto (para sync_all_stories)"
                        },
                        "stories": {
                            "type": "array",
                            "description": "Lista de stories para sincronizar"
                        },
                        "direction": {
                            "type": "string",
                            "description": "Direcao da sincronizacao",
                            "enum": ["to_azure", "from_azure", "bidirectional"]
                        },
                        "create_if_not_exists": {
                            "type": "boolean",
                            "description": "Criar work item se nao existir (padrao: true)"
                        }
                    }
                }
            },
            "required": ["action"]
        }
    }

    def __init__(self, azure_client, story_repository=None):
        """
        Inicializa a skill.

        Args:
            azure_client: AzureDevOpsIntegration autenticado
            story_repository: Repositorio de stories (opcional)
        """
        self.azure = azure_client
        self.story_repo = story_repository
        self._sync_history: List[SyncResult] = []

    # ==================== SYNC OPERATIONS ====================

    async def sync_story(
        self,
        story_id: Optional[str] = None,
        story: Optional[Dict] = None,
        direction: SyncDirection = SyncDirection.TO_AZURE,
        create_if_not_exists: bool = True
    ) -> SkillResult:
        """
        Sincroniza uma story com Azure DevOps.

        Args:
            story_id: ID da story (busca do repositorio)
            story: Dados da story (se ja tiver os dados)
            direction: Direcao da sincronizacao
            create_if_not_exists: Criar work item se nao existir

        Returns:
            SkillResult com resultado da sincronizacao
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            # Obtem dados da story
            if story is None:
                if story_id is None:
                    return SkillResult(
                        success=False,
                        message="story_id ou story obrigatorio",
                        errors=["Nenhuma story informada"]
                    )

                if self.story_repo:
                    story = await self._get_story_from_repo(story_id)
                    if story is None:
                        return SkillResult(
                            success=False,
                            message=f"Story {story_id} nao encontrada",
                            errors=["Story nao existe no repositorio"]
                        )
                else:
                    return SkillResult(
                        success=False,
                        message="Repositorio de stories nao configurado",
                        errors=["story_repo nao disponivel, forneça story diretamente"]
                    )
            else:
                story_id = story.get("story_id", story_id)

            sync_result = SyncResult(
                story_id=story_id or "unknown",
                status=SyncStatus.IN_PROGRESS,
                direction=direction
            )

            # Executa sincronizacao baseado na direcao
            if direction == SyncDirection.TO_AZURE:
                result = await self._sync_to_azure(story, create_if_not_exists)
            elif direction == SyncDirection.FROM_AZURE:
                result = await self._sync_from_azure(story)
            else:
                # Bidirecional: sincroniza para Azure primeiro, depois atualiza local
                result = await self._sync_bidirectional(story, create_if_not_exists)

            if result["success"]:
                sync_result.status = SyncStatus.COMPLETED
                sync_result.work_item_id = result.get("work_item_id")
                sync_result.message = result.get("message", "Sincronizado com sucesso")
                sync_result.synced_at = datetime.utcnow()
            else:
                sync_result.status = SyncStatus.FAILED
                sync_result.error = result.get("error")
                sync_result.message = result.get("message", "Falha na sincronizacao")

            # Registra no historico
            self._sync_history.append(sync_result)

            return SkillResult(
                success=result["success"],
                data=sync_result.to_dict(),
                message=sync_result.message,
                errors=[sync_result.error] if sync_result.error else []
            )

        except Exception as e:
            logger.error(f"Erro ao sincronizar story {story_id}: {e}")
            return SkillResult(
                success=False,
                message=f"Erro na sincronizacao: {str(e)}",
                errors=[str(e)]
            )

    async def sync_all_stories(
        self,
        project_id: Optional[str] = None,
        stories: Optional[List[Dict]] = None,
        direction: SyncDirection = SyncDirection.TO_AZURE,
        create_if_not_exists: bool = True
    ) -> SkillResult:
        """
        Sincroniza todas as stories de um projeto.

        Args:
            project_id: ID do projeto (busca stories do repositorio)
            stories: Lista de stories (se ja tiver os dados)
            direction: Direcao da sincronizacao
            create_if_not_exists: Criar work items se nao existirem

        Returns:
            SkillResult com resultado da sincronizacao
        """
        try:
            if not self.azure.is_connected:
                return SkillResult(
                    success=False,
                    message="Nao conectado ao Azure DevOps",
                    errors=["Cliente nao conectado"]
                )

            # Obtem lista de stories
            if stories is None:
                if project_id is None:
                    return SkillResult(
                        success=False,
                        message="project_id ou stories obrigatorio",
                        errors=["Nenhuma lista de stories informada"]
                    )

                if self.story_repo:
                    stories = await self._get_stories_from_repo(project_id)
                else:
                    return SkillResult(
                        success=False,
                        message="Repositorio de stories nao configurado",
                        errors=["story_repo nao disponivel, forneça stories diretamente"]
                    )

            if not stories:
                return SkillResult(
                    success=True,
                    data={"synced": 0, "failed": 0, "skipped": 0},
                    message="Nenhuma story para sincronizar"
                )

            # Sincroniza cada story
            results = {
                "synced": 0,
                "failed": 0,
                "skipped": 0,
                "details": []
            }

            for story in stories:
                story_id = story.get("story_id", "unknown")

                try:
                    result = await self.sync_story(
                        story=story,
                        direction=direction,
                        create_if_not_exists=create_if_not_exists
                    )

                    if result.success:
                        results["synced"] += 1
                    else:
                        results["failed"] += 1

                    results["details"].append({
                        "story_id": story_id,
                        "success": result.success,
                        "message": result.message
                    })

                except Exception as e:
                    logger.error(f"Erro ao sincronizar story {story_id}: {e}")
                    results["failed"] += 1
                    results["details"].append({
                        "story_id": story_id,
                        "success": False,
                        "message": str(e)
                    })

            total = len(stories)
            success = results["failed"] == 0

            return SkillResult(
                success=success,
                data=results,
                message=f"Sincronizacao concluida: {results['synced']}/{total} sucesso, "
                        f"{results['failed']} falhas",
                errors=[d["message"] for d in results["details"] if not d["success"]]
            )

        except Exception as e:
            logger.error(f"Erro ao sincronizar stories: {e}")
            return SkillResult(
                success=False,
                message=f"Erro na sincronizacao em lote: {str(e)}",
                errors=[str(e)]
            )

    async def get_sync_status(self) -> SkillResult:
        """
        Obtem status da sincronizacao.

        Returns:
            SkillResult com estatisticas de sincronizacao
        """
        try:
            if not self.azure.is_connected:
                connection_status = "disconnected"
            else:
                connection_status = "connected"

            # Estatisticas do historico
            total = len(self._sync_history)
            completed = sum(1 for r in self._sync_history if r.status == SyncStatus.COMPLETED)
            failed = sum(1 for r in self._sync_history if r.status == SyncStatus.FAILED)
            pending = sum(1 for r in self._sync_history if r.status == SyncStatus.PENDING)

            # Ultima sincronizacao
            last_sync = None
            if self._sync_history:
                last = sorted(
                    [r for r in self._sync_history if r.synced_at],
                    key=lambda x: x.synced_at,
                    reverse=True
                )
                if last:
                    last_sync = {
                        "story_id": last[0].story_id,
                        "work_item_id": last[0].work_item_id,
                        "status": last[0].status.value,
                        "synced_at": last[0].synced_at.isoformat()
                    }

            # Configuracao da integracao
            config_info = {
                "organization": self.azure.config.organization,
                "project": self.azure.config.project,
                "team": self.azure.config.team,
                "auto_sync": self.azure.config.auto_sync,
                "sync_interval_minutes": self.azure.config.sync_interval_minutes
            }

            status_data = {
                "connection_status": connection_status,
                "config": config_info,
                "statistics": {
                    "total_synced": total,
                    "completed": completed,
                    "failed": failed,
                    "pending": pending,
                    "success_rate": f"{(completed/total)*100:.1f}%" if total > 0 else "N/A"
                },
                "last_sync": last_sync,
                "recent_history": [
                    r.to_dict() for r in self._sync_history[-10:]  # Ultimos 10
                ]
            }

            return SkillResult(
                success=True,
                data=status_data,
                message=f"Status: {connection_status}, {completed}/{total} sincronizacoes bem-sucedidas"
            )

        except Exception as e:
            logger.error(f"Erro ao obter status de sincronizacao: {e}")
            return SkillResult(
                success=False,
                message=f"Erro ao obter status: {str(e)}",
                errors=[str(e)]
            )

    # ==================== INTERNAL SYNC METHODS ====================

    async def _sync_to_azure(
        self,
        story: Dict,
        create_if_not_exists: bool
    ) -> Dict[str, Any]:
        """Sincroniza story local para Azure DevOps"""
        try:
            external_id = story.get("external_id")
            external_system = story.get("external_system")

            # Verifica se ja existe work item vinculado
            if external_id and external_system == "azure_devops":
                # Atualiza work item existente
                fields = self.azure._story_to_work_item_fields(story)
                success = await self.azure.update_work_item(int(external_id), fields)

                if success:
                    return {
                        "success": True,
                        "work_item_id": int(external_id),
                        "message": f"Work item {external_id} atualizado"
                    }
                else:
                    return {
                        "success": False,
                        "error": f"Falha ao atualizar work item {external_id}"
                    }
            elif create_if_not_exists:
                # Cria novo work item
                category = story.get("category", "feature")
                work_item_type = self.azure._category_to_work_item_type(category)
                fields = self.azure._story_to_work_item_fields(story)

                created = await self.azure.create_work_item(work_item_type, fields)

                if created:
                    work_item_id = created.get("id")
                    return {
                        "success": True,
                        "work_item_id": work_item_id,
                        "message": f"Work item {work_item_id} criado"
                    }
                else:
                    return {
                        "success": False,
                        "error": "Falha ao criar work item"
                    }
            else:
                return {
                    "success": False,
                    "error": "Story sem work item vinculado e create_if_not_exists=False"
                }

        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }

    async def _sync_from_azure(self, story: Dict) -> Dict[str, Any]:
        """Sincroniza work item do Azure DevOps para story local"""
        try:
            external_id = story.get("external_id")

            if not external_id:
                return {
                    "success": False,
                    "error": "Story sem external_id - nao ha work item vinculado"
                }

            # Busca work item atualizado
            work_item = await self.azure.get_work_item(int(external_id))

            if not work_item:
                return {
                    "success": False,
                    "error": f"Work item {external_id} nao encontrado no Azure DevOps"
                }

            # Converte para formato de story
            updated_story = self.azure._work_item_to_story(work_item)

            # Se tiver repositorio, atualiza story local
            if self.story_repo:
                await self._update_story_in_repo(story.get("story_id"), updated_story)

            return {
                "success": True,
                "work_item_id": int(external_id),
                "message": f"Story atualizada com dados do work item {external_id}",
                "updated_data": updated_story
            }

        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }

    async def _sync_bidirectional(
        self,
        story: Dict,
        create_if_not_exists: bool
    ) -> Dict[str, Any]:
        """Sincronizacao bidirecional - compara e sincroniza baseado em timestamps"""
        try:
            external_id = story.get("external_id")

            if not external_id:
                # Sem work item, apenas envia para Azure
                return await self._sync_to_azure(story, create_if_not_exists)

            # Busca work item atual
            work_item = await self.azure.get_work_item(int(external_id))

            if not work_item:
                # Work item foi deletado, recria se permitido
                if create_if_not_exists:
                    story_copy = dict(story)
                    story_copy.pop("external_id", None)
                    return await self._sync_to_azure(story_copy, True)
                else:
                    return {
                        "success": False,
                        "error": f"Work item {external_id} nao existe mais"
                    }

            # Compara timestamps para decidir direcao
            local_updated = story.get("updated_at")
            azure_updated = work_item.get("fields", {}).get("System.ChangedDate")

            # Se Azure mais recente, sincroniza de la
            if azure_updated and local_updated:
                try:
                    azure_dt = datetime.fromisoformat(azure_updated.replace("Z", "+00:00"))
                    local_dt = datetime.fromisoformat(local_updated.replace("Z", "+00:00"))

                    if azure_dt > local_dt:
                        return await self._sync_from_azure(story)
                except Exception:
                    pass

            # Caso contrario, sincroniza para Azure
            return await self._sync_to_azure(story, create_if_not_exists)

        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }

    # ==================== REPOSITORY HELPERS ====================

    async def _get_story_from_repo(self, story_id: str) -> Optional[Dict]:
        """Busca story do repositorio"""
        if not self.story_repo:
            return None

        try:
            # Assume interface padrao do repositorio
            if hasattr(self.story_repo, "get_by_id"):
                story = await self.story_repo.get_by_id(story_id)
                return story.to_dict() if hasattr(story, "to_dict") else story
            elif hasattr(self.story_repo, "get"):
                story = await self.story_repo.get(story_id)
                return story.to_dict() if hasattr(story, "to_dict") else story
        except Exception as e:
            logger.error(f"Erro ao buscar story {story_id}: {e}")

        return None

    async def _get_stories_from_repo(self, project_id: str) -> List[Dict]:
        """Busca stories de um projeto do repositorio"""
        if not self.story_repo:
            return []

        try:
            if hasattr(self.story_repo, "get_by_project"):
                stories = await self.story_repo.get_by_project(project_id)
                return [s.to_dict() if hasattr(s, "to_dict") else s for s in stories]
            elif hasattr(self.story_repo, "list"):
                stories = await self.story_repo.list(project_id=project_id)
                return [s.to_dict() if hasattr(s, "to_dict") else s for s in stories]
        except Exception as e:
            logger.error(f"Erro ao buscar stories do projeto {project_id}: {e}")

        return []

    async def _update_story_in_repo(self, story_id: str, data: Dict) -> bool:
        """Atualiza story no repositorio"""
        if not self.story_repo or not story_id:
            return False

        try:
            if hasattr(self.story_repo, "update"):
                await self.story_repo.update(story_id, data)
                return True
        except Exception as e:
            logger.error(f"Erro ao atualizar story {story_id}: {e}")

        return False

    # ==================== SKILL INTERFACE ====================

    async def execute(
        self,
        action: str,
        params: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Executa uma acao do skill.

        Args:
            action: Acao a executar
            params: Parametros da acao

        Returns:
            Resultado da acao como dicionario
        """
        params = params or {}

        actions = {
            "sync_story": self._action_sync_story,
            "sync_all_stories": self._action_sync_all_stories,
            "get_sync_status": self._action_get_sync_status
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

    async def _action_sync_story(self, params: Dict) -> SkillResult:
        """Acao para sincronizar uma story"""
        direction_str = params.get("direction", "to_azure")
        try:
            direction = SyncDirection(direction_str)
        except ValueError:
            direction = SyncDirection.TO_AZURE

        return await self.sync_story(
            story_id=params.get("story_id"),
            story=params.get("story"),
            direction=direction,
            create_if_not_exists=params.get("create_if_not_exists", True)
        )

    async def _action_sync_all_stories(self, params: Dict) -> SkillResult:
        """Acao para sincronizar todas as stories"""
        direction_str = params.get("direction", "to_azure")
        try:
            direction = SyncDirection(direction_str)
        except ValueError:
            direction = SyncDirection.TO_AZURE

        return await self.sync_all_stories(
            project_id=params.get("project_id"),
            stories=params.get("stories"),
            direction=direction,
            create_if_not_exists=params.get("create_if_not_exists", True)
        )

    async def _action_get_sync_status(self, params: Dict) -> SkillResult:
        """Acao para obter status de sincronizacao"""
        return await self.get_sync_status()

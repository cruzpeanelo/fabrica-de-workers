# -*- coding: utf-8 -*-
"""
Sync Service Module
===================
Servico unificado de sincronizacao entre a Plataforma E
e sistemas externos (Jira, Azure DevOps, GitHub).

Funcionalidades:
- Sincronizacao bidirecional automatica
- Resolucao de conflitos
- Log de sincronizacoes
- Agendamento de sync periodico

Uso:
```python
from factory.integrations.sync_service import SyncService

service = SyncService()
await service.initialize()

# Sincronizar do Jira
result = await service.sync_from_external("jira", "MEU-PROJETO")

# Sincronizar para Azure DevOps
result = await service.sync_to_external("azure_devops", stories)

# Sincronizar todas as integracoes
results = await service.sync_all("MEU-PROJETO")
```
"""

import logging
import asyncio
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Callable, Awaitable
from enum import Enum

from .base import IntegrationBase, IntegrationStatus, SyncResult
from .jira import JiraIntegration, get_jira_integration, init_jira_integration
from .azure_devops import AzureDevOpsIntegration, get_azure_devops_integration, init_azure_devops_integration
from .github import GitHubIntegration, get_github_integration, init_github_integration

logger = logging.getLogger(__name__)


class ConflictResolution(str, Enum):
    """Estrategia de resolucao de conflitos"""
    LOCAL_WINS = "local_wins"       # Dados locais prevalecem
    EXTERNAL_WINS = "external_wins" # Dados externos prevalecem
    NEWEST_WINS = "newest_wins"     # Dados mais recentes prevalecem
    MANUAL = "manual"               # Requer intervencao manual


class SyncDirection(str, Enum):
    """Direcao da sincronizacao"""
    FROM_EXTERNAL = "from_external"
    TO_EXTERNAL = "to_external"
    BIDIRECTIONAL = "bidirectional"


@dataclass
class SyncLogEntry:
    """Entrada do log de sincronizacao"""
    timestamp: datetime
    system: str
    direction: str
    project_id: str
    result: SyncResult
    duration_ms: int

    def to_dict(self) -> Dict[str, Any]:
        return {
            "timestamp": self.timestamp.isoformat(),
            "system": self.system,
            "direction": self.direction,
            "project_id": self.project_id,
            "result": self.result.to_dict(),
            "duration_ms": self.duration_ms
        }


@dataclass
class SyncConflict:
    """Representa um conflito de sincronizacao"""
    story_id: str
    external_id: str
    system: str
    field: str
    local_value: Any
    external_value: Any
    local_updated_at: Optional[datetime]
    external_updated_at: Optional[datetime]
    resolved: bool = False
    resolution: Optional[str] = None
    resolved_value: Any = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "story_id": self.story_id,
            "external_id": self.external_id,
            "system": self.system,
            "field": self.field,
            "local_value": str(self.local_value),
            "external_value": str(self.external_value),
            "local_updated_at": self.local_updated_at.isoformat() if self.local_updated_at else None,
            "external_updated_at": self.external_updated_at.isoformat() if self.external_updated_at else None,
            "resolved": self.resolved,
            "resolution": self.resolution,
            "resolved_value": str(self.resolved_value) if self.resolved_value else None
        }


class SyncService:
    """
    Servico unificado de sincronizacao.

    Gerencia sincronizacoes entre a Plataforma E e
    sistemas externos de forma centralizada.
    """

    def __init__(self):
        self._integrations: Dict[str, IntegrationBase] = {}
        self._sync_log: List[SyncLogEntry] = []
        self._conflicts: List[SyncConflict] = []
        self._auto_sync_tasks: Dict[str, asyncio.Task] = {}
        self._conflict_resolution = ConflictResolution.NEWEST_WINS
        self._on_sync_complete: Optional[Callable[[str, SyncResult], Awaitable[None]]] = None
        self._on_conflict: Optional[Callable[[SyncConflict], Awaitable[None]]] = None
        self._initialized = False

    async def initialize(self) -> None:
        """
        Inicializa o servico de sincronizacao.

        Conecta a todas as integracoes configuradas e habilitadas.
        """
        logger.info("Inicializando servico de sincronizacao...")

        # Inicializa integracoes
        jira = await init_jira_integration()
        if jira:
            self._integrations["jira"] = jira
            logger.info("Integracao Jira inicializada")

        azure = await init_azure_devops_integration()
        if azure:
            self._integrations["azure_devops"] = azure
            logger.info("Integracao Azure DevOps inicializada")

        github = await init_github_integration()
        if github:
            self._integrations["github"] = github
            logger.info("Integracao GitHub inicializada")

        self._initialized = True
        logger.info(f"Servico de sincronizacao inicializado com {len(self._integrations)} integracoes")

    async def shutdown(self) -> None:
        """
        Encerra o servico de sincronizacao.

        Desconecta de todas as integracoes e cancela tasks de auto-sync.
        """
        logger.info("Encerrando servico de sincronizacao...")

        # Cancela tasks de auto-sync
        for task_name, task in self._auto_sync_tasks.items():
            task.cancel()
            try:
                await task
            except asyncio.CancelledError:
                pass

        self._auto_sync_tasks.clear()

        # Desconecta integracoes
        for name, integration in self._integrations.items():
            await integration.disconnect()
            logger.info(f"Integracao {name} desconectada")

        self._integrations.clear()
        self._initialized = False
        logger.info("Servico de sincronizacao encerrado")

    def get_integration(self, system: str) -> Optional[IntegrationBase]:
        """
        Retorna uma integracao especifica.

        Args:
            system: Nome do sistema (jira, azure_devops, github)

        Returns:
            IntegrationBase ou None se nao encontrada
        """
        return self._integrations.get(system)

    def list_integrations(self) -> Dict[str, Dict[str, Any]]:
        """
        Lista todas as integracoes e seus status.

        Returns:
            Dict com status de cada integracao
        """
        return {
            name: integration.get_status()
            for name, integration in self._integrations.items()
        }

    def set_conflict_resolution(self, strategy: ConflictResolution) -> None:
        """
        Define estrategia de resolucao de conflitos.

        Args:
            strategy: Estrategia a ser usada
        """
        self._conflict_resolution = strategy
        logger.info(f"Estrategia de resolucao de conflitos: {strategy.value}")

    def on_sync_complete(self, callback: Callable[[str, SyncResult], Awaitable[None]]) -> None:
        """
        Registra callback para quando uma sincronizacao completa.

        Args:
            callback: Funcao async(system, result)
        """
        self._on_sync_complete = callback

    def on_conflict(self, callback: Callable[[SyncConflict], Awaitable[None]]) -> None:
        """
        Registra callback para quando um conflito e detectado.

        Args:
            callback: Funcao async(conflict)
        """
        self._on_conflict = callback

    async def sync_from_external(
        self,
        system: str,
        project_id: str
    ) -> SyncResult:
        """
        Sincroniza dados de um sistema externo para local.

        Args:
            system: Nome do sistema (jira, azure_devops, github)
            project_id: ID do projeto local

        Returns:
            SyncResult com os dados importados
        """
        start_time = datetime.utcnow()

        integration = self._integrations.get(system)
        if not integration:
            return SyncResult(
                success=False,
                errors=[f"Integracao '{system}' nao encontrada ou nao conectada"]
            )

        if not integration.is_connected:
            return SyncResult(
                success=False,
                errors=[f"Integracao '{system}' nao esta conectada"]
            )

        logger.info(f"Sincronizando de {system} para projeto {project_id}")

        try:
            result = await integration.sync_from_external(project_id)

            # Registra no log
            duration_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
            log_entry = SyncLogEntry(
                timestamp=start_time,
                system=system,
                direction=SyncDirection.FROM_EXTERNAL.value,
                project_id=project_id,
                result=result,
                duration_ms=duration_ms
            )
            self._sync_log.append(log_entry)

            # Chama callback se registrado
            if self._on_sync_complete:
                await self._on_sync_complete(system, result)

            logger.info(
                f"Sincronizacao de {system} completa: "
                f"{result.items_synced} itens em {duration_ms}ms"
            )

            return result

        except Exception as e:
            logger.error(f"Erro na sincronizacao de {system}: {e}")
            return SyncResult(
                success=False,
                errors=[str(e)]
            )

    async def sync_to_external(
        self,
        system: str,
        stories: List[Dict]
    ) -> SyncResult:
        """
        Sincroniza stories locais para um sistema externo.

        Args:
            system: Nome do sistema (jira, azure_devops, github)
            stories: Lista de stories para sincronizar

        Returns:
            SyncResult com os resultados
        """
        start_time = datetime.utcnow()

        integration = self._integrations.get(system)
        if not integration:
            return SyncResult(
                success=False,
                errors=[f"Integracao '{system}' nao encontrada ou nao conectada"]
            )

        if not integration.is_connected:
            return SyncResult(
                success=False,
                errors=[f"Integracao '{system}' nao esta conectada"]
            )

        logger.info(f"Sincronizando {len(stories)} stories para {system}")

        try:
            result = await integration.sync_to_external(stories)

            # Registra no log
            duration_ms = int((datetime.utcnow() - start_time).total_seconds() * 1000)
            log_entry = SyncLogEntry(
                timestamp=start_time,
                system=system,
                direction=SyncDirection.TO_EXTERNAL.value,
                project_id=stories[0].get("project_id", "unknown") if stories else "unknown",
                result=result,
                duration_ms=duration_ms
            )
            self._sync_log.append(log_entry)

            # Chama callback se registrado
            if self._on_sync_complete:
                await self._on_sync_complete(system, result)

            logger.info(
                f"Sincronizacao para {system} completa: "
                f"{result.items_created} criados, {result.items_updated} atualizados"
            )

            return result

        except Exception as e:
            logger.error(f"Erro na sincronizacao para {system}: {e}")
            return SyncResult(
                success=False,
                errors=[str(e)]
            )

    async def sync_all(
        self,
        project_id: str,
        direction: SyncDirection = SyncDirection.FROM_EXTERNAL
    ) -> Dict[str, SyncResult]:
        """
        Sincroniza todas as integracoes ativas.

        Args:
            project_id: ID do projeto local
            direction: Direcao da sincronizacao

        Returns:
            Dict com resultados por sistema
        """
        results = {}

        for system, integration in self._integrations.items():
            if not integration.is_connected:
                results[system] = SyncResult(
                    success=False,
                    errors=["Nao conectado"]
                )
                continue

            if direction == SyncDirection.FROM_EXTERNAL:
                results[system] = await self.sync_from_external(system, project_id)
            elif direction == SyncDirection.TO_EXTERNAL:
                # Para to_external, precisaria das stories
                results[system] = SyncResult(
                    success=False,
                    errors=["Sync to_external requer lista de stories"]
                )

        return results

    async def start_auto_sync(
        self,
        system: str,
        project_id: str,
        interval_minutes: int = 30
    ) -> bool:
        """
        Inicia sincronizacao automatica para um sistema.

        Args:
            system: Nome do sistema
            project_id: ID do projeto
            interval_minutes: Intervalo em minutos

        Returns:
            bool: True se iniciado com sucesso
        """
        if system in self._auto_sync_tasks:
            logger.warning(f"Auto-sync ja ativo para {system}")
            return False

        integration = self._integrations.get(system)
        if not integration:
            logger.error(f"Integracao {system} nao encontrada")
            return False

        async def auto_sync_loop():
            while True:
                try:
                    await asyncio.sleep(interval_minutes * 60)
                    await self.sync_from_external(system, project_id)
                except asyncio.CancelledError:
                    break
                except Exception as e:
                    logger.error(f"Erro no auto-sync de {system}: {e}")

        task = asyncio.create_task(auto_sync_loop())
        self._auto_sync_tasks[system] = task

        logger.info(f"Auto-sync iniciado para {system} (intervalo: {interval_minutes}min)")
        return True

    async def stop_auto_sync(self, system: str) -> bool:
        """
        Para sincronizacao automatica de um sistema.

        Args:
            system: Nome do sistema

        Returns:
            bool: True se parado com sucesso
        """
        task = self._auto_sync_tasks.pop(system, None)
        if task:
            task.cancel()
            try:
                await task
            except asyncio.CancelledError:
                pass
            logger.info(f"Auto-sync parado para {system}")
            return True

        return False

    def detect_conflicts(
        self,
        local_story: Dict,
        external_story: Dict,
        system: str
    ) -> List[SyncConflict]:
        """
        Detecta conflitos entre versao local e externa de uma story.

        Args:
            local_story: Story local
            external_story: Story do sistema externo
            system: Nome do sistema

        Returns:
            List[SyncConflict]: Lista de conflitos detectados
        """
        conflicts = []

        # Campos a verificar
        fields_to_check = [
            "title", "description", "status", "priority",
            "story_points", "assignee", "tags"
        ]

        local_updated = None
        external_updated = None

        if local_story.get("updated_at"):
            try:
                local_updated = datetime.fromisoformat(
                    local_story["updated_at"].replace("Z", "+00:00")
                )
            except (ValueError, TypeError):
                pass

        if external_story.get("updated_at"):
            try:
                external_updated = datetime.fromisoformat(
                    external_story["updated_at"].replace("Z", "+00:00")
                )
            except (ValueError, TypeError):
                pass

        for field in fields_to_check:
            local_value = local_story.get(field)
            external_value = external_story.get(field)

            # Normaliza para comparacao
            if isinstance(local_value, list):
                local_value = sorted(local_value) if local_value else []
            if isinstance(external_value, list):
                external_value = sorted(external_value) if external_value else []

            if local_value != external_value:
                conflict = SyncConflict(
                    story_id=local_story.get("story_id", ""),
                    external_id=external_story.get("external_id", ""),
                    system=system,
                    field=field,
                    local_value=local_value,
                    external_value=external_value,
                    local_updated_at=local_updated,
                    external_updated_at=external_updated
                )
                conflicts.append(conflict)

        return conflicts

    def resolve_conflict(self, conflict: SyncConflict) -> Any:
        """
        Resolve um conflito baseado na estrategia configurada.

        Args:
            conflict: Conflito a resolver

        Returns:
            Valor resolvido
        """
        if self._conflict_resolution == ConflictResolution.LOCAL_WINS:
            conflict.resolved_value = conflict.local_value
            conflict.resolution = "local_wins"

        elif self._conflict_resolution == ConflictResolution.EXTERNAL_WINS:
            conflict.resolved_value = conflict.external_value
            conflict.resolution = "external_wins"

        elif self._conflict_resolution == ConflictResolution.NEWEST_WINS:
            if conflict.local_updated_at and conflict.external_updated_at:
                if conflict.local_updated_at >= conflict.external_updated_at:
                    conflict.resolved_value = conflict.local_value
                    conflict.resolution = "local_newer"
                else:
                    conflict.resolved_value = conflict.external_value
                    conflict.resolution = "external_newer"
            else:
                # Se nao tiver timestamp, usa local como padrao
                conflict.resolved_value = conflict.local_value
                conflict.resolution = "local_default"

        elif self._conflict_resolution == ConflictResolution.MANUAL:
            # Nao resolve automaticamente
            conflict.resolution = "pending_manual"
            return None

        conflict.resolved = True
        return conflict.resolved_value

    def get_sync_log(
        self,
        system: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retorna log de sincronizacoes.

        Args:
            system: Filtrar por sistema (opcional)
            limit: Numero maximo de entradas

        Returns:
            List[Dict]: Entradas do log
        """
        log_entries = self._sync_log

        if system:
            log_entries = [e for e in log_entries if e.system == system]

        # Ordena por timestamp decrescente e limita
        log_entries = sorted(
            log_entries,
            key=lambda x: x.timestamp,
            reverse=True
        )[:limit]

        return [e.to_dict() for e in log_entries]

    def get_pending_conflicts(self) -> List[Dict[str, Any]]:
        """
        Retorna conflitos pendentes de resolucao.

        Returns:
            List[Dict]: Conflitos nao resolvidos
        """
        return [
            c.to_dict() for c in self._conflicts
            if not c.resolved
        ]

    def get_status(self) -> Dict[str, Any]:
        """
        Retorna status geral do servico.

        Returns:
            Dict com status do servico e integracoes
        """
        return {
            "initialized": self._initialized,
            "integrations": self.list_integrations(),
            "auto_sync_active": list(self._auto_sync_tasks.keys()),
            "conflict_resolution": self._conflict_resolution.value,
            "pending_conflicts": len([c for c in self._conflicts if not c.resolved]),
            "total_syncs": len(self._sync_log),
            "last_sync": self._sync_log[-1].to_dict() if self._sync_log else None
        }


# Instancia global (singleton)
_sync_service: Optional[SyncService] = None


def get_sync_service() -> SyncService:
    """Retorna instancia global do servico de sincronizacao"""
    global _sync_service
    if _sync_service is None:
        _sync_service = SyncService()
    return _sync_service


async def init_sync_service() -> SyncService:
    """Inicializa e retorna o servico de sincronizacao"""
    service = get_sync_service()
    if not service._initialized:
        await service.initialize()
    return service

# -*- coding: utf-8 -*-
"""
Jira Sync Manager
=================
Sincronizacao bidirecional Jira <-> Sistema.

Issue #364 - Terminal A
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional

from .field_mapper import FieldMapper, DEFAULT_JIRA_MAPPINGS
from .conflict_resolver import ConflictResolver, ConflictStrategy, SyncConflict

logger = logging.getLogger(__name__)


class SyncDirection(str, Enum):
    """Direcao da sincronizacao"""
    JIRA_TO_SYSTEM = "jira_to_system"
    SYSTEM_TO_JIRA = "system_to_jira"
    BIDIRECTIONAL = "bidirectional"


class SyncStatus(str, Enum):
    """Status da sincronizacao"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    PARTIAL = "partial"


@dataclass
class SyncConfig:
    """
    Configuracao de sincronizacao.

    Attributes:
        tenant_id: ID do tenant
        project_key: Chave do projeto Jira
        jql_filter: Filtro JQL para issues
        direction: Direcao da sync
        sync_interval_minutes: Intervalo entre syncs
        conflict_strategy: Estrategia de resolucao de conflitos
        auto_sync: Se deve sincronizar automaticamente
        max_issues_per_sync: Maximo de issues por sync
    """
    tenant_id: str
    project_key: str
    jql_filter: str = ""
    direction: SyncDirection = SyncDirection.BIDIRECTIONAL
    sync_interval_minutes: int = 30
    conflict_strategy: ConflictStrategy = ConflictStrategy.NEWEST_WINS
    auto_sync: bool = True
    max_issues_per_sync: int = 100
    sync_comments: bool = True
    sync_attachments: bool = False
    create_missing: bool = True  # Criar stories/issues que nao existem
    field_mappings: List[Dict] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "project_key": self.project_key,
            "jql_filter": self.jql_filter,
            "direction": self.direction.value,
            "sync_interval_minutes": self.sync_interval_minutes,
            "conflict_strategy": self.conflict_strategy.value,
            "auto_sync": self.auto_sync,
            "max_issues_per_sync": self.max_issues_per_sync,
            "sync_comments": self.sync_comments,
            "sync_attachments": self.sync_attachments,
            "create_missing": self.create_missing
        }


@dataclass
class SyncResult:
    """
    Resultado de uma sincronizacao.

    Attributes:
        sync_id: ID da sincronizacao
        direction: Direcao da sync
        status: Status final
        started_at: Inicio da sync
        completed_at: Fim da sync
        issues_synced: Numero de issues sincronizadas
        issues_created: Issues criadas
        issues_updated: Issues atualizadas
        conflicts_found: Conflitos encontrados
        conflicts_resolved: Conflitos resolvidos
        errors: Lista de erros
    """
    sync_id: str = ""
    direction: SyncDirection = SyncDirection.BIDIRECTIONAL
    status: SyncStatus = SyncStatus.PENDING
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    issues_synced: int = 0
    issues_created: int = 0
    issues_updated: int = 0
    issues_failed: int = 0
    conflicts_found: int = 0
    conflicts_resolved: int = 0
    conflicts_pending: int = 0
    errors: List[str] = field(default_factory=list)

    @property
    def duration_seconds(self) -> Optional[float]:
        if self.started_at and self.completed_at:
            return (self.completed_at - self.started_at).total_seconds()
        return None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "sync_id": self.sync_id,
            "direction": self.direction.value,
            "status": self.status.value,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration_seconds": self.duration_seconds,
            "issues_synced": self.issues_synced,
            "issues_created": self.issues_created,
            "issues_updated": self.issues_updated,
            "issues_failed": self.issues_failed,
            "conflicts_found": self.conflicts_found,
            "conflicts_resolved": self.conflicts_resolved,
            "conflicts_pending": self.conflicts_pending,
            "errors": self.errors
        }


class JiraSyncManager:
    """
    Gerenciador de sincronizacao Jira <-> Sistema.

    Exemplo:
        config = SyncConfig(
            tenant_id="tenant-1",
            project_key="PROJ",
            direction=SyncDirection.BIDIRECTIONAL
        )
        sync_manager = JiraSyncManager(config)

        # Sincronizar Jira -> Sistema
        result = await sync_manager.sync_from_jira()

        # Sincronizar Sistema -> Jira
        result = await sync_manager.sync_to_jira(story_ids=["STR-001"])

        # Sincronizar bidirecional
        result = await sync_manager.sync_all()
    """

    def __init__(self, config: SyncConfig):
        self.config = config
        self.field_mapper = FieldMapper()
        self.conflict_resolver = ConflictResolver(
            default_strategy=config.conflict_strategy
        )

        self._jira_client = None
        self._last_sync: Optional[datetime] = None
        self._sync_history: List[SyncResult] = []
        self._sync_count = 0

        # Cache de mapeamento story_id <-> jira_key
        self._story_to_jira: Dict[str, str] = {}
        self._jira_to_story: Dict[str, str] = {}

    async def _get_jira_client(self):
        """Obtem cliente Jira"""
        if self._jira_client is None:
            try:
                from ..jira import JiraIntegration, JiraConfig
                jira_config = JiraConfig.from_env()
                self._jira_client = JiraIntegration(jira_config)
                await self._jira_client.connect()
            except ImportError:
                logger.error("Modulo Jira nao disponivel")
                raise
            except Exception as e:
                logger.error(f"Erro ao conectar ao Jira: {e}")
                raise
        return self._jira_client

    def _generate_sync_id(self) -> str:
        """Gera ID de sincronizacao"""
        self._sync_count += 1
        return f"SYNC-{datetime.utcnow().strftime('%Y%m%d%H%M%S')}-{self._sync_count:04d}"

    async def sync_from_jira(
        self,
        jql: Optional[str] = None,
        max_results: Optional[int] = None
    ) -> SyncResult:
        """
        Sincroniza issues do Jira para o sistema.

        Args:
            jql: Filtro JQL (opcional, usa config se nao fornecido)
            max_results: Maximo de resultados

        Returns:
            Resultado da sincronizacao
        """
        result = SyncResult(
            sync_id=self._generate_sync_id(),
            direction=SyncDirection.JIRA_TO_SYSTEM,
            status=SyncStatus.IN_PROGRESS,
            started_at=datetime.utcnow()
        )

        try:
            jira = await self._get_jira_client()

            # Constroi JQL
            filter_jql = jql or self.config.jql_filter
            if not filter_jql:
                filter_jql = f"project = {self.config.project_key}"

            if self._last_sync:
                # Sincroniza apenas alteracoes desde ultimo sync
                since = self._last_sync.strftime("%Y-%m-%d %H:%M")
                filter_jql = f"({filter_jql}) AND updated >= '{since}'"

            # Busca issues
            max_issues = max_results or self.config.max_issues_per_sync
            issues = await jira.search_issues(filter_jql, max_results=max_issues)

            logger.info(f"Encontradas {len(issues)} issues para sincronizar")

            for issue in issues:
                try:
                    await self._sync_issue_to_system(issue, result)
                except Exception as e:
                    result.issues_failed += 1
                    result.errors.append(f"Erro em {issue.get('key')}: {str(e)}")
                    logger.error(f"Erro sincronizando {issue.get('key')}: {e}")

            result.status = SyncStatus.COMPLETED if not result.errors else SyncStatus.PARTIAL
            result.completed_at = datetime.utcnow()
            self._last_sync = result.completed_at

        except Exception as e:
            result.status = SyncStatus.FAILED
            result.errors.append(str(e))
            result.completed_at = datetime.utcnow()
            logger.error(f"Sync from Jira failed: {e}")

        self._sync_history.append(result)
        return result

    async def _sync_issue_to_system(self, issue: Dict, result: SyncResult):
        """Sincroniza uma issue para o sistema"""
        jira_key = issue.get("key")

        # Mapeia campos
        story_data = self.field_mapper.map_to_system(issue)
        story_data["tenant_id"] = self.config.tenant_id
        story_data["jira_key"] = jira_key

        # Verifica se story ja existe
        story_id = self._jira_to_story.get(jira_key)

        if story_id:
            # Atualiza story existente
            existing = await self._get_story(story_id)
            if existing:
                # Detecta conflitos
                for field_name, new_value in story_data.items():
                    old_value = existing.get(field_name)
                    if old_value != new_value and old_value is not None:
                        conflict = self.conflict_resolver.detect_conflict(
                            story_id=story_id,
                            jira_key=jira_key,
                            field=field_name,
                            jira_value=new_value,
                            system_value=old_value,
                            jira_updated_at=self._parse_date(issue.get("fields", {}).get("updated")),
                            system_updated_at=self._parse_date(existing.get("updated_at"))
                        )
                        if conflict:
                            result.conflicts_found += 1
                            resolution = self.conflict_resolver.resolve(conflict)
                            if resolution.requires_manual:
                                result.conflicts_pending += 1
                            else:
                                story_data[field_name] = resolution.resolved_value
                                result.conflicts_resolved += 1

                await self._update_story(story_id, story_data)
                result.issues_updated += 1
            else:
                logger.warning(f"Story {story_id} nao encontrada para atualizacao")
        else:
            # Cria nova story
            if self.config.create_missing:
                story_id = await self._create_story(story_data)
                self._jira_to_story[jira_key] = story_id
                self._story_to_jira[story_id] = jira_key
                result.issues_created += 1
            else:
                logger.debug(f"Issue {jira_key} ignorada (create_missing=False)")

        result.issues_synced += 1

    async def sync_to_jira(
        self,
        story_ids: Optional[List[str]] = None
    ) -> SyncResult:
        """
        Sincroniza stories do sistema para o Jira.

        Args:
            story_ids: IDs das stories a sincronizar (todas se None)

        Returns:
            Resultado da sincronizacao
        """
        result = SyncResult(
            sync_id=self._generate_sync_id(),
            direction=SyncDirection.SYSTEM_TO_JIRA,
            status=SyncStatus.IN_PROGRESS,
            started_at=datetime.utcnow()
        )

        try:
            jira = await self._get_jira_client()

            # Busca stories
            if story_ids:
                stories = [await self._get_story(sid) for sid in story_ids]
                stories = [s for s in stories if s is not None]
            else:
                stories = await self._get_stories_to_sync()

            logger.info(f"Sincronizando {len(stories)} stories para Jira")

            for story in stories:
                try:
                    await self._sync_story_to_jira(story, result, jira)
                except Exception as e:
                    result.issues_failed += 1
                    result.errors.append(f"Erro em {story.get('story_id')}: {str(e)}")
                    logger.error(f"Erro sincronizando {story.get('story_id')}: {e}")

            result.status = SyncStatus.COMPLETED if not result.errors else SyncStatus.PARTIAL
            result.completed_at = datetime.utcnow()

        except Exception as e:
            result.status = SyncStatus.FAILED
            result.errors.append(str(e))
            result.completed_at = datetime.utcnow()
            logger.error(f"Sync to Jira failed: {e}")

        self._sync_history.append(result)
        return result

    async def _sync_story_to_jira(self, story: Dict, result: SyncResult, jira):
        """Sincroniza uma story para o Jira"""
        story_id = story.get("story_id")

        # Mapeia campos
        jira_data = self.field_mapper.map_to_jira(story)

        # Verifica se issue ja existe
        jira_key = self._story_to_jira.get(story_id)

        if jira_key:
            # Atualiza issue existente
            try:
                existing_issue = await jira.get_issue(jira_key)
                if existing_issue:
                    # Detecta conflitos
                    existing_data = self.field_mapper.map_to_system(existing_issue)

                    for field_name, new_value in story.items():
                        old_value = existing_data.get(field_name)
                        if old_value != new_value and old_value is not None:
                            conflict = self.conflict_resolver.detect_conflict(
                                story_id=story_id,
                                jira_key=jira_key,
                                field=field_name,
                                jira_value=old_value,
                                system_value=new_value,
                                jira_updated_at=self._parse_date(existing_issue.get("fields", {}).get("updated")),
                                system_updated_at=self._parse_date(story.get("updated_at"))
                            )
                            if conflict:
                                result.conflicts_found += 1
                                resolution = self.conflict_resolver.resolve(conflict)
                                if resolution.requires_manual:
                                    result.conflicts_pending += 1
                                else:
                                    jira_data["fields"][field_name] = resolution.resolved_value
                                    result.conflicts_resolved += 1

                    await jira.update_issue(jira_key, jira_data)
                    result.issues_updated += 1
            except Exception as e:
                logger.warning(f"Issue {jira_key} nao encontrada: {e}")
                jira_key = None

        if not jira_key and self.config.create_missing:
            # Cria nova issue
            jira_data["fields"]["project"] = {"key": self.config.project_key}
            jira_data["fields"]["issuetype"] = {"name": "Story"}

            created = await jira.create_issue(jira_data)
            jira_key = created.get("key")

            self._story_to_jira[story_id] = jira_key
            self._jira_to_story[jira_key] = story_id
            result.issues_created += 1

        result.issues_synced += 1

    async def sync_all(self) -> SyncResult:
        """
        Executa sincronizacao bidirecional completa.

        Returns:
            Resultado combinado
        """
        result = SyncResult(
            sync_id=self._generate_sync_id(),
            direction=SyncDirection.BIDIRECTIONAL,
            status=SyncStatus.IN_PROGRESS,
            started_at=datetime.utcnow()
        )

        try:
            # Primeiro sincroniza do Jira para o sistema
            jira_result = await self.sync_from_jira()
            result.issues_synced += jira_result.issues_synced
            result.issues_created += jira_result.issues_created
            result.issues_updated += jira_result.issues_updated
            result.conflicts_found += jira_result.conflicts_found
            result.conflicts_resolved += jira_result.conflicts_resolved
            result.errors.extend(jira_result.errors)

            # Depois sincroniza do sistema para o Jira
            system_result = await self.sync_to_jira()
            result.issues_synced += system_result.issues_synced
            result.issues_created += system_result.issues_created
            result.issues_updated += system_result.issues_updated
            result.conflicts_found += system_result.conflicts_found
            result.conflicts_resolved += system_result.conflicts_resolved
            result.errors.extend(system_result.errors)

            result.status = SyncStatus.COMPLETED if not result.errors else SyncStatus.PARTIAL
            result.completed_at = datetime.utcnow()

        except Exception as e:
            result.status = SyncStatus.FAILED
            result.errors.append(str(e))
            result.completed_at = datetime.utcnow()

        self._sync_history.append(result)
        return result

    def _parse_date(self, date_str: Optional[str]) -> Optional[datetime]:
        """Parse de data ISO"""
        if not date_str:
            return None
        try:
            return datetime.fromisoformat(date_str.replace("Z", "+00:00"))
        except:
            return None

    # Metodos de acesso ao banco (a serem implementados pela aplicacao)

    async def _get_story(self, story_id: str) -> Optional[Dict]:
        """Busca story do banco"""
        # Implementar busca no banco
        logger.debug(f"Buscando story {story_id}")
        return None

    async def _create_story(self, data: Dict) -> str:
        """Cria story no banco"""
        # Implementar criacao no banco
        story_id = f"STR-{datetime.utcnow().strftime('%Y%m%d%H%M%S')}"
        logger.info(f"Story criada: {story_id}")
        return story_id

    async def _update_story(self, story_id: str, data: Dict):
        """Atualiza story no banco"""
        # Implementar atualizacao no banco
        logger.info(f"Story atualizada: {story_id}")

    async def _get_stories_to_sync(self) -> List[Dict]:
        """Busca stories para sincronizar"""
        # Implementar busca no banco
        return []

    def get_sync_history(self, limit: int = 10) -> List[SyncResult]:
        """Retorna historico de sincronizacoes"""
        return self._sync_history[-limit:]

    def get_pending_conflicts(self) -> List[SyncConflict]:
        """Retorna conflitos pendentes de resolucao"""
        return self.conflict_resolver.get_pending_conflicts()

    def get_mapping(self, story_id: str = None, jira_key: str = None) -> Optional[str]:
        """Retorna mapeamento story <-> jira"""
        if story_id:
            return self._story_to_jira.get(story_id)
        if jira_key:
            return self._jira_to_story.get(jira_key)
        return None

    def set_mapping(self, story_id: str, jira_key: str):
        """Define mapeamento story <-> jira"""
        self._story_to_jira[story_id] = jira_key
        self._jira_to_story[jira_key] = story_id

    def get_status(self) -> Dict[str, Any]:
        """Retorna status do sync manager"""
        return {
            "tenant_id": self.config.tenant_id,
            "project_key": self.config.project_key,
            "direction": self.config.direction.value,
            "auto_sync": self.config.auto_sync,
            "sync_interval_minutes": self.config.sync_interval_minutes,
            "last_sync": self._last_sync.isoformat() if self._last_sync else None,
            "total_syncs": len(self._sync_history),
            "mappings_count": len(self._story_to_jira),
            "pending_conflicts": len(self.get_pending_conflicts())
        }

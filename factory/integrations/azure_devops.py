# -*- coding: utf-8 -*-
"""
Azure DevOps Integration Module
===============================
Integracao com Azure DevOps via API REST.

Funcionalidades:
- Conectar/desconectar via API REST
- Sincronizar work items <-> stories bidirecionalmente
- Mapear estados Azure DevOps -> Kanban interno
- Webhook para atualizacoes em tempo real

Configuracao via variaveis de ambiente:
- AZURE_DEVOPS_ORG: Nome da organizacao
- AZURE_DEVOPS_PROJECT: Nome do projeto
- AZURE_DEVOPS_PAT: Personal Access Token
- AZURE_DEVOPS_TEAM: Nome do time (opcional)
"""

import os
import logging
import aiohttp
import base64
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional
from enum import Enum

from .base import (
    IntegrationBase,
    IntegrationConfig,
    IntegrationStatus,
    SyncResult,
    map_status_to_internal,
    map_status_to_external,
    map_priority_to_internal,
    map_priority_to_external
)

logger = logging.getLogger(__name__)


class AzureWorkItemType(str, Enum):
    """Tipos de work item no Azure DevOps"""
    USER_STORY = "User Story"
    TASK = "Task"
    BUG = "Bug"
    EPIC = "Epic"
    FEATURE = "Feature"
    ISSUE = "Issue"
    PRODUCT_BACKLOG_ITEM = "Product Backlog Item"  # Scrum template


@dataclass
class AzureDevOpsConfig(IntegrationConfig):
    """Configuracao especifica para Azure DevOps"""
    organization: str = ""
    project: str = ""
    pat: str = ""  # Personal Access Token
    team: str = ""
    default_work_item_type: str = "User Story"
    area_path: str = ""
    iteration_path: str = ""
    sync_comments: bool = True
    sync_attachments: bool = False
    custom_field_mapping: Dict[str, str] = field(default_factory=dict)
    webhook_secret: str = ""

    @classmethod
    def from_env(cls) -> "AzureDevOpsConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("AZURE_DEVOPS_ENABLED", "false").lower() == "true",
            organization=os.getenv("AZURE_DEVOPS_ORG", ""),
            project=os.getenv("AZURE_DEVOPS_PROJECT", ""),
            pat=os.getenv("AZURE_DEVOPS_PAT", ""),
            team=os.getenv("AZURE_DEVOPS_TEAM", ""),
            default_work_item_type=os.getenv("AZURE_DEVOPS_WORK_ITEM_TYPE", "User Story"),
            area_path=os.getenv("AZURE_DEVOPS_AREA_PATH", ""),
            iteration_path=os.getenv("AZURE_DEVOPS_ITERATION_PATH", ""),
            sync_comments=os.getenv("AZURE_DEVOPS_SYNC_COMMENTS", "true").lower() == "true",
            sync_attachments=os.getenv("AZURE_DEVOPS_SYNC_ATTACHMENTS", "false").lower() == "true",
            webhook_secret=os.getenv("AZURE_DEVOPS_WEBHOOK_SECRET", ""),
            auto_sync=os.getenv("AZURE_DEVOPS_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("AZURE_DEVOPS_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.organization and self.project and self.pat)


class AzureDevOpsIntegration(IntegrationBase):
    """
    Integracao com Azure DevOps via API REST.

    Exemplo de uso:
    ```python
    config = AzureDevOpsConfig.from_env()
    azure = AzureDevOpsIntegration(config)

    if await azure.connect():
        result = await azure.sync_from_external("my-project")
        print(f"Sincronizados {result.items_synced} work items")
    ```
    """

    API_VERSION = "7.1"

    def __init__(self, config: AzureDevOpsConfig):
        super().__init__(config)
        self.config: AzureDevOpsConfig = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_info: Optional[Dict] = None

    @property
    def base_url(self) -> str:
        """URL base da API"""
        return f"https://dev.azure.com/{self.config.organization}"

    @property
    def project_url(self) -> str:
        """URL do projeto"""
        return f"{self.base_url}/{self.config.project}"

    @property
    def auth_header(self) -> str:
        """Header de autenticacao Basic Auth com PAT"""
        # Azure DevOps usa user vazio com PAT como senha
        credentials = f":{self.config.pat}"
        encoded = base64.b64encode(credentials.encode()).decode()
        return f"Basic {encoded}"

    def _get_headers(self) -> Dict[str, str]:
        """Retorna headers para requisicoes"""
        return {
            "Authorization": self.auth_header,
            "Content-Type": "application/json",
            "Accept": "application/json"
        }

    async def _ensure_session(self) -> aiohttp.ClientSession:
        """Garante que existe uma sessao HTTP ativa"""
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(headers=self._get_headers())
        return self._session

    async def connect(self) -> bool:
        """
        Conecta ao Azure DevOps e valida credenciais.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique organizacao, projeto e PAT."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao Azure DevOps: {self.config.organization}/{self.config.project}")

        try:
            session = await self._ensure_session()

            # Testa conexao buscando informacoes do projeto
            url = f"{self.project_url}/_apis/projects/{self.config.project}"
            params = {"api-version": self.API_VERSION}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    project_info = await response.json()
                    self._user_info = {"project": project_info}
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(f"Conectado ao Azure DevOps projeto: {project_info.get('name')}")
                    return True
                elif response.status == 401:
                    self._last_error = "PAT invalido ou expirado"
                elif response.status == 403:
                    self._last_error = "Acesso negado. Verifique permissoes do PAT."
                elif response.status == 404:
                    self._last_error = f"Projeto {self.config.project} nao encontrado"
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        logger.error(f"Falha ao conectar ao Azure DevOps: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """
        Desconecta do Azure DevOps.

        Returns:
            bool: True se desconectado com sucesso
        """
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._user_info = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Azure DevOps")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o Azure DevOps.

        Returns:
            bool: True se a conexao esta funcionando
        """
        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/_apis/projects/{self.config.project}"
            params = {"api-version": self.API_VERSION}

            async with session.get(url, params=params) as response:
                return response.status == 200
        except Exception:
            return False

    async def get_work_item(self, work_item_id: int) -> Optional[Dict]:
        """
        Busca um work item especifico.

        Args:
            work_item_id: ID do work item

        Returns:
            Dict ou None se nao encontrado
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/_apis/wit/workitems/{work_item_id}"
            params = {
                "api-version": self.API_VERSION,
                "$expand": "all"
            }

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.json()
                elif response.status == 404:
                    logger.warning(f"Work item nao encontrado: {work_item_id}")
        except Exception as e:
            logger.error(f"Erro ao buscar work item {work_item_id}: {e}")

        return None

    async def query_work_items(
        self,
        wiql: Optional[str] = None,
        work_item_type: Optional[str] = None,
        state: Optional[str] = None,
        max_results: int = 200
    ) -> List[Dict]:
        """
        Busca work items usando WIQL.

        Args:
            wiql: Query WIQL customizada
            work_item_type: Filtrar por tipo
            state: Filtrar por estado
            max_results: Maximo de resultados

        Returns:
            List[Dict]: Lista de work items
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()

            # Monta query WIQL
            if not wiql:
                conditions = [f"[System.TeamProject] = '{self.config.project}'"]

                if work_item_type:
                    conditions.append(f"[System.WorkItemType] = '{work_item_type}'")

                if state:
                    conditions.append(f"[System.State] = '{state}'")

                if self.config.area_path:
                    conditions.append(f"[System.AreaPath] UNDER '{self.config.area_path}'")

                where_clause = " AND ".join(conditions)
                wiql = f"SELECT [System.Id] FROM WorkItems WHERE {where_clause} ORDER BY [System.ChangedDate] DESC"

            # Executa query
            url = f"{self.project_url}/_apis/wit/wiql"
            params = {"api-version": self.API_VERSION, "$top": max_results}
            body = {"query": wiql}

            async with session.post(url, params=params, json=body) as response:
                if response.status != 200:
                    error = await response.text()
                    logger.error(f"Erro na query WIQL: {error}")
                    return []

                query_result = await response.json()
                work_item_refs = query_result.get("workItems", [])

                if not work_item_refs:
                    return []

                # Busca detalhes dos work items (em lotes de 200)
                work_item_ids = [str(wi["id"]) for wi in work_item_refs[:max_results]]
                return await self._get_work_items_batch(work_item_ids)

        except Exception as e:
            logger.error(f"Erro na query WIQL: {e}")

        return []

    async def _get_work_items_batch(self, ids: List[str]) -> List[Dict]:
        """
        Busca work items em lote.

        Args:
            ids: Lista de IDs

        Returns:
            List[Dict]: Work items
        """
        if not ids:
            return []

        try:
            session = await self._ensure_session()

            # Azure DevOps suporta ate 200 IDs por requisicao
            batch_size = 200
            all_items = []

            for i in range(0, len(ids), batch_size):
                batch_ids = ids[i:i + batch_size]
                url = f"{self.project_url}/_apis/wit/workitems"
                params = {
                    "api-version": self.API_VERSION,
                    "ids": ",".join(batch_ids),
                    "$expand": "all"
                }

                async with session.get(url, params=params) as response:
                    if response.status == 200:
                        data = await response.json()
                        all_items.extend(data.get("value", []))

            return all_items

        except Exception as e:
            logger.error(f"Erro ao buscar work items em lote: {e}")
            return []

    async def create_work_item(self, work_item_type: str, fields: Dict) -> Optional[Dict]:
        """
        Cria um novo work item.

        Args:
            work_item_type: Tipo do work item (User Story, Bug, etc)
            fields: Campos do work item

        Returns:
            Dict com o work item criado ou None
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/_apis/wit/workitems/${work_item_type}"
            params = {"api-version": self.API_VERSION}

            # Formata operacoes de patch
            operations = []
            for field, value in fields.items():
                operations.append({
                    "op": "add",
                    "path": f"/fields/{field}",
                    "value": value
                })

            headers = {
                **self._get_headers(),
                "Content-Type": "application/json-patch+json"
            }

            async with session.post(url, params=params, json=operations, headers=headers) as response:
                if response.status in (200, 201):
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar work item: {error}")
        except Exception as e:
            logger.error(f"Erro ao criar work item: {e}")

        return None

    async def update_work_item(self, work_item_id: int, fields: Dict) -> bool:
        """
        Atualiza um work item existente.

        Args:
            work_item_id: ID do work item
            fields: Campos para atualizar

        Returns:
            bool: True se atualizado com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/_apis/wit/workitems/{work_item_id}"
            params = {"api-version": self.API_VERSION}

            # Formata operacoes de patch
            operations = []
            for field, value in fields.items():
                operations.append({
                    "op": "replace",
                    "path": f"/fields/{field}",
                    "value": value
                })

            headers = {
                **self._get_headers(),
                "Content-Type": "application/json-patch+json"
            }

            async with session.patch(url, params=params, json=operations, headers=headers) as response:
                return response.status == 200
        except Exception as e:
            logger.error(f"Erro ao atualizar work item {work_item_id}: {e}")

        return False

    def _work_item_to_story(self, work_item: Dict) -> Dict:
        """
        Converte work item Azure DevOps para formato Story interno.

        Args:
            work_item: Work item no formato Azure DevOps

        Returns:
            Dict: Story no formato interno
        """
        fields = work_item.get("fields", {})

        # Extrai informacoes basicas
        title = fields.get("System.Title", "")
        description = fields.get("System.Description", "")

        # Remove HTML da descricao
        if description:
            import re
            description = re.sub(r'<[^>]+>', '', description)
            description = description.strip()

        # Status
        azure_state = fields.get("System.State", "New")
        internal_status = map_status_to_internal(azure_state, "azure_devops")

        # Prioridade
        priority = fields.get("Microsoft.VSTS.Common.Priority", 3)
        internal_priority = map_priority_to_internal(str(priority), "azure_devops")

        # Story Points
        story_points = fields.get("Microsoft.VSTS.Scheduling.StoryPoints") or \
                       fields.get("Microsoft.VSTS.Scheduling.Effort") or 0

        # Assignee
        assigned_to = fields.get("System.AssignedTo", {})
        assignee = assigned_to.get("displayName") if isinstance(assigned_to, dict) else None

        # Tags
        tags_str = fields.get("System.Tags", "")
        tags = [t.strip() for t in tags_str.split(";")] if tags_str else []

        # Acceptance Criteria
        acceptance_criteria_html = fields.get("Microsoft.VSTS.Common.AcceptanceCriteria", "")
        acceptance_criteria = []
        if acceptance_criteria_html:
            import re
            # Extrai itens de lista ou linhas
            items = re.findall(r'<li[^>]*>([^<]+)</li>', acceptance_criteria_html)
            if items:
                acceptance_criteria = [item.strip() for item in items]
            else:
                # Remove HTML e divide por linhas
                text = re.sub(r'<[^>]+>', '\n', acceptance_criteria_html)
                acceptance_criteria = [line.strip() for line in text.split('\n') if line.strip()]

        # Area e Iteration
        area_path = fields.get("System.AreaPath", "")
        iteration_path = fields.get("System.IterationPath", "")

        # URL do work item
        work_item_url = work_item.get("_links", {}).get("html", {}).get("href", "")

        return {
            "external_id": str(work_item.get("id")),
            "external_system": "azure_devops",
            "external_url": work_item_url,
            "title": title,
            "description": description,
            "status": internal_status,
            "priority": internal_priority,
            "story_points": int(story_points) if story_points else 0,
            "assignee": assignee,
            "tags": tags,
            "acceptance_criteria": acceptance_criteria,
            "category": self._work_item_type_to_category(fields.get("System.WorkItemType", "User Story")),
            "created_at": fields.get("System.CreatedDate"),
            "updated_at": fields.get("System.ChangedDate"),
            "external_data": {
                "work_item_type": fields.get("System.WorkItemType"),
                "area_path": area_path,
                "iteration_path": iteration_path,
                "parent_id": fields.get("System.Parent"),
                "created_by": fields.get("System.CreatedBy", {}).get("displayName") if isinstance(fields.get("System.CreatedBy"), dict) else None,
                "reason": fields.get("System.Reason")
            }
        }

    def _story_to_work_item_fields(self, story: Dict) -> Dict:
        """
        Converte Story interno para campos do Azure DevOps.

        Args:
            story: Story no formato interno

        Returns:
            Dict: Campos do work item
        """
        # Monta descricao HTML
        description_parts = []

        if story.get("description"):
            description_parts.append(f"<p>{story['description']}</p>")

        # Adiciona narrativa Agile se disponivel
        if story.get("persona") or story.get("action") or story.get("benefit"):
            narrative = f"<h3>User Story</h3><p>"
            narrative += f"<strong>Como</strong> {story.get('persona', '[persona]')}, "
            narrative += f"<strong>eu quero</strong> {story.get('action', '[acao]')}, "
            narrative += f"<strong>para que</strong> {story.get('benefit', '[beneficio]')}.</p>"
            description_parts.append(narrative)

        description = "".join(description_parts)

        # Monta acceptance criteria HTML
        acceptance_criteria = ""
        if story.get("acceptance_criteria"):
            acceptance_criteria = "<ul>"
            for ac in story["acceptance_criteria"]:
                acceptance_criteria += f"<li>{ac}</li>"
            acceptance_criteria += "</ul>"

        # Definition of Done como parte de acceptance criteria
        if story.get("definition_of_done"):
            acceptance_criteria += "<h4>Definition of Done</h4><ul>"
            for dod in story["definition_of_done"]:
                acceptance_criteria += f"<li>{dod}</li>"
            acceptance_criteria += "</ul>"

        # Monta campos
        fields = {
            "System.Title": story.get("title", "")[:255],
            "System.Description": description
        }

        # Priority
        if story.get("priority"):
            priority = map_priority_to_external(story["priority"], "azure_devops")
            fields["Microsoft.VSTS.Common.Priority"] = int(priority)

        # Story Points
        if story.get("story_points"):
            fields["Microsoft.VSTS.Scheduling.StoryPoints"] = story["story_points"]

        # Tags
        if story.get("tags"):
            fields["System.Tags"] = "; ".join(story["tags"])

        # Acceptance Criteria
        if acceptance_criteria:
            fields["Microsoft.VSTS.Common.AcceptanceCriteria"] = acceptance_criteria

        # Area Path
        if self.config.area_path:
            fields["System.AreaPath"] = self.config.area_path

        # Iteration Path
        if self.config.iteration_path:
            fields["System.IterationPath"] = self.config.iteration_path

        return fields

    def _work_item_type_to_category(self, work_item_type: str) -> str:
        """Converte tipo de work item Azure para categoria interna"""
        mapping = {
            "User Story": "feature",
            "Product Backlog Item": "feature",
            "Feature": "feature",
            "Bug": "bug",
            "Task": "feature",
            "Issue": "improvement",
            "Epic": "feature",
            "Technical Debt": "tech_debt"
        }
        return mapping.get(work_item_type, "feature")

    def _category_to_work_item_type(self, category: str) -> str:
        """Converte categoria interna para tipo de work item Azure"""
        mapping = {
            "feature": "User Story",
            "bug": "Bug",
            "tech_debt": "Task",
            "spike": "Task",
            "improvement": "Issue"
        }
        return mapping.get(category, self.config.default_work_item_type)

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """
        Sincroniza stories locais para o Azure DevOps.

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
            result.errors.append("Nao conectado ao Azure DevOps")
            return result

        self.status = IntegrationStatus.SYNCING

        for story in stories:
            try:
                external_id = story.get("external_id")
                fields = self._story_to_work_item_fields(story)

                if external_id and story.get("external_system") == "azure_devops":
                    # Atualiza work item existente
                    if await self.update_work_item(int(external_id), fields):
                        result.items_updated += 1
                    else:
                        result.items_failed += 1
                        result.errors.append(f"Falha ao atualizar work item {external_id}")
                else:
                    # Cria novo work item
                    work_item_type = self._category_to_work_item_type(story.get("category", "feature"))
                    created = await self.create_work_item(work_item_type, fields)
                    if created:
                        result.items_created += 1
                        result.details[story.get("story_id", "unknown")] = created.get("id")
                    else:
                        result.items_failed += 1
                        result.errors.append(f"Falha ao criar work item para {story.get('story_id')}")

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
        Sincroniza work items do Azure DevOps para stories locais.

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
            result.errors.append("Nao conectado ao Azure DevOps")
            return result

        self.status = IntegrationStatus.SYNCING

        try:
            # Busca User Stories e Product Backlog Items
            work_items = await self.query_work_items(max_results=200)

            for work_item in work_items:
                try:
                    story_data = self._work_item_to_story(work_item)
                    story_data["project_id"] = project_id
                    result.details["stories"].append(story_data)
                    result.items_synced += 1
                except Exception as e:
                    result.items_failed += 1
                    result.errors.append(f"Erro ao converter work item {work_item.get('id')}: {str(e)}")

        except Exception as e:
            result.success = False
            result.errors.append(f"Erro na sincronizacao: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def handle_webhook(self, payload: Dict) -> bool:
        """
        Processa webhook do Azure DevOps.

        Tipos de eventos suportados:
        - workitem.created
        - workitem.updated
        - workitem.deleted

        Args:
            payload: Payload do webhook

        Returns:
            bool: True se processado com sucesso
        """
        try:
            event_type = payload.get("eventType", "")
            resource = payload.get("resource", {})

            if not resource:
                logger.warning("Webhook sem dados de resource")
                return False

            # Work item events
            if event_type == "workitem.created":
                work_item_id = resource.get("id")
                logger.info(f"Webhook: Work item criado {work_item_id}")

                # Busca work item completo
                work_item = await self.get_work_item(work_item_id)
                if work_item:
                    story_data = self._work_item_to_story(work_item)
                    # Aqui seria chamado o callback para criar a story localmente
                return True

            elif event_type == "workitem.updated":
                work_item_id = resource.get("workItemId") or resource.get("id")
                logger.info(f"Webhook: Work item atualizado {work_item_id}")

                # Verifica campos alterados
                fields_changed = resource.get("fields", {})
                revision = resource.get("revision", {})

                if "System.State" in fields_changed:
                    old_state = fields_changed["System.State"].get("oldValue")
                    new_state = fields_changed["System.State"].get("newValue")
                    logger.info(f"Estado alterado: {old_state} -> {new_state}")

                return True

            elif event_type == "workitem.deleted":
                work_item_id = resource.get("id")
                logger.info(f"Webhook: Work item deletado {work_item_id}")
                return True

            logger.debug(f"Webhook ignorado: {event_type}")
            return True

        except Exception as e:
            logger.error(f"Erro ao processar webhook: {e}")
            return False

    async def get_iterations(self) -> List[Dict]:
        """
        Lista iterations (sprints) do projeto.

        Returns:
            List[Dict]: Lista de iterations
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            team = self.config.team or f"{self.config.project} Team"
            url = f"{self.project_url}/{team}/_apis/work/teamsettings/iterations"
            params = {"api-version": self.API_VERSION}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("value", [])
        except Exception as e:
            logger.error(f"Erro ao listar iterations: {e}")

        return []

    async def get_areas(self) -> List[Dict]:
        """
        Lista areas do projeto.

        Returns:
            List[Dict]: Lista de areas
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.project_url}/_apis/wit/classificationnodes/areas"
            params = {"api-version": self.API_VERSION, "$depth": 5}

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.json()
        except Exception as e:
            logger.error(f"Erro ao listar areas: {e}")

        return []

    def get_status(self) -> Dict[str, Any]:
        """Retorna status detalhado da integracao"""
        status = super().get_status()
        status.update({
            "system": "azure_devops",
            "organization": self.config.organization,
            "project": self.config.project,
            "team": self.config.team,
            "area_path": self.config.area_path,
            "iteration_path": self.config.iteration_path
        })
        return status


# Instancia global (singleton)
_azure_instance: Optional[AzureDevOpsIntegration] = None


def get_azure_devops_integration() -> AzureDevOpsIntegration:
    """Retorna instancia global da integracao Azure DevOps"""
    global _azure_instance
    if _azure_instance is None:
        config = AzureDevOpsConfig.from_env()
        _azure_instance = AzureDevOpsIntegration(config)
    return _azure_instance


async def init_azure_devops_integration() -> Optional[AzureDevOpsIntegration]:
    """Inicializa e conecta a integracao Azure DevOps se configurada"""
    azure = get_azure_devops_integration()
    if azure.config.is_valid() and azure.config.enabled:
        if await azure.connect():
            return azure
    return None

# -*- coding: utf-8 -*-
"""
Jira Integration Module
=======================
Integracao com Jira via API REST.

Funcionalidades:
- Conectar/desconectar via API REST
- Sincronizar issues <-> stories bidirecionalmente
- Mapear status Jira -> Kanban interno
- Webhook para atualizacoes em tempo real

Configuracao via variaveis de ambiente:
- JIRA_URL: URL base do Jira (ex: https://empresa.atlassian.net)
- JIRA_EMAIL: Email do usuario
- JIRA_API_TOKEN: Token de API
- JIRA_PROJECT_KEY: Chave do projeto padrao (ex: PROJ)
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


class JiraIssueType(str, Enum):
    """Tipos de issue no Jira"""
    STORY = "Story"
    TASK = "Task"
    BUG = "Bug"
    EPIC = "Epic"
    SUBTASK = "Sub-task"


@dataclass
class JiraConfig(IntegrationConfig):
    """Configuracao especifica para Jira"""
    url: str = ""
    email: str = ""
    api_token: str = ""
    project_key: str = ""
    default_issue_type: str = "Story"
    sync_comments: bool = True
    sync_attachments: bool = False
    custom_field_mapping: Dict[str, str] = field(default_factory=dict)
    webhook_secret: str = ""

    @classmethod
    def from_env(cls) -> "JiraConfig":
        """Cria configuracao a partir de variaveis de ambiente"""
        return cls(
            enabled=os.getenv("JIRA_ENABLED", "false").lower() == "true",
            url=os.getenv("JIRA_URL", ""),
            email=os.getenv("JIRA_EMAIL", ""),
            api_token=os.getenv("JIRA_API_TOKEN", ""),
            project_key=os.getenv("JIRA_PROJECT_KEY", ""),
            default_issue_type=os.getenv("JIRA_DEFAULT_ISSUE_TYPE", "Story"),
            sync_comments=os.getenv("JIRA_SYNC_COMMENTS", "true").lower() == "true",
            sync_attachments=os.getenv("JIRA_SYNC_ATTACHMENTS", "false").lower() == "true",
            webhook_secret=os.getenv("JIRA_WEBHOOK_SECRET", ""),
            auto_sync=os.getenv("JIRA_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("JIRA_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        """Verifica se a configuracao e valida"""
        return bool(self.url and self.email and self.api_token)


class JiraIntegration(IntegrationBase):
    """
    Integracao com Jira via API REST v3.

    Exemplo de uso:
    ```python
    config = JiraConfig.from_env()
    jira = JiraIntegration(config)

    if await jira.connect():
        result = await jira.sync_from_external("PROJ")
        print(f"Sincronizadas {result.items_synced} issues")
    ```
    """

    API_VERSION = "3"

    def __init__(self, config: JiraConfig):
        super().__init__(config)
        self.config: JiraConfig = config
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_info: Optional[Dict] = None

    @property
    def base_url(self) -> str:
        """URL base da API"""
        url = self.config.url.rstrip("/")
        return f"{url}/rest/api/{self.API_VERSION}"

    @property
    def auth_header(self) -> str:
        """Header de autenticacao Basic Auth"""
        credentials = f"{self.config.email}:{self.config.api_token}"
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
        Conecta ao Jira e valida credenciais.

        Returns:
            bool: True se conectado com sucesso
        """
        if not self.config.is_valid():
            self._last_error = "Configuracao invalida. Verifique URL, email e token."
            self.status = IntegrationStatus.ERROR
            return False

        self.status = IntegrationStatus.CONNECTING
        logger.info(f"Conectando ao Jira: {self.config.url}")

        try:
            session = await self._ensure_session()

            # Testa conexao buscando informacoes do usuario
            async with session.get(f"{self.base_url}/myself") as response:
                if response.status == 200:
                    self._user_info = await response.json()
                    self.status = IntegrationStatus.CONNECTED
                    logger.info(f"Conectado ao Jira como: {self._user_info.get('displayName')}")
                    return True
                elif response.status == 401:
                    self._last_error = "Credenciais invalidas"
                elif response.status == 403:
                    self._last_error = "Acesso negado. Verifique permissoes do token."
                else:
                    error_text = await response.text()
                    self._last_error = f"Erro {response.status}: {error_text}"

        except aiohttp.ClientError as e:
            self._last_error = f"Erro de conexao: {str(e)}"
        except Exception as e:
            self._last_error = f"Erro inesperado: {str(e)}"

        self.status = IntegrationStatus.ERROR
        logger.error(f"Falha ao conectar ao Jira: {self._last_error}")
        return False

    async def disconnect(self) -> bool:
        """
        Desconecta do Jira.

        Returns:
            bool: True se desconectado com sucesso
        """
        if self._session and not self._session.closed:
            await self._session.close()

        self._session = None
        self._user_info = None
        self.status = IntegrationStatus.DISCONNECTED
        logger.info("Desconectado do Jira")
        return True

    async def test_connection(self) -> bool:
        """
        Testa a conexao com o Jira.

        Returns:
            bool: True se a conexao esta funcionando
        """
        try:
            session = await self._ensure_session()
            async with session.get(f"{self.base_url}/myself") as response:
                return response.status == 200
        except Exception:
            return False

    async def get_projects(self) -> List[Dict]:
        """
        Lista projetos disponiveis no Jira.

        Returns:
            List[Dict]: Lista de projetos
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            async with session.get(f"{self.base_url}/project") as response:
                if response.status == 200:
                    return await response.json()
        except Exception as e:
            logger.error(f"Erro ao listar projetos: {e}")

        return []

    async def get_issue(self, issue_key: str) -> Optional[Dict]:
        """
        Busca uma issue especifica.

        Args:
            issue_key: Chave da issue (ex: PROJ-123)

        Returns:
            Dict ou None se nao encontrada
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}"
            params = {
                "expand": "renderedFields,transitions"
            }

            async with session.get(url, params=params) as response:
                if response.status == 200:
                    return await response.json()
                elif response.status == 404:
                    logger.warning(f"Issue nao encontrada: {issue_key}")
        except Exception as e:
            logger.error(f"Erro ao buscar issue {issue_key}: {e}")

        return None

    async def search_issues(
        self,
        project_key: Optional[str] = None,
        jql: Optional[str] = None,
        max_results: int = 50,
        start_at: int = 0
    ) -> List[Dict]:
        """
        Busca issues usando JQL.

        Args:
            project_key: Chave do projeto (opcional se jql fornecido)
            jql: Query JQL customizada
            max_results: Maximo de resultados
            start_at: Offset para paginacao

        Returns:
            List[Dict]: Lista de issues
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()

            if not jql:
                project = project_key or self.config.project_key
                jql = f"project = {project} ORDER BY updated DESC"

            params = {
                "jql": jql,
                "maxResults": max_results,
                "startAt": start_at,
                "expand": "renderedFields"
            }

            async with session.get(f"{self.base_url}/search", params=params) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("issues", [])
        except Exception as e:
            logger.error(f"Erro na busca JQL: {e}")

        return []

    async def create_issue(self, issue_data: Dict) -> Optional[Dict]:
        """
        Cria uma nova issue no Jira.

        Args:
            issue_data: Dados da issue no formato Jira

        Returns:
            Dict com a issue criada ou None
        """
        if not self.is_connected:
            return None

        try:
            session = await self._ensure_session()

            async with session.post(f"{self.base_url}/issue", json=issue_data) as response:
                if response.status == 201:
                    return await response.json()
                else:
                    error = await response.text()
                    logger.error(f"Erro ao criar issue: {error}")
        except Exception as e:
            logger.error(f"Erro ao criar issue: {e}")

        return None

    async def update_issue(self, issue_key: str, update_data: Dict) -> bool:
        """
        Atualiza uma issue existente.

        Args:
            issue_key: Chave da issue
            update_data: Dados para atualizar

        Returns:
            bool: True se atualizada com sucesso
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}"

            async with session.put(url, json=update_data) as response:
                return response.status == 204
        except Exception as e:
            logger.error(f"Erro ao atualizar issue {issue_key}: {e}")

        return False

    async def transition_issue(self, issue_key: str, transition_id: str) -> bool:
        """
        Muda o status de uma issue usando transicao.

        Args:
            issue_key: Chave da issue
            transition_id: ID da transicao

        Returns:
            bool: True se transicao realizada
        """
        if not self.is_connected:
            return False

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}/transitions"
            data = {"transition": {"id": transition_id}}

            async with session.post(url, json=data) as response:
                return response.status == 204
        except Exception as e:
            logger.error(f"Erro na transicao de {issue_key}: {e}")

        return False

    async def get_transitions(self, issue_key: str) -> List[Dict]:
        """
        Lista transicoes disponiveis para uma issue.

        Args:
            issue_key: Chave da issue

        Returns:
            List[Dict]: Lista de transicoes
        """
        if not self.is_connected:
            return []

        try:
            session = await self._ensure_session()
            url = f"{self.base_url}/issue/{issue_key}/transitions"

            async with session.get(url) as response:
                if response.status == 200:
                    data = await response.json()
                    return data.get("transitions", [])
        except Exception as e:
            logger.error(f"Erro ao listar transicoes: {e}")

        return []

    def _jira_issue_to_story(self, issue: Dict) -> Dict:
        """
        Converte issue Jira para formato Story interno.

        Args:
            issue: Issue no formato Jira

        Returns:
            Dict: Story no formato interno
        """
        fields = issue.get("fields", {})

        # Extrai informacoes basicas
        summary = fields.get("summary", "")
        description = fields.get("description", "")

        # Se descricao e um objeto (Atlassian Document Format), extrai texto
        if isinstance(description, dict):
            description = self._extract_text_from_adf(description)

        # Status
        status_obj = fields.get("status", {})
        jira_status = status_obj.get("name", "To Do")
        internal_status = map_status_to_internal(jira_status, "jira")

        # Prioridade
        priority_obj = fields.get("priority", {})
        jira_priority = priority_obj.get("name", "Medium")
        internal_priority = map_priority_to_internal(jira_priority, "jira")

        # Story Points (campo customizado comum)
        story_points = fields.get("customfield_10016") or fields.get("customfield_10024") or 0

        # Assignee
        assignee_obj = fields.get("assignee", {})
        assignee = assignee_obj.get("displayName") if assignee_obj else None

        # Epic
        epic_link = fields.get("customfield_10014") or fields.get("parent", {}).get("key")

        # Labels como tags
        labels = fields.get("labels", [])

        # Acceptance criteria (pode estar em campo customizado ou na descricao)
        acceptance_criteria = []
        if "acceptance" in description.lower():
            # Tenta extrair criterios de aceite da descricao
            lines = description.split("\n")
            in_ac_section = False
            for line in lines:
                if "acceptance" in line.lower() and "criteria" in line.lower():
                    in_ac_section = True
                    continue
                if in_ac_section:
                    if line.strip().startswith(("-", "*", "+")):
                        acceptance_criteria.append(line.strip().lstrip("-*+ "))
                    elif line.strip() and not line.strip().startswith("#"):
                        break

        return {
            "external_id": issue.get("key"),
            "external_system": "jira",
            "external_url": f"{self.config.url}/browse/{issue.get('key')}",
            "title": summary,
            "description": description,
            "status": internal_status,
            "priority": internal_priority,
            "story_points": int(story_points) if story_points else 0,
            "assignee": assignee,
            "epic_id": epic_link,
            "tags": labels,
            "acceptance_criteria": acceptance_criteria,
            "category": self._issue_type_to_category(fields.get("issuetype", {}).get("name", "Story")),
            "created_at": fields.get("created"),
            "updated_at": fields.get("updated"),
            "external_data": {
                "issue_type": fields.get("issuetype", {}).get("name"),
                "project_key": fields.get("project", {}).get("key"),
                "reporter": fields.get("reporter", {}).get("displayName") if fields.get("reporter") else None,
                "components": [c.get("name") for c in fields.get("components", [])],
                "fix_versions": [v.get("name") for v in fields.get("fixVersions", [])]
            }
        }

    def _story_to_jira_issue(self, story: Dict) -> Dict:
        """
        Converte Story interno para formato Jira.

        Args:
            story: Story no formato interno

        Returns:
            Dict: Issue no formato Jira
        """
        # Monta descricao com formato Atlassian Document
        description_parts = []

        if story.get("description"):
            description_parts.append(story["description"])

        # Adiciona narrativa Agile se disponivel
        if story.get("persona") or story.get("action") or story.get("benefit"):
            narrative = f"\n\n*User Story:*\nComo um {story.get('persona', '[persona]')}, "
            narrative += f"eu quero {story.get('action', '[acao]')}, "
            narrative += f"para que {story.get('benefit', '[beneficio]')}."
            description_parts.append(narrative)

        # Adiciona criterios de aceite
        if story.get("acceptance_criteria"):
            ac_text = "\n\n*Criterios de Aceite:*\n"
            for ac in story["acceptance_criteria"]:
                ac_text += f"* {ac}\n"
            description_parts.append(ac_text)

        # Adiciona Definition of Done
        if story.get("definition_of_done"):
            dod_text = "\n\n*Definition of Done:*\n"
            for dod in story["definition_of_done"]:
                dod_text += f"* {dod}\n"
            description_parts.append(dod_text)

        description = "".join(description_parts)

        # Monta campos da issue
        issue_data = {
            "fields": {
                "project": {"key": self.config.project_key},
                "summary": story.get("title", "")[:255],
                "description": description,
                "issuetype": {"name": self._category_to_issue_type(story.get("category", "feature"))},
                "labels": story.get("tags", [])
            }
        }

        # Adiciona prioridade se disponivel
        if story.get("priority"):
            jira_priority = map_priority_to_external(story["priority"], "jira")
            issue_data["fields"]["priority"] = {"name": jira_priority}

        # Story points (campo customizado - ajuste conforme sua instancia)
        if story.get("story_points"):
            # Tenta os campos customizados mais comuns
            issue_data["fields"]["customfield_10016"] = story["story_points"]

        return issue_data

    def _extract_text_from_adf(self, adf: Dict) -> str:
        """
        Extrai texto de Atlassian Document Format.

        Args:
            adf: Documento no formato ADF

        Returns:
            str: Texto extraido
        """
        if not isinstance(adf, dict):
            return str(adf) if adf else ""

        content = adf.get("content", [])
        text_parts = []

        for node in content:
            if node.get("type") == "paragraph":
                para_text = self._extract_text_from_adf_content(node.get("content", []))
                text_parts.append(para_text)
            elif node.get("type") == "bulletList":
                for item in node.get("content", []):
                    item_text = self._extract_text_from_adf_content(item.get("content", []))
                    text_parts.append(f"* {item_text}")
            elif node.get("type") == "heading":
                heading_text = self._extract_text_from_adf_content(node.get("content", []))
                level = node.get("attrs", {}).get("level", 1)
                text_parts.append(f"{'#' * level} {heading_text}")

        return "\n".join(text_parts)

    def _extract_text_from_adf_content(self, content: List[Dict]) -> str:
        """Extrai texto de uma lista de nodes ADF"""
        text_parts = []
        for node in content:
            if node.get("type") == "text":
                text_parts.append(node.get("text", ""))
            elif node.get("content"):
                text_parts.append(self._extract_text_from_adf_content(node["content"]))
        return "".join(text_parts)

    def _issue_type_to_category(self, issue_type: str) -> str:
        """Converte tipo de issue Jira para categoria interna"""
        mapping = {
            "Story": "feature",
            "Bug": "bug",
            "Task": "feature",
            "Technical Debt": "tech_debt",
            "Spike": "spike",
            "Improvement": "improvement",
            "Epic": "feature"
        }
        return mapping.get(issue_type, "feature")

    def _category_to_issue_type(self, category: str) -> str:
        """Converte categoria interna para tipo de issue Jira"""
        mapping = {
            "feature": "Story",
            "bug": "Bug",
            "tech_debt": "Task",
            "spike": "Task",
            "improvement": "Improvement"
        }
        return mapping.get(category, self.config.default_issue_type)

    async def sync_to_external(self, stories: List[Dict]) -> SyncResult:
        """
        Sincroniza stories locais para o Jira.

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
            result.errors.append("Nao conectado ao Jira")
            return result

        self.status = IntegrationStatus.SYNCING

        for story in stories:
            try:
                external_id = story.get("external_id")

                if external_id and story.get("external_system") == "jira":
                    # Atualiza issue existente
                    issue_data = self._story_to_jira_issue(story)
                    if await self.update_issue(external_id, issue_data):
                        result.items_updated += 1
                    else:
                        result.items_failed += 1
                        result.errors.append(f"Falha ao atualizar {external_id}")
                else:
                    # Cria nova issue
                    issue_data = self._story_to_jira_issue(story)
                    created = await self.create_issue(issue_data)
                    if created:
                        result.items_created += 1
                        result.details[story.get("story_id", "unknown")] = created.get("key")
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
        Sincroniza issues do Jira para stories locais.

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
            result.errors.append("Nao conectado ao Jira")
            return result

        self.status = IntegrationStatus.SYNCING

        try:
            # Busca issues do projeto
            issues = await self.search_issues(max_results=100)

            for issue in issues:
                try:
                    story_data = self._jira_issue_to_story(issue)
                    story_data["project_id"] = project_id
                    result.details["stories"].append(story_data)
                    result.items_synced += 1
                except Exception as e:
                    result.items_failed += 1
                    result.errors.append(f"Erro ao converter {issue.get('key')}: {str(e)}")

        except Exception as e:
            result.success = False
            result.errors.append(f"Erro na sincronizacao: {str(e)}")

        result.completed_at = datetime.utcnow()
        self.config.last_sync = result.completed_at
        self.status = IntegrationStatus.CONNECTED

        return result

    async def handle_webhook(self, payload: Dict) -> bool:
        """
        Processa webhook do Jira.

        Tipos de eventos suportados:
        - jira:issue_created
        - jira:issue_updated
        - jira:issue_deleted

        Args:
            payload: Payload do webhook

        Returns:
            bool: True se processado com sucesso
        """
        try:
            webhook_event = payload.get("webhookEvent", "")
            issue_data = payload.get("issue", {})

            if not issue_data:
                logger.warning("Webhook sem dados de issue")
                return False

            issue_key = issue_data.get("key")

            if webhook_event == "jira:issue_created":
                logger.info(f"Webhook: Issue criada {issue_key}")
                # Converte para story e retorna para processamento
                story_data = self._jira_issue_to_story(issue_data)
                # Aqui seria chamado o callback para criar a story localmente
                return True

            elif webhook_event == "jira:issue_updated":
                logger.info(f"Webhook: Issue atualizada {issue_key}")
                changelog = payload.get("changelog", {})
                items = changelog.get("items", [])

                # Verifica mudancas de status
                for item in items:
                    if item.get("field") == "status":
                        from_status = item.get("fromString")
                        to_status = item.get("toString")
                        logger.info(f"Status alterado: {from_status} -> {to_status}")

                return True

            elif webhook_event == "jira:issue_deleted":
                logger.info(f"Webhook: Issue deletada {issue_key}")
                return True

            logger.debug(f"Webhook ignorado: {webhook_event}")
            return True

        except Exception as e:
            logger.error(f"Erro ao processar webhook: {e}")
            return False

    def get_status(self) -> Dict[str, Any]:
        """Retorna status detalhado da integracao"""
        status = super().get_status()
        status.update({
            "system": "jira",
            "url": self.config.url,
            "project_key": self.config.project_key,
            "user": self._user_info.get("displayName") if self._user_info else None,
            "user_email": self._user_info.get("emailAddress") if self._user_info else None
        })
        return status


# Instancia global (singleton)
_jira_instance: Optional[JiraIntegration] = None


def get_jira_integration() -> JiraIntegration:
    """Retorna instancia global da integracao Jira"""
    global _jira_instance
    if _jira_instance is None:
        config = JiraConfig.from_env()
        _jira_instance = JiraIntegration(config)
    return _jira_instance


async def init_jira_integration() -> Optional[JiraIntegration]:
    """Inicializa e conecta a integracao Jira se configurada"""
    jira = get_jira_integration()
    if jira.config.is_valid() and jira.config.enabled:
        if await jira.connect():
            return jira
    return None

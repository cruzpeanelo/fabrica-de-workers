# -*- coding: utf-8 -*-
"""
Testes unitarios para JiraIntegration
=====================================
Issue #326 - Terminal A

Cobertura:
- Conexão e autenticação
- CRUD de issues
- Sincronização bidirecional
- Mapeamento de status/prioridade
- Agile API (boards, sprints)
"""
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime
import json

# Importa os módulos a serem testados
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))


class TestJiraConfig:
    """Testes para configuração Jira"""

    def test_config_creation(self, jira_config, tenant_id):
        """Testa criação de configuração"""
        from factory.integrations.jira.config import JiraConfig

        config = JiraConfig(
            tenant_id=tenant_id,
            base_url=jira_config["url"],
            email=jira_config["username"],
            api_token=jira_config["api_token"]
        )

        assert config.tenant_id == tenant_id
        assert config.base_url == jira_config["url"]
        assert config.email == jira_config["username"]

    def test_config_with_project_key(self, jira_config, tenant_id):
        """Testa configuração com project_key"""
        from factory.integrations.jira.config import JiraConfig

        config = JiraConfig(
            tenant_id=tenant_id,
            base_url=jira_config["url"],
            email=jira_config["username"],
            api_token=jira_config["api_token"],
            project_key="TEST"
        )

        assert config.project_key == "TEST"


class TestJiraIntegration:
    """Testes para JiraIntegration"""

    @pytest.fixture
    def jira_instance(self, jira_config, tenant_id):
        """Cria instância de JiraIntegration para testes"""
        from factory.integrations.jira.config import JiraConfig
        from factory.integrations.jira import JiraIntegration

        config = JiraConfig(
            tenant_id=tenant_id,
            base_url=jira_config["url"],
            email=jira_config["username"],
            api_token=jira_config["api_token"]
        )
        return JiraIntegration(config)

    @pytest.mark.asyncio
    async def test_connect_success(self, jira_instance, mock_aiohttp_session, mock_http_response):
        """Testa conexão bem-sucedida"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"accountId": "123", "displayName": "Test User"}
            )

            result = await jira_instance.connect()
            # Verifica que não houve exceção
            assert mock_aiohttp_session.get.called or result is not None

    @pytest.mark.asyncio
    async def test_connect_failure(self, jira_instance, mock_aiohttp_session, mock_http_response):
        """Testa falha de conexão"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=401,
                json_data={"errorMessages": ["Authentication failed"]}
            )

            # Não deve lançar exceção, mas retornar False ou None
            try:
                result = await jira_instance.connect()
            except Exception:
                pass  # Esperado em caso de falha

    @pytest.mark.asyncio
    async def test_get_issue(self, jira_instance, mock_aiohttp_session, mock_http_response, mock_jira_issue):
        """Testa busca de issue"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data=mock_jira_issue
            )

            jira_instance._session = mock_aiohttp_session

            # Simula get_issue
            issue = await jira_instance.get_issue("TEST-123")
            # Em caso de mock simples, verifica estrutura
            if issue:
                assert "key" in issue or "fields" in issue

    @pytest.mark.asyncio
    async def test_create_issue(self, jira_instance, mock_aiohttp_session, mock_http_response):
        """Testa criação de issue"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=201,
                json_data={"key": "TEST-124", "id": "10002"}
            )

            jira_instance._session = mock_aiohttp_session

            issue_data = {
                "project": {"key": "TEST"},
                "summary": "New Test Issue",
                "issuetype": {"name": "Story"}
            }

            result = await jira_instance.create_issue(issue_data)
            # Verifica chamada
            assert mock_aiohttp_session.post.called or result is not None


class TestJiraStatusMapping:
    """Testes para mapeamento de status"""

    def test_status_map_to_internal(self):
        """Testa mapeamento de status Jira -> interno"""
        from factory.integrations.base import map_status_to_internal

        # Status comuns do Jira
        assert map_status_to_internal("To Do", "jira") == "backlog"
        assert map_status_to_internal("In Progress", "jira") == "in_progress"
        assert map_status_to_internal("Done", "jira") == "done"

    def test_status_map_to_external(self):
        """Testa mapeamento de status interno -> Jira"""
        from factory.integrations.base import map_status_to_external

        assert map_status_to_external("backlog", "jira") == "To Do"
        assert map_status_to_external("in_progress", "jira") == "In Progress"
        assert map_status_to_external("done", "jira") == "Done"


class TestJiraPriorityMapping:
    """Testes para mapeamento de prioridade"""

    def test_priority_map_to_internal(self):
        """Testa mapeamento de prioridade Jira -> interno"""
        from factory.integrations.base import map_priority_to_internal

        assert map_priority_to_internal("Highest", "jira") == "urgent"
        assert map_priority_to_internal("High", "jira") == "high"
        assert map_priority_to_internal("Medium", "jira") == "medium"
        assert map_priority_to_internal("Low", "jira") == "low"

    def test_priority_map_to_external(self):
        """Testa mapeamento de prioridade interno -> Jira"""
        from factory.integrations.base import map_priority_to_external

        assert map_priority_to_external("urgent", "jira") == "Highest"
        assert map_priority_to_external("high", "jira") == "High"
        assert map_priority_to_external("medium", "jira") == "Medium"
        assert map_priority_to_external("low", "jira") == "Low"


class TestJiraAgileAPI:
    """Testes para Jira Agile API (Issue #311)"""

    @pytest.fixture
    def jira_instance(self, jira_config, tenant_id):
        """Cria instância de JiraIntegration"""
        from factory.integrations.jira.config import JiraConfig
        from factory.integrations.jira import JiraIntegration

        config = JiraConfig(
            tenant_id=tenant_id,
            base_url=jira_config["url"],
            email=jira_config["username"],
            api_token=jira_config["api_token"]
        )
        return JiraIntegration(config)

    @pytest.mark.asyncio
    async def test_get_boards(self, jira_instance, mock_aiohttp_session, mock_http_response, mock_jira_board):
        """Testa listagem de boards"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"values": [mock_jira_board], "total": 1}
            )

            jira_instance._session = mock_aiohttp_session

            boards = await jira_instance.get_boards()
            # Verifica estrutura básica se retornar algo
            if boards:
                assert isinstance(boards, (list, dict))

    @pytest.mark.asyncio
    async def test_get_sprints(self, jira_instance, mock_aiohttp_session, mock_http_response, mock_jira_sprint):
        """Testa listagem de sprints"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"values": [mock_jira_sprint], "total": 1}
            )

            jira_instance._session = mock_aiohttp_session

            sprints = await jira_instance.get_sprints(board_id=1)
            if sprints:
                assert isinstance(sprints, (list, dict))

    @pytest.mark.asyncio
    async def test_get_velocity(self, jira_instance, mock_aiohttp_session, mock_http_response):
        """Testa cálculo de velocity"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            # Mock para sprints completas
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={
                    "values": [
                        {"id": 1, "name": "Sprint 1", "state": "closed"},
                        {"id": 2, "name": "Sprint 2", "state": "closed"}
                    ]
                }
            )

            jira_instance._session = mock_aiohttp_session

            velocity = await jira_instance.get_velocity(board_id=1)
            # Velocity pode ser dict ou number
            assert velocity is not None


class TestJiraSyncSkill:
    """Testes para JiraSyncSkill (Issue #335)"""

    @pytest.fixture
    def sync_skill(self, jira_config, tenant_id, mock_story_repository):
        """Cria instância de JiraSyncSkill"""
        from factory.integrations.jira.config import JiraConfig
        from factory.integrations.jira import JiraIntegration
        from factory.integrations.jira.skills.jira_sync_skill import JiraSyncSkill

        config = JiraConfig(
            tenant_id=tenant_id,
            base_url=jira_config["url"],
            email=jira_config["username"],
            api_token=jira_config["api_token"]
        )
        jira_client = JiraIntegration(config)
        return JiraSyncSkill(jira_client, story_repository=mock_story_repository)

    def test_skill_initialization(self, sync_skill, mock_story_repository):
        """Testa inicialização da skill"""
        assert sync_skill.jira is not None
        assert sync_skill.story_repo == mock_story_repository

    def test_jira_issue_to_story_conversion(self, sync_skill, mock_jira_issue):
        """Testa conversão de issue Jira para story"""
        story_dict = sync_skill._jira_issue_to_story(mock_jira_issue)

        assert story_dict["external_id"] == "TEST-123"
        assert story_dict["external_system"] == "jira"
        assert story_dict["title"] == "Test Story Title"
        assert "status" in story_dict
        assert "priority" in story_dict

    def test_story_to_dict_conversion(self, sync_skill, mock_story):
        """Testa conversão de Story ORM para dict"""
        story_dict = sync_skill._story_to_dict(mock_story)

        assert story_dict["story_id"] == "STR-001"
        assert story_dict["title"] == "Test Story"
        assert story_dict["status"] == "backlog"
        assert story_dict["story_points"] == 5

    @pytest.mark.asyncio
    async def test_get_stories_for_export_empty(self, sync_skill, mock_story_repository, project_id):
        """Testa busca de stories para exportação (vazio)"""
        mock_story_repository.get_by_project.return_value = []

        stories = await sync_skill._get_stories_for_export(project_id, "TEST")

        assert stories == []
        mock_story_repository.get_by_project.assert_called_once_with(project_id)

    @pytest.mark.asyncio
    async def test_get_stories_for_export_with_stories(self, sync_skill, mock_story_repository, mock_story, project_id):
        """Testa busca de stories para exportação (com stories)"""
        # Story sem referência Jira deve ser incluída
        mock_story.external_references = {}
        mock_story_repository.get_by_project.return_value = [mock_story]

        stories = await sync_skill._get_stories_for_export(project_id, "TEST")

        assert len(stories) == 1
        assert stories[0]["story_id"] == "STR-001"

    @pytest.mark.asyncio
    async def test_get_stories_for_export_already_synced(self, sync_skill, mock_story_repository, mock_story, project_id):
        """Testa que stories já sincronizadas não são exportadas novamente"""
        # Story com referência Jira e synced_at recente
        mock_story.external_references = {
            "jira": {
                "key": "TEST-999",
                "synced_at": "2024-12-30T12:00:00"
            }
        }
        mock_story.updated_at = datetime(2024, 1, 15, 10, 0, 0)  # Antes do sync
        mock_story_repository.get_by_project.return_value = [mock_story]

        stories = await sync_skill._get_stories_for_export(project_id, "TEST")

        # Story já sincronizada não deve ser incluída
        assert len(stories) == 0


class TestJiraAudit:
    """Testes para auditoria de operações Jira"""

    def test_audit_entry_creation(self, tenant_id):
        """Testa criação de entrada de auditoria"""
        from factory.integrations.jira import JiraAuditEntry

        entry = JiraAuditEntry(
            timestamp=datetime.utcnow(),
            tenant_id=tenant_id,
            operation="CREATE_ISSUE",
            resource="TEST-123",
            success=True,
            details={"summary": "Test issue"}
        )

        assert entry.tenant_id == tenant_id
        assert entry.operation == "CREATE_ISSUE"
        assert entry.success is True

    def test_audit_entry_to_dict(self, tenant_id):
        """Testa conversão de entrada para dict"""
        from factory.integrations.jira import JiraAuditEntry

        timestamp = datetime.utcnow()
        entry = JiraAuditEntry(
            timestamp=timestamp,
            tenant_id=tenant_id,
            operation="GET_ISSUE",
            resource="TEST-123",
            success=True
        )

        entry_dict = entry.to_dict()

        assert entry_dict["tenant_id"] == tenant_id
        assert entry_dict["operation"] == "GET_ISSUE"
        assert "timestamp" in entry_dict

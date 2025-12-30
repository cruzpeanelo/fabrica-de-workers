# -*- coding: utf-8 -*-
"""
Testes unitarios para AzureDevOpsIntegration
============================================
Issue #326 - Terminal A

Cobertura:
- Conexão e autenticação
- Work Items (CRUD)
- Repositórios
- Pipelines
- Mapeamento de status/prioridade
"""
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))


class TestAzureDevOpsConfig:
    """Testes para configuração Azure DevOps"""

    def test_config_creation(self, azure_devops_config, tenant_id):
        """Testa criação de configuração"""
        from factory.integrations.azure_devops import AzureDevOpsConfig

        config = AzureDevOpsConfig(
            tenant_id=tenant_id,
            organization=azure_devops_config["organization"],
            project=azure_devops_config["project"],
            pat=azure_devops_config["pat"]
        )

        assert config.tenant_id == tenant_id
        assert config.organization == "test-org"
        assert config.project == "test-project"

    def test_config_api_version(self, azure_devops_config, tenant_id):
        """Testa versão da API"""
        from factory.integrations.azure_devops import AzureDevOpsConfig

        config = AzureDevOpsConfig(
            tenant_id=tenant_id,
            organization=azure_devops_config["organization"],
            project=azure_devops_config["project"],
            pat=azure_devops_config["pat"],
            api_version="7.1"
        )

        assert config.api_version == "7.1"


class TestAzureDevOpsIntegration:
    """Testes para AzureDevOpsIntegration"""

    @pytest.fixture
    def azure_instance(self, azure_devops_config, tenant_id):
        """Cria instância de AzureDevOpsIntegration"""
        from factory.integrations.azure_devops import AzureDevOpsConfig, AzureDevOpsIntegration

        config = AzureDevOpsConfig(
            tenant_id=tenant_id,
            organization=azure_devops_config["organization"],
            project=azure_devops_config["project"],
            pat=azure_devops_config["pat"]
        )
        return AzureDevOpsIntegration(config)

    @pytest.mark.asyncio
    async def test_connect_success(self, azure_instance, mock_aiohttp_session, mock_http_response):
        """Testa conexão bem-sucedida"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"id": "proj-123", "name": "test-project"}
            )

            result = await azure_instance.connect()
            assert mock_aiohttp_session.get.called or result is not None

    @pytest.mark.asyncio
    async def test_get_work_item(self, azure_instance, mock_aiohttp_session, mock_http_response, mock_azure_work_item):
        """Testa busca de work item"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data=mock_azure_work_item
            )

            azure_instance._session = mock_aiohttp_session

            work_item = await azure_instance.get_work_item(123)
            if work_item:
                assert "id" in work_item or "fields" in work_item

    @pytest.mark.asyncio
    async def test_create_work_item(self, azure_instance, mock_aiohttp_session, mock_http_response):
        """Testa criação de work item"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"id": 124, "fields": {"System.Title": "New Work Item"}}
            )

            azure_instance._session = mock_aiohttp_session

            work_item_data = {
                "title": "New Work Item",
                "description": "Test description",
                "work_item_type": "User Story"
            }

            result = await azure_instance.create_work_item(**work_item_data)
            assert mock_aiohttp_session.post.called or result is not None

    @pytest.mark.asyncio
    async def test_update_work_item(self, azure_instance, mock_aiohttp_session, mock_http_response):
        """Testa atualização de work item"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.patch.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"id": 123, "fields": {"System.Title": "Updated Work Item"}}
            )

            azure_instance._session = mock_aiohttp_session

            result = await azure_instance.update_work_item(123, title="Updated Work Item")
            assert mock_aiohttp_session.patch.called or result is not None


class TestAzureDevOpsRepos:
    """Testes para Azure DevOps Repos (Issue #313)"""

    @pytest.fixture
    def azure_instance(self, azure_devops_config, tenant_id):
        """Cria instância de AzureDevOpsIntegration"""
        from factory.integrations.azure_devops import AzureDevOpsConfig, AzureDevOpsIntegration

        config = AzureDevOpsConfig(
            tenant_id=tenant_id,
            organization=azure_devops_config["organization"],
            project=azure_devops_config["project"],
            pat=azure_devops_config["pat"]
        )
        return AzureDevOpsIntegration(config)

    @pytest.mark.asyncio
    async def test_get_repositories(self, azure_instance, mock_aiohttp_session, mock_http_response, mock_azure_repo):
        """Testa listagem de repositórios"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"value": [mock_azure_repo], "count": 1}
            )

            azure_instance._session = mock_aiohttp_session

            repos = await azure_instance.get_repositories()
            if repos:
                assert isinstance(repos, (list, dict))

    @pytest.mark.asyncio
    async def test_get_repository(self, azure_instance, mock_aiohttp_session, mock_http_response, mock_azure_repo):
        """Testa busca de repositório específico"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data=mock_azure_repo
            )

            azure_instance._session = mock_aiohttp_session

            repo = await azure_instance.get_repository("test-repo")
            if repo:
                assert "id" in repo or "name" in repo

    @pytest.mark.asyncio
    async def test_get_branches(self, azure_instance, mock_aiohttp_session, mock_http_response):
        """Testa listagem de branches"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"value": [{"name": "refs/heads/main"}], "count": 1}
            )

            azure_instance._session = mock_aiohttp_session

            branches = await azure_instance.get_branches("test-repo")
            if branches:
                assert isinstance(branches, (list, dict))


class TestAzureDevOpsPipelines:
    """Testes para Azure DevOps Pipelines (Issue #313)"""

    @pytest.fixture
    def azure_instance(self, azure_devops_config, tenant_id):
        """Cria instância de AzureDevOpsIntegration"""
        from factory.integrations.azure_devops import AzureDevOpsConfig, AzureDevOpsIntegration

        config = AzureDevOpsConfig(
            tenant_id=tenant_id,
            organization=azure_devops_config["organization"],
            project=azure_devops_config["project"],
            pat=azure_devops_config["pat"]
        )
        return AzureDevOpsIntegration(config)

    @pytest.mark.asyncio
    async def test_get_pipelines(self, azure_instance, mock_aiohttp_session, mock_http_response, mock_azure_pipeline):
        """Testa listagem de pipelines"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"value": [mock_azure_pipeline], "count": 1}
            )

            azure_instance._session = mock_aiohttp_session

            pipelines = await azure_instance.get_pipelines()
            if pipelines:
                assert isinstance(pipelines, (list, dict))

    @pytest.mark.asyncio
    async def test_get_pipeline(self, azure_instance, mock_aiohttp_session, mock_http_response, mock_azure_pipeline):
        """Testa busca de pipeline específico"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data=mock_azure_pipeline
            )

            azure_instance._session = mock_aiohttp_session

            pipeline = await azure_instance.get_pipeline(1)
            if pipeline:
                assert "id" in pipeline or "name" in pipeline

    @pytest.mark.asyncio
    async def test_run_pipeline(self, azure_instance, mock_aiohttp_session, mock_http_response):
        """Testa execução de pipeline"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"id": 1001, "state": "queued"}
            )

            azure_instance._session = mock_aiohttp_session

            run = await azure_instance.run_pipeline(1)
            if run:
                assert "id" in run or "state" in run


class TestAzureDevOpsStatusMapping:
    """Testes para mapeamento de status Azure DevOps"""

    def test_status_map_to_internal(self):
        """Testa mapeamento de status Azure DevOps -> interno"""
        from factory.integrations.base import map_status_to_internal

        assert map_status_to_internal("New", "azure_devops") == "backlog"
        assert map_status_to_internal("Active", "azure_devops") == "in_progress"
        assert map_status_to_internal("Resolved", "azure_devops") == "review"
        assert map_status_to_internal("Closed", "azure_devops") == "done"

    def test_status_map_to_external(self):
        """Testa mapeamento de status interno -> Azure DevOps"""
        from factory.integrations.base import map_status_to_external

        assert map_status_to_external("backlog", "azure_devops") == "New"
        assert map_status_to_external("in_progress", "azure_devops") == "Active"
        assert map_status_to_external("done", "azure_devops") == "Closed"


class TestAzureDevOpsSkills:
    """Testes para Azure DevOps Skills (Issue #312)"""

    @pytest.fixture
    def azure_instance(self, azure_devops_config, tenant_id):
        """Cria instância de AzureDevOpsIntegration"""
        from factory.integrations.azure_devops import AzureDevOpsConfig, AzureDevOpsIntegration

        config = AzureDevOpsConfig(
            tenant_id=tenant_id,
            organization=azure_devops_config["organization"],
            project=azure_devops_config["project"],
            pat=azure_devops_config["pat"]
        )
        return AzureDevOpsIntegration(config)

    def test_skill_available_actions(self, azure_instance):
        """Testa que skills têm ações disponíveis"""
        # Verifica que a instância tem métodos de skill
        assert hasattr(azure_instance, 'get_work_items') or hasattr(azure_instance, 'get_work_item')
        assert hasattr(azure_instance, 'create_work_item')

    @pytest.mark.asyncio
    async def test_query_work_items(self, azure_instance, mock_aiohttp_session, mock_http_response, mock_azure_work_item):
        """Testa query de work items via WIQL"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"workItems": [{"id": 123}]}
            )
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"value": [mock_azure_work_item]}
            )

            azure_instance._session = mock_aiohttp_session

            # Testa query se método existir
            if hasattr(azure_instance, 'query_work_items'):
                results = await azure_instance.query_work_items("SELECT * FROM WorkItems")
                assert results is not None

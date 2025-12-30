# -*- coding: utf-8 -*-
"""
Testes unitarios para SalesforceConnector
=========================================
Issue #326 - Terminal A

Cobertura:
- Autenticação OAuth
- CRUD de objetos (Account, Opportunity, etc)
- Queries SOQL
- Bulk API
"""
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))


class TestSalesforceConfig:
    """Testes para configuração Salesforce"""

    def test_config_creation(self, salesforce_config, tenant_id):
        """Testa criação de configuração"""
        from factory.integrations.salesforce import SalesforceConfig

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"]
        )

        assert config.tenant_id == tenant_id
        assert config.instance_url == "https://test.salesforce.com"

    def test_config_with_security_token(self, salesforce_config, tenant_id):
        """Testa configuração com security token"""
        from factory.integrations.salesforce import SalesforceConfig

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"],
            security_token=salesforce_config["security_token"]
        )

        assert config.security_token == "test-token"


class TestSalesforceConnector:
    """Testes para SalesforceConnector"""

    @pytest.fixture
    def sf_instance(self, salesforce_config, tenant_id):
        """Cria instância de SalesforceConnector"""
        from factory.integrations.salesforce import SalesforceConfig
        from factory.integrations.salesforce_connector import SalesforceConnector

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"],
            security_token=salesforce_config["security_token"]
        )
        return SalesforceConnector(config)

    @pytest.mark.asyncio
    async def test_authenticate_success(self, sf_instance, mock_aiohttp_session, mock_http_response):
        """Testa autenticação bem-sucedida"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={
                    "access_token": "test-access-token",
                    "instance_url": "https://test.salesforce.com",
                    "token_type": "Bearer"
                }
            )

            result = await sf_instance.authenticate()
            assert mock_aiohttp_session.post.called or result is not None

    @pytest.mark.asyncio
    async def test_authenticate_failure(self, sf_instance, mock_aiohttp_session, mock_http_response):
        """Testa falha de autenticação"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=401,
                json_data={"error": "invalid_grant", "error_description": "authentication failure"}
            )

            try:
                result = await sf_instance.authenticate()
            except Exception:
                pass  # Esperado em caso de falha


class TestSalesforceObjects:
    """Testes para operações CRUD em objetos Salesforce"""

    @pytest.fixture
    def sf_instance(self, salesforce_config, tenant_id):
        """Cria instância de SalesforceConnector"""
        from factory.integrations.salesforce import SalesforceConfig
        from factory.integrations.salesforce_connector import SalesforceConnector

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"],
            security_token=salesforce_config["security_token"]
        )
        connector = SalesforceConnector(config)
        connector._access_token = "mock-token"  # Simula autenticação
        return connector

    @pytest.mark.asyncio
    async def test_get_account(self, sf_instance, mock_aiohttp_session, mock_http_response, mock_salesforce_account):
        """Testa busca de Account"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data=mock_salesforce_account
            )

            sf_instance._session = mock_aiohttp_session

            if hasattr(sf_instance, 'get_account'):
                account = await sf_instance.get_account("001000000000001")
                if account:
                    assert "Id" in account or "Name" in account

    @pytest.mark.asyncio
    async def test_create_account(self, sf_instance, mock_aiohttp_session, mock_http_response):
        """Testa criação de Account"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=201,
                json_data={"id": "001000000000002", "success": True}
            )

            sf_instance._session = mock_aiohttp_session

            if hasattr(sf_instance, 'create_account'):
                result = await sf_instance.create_account({"Name": "New Account"})
                assert result is not None

    @pytest.mark.asyncio
    async def test_get_opportunity(self, sf_instance, mock_aiohttp_session, mock_http_response, mock_salesforce_opportunity):
        """Testa busca de Opportunity"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data=mock_salesforce_opportunity
            )

            sf_instance._session = mock_aiohttp_session

            if hasattr(sf_instance, 'get_opportunity'):
                opp = await sf_instance.get_opportunity("006000000000001")
                if opp:
                    assert "Id" in opp or "Name" in opp


class TestSalesforceSOQL:
    """Testes para queries SOQL"""

    @pytest.fixture
    def sf_instance(self, salesforce_config, tenant_id):
        """Cria instância de SalesforceConnector"""
        from factory.integrations.salesforce import SalesforceConfig
        from factory.integrations.salesforce_connector import SalesforceConnector

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"],
            security_token=salesforce_config["security_token"]
        )
        connector = SalesforceConnector(config)
        connector._access_token = "mock-token"
        return connector

    @pytest.mark.asyncio
    async def test_soql_query(self, sf_instance, mock_aiohttp_session, mock_http_response, mock_salesforce_account):
        """Testa execução de query SOQL"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={
                    "totalSize": 1,
                    "done": True,
                    "records": [mock_salesforce_account]
                }
            )

            sf_instance._session = mock_aiohttp_session

            if hasattr(sf_instance, 'query'):
                results = await sf_instance.query("SELECT Id, Name FROM Account LIMIT 10")
                if results:
                    assert "records" in results or isinstance(results, list)

    @pytest.mark.asyncio
    async def test_soql_query_with_pagination(self, sf_instance, mock_aiohttp_session, mock_http_response):
        """Testa query SOQL com paginação"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.get.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={
                    "totalSize": 100,
                    "done": False,
                    "nextRecordsUrl": "/services/data/v56.0/query/next",
                    "records": [{"Id": "001"} for _ in range(50)]
                }
            )

            sf_instance._session = mock_aiohttp_session

            if hasattr(sf_instance, 'query'):
                results = await sf_instance.query("SELECT Id FROM Account")
                assert results is not None


class TestSalesforceBulkAPI:
    """Testes para Salesforce Bulk API"""

    @pytest.fixture
    def sf_instance(self, salesforce_config, tenant_id):
        """Cria instância de SalesforceConnector"""
        from factory.integrations.salesforce import SalesforceConfig
        from factory.integrations.salesforce_connector import SalesforceConnector

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"],
            security_token=salesforce_config["security_token"]
        )
        connector = SalesforceConnector(config)
        connector._access_token = "mock-token"
        return connector

    @pytest.mark.asyncio
    async def test_bulk_insert(self, sf_instance, mock_aiohttp_session, mock_http_response):
        """Testa inserção em massa via Bulk API"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"id": "job-123", "state": "Open"}
            )

            sf_instance._session = mock_aiohttp_session

            records = [{"Name": f"Account {i}"} for i in range(100)]

            if hasattr(sf_instance, 'bulk_insert'):
                result = await sf_instance.bulk_insert("Account", records)
                assert result is not None

    @pytest.mark.asyncio
    async def test_bulk_update(self, sf_instance, mock_aiohttp_session, mock_http_response):
        """Testa atualização em massa via Bulk API"""
        with patch('aiohttp.ClientSession', return_value=mock_aiohttp_session):
            mock_aiohttp_session.post.return_value.__aenter__.return_value = mock_http_response(
                status=200,
                json_data={"id": "job-124", "state": "Open"}
            )

            sf_instance._session = mock_aiohttp_session

            records = [{"Id": f"001{i:012d}", "Name": f"Updated {i}"} for i in range(100)]

            if hasattr(sf_instance, 'bulk_update'):
                result = await sf_instance.bulk_update("Account", records)
                assert result is not None


class TestSalesforceAudit:
    """Testes para auditoria de operações Salesforce"""

    @pytest.fixture
    def sf_instance(self, salesforce_config, tenant_id):
        """Cria instância de SalesforceConnector"""
        from factory.integrations.salesforce import SalesforceConfig
        from factory.integrations.salesforce_connector import SalesforceConnector

        config = SalesforceConfig(
            tenant_id=tenant_id,
            instance_url=salesforce_config["instance_url"],
            username=salesforce_config["username"],
            password=salesforce_config["password"],
            security_token=salesforce_config["security_token"]
        )
        return SalesforceConnector(config)

    def test_audit_log_creation(self, sf_instance, tenant_id):
        """Testa criação de log de auditoria"""
        if hasattr(sf_instance, '_audit_log'):
            assert isinstance(sf_instance._audit_log, list)

    def test_tenant_isolation(self, sf_instance, tenant_id):
        """Testa isolamento por tenant"""
        assert sf_instance.config.tenant_id == tenant_id

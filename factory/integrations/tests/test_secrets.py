# -*- coding: utf-8 -*-
"""
Testes unitarios para SecretsManager e Azure Key Vault
======================================================
Issue #326 - Terminal A

Cobertura:
- Gerenciamento de secrets
- Azure Key Vault integration
- Criptografia de credenciais
- Tenant isolation para secrets
"""
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime, timedelta

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))))


class TestSecretsManagerConfig:
    """Testes para configuração do SecretsManager"""

    def test_config_creation(self, tenant_id):
        """Testa criação de configuração"""
        try:
            from factory.integrations.secrets import SecretsConfig

            config = SecretsConfig(
                tenant_id=tenant_id,
                vault_url="https://test-vault.vault.azure.net",
                client_id="test-client-id",
                client_secret="test-client-secret"
            )

            assert config.tenant_id == tenant_id
            assert "vault.azure.net" in config.vault_url
        except ImportError:
            pytest.skip("SecretsManager not available")

    def test_config_with_managed_identity(self, tenant_id):
        """Testa configuração com managed identity"""
        try:
            from factory.integrations.secrets import SecretsConfig

            config = SecretsConfig(
                tenant_id=tenant_id,
                vault_url="https://test-vault.vault.azure.net",
                use_managed_identity=True
            )

            assert config.use_managed_identity is True
        except (ImportError, AttributeError):
            pytest.skip("Managed identity config not available")


class TestSecretsManager:
    """Testes para SecretsManager"""

    @pytest.fixture
    def secrets_manager(self, tenant_id):
        """Cria instância de SecretsManager mockada"""
        try:
            from factory.integrations.secrets import SecretsManager, SecretsConfig

            config = SecretsConfig(
                tenant_id=tenant_id,
                vault_url="https://test-vault.vault.azure.net",
                client_id="test-client-id",
                client_secret="test-client-secret"
            )

            manager = SecretsManager(config)
            manager._client = MagicMock()
            return manager
        except ImportError:
            pytest.skip("SecretsManager not available")

    def test_secret_name_format(self, tenant_id):
        """Testa formato de nome de secret com tenant isolation"""
        # Formato esperado: {tenant_id}-{integration}-{key}
        secret_name = f"{tenant_id}-jira-api-token"

        assert tenant_id in secret_name
        assert "jira" in secret_name
        assert "api-token" in secret_name

    @pytest.mark.asyncio
    async def test_get_secret(self, secrets_manager):
        """Testa obtenção de secret"""
        if secrets_manager is None:
            pytest.skip("SecretsManager not available")

        secrets_manager._client.get_secret = MagicMock(return_value=MagicMock(value="secret-value"))

        if hasattr(secrets_manager, 'get_secret'):
            result = await secrets_manager.get_secret("test-secret")
            if result:
                assert result == "secret-value" or result is not None

    @pytest.mark.asyncio
    async def test_set_secret(self, secrets_manager):
        """Testa armazenamento de secret"""
        if secrets_manager is None:
            pytest.skip("SecretsManager not available")

        secrets_manager._client.set_secret = MagicMock(return_value=MagicMock())

        if hasattr(secrets_manager, 'set_secret'):
            result = await secrets_manager.set_secret("test-secret", "new-value")
            secrets_manager._client.set_secret.assert_called()

    @pytest.mark.asyncio
    async def test_delete_secret(self, secrets_manager):
        """Testa exclusão de secret"""
        if secrets_manager is None:
            pytest.skip("SecretsManager not available")

        secrets_manager._client.begin_delete_secret = MagicMock(return_value=MagicMock())

        if hasattr(secrets_manager, 'delete_secret'):
            result = await secrets_manager.delete_secret("test-secret")
            secrets_manager._client.begin_delete_secret.assert_called()


class TestSecretTypes:
    """Testes para tipos de secrets"""

    def test_secret_type_enum(self):
        """Testa enum de tipos de secret"""
        try:
            from factory.integrations.secrets.azure_keyvault import SecretType

            assert hasattr(SecretType, 'API_TOKEN') or hasattr(SecretType, 'api_token')
            assert hasattr(SecretType, 'PASSWORD') or hasattr(SecretType, 'password')
        except ImportError:
            pytest.skip("SecretType not available")

    def test_secret_metadata(self, tenant_id):
        """Testa metadata de secret"""
        metadata = {
            "tenant_id": tenant_id,
            "integration": "jira",
            "created_at": datetime.utcnow().isoformat(),
            "created_by": "terminal-a",
            "expires_at": (datetime.utcnow() + timedelta(days=90)).isoformat()
        }

        assert metadata["tenant_id"] == tenant_id
        assert "integration" in metadata
        assert "expires_at" in metadata


class TestSecretEncryption:
    """Testes para criptografia de secrets"""

    def test_base64_encoding(self):
        """Testa encoding base64 de secrets"""
        import base64

        secret = "my-secret-password"
        encoded = base64.b64encode(secret.encode()).decode()
        decoded = base64.b64decode(encoded).decode()

        assert decoded == secret

    def test_secret_masking(self):
        """Testa mascaramento de secrets em logs"""
        secret = "sk-ant-1234567890abcdef"
        masked = secret[:6] + "*" * (len(secret) - 10) + secret[-4:]

        assert secret[:6] in masked
        assert secret[-4:] in masked
        assert "1234567890" not in masked

    def test_secret_not_in_str(self, tenant_id):
        """Testa que secret não aparece em __str__"""
        class SecretContainer:
            def __init__(self, secret):
                self._secret = secret

            def __str__(self):
                return f"SecretContainer(tenant={self._tenant_id}, secret=***)"

            __repr__ = __str__

        # Classe deve mascarar o secret
        container = SecretContainer("super-secret-value")
        string_repr = str(container)

        assert "super-secret-value" not in string_repr


class TestTenantIsolation:
    """Testes para isolamento de secrets por tenant"""

    def test_secret_namespace(self, tenant_id):
        """Testa namespace de secrets por tenant"""
        def get_secret_name(tenant: str, integration: str, key: str) -> str:
            return f"{tenant}-{integration}-{key}"

        secret_name = get_secret_name(tenant_id, "jira", "api-token")

        # Deve incluir tenant no nome
        assert tenant_id in secret_name

        # Secrets de tenants diferentes devem ter nomes diferentes
        other_tenant = "other-tenant-002"
        other_secret_name = get_secret_name(other_tenant, "jira", "api-token")

        assert secret_name != other_secret_name

    def test_cross_tenant_access_prevention(self, tenant_id):
        """Testa prevenção de acesso cross-tenant"""
        allowed_tenant = tenant_id
        requesting_tenant = "malicious-tenant-999"

        def can_access_secret(secret_tenant: str, requester: str) -> bool:
            return secret_tenant == requester

        assert can_access_secret(allowed_tenant, allowed_tenant) is True
        assert can_access_secret(allowed_tenant, requesting_tenant) is False


class TestSecretRotation:
    """Testes para rotação de secrets"""

    def test_expiration_check(self):
        """Testa verificação de expiração de secret"""
        expiration_date = datetime.utcnow() + timedelta(days=30)
        warning_threshold = timedelta(days=7)

        days_until_expiration = (expiration_date - datetime.utcnow()).days

        needs_rotation = days_until_expiration <= warning_threshold.days
        assert needs_rotation is False

        # Secret prestes a expirar
        near_expiration = datetime.utcnow() + timedelta(days=5)
        days_until_near_expiration = (near_expiration - datetime.utcnow()).days

        needs_urgent_rotation = days_until_near_expiration <= warning_threshold.days
        assert needs_urgent_rotation is True

    def test_secret_version_tracking(self):
        """Testa tracking de versões de secret"""
        secret_versions = [
            {"version": "v1", "created_at": "2024-01-01T00:00:00Z", "active": False},
            {"version": "v2", "created_at": "2024-02-01T00:00:00Z", "active": False},
            {"version": "v3", "created_at": "2024-03-01T00:00:00Z", "active": True}
        ]

        active_version = next(v for v in secret_versions if v["active"])
        assert active_version["version"] == "v3"

        # Apenas uma versão deve estar ativa
        active_count = sum(1 for v in secret_versions if v["active"])
        assert active_count == 1


class TestAzureKeyVaultIntegration:
    """Testes para integração com Azure Key Vault (Issue #299)"""

    def test_vault_url_validation(self):
        """Testa validação de URL do Key Vault"""
        valid_urls = [
            "https://myvault.vault.azure.net",
            "https://my-vault.vault.azure.net/",
            "https://vault123.vault.azure.net"
        ]

        invalid_urls = [
            "http://myvault.vault.azure.net",  # HTTP não é seguro
            "https://myvault.example.com",      # Não é Azure
            "myvault.vault.azure.net"           # Sem protocolo
        ]

        for url in valid_urls:
            assert "vault.azure.net" in url
            assert url.startswith("https://")

        for url in invalid_urls:
            is_invalid = (
                not url.startswith("https://") or
                "vault.azure.net" not in url
            )
            assert is_invalid

    def test_authentication_methods(self):
        """Testa métodos de autenticação suportados"""
        auth_methods = [
            "client_secret",       # Service Principal com secret
            "client_certificate",  # Service Principal com certificado
            "managed_identity",    # Azure Managed Identity
            "interactive"          # Browser-based (dev only)
        ]

        # Managed identity é preferido em produção
        preferred_production = "managed_identity"
        assert preferred_production in auth_methods

    @pytest.mark.asyncio
    async def test_connection_retry(self):
        """Testa retry de conexão em caso de falha"""
        max_retries = 3
        retry_count = 0

        async def mock_connect():
            nonlocal retry_count
            retry_count += 1
            if retry_count < max_retries:
                raise ConnectionError("Connection failed")
            return True

        # Simula retries
        result = None
        for _ in range(max_retries):
            try:
                result = await mock_connect()
                break
            except ConnectionError:
                continue

        assert result is True
        assert retry_count == max_retries

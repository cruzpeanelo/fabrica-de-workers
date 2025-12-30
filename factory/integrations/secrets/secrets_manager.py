# -*- coding: utf-8 -*-
"""
Secrets Manager
===============
Gerenciador unificado de secrets com suporte a Azure Key Vault e fallback local.

Terminal 5 - Issue #299
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, List, Optional, Union
from enum import Enum

from .azure_keyvault import AzureKeyVaultClient, AzureKeyVaultConfig, SecretType
from .local_encryption import LocalEncryption, LocalEncryptionConfig

logger = logging.getLogger(__name__)


class StorageBackend(str, Enum):
    """Backends de armazenamento suportados"""
    AZURE_KEY_VAULT = "azure_key_vault"
    LOCAL = "local"
    AUTO = "auto"  # Tenta Azure, fallback para local


@dataclass
class SecretValue:
    """Valor de um secret com metadados"""
    value: str
    name: str
    tenant_id: Optional[str] = None
    backend: StorageBackend = StorageBackend.AUTO
    cached: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class SecretsManagerConfig:
    """
    Configuracao do Secrets Manager.

    Attributes:
        backend: Backend principal (auto, azure_key_vault, local)
        azure_config: Configuracao do Azure Key Vault
        local_config: Configuracao de criptografia local
        fallback_to_local: Usar local como fallback se Azure falhar
    """
    backend: StorageBackend = StorageBackend.AUTO
    azure_config: Optional[AzureKeyVaultConfig] = None
    local_config: Optional[LocalEncryptionConfig] = None
    fallback_to_local: bool = True


class SecretsManager:
    """
    Gerenciador unificado de secrets.

    Abstrai o acesso a secrets com suporte a:
    - Azure Key Vault (producao)
    - Criptografia local (desenvolvimento)
    - Fallback automatico

    Isolamento por tenant garantido atraves do tenant_id.

    Exemplo:
        # Configuracao para producao
        config = SecretsManagerConfig(
            backend=StorageBackend.AUTO,
            azure_config=AzureKeyVaultConfig(
                vault_url="https://meu-vault.vault.azure.net/",
                tenant_id="...",
                client_id="...",
                client_secret="..."
            ),
            fallback_to_local=True
        )

        manager = SecretsManager(config)

        # Salvar credencial de integracao
        await manager.set_integration_secret(
            tenant_id="TENANT-001",
            integration="salesforce",
            secret_type=SecretType.CLIENT_SECRET,
            value="..."
        )

        # Recuperar credencial
        secret = await manager.get_integration_secret(
            tenant_id="TENANT-001",
            integration="salesforce",
            secret_type=SecretType.CLIENT_SECRET
        )
    """

    def __init__(self, config: SecretsManagerConfig):
        """
        Inicializa o Secrets Manager.

        Args:
            config: Configuracao do manager
        """
        self.config = config
        self._azure_client: Optional[AzureKeyVaultClient] = None
        self._local_client: Optional[LocalEncryption] = None
        self._active_backend: Optional[StorageBackend] = None

        self._initialize_backends()

    def _initialize_backends(self):
        """Inicializa os backends configurados"""
        if self.config.backend in [StorageBackend.AZURE_KEY_VAULT, StorageBackend.AUTO]:
            if self.config.azure_config:
                try:
                    self._azure_client = AzureKeyVaultClient(self.config.azure_config)
                    self._active_backend = StorageBackend.AZURE_KEY_VAULT
                    logger.info("Azure Key Vault inicializado")
                except ImportError as e:
                    logger.warning(f"Azure SDK nao disponivel: {e}")
                except Exception as e:
                    logger.warning(f"Erro ao inicializar Azure Key Vault: {e}")

        if (
            self.config.backend == StorageBackend.LOCAL or
            (self.config.fallback_to_local and self._azure_client is None)
        ):
            if self.config.local_config is None:
                self.config.local_config = LocalEncryptionConfig()

            try:
                self._local_client = LocalEncryption(self.config.local_config)
                if self._active_backend is None:
                    self._active_backend = StorageBackend.LOCAL
                logger.info("Criptografia local inicializada")
            except ImportError as e:
                logger.warning(f"Cryptography nao disponivel: {e}")
            except Exception as e:
                logger.warning(f"Erro ao inicializar criptografia local: {e}")

        if self._active_backend is None:
            raise RuntimeError("Nenhum backend de secrets disponivel")

    def _get_client(self) -> Union[AzureKeyVaultClient, LocalEncryption]:
        """Retorna o cliente ativo"""
        if self._active_backend == StorageBackend.AZURE_KEY_VAULT:
            return self._azure_client
        return self._local_client

    def _build_secret_name(
        self,
        integration: str,
        secret_type: SecretType,
        suffix: Optional[str] = None
    ) -> str:
        """
        Constroi nome padronizado do secret.

        Formato: integration-{integration}-{secret_type}[-{suffix}]

        Args:
            integration: Nome da integracao
            secret_type: Tipo do secret
            suffix: Sufixo adicional

        Returns:
            Nome do secret
        """
        name = f"integration-{integration}-{secret_type.value}"
        if suffix:
            name = f"{name}-{suffix}"
        return name

    # =========================================================================
    # API Principal - Secrets de Integracao
    # =========================================================================

    async def get_integration_secret(
        self,
        tenant_id: str,
        integration: str,
        secret_type: SecretType,
        suffix: Optional[str] = None
    ) -> Optional[SecretValue]:
        """
        Recupera um secret de integracao.

        Args:
            tenant_id: ID do tenant
            integration: Nome da integracao (salesforce, sap, teams, etc)
            secret_type: Tipo do secret
            suffix: Sufixo opcional

        Returns:
            SecretValue ou None se nao encontrado
        """
        name = self._build_secret_name(integration, secret_type, suffix)

        try:
            if self._azure_client:
                value = await self._azure_client.get_secret(name, tenant_id)
                if value:
                    return SecretValue(
                        value=value,
                        name=name,
                        tenant_id=tenant_id,
                        backend=StorageBackend.AZURE_KEY_VAULT,
                        metadata={"integration": integration, "secret_type": secret_type.value}
                    )

            # Fallback para local
            if self._local_client and self.config.fallback_to_local:
                value = self._local_client.get_secret(name, tenant_id)
                if value:
                    return SecretValue(
                        value=value,
                        name=name,
                        tenant_id=tenant_id,
                        backend=StorageBackend.LOCAL,
                        metadata={"integration": integration, "secret_type": secret_type.value}
                    )

        except Exception as e:
            logger.error(f"Erro ao recuperar secret {name}: {e}")

        return None

    async def set_integration_secret(
        self,
        tenant_id: str,
        integration: str,
        secret_type: SecretType,
        value: str,
        suffix: Optional[str] = None,
        expires_on: Optional[datetime] = None
    ) -> bool:
        """
        Salva um secret de integracao.

        Args:
            tenant_id: ID do tenant
            integration: Nome da integracao
            secret_type: Tipo do secret
            value: Valor do secret
            suffix: Sufixo opcional
            expires_on: Data de expiracao

        Returns:
            True se salvo com sucesso
        """
        name = self._build_secret_name(integration, secret_type, suffix)

        try:
            if self._azure_client:
                success = await self._azure_client.set_secret(
                    name=name,
                    value=value,
                    tenant_id=tenant_id,
                    secret_type=secret_type,
                    expires_on=expires_on,
                    tags={"integration": integration}
                )
                if success:
                    return True

            # Fallback para local
            if self._local_client and self.config.fallback_to_local:
                return self._local_client.set_secret(
                    name=name,
                    value=value,
                    tenant_id=tenant_id,
                    tags={"integration": integration, "secret_type": secret_type.value}
                )

        except Exception as e:
            logger.error(f"Erro ao salvar secret {name}: {e}")

        return False

    async def delete_integration_secret(
        self,
        tenant_id: str,
        integration: str,
        secret_type: SecretType,
        suffix: Optional[str] = None
    ) -> bool:
        """
        Remove um secret de integracao.

        Args:
            tenant_id: ID do tenant
            integration: Nome da integracao
            secret_type: Tipo do secret
            suffix: Sufixo opcional

        Returns:
            True se removido com sucesso
        """
        name = self._build_secret_name(integration, secret_type, suffix)

        try:
            if self._azure_client:
                success = await self._azure_client.delete_secret(name, tenant_id)
                if success:
                    return True

            if self._local_client:
                return self._local_client.delete_secret(name, tenant_id)

        except Exception as e:
            logger.error(f"Erro ao deletar secret {name}: {e}")

        return False

    # =========================================================================
    # API de Conveniencia para Integracoes Especificas
    # =========================================================================

    async def get_salesforce_credentials(
        self,
        tenant_id: str
    ) -> Optional[Dict[str, str]]:
        """
        Recupera credenciais Salesforce de um tenant.

        Returns:
            Dict com client_id, client_secret, username, password, security_token
        """
        creds = {}

        for secret_type, key in [
            (SecretType.CLIENT_SECRET, "client_id"),
            (SecretType.CLIENT_SECRET, "client_secret"),
            (SecretType.PASSWORD, "username"),
            (SecretType.PASSWORD, "password"),
            (SecretType.API_KEY, "security_token")
        ]:
            secret = await self.get_integration_secret(
                tenant_id, "salesforce", secret_type, key
            )
            if secret:
                creds[key] = secret.value

        return creds if creds else None

    async def get_sap_credentials(
        self,
        tenant_id: str,
        system: str = "s4"  # s4, ecc, cpi
    ) -> Optional[Dict[str, str]]:
        """
        Recupera credenciais SAP de um tenant.

        Returns:
            Dict com username, password, client_id, client_secret
        """
        integration = f"sap-{system}"
        creds = {}

        for secret_type, key in [
            (SecretType.PASSWORD, "username"),
            (SecretType.PASSWORD, "password"),
            (SecretType.CLIENT_SECRET, "client_id"),
            (SecretType.CLIENT_SECRET, "client_secret")
        ]:
            secret = await self.get_integration_secret(
                tenant_id, integration, secret_type, key
            )
            if secret:
                creds[key] = secret.value

        return creds if creds else None

    async def get_microsoft_graph_credentials(
        self,
        tenant_id: str
    ) -> Optional[Dict[str, str]]:
        """
        Recupera credenciais Microsoft Graph de um tenant.

        Returns:
            Dict com tenant_id, client_id, client_secret
        """
        creds = {}

        for secret_type, key in [
            (SecretType.API_KEY, "azure_tenant_id"),
            (SecretType.CLIENT_SECRET, "client_id"),
            (SecretType.CLIENT_SECRET, "client_secret")
        ]:
            secret = await self.get_integration_secret(
                tenant_id, "microsoft-graph", secret_type, key
            )
            if secret:
                creds[key] = secret.value

        return creds if creds else None

    # =========================================================================
    # Listagem e Auditoria
    # =========================================================================

    async def list_integration_secrets(
        self,
        tenant_id: str,
        integration: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista secrets de integracao de um tenant.

        Args:
            tenant_id: ID do tenant
            integration: Filtrar por integracao

        Returns:
            Lista de secrets (sem valores)
        """
        secrets = []

        if self._azure_client:
            try:
                azure_secrets = await self._azure_client.list_secrets(tenant_id)
                secrets.extend(azure_secrets)
            except Exception as e:
                logger.warning(f"Erro ao listar secrets do Azure: {e}")

        if self._local_client:
            local_secrets = self._local_client.list_secrets(tenant_id)
            secrets.extend(local_secrets)

        # Filtra por integracao se especificado
        if integration:
            secrets = [
                s for s in secrets
                if s.get("tags", {}).get("integration") == integration or
                f"integration-{integration}" in s.get("name", "")
            ]

        return secrets

    def get_audit_log(
        self,
        tenant_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retorna log de auditoria combinado.

        Args:
            tenant_id: Filtrar por tenant
            limit: Limite de registros

        Returns:
            Lista de registros de auditoria
        """
        logs = []

        if self._azure_client:
            logs.extend(self._azure_client.get_audit_log(tenant_id, limit))

        if self._local_client:
            logs.extend(self._local_client.get_audit_log(tenant_id, limit))

        # Ordena por timestamp
        logs.sort(key=lambda x: x.get("timestamp", ""), reverse=True)

        return logs[:limit]

    def get_stats(self) -> Dict[str, Any]:
        """
        Retorna estatisticas do manager.

        Returns:
            Estatisticas combinadas
        """
        stats = {
            "active_backend": self._active_backend.value if self._active_backend else None,
            "azure_available": self._azure_client is not None,
            "local_available": self._local_client is not None,
            "fallback_enabled": self.config.fallback_to_local
        }

        if self._azure_client:
            stats["azure"] = self._azure_client.get_stats()

        if self._local_client:
            stats["local"] = self._local_client.get_stats()

        return stats

    # =========================================================================
    # Rotacao de Secrets
    # =========================================================================

    async def rotate_integration_secret(
        self,
        tenant_id: str,
        integration: str,
        secret_type: SecretType,
        new_value: str,
        suffix: Optional[str] = None
    ) -> bool:
        """
        Rotaciona um secret de integracao.

        Args:
            tenant_id: ID do tenant
            integration: Nome da integracao
            secret_type: Tipo do secret
            new_value: Novo valor
            suffix: Sufixo opcional

        Returns:
            True se rotacionado com sucesso
        """
        return await self.set_integration_secret(
            tenant_id, integration, secret_type, new_value, suffix
        )

    async def rotate_all_tenant_secrets(
        self,
        tenant_id: str,
        integration: str,
        new_credentials: Dict[str, str]
    ) -> Dict[str, bool]:
        """
        Rotaciona todos os secrets de uma integracao.

        Args:
            tenant_id: ID do tenant
            integration: Nome da integracao
            new_credentials: Dict com tipo -> novo valor

        Returns:
            Dict com tipo -> sucesso
        """
        results = {}

        for secret_type_str, value in new_credentials.items():
            try:
                secret_type = SecretType(secret_type_str)
                success = await self.set_integration_secret(
                    tenant_id, integration, secret_type, value
                )
                results[secret_type_str] = success
            except ValueError:
                results[secret_type_str] = False
                logger.warning(f"Tipo de secret invalido: {secret_type_str}")

        return results

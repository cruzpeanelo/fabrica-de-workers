# -*- coding: utf-8 -*-
"""
Azure Key Vault Client
======================
Cliente para Azure Key Vault para gerenciamento seguro de secrets.

Terminal 5 - Issue #299
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum

logger = logging.getLogger(__name__)

# Tenta importar azure-identity e azure-keyvault-secrets
try:
    from azure.identity import ClientSecretCredential, DefaultAzureCredential
    from azure.keyvault.secrets import SecretClient
    from azure.core.exceptions import ResourceNotFoundError, HttpResponseError
    AZURE_SDK_AVAILABLE = True
except ImportError:
    AZURE_SDK_AVAILABLE = False
    logger.warning("Azure SDK nao instalado. Use: pip install azure-identity azure-keyvault-secrets")


class SecretType(str, Enum):
    """Tipos de secrets suportados"""
    API_KEY = "api_key"
    CLIENT_SECRET = "client_secret"
    PASSWORD = "password"
    CONNECTION_STRING = "connection_string"
    CERTIFICATE = "certificate"
    PRIVATE_KEY = "private_key"


@dataclass
class AzureKeyVaultConfig:
    """
    Configuracao do Azure Key Vault.

    Attributes:
        vault_url: URL do Key Vault (https://{vault-name}.vault.azure.net/)
        tenant_id: Azure AD Tenant ID
        client_id: Azure AD Application ID
        client_secret: Azure AD Application Secret
        use_managed_identity: Usar Managed Identity ao inves de client credentials
        cache_ttl_seconds: Tempo de cache para secrets em segundos
    """
    vault_url: str = ""
    tenant_id: str = ""
    client_id: str = ""
    client_secret: str = ""
    use_managed_identity: bool = False
    cache_ttl_seconds: int = 300  # 5 minutos
    max_retries: int = 3
    retry_delay_seconds: float = 1.0

    def validate(self) -> List[str]:
        """Valida a configuracao"""
        errors = []
        if not self.vault_url:
            errors.append("vault_url e obrigatorio")
        if not self.use_managed_identity:
            if not self.tenant_id:
                errors.append("tenant_id e obrigatorio quando nao usa managed identity")
            if not self.client_id:
                errors.append("client_id e obrigatorio quando nao usa managed identity")
            if not self.client_secret:
                errors.append("client_secret e obrigatorio quando nao usa managed identity")
        return errors


@dataclass
class CachedSecret:
    """Secret em cache com expiracao"""
    value: str
    cached_at: datetime
    expires_at: datetime
    metadata: Dict[str, Any] = field(default_factory=dict)

    @property
    def is_expired(self) -> bool:
        return datetime.utcnow() >= self.expires_at


class AzureKeyVaultClient:
    """
    Cliente para Azure Key Vault.

    Permite:
    - Criar, ler, atualizar e deletar secrets
    - Cache local com TTL configuravel
    - Audit logging de acessos
    - Suporte a Managed Identity e Client Credentials

    Exemplo:
        config = AzureKeyVaultConfig(
            vault_url="https://meu-vault.vault.azure.net/",
            tenant_id="...",
            client_id="...",
            client_secret="..."
        )
        client = AzureKeyVaultClient(config)

        # Salvar secret
        await client.set_secret("api-key-cliente-001", "sk-...")

        # Recuperar secret
        value = await client.get_secret("api-key-cliente-001")
    """

    def __init__(self, config: AzureKeyVaultConfig):
        """
        Inicializa o cliente.

        Args:
            config: Configuracao do Key Vault
        """
        if not AZURE_SDK_AVAILABLE:
            raise ImportError(
                "Azure SDK nao instalado. Use: pip install azure-identity azure-keyvault-secrets"
            )

        self.config = config
        self._client: Optional[SecretClient] = None
        self._cache: Dict[str, CachedSecret] = {}
        self._audit_log: List[Dict[str, Any]] = []

    def _get_credential(self):
        """Obtem credencial Azure"""
        if self.config.use_managed_identity:
            return DefaultAzureCredential()
        else:
            return ClientSecretCredential(
                tenant_id=self.config.tenant_id,
                client_id=self.config.client_id,
                client_secret=self.config.client_secret
            )

    def _get_client(self) -> SecretClient:
        """Obtem ou cria cliente do Key Vault"""
        if self._client is None:
            credential = self._get_credential()
            self._client = SecretClient(
                vault_url=self.config.vault_url,
                credential=credential
            )
        return self._client

    def _log_access(
        self,
        operation: str,
        secret_name: str,
        success: bool,
        tenant_id: Optional[str] = None,
        error: Optional[str] = None
    ):
        """Registra acesso para auditoria"""
        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "operation": operation,
            "secret_name": secret_name,
            "success": success,
            "tenant_id": tenant_id,
            "error": error
        }
        self._audit_log.append(log_entry)

        # Mantem apenas os ultimos 1000 registros em memoria
        if len(self._audit_log) > 1000:
            self._audit_log = self._audit_log[-1000:]

        if success:
            logger.info(f"KeyVault: {operation} {secret_name}")
        else:
            logger.warning(f"KeyVault: {operation} {secret_name} falhou: {error}")

    def _get_secret_key(self, name: str, tenant_id: Optional[str] = None) -> str:
        """
        Gera chave do secret com isolamento de tenant.

        Args:
            name: Nome do secret
            tenant_id: ID do tenant para isolamento

        Returns:
            Nome do secret no Key Vault
        """
        if tenant_id:
            # Formato: tenant-{tenant_id}-{name}
            return f"tenant-{tenant_id}-{name}"
        return name

    # =========================================================================
    # Operacoes CRUD
    # =========================================================================

    async def get_secret(
        self,
        name: str,
        tenant_id: Optional[str] = None,
        use_cache: bool = True
    ) -> Optional[str]:
        """
        Recupera um secret do Key Vault.

        Args:
            name: Nome do secret
            tenant_id: ID do tenant para isolamento
            use_cache: Usar cache local

        Returns:
            Valor do secret ou None se nao encontrado
        """
        secret_key = self._get_secret_key(name, tenant_id)

        # Verifica cache
        if use_cache and secret_key in self._cache:
            cached = self._cache[secret_key]
            if not cached.is_expired:
                self._log_access("GET_CACHED", secret_key, True, tenant_id)
                return cached.value
            else:
                del self._cache[secret_key]

        # Busca no Key Vault
        try:
            client = self._get_client()
            secret = client.get_secret(secret_key)

            # Adiciona ao cache
            if use_cache:
                self._cache[secret_key] = CachedSecret(
                    value=secret.value,
                    cached_at=datetime.utcnow(),
                    expires_at=datetime.utcnow() + timedelta(
                        seconds=self.config.cache_ttl_seconds
                    ),
                    metadata={
                        "id": secret.id,
                        "content_type": secret.properties.content_type,
                        "enabled": secret.properties.enabled
                    }
                )

            self._log_access("GET", secret_key, True, tenant_id)
            return secret.value

        except ResourceNotFoundError:
            self._log_access("GET", secret_key, False, tenant_id, "Not found")
            return None
        except HttpResponseError as e:
            self._log_access("GET", secret_key, False, tenant_id, str(e))
            return None
        except Exception as e:
            self._log_access("GET", secret_key, False, tenant_id, str(e))
            logger.error(f"Erro ao recuperar secret: {e}")
            return None

    async def set_secret(
        self,
        name: str,
        value: str,
        tenant_id: Optional[str] = None,
        content_type: Optional[str] = None,
        secret_type: SecretType = SecretType.API_KEY,
        expires_on: Optional[datetime] = None,
        tags: Optional[Dict[str, str]] = None
    ) -> bool:
        """
        Salva um secret no Key Vault.

        Args:
            name: Nome do secret
            value: Valor do secret
            tenant_id: ID do tenant para isolamento
            content_type: Tipo de conteudo
            secret_type: Tipo de secret
            expires_on: Data de expiracao
            tags: Tags adicionais

        Returns:
            True se salvo com sucesso
        """
        secret_key = self._get_secret_key(name, tenant_id)

        try:
            client = self._get_client()

            # Prepara tags com metadados
            secret_tags = tags or {}
            secret_tags["secret_type"] = secret_type.value
            if tenant_id:
                secret_tags["tenant_id"] = tenant_id
            secret_tags["created_by"] = "fabrica-de-agentes"
            secret_tags["created_at"] = datetime.utcnow().isoformat()

            # Salva o secret
            secret = client.set_secret(
                name=secret_key,
                value=value,
                content_type=content_type or secret_type.value,
                expires_on=expires_on,
                tags=secret_tags
            )

            # Atualiza cache
            self._cache[secret_key] = CachedSecret(
                value=value,
                cached_at=datetime.utcnow(),
                expires_at=datetime.utcnow() + timedelta(
                    seconds=self.config.cache_ttl_seconds
                ),
                metadata={"id": secret.id}
            )

            self._log_access("SET", secret_key, True, tenant_id)
            return True

        except HttpResponseError as e:
            self._log_access("SET", secret_key, False, tenant_id, str(e))
            return False
        except Exception as e:
            self._log_access("SET", secret_key, False, tenant_id, str(e))
            logger.error(f"Erro ao salvar secret: {e}")
            return False

    async def delete_secret(
        self,
        name: str,
        tenant_id: Optional[str] = None
    ) -> bool:
        """
        Remove um secret do Key Vault.

        Args:
            name: Nome do secret
            tenant_id: ID do tenant para isolamento

        Returns:
            True se removido com sucesso
        """
        secret_key = self._get_secret_key(name, tenant_id)

        try:
            client = self._get_client()

            # Inicia soft-delete
            poller = client.begin_delete_secret(secret_key)
            poller.wait()

            # Remove do cache
            if secret_key in self._cache:
                del self._cache[secret_key]

            self._log_access("DELETE", secret_key, True, tenant_id)
            return True

        except ResourceNotFoundError:
            self._log_access("DELETE", secret_key, False, tenant_id, "Not found")
            return False
        except HttpResponseError as e:
            self._log_access("DELETE", secret_key, False, tenant_id, str(e))
            return False
        except Exception as e:
            self._log_access("DELETE", secret_key, False, tenant_id, str(e))
            logger.error(f"Erro ao deletar secret: {e}")
            return False

    async def list_secrets(
        self,
        tenant_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Lista secrets do Key Vault.

        Args:
            tenant_id: Filtrar por tenant

        Returns:
            Lista de secrets (apenas metadados, nao valores)
        """
        try:
            client = self._get_client()
            secrets = []

            for secret_props in client.list_properties_of_secrets():
                # Filtra por tenant se especificado
                if tenant_id:
                    secret_tenant = secret_props.tags.get("tenant_id") if secret_props.tags else None
                    if secret_tenant != tenant_id:
                        continue

                secrets.append({
                    "name": secret_props.name,
                    "enabled": secret_props.enabled,
                    "created_on": secret_props.created_on.isoformat() if secret_props.created_on else None,
                    "updated_on": secret_props.updated_on.isoformat() if secret_props.updated_on else None,
                    "expires_on": secret_props.expires_on.isoformat() if secret_props.expires_on else None,
                    "content_type": secret_props.content_type,
                    "tags": secret_props.tags or {}
                })

            self._log_access("LIST", f"tenant:{tenant_id}", True, tenant_id)
            return secrets

        except Exception as e:
            self._log_access("LIST", f"tenant:{tenant_id}", False, tenant_id, str(e))
            logger.error(f"Erro ao listar secrets: {e}")
            return []

    # =========================================================================
    # Operacoes em Lote
    # =========================================================================

    async def get_secrets_batch(
        self,
        names: List[str],
        tenant_id: Optional[str] = None
    ) -> Dict[str, Optional[str]]:
        """
        Recupera multiplos secrets.

        Args:
            names: Lista de nomes
            tenant_id: ID do tenant

        Returns:
            Dict com nome -> valor
        """
        results = {}
        for name in names:
            results[name] = await self.get_secret(name, tenant_id)
        return results

    async def set_secrets_batch(
        self,
        secrets: Dict[str, str],
        tenant_id: Optional[str] = None,
        secret_type: SecretType = SecretType.API_KEY
    ) -> Dict[str, bool]:
        """
        Salva multiplos secrets.

        Args:
            secrets: Dict com nome -> valor
            tenant_id: ID do tenant
            secret_type: Tipo dos secrets

        Returns:
            Dict com nome -> sucesso
        """
        results = {}
        for name, value in secrets.items():
            results[name] = await self.set_secret(
                name, value, tenant_id, secret_type=secret_type
            )
        return results

    # =========================================================================
    # Rotacao de Secrets
    # =========================================================================

    async def rotate_secret(
        self,
        name: str,
        new_value: str,
        tenant_id: Optional[str] = None
    ) -> bool:
        """
        Rotaciona um secret (cria nova versao).

        Args:
            name: Nome do secret
            new_value: Novo valor
            tenant_id: ID do tenant

        Returns:
            True se rotacionado com sucesso
        """
        # Key Vault automaticamente cria nova versao ao atualizar
        return await self.set_secret(name, new_value, tenant_id)

    # =========================================================================
    # Cache e Auditoria
    # =========================================================================

    def clear_cache(self, tenant_id: Optional[str] = None):
        """
        Limpa o cache de secrets.

        Args:
            tenant_id: Limpar apenas secrets deste tenant
        """
        if tenant_id:
            prefix = f"tenant-{tenant_id}-"
            keys_to_remove = [k for k in self._cache if k.startswith(prefix)]
            for key in keys_to_remove:
                del self._cache[key]
        else:
            self._cache.clear()

        logger.info(f"Cache limpo para tenant: {tenant_id or 'todos'}")

    def get_audit_log(
        self,
        tenant_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retorna log de auditoria.

        Args:
            tenant_id: Filtrar por tenant
            limit: Limite de registros

        Returns:
            Lista de registros de auditoria
        """
        logs = self._audit_log
        if tenant_id:
            logs = [l for l in logs if l.get("tenant_id") == tenant_id]
        return logs[-limit:]

    def get_stats(self) -> Dict[str, Any]:
        """
        Retorna estatisticas do cliente.

        Returns:
            Estatisticas de cache e operacoes
        """
        return {
            "cache_size": len(self._cache),
            "audit_log_size": len(self._audit_log),
            "vault_url": self.config.vault_url,
            "using_managed_identity": self.config.use_managed_identity,
            "cache_ttl_seconds": self.config.cache_ttl_seconds
        }

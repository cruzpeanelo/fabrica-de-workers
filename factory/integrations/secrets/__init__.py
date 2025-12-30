# -*- coding: utf-8 -*-
"""
Secrets Management Module
=========================
Gerenciamento seguro de credenciais com Azure Key Vault e fallback local.

Terminal 5 - Issue #299
"""

from .secrets_manager import SecretsManager, SecretValue
from .azure_keyvault import AzureKeyVaultClient, AzureKeyVaultConfig
from .local_encryption import LocalEncryption, LocalEncryptionConfig

__all__ = [
    "SecretsManager",
    "SecretValue",
    "AzureKeyVaultClient",
    "AzureKeyVaultConfig",
    "LocalEncryption",
    "LocalEncryptionConfig",
]

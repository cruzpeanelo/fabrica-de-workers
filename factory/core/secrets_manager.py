"""
Secrets Manager - Field-Level Encryption and Secrets Handling
Issue #200: Data encryption at rest

Provides:
- Field-level encryption for sensitive database columns
- Integration with HashiCorp Vault (optional)
- Key rotation support
- Encrypted field types for SQLAlchemy
"""
import os
import base64
import hashlib
import logging
from typing import Optional, Any, Union
from datetime import datetime, timedelta
from functools import lru_cache

logger = logging.getLogger(__name__)


# Try to import cryptography, provide fallback
try:
    from cryptography.fernet import Fernet, InvalidToken
    from cryptography.hazmat.primitives import hashes
    from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
    CRYPTO_AVAILABLE = True
except ImportError:
    CRYPTO_AVAILABLE = False
    logger.warning("[SecretsManager] cryptography not installed. Install with: pip install cryptography")


class EncryptionConfig:
    """Configuration for encryption"""
    # Environment variable for master key
    MASTER_KEY_ENV = "ENCRYPTION_MASTER_KEY"

    # Salt for key derivation (should be unique per deployment)
    SALT_ENV = "ENCRYPTION_SALT"

    # Key rotation interval (days)
    KEY_ROTATION_DAYS = 90

    # Vault settings
    VAULT_ADDR_ENV = "VAULT_ADDR"
    VAULT_TOKEN_ENV = "VAULT_TOKEN"
    VAULT_PATH_PREFIX = "secret/fabrica"


class SecretsManager:
    """
    Manages secrets and encryption for sensitive data.

    Features:
    - Field-level encryption using Fernet (AES-128-CBC)
    - Integration with HashiCorp Vault
    - Key derivation from master password
    - Key rotation support
    """

    def __init__(self, master_key: Optional[str] = None):
        """
        Initialize secrets manager.

        Args:
            master_key: Optional master key. If not provided, reads from env.
        """
        self._master_key = master_key or os.getenv(EncryptionConfig.MASTER_KEY_ENV)
        self._salt = os.getenv(EncryptionConfig.SALT_ENV, "fabrica-default-salt")
        self._fernet: Optional[Fernet] = None
        self._vault_client = None
        self._key_cache = {}

        if CRYPTO_AVAILABLE and self._master_key:
            self._init_fernet()

    def _init_fernet(self):
        """Initialize Fernet cipher from master key"""
        if not CRYPTO_AVAILABLE:
            logger.error("[SecretsManager] cryptography not available")
            return

        try:
            # Derive a Fernet key from master key using PBKDF2
            kdf = PBKDF2HMAC(
                algorithm=hashes.SHA256(),
                length=32,
                salt=self._salt.encode(),
                iterations=100000,
            )
            key = base64.urlsafe_b64encode(kdf.derive(self._master_key.encode()))
            self._fernet = Fernet(key)
            logger.info("[SecretsManager] Encryption initialized")
        except Exception as e:
            logger.error(f"[SecretsManager] Failed to initialize: {e}")

    def encrypt(self, plaintext: str) -> str:
        """
        Encrypt a string value.

        Args:
            plaintext: String to encrypt

        Returns:
            Encrypted string (base64 encoded)
        """
        if not plaintext:
            return ""

        if not self._fernet:
            logger.warning("[SecretsManager] Encryption not initialized, storing as-is")
            return f"UNENCRYPTED:{plaintext}"

        try:
            encrypted = self._fernet.encrypt(plaintext.encode('utf-8'))
            return f"ENC:{encrypted.decode('utf-8')}"
        except Exception as e:
            logger.error(f"[SecretsManager] Encryption failed: {e}")
            return f"UNENCRYPTED:{plaintext}"

    def decrypt(self, ciphertext: str) -> str:
        """
        Decrypt an encrypted string value.

        Args:
            ciphertext: Encrypted string

        Returns:
            Decrypted string
        """
        if not ciphertext:
            return ""

        # Handle unencrypted fallback
        if ciphertext.startswith("UNENCRYPTED:"):
            return ciphertext[12:]

        # Handle encrypted data
        if ciphertext.startswith("ENC:"):
            if not self._fernet:
                logger.error("[SecretsManager] Cannot decrypt - encryption not initialized")
                return ""

            try:
                encrypted_data = ciphertext[4:].encode('utf-8')
                decrypted = self._fernet.decrypt(encrypted_data)
                return decrypted.decode('utf-8')
            except InvalidToken:
                logger.error("[SecretsManager] Decryption failed - invalid token or wrong key")
                return ""
            except Exception as e:
                logger.error(f"[SecretsManager] Decryption failed: {e}")
                return ""

        # Legacy unencrypted data - return as-is
        logger.warning("[SecretsManager] Found legacy unencrypted data")
        return ciphertext

    def is_encrypted(self, value: str) -> bool:
        """Check if a value is encrypted"""
        return value.startswith("ENC:") if value else False

    def encrypt_dict(self, data: dict, fields: list) -> dict:
        """
        Encrypt specific fields in a dictionary.

        Args:
            data: Dictionary with data
            fields: List of field names to encrypt

        Returns:
            Dictionary with encrypted fields
        """
        result = data.copy()
        for field in fields:
            if field in result and result[field]:
                result[field] = self.encrypt(str(result[field]))
        return result

    def decrypt_dict(self, data: dict, fields: list) -> dict:
        """
        Decrypt specific fields in a dictionary.

        Args:
            data: Dictionary with encrypted data
            fields: List of field names to decrypt

        Returns:
            Dictionary with decrypted fields
        """
        result = data.copy()
        for field in fields:
            if field in result and result[field]:
                result[field] = self.decrypt(str(result[field]))
        return result

    # =========================================================================
    # Vault Integration
    # =========================================================================

    def _get_vault_client(self):
        """Get or create Vault client"""
        if self._vault_client is not None:
            return self._vault_client

        vault_addr = os.getenv(EncryptionConfig.VAULT_ADDR_ENV)
        vault_token = os.getenv(EncryptionConfig.VAULT_TOKEN_ENV)

        if not vault_addr or not vault_token:
            return None

        try:
            import hvac
            self._vault_client = hvac.Client(url=vault_addr, token=vault_token)
            if not self._vault_client.is_authenticated():
                logger.error("[SecretsManager] Vault authentication failed")
                self._vault_client = None
        except ImportError:
            logger.warning("[SecretsManager] hvac not installed for Vault support")
            self._vault_client = None
        except Exception as e:
            logger.error(f"[SecretsManager] Vault connection failed: {e}")
            self._vault_client = None

        return self._vault_client

    def get_secret_from_vault(self, path: str, key: str = "value") -> Optional[str]:
        """
        Get a secret from HashiCorp Vault.

        Args:
            path: Secret path (relative to VAULT_PATH_PREFIX)
            key: Key within the secret (default: "value")

        Returns:
            Secret value or None
        """
        client = self._get_vault_client()
        if not client:
            return None

        try:
            full_path = f"{EncryptionConfig.VAULT_PATH_PREFIX}/{path}"
            secret = client.secrets.kv.v2.read_secret_version(path=full_path)
            return secret['data']['data'].get(key)
        except Exception as e:
            logger.error(f"[SecretsManager] Vault read failed: {e}")
            return None

    def store_secret_in_vault(self, path: str, data: dict) -> bool:
        """
        Store a secret in HashiCorp Vault.

        Args:
            path: Secret path (relative to VAULT_PATH_PREFIX)
            data: Secret data dictionary

        Returns:
            True if successful
        """
        client = self._get_vault_client()
        if not client:
            return False

        try:
            full_path = f"{EncryptionConfig.VAULT_PATH_PREFIX}/{path}"
            client.secrets.kv.v2.create_or_update_secret(
                path=full_path,
                secret=data
            )
            return True
        except Exception as e:
            logger.error(f"[SecretsManager] Vault write failed: {e}")
            return False

    # =========================================================================
    # Key Rotation
    # =========================================================================

    def rotate_key(self, new_master_key: str) -> bool:
        """
        Rotate the encryption key.

        This requires re-encrypting all data with the new key.

        Args:
            new_master_key: New master key

        Returns:
            True if rotation successful
        """
        if not self._fernet:
            logger.error("[SecretsManager] Cannot rotate - encryption not initialized")
            return False

        # Store old fernet
        old_fernet = self._fernet

        # Initialize new fernet
        old_master = self._master_key
        self._master_key = new_master_key
        self._init_fernet()

        if not self._fernet:
            # Rollback
            self._master_key = old_master
            self._fernet = old_fernet
            return False

        logger.info("[SecretsManager] Key rotation successful - data migration required")
        return True

    def migrate_encrypted_value(self, old_ciphertext: str, old_key: str) -> str:
        """
        Migrate a value encrypted with old key to new key.

        Args:
            old_ciphertext: Value encrypted with old key
            old_key: Old master key

        Returns:
            Value encrypted with current key
        """
        # Create temporary manager with old key
        old_manager = SecretsManager(master_key=old_key)

        # Decrypt with old key
        plaintext = old_manager.decrypt(old_ciphertext)

        # Encrypt with current key
        return self.encrypt(plaintext)


# Singleton instance
_secrets_manager: Optional[SecretsManager] = None


def get_secrets_manager() -> SecretsManager:
    """Get or create secrets manager singleton"""
    global _secrets_manager
    if _secrets_manager is None:
        _secrets_manager = SecretsManager()
    return _secrets_manager


# =========================================================================
# SQLAlchemy Type Decorators
# =========================================================================

from sqlalchemy import TypeDecorator, String, Text


class EncryptedString(TypeDecorator):
    """
    SQLAlchemy type for encrypted string columns.

    Usage:
        class User(Base):
            api_key = Column(EncryptedString(500))
    """
    impl = String
    cache_ok = True

    def __init__(self, length=255, **kwargs):
        # Encrypted data is longer than plaintext
        super().__init__(length=length * 2, **kwargs)

    def process_bind_param(self, value, dialect) -> Optional[str]:
        """Encrypt value before storing"""
        if value is None:
            return None
        manager = get_secrets_manager()
        return manager.encrypt(str(value))

    def process_result_value(self, value, dialect) -> Optional[str]:
        """Decrypt value when loading"""
        if value is None:
            return None
        manager = get_secrets_manager()
        return manager.decrypt(value)


class EncryptedText(TypeDecorator):
    """
    SQLAlchemy type for encrypted text columns (no length limit).

    Usage:
        class Integration(Base):
            oauth_token = Column(EncryptedText())
    """
    impl = Text
    cache_ok = True

    def process_bind_param(self, value, dialect) -> Optional[str]:
        """Encrypt value before storing"""
        if value is None:
            return None
        manager = get_secrets_manager()
        return manager.encrypt(str(value))

    def process_result_value(self, value, dialect) -> Optional[str]:
        """Decrypt value when loading"""
        if value is None:
            return None
        manager = get_secrets_manager()
        return manager.decrypt(value)


class EncryptedJSON(TypeDecorator):
    """
    SQLAlchemy type for encrypted JSON columns.

    Usage:
        class Config(Base):
            sensitive_config = Column(EncryptedJSON())
    """
    impl = Text
    cache_ok = True

    def process_bind_param(self, value, dialect) -> Optional[str]:
        """Encrypt JSON value before storing"""
        if value is None:
            return None
        import json
        json_str = json.dumps(value)
        manager = get_secrets_manager()
        return manager.encrypt(json_str)

    def process_result_value(self, value, dialect) -> Optional[dict]:
        """Decrypt and parse JSON when loading"""
        if value is None:
            return None
        import json
        manager = get_secrets_manager()
        decrypted = manager.decrypt(value)
        try:
            return json.loads(decrypted)
        except json.JSONDecodeError:
            logger.error("[EncryptedJSON] Failed to parse decrypted JSON")
            return None


# =========================================================================
# Migration Utilities
# =========================================================================

async def migrate_sensitive_fields(
    session,
    model,
    fields: list,
    batch_size: int = 100
):
    """
    Migrate existing unencrypted data to encrypted format.

    Args:
        session: Database session
        model: SQLAlchemy model class
        fields: List of field names to encrypt
        batch_size: Number of records per batch
    """
    from sqlalchemy import select

    manager = get_secrets_manager()
    if not manager._fernet:
        logger.error("[Migration] Encryption not initialized")
        return

    offset = 0
    total_migrated = 0

    while True:
        # Fetch batch
        stmt = select(model).offset(offset).limit(batch_size)
        result = await session.execute(stmt)
        records = result.scalars().all()

        if not records:
            break

        for record in records:
            needs_update = False

            for field in fields:
                value = getattr(record, field, None)
                if value and not manager.is_encrypted(value):
                    setattr(record, field, manager.encrypt(value))
                    needs_update = True

            if needs_update:
                total_migrated += 1

        await session.commit()
        offset += batch_size
        logger.info(f"[Migration] Processed {offset} records, migrated {total_migrated}")

    logger.info(f"[Migration] Complete. Total migrated: {total_migrated}")


def generate_encryption_key() -> str:
    """
    Generate a new encryption master key.

    Returns:
        Base64-encoded key suitable for ENCRYPTION_MASTER_KEY
    """
    import secrets
    return base64.urlsafe_b64encode(secrets.token_bytes(32)).decode()


def hash_sensitive_data(value: str, salt: Optional[str] = None) -> str:
    """
    Create a one-way hash of sensitive data (for lookup without storing plaintext).

    Args:
        value: Value to hash
        salt: Optional salt (uses default if not provided)

    Returns:
        Hex-encoded SHA-256 hash
    """
    if not salt:
        salt = os.getenv(EncryptionConfig.SALT_ENV, "")

    return hashlib.sha256(f"{salt}{value}".encode()).hexdigest()

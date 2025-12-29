"""
Vault Client for External Secrets Management - Issue #94
HashiCorp Vault integration with automatic secret rotation and fallback to env vars.

Features:
- Kubernetes authentication support
- Token-based authentication support
- Automatic secret rotation with configurable TTL
- In-memory caching with expiration
- Fallback to environment variables in development
- Health checks and connection validation
- Async support for non-blocking operations
"""

import os
import json
import time
import logging
import asyncio
import threading
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from functools import wraps
from typing import Any, Callable, Dict, List, Optional, TypeVar, Union
from pathlib import Path

# Optional imports with fallbacks
try:
    import hvac
    from hvac.exceptions import (
        InvalidPath,
        Forbidden,
        VaultError,
        InvalidRequest,
    )
    HVAC_AVAILABLE = True
except ImportError:
    HVAC_AVAILABLE = False
    hvac = None

try:
    import aiofiles
    AIOFILES_AVAILABLE = True
except ImportError:
    AIOFILES_AVAILABLE = False

logger = logging.getLogger(__name__)


class VaultAuthMethod(Enum):
    """Supported Vault authentication methods."""
    TOKEN = "token"
    KUBERNETES = "kubernetes"
    APPROLE = "approle"
    AWS_IAM = "aws_iam"
    LDAP = "ldap"


class SecretEngine(Enum):
    """Supported Vault secret engines."""
    KV_V1 = "kv-v1"
    KV_V2 = "kv-v2"
    DATABASE = "database"
    AWS = "aws"
    PKI = "pki"


@dataclass
class CachedSecret:
    """Cached secret with expiration tracking."""
    value: Dict[str, Any]
    created_at: datetime
    expires_at: datetime
    lease_id: Optional[str] = None
    renewable: bool = False

    def is_expired(self) -> bool:
        """Check if the cached secret has expired."""
        return datetime.utcnow() >= self.expires_at

    def time_until_expiry(self) -> timedelta:
        """Get time remaining until expiration."""
        return self.expires_at - datetime.utcnow()


@dataclass
class VaultConfig:
    """Configuration for Vault client."""
    # Connection settings
    vault_addr: str = field(default_factory=lambda: os.getenv("VAULT_ADDR", "http://localhost:8200"))
    vault_namespace: Optional[str] = field(default_factory=lambda: os.getenv("VAULT_NAMESPACE"))
    verify_ssl: bool = field(default_factory=lambda: os.getenv("VAULT_SKIP_VERIFY", "false").lower() != "true")
    timeout: int = 30

    # Authentication
    auth_method: VaultAuthMethod = field(default_factory=lambda: VaultAuthMethod(
        os.getenv("VAULT_AUTH_METHOD", "token")
    ))
    token: Optional[str] = field(default_factory=lambda: os.getenv("VAULT_TOKEN"))
    role_id: Optional[str] = field(default_factory=lambda: os.getenv("VAULT_ROLE_ID"))
    secret_id: Optional[str] = field(default_factory=lambda: os.getenv("VAULT_SECRET_ID"))
    kubernetes_role: Optional[str] = field(default_factory=lambda: os.getenv("VAULT_K8S_ROLE", "factory"))
    kubernetes_jwt_path: str = "/var/run/secrets/kubernetes.io/serviceaccount/token"

    # Secret engine
    secret_engine: SecretEngine = field(default_factory=lambda: SecretEngine(
        os.getenv("VAULT_SECRET_ENGINE", "kv-v2")
    ))
    secret_mount_point: str = field(default_factory=lambda: os.getenv("VAULT_MOUNT_POINT", "secret"))
    secret_base_path: str = field(default_factory=lambda: os.getenv("VAULT_SECRET_PATH", "factory"))

    # Caching and rotation
    cache_ttl_seconds: int = 300  # 5 minutes default cache
    auto_rotate: bool = True
    rotation_buffer_seconds: int = 60  # Rotate 60s before expiry
    max_retries: int = 3
    retry_delay_seconds: float = 1.0

    # Fallback
    fallback_to_env: bool = field(default_factory=lambda: os.getenv("VAULT_FALLBACK_TO_ENV", "true").lower() == "true")
    env_prefix: str = "FACTORY_"

    # Development mode
    dev_mode: bool = field(default_factory=lambda: os.getenv("VAULT_DEV_MODE", "false").lower() == "true")


class SecretCache:
    """Thread-safe secret cache with automatic expiration."""

    def __init__(self, default_ttl: int = 300):
        self._cache: Dict[str, CachedSecret] = {}
        self._lock = threading.RLock()
        self._default_ttl = default_ttl

    def get(self, key: str) -> Optional[Dict[str, Any]]:
        """Get a cached secret if not expired."""
        with self._lock:
            cached = self._cache.get(key)
            if cached and not cached.is_expired():
                return cached.value
            elif cached:
                # Remove expired entry
                del self._cache[key]
            return None

    def set(
        self,
        key: str,
        value: Dict[str, Any],
        ttl: Optional[int] = None,
        lease_id: Optional[str] = None,
        renewable: bool = False
    ) -> None:
        """Cache a secret with optional TTL."""
        ttl = ttl or self._default_ttl
        with self._lock:
            self._cache[key] = CachedSecret(
                value=value,
                created_at=datetime.utcnow(),
                expires_at=datetime.utcnow() + timedelta(seconds=ttl),
                lease_id=lease_id,
                renewable=renewable
            )

    def delete(self, key: str) -> None:
        """Remove a cached secret."""
        with self._lock:
            self._cache.pop(key, None)

    def clear(self) -> None:
        """Clear all cached secrets."""
        with self._lock:
            self._cache.clear()

    def get_all_lease_ids(self) -> List[str]:
        """Get all active lease IDs for renewal."""
        with self._lock:
            return [
                cached.lease_id
                for cached in self._cache.values()
                if cached.lease_id and not cached.is_expired()
            ]

    def get_expiring_soon(self, buffer_seconds: int = 60) -> List[str]:
        """Get keys of secrets expiring soon."""
        threshold = datetime.utcnow() + timedelta(seconds=buffer_seconds)
        with self._lock:
            return [
                key for key, cached in self._cache.items()
                if cached.expires_at <= threshold and not cached.is_expired()
            ]


class VaultClientBase(ABC):
    """Abstract base class for Vault clients."""

    @abstractmethod
    def get_secret(self, path: str, key: Optional[str] = None) -> Optional[Union[str, Dict[str, Any]]]:
        """Get a secret from Vault."""
        pass

    @abstractmethod
    def set_secret(self, path: str, data: Dict[str, Any]) -> bool:
        """Set a secret in Vault."""
        pass

    @abstractmethod
    def delete_secret(self, path: str) -> bool:
        """Delete a secret from Vault."""
        pass

    @abstractmethod
    def is_healthy(self) -> bool:
        """Check if Vault is healthy and accessible."""
        pass


class EnvFallbackClient(VaultClientBase):
    """Fallback client that reads secrets from environment variables."""

    def __init__(self, prefix: str = "FACTORY_"):
        self.prefix = prefix
        logger.info("Using environment variable fallback for secrets")

    def get_secret(self, path: str, key: Optional[str] = None) -> Optional[Union[str, Dict[str, Any]]]:
        """Get a secret from environment variables."""
        # Convert path to env var name: factory/database -> FACTORY_DATABASE
        env_key = self.prefix + path.replace("/", "_").upper()

        if key:
            env_key = f"{env_key}_{key.upper()}"
            value = os.getenv(env_key)
            logger.debug(f"Reading env var: {env_key}")
            return value

        # Try to get all related env vars
        result = {}
        for env_name, env_value in os.environ.items():
            if env_name.startswith(env_key + "_"):
                sub_key = env_name[len(env_key) + 1:].lower()
                result[sub_key] = env_value

        if result:
            return result

        # Single value
        value = os.getenv(env_key)
        if value:
            try:
                return json.loads(value)
            except json.JSONDecodeError:
                return {"value": value}
        return None

    def set_secret(self, path: str, data: Dict[str, Any]) -> bool:
        """Cannot set environment variables at runtime."""
        logger.warning("Cannot set secrets in environment fallback mode")
        return False

    def delete_secret(self, path: str) -> bool:
        """Cannot delete environment variables at runtime."""
        logger.warning("Cannot delete secrets in environment fallback mode")
        return False

    def is_healthy(self) -> bool:
        """Environment fallback is always healthy."""
        return True


class VaultClient(VaultClientBase):
    """
    HashiCorp Vault client with automatic rotation and caching.

    Usage:
        # Initialize client
        client = VaultClient()

        # Get a secret
        db_password = client.get_secret("database/credentials", "password")

        # Get all secrets at a path
        db_creds = client.get_secret("database/credentials")

        # Set a secret
        client.set_secret("api/keys", {"api_key": "xxx", "api_secret": "yyy"})

        # Health check
        if client.is_healthy():
            print("Vault is accessible")
    """

    _instance: Optional["VaultClient"] = None
    _lock = threading.Lock()

    def __new__(cls, config: Optional[VaultConfig] = None):
        """Singleton pattern for Vault client."""
        with cls._lock:
            if cls._instance is None:
                cls._instance = super().__new__(cls)
                cls._instance._initialized = False
            return cls._instance

    def __init__(self, config: Optional[VaultConfig] = None):
        """Initialize Vault client."""
        if self._initialized:
            return

        self.config = config or VaultConfig()
        self._cache = SecretCache(self.config.cache_ttl_seconds)
        self._client: Optional[Any] = None
        self._fallback: Optional[EnvFallbackClient] = None
        self._rotation_task: Optional[asyncio.Task] = None
        self._stop_rotation = threading.Event()

        # Initialize based on mode
        if self.config.dev_mode or not HVAC_AVAILABLE:
            self._setup_fallback()
        else:
            self._setup_vault_client()

        self._initialized = True

    def _setup_fallback(self) -> None:
        """Setup environment variable fallback."""
        if not HVAC_AVAILABLE:
            logger.warning("hvac library not installed, using environment fallback")
        elif self.config.dev_mode:
            logger.info("Development mode enabled, using environment fallback")

        self._fallback = EnvFallbackClient(self.config.env_prefix)

    def _setup_vault_client(self) -> None:
        """Setup HashiCorp Vault client."""
        try:
            self._client = hvac.Client(
                url=self.config.vault_addr,
                namespace=self.config.vault_namespace,
                verify=self.config.verify_ssl,
                timeout=self.config.timeout,
            )

            self._authenticate()

            if self._client.is_authenticated():
                logger.info(f"Successfully connected to Vault at {self.config.vault_addr}")

                # Start rotation task if enabled
                if self.config.auto_rotate:
                    self._start_rotation_thread()
            else:
                logger.warning("Vault authentication failed, falling back to env vars")
                if self.config.fallback_to_env:
                    self._setup_fallback()

        except Exception as e:
            logger.error(f"Failed to connect to Vault: {e}")
            if self.config.fallback_to_env:
                self._setup_fallback()
            else:
                raise

    def _authenticate(self) -> None:
        """Authenticate with Vault based on configured method."""
        if self.config.auth_method == VaultAuthMethod.TOKEN:
            self._auth_token()
        elif self.config.auth_method == VaultAuthMethod.KUBERNETES:
            self._auth_kubernetes()
        elif self.config.auth_method == VaultAuthMethod.APPROLE:
            self._auth_approle()
        else:
            raise ValueError(f"Unsupported auth method: {self.config.auth_method}")

    def _auth_token(self) -> None:
        """Authenticate using static token."""
        if not self.config.token:
            raise ValueError("VAULT_TOKEN not set for token authentication")
        self._client.token = self.config.token

    def _auth_kubernetes(self) -> None:
        """Authenticate using Kubernetes service account."""
        jwt_path = Path(self.config.kubernetes_jwt_path)

        if not jwt_path.exists():
            raise FileNotFoundError(
                f"Kubernetes JWT not found at {jwt_path}. "
                "Are you running in a Kubernetes pod?"
            )

        jwt = jwt_path.read_text().strip()

        response = self._client.auth.kubernetes.login(
            role=self.config.kubernetes_role,
            jwt=jwt,
            mount_point="kubernetes",
        )

        self._client.token = response["auth"]["client_token"]
        logger.info(f"Authenticated with Kubernetes role: {self.config.kubernetes_role}")

    def _auth_approle(self) -> None:
        """Authenticate using AppRole."""
        if not self.config.role_id or not self.config.secret_id:
            raise ValueError("VAULT_ROLE_ID and VAULT_SECRET_ID required for AppRole auth")

        response = self._client.auth.approle.login(
            role_id=self.config.role_id,
            secret_id=self.config.secret_id,
        )

        self._client.token = response["auth"]["client_token"]
        logger.info("Authenticated with AppRole")

    def _start_rotation_thread(self) -> None:
        """Start background thread for secret rotation."""
        def rotation_loop():
            while not self._stop_rotation.is_set():
                try:
                    self._rotate_expiring_secrets()
                    self._renew_leases()
                except Exception as e:
                    logger.error(f"Error in rotation loop: {e}")

                # Wait before next check
                self._stop_rotation.wait(self.config.rotation_buffer_seconds)

        thread = threading.Thread(target=rotation_loop, daemon=True, name="vault-rotation")
        thread.start()
        logger.info("Started secret rotation thread")

    def _rotate_expiring_secrets(self) -> None:
        """Rotate secrets that are about to expire."""
        expiring_keys = self._cache.get_expiring_soon(self.config.rotation_buffer_seconds)

        for key in expiring_keys:
            try:
                # Re-fetch the secret
                self._cache.delete(key)
                self._read_secret_from_vault(key)
                logger.info(f"Rotated expiring secret: {key}")
            except Exception as e:
                logger.error(f"Failed to rotate secret {key}: {e}")

    def _renew_leases(self) -> None:
        """Renew active leases."""
        if not self._client:
            return

        lease_ids = self._cache.get_all_lease_ids()
        for lease_id in lease_ids:
            try:
                self._client.sys.renew_lease(lease_id)
                logger.debug(f"Renewed lease: {lease_id}")
            except Exception as e:
                logger.warning(f"Failed to renew lease {lease_id}: {e}")

    def _get_full_path(self, path: str) -> str:
        """Get the full path including base path."""
        if path.startswith("/"):
            path = path[1:]
        return f"{self.config.secret_base_path}/{path}"

    def _read_secret_from_vault(self, path: str) -> Optional[Dict[str, Any]]:
        """Read a secret directly from Vault."""
        if not self._client:
            return None

        full_path = self._get_full_path(path)

        for attempt in range(self.config.max_retries):
            try:
                if self.config.secret_engine == SecretEngine.KV_V2:
                    response = self._client.secrets.kv.v2.read_secret_version(
                        path=full_path,
                        mount_point=self.config.secret_mount_point,
                    )
                    data = response["data"]["data"]
                    metadata = response["data"]["metadata"]

                    # Cache with version info
                    self._cache.set(
                        path,
                        data,
                        ttl=self.config.cache_ttl_seconds,
                    )
                    return data

                elif self.config.secret_engine == SecretEngine.KV_V1:
                    response = self._client.secrets.kv.v1.read_secret(
                        path=full_path,
                        mount_point=self.config.secret_mount_point,
                    )
                    data = response["data"]
                    self._cache.set(path, data)
                    return data

                elif self.config.secret_engine == SecretEngine.DATABASE:
                    response = self._client.secrets.database.generate_credentials(
                        name=full_path,
                        mount_point=self.config.secret_mount_point,
                    )
                    data = response["data"]
                    lease_id = response.get("lease_id")
                    lease_duration = response.get("lease_duration", 3600)

                    self._cache.set(
                        path,
                        data,
                        ttl=lease_duration,
                        lease_id=lease_id,
                        renewable=response.get("renewable", False)
                    )
                    return data

            except InvalidPath:
                logger.warning(f"Secret not found at path: {full_path}")
                return None
            except Forbidden:
                logger.error(f"Access denied to secret: {full_path}")
                return None
            except Exception as e:
                logger.warning(f"Attempt {attempt + 1} failed reading {path}: {e}")
                if attempt < self.config.max_retries - 1:
                    time.sleep(self.config.retry_delay_seconds * (attempt + 1))
                else:
                    raise

        return None

    def get_secret(self, path: str, key: Optional[str] = None) -> Optional[Union[str, Dict[str, Any]]]:
        """
        Get a secret from Vault with caching.

        Args:
            path: Secret path (relative to base path)
            key: Optional specific key within the secret

        Returns:
            Secret value(s) or None if not found
        """
        # Use fallback if configured
        if self._fallback:
            return self._fallback.get_secret(path, key)

        # Check cache first
        cached = self._cache.get(path)
        if cached is not None:
            logger.debug(f"Cache hit for secret: {path}")
            if key:
                return cached.get(key)
            return cached

        # Read from Vault
        logger.debug(f"Cache miss, reading from Vault: {path}")
        data = self._read_secret_from_vault(path)

        if data is None:
            # Fallback to env if configured and Vault fails
            if self.config.fallback_to_env:
                logger.info(f"Falling back to env for: {path}")
                fallback = EnvFallbackClient(self.config.env_prefix)
                return fallback.get_secret(path, key)
            return None

        if key:
            return data.get(key)
        return data

    def set_secret(self, path: str, data: Dict[str, Any]) -> bool:
        """
        Set a secret in Vault.

        Args:
            path: Secret path (relative to base path)
            data: Secret data to store

        Returns:
            True if successful, False otherwise
        """
        if self._fallback:
            return self._fallback.set_secret(path, data)

        if not self._client:
            return False

        full_path = self._get_full_path(path)

        try:
            if self.config.secret_engine == SecretEngine.KV_V2:
                self._client.secrets.kv.v2.create_or_update_secret(
                    path=full_path,
                    secret=data,
                    mount_point=self.config.secret_mount_point,
                )
            elif self.config.secret_engine == SecretEngine.KV_V1:
                self._client.secrets.kv.v1.create_or_update_secret(
                    path=full_path,
                    secret=data,
                    mount_point=self.config.secret_mount_point,
                )
            else:
                logger.error(f"Cannot write to {self.config.secret_engine} engine")
                return False

            # Update cache
            self._cache.set(path, data)
            logger.info(f"Successfully set secret at: {path}")
            return True

        except Exception as e:
            logger.error(f"Failed to set secret at {path}: {e}")
            return False

    def delete_secret(self, path: str) -> bool:
        """
        Delete a secret from Vault.

        Args:
            path: Secret path (relative to base path)

        Returns:
            True if successful, False otherwise
        """
        if self._fallback:
            return self._fallback.delete_secret(path)

        if not self._client:
            return False

        full_path = self._get_full_path(path)

        try:
            if self.config.secret_engine == SecretEngine.KV_V2:
                self._client.secrets.kv.v2.delete_metadata_and_all_versions(
                    path=full_path,
                    mount_point=self.config.secret_mount_point,
                )
            elif self.config.secret_engine == SecretEngine.KV_V1:
                self._client.secrets.kv.v1.delete_secret(
                    path=full_path,
                    mount_point=self.config.secret_mount_point,
                )

            # Clear cache
            self._cache.delete(path)
            logger.info(f"Successfully deleted secret at: {path}")
            return True

        except Exception as e:
            logger.error(f"Failed to delete secret at {path}: {e}")
            return False

    def is_healthy(self) -> bool:
        """
        Check if Vault is healthy and accessible.

        Returns:
            True if healthy, False otherwise
        """
        if self._fallback:
            return self._fallback.is_healthy()

        if not self._client:
            return False

        try:
            health = self._client.sys.read_health_status(method="GET")
            return health.get("initialized", False) and not health.get("sealed", True)
        except Exception as e:
            logger.error(f"Vault health check failed: {e}")
            return False

    def get_database_credentials(self, role: str) -> Optional[Dict[str, str]]:
        """
        Get dynamic database credentials from Vault.

        Args:
            role: Database role name

        Returns:
            Dict with 'username' and 'password' or None
        """
        if self._fallback:
            # In fallback mode, use static credentials from env
            return {
                "username": os.getenv(f"{self.config.env_prefix}DATABASE_USERNAME", ""),
                "password": os.getenv(f"{self.config.env_prefix}DATABASE_PASSWORD", ""),
            }

        if not self._client:
            return None

        try:
            response = self._client.secrets.database.generate_credentials(
                name=role,
                mount_point="database",
            )

            data = response["data"]
            lease_id = response.get("lease_id")
            lease_duration = response.get("lease_duration", 3600)

            # Cache with lease info for rotation
            self._cache.set(
                f"database/{role}",
                data,
                ttl=lease_duration - self.config.rotation_buffer_seconds,
                lease_id=lease_id,
                renewable=response.get("renewable", False)
            )

            return {
                "username": data["username"],
                "password": data["password"],
            }

        except Exception as e:
            logger.error(f"Failed to get database credentials for role {role}: {e}")
            return None

    def close(self) -> None:
        """Close the Vault client and stop background tasks."""
        self._stop_rotation.set()
        self._cache.clear()

        if self._client:
            # Revoke our token if using AppRole
            if self.config.auth_method == VaultAuthMethod.APPROLE:
                try:
                    self._client.auth.token.revoke_self()
                except Exception:
                    pass

        logger.info("Vault client closed")

    def __enter__(self) -> "VaultClient":
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.close()


# Async version for async applications
class AsyncVaultClient:
    """Async wrapper for VaultClient."""

    def __init__(self, config: Optional[VaultConfig] = None):
        self._sync_client = VaultClient(config)

    async def get_secret(
        self,
        path: str,
        key: Optional[str] = None
    ) -> Optional[Union[str, Dict[str, Any]]]:
        """Get a secret asynchronously."""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            None,
            lambda: self._sync_client.get_secret(path, key)
        )

    async def set_secret(self, path: str, data: Dict[str, Any]) -> bool:
        """Set a secret asynchronously."""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            None,
            lambda: self._sync_client.set_secret(path, data)
        )

    async def delete_secret(self, path: str) -> bool:
        """Delete a secret asynchronously."""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            None,
            lambda: self._sync_client.delete_secret(path)
        )

    async def is_healthy(self) -> bool:
        """Check health asynchronously."""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            None,
            self._sync_client.is_healthy
        )

    async def get_database_credentials(self, role: str) -> Optional[Dict[str, str]]:
        """Get database credentials asynchronously."""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            None,
            lambda: self._sync_client.get_database_credentials(role)
        )

    def close(self) -> None:
        """Close the client."""
        self._sync_client.close()


# Decorator for injecting secrets
T = TypeVar("T")


def inject_secret(
    path: str,
    key: Optional[str] = None,
    param_name: str = "secret"
) -> Callable[[Callable[..., T]], Callable[..., T]]:
    """
    Decorator to inject a secret into a function.

    Usage:
        @inject_secret("api/keys", "api_key")
        def call_api(secret: str):
            headers = {"Authorization": f"Bearer {secret}"}
            ...
    """
    def decorator(func: Callable[..., T]) -> Callable[..., T]:
        @wraps(func)
        def wrapper(*args, **kwargs):
            client = VaultClient()
            secret_value = client.get_secret(path, key)
            kwargs[param_name] = secret_value
            return func(*args, **kwargs)
        return wrapper
    return decorator


# Singleton accessor
def get_vault_client(config: Optional[VaultConfig] = None) -> VaultClient:
    """Get the singleton Vault client instance."""
    return VaultClient(config)


# Factory function for creating configured clients
def create_vault_client(
    vault_addr: Optional[str] = None,
    auth_method: Optional[str] = None,
    **kwargs
) -> VaultClient:
    """
    Create a configured Vault client.

    Args:
        vault_addr: Vault server address
        auth_method: Authentication method (token, kubernetes, approle)
        **kwargs: Additional configuration options

    Returns:
        Configured VaultClient instance
    """
    config = VaultConfig()

    if vault_addr:
        config.vault_addr = vault_addr

    if auth_method:
        config.auth_method = VaultAuthMethod(auth_method)

    for key, value in kwargs.items():
        if hasattr(config, key):
            setattr(config, key, value)

    # Reset singleton
    VaultClient._instance = None

    return VaultClient(config)


# Quick access functions for common operations
def get_secret(path: str, key: Optional[str] = None) -> Optional[Union[str, Dict[str, Any]]]:
    """Quick function to get a secret."""
    return get_vault_client().get_secret(path, key)


def get_database_url() -> str:
    """
    Get the complete database URL from Vault or env.

    Returns:
        Database connection URL
    """
    client = get_vault_client()

    # Try to get from Vault
    db_config = client.get_secret("database/config")

    if db_config:
        return (
            f"postgresql://{db_config.get('username')}:{db_config.get('password')}"
            f"@{db_config.get('host', 'localhost')}:{db_config.get('port', 5432)}"
            f"/{db_config.get('database', 'factory')}"
        )

    # Fallback to env
    return os.getenv("DATABASE_URL", "sqlite:///factory/database/factory.db")


def get_redis_url() -> str:
    """
    Get the Redis URL from Vault or env.

    Returns:
        Redis connection URL
    """
    client = get_vault_client()

    redis_config = client.get_secret("redis/config")

    if redis_config:
        password = redis_config.get("password", "")
        host = redis_config.get("host", "localhost")
        port = redis_config.get("port", 6379)
        db = redis_config.get("db", 0)

        if password:
            return f"redis://:{password}@{host}:{port}/{db}"
        return f"redis://{host}:{port}/{db}"

    return os.getenv("REDIS_URL", "redis://localhost:6379/0")


def get_api_key(service: str) -> Optional[str]:
    """
    Get an API key for a service.

    Args:
        service: Service name (e.g., 'anthropic', 'openai')

    Returns:
        API key or None
    """
    client = get_vault_client()
    return client.get_secret(f"api-keys/{service}", "api_key")


if __name__ == "__main__":
    # Example usage and testing
    logging.basicConfig(level=logging.DEBUG)

    # Test with environment fallback
    os.environ["VAULT_DEV_MODE"] = "true"
    os.environ["FACTORY_DATABASE_USERNAME"] = "test_user"
    os.environ["FACTORY_DATABASE_PASSWORD"] = "test_pass"

    client = get_vault_client()

    print(f"Vault healthy: {client.is_healthy()}")
    print(f"Database credentials: {client.get_secret('database')}")

    # Test quick functions
    print(f"Database URL: {get_database_url()}")
    print(f"Redis URL: {get_redis_url()}")

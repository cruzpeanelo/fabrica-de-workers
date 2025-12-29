"""
Secrets Management Module - Issue #94
HashiCorp Vault integration for secure secrets management
Supports both token-based and Kubernetes Service Account authentication
"""

import os
import logging
from functools import lru_cache
from typing import Optional, Dict, Any
from abc import ABC, abstractmethod
from pathlib import Path

logger = logging.getLogger(__name__)


class SecretsBackend(ABC):
    """Abstract base class for secrets backends"""

    @abstractmethod
    def get_secret(self, path: str) -> Dict[str, Any]:
        """Retrieve a secret from the backend"""
        pass

    @abstractmethod
    def set_secret(self, path: str, data: Dict[str, Any]) -> None:
        """Store a secret in the backend"""
        pass

    @abstractmethod
    def delete_secret(self, path: str) -> None:
        """Delete a secret from the backend"""
        pass

    @abstractmethod
    def list_secrets(self, path: str) -> list:
        """List secrets at a given path"""
        pass


class VaultSecrets(SecretsBackend):
    """
    HashiCorp Vault secrets backend

    Environment variables:
        VAULT_ADDR: Vault server address (e.g., https://vault.example.com:8200)
        VAULT_TOKEN: Vault token for authentication (token auth)
        VAULT_NAMESPACE: Vault namespace (optional, for enterprise)
        VAULT_MOUNT_POINT: KV secrets engine mount point (default: 'secret')
    """

    def __init__(
        self,
        addr: Optional[str] = None,
        token: Optional[str] = None,
        namespace: Optional[str] = None,
        mount_point: str = "secret"
    ):
        try:
            import hvac
        except ImportError:
            raise ImportError(
                "hvac library is required for Vault integration. "
                "Install it with: pip install hvac"
            )

        self.addr = addr or os.getenv('VAULT_ADDR', 'http://127.0.0.1:8200')
        self.namespace = namespace or os.getenv('VAULT_NAMESPACE')
        self.mount_point = mount_point or os.getenv('VAULT_MOUNT_POINT', 'secret')

        # Initialize client
        self.client = hvac.Client(
            url=self.addr,
            token=token or os.getenv('VAULT_TOKEN'),
            namespace=self.namespace
        )

        # Validate connection
        if not self.client.is_authenticated():
            raise ValueError("Vault authentication failed. Check VAULT_TOKEN.")

        logger.info(f"Connected to Vault at {self.addr}")

    @lru_cache(maxsize=100)
    def get_secret(self, path: str) -> Dict[str, Any]:
        """
        Retrieve a secret from Vault KV v2 secrets engine

        Args:
            path: Secret path (e.g., 'factory/database')

        Returns:
            Dictionary containing secret data
        """
        try:
            response = self.client.secrets.kv.v2.read_secret_version(
                path=path,
                mount_point=self.mount_point
            )
            return response['data']['data']
        except Exception as e:
            logger.error(f"Failed to retrieve secret at {path}: {e}")
            raise

    def set_secret(self, path: str, data: Dict[str, Any]) -> None:
        """
        Store a secret in Vault KV v2 secrets engine

        Args:
            path: Secret path
            data: Dictionary of secret key-value pairs
        """
        try:
            self.client.secrets.kv.v2.create_or_update_secret(
                path=path,
                secret=data,
                mount_point=self.mount_point
            )
            # Clear cache for this path
            self.get_secret.cache_clear()
            logger.info(f"Secret stored at {path}")
        except Exception as e:
            logger.error(f"Failed to store secret at {path}: {e}")
            raise

    def delete_secret(self, path: str) -> None:
        """Delete a secret from Vault"""
        try:
            self.client.secrets.kv.v2.delete_metadata_and_all_versions(
                path=path,
                mount_point=self.mount_point
            )
            self.get_secret.cache_clear()
            logger.info(f"Secret deleted at {path}")
        except Exception as e:
            logger.error(f"Failed to delete secret at {path}: {e}")
            raise

    def list_secrets(self, path: str) -> list:
        """List secrets at a given path"""
        try:
            response = self.client.secrets.kv.v2.list_secrets(
                path=path,
                mount_point=self.mount_point
            )
            return response['data']['keys']
        except Exception as e:
            logger.error(f"Failed to list secrets at {path}: {e}")
            raise

    def get_database_credentials(self) -> Dict[str, str]:
        """Get database credentials from Vault"""
        return self.get_secret('factory/database')

    def get_api_keys(self) -> Dict[str, str]:
        """Get API keys from Vault"""
        return self.get_secret('factory/api-keys')

    def get_jwt_secret(self) -> str:
        """Get JWT secret key from Vault"""
        secrets = self.get_secret('factory/auth')
        return secrets.get('jwt_secret')

    def get_redis_password(self) -> str:
        """Get Redis password from Vault"""
        secrets = self.get_secret('factory/redis')
        return secrets.get('password')


class KubernetesVaultAuth(VaultSecrets):
    """
    Vault authentication using Kubernetes Service Account

    This class automatically authenticates with Vault using the
    Kubernetes service account JWT token mounted in the pod.

    Environment variables:
        VAULT_ADDR: Vault server address
        VAULT_ROLE: Kubernetes auth role name
        VAULT_NAMESPACE: Vault namespace (optional)
    """

    SERVICE_ACCOUNT_TOKEN_PATH = '/var/run/secrets/kubernetes.io/serviceaccount/token'

    def __init__(
        self,
        addr: Optional[str] = None,
        role: Optional[str] = None,
        namespace: Optional[str] = None,
        mount_point: str = "secret"
    ):
        try:
            import hvac
        except ImportError:
            raise ImportError(
                "hvac library is required for Vault integration. "
                "Install it with: pip install hvac"
            )

        self.addr = addr or os.getenv('VAULT_ADDR', 'http://vault.vault.svc.cluster.local:8200')
        self.role = role or os.getenv('VAULT_ROLE', 'factory-api')
        self.namespace = namespace or os.getenv('VAULT_NAMESPACE')
        self.mount_point = mount_point

        # Initialize client without token
        self.client = hvac.Client(
            url=self.addr,
            namespace=self.namespace
        )

        # Authenticate with Kubernetes
        self._authenticate_kubernetes()

        logger.info(f"Authenticated to Vault via Kubernetes SA with role {self.role}")

    def _authenticate_kubernetes(self) -> None:
        """Authenticate with Vault using Kubernetes service account"""
        jwt_path = Path(self.SERVICE_ACCOUNT_TOKEN_PATH)

        if not jwt_path.exists():
            raise FileNotFoundError(
                f"Kubernetes service account token not found at {jwt_path}. "
                "Are you running inside a Kubernetes pod?"
            )

        jwt = jwt_path.read_text().strip()

        try:
            self.client.auth.kubernetes.login(
                role=self.role,
                jwt=jwt,
                mount_point='kubernetes'
            )
        except Exception as e:
            logger.error(f"Kubernetes Vault authentication failed: {e}")
            raise

    def _ensure_authenticated(self) -> None:
        """Re-authenticate if token has expired"""
        if not self.client.is_authenticated():
            logger.info("Vault token expired, re-authenticating...")
            self._authenticate_kubernetes()


class EnvironmentSecrets(SecretsBackend):
    """
    Fallback secrets backend using environment variables

    Useful for local development or environments without Vault.
    Expects secrets in format: SECRET_PATH_KEY (e.g., FACTORY_DATABASE_HOST)
    """

    def __init__(self, prefix: str = "FACTORY"):
        self.prefix = prefix
        logger.info("Using environment variables for secrets (development mode)")

    def _env_key(self, path: str, key: str) -> str:
        """Convert secret path to environment variable name"""
        # factory/database -> FACTORY_DATABASE
        path_parts = path.replace('/', '_').upper()
        return f"{self.prefix}_{path_parts}_{key.upper()}"

    def get_secret(self, path: str) -> Dict[str, Any]:
        """Get secret from environment variables"""
        # List of expected keys for each path
        expected_keys = {
            'factory/database': ['host', 'port', 'user', 'password', 'database'],
            'factory/api-keys': ['anthropic_key', 'openai_key'],
            'factory/auth': ['jwt_secret'],
            'factory/redis': ['host', 'port', 'password'],
        }

        keys = expected_keys.get(path, [])
        result = {}

        for key in keys:
            env_key = self._env_key(path, key)
            value = os.getenv(env_key)
            if value:
                result[key] = value

        return result

    def set_secret(self, path: str, data: Dict[str, Any]) -> None:
        """Cannot set secrets in environment - log warning"""
        logger.warning(
            f"Cannot set secret at {path} - environment backend is read-only"
        )

    def delete_secret(self, path: str) -> None:
        """Cannot delete secrets from environment"""
        logger.warning(
            f"Cannot delete secret at {path} - environment backend is read-only"
        )

    def list_secrets(self, path: str) -> list:
        """List is not supported for environment backend"""
        return []


class SecretsManager:
    """
    Unified secrets manager with automatic backend selection

    Automatically selects the appropriate backend:
    1. Kubernetes Vault auth if running in K8s
    2. Token-based Vault auth if VAULT_TOKEN is set
    3. Environment variables as fallback

    Usage:
        secrets = SecretsManager.get_instance()
        db_creds = secrets.get_secret('factory/database')
    """

    _instance: Optional['SecretsManager'] = None

    def __init__(self):
        self.backend = self._select_backend()

    @classmethod
    def get_instance(cls) -> 'SecretsManager':
        """Get singleton instance of SecretsManager"""
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    def _select_backend(self) -> SecretsBackend:
        """Select appropriate secrets backend based on environment"""

        # Check if running in Kubernetes
        k8s_token_path = Path('/var/run/secrets/kubernetes.io/serviceaccount/token')
        if k8s_token_path.exists() and os.getenv('VAULT_ADDR'):
            try:
                logger.info("Detected Kubernetes environment, using K8s Vault auth")
                return KubernetesVaultAuth()
            except Exception as e:
                logger.warning(f"K8s Vault auth failed, falling back: {e}")

        # Check if Vault token is available
        if os.getenv('VAULT_TOKEN') and os.getenv('VAULT_ADDR'):
            try:
                logger.info("Using token-based Vault authentication")
                return VaultSecrets()
            except Exception as e:
                logger.warning(f"Vault token auth failed, falling back: {e}")

        # Fall back to environment variables
        logger.info("Using environment variables for secrets")
        return EnvironmentSecrets()

    def get_secret(self, path: str) -> Dict[str, Any]:
        """Retrieve a secret"""
        return self.backend.get_secret(path)

    def set_secret(self, path: str, data: Dict[str, Any]) -> None:
        """Store a secret"""
        return self.backend.set_secret(path, data)

    def delete_secret(self, path: str) -> None:
        """Delete a secret"""
        return self.backend.delete_secret(path)

    def get_database_url(self) -> str:
        """Get database connection URL from secrets"""
        try:
            creds = self.get_secret('factory/database')
            if creds:
                return (
                    f"postgresql://{creds['user']}:{creds['password']}"
                    f"@{creds['host']}:{creds.get('port', 5432)}/{creds['database']}"
                )
        except Exception:
            pass

        # Fall back to DATABASE_URL environment variable
        return os.getenv('DATABASE_URL', 'sqlite:///factory/database/factory.db')

    def get_anthropic_api_key(self) -> Optional[str]:
        """Get Anthropic API key from secrets"""
        try:
            keys = self.get_secret('factory/api-keys')
            if keys and 'anthropic_key' in keys:
                return keys['anthropic_key']
        except Exception:
            pass

        return os.getenv('ANTHROPIC_API_KEY')

    def get_jwt_secret(self) -> str:
        """Get JWT secret key"""
        try:
            auth = self.get_secret('factory/auth')
            if auth and 'jwt_secret' in auth:
                return auth['jwt_secret']
        except Exception:
            pass

        secret = os.getenv('JWT_SECRET_KEY')
        if not secret:
            logger.warning("No JWT secret configured, using insecure default")
            secret = "insecure-default-secret-change-in-production"
        return secret


# Convenience functions for easy access
def get_secrets() -> SecretsManager:
    """Get the secrets manager instance"""
    return SecretsManager.get_instance()


def get_secret(path: str) -> Dict[str, Any]:
    """Quick access to get a secret"""
    return get_secrets().get_secret(path)


def get_database_url() -> str:
    """Quick access to database URL"""
    return get_secrets().get_database_url()


def get_api_key() -> Optional[str]:
    """Quick access to Anthropic API key"""
    return get_secrets().get_anthropic_api_key()

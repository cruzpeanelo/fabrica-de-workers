# -*- coding: utf-8 -*-
"""
Salesforce Configuration
========================
Configuracoes para conexao com Salesforce CRM.

Suporta dois metodos de autenticacao:
1. Username/Password + Security Token (simples)
2. OAuth 2.0 Web Server Flow (recomendado para producao)

Exemplo de configuracao via variaveis de ambiente:
    SALESFORCE_USERNAME=user@empresa.com
    SALESFORCE_PASSWORD=senha123
    SALESFORCE_SECURITY_TOKEN=token_seguranca
    SALESFORCE_DOMAIN=login  # ou "test" para sandbox
    SALESFORCE_API_VERSION=59.0
"""

import os
from dataclasses import dataclass, field
from typing import Optional, Dict, Any
from enum import Enum


class SalesforceEnvironment(str, Enum):
    """Ambiente Salesforce"""
    PRODUCTION = "login"
    SANDBOX = "test"
    CUSTOM = "custom"


class AuthenticationType(str, Enum):
    """Tipo de autenticacao"""
    USERNAME_PASSWORD = "username_password"
    OAUTH_WEB_SERVER = "oauth_web_server"
    OAUTH_JWT = "oauth_jwt"
    OAUTH_DEVICE = "oauth_device"


@dataclass
class SalesforceOAuthConfig:
    """
    Configuracao OAuth 2.0 para Salesforce

    Attributes:
        client_id: Consumer Key do Connected App
        client_secret: Consumer Secret do Connected App
        redirect_uri: URL de callback para OAuth
        auth_url: URL de autorizacao (geralmente nao precisa alterar)
        token_url: URL para obter token (geralmente nao precisa alterar)
        scope: Escopos OAuth solicitados
    """
    client_id: str
    client_secret: str
    redirect_uri: str = "https://localhost/callback"
    auth_url: Optional[str] = None
    token_url: Optional[str] = None
    scope: str = "api refresh_token"

    # Tokens armazenados
    access_token: Optional[str] = None
    refresh_token: Optional[str] = None
    instance_url: Optional[str] = None
    token_expiry: Optional[float] = None

    def __post_init__(self):
        """Define URLs padrao se nao fornecidas"""
        if not self.auth_url:
            self.auth_url = "https://login.salesforce.com/services/oauth2/authorize"
        if not self.token_url:
            self.token_url = "https://login.salesforce.com/services/oauth2/token"

    def set_sandbox(self):
        """Configura URLs para ambiente sandbox"""
        self.auth_url = "https://test.salesforce.com/services/oauth2/authorize"
        self.token_url = "https://test.salesforce.com/services/oauth2/token"

    def get_authorization_url(self, state: Optional[str] = None) -> str:
        """
        Gera URL de autorizacao para OAuth

        Args:
            state: Estado opcional para CSRF protection

        Returns:
            URL completa para redirecionar o usuario
        """
        params = {
            "response_type": "code",
            "client_id": self.client_id,
            "redirect_uri": self.redirect_uri,
            "scope": self.scope
        }
        if state:
            params["state"] = state

        query = "&".join(f"{k}={v}" for k, v in params.items())
        return f"{self.auth_url}?{query}"

    def to_dict(self) -> Dict[str, Any]:
        """Converte configuracao para dicionario"""
        return {
            "client_id": self.client_id,
            "redirect_uri": self.redirect_uri,
            "auth_url": self.auth_url,
            "token_url": self.token_url,
            "scope": self.scope,
            "has_access_token": bool(self.access_token),
            "has_refresh_token": bool(self.refresh_token),
            "instance_url": self.instance_url
        }


@dataclass
class SalesforceConfig:
    """
    Configuracao principal para conexao com Salesforce

    Attributes:
        username: Nome de usuario Salesforce
        password: Senha do usuario
        security_token: Token de seguranca (obtido em Configuracoes do usuario)
        domain: Dominio de login ("login" para producao, "test" para sandbox)
        api_version: Versao da API Salesforce (ex: "59.0")
        oauth_config: Configuracao OAuth alternativa
        timeout: Timeout em segundos para requisicoes
        max_retries: Numero maximo de tentativas em caso de erro

    Exemplo:
        config = SalesforceConfig(
            username="user@empresa.com",
            password="senha123",
            security_token="abc123def456"
        )
    """
    # Autenticacao Username/Password
    username: Optional[str] = None
    password: Optional[str] = None
    security_token: Optional[str] = None

    # Configuracao de ambiente
    domain: str = "login"  # "login" ou "test"
    api_version: str = "59.0"
    instance_url: Optional[str] = None

    # OAuth (alternativo)
    oauth_config: Optional[SalesforceOAuthConfig] = None

    # Configuracoes de conexao
    timeout: int = 30
    max_retries: int = 3
    verify_ssl: bool = True

    # Tokens (preenchidos apos autenticacao)
    access_token: Optional[str] = None
    session_id: Optional[str] = None

    # Metadados da conexao
    organization_id: Optional[str] = None
    user_id: Optional[str] = None

    # Configuracoes avancadas
    proxy: Optional[Dict[str, str]] = None
    custom_headers: Dict[str, str] = field(default_factory=dict)

    @classmethod
    def from_env(cls) -> "SalesforceConfig":
        """
        Cria configuracao a partir de variaveis de ambiente

        Variaveis suportadas:
            SALESFORCE_USERNAME
            SALESFORCE_PASSWORD
            SALESFORCE_SECURITY_TOKEN
            SALESFORCE_DOMAIN
            SALESFORCE_API_VERSION
            SALESFORCE_INSTANCE_URL
            SALESFORCE_CLIENT_ID
            SALESFORCE_CLIENT_SECRET
            SALESFORCE_REDIRECT_URI
        """
        oauth_config = None

        # Verificar se tem configuracao OAuth
        client_id = os.getenv("SALESFORCE_CLIENT_ID")
        if client_id:
            oauth_config = SalesforceOAuthConfig(
                client_id=client_id,
                client_secret=os.getenv("SALESFORCE_CLIENT_SECRET", ""),
                redirect_uri=os.getenv("SALESFORCE_REDIRECT_URI", "https://localhost/callback")
            )

            # Verificar ambiente
            if os.getenv("SALESFORCE_DOMAIN", "login") == "test":
                oauth_config.set_sandbox()

        return cls(
            username=os.getenv("SALESFORCE_USERNAME"),
            password=os.getenv("SALESFORCE_PASSWORD"),
            security_token=os.getenv("SALESFORCE_SECURITY_TOKEN"),
            domain=os.getenv("SALESFORCE_DOMAIN", "login"),
            api_version=os.getenv("SALESFORCE_API_VERSION", "59.0"),
            instance_url=os.getenv("SALESFORCE_INSTANCE_URL"),
            oauth_config=oauth_config,
            timeout=int(os.getenv("SALESFORCE_TIMEOUT", "30")),
            max_retries=int(os.getenv("SALESFORCE_MAX_RETRIES", "3"))
        )

    @property
    def login_url(self) -> str:
        """URL de login baseada no dominio"""
        if self.domain == "test":
            return "https://test.salesforce.com"
        elif self.domain == "login":
            return "https://login.salesforce.com"
        else:
            return f"https://{self.domain}.salesforce.com"

    @property
    def soap_url(self) -> str:
        """URL do SOAP API"""
        return f"{self.login_url}/services/Soap/u/{self.api_version}"

    @property
    def rest_url(self) -> str:
        """URL base da REST API"""
        if self.instance_url:
            return f"{self.instance_url}/services/data/v{self.api_version}"
        return None

    @property
    def metadata_url(self) -> str:
        """URL da Metadata API"""
        if self.instance_url:
            return f"{self.instance_url}/services/Soap/m/{self.api_version}"
        return None

    @property
    def tooling_url(self) -> str:
        """URL da Tooling API"""
        if self.instance_url:
            return f"{self.instance_url}/services/data/v{self.api_version}/tooling"
        return None

    @property
    def bulk_url(self) -> str:
        """URL da Bulk API 2.0"""
        if self.instance_url:
            return f"{self.instance_url}/services/data/v{self.api_version}/jobs/ingest"
        return None

    @property
    def auth_type(self) -> AuthenticationType:
        """Determina o tipo de autenticacao configurado"""
        if self.oauth_config and self.oauth_config.access_token:
            return AuthenticationType.OAUTH_WEB_SERVER
        elif self.username and self.password:
            return AuthenticationType.USERNAME_PASSWORD
        else:
            return AuthenticationType.USERNAME_PASSWORD

    @property
    def is_sandbox(self) -> bool:
        """Verifica se e ambiente sandbox"""
        return self.domain == "test"

    def validate(self) -> bool:
        """
        Valida se a configuracao esta completa

        Returns:
            True se a configuracao e valida

        Raises:
            ValueError: Se a configuracao e invalida
        """
        # Verificar OAuth
        if self.oauth_config:
            if not self.oauth_config.client_id:
                raise ValueError("OAuth client_id e obrigatorio")
            if not self.oauth_config.client_secret:
                raise ValueError("OAuth client_secret e obrigatorio")
            return True

        # Verificar Username/Password
        if not self.username:
            raise ValueError("Username e obrigatorio")
        if not self.password:
            raise ValueError("Password e obrigatorio")
        if not self.security_token:
            raise ValueError("Security Token e obrigatorio (ou configure IP na whitelist)")

        return True

    def to_dict(self) -> Dict[str, Any]:
        """Converte configuracao para dicionario (sem dados sensiveis)"""
        return {
            "username": self.username,
            "domain": self.domain,
            "api_version": self.api_version,
            "instance_url": self.instance_url,
            "has_oauth": bool(self.oauth_config),
            "is_sandbox": self.is_sandbox,
            "timeout": self.timeout,
            "max_retries": self.max_retries,
            "organization_id": self.organization_id,
            "user_id": self.user_id
        }

    def __repr__(self) -> str:
        return (
            f"SalesforceConfig(username={self.username}, "
            f"domain={self.domain}, api_version={self.api_version})"
        )


# Configuracao global padrao
DEFAULT_CONFIG: Optional[SalesforceConfig] = None


def set_default_config(config: SalesforceConfig):
    """Define configuracao padrao global"""
    global DEFAULT_CONFIG
    DEFAULT_CONFIG = config


def get_default_config() -> Optional[SalesforceConfig]:
    """Obtem configuracao padrao global"""
    global DEFAULT_CONFIG
    if DEFAULT_CONFIG is None:
        try:
            DEFAULT_CONFIG = SalesforceConfig.from_env()
        except Exception:
            pass
    return DEFAULT_CONFIG

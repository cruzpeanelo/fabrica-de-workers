# -*- coding: utf-8 -*-
"""
Jira Configuration Module
=========================
Configuracao para integracao Jira com isolamento por tenant.

Terminal 5 - Issue #314: Tenant isolation for Jira integration.
"""

import os
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, Optional


@dataclass
class JiraConfig:
    """
    Configuracao especifica para Jira com isolamento por tenant.

    Attributes:
        tenant_id: ID do tenant (obrigatorio para isolamento)
        base_url: URL base do Jira (ex: https://empresa.atlassian.net)
        email: Email do usuario
        api_token: Token de API
        project_key: Chave do projeto padrao
        default_issue_type: Tipo de issue padrao
        sync_comments: Sincronizar comentarios
        sync_attachments: Sincronizar anexos
        custom_field_mapping: Mapeamento de campos customizados
        webhook_secret: Secret para validacao de webhooks
        enabled: Integracao habilitada
        auto_sync: Sincronizacao automatica
        sync_interval_minutes: Intervalo de sincronizacao
        last_sync: Ultima sincronizacao

    Exemplo:
        config = JiraConfig(
            tenant_id="TENANT-001",
            base_url="https://empresa.atlassian.net",
            email="user@empresa.com",
            api_token="..."
        )

        # Gerar chave para secret manager
        email_key = config.get_secret_key("jira_email")
        # Resultado: "TENANT-001:jira_email"
    """
    tenant_id: str = ""  # Required - identifies the client
    base_url: str = ""
    email: str = ""
    api_token: str = ""
    project_key: str = ""
    default_issue_type: str = "Story"
    sync_comments: bool = True
    sync_attachments: bool = False
    custom_field_mapping: Dict[str, str] = field(default_factory=dict)
    webhook_secret: str = ""
    enabled: bool = False
    auto_sync: bool = False
    sync_interval_minutes: int = 30
    last_sync: Optional[datetime] = None

    def get_secret_key(self, key: str) -> str:
        """
        Gera chave de secret com prefixo do tenant.

        Args:
            key: Nome do secret (ex: "jira_email", "jira_token")

        Returns:
            Chave formatada com tenant_id

        Raises:
            ValueError: Se tenant_id nao estiver definido
        """
        if not self.tenant_id:
            raise ValueError("tenant_id e obrigatorio para gerar chave de secret")
        return f"{self.tenant_id}:{key}"

    @classmethod
    def from_env(cls, tenant_id: str = "") -> "JiraConfig":
        """
        Cria configuracao a partir de variaveis de ambiente.

        Args:
            tenant_id: ID do tenant

        Returns:
            JiraConfig configurado
        """
        return cls(
            tenant_id=tenant_id or os.getenv("JIRA_TENANT_ID", ""),
            enabled=os.getenv("JIRA_ENABLED", "false").lower() == "true",
            base_url=os.getenv("JIRA_URL", ""),
            email=os.getenv("JIRA_EMAIL", ""),
            api_token=os.getenv("JIRA_API_TOKEN", ""),
            project_key=os.getenv("JIRA_PROJECT_KEY", ""),
            default_issue_type=os.getenv("JIRA_DEFAULT_ISSUE_TYPE", "Story"),
            sync_comments=os.getenv("JIRA_SYNC_COMMENTS", "true").lower() == "true",
            sync_attachments=os.getenv("JIRA_SYNC_ATTACHMENTS", "false").lower() == "true",
            webhook_secret=os.getenv("JIRA_WEBHOOK_SECRET", ""),
            auto_sync=os.getenv("JIRA_AUTO_SYNC", "false").lower() == "true",
            sync_interval_minutes=int(os.getenv("JIRA_SYNC_INTERVAL", "30"))
        )

    def is_valid(self) -> bool:
        """
        Verifica se a configuracao e valida.

        Returns:
            True se tenant_id, base_url, email e api_token estao definidos
        """
        return bool(self.tenant_id and self.base_url and self.email and self.api_token)

    def validate(self) -> list:
        """
        Valida a configuracao e retorna lista de erros.

        Returns:
            Lista de mensagens de erro
        """
        errors = []
        if not self.tenant_id:
            errors.append("tenant_id e obrigatorio")
        if not self.base_url:
            errors.append("base_url e obrigatorio")
        if not self.email:
            errors.append("email e obrigatorio")
        if not self.api_token:
            errors.append("api_token e obrigatorio")
        return errors

    def to_dict(self) -> Dict:
        """
        Converte configuracao para dicionario (sem dados sensiveis).

        Returns:
            Dicionario com configuracao
        """
        return {
            "tenant_id": self.tenant_id,
            "base_url": self.base_url,
            "email": self.email[:3] + "***" if self.email else "",
            "project_key": self.project_key,
            "default_issue_type": self.default_issue_type,
            "sync_comments": self.sync_comments,
            "sync_attachments": self.sync_attachments,
            "enabled": self.enabled,
            "auto_sync": self.auto_sync,
            "sync_interval_minutes": self.sync_interval_minutes,
            "last_sync": self.last_sync.isoformat() if self.last_sync else None
        }

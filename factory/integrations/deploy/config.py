# -*- coding: utf-8 -*-
"""
Deploy Configuration
====================
Configuracoes para sistema de deploy por tenant.

Terminal 5 - Issue #300
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from enum import Enum


class DeployMode(str, Enum):
    """Modos de deploy disponiveis"""
    MANUAL = "manual"  # Requer aprovacao explicita
    AUTOMATIC = "automatic"  # Deploy automatico com rollback
    HYBRID = "hybrid"  # Automatico para dev/staging, manual para prod


class DeployEnvironment(str, Enum):
    """Ambientes de deploy"""
    DEVELOPMENT = "development"
    STAGING = "staging"
    PRODUCTION = "production"
    SANDBOX = "sandbox"


class DeployStatus(str, Enum):
    """Status de um deploy"""
    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    ROLLED_BACK = "rolled_back"


class ApprovalStatus(str, Enum):
    """Status de aprovacao"""
    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"
    EXPIRED = "expired"


@dataclass
class NotificationConfig:
    """Configuracao de notificacoes de deploy"""
    email_enabled: bool = True
    teams_enabled: bool = False
    slack_enabled: bool = False
    email_recipients: List[str] = field(default_factory=list)
    teams_webhook_url: Optional[str] = None
    slack_webhook_url: Optional[str] = None
    notify_on_start: bool = True
    notify_on_success: bool = True
    notify_on_failure: bool = True
    notify_on_approval_required: bool = True


@dataclass
class RollbackConfig:
    """Configuracao de rollback automatico"""
    enabled: bool = True
    auto_rollback_on_failure: bool = True
    max_rollback_attempts: int = 3
    rollback_timeout_seconds: int = 300
    keep_backup_count: int = 5


@dataclass
class ApprovalConfig:
    """Configuracao de aprovacao"""
    required_approvers: int = 1
    approval_timeout_hours: int = 24
    approver_roles: List[str] = field(default_factory=lambda: ["admin", "lead"])
    auto_reject_on_timeout: bool = True
    allow_self_approval: bool = False


@dataclass
class EnvironmentConfig:
    """Configuracao por ambiente"""
    environment: DeployEnvironment
    deploy_mode: DeployMode = DeployMode.MANUAL
    approval_required: bool = True
    approval_config: ApprovalConfig = field(default_factory=ApprovalConfig)
    notification_config: NotificationConfig = field(default_factory=NotificationConfig)
    rollback_config: RollbackConfig = field(default_factory=RollbackConfig)
    pre_deploy_checks: List[str] = field(default_factory=list)
    post_deploy_checks: List[str] = field(default_factory=list)


@dataclass
class DeployConfig:
    """
    Configuracao completa de deploy por tenant.

    Attributes:
        tenant_id: ID do tenant
        default_mode: Modo padrao de deploy
        environments: Configuracoes por ambiente
        notification_config: Configuracao global de notificacoes
        rollback_config: Configuracao global de rollback
        enabled_integrations: Integracoes habilitadas para deploy
        metadata: Metadados adicionais
    """
    tenant_id: str
    default_mode: DeployMode = DeployMode.MANUAL
    environments: Dict[str, EnvironmentConfig] = field(default_factory=dict)
    notification_config: NotificationConfig = field(default_factory=NotificationConfig)
    rollback_config: RollbackConfig = field(default_factory=RollbackConfig)
    enabled_integrations: List[str] = field(default_factory=lambda: [
        "sap_s4", "sap_ecc", "sap_cpi", "salesforce", "azure_devops"
    ])
    metadata: Dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        """Inicializa configuracoes padrao por ambiente"""
        if not self.environments:
            self.environments = {
                DeployEnvironment.DEVELOPMENT.value: EnvironmentConfig(
                    environment=DeployEnvironment.DEVELOPMENT,
                    deploy_mode=DeployMode.AUTOMATIC,
                    approval_required=False
                ),
                DeployEnvironment.STAGING.value: EnvironmentConfig(
                    environment=DeployEnvironment.STAGING,
                    deploy_mode=DeployMode.AUTOMATIC,
                    approval_required=True,
                    approval_config=ApprovalConfig(required_approvers=1)
                ),
                DeployEnvironment.PRODUCTION.value: EnvironmentConfig(
                    environment=DeployEnvironment.PRODUCTION,
                    deploy_mode=DeployMode.MANUAL,
                    approval_required=True,
                    approval_config=ApprovalConfig(required_approvers=2)
                ),
            }

    def get_environment_config(self, env: DeployEnvironment) -> EnvironmentConfig:
        """Obtem configuracao de um ambiente"""
        env_key = env.value if isinstance(env, DeployEnvironment) else env
        return self.environments.get(env_key, EnvironmentConfig(environment=env))

    def is_approval_required(self, env: DeployEnvironment) -> bool:
        """Verifica se aprovacao e necessaria para ambiente"""
        env_config = self.get_environment_config(env)
        return env_config.approval_required

    def get_deploy_mode(self, env: DeployEnvironment) -> DeployMode:
        """Obtem modo de deploy para ambiente"""
        env_config = self.get_environment_config(env)
        return env_config.deploy_mode

    def validate(self) -> List[str]:
        """Valida a configuracao"""
        errors = []
        if not self.tenant_id:
            errors.append("tenant_id e obrigatorio")
        if not self.enabled_integrations:
            errors.append("Pelo menos uma integracao deve estar habilitada")
        return errors

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario"""
        return {
            "tenant_id": self.tenant_id,
            "default_mode": self.default_mode.value,
            "environments": {
                k: {
                    "environment": v.environment.value,
                    "deploy_mode": v.deploy_mode.value,
                    "approval_required": v.approval_required
                }
                for k, v in self.environments.items()
            },
            "enabled_integrations": self.enabled_integrations,
            "metadata": self.metadata
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "DeployConfig":
        """Cria a partir de dicionario"""
        environments = {}
        for env_key, env_data in data.get("environments", {}).items():
            environments[env_key] = EnvironmentConfig(
                environment=DeployEnvironment(env_data.get("environment", env_key)),
                deploy_mode=DeployMode(env_data.get("deploy_mode", "manual")),
                approval_required=env_data.get("approval_required", True)
            )

        return cls(
            tenant_id=data.get("tenant_id", ""),
            default_mode=DeployMode(data.get("default_mode", "manual")),
            environments=environments,
            enabled_integrations=data.get("enabled_integrations", []),
            metadata=data.get("metadata", {})
        )

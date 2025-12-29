# -*- coding: utf-8 -*-
"""
Feature Flags Configuration System
===================================

Issue #125 - Configuracao Local vs Producao (Feature Flags)

This module provides a comprehensive feature flag system for:
- Environment-based feature toggles (development, staging, production)
- Tenant-specific overrides
- Gradual rollout (percentage-based)
- A/B testing support
- Dashboard for flag management

Usage:
    from factory.config.feature_flags import FeatureFlags, is_feature_enabled

    # Check if feature is enabled
    if is_feature_enabled("new_dashboard"):
        # New feature code
        pass

    # Check for specific tenant
    if is_feature_enabled("beta_ai_model", tenant_id="ENT-001"):
        # Tenant-specific feature
        pass

    # Get all flags
    flags = FeatureFlags.get_all_flags()

Author: Fabrica de Agentes
"""

import os
import json
import hashlib
import logging
from dataclasses import dataclass, field, asdict
from datetime import datetime
from enum import Enum
from functools import lru_cache
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Union
from threading import Lock

logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS
# =============================================================================

class Environment(str, Enum):
    """Application environments."""
    DEVELOPMENT = "development"
    STAGING = "staging"
    PRODUCTION = "production"
    TEST = "test"


class FlagType(str, Enum):
    """Types of feature flags."""
    BOOLEAN = "boolean"  # Simple on/off
    PERCENTAGE = "percentage"  # Gradual rollout (0-100%)
    VARIANT = "variant"  # A/B testing with named variants
    TENANT = "tenant"  # Tenant-specific
    USER = "user"  # User-specific


class FlagStatus(str, Enum):
    """Status of a feature flag."""
    ACTIVE = "active"
    INACTIVE = "inactive"
    DEPRECATED = "deprecated"


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class FeatureFlag:
    """Represents a single feature flag."""
    key: str
    name: str
    description: str = ""
    flag_type: FlagType = FlagType.BOOLEAN
    status: FlagStatus = FlagStatus.ACTIVE

    # Default value (boolean, percentage 0-100, or variant name)
    default_value: Any = False

    # Environment-specific overrides
    environments: Dict[str, Any] = field(default_factory=dict)

    # Tenant-specific overrides
    tenant_overrides: Dict[str, Any] = field(default_factory=dict)

    # User-specific overrides
    user_overrides: Dict[str, Any] = field(default_factory=dict)

    # Rollout configuration
    rollout_percentage: int = 0  # 0-100
    rollout_users: List[str] = field(default_factory=list)
    rollout_tenants: List[str] = field(default_factory=list)

    # A/B testing variants
    variants: Dict[str, int] = field(default_factory=dict)  # {"A": 50, "B": 50}

    # Metadata
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    created_by: str = "system"

    # Tags for organization
    tags: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        data = asdict(self)
        data["created_at"] = self.created_at.isoformat() if self.created_at else None
        data["updated_at"] = self.updated_at.isoformat() if self.updated_at else None
        data["flag_type"] = self.flag_type.value if isinstance(self.flag_type, FlagType) else self.flag_type
        data["status"] = self.status.value if isinstance(self.status, FlagStatus) else self.status
        return data


# =============================================================================
# DEFAULT FLAGS CONFIGURATION
# =============================================================================

DEFAULT_FLAGS: Dict[str, FeatureFlag] = {
    # Core Features
    "stories_module": FeatureFlag(
        key="stories_module",
        name="Stories Module",
        description="Enable the Agile Stories feature with Kanban board",
        default_value=True,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["core", "stories"]
    ),

    "workers_module": FeatureFlag(
        key="workers_module",
        name="Workers Module",
        description="Enable Claude AI workers for autonomous development",
        default_value=True,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["core", "ai"]
    ),

    "chat_assistant": FeatureFlag(
        key="chat_assistant",
        name="Chat Assistant",
        description="Enable AI chat assistant in dashboard",
        default_value=True,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["core", "ai"]
    ),

    # Beta Features
    "beta_new_dashboard": FeatureFlag(
        key="beta_new_dashboard",
        name="New Dashboard (Beta)",
        description="Enable the redesigned dashboard interface",
        default_value=False,
        flag_type=FlagType.PERCENTAGE,
        rollout_percentage=20,  # 20% of users
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: False,
        },
        tags=["beta", "ui"]
    ),

    "beta_multi_agent": FeatureFlag(
        key="beta_multi_agent",
        name="Multi-Agent System (Beta)",
        description="Enable multiple AI agents working in parallel",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: False,
        },
        rollout_tenants=["ENT-001"],  # Enterprise tenants only
        tags=["beta", "ai", "enterprise"]
    ),

    "beta_code_review_ai": FeatureFlag(
        key="beta_code_review_ai",
        name="AI Code Review (Beta)",
        description="Automatic code review by AI for generated code",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: False,
        },
        tags=["beta", "ai"]
    ),

    # Experimental Features
    "experimental_mcp_tools": FeatureFlag(
        key="experimental_mcp_tools",
        name="MCP Tools Integration",
        description="Enable Model Context Protocol tools",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: False,
            Environment.PRODUCTION.value: False,
        },
        tags=["experimental", "ai"]
    ),

    "experimental_voice_input": FeatureFlag(
        key="experimental_voice_input",
        name="Voice Input",
        description="Enable voice input for creating stories",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: False,
            Environment.PRODUCTION.value: False,
        },
        tags=["experimental", "ui"]
    ),

    # Enterprise Features
    "enterprise_sso": FeatureFlag(
        key="enterprise_sso",
        name="Enterprise SSO",
        description="Enable SSO/SAML authentication",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tenant_overrides={
            "ENT-001": True,  # Enterprise tenant has SSO
        },
        tags=["enterprise", "security"]
    ),

    "enterprise_audit_logs": FeatureFlag(
        key="enterprise_audit_logs",
        name="Enterprise Audit Logs",
        description="Enable detailed audit logging",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tenant_overrides={
            "ENT-001": True,
        },
        tags=["enterprise", "security"]
    ),

    "enterprise_custom_branding": FeatureFlag(
        key="enterprise_custom_branding",
        name="Custom Branding",
        description="Enable white-label customization",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["enterprise", "ui"]
    ),

    # Billing Features
    "billing_stripe": FeatureFlag(
        key="billing_stripe",
        name="Stripe Billing",
        description="Enable Stripe payment integration",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: False,  # Use test mode
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["billing"]
    ),

    "billing_usage_tracking": FeatureFlag(
        key="billing_usage_tracking",
        name="Usage Tracking",
        description="Track usage for metered billing",
        default_value=True,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["billing"]
    ),

    # Performance Features
    "performance_caching": FeatureFlag(
        key="performance_caching",
        name="Redis Caching",
        description="Enable Redis caching for improved performance",
        default_value=True,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["performance"]
    ),

    "performance_async_workers": FeatureFlag(
        key="performance_async_workers",
        name="Async Workers",
        description="Use async workers for background jobs",
        default_value=True,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["performance"]
    ),

    # A/B Test Example
    "ab_test_checkout_flow": FeatureFlag(
        key="ab_test_checkout_flow",
        name="Checkout Flow A/B Test",
        description="Test new vs old checkout flow",
        flag_type=FlagType.VARIANT,
        default_value="control",
        variants={"control": 50, "new_flow": 50},
        environments={
            Environment.DEVELOPMENT.value: "new_flow",
            Environment.STAGING.value: True,
            Environment.PRODUCTION.value: True,
        },
        tags=["ab_test", "billing"]
    ),

    # Maintenance Mode
    "maintenance_mode": FeatureFlag(
        key="maintenance_mode",
        name="Maintenance Mode",
        description="Put the application in maintenance mode",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: False,
            Environment.STAGING.value: False,
            Environment.PRODUCTION.value: False,
        },
        tags=["system"]
    ),

    # Debug Features
    "debug_mode": FeatureFlag(
        key="debug_mode",
        name="Debug Mode",
        description="Enable debug mode with detailed logging",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: False,
            Environment.PRODUCTION.value: False,
        },
        tags=["debug"]
    ),

    "debug_sql_logging": FeatureFlag(
        key="debug_sql_logging",
        name="SQL Query Logging",
        description="Log all SQL queries for debugging",
        default_value=False,
        environments={
            Environment.DEVELOPMENT.value: True,
            Environment.STAGING.value: False,
            Environment.PRODUCTION.value: False,
        },
        tags=["debug"]
    ),
}


# =============================================================================
# FEATURE FLAGS MANAGER
# =============================================================================

class FeatureFlagsManager:
    """
    Manager class for feature flags.
    Provides thread-safe access to flags with caching.
    """

    _instance: Optional["FeatureFlagsManager"] = None
    _lock: Lock = Lock()

    def __new__(cls) -> "FeatureFlagsManager":
        """Singleton pattern for feature flags manager."""
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
                    cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        """Initialize the feature flags manager."""
        if self._initialized:
            return

        self._flags: Dict[str, FeatureFlag] = {}
        self._environment = self._get_environment()
        self._custom_flags_path: Optional[Path] = None
        self._cache_ttl = 60  # seconds
        self._last_refresh = datetime.min

        # Load default flags
        self._flags = DEFAULT_FLAGS.copy()

        # Try to load custom flags from file/database
        self._load_custom_flags()

        self._initialized = True
        logger.info(f"FeatureFlagsManager initialized for environment: {self._environment}")

    def _get_environment(self) -> Environment:
        """Get current environment from env vars."""
        env_str = os.getenv("ENVIRONMENT", os.getenv("ENV", "development")).lower()
        try:
            return Environment(env_str)
        except ValueError:
            logger.warning(f"Unknown environment '{env_str}', defaulting to development")
            return Environment.DEVELOPMENT

    def _load_custom_flags(self):
        """Load custom flags from configuration file if exists."""
        custom_path = Path(os.getenv("FEATURE_FLAGS_PATH", ""))
        if custom_path.exists() and custom_path.suffix == ".json":
            try:
                with open(custom_path) as f:
                    custom_data = json.load(f)
                for key, flag_data in custom_data.items():
                    if key in self._flags:
                        # Update existing flag
                        for attr, value in flag_data.items():
                            if hasattr(self._flags[key], attr):
                                setattr(self._flags[key], attr, value)
                    else:
                        # Create new flag
                        self._flags[key] = FeatureFlag(key=key, name=key, **flag_data)
                logger.info(f"Loaded custom flags from {custom_path}")
            except Exception as e:
                logger.error(f"Error loading custom flags: {e}")

    def _hash_id(self, identifier: str, flag_key: str) -> int:
        """Generate consistent hash for percentage rollout."""
        combined = f"{flag_key}:{identifier}"
        return int(hashlib.md5(combined.encode()).hexdigest(), 16) % 100

    def is_enabled(
        self,
        flag_key: str,
        tenant_id: Optional[str] = None,
        user_id: Optional[str] = None,
        default: bool = False
    ) -> bool:
        """
        Check if a feature flag is enabled.

        Args:
            flag_key: The unique key of the feature flag
            tenant_id: Optional tenant ID for tenant-specific checks
            user_id: Optional user ID for user-specific checks
            default: Default value if flag not found

        Returns:
            bool: Whether the feature is enabled
        """
        flag = self._flags.get(flag_key)
        if not flag:
            logger.debug(f"Flag '{flag_key}' not found, returning default: {default}")
            return default

        # Check if flag is deprecated/inactive
        if flag.status != FlagStatus.ACTIVE:
            return False

        # Get environment-specific value
        env_value = flag.environments.get(self._environment.value)

        # Check user-specific override first
        if user_id and user_id in flag.user_overrides:
            return bool(flag.user_overrides[user_id])

        # Check if user is in rollout list
        if user_id and user_id in flag.rollout_users:
            return True

        # Check tenant-specific override
        if tenant_id and tenant_id in flag.tenant_overrides:
            return bool(flag.tenant_overrides[tenant_id])

        # Check if tenant is in rollout list
        if tenant_id and tenant_id in flag.rollout_tenants:
            return True

        # Handle percentage rollout
        if flag.flag_type == FlagType.PERCENTAGE:
            identifier = user_id or tenant_id or "default"
            hash_value = self._hash_id(identifier, flag_key)
            return hash_value < flag.rollout_percentage

        # Return environment value or default
        if env_value is not None:
            return bool(env_value)

        return bool(flag.default_value)

    def get_variant(
        self,
        flag_key: str,
        tenant_id: Optional[str] = None,
        user_id: Optional[str] = None,
        default: str = "control"
    ) -> str:
        """
        Get A/B test variant for a flag.

        Args:
            flag_key: The unique key of the feature flag
            tenant_id: Optional tenant ID
            user_id: Optional user ID
            default: Default variant if flag not found

        Returns:
            str: The variant name
        """
        flag = self._flags.get(flag_key)
        if not flag or flag.flag_type != FlagType.VARIANT:
            return default

        if not flag.variants:
            return str(flag.default_value) if flag.default_value else default

        # Get consistent variant based on user/tenant
        identifier = user_id or tenant_id or "default"
        hash_value = self._hash_id(identifier, flag_key)

        # Calculate variant based on percentage weights
        cumulative = 0
        for variant, percentage in flag.variants.items():
            cumulative += percentage
            if hash_value < cumulative:
                return variant

        return default

    def get_flag(self, flag_key: str) -> Optional[FeatureFlag]:
        """Get a feature flag by key."""
        return self._flags.get(flag_key)

    def get_all_flags(self, tags: Optional[List[str]] = None) -> List[FeatureFlag]:
        """Get all feature flags, optionally filtered by tags."""
        flags = list(self._flags.values())
        if tags:
            flags = [f for f in flags if any(t in f.tags for t in tags)]
        return flags

    def set_flag(self, flag_key: str, flag: FeatureFlag):
        """Set or update a feature flag."""
        flag.updated_at = datetime.utcnow()
        self._flags[flag_key] = flag
        logger.info(f"Flag '{flag_key}' updated")

    def override_for_tenant(self, flag_key: str, tenant_id: str, value: Any):
        """Set tenant-specific override for a flag."""
        if flag_key in self._flags:
            self._flags[flag_key].tenant_overrides[tenant_id] = value
            self._flags[flag_key].updated_at = datetime.utcnow()
            logger.info(f"Tenant override set for '{flag_key}': {tenant_id}={value}")

    def override_for_user(self, flag_key: str, user_id: str, value: Any):
        """Set user-specific override for a flag."""
        if flag_key in self._flags:
            self._flags[flag_key].user_overrides[user_id] = value
            self._flags[flag_key].updated_at = datetime.utcnow()
            logger.info(f"User override set for '{flag_key}': {user_id}={value}")

    def remove_tenant_override(self, flag_key: str, tenant_id: str):
        """Remove tenant-specific override."""
        if flag_key in self._flags:
            self._flags[flag_key].tenant_overrides.pop(tenant_id, None)

    def remove_user_override(self, flag_key: str, user_id: str):
        """Remove user-specific override."""
        if flag_key in self._flags:
            self._flags[flag_key].user_overrides.pop(user_id, None)

    def get_flags_for_tenant(self, tenant_id: str) -> Dict[str, bool]:
        """Get all flag values for a specific tenant."""
        return {
            key: self.is_enabled(key, tenant_id=tenant_id)
            for key in self._flags.keys()
        }

    def export_flags(self) -> Dict[str, Dict]:
        """Export all flags as dictionary."""
        return {key: flag.to_dict() for key, flag in self._flags.items()}

    def get_environment(self) -> Environment:
        """Get current environment."""
        return self._environment


# =============================================================================
# MODULE-LEVEL FUNCTIONS
# =============================================================================

# Global instance
_manager: Optional[FeatureFlagsManager] = None


def get_manager() -> FeatureFlagsManager:
    """Get the global feature flags manager instance."""
    global _manager
    if _manager is None:
        _manager = FeatureFlagsManager()
    return _manager


def is_feature_enabled(
    flag_key: str,
    tenant_id: Optional[str] = None,
    user_id: Optional[str] = None,
    default: bool = False
) -> bool:
    """
    Check if a feature flag is enabled.

    This is the main function for checking feature flags.

    Args:
        flag_key: The unique key of the feature flag
        tenant_id: Optional tenant ID for tenant-specific checks
        user_id: Optional user ID for user-specific checks
        default: Default value if flag not found

    Returns:
        bool: Whether the feature is enabled

    Example:
        if is_feature_enabled("beta_new_dashboard"):
            render_new_dashboard()
        else:
            render_old_dashboard()
    """
    return get_manager().is_enabled(flag_key, tenant_id, user_id, default)


def get_variant(
    flag_key: str,
    tenant_id: Optional[str] = None,
    user_id: Optional[str] = None,
    default: str = "control"
) -> str:
    """
    Get A/B test variant for a feature flag.

    Args:
        flag_key: The unique key of the feature flag
        tenant_id: Optional tenant ID
        user_id: Optional user ID
        default: Default variant if flag not found

    Returns:
        str: The variant name

    Example:
        variant = get_variant("ab_test_checkout_flow", user_id="user123")
        if variant == "new_flow":
            show_new_checkout()
        else:
            show_old_checkout()
    """
    return get_manager().get_variant(flag_key, tenant_id, user_id, default)


def get_all_flags(tags: Optional[List[str]] = None) -> List[FeatureFlag]:
    """
    Get all feature flags.

    Args:
        tags: Optional list of tags to filter by

    Returns:
        List of FeatureFlag objects
    """
    return get_manager().get_all_flags(tags)


def override_flag_for_tenant(flag_key: str, tenant_id: str, value: Any):
    """Set a tenant-specific override for a flag."""
    get_manager().override_for_tenant(flag_key, tenant_id, value)


def override_flag_for_user(flag_key: str, user_id: str, value: Any):
    """Set a user-specific override for a flag."""
    get_manager().override_for_user(flag_key, user_id, value)


def get_flags_for_tenant(tenant_id: str) -> Dict[str, bool]:
    """Get all flag values for a specific tenant."""
    return get_manager().get_flags_for_tenant(tenant_id)


def get_current_environment() -> Environment:
    """Get the current environment."""
    return get_manager().get_environment()


# =============================================================================
# FEATURE FLAGS DASHBOARD API
# =============================================================================

class FeatureFlagsDashboard:
    """
    Dashboard API for managing feature flags.
    Can be integrated into admin interface.
    """

    def __init__(self):
        self.manager = get_manager()

    def get_dashboard_data(self) -> Dict[str, Any]:
        """Get data for feature flags dashboard."""
        flags = self.manager.get_all_flags()

        # Group by tags
        tags_map: Dict[str, List[str]] = {}
        for flag in flags:
            for tag in flag.tags:
                if tag not in tags_map:
                    tags_map[tag] = []
                tags_map[tag].append(flag.key)

        # Count by status
        status_counts = {
            "active": len([f for f in flags if f.status == FlagStatus.ACTIVE]),
            "inactive": len([f for f in flags if f.status == FlagStatus.INACTIVE]),
            "deprecated": len([f for f in flags if f.status == FlagStatus.DEPRECATED]),
        }

        return {
            "environment": self.manager.get_environment().value,
            "total_flags": len(flags),
            "status_counts": status_counts,
            "tags": tags_map,
            "flags": [f.to_dict() for f in flags],
        }

    def toggle_flag(self, flag_key: str, enabled: bool) -> bool:
        """Toggle a flag on/off for current environment."""
        flag = self.manager.get_flag(flag_key)
        if flag:
            env = self.manager.get_environment().value
            flag.environments[env] = enabled
            flag.updated_at = datetime.utcnow()
            return True
        return False

    def update_rollout_percentage(self, flag_key: str, percentage: int) -> bool:
        """Update rollout percentage for a flag."""
        flag = self.manager.get_flag(flag_key)
        if flag and 0 <= percentage <= 100:
            flag.rollout_percentage = percentage
            flag.updated_at = datetime.utcnow()
            return True
        return False


# =============================================================================
# CLI INTERFACE
# =============================================================================

def main():
    """CLI for testing feature flags."""
    import argparse

    parser = argparse.ArgumentParser(description="Feature Flags CLI")
    parser.add_argument("--list", "-l", action="store_true", help="List all flags")
    parser.add_argument("--check", "-c", type=str, help="Check if flag is enabled")
    parser.add_argument("--tenant", "-t", type=str, help="Tenant ID for check")
    parser.add_argument("--user", "-u", type=str, help="User ID for check")
    parser.add_argument("--export", "-e", type=str, help="Export flags to JSON file")

    args = parser.parse_args()

    manager = get_manager()
    print(f"Environment: {manager.get_environment().value}")
    print("=" * 50)

    if args.list:
        flags = manager.get_all_flags()
        for flag in flags:
            status = "[ON]" if manager.is_enabled(flag.key) else "[OFF]"
            print(f"{status} {flag.key}: {flag.name}")

    elif args.check:
        enabled = is_feature_enabled(
            args.check,
            tenant_id=args.tenant,
            user_id=args.user
        )
        print(f"Flag '{args.check}': {'ENABLED' if enabled else 'DISABLED'}")

    elif args.export:
        data = manager.export_flags()
        with open(args.export, "w") as f:
            json.dump(data, f, indent=2, default=str)
        print(f"Flags exported to {args.export}")

    else:
        parser.print_help()


if __name__ == "__main__":
    main()

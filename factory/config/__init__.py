# -*- coding: utf-8 -*-
"""
Factory Configuration Package
==============================

This package provides configuration management for the Plataforma E platform:
- Feature flags for environment and tenant-based feature toggles
- Environment configuration (development, staging, production)

Usage:
    from factory.config import is_feature_enabled, get_current_environment

    if is_feature_enabled("beta_feature"):
        # New feature code
        pass
"""

from .feature_flags import (
    # Main functions
    is_feature_enabled,
    get_variant,
    get_all_flags,
    get_current_environment,
    get_flags_for_tenant,

    # Override functions
    override_flag_for_tenant,
    override_flag_for_user,

    # Manager and Dashboard
    FeatureFlagsManager,
    FeatureFlagsDashboard,
    get_manager,

    # Data classes and enums
    FeatureFlag,
    Environment,
    FlagType,
    FlagStatus,
)

from .security import (
    is_credential_blocked,
    validate_password_strength,
    get_password_requirements,
    BLOCKED_CREDENTIALS,
    PASSWORD_MIN_LENGTH,
)

# JWT Configuration - Issue #417
import os
JWT_SECRET_KEY = os.getenv("JWT_SECRET_KEY", "dev-secret-key-change-in-production")
JWT_ALGORITHM = "HS256"
JWT_ACCESS_TOKEN_EXPIRE_MINUTES = 30

# Backward compatibility aliases for oauth2, api/v1/oauth, etc.
ACCESS_TOKEN_EXPIRE_MINUTES = int(os.getenv("ACCESS_TOKEN_EXPIRE_MINUTES", str(JWT_ACCESS_TOKEN_EXPIRE_MINUTES)))

# Dashboard Configuration (backward compatibility with factory/config.py)
DASHBOARD_HOST = os.getenv("DASHBOARD_HOST", "127.0.0.1")
DASHBOARD_PORT = int(os.getenv("DASHBOARD_PORT", "9001"))
DASHBOARD_TITLE = "Plataforma E v6.5"

# Paths Configuration
from pathlib import Path
BASE_DIR = Path(__file__).parent.parent.parent
PROJECTS_DIR = BASE_DIR / "projects"
UPLOADS_DIR = BASE_DIR / "uploads"

# Create directories if they don't exist
PROJECTS_DIR.mkdir(parents=True, exist_ok=True)
UPLOADS_DIR.mkdir(parents=True, exist_ok=True)

__all__ = [
    # Main functions
    "is_feature_enabled",
    "get_variant",
    "get_all_flags",
    "get_current_environment",
    "get_flags_for_tenant",

    # Override functions
    "override_flag_for_tenant",
    "override_flag_for_user",

    # Manager and Dashboard
    "FeatureFlagsManager",
    "FeatureFlagsDashboard",
    "get_manager",

    # Data classes and enums
    "FeatureFlag",
    "Environment",
    "FlagType",
    "FlagStatus",

    # Security (Issue #138)
    "is_credential_blocked",
    "validate_password_strength",
    "get_password_requirements",
    "BLOCKED_CREDENTIALS",
    "PASSWORD_MIN_LENGTH",

    # JWT Configuration (Issue #417)
    "JWT_SECRET_KEY",
    "JWT_ALGORITHM",
    "JWT_ACCESS_TOKEN_EXPIRE_MINUTES",
    "ACCESS_TOKEN_EXPIRE_MINUTES",  # Backward compatibility

    # Dashboard Configuration
    "DASHBOARD_HOST",
    "DASHBOARD_PORT",
    "DASHBOARD_TITLE",

    # Paths Configuration
    "BASE_DIR",
    "PROJECTS_DIR",
    "UPLOADS_DIR",
]

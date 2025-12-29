# -*- coding: utf-8 -*-
"""
Factory Configuration Package
==============================

This package provides configuration management for the Fabrica de Agentes platform:
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
]

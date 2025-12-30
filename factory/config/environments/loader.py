# -*- coding: utf-8 -*-
"""
Environment Loader - Issue #212
===============================
Loads the appropriate configuration based on ENVIRONMENT variable.
"""

import os
import logging
from typing import Union
from functools import lru_cache

from .base import BaseSettings
from .development import DevelopmentSettings
from .staging import StagingSettings
from .production import ProductionSettings

logger = logging.getLogger(__name__)

# Environment mapping
ENVIRONMENT_CONFIGS = {
    "development": DevelopmentSettings,
    "dev": DevelopmentSettings,
    "staging": StagingSettings,
    "stg": StagingSettings,
    "production": ProductionSettings,
    "prod": ProductionSettings
}


def get_environment() -> str:
    """
    Get the current environment name.

    Returns:
        Environment name (development, staging, or production)
    """
    env = os.getenv("ENVIRONMENT", "development").lower()

    # Normalize environment name
    if env in ("dev", "development"):
        return "development"
    elif env in ("stg", "staging"):
        return "staging"
    elif env in ("prod", "production"):
        return "production"
    else:
        logger.warning(f"Unknown environment '{env}', defaulting to development")
        return "development"


@lru_cache()
def get_settings() -> Union[DevelopmentSettings, StagingSettings, ProductionSettings]:
    """
    Get settings for the current environment.

    Uses caching to ensure settings are only loaded once.

    Returns:
        Environment-specific settings instance
    """
    env = get_environment()
    config_class = ENVIRONMENT_CONFIGS.get(env, DevelopmentSettings)

    logger.info(f"Loading {env} configuration")

    settings = config_class()

    # Validate production settings
    if env == "production" and hasattr(settings, "validate_required"):
        try:
            settings.validate_required()
        except ValueError as e:
            logger.error(f"Production configuration error: {e}")
            raise

    return settings


def reload_settings() -> None:
    """
    Reload settings (clears cache).

    Useful for testing or dynamic configuration updates.
    """
    get_settings.cache_clear()
    logger.info("Settings cache cleared")

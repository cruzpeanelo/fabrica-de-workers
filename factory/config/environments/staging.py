# -*- coding: utf-8 -*-
"""
Staging Settings - Issue #212
=============================
Configuration for staging environment.
"""

from typing import List
from .base import BaseSettings


class StagingSettings(BaseSettings):
    """
    Staging environment settings.

    Production-like environment for testing before production deployment.
    """

    ENVIRONMENT: str = "staging"
    DEBUG: bool = False

    # Logging
    LOG_LEVEL: str = "INFO"
    LOG_FORMAT: str = "json"

    # Database (PostgreSQL in staging)
    DATABASE_URL: str = "postgresql://factory:password@staging-db:5432/factory_staging"

    # Redis
    REDIS_URL: str = "redis://staging-redis:6379"

    # Security
    JWT_SECRET_KEY: str = ""  # Must be set via environment
    JWT_ACCESS_TOKEN_EXPIRE_MINUTES: int = 60

    # CORS (staging domains only)
    CORS_ORIGINS: List[str] = [
        "https://staging.example.com",
        "https://staging-api.example.com"
    ]

    # Features (same as production for accurate testing)
    ENABLE_WEBSOCKETS: bool = True
    ENABLE_ANALYTICS: bool = True
    ENABLE_AI_ASSISTANT: bool = True

    # Observability (enabled for debugging)
    SENTRY_DSN: str = ""  # Staging Sentry project
    OTLP_ENDPOINT: str = ""

    # Server
    WORKERS: int = 2
    RELOAD: bool = False

    class Config:
        env_file = ".env.staging"
        env_file_encoding = "utf-8"

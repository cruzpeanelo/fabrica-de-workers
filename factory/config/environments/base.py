# -*- coding: utf-8 -*-
"""
Base Settings - Issue #212
==========================
Base configuration class with common settings.
"""

import os
from typing import Optional, List
from pydantic_settings import BaseSettings as PydanticBaseSettings


class BaseSettings(PydanticBaseSettings):
    """
    Base settings for all environments.

    All settings can be overridden via environment variables.
    """

    # Environment
    ENVIRONMENT: str = "development"
    DEBUG: bool = False
    APP_NAME: str = "Plataforma E"
    APP_VERSION: str = "6.5.0"

    # Server
    HOST: str = "0.0.0.0"
    PORT: int = 9001

    # Database
    DATABASE_URL: str = "sqlite:///factory/database/factory.db"

    # Redis
    REDIS_URL: str = "redis://localhost:6379"

    # Security
    JWT_SECRET_KEY: str = ""
    JWT_ACCESS_TOKEN_EXPIRE_MINUTES: int = 60
    JWT_REFRESH_TOKEN_EXPIRE_DAYS: int = 7

    # Claude API
    ANTHROPIC_API_KEY: str = ""
    CLAUDE_MODEL: str = "claude-sonnet-4-20250514"

    # Logging
    LOG_LEVEL: str = "INFO"
    LOG_FORMAT: str = "json"

    # CORS
    CORS_ORIGINS: List[str] = ["*"]

    # Feature Flags
    ENABLE_WEBSOCKETS: bool = True
    ENABLE_ANALYTICS: bool = True
    ENABLE_AI_ASSISTANT: bool = True

    # Observability
    SENTRY_DSN: Optional[str] = None
    OTLP_ENDPOINT: Optional[str] = None

    # Storage
    UPLOAD_DIR: str = "uploads"
    MAX_UPLOAD_SIZE_MB: int = 50

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        case_sensitive = True

    def is_production(self) -> bool:
        """Check if running in production."""
        return self.ENVIRONMENT == "production"

    def is_staging(self) -> bool:
        """Check if running in staging."""
        return self.ENVIRONMENT == "staging"

    def is_development(self) -> bool:
        """Check if running in development."""
        return self.ENVIRONMENT == "development"

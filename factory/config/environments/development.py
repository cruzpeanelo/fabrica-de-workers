# -*- coding: utf-8 -*-
"""
Development Settings - Issue #212
=================================
Configuration for development environment.
"""

from .base import BaseSettings


class DevelopmentSettings(BaseSettings):
    """
    Development environment settings.

    Optimized for local development with debugging enabled.
    """

    ENVIRONMENT: str = "development"
    DEBUG: bool = True

    # Logging
    LOG_LEVEL: str = "DEBUG"
    LOG_FORMAT: str = "readable"

    # Database (SQLite for simplicity)
    DATABASE_URL: str = "sqlite:///factory/database/factory.db"

    # Redis (local or skip if not available)
    REDIS_URL: str = "redis://localhost:6379"

    # Security (relaxed for development)
    JWT_SECRET_KEY: str = "dev-secret-key-change-in-production"
    JWT_ACCESS_TOKEN_EXPIRE_MINUTES: int = 480  # 8 hours for dev convenience

    # CORS (allow all for development)
    CORS_ORIGINS: list = ["*"]

    # Features (all enabled for testing)
    ENABLE_WEBSOCKETS: bool = True
    ENABLE_ANALYTICS: bool = True
    ENABLE_AI_ASSISTANT: bool = True

    # Development-specific
    RELOAD: bool = True
    WORKERS: int = 1

    class Config:
        env_file = ".env.development"
        env_file_encoding = "utf-8"

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

    # CORS (Issue #468: Replaced wildcard with explicit localhost origins)
    # Using explicit origins even in development for security
    CORS_ORIGINS: list = [
        "http://localhost:3000",
        "http://localhost:8000",
        "http://localhost:9001",
        "http://127.0.0.1:3000",
        "http://127.0.0.1:8000",
        "http://127.0.0.1:9001",
    ]

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

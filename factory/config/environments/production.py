# -*- coding: utf-8 -*-
"""
Production Settings - Issue #212
================================
Configuration for production environment.
"""

from typing import List
from .base import BaseSettings


class ProductionSettings(BaseSettings):
    """
    Production environment settings.

    Optimized for security, performance, and reliability.
    """

    ENVIRONMENT: str = "production"
    DEBUG: bool = False

    # Logging (minimal for performance)
    LOG_LEVEL: str = "WARNING"
    LOG_FORMAT: str = "json"

    # Database (PostgreSQL with connection pooling)
    DATABASE_URL: str = ""  # Must be set via environment

    # Redis (cluster mode recommended)
    REDIS_URL: str = ""  # Must be set via environment

    # Security (strict)
    JWT_SECRET_KEY: str = ""  # Must be set via environment
    JWT_ACCESS_TOKEN_EXPIRE_MINUTES: int = 30
    JWT_REFRESH_TOKEN_EXPIRE_DAYS: int = 7

    # CORS (production domains only)
    CORS_ORIGINS: List[str] = [
        "https://factory.example.com",
        "https://api.factory.example.com"
    ]

    # Features
    ENABLE_WEBSOCKETS: bool = True
    ENABLE_ANALYTICS: bool = True
    ENABLE_AI_ASSISTANT: bool = True

    # Observability
    SENTRY_DSN: str = ""  # Production Sentry project
    OTLP_ENDPOINT: str = ""

    # Server (optimized for production)
    WORKERS: int = 4
    RELOAD: bool = False

    # Security headers
    ENABLE_HSTS: bool = True
    ENABLE_XSS_PROTECTION: bool = True

    class Config:
        env_file = ".env.production"
        env_file_encoding = "utf-8"

    def validate_required(self) -> None:
        """Validate that required settings are configured."""
        required = ["DATABASE_URL", "REDIS_URL", "JWT_SECRET_KEY"]
        missing = [key for key in required if not getattr(self, key)]
        if missing:
            raise ValueError(f"Missing required production settings: {missing}")

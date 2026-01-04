# -*- coding: utf-8 -*-
"""
Sentry Error Tracking - Issue #209
==================================
Error tracking and performance monitoring with Sentry.

Features:
- Automatic error capture
- Performance tracing
- User context
- Release tracking

Usage:
    from factory.observability.sentry import setup_sentry

    # During app initialization
    setup_sentry(
        dsn="your-sentry-dsn",
        environment="production"
    )
"""

import os
import logging
from typing import Optional, Dict, Any

logger = logging.getLogger(__name__)

# Sentry imports (graceful fallback)
try:
    import sentry_sdk
    from sentry_sdk.integrations.fastapi import FastApiIntegration
    from sentry_sdk.integrations.sqlalchemy import SqlalchemyIntegration
    from sentry_sdk.integrations.redis import RedisIntegration
    from sentry_sdk.integrations.logging import LoggingIntegration
    HAS_SENTRY = True
except ImportError:
    HAS_SENTRY = False
    sentry_sdk = None
    logger.info("[Sentry] Sentry SDK not installed. Run: pip install sentry-sdk[fastapi]")


def setup_sentry(
    dsn: str = None,
    environment: str = None,
    release: str = None,
    traces_sample_rate: float = 0.1,
    profiles_sample_rate: float = 0.1,
    debug: bool = False
) -> bool:
    """
    Setup Sentry error tracking.

    Args:
        dsn: Sentry DSN (from environment SENTRY_DSN if not provided)
        environment: Environment name (dev, staging, prod)
        release: Release/version string
        traces_sample_rate: Performance tracing sample rate (0.0-1.0)
        profiles_sample_rate: Profiling sample rate (0.0-1.0)
        debug: Enable debug mode

    Returns:
        True if Sentry was configured successfully
    """
    if not HAS_SENTRY:
        logger.warning("[Sentry] Sentry SDK not available. Error tracking disabled.")
        return False

    # Get DSN from environment if not provided
    dsn = dsn or os.getenv("SENTRY_DSN")
    if not dsn:
        logger.info("[Sentry] No DSN configured. Error tracking disabled.")
        return False

    # Get environment
    environment = environment or os.getenv("ENVIRONMENT", "development")
    release = release or os.getenv("APP_VERSION", "1.0.0")

    try:
        sentry_sdk.init(
            dsn=dsn,
            environment=environment,
            release=f"plataforma-e@{release}",
            traces_sample_rate=traces_sample_rate,
            profiles_sample_rate=profiles_sample_rate,
            debug=debug,
            integrations=[
                FastApiIntegration(transaction_style="endpoint"),
                SqlalchemyIntegration(),
                RedisIntegration(),
                LoggingIntegration(
                    level=logging.INFO,
                    event_level=logging.ERROR
                )
            ],
            # Don't send PII by default
            send_default_pii=False,
            # Attach stacktrace to messages
            attach_stacktrace=True,
            # Filter sensitive data
            before_send=_filter_sensitive_data
        )

        logger.info(f"[Sentry] Error tracking enabled for {environment}")
        return True

    except Exception as e:
        logger.error(f"[Sentry] Failed to initialize: {e}")
        return False


def _filter_sensitive_data(event: Dict[str, Any], hint: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """
    Filter sensitive data before sending to Sentry.

    Removes passwords, tokens, and other sensitive information.
    """
    # List of sensitive keys to filter
    sensitive_keys = {
        "password", "token", "secret", "api_key", "apikey",
        "authorization", "auth", "credential", "jwt"
    }

    def filter_dict(d: Dict) -> Dict:
        if not isinstance(d, dict):
            return d

        filtered = {}
        for key, value in d.items():
            key_lower = key.lower()
            if any(s in key_lower for s in sensitive_keys):
                filtered[key] = "[FILTERED]"
            elif isinstance(value, dict):
                filtered[key] = filter_dict(value)
            elif isinstance(value, list):
                filtered[key] = [filter_dict(v) if isinstance(v, dict) else v for v in value]
            else:
                filtered[key] = value
        return filtered

    # Filter request data
    if "request" in event:
        if "data" in event["request"]:
            event["request"]["data"] = filter_dict(event["request"]["data"])
        if "headers" in event["request"]:
            event["request"]["headers"] = filter_dict(event["request"]["headers"])

    # Filter extra context
    if "extra" in event:
        event["extra"] = filter_dict(event["extra"])

    return event


def capture_exception(exception: Exception, **extra):
    """
    Capture an exception to Sentry.

    Args:
        exception: The exception to capture
        **extra: Additional context to attach
    """
    if HAS_SENTRY and sentry_sdk:
        with sentry_sdk.push_scope() as scope:
            for key, value in extra.items():
                scope.set_extra(key, value)
            sentry_sdk.capture_exception(exception)
    else:
        logger.error(f"Exception: {exception}", extra=extra)


def capture_message(message: str, level: str = "info", **extra):
    """
    Capture a message to Sentry.

    Args:
        message: The message to capture
        level: Log level (debug, info, warning, error, fatal)
        **extra: Additional context to attach
    """
    if HAS_SENTRY and sentry_sdk:
        with sentry_sdk.push_scope() as scope:
            for key, value in extra.items():
                scope.set_extra(key, value)
            sentry_sdk.capture_message(message, level=level)
    else:
        log_func = getattr(logger, level, logger.info)
        log_func(message, extra=extra)


def set_user_context(user_id: str, email: str = None, username: str = None, tenant_id: str = None):
    """
    Set user context for Sentry.

    Args:
        user_id: User identifier
        email: User email
        username: Username
        tenant_id: Tenant identifier
    """
    if HAS_SENTRY and sentry_sdk:
        sentry_sdk.set_user({
            "id": user_id,
            "email": email,
            "username": username,
            "tenant_id": tenant_id
        })


def set_tag(key: str, value: str):
    """Set a tag for the current scope."""
    if HAS_SENTRY and sentry_sdk:
        sentry_sdk.set_tag(key, value)


def add_breadcrumb(message: str, category: str = "custom", level: str = "info", data: Dict = None):
    """Add a breadcrumb for debugging."""
    if HAS_SENTRY and sentry_sdk:
        sentry_sdk.add_breadcrumb(
            message=message,
            category=category,
            level=level,
            data=data or {}
        )

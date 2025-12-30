# -*- coding: utf-8 -*-
"""
Unified Observability Setup - Issue #209
========================================
Unified setup for all observability components.

Usage:
    from factory.observability import setup_observability

    # During app initialization
    setup_observability(app)
"""

import os
import logging
from typing import Optional

from .tracing import setup_tracing
from .sentry import setup_sentry

logger = logging.getLogger(__name__)


def setup_observability(
    app,
    service_name: str = "fabrica-de-agentes",
    environment: str = None,
    sentry_dsn: str = None,
    otlp_endpoint: str = None,
    jaeger_host: str = None,
    enable_console_tracing: bool = False
) -> dict:
    """
    Setup all observability components.

    Args:
        app: FastAPI application
        service_name: Name of the service
        environment: Environment (dev, staging, prod)
        sentry_dsn: Sentry DSN for error tracking
        otlp_endpoint: OTLP collector endpoint
        jaeger_host: Jaeger agent host
        enable_console_tracing: Enable console tracing output

    Returns:
        Dict with status of each component
    """
    environment = environment or os.getenv("ENVIRONMENT", "development")

    results = {
        "environment": environment,
        "sentry": False,
        "tracing": False
    }

    # Setup Sentry error tracking
    try:
        sentry_enabled = setup_sentry(
            dsn=sentry_dsn,
            environment=environment
        )
        results["sentry"] = sentry_enabled
        if sentry_enabled:
            logger.info(f"[Observability] Sentry enabled for {environment}")
    except Exception as e:
        logger.warning(f"[Observability] Sentry setup failed: {e}")

    # Setup distributed tracing
    try:
        tracing_enabled = setup_tracing(
            app=app,
            service_name=service_name,
            environment=environment,
            otlp_endpoint=otlp_endpoint,
            jaeger_host=jaeger_host,
            console_export=enable_console_tracing
        )
        results["tracing"] = tracing_enabled
        if tracing_enabled:
            logger.info(f"[Observability] Tracing enabled for {service_name}")
    except Exception as e:
        logger.warning(f"[Observability] Tracing setup failed: {e}")

    # Summary
    enabled_count = sum(1 for v in results.values() if v is True)
    logger.info(f"[Observability] Setup complete: {enabled_count} components enabled")

    return results


def get_observability_status() -> dict:
    """
    Get current observability status.

    Returns:
        Dict with status of each component
    """
    from .tracing import HAS_OPENTELEMETRY, _tracer
    from .sentry import HAS_SENTRY

    return {
        "opentelemetry_available": HAS_OPENTELEMETRY,
        "opentelemetry_configured": _tracer is not None,
        "sentry_available": HAS_SENTRY
    }

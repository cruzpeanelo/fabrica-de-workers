# -*- coding: utf-8 -*-
"""
Observability Module - Issue #209
=================================
Distributed tracing, metrics, and error tracking for Plataforma E.

Components:
- tracing.py: OpenTelemetry distributed tracing
- sentry.py: Error tracking with Sentry
- setup.py: Unified observability setup

Usage:
    from factory.observability import setup_observability
    setup_observability(app)
"""

from .tracing import setup_tracing, get_tracer, trace_span
from .sentry import setup_sentry
from .setup import setup_observability

__all__ = [
    "setup_observability",
    "setup_tracing",
    "setup_sentry",
    "get_tracer",
    "trace_span"
]

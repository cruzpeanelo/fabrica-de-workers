# -*- coding: utf-8 -*-
"""
OpenTelemetry Distributed Tracing - Issue #209
==============================================
Distributed tracing for request tracking across services.

Features:
- Automatic instrumentation of FastAPI requests
- Manual span creation for custom operations
- Context propagation for distributed systems
- Export to Jaeger, Zipkin, or OTLP collectors

Usage:
    from factory.observability.tracing import setup_tracing, trace_span

    # Setup during app initialization
    setup_tracing(app, service_name="factory-api")

    # Manual tracing
    with trace_span("process_story") as span:
        span.set_attribute("story_id", story_id)
        result = process(story)
"""

import os
import logging
from typing import Optional, Any, Dict
from functools import wraps
from contextlib import contextmanager

logger = logging.getLogger(__name__)

# OpenTelemetry imports (graceful fallback if not installed)
try:
    from opentelemetry import trace
    from opentelemetry.sdk.trace import TracerProvider
    from opentelemetry.sdk.trace.export import BatchSpanProcessor, ConsoleSpanExporter
    from opentelemetry.sdk.resources import Resource, SERVICE_NAME
    from opentelemetry.instrumentation.fastapi import FastAPIInstrumentor
    from opentelemetry.instrumentation.requests import RequestsInstrumentor
    from opentelemetry.instrumentation.sqlalchemy import SQLAlchemyInstrumentor
    HAS_OPENTELEMETRY = True
except ImportError:
    HAS_OPENTELEMETRY = False
    logger.warning("[Tracing] OpenTelemetry not installed. Run: pip install opentelemetry-api opentelemetry-sdk opentelemetry-instrumentation-fastapi")

# OTLP exporter (optional)
try:
    from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
    HAS_OTLP = True
except ImportError:
    HAS_OTLP = False

# Jaeger exporter (optional)
try:
    from opentelemetry.exporter.jaeger.thrift import JaegerExporter
    HAS_JAEGER = True
except ImportError:
    HAS_JAEGER = False


# Global tracer
_tracer: Optional[Any] = None


def get_tracer(name: str = __name__) -> Any:
    """
    Get a tracer instance.

    Args:
        name: Name of the tracer (usually __name__)

    Returns:
        Tracer instance or NoopTracer if not configured
    """
    global _tracer
    if _tracer is None:
        if HAS_OPENTELEMETRY:
            _tracer = trace.get_tracer(name)
        else:
            _tracer = NoopTracer()
    return _tracer


class NoopSpan:
    """No-op span for when tracing is disabled."""
    def set_attribute(self, key: str, value: Any) -> None:
        pass
    def set_status(self, status: Any) -> None:
        pass
    def record_exception(self, exception: Exception) -> None:
        pass
    def end(self) -> None:
        pass
    def __enter__(self):
        return self
    def __exit__(self, *args):
        pass


class NoopTracer:
    """No-op tracer for when OpenTelemetry is not installed."""
    def start_span(self, name: str, **kwargs) -> NoopSpan:
        return NoopSpan()
    def start_as_current_span(self, name: str, **kwargs):
        return NoopSpan()


@contextmanager
def trace_span(name: str, attributes: Optional[Dict[str, Any]] = None):
    """
    Context manager for creating a traced span.

    Args:
        name: Name of the span
        attributes: Optional attributes to add to span

    Usage:
        with trace_span("process_worker", {"worker_id": "w1"}) as span:
            result = do_work()
            span.set_attribute("result_count", len(result))
    """
    tracer = get_tracer()

    if HAS_OPENTELEMETRY and not isinstance(tracer, NoopTracer):
        with tracer.start_as_current_span(name) as span:
            if attributes:
                for key, value in attributes.items():
                    span.set_attribute(key, value)
            try:
                yield span
            except Exception as e:
                span.record_exception(e)
                span.set_status(trace.Status(trace.StatusCode.ERROR, str(e)))
                raise
    else:
        yield NoopSpan()


def trace_function(name: Optional[str] = None):
    """
    Decorator to trace a function.

    Args:
        name: Optional span name (defaults to function name)

    Usage:
        @trace_function("process_story")
        async def process_story(story_id: str):
            ...
    """
    def decorator(func):
        span_name = name or func.__name__

        @wraps(func)
        async def async_wrapper(*args, **kwargs):
            with trace_span(span_name) as span:
                return await func(*args, **kwargs)

        @wraps(func)
        def sync_wrapper(*args, **kwargs):
            with trace_span(span_name) as span:
                return func(*args, **kwargs)

        import asyncio
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        return sync_wrapper

    return decorator


def setup_tracing(
    app,
    service_name: str = "fabrica-de-agentes",
    environment: str = None,
    otlp_endpoint: str = None,
    jaeger_host: str = None,
    jaeger_port: int = 6831,
    console_export: bool = False
) -> bool:
    """
    Setup OpenTelemetry distributed tracing.

    Args:
        app: FastAPI application
        service_name: Name of the service
        environment: Environment (dev, staging, prod)
        otlp_endpoint: OTLP collector endpoint (e.g., "localhost:4317")
        jaeger_host: Jaeger agent host
        jaeger_port: Jaeger agent port
        console_export: Enable console exporter for debugging

    Returns:
        True if tracing was configured successfully
    """
    if not HAS_OPENTELEMETRY:
        logger.warning("[Tracing] OpenTelemetry not available. Tracing disabled.")
        return False

    global _tracer

    # Get config from environment
    environment = environment or os.getenv("ENVIRONMENT", "development")
    otlp_endpoint = otlp_endpoint or os.getenv("OTLP_ENDPOINT")
    jaeger_host = jaeger_host or os.getenv("JAEGER_HOST")

    # Create resource
    resource = Resource.create({
        SERVICE_NAME: service_name,
        "service.environment": environment,
        "service.version": "1.0.0"
    })

    # Create tracer provider
    provider = TracerProvider(resource=resource)

    # Add exporters
    exporters_configured = 0

    # OTLP exporter (preferred)
    if otlp_endpoint and HAS_OTLP:
        try:
            otlp_exporter = OTLPSpanExporter(endpoint=otlp_endpoint, insecure=True)
            provider.add_span_processor(BatchSpanProcessor(otlp_exporter))
            logger.info(f"[Tracing] OTLP exporter configured: {otlp_endpoint}")
            exporters_configured += 1
        except Exception as e:
            logger.warning(f"[Tracing] Failed to configure OTLP exporter: {e}")

    # Jaeger exporter
    if jaeger_host and HAS_JAEGER:
        try:
            jaeger_exporter = JaegerExporter(
                agent_host_name=jaeger_host,
                agent_port=jaeger_port
            )
            provider.add_span_processor(BatchSpanProcessor(jaeger_exporter))
            logger.info(f"[Tracing] Jaeger exporter configured: {jaeger_host}:{jaeger_port}")
            exporters_configured += 1
        except Exception as e:
            logger.warning(f"[Tracing] Failed to configure Jaeger exporter: {e}")

    # Console exporter (for debugging)
    if console_export or (exporters_configured == 0 and environment == "development"):
        provider.add_span_processor(BatchSpanProcessor(ConsoleSpanExporter()))
        logger.info("[Tracing] Console exporter configured (development mode)")
        exporters_configured += 1

    # Set global tracer provider
    trace.set_tracer_provider(provider)
    _tracer = trace.get_tracer(service_name)

    # Instrument FastAPI
    try:
        FastAPIInstrumentor.instrument_app(app)
        logger.info("[Tracing] FastAPI instrumented")
    except Exception as e:
        logger.warning(f"[Tracing] Failed to instrument FastAPI: {e}")

    # Instrument requests library
    try:
        RequestsInstrumentor().instrument()
        logger.info("[Tracing] Requests library instrumented")
    except Exception as e:
        logger.debug(f"[Tracing] Requests instrumentation skipped: {e}")

    # Instrument SQLAlchemy
    try:
        from factory.database.connection import engine
        SQLAlchemyInstrumentor().instrument(engine=engine)
        logger.info("[Tracing] SQLAlchemy instrumented")
    except Exception as e:
        logger.debug(f"[Tracing] SQLAlchemy instrumentation skipped: {e}")

    logger.info(f"[Tracing] Distributed tracing enabled for {service_name} ({environment})")
    return True

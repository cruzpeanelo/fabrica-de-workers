# -*- coding: utf-8 -*-
"""
JSON Logging Configuration - Issue #206
=======================================
Structured JSON logging for stateless deployments.

Logs are sent to stdout in JSON format for collection by
Kubernetes/Fluentd/Loki.

Usage:
    from factory.logging_config import setup_logging

    setup_logging()
"""

import os
import sys
import logging
from typing import Optional
from datetime import datetime

# JSON formatter (graceful fallback)
try:
    from pythonjsonlogger import jsonlogger
    HAS_JSON_LOGGER = True
except ImportError:
    HAS_JSON_LOGGER = False


class CustomJsonFormatter(logging.Formatter):
    """Custom JSON formatter for structured logging."""

    def __init__(self, *args, **kwargs):
        self.service_name = kwargs.pop("service_name", "plataforma-e")
        self.environment = kwargs.pop("environment", os.getenv("ENVIRONMENT", "development"))
        super().__init__(*args, **kwargs)

    def format(self, record: logging.LogRecord) -> str:
        """Format log record as JSON."""
        import json

        log_data = {
            "timestamp": datetime.utcnow().isoformat() + "Z",
            "level": record.levelname,
            "logger": record.name,
            "message": record.getMessage(),
            "service": self.service_name,
            "environment": self.environment
        }

        # Add exception info
        if record.exc_info:
            log_data["exception"] = self.formatException(record.exc_info)

        # Add extra fields
        if hasattr(record, "request_id"):
            log_data["request_id"] = record.request_id
        if hasattr(record, "user_id"):
            log_data["user_id"] = record.user_id
        if hasattr(record, "tenant_id"):
            log_data["tenant_id"] = record.tenant_id

        # Add source location in debug mode
        if record.levelno <= logging.DEBUG:
            log_data["source"] = {
                "file": record.pathname,
                "line": record.lineno,
                "function": record.funcName
            }

        return json.dumps(log_data, ensure_ascii=False)


def setup_logging(
    level: str = None,
    service_name: str = "plataforma-e",
    json_format: bool = None
) -> None:
    """
    Setup structured logging.

    Args:
        level: Log level (DEBUG, INFO, WARNING, ERROR)
        service_name: Service name for log entries
        json_format: Force JSON format (auto-detected if None)
    """
    level = level or os.getenv("LOG_LEVEL", "INFO")
    environment = os.getenv("ENVIRONMENT", "development")

    # Auto-detect JSON format: use JSON in production, readable in dev
    if json_format is None:
        json_format = environment in ("production", "staging")

    # Root logger
    root_logger = logging.getLogger()
    root_logger.setLevel(getattr(logging, level.upper(), logging.INFO))

    # Remove existing handlers
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    # Create stdout handler
    handler = logging.StreamHandler(sys.stdout)

    if json_format:
        # JSON format for production
        if HAS_JSON_LOGGER:
            formatter = jsonlogger.JsonFormatter(
                fmt="%(timestamp)s %(level)s %(name)s %(message)s",
                rename_fields={"levelname": "level", "name": "logger"}
            )
        else:
            formatter = CustomJsonFormatter(service_name=service_name, environment=environment)
    else:
        # Readable format for development
        formatter = logging.Formatter(
            fmt="%(asctime)s | %(levelname)-8s | %(name)s | %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        )

    handler.setFormatter(formatter)
    root_logger.addHandler(handler)

    # Reduce noise from libraries
    logging.getLogger("urllib3").setLevel(logging.WARNING)
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)
    logging.getLogger("uvicorn.access").setLevel(logging.WARNING)

    logger = logging.getLogger(__name__)
    logger.info(f"Logging configured: level={level}, json={json_format}, env={environment}")


class LogContext:
    """
    Context manager for adding request context to logs.

    Usage:
        with LogContext(request_id="abc123", user_id="user1"):
            logger.info("Processing request")
    """

    def __init__(self, **kwargs):
        self.context = kwargs
        self._old_factory = None

    def __enter__(self):
        self._old_factory = logging.getLogRecordFactory()

        def record_factory(*args, **kwargs):
            record = self._old_factory(*args, **kwargs)
            for key, value in self.context.items():
                setattr(record, key, value)
            return record

        logging.setLogRecordFactory(record_factory)
        return self

    def __exit__(self, *args):
        logging.setLogRecordFactory(self._old_factory)


def get_request_logger(request_id: str = None, user_id: str = None, tenant_id: str = None):
    """
    Get a logger with request context.

    Usage:
        logger = get_request_logger(request_id="abc123")
        logger.info("Processing request")
    """
    import uuid

    logger = logging.getLogger("request")

    class ContextAdapter(logging.LoggerAdapter):
        def process(self, msg, kwargs):
            extra = kwargs.get("extra", {})
            extra.update(self.extra)
            kwargs["extra"] = extra
            return msg, kwargs

    context = {
        "request_id": request_id or str(uuid.uuid4())[:8],
        "user_id": user_id,
        "tenant_id": tenant_id
    }

    return ContextAdapter(logger, context)

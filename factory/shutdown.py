# -*- coding: utf-8 -*-
"""
Graceful Shutdown Handler - Issue #206
======================================
Handles graceful shutdown for stateless deployments.

Usage:
    from factory.shutdown import setup_graceful_shutdown

    @app.on_event("startup")
    async def startup():
        setup_graceful_shutdown(app)
"""

import os
import signal
import logging
import asyncio
from typing import Callable, List, Optional
from contextlib import asynccontextmanager

logger = logging.getLogger(__name__)

# Global shutdown state
_shutdown_handlers: List[Callable] = []
_is_shutting_down = False


def is_shutting_down() -> bool:
    """Check if the application is shutting down."""
    return _is_shutting_down


def register_shutdown_handler(handler: Callable) -> None:
    """
    Register a shutdown handler.

    Args:
        handler: Async or sync function to call on shutdown
    """
    _shutdown_handlers.append(handler)
    logger.debug(f"[Shutdown] Registered handler: {handler.__name__}")


async def _execute_shutdown_handlers() -> None:
    """Execute all registered shutdown handlers."""
    global _is_shutting_down
    _is_shutting_down = True

    logger.info(f"[Shutdown] Executing {len(_shutdown_handlers)} shutdown handlers...")

    for handler in reversed(_shutdown_handlers):
        try:
            if asyncio.iscoroutinefunction(handler):
                await handler()
            else:
                handler()
            logger.debug(f"[Shutdown] Handler {handler.__name__} completed")
        except Exception as e:
            logger.error(f"[Shutdown] Handler {handler.__name__} failed: {e}")

    logger.info("[Shutdown] All handlers completed")


def setup_graceful_shutdown(app) -> None:
    """
    Setup graceful shutdown for FastAPI app.

    Args:
        app: FastAPI application instance
    """
    # Get shutdown timeout from environment
    shutdown_timeout = int(os.getenv("SHUTDOWN_TIMEOUT", "30"))

    async def shutdown_event():
        """Handle shutdown event."""
        logger.info(f"[Shutdown] Initiating graceful shutdown (timeout: {shutdown_timeout}s)")
        await _execute_shutdown_handlers()

    # Register FastAPI shutdown event
    app.add_event_handler("shutdown", shutdown_event)

    # Register signal handlers for Kubernetes
    def signal_handler(signum, frame):
        logger.info(f"[Shutdown] Received signal {signum}")
        global _is_shutting_down
        _is_shutting_down = True

    try:
        signal.signal(signal.SIGTERM, signal_handler)
        signal.signal(signal.SIGINT, signal_handler)
        logger.info("[Shutdown] Signal handlers registered (SIGTERM, SIGINT)")
    except Exception as e:
        logger.debug(f"[Shutdown] Could not register signal handlers: {e}")

    # Register default handlers
    register_shutdown_handler(_close_database_connections)
    register_shutdown_handler(_close_redis_connections)
    register_shutdown_handler(_flush_logs)

    logger.info("[Shutdown] Graceful shutdown configured")


async def _close_database_connections() -> None:
    """Close database connections."""
    try:
        from factory.database.connection import engine
        if engine:
            engine.dispose()
            logger.info("[Shutdown] Database connections closed")
    except Exception as e:
        logger.debug(f"[Shutdown] Database cleanup skipped: {e}")


async def _close_redis_connections() -> None:
    """Close Redis connections."""
    try:
        from factory.cache import get_cache
        cache = get_cache()
        if cache._redis:
            cache._redis.close()
            logger.info("[Shutdown] Redis connections closed")
    except Exception as e:
        logger.debug(f"[Shutdown] Redis cleanup skipped: {e}")


async def _flush_logs() -> None:
    """Flush log handlers."""
    try:
        for handler in logging.root.handlers:
            handler.flush()
        logger.info("[Shutdown] Logs flushed")
    except Exception as e:
        logger.debug(f"[Shutdown] Log flush skipped: {e}")


@asynccontextmanager
async def lifespan(app):
    """
    FastAPI lifespan context manager.

    Usage:
        from factory.shutdown import lifespan

        app = FastAPI(lifespan=lifespan)
    """
    # Startup
    logger.info("[Lifecycle] Application starting...")
    setup_graceful_shutdown(app)

    yield

    # Shutdown
    logger.info("[Lifecycle] Application shutting down...")
    await _execute_shutdown_handlers()

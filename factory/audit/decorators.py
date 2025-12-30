# -*- coding: utf-8 -*-
"""
Audit Decorators - Issue #339
=============================
Decorators for automatic audit logging.
"""

import logging
import functools
import asyncio
from typing import Optional, Callable, Any

logger = logging.getLogger(__name__)


def audit_log(
    action: str,
    resource_type: str,
    resource_id_param: str = None,
    include_request: bool = True,
    log_result: bool = False
):
    """
    Decorator to automatically log function calls to audit log.

    Usage:
        @audit_log(action="CREATE", resource_type="story")
        async def create_story(story_data: dict):
            ...

        @audit_log(action="UPDATE", resource_type="story", resource_id_param="story_id")
        async def update_story(story_id: str, data: dict):
            ...

    Args:
        action: Action type (CREATE, UPDATE, DELETE, etc)
        resource_type: Type of resource being modified
        resource_id_param: Name of parameter containing resource ID
        include_request: Include request context (requires FastAPI Request)
        log_result: Log the function result in new_value
    """
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        async def async_wrapper(*args, **kwargs):
            from .service import log_action

            # Extract resource_id from parameters
            resource_id = None
            if resource_id_param:
                resource_id = kwargs.get(resource_id_param)
                if resource_id is None and args:
                    # Try to get from positional args
                    import inspect
                    sig = inspect.signature(func)
                    params = list(sig.parameters.keys())
                    if resource_id_param in params:
                        idx = params.index(resource_id_param)
                        if idx < len(args):
                            resource_id = args[idx]

            # Extract request context
            request_context = {}
            if include_request:
                request = kwargs.get("request")
                if request:
                    request_context = {
                        "ip_address": getattr(request.client, "host", None) if request.client else None,
                        "user_agent": request.headers.get("user-agent"),
                        "request_path": str(request.url.path),
                        "request_method": request.method,
                        "request_id": request.headers.get("x-request-id")
                    }

            # Extract user context from request state
            user_context = {}
            if include_request:
                request = kwargs.get("request")
                if request and hasattr(request, "state"):
                    user_context = {
                        "user_id": getattr(request.state, "user_id", None),
                        "tenant_id": getattr(request.state, "tenant_id", None),
                        "username": getattr(request.state, "username", None)
                    }

            # Execute function
            status = "success"
            error_message = None
            result = None

            try:
                result = await func(*args, **kwargs)
                return result

            except Exception as e:
                status = "error"
                error_message = str(e)
                raise

            finally:
                # Log the action
                try:
                    new_value = None
                    if log_result and result is not None:
                        if hasattr(result, "dict"):
                            new_value = result.dict()
                        elif isinstance(result, dict):
                            new_value = result

                    log_action(
                        action=action,
                        resource_type=resource_type,
                        resource_id=str(resource_id) if resource_id else None,
                        new_value=new_value,
                        status=status,
                        error_message=error_message,
                        **user_context,
                        **request_context
                    )
                except Exception as log_error:
                    logger.error(f"[Audit] Failed to log action: {log_error}")

        @functools.wraps(func)
        def sync_wrapper(*args, **kwargs):
            from .service import log_action

            # Extract resource_id
            resource_id = kwargs.get(resource_id_param) if resource_id_param else None

            status = "success"
            error_message = None
            result = None

            try:
                result = func(*args, **kwargs)
                return result

            except Exception as e:
                status = "error"
                error_message = str(e)
                raise

            finally:
                try:
                    log_action(
                        action=action,
                        resource_type=resource_type,
                        resource_id=str(resource_id) if resource_id else None,
                        status=status,
                        error_message=error_message
                    )
                except Exception as log_error:
                    logger.error(f"[Audit] Failed to log action: {log_error}")

        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        return sync_wrapper

    return decorator

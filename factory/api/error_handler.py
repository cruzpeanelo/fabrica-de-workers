# -*- coding: utf-8 -*-
"""
Safe Error Handler - Plataforma E v6.5
=============================================

Issue #172 - Generic exception handling exposes database errors.

This module provides safe error handling that:
1. Logs full error details for debugging
2. Returns sanitized messages to users (no sensitive info)
3. Maps database errors to user-friendly messages
4. Prevents information disclosure attacks

Usage:
    from factory.api.error_handler import SafeErrorHandler

    try:
        # ... code that might fail ...
    except Exception as e:
        raise HTTPException(500, SafeErrorHandler.sanitize_error(e))
"""

import logging
from typing import Dict, Optional

# Configure logger for this module
logger = logging.getLogger(__name__)


class SafeErrorHandler:
    """
    Handles exceptions safely by logging full details and returning sanitized messages.

    Prevents exposure of:
    - Database schema information
    - SQL queries and errors
    - Internal file paths
    - Stack traces
    - Configuration details
    """

    # Safe messages for common database errors
    SAFE_MESSAGES: Dict[str, str] = {
        # SQLAlchemy/Database errors
        "IntegrityError": "A database constraint was violated",
        "OperationalError": "Database operation failed",
        "ProgrammingError": "Invalid database operation",
        "DataError": "Invalid data format",
        "DatabaseError": "Database error occurred",
        "InternalError": "Internal database error",
        "NotSupportedError": "Operation not supported",
        "InterfaceError": "Database interface error",

        # Connection errors
        "ConnectionError": "Connection failed",
        "TimeoutError": "Operation timed out",
        "ConnectionRefusedError": "Service unavailable",
        "ConnectionResetError": "Connection was reset",

        # Authentication/Authorization errors
        "AuthenticationError": "Authentication failed",
        "PermissionError": "Permission denied",
        "AuthorizationError": "Authorization failed",

        # Validation errors
        "ValidationError": "Invalid input data",
        "ValueError": "Invalid value provided",
        "TypeError": "Invalid data type",
        "KeyError": "Required field missing",
        "AttributeError": "Invalid attribute",

        # File system errors
        "FileNotFoundError": "Resource not found",
        "IOError": "I/O operation failed",
        "OSError": "System operation failed",

        # JSON/Serialization errors
        "JSONDecodeError": "Invalid JSON format",
        "SerializationError": "Data serialization failed",

        # HTTP/Network errors
        "HTTPError": "HTTP request failed",
        "RequestException": "Request failed",

        # Redis errors
        "RedisError": "Cache operation failed",
        "ConnectionPoolError": "Connection pool exhausted",

        # General errors
        "RuntimeError": "Runtime error occurred",
        "Exception": "An unexpected error occurred",
    }

    # Patterns that indicate sensitive information (should be redacted)
    SENSITIVE_PATTERNS = [
        "password",
        "secret",
        "token",
        "api_key",
        "apikey",
        "credential",
        "auth",
        "private",
        "key=",
        "pwd",
    ]

    @staticmethod
    def sanitize_error(e: Exception, include_type: bool = False) -> str:
        """
        Sanitize an exception to return a safe message to users.

        Args:
            e: The exception to sanitize
            include_type: If True, include the error type in the message

        Returns:
            A safe, user-friendly error message
        """
        error_type = type(e).__name__
        error_message = str(e)

        # Log full error for debugging (with stack trace)
        logger.error(
            f"Error: {error_type}: {error_message}",
            exc_info=True,
            extra={
                "error_type": error_type,
                "error_message": error_message,
            }
        )

        # Check if the error type has a safe message
        safe_message = SafeErrorHandler.SAFE_MESSAGES.get(error_type)

        if safe_message:
            if include_type:
                return f"{safe_message} ({error_type})"
            return safe_message

        # Check parent classes for known error types
        for base_class in type(e).__mro__:
            base_name = base_class.__name__
            if base_name in SafeErrorHandler.SAFE_MESSAGES:
                safe_message = SafeErrorHandler.SAFE_MESSAGES[base_name]
                if include_type:
                    return f"{safe_message} ({error_type})"
                return safe_message

        # Default safe message
        return "An unexpected error occurred"

    @staticmethod
    def sanitize_error_with_context(
        e: Exception,
        context: str,
        include_type: bool = False
    ) -> str:
        """
        Sanitize an exception with additional context.

        Args:
            e: The exception to sanitize
            context: Additional context (e.g., "while creating user")
            include_type: If True, include the error type in the message

        Returns:
            A safe, user-friendly error message with context
        """
        base_message = SafeErrorHandler.sanitize_error(e, include_type)

        # Ensure context doesn't contain sensitive information
        safe_context = SafeErrorHandler._redact_sensitive(context)

        return f"{base_message} {safe_context}"

    @staticmethod
    def _redact_sensitive(text: str) -> str:
        """
        Redact sensitive information from text.

        Args:
            text: Text that might contain sensitive information

        Returns:
            Text with sensitive information redacted
        """
        text_lower = text.lower()
        for pattern in SafeErrorHandler.SENSITIVE_PATTERNS:
            if pattern in text_lower:
                return "[redacted]"
        return text

    @staticmethod
    def get_safe_message(error_type: str) -> str:
        """
        Get the safe message for a specific error type.

        Args:
            error_type: The exception class name

        Returns:
            The safe message for that error type
        """
        return SafeErrorHandler.SAFE_MESSAGES.get(
            error_type,
            "An unexpected error occurred"
        )

    @staticmethod
    def is_database_error(e: Exception) -> bool:
        """
        Check if an exception is a database-related error.

        Args:
            e: The exception to check

        Returns:
            True if it's a database error
        """
        db_error_types = {
            "IntegrityError",
            "OperationalError",
            "ProgrammingError",
            "DataError",
            "DatabaseError",
            "InternalError",
            "NotSupportedError",
            "InterfaceError",
        }

        error_type = type(e).__name__
        if error_type in db_error_types:
            return True

        # Check parent classes
        for base_class in type(e).__mro__:
            if base_class.__name__ in db_error_types:
                return True

        return False

    @staticmethod
    def is_auth_error(e: Exception) -> bool:
        """
        Check if an exception is an authentication/authorization error.

        Args:
            e: The exception to check

        Returns:
            True if it's an auth error
        """
        auth_error_types = {
            "AuthenticationError",
            "PermissionError",
            "AuthorizationError",
        }

        error_type = type(e).__name__
        return error_type in auth_error_types


# Convenience function for quick usage
def sanitize_error(e: Exception) -> str:
    """
    Convenience function to sanitize an exception.

    Args:
        e: The exception to sanitize

    Returns:
        A safe, user-friendly error message
    """
    return SafeErrorHandler.sanitize_error(e)

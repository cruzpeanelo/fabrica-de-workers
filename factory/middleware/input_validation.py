# -*- coding: utf-8 -*-
"""
Input Validation Middleware - Issue #357
========================================
Centralized input validation and sanitization.

Features:
- Content-Type validation
- Payload size limits
- XSS/SQL injection prevention
- JSON schema validation
- Suspicious input logging
"""

import re
import json
import html
import logging
from typing import Optional, List, Dict, Any, Set, Callable
from dataclasses import dataclass, field
from datetime import datetime

from fastapi import Request, HTTPException
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware

logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Maximum payload sizes (bytes)
MAX_JSON_SIZE = 1 * 1024 * 1024  # 1 MB
MAX_FORM_SIZE = 10 * 1024 * 1024  # 10 MB
MAX_FILE_SIZE = 50 * 1024 * 1024  # 50 MB

# Dangerous patterns to detect
SQL_INJECTION_PATTERNS = [
    r"(\b(SELECT|INSERT|UPDATE|DELETE|DROP|UNION|ALTER|CREATE|TRUNCATE)\b.*\b(FROM|INTO|WHERE|TABLE)\b)",
    r"(--)|(;--)|(/\*)",
    r"(\bOR\b\s+\d+\s*=\s*\d+)",
    r"(\bAND\b\s+\d+\s*=\s*\d+)",
    r"('\s*OR\s*')",
    r"(WAITFOR\s+DELAY)",
    r"(BENCHMARK\s*\()",
]

XSS_PATTERNS = [
    r"<script[^>]*>.*?</script>",
    r"javascript\s*:",
    r"on\w+\s*=",
    r"<iframe[^>]*>",
    r"<object[^>]*>",
    r"<embed[^>]*>",
    r"<svg[^>]*onload",
    r"expression\s*\(",
    r"vbscript\s*:",
]

PATH_TRAVERSAL_PATTERNS = [
    r"\.\./",
    r"\.\.\\",
    r"%2e%2e%2f",
    r"%2e%2e/",
    r"\.%2e/",
]


# =============================================================================
# MODELS
# =============================================================================

@dataclass
class ValidationConfig:
    """Input validation configuration."""
    max_json_size: int = MAX_JSON_SIZE
    max_form_size: int = MAX_FORM_SIZE
    max_string_length: int = 10000
    max_array_length: int = 1000
    max_nested_depth: int = 10
    check_sql_injection: bool = True
    check_xss: bool = True
    check_path_traversal: bool = True
    sanitize_html: bool = True
    log_suspicious: bool = True
    block_suspicious: bool = False  # Set True in production


@dataclass
class ValidationResult:
    """Result of input validation."""
    is_valid: bool
    sanitized_data: Any = None
    warnings: List[str] = field(default_factory=list)
    blocked_reason: Optional[str] = None


@dataclass
class SuspiciousInput:
    """Record of suspicious input attempt."""
    timestamp: datetime
    client_ip: str
    path: str
    pattern_type: str
    matched_pattern: str
    input_sample: str


# =============================================================================
# SUSPICIOUS INPUT LOG
# =============================================================================

_suspicious_inputs: List[SuspiciousInput] = []


def log_suspicious_input(
    client_ip: str,
    path: str,
    pattern_type: str,
    matched_pattern: str,
    input_sample: str
):
    """Log a suspicious input attempt."""
    record = SuspiciousInput(
        timestamp=datetime.utcnow(),
        client_ip=client_ip,
        path=path,
        pattern_type=pattern_type,
        matched_pattern=matched_pattern,
        input_sample=input_sample[:200]  # Truncate for safety
    )
    _suspicious_inputs.append(record)

    # Keep only last 10000 records
    if len(_suspicious_inputs) > 10000:
        _suspicious_inputs.pop(0)

    logger.warning(
        f"Suspicious input detected: {pattern_type} from {client_ip} on {path}"
    )


def get_suspicious_inputs(
    limit: int = 100,
    pattern_type: Optional[str] = None
) -> List[Dict[str, Any]]:
    """Get suspicious input log."""
    inputs = _suspicious_inputs
    if pattern_type:
        inputs = [i for i in inputs if i.pattern_type == pattern_type]

    return [
        {
            "timestamp": i.timestamp.isoformat(),
            "client_ip": i.client_ip,
            "path": i.path,
            "pattern_type": i.pattern_type,
            "matched_pattern": i.matched_pattern,
            "input_sample": i.input_sample
        }
        for i in inputs[-limit:]
    ]


# =============================================================================
# INPUT VALIDATOR
# =============================================================================

class InputValidator:
    """
    Validates and sanitizes input data.
    """

    def __init__(self, config: Optional[ValidationConfig] = None):
        self.config = config or ValidationConfig()
        self._sql_patterns = [re.compile(p, re.IGNORECASE) for p in SQL_INJECTION_PATTERNS]
        self._xss_patterns = [re.compile(p, re.IGNORECASE | re.DOTALL) for p in XSS_PATTERNS]
        self._path_patterns = [re.compile(p, re.IGNORECASE) for p in PATH_TRAVERSAL_PATTERNS]

    def validate(
        self,
        data: Any,
        client_ip: str = "unknown",
        path: str = "unknown"
    ) -> ValidationResult:
        """
        Validate and sanitize input data.

        Returns ValidationResult with sanitized data.
        """
        warnings = []
        blocked_reason = None

        try:
            sanitized = self._sanitize_value(
                data,
                warnings,
                client_ip,
                path,
                depth=0
            )
        except ValueError as e:
            return ValidationResult(
                is_valid=False,
                blocked_reason=str(e)
            )

        # Check if we should block
        if self.config.block_suspicious and warnings:
            blocked_reason = f"Suspicious input detected: {warnings[0]}"
            return ValidationResult(
                is_valid=False,
                warnings=warnings,
                blocked_reason=blocked_reason
            )

        return ValidationResult(
            is_valid=True,
            sanitized_data=sanitized,
            warnings=warnings
        )

    def _sanitize_value(
        self,
        value: Any,
        warnings: List[str],
        client_ip: str,
        path: str,
        depth: int
    ) -> Any:
        """Recursively sanitize a value."""
        # Check depth
        if depth > self.config.max_nested_depth:
            raise ValueError(f"Exceeded maximum nesting depth: {self.config.max_nested_depth}")

        if value is None:
            return None

        if isinstance(value, bool):
            return value

        if isinstance(value, (int, float)):
            return value

        if isinstance(value, str):
            return self._sanitize_string(value, warnings, client_ip, path)

        if isinstance(value, list):
            if len(value) > self.config.max_array_length:
                raise ValueError(f"Array exceeds maximum length: {self.config.max_array_length}")
            return [
                self._sanitize_value(item, warnings, client_ip, path, depth + 1)
                for item in value
            ]

        if isinstance(value, dict):
            return {
                self._sanitize_string(k, warnings, client_ip, path):
                self._sanitize_value(v, warnings, client_ip, path, depth + 1)
                for k, v in value.items()
            }

        # Unknown type - return as string
        return str(value)

    def _sanitize_string(
        self,
        value: str,
        warnings: List[str],
        client_ip: str,
        path: str
    ) -> str:
        """Sanitize a string value."""
        # Check length
        if len(value) > self.config.max_string_length:
            raise ValueError(f"String exceeds maximum length: {self.config.max_string_length}")

        # Check for SQL injection
        if self.config.check_sql_injection:
            for pattern in self._sql_patterns:
                if pattern.search(value):
                    warning = f"Potential SQL injection detected"
                    warnings.append(warning)
                    log_suspicious_input(
                        client_ip, path, "sql_injection",
                        pattern.pattern, value
                    )
                    break

        # Check for XSS
        if self.config.check_xss:
            for pattern in self._xss_patterns:
                if pattern.search(value):
                    warning = f"Potential XSS detected"
                    warnings.append(warning)
                    log_suspicious_input(
                        client_ip, path, "xss",
                        pattern.pattern, value
                    )
                    break

        # Check for path traversal
        if self.config.check_path_traversal:
            for pattern in self._path_patterns:
                if pattern.search(value):
                    warning = f"Potential path traversal detected"
                    warnings.append(warning)
                    log_suspicious_input(
                        client_ip, path, "path_traversal",
                        pattern.pattern, value
                    )
                    break

        # Sanitize HTML if enabled
        if self.config.sanitize_html:
            value = html.escape(value)

        return value


# =============================================================================
# MIDDLEWARE
# =============================================================================

class InputValidationMiddleware(BaseHTTPMiddleware):
    """
    Middleware for input validation on all requests.
    """

    def __init__(
        self,
        app,
        config: Optional[ValidationConfig] = None,
        excluded_paths: Optional[List[str]] = None
    ):
        super().__init__(app)
        self.config = config or ValidationConfig()
        self.validator = InputValidator(self.config)
        self.excluded_paths = excluded_paths or [
            "/docs",
            "/redoc",
            "/openapi.json",
            "/health",
            "/api/upload",  # File uploads handled separately
        ]

    async def dispatch(self, request: Request, call_next: Callable):
        """Process the request."""
        path = request.url.path

        # Skip excluded paths
        if self._is_excluded(path):
            return await call_next(request)

        # Check content length
        content_length = request.headers.get("content-length")
        if content_length:
            try:
                size = int(content_length)
                content_type = request.headers.get("content-type", "")

                if "application/json" in content_type and size > self.config.max_json_size:
                    return JSONResponse(
                        status_code=413,
                        content={
                            "error": "payload_too_large",
                            "message": f"JSON payload exceeds {self.config.max_json_size} bytes"
                        }
                    )
            except ValueError:
                pass

        # Get client IP
        client_ip = self._get_client_ip(request)

        # For JSON requests, validate body
        content_type = request.headers.get("content-type", "")
        if "application/json" in content_type and request.method in ["POST", "PUT", "PATCH"]:
            try:
                body = await request.body()
                if body:
                    data = json.loads(body)
                    result = self.validator.validate(data, client_ip, path)

                    if not result.is_valid:
                        return JSONResponse(
                            status_code=400,
                            content={
                                "error": "invalid_input",
                                "message": result.blocked_reason or "Input validation failed",
                                "warnings": result.warnings
                            }
                        )

                    # Store sanitized data in request state
                    request.state.sanitized_body = result.sanitized_data
                    request.state.input_warnings = result.warnings

            except json.JSONDecodeError:
                return JSONResponse(
                    status_code=400,
                    content={
                        "error": "invalid_json",
                        "message": "Request body is not valid JSON"
                    }
                )

        # Validate query parameters
        if request.query_params:
            query_dict = dict(request.query_params)
            result = self.validator.validate(query_dict, client_ip, path)

            if not result.is_valid:
                return JSONResponse(
                    status_code=400,
                    content={
                        "error": "invalid_query",
                        "message": result.blocked_reason or "Query parameter validation failed"
                    }
                )

        return await call_next(request)

    def _is_excluded(self, path: str) -> bool:
        """Check if path is excluded."""
        for excluded in self.excluded_paths:
            if path.startswith(excluded):
                return True
        return False

    def _get_client_ip(self, request: Request) -> str:
        """Get client IP address."""
        forwarded_for = request.headers.get("X-Forwarded-For")
        if forwarded_for:
            return forwarded_for.split(",")[0].strip()
        return request.client.host if request.client else "unknown"


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

def sanitize_string(value: str, escape_html: bool = True) -> str:
    """
    Sanitize a single string value.

    Utility function for use outside middleware.
    """
    validator = InputValidator()
    result = validator.validate(value)
    return result.sanitized_data if result.is_valid else ""


def check_for_injection(value: str) -> Dict[str, bool]:
    """
    Check a string for injection patterns.

    Returns dict with detected threats.
    """
    validator = InputValidator()
    result = validator.validate(value)

    return {
        "sql_injection": any("SQL" in w for w in result.warnings),
        "xss": any("XSS" in w for w in result.warnings),
        "path_traversal": any("path" in w for w in result.warnings),
        "is_clean": len(result.warnings) == 0
    }

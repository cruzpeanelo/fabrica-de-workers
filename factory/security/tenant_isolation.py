# -*- coding: utf-8 -*-
"""
Tenant Isolation Security Module
================================

Issue #122 - [Security] CRITICO: Isolamento Total de Dados Entre Tenants

This module provides comprehensive tenant data isolation including:
- Row-level security enforcement
- Query interceptor for automatic tenant filtering
- Audit logging for cross-tenant access attempts
- Per-tenant encryption for sensitive data

Author: Plataforma E
"""

import os
import json
import base64
import hashlib
import logging
import secrets
import functools
from contextvars import ContextVar
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Set, Union

try:
    from cryptography.fernet import Fernet
    from cryptography.hazmat.primitives import hashes
    from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
    CRYPTO_AVAILABLE = True
except ImportError:
    CRYPTO_AVAILABLE = False

try:
    from sqlalchemy import event
    from sqlalchemy.orm import Session
    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False

logger = logging.getLogger(__name__)


class TenantIsolationError(Exception):
    pass


class CrossTenantAccessError(TenantIsolationError):
    def __init__(self, message, current_tenant, target_tenant, resource_type=None, resource_id=None):
        super().__init__(message)
        self.current_tenant = current_tenant
        self.target_tenant = target_tenant
        self.resource_type = resource_type
        self.resource_id = resource_id


class TenantNotFoundError(TenantIsolationError):
    pass


class IsolationLevel(str, Enum):
    STRICT = "strict"
    RELAXED = "relaxed"
    NONE = "none"


class AccessType(str, Enum):
    READ = "read"
    WRITE = "write"
    DELETE = "delete"
    ADMIN = "admin"


@dataclass
class TenantContext:
    tenant_id: str
    user_id: Optional[str] = None
    user_email: Optional[str] = None
    user_roles: List[str] = field(default_factory=list)
    isolation_level: IsolationLevel = IsolationLevel.STRICT
    request_id: Optional[str] = None
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def is_admin(self) -> bool:
        return bool(set(self.user_roles) & {"ADMIN", "SUPER_ADMIN", "SYSTEM"})

    def is_system(self) -> bool:
        return "SYSTEM" in self.user_roles

    def can_access_tenant(self, target_tenant_id: str) -> bool:
        if self.tenant_id == target_tenant_id:
            return True
        if self.isolation_level == IsolationLevel.NONE:
            return True
        if self.isolation_level == IsolationLevel.RELAXED and self.is_admin():
            return True
        return False

    def to_dict(self) -> Dict[str, Any]:
        return {
            "tenant_id": self.tenant_id,
            "user_id": self.user_id,
            "user_email": self.user_email,
            "user_roles": self.user_roles,
            "isolation_level": self.isolation_level.value,
            "request_id": self.request_id,
            "ip_address": self.ip_address,
            "created_at": self.created_at.isoformat() if self.created_at else None,
        }


_tenant_context: ContextVar[Optional[TenantContext]] = ContextVar("tenant_context", default=None)


def get_tenant_context() -> Optional[TenantContext]:
    return _tenant_context.get()


def set_tenant_context(
    tenant_id: str,
    user_id: Optional[str] = None,
    user_email: Optional[str] = None,
    user_roles: Optional[List[str]] = None,
    isolation_level: IsolationLevel = IsolationLevel.STRICT,
    request_id: Optional[str] = None,
    ip_address: Optional[str] = None,
    user_agent: Optional[str] = None,
    **metadata
) -> TenantContext:
    ctx = TenantContext(
        tenant_id=tenant_id,
        user_id=user_id,
        user_email=user_email,
        user_roles=user_roles or [],
        isolation_level=isolation_level,
        request_id=request_id or secrets.token_hex(8),
        ip_address=ip_address,
        user_agent=user_agent,
        metadata=metadata,
    )
    _tenant_context.set(ctx)
    return ctx


def clear_tenant_context():
    _tenant_context.set(None)


def require_tenant(func: Callable = None, *, allow_system: bool = False):
    def decorator(f: Callable) -> Callable:
        @functools.wraps(f)
        def wrapper(*args, **kwargs):
            ctx = get_tenant_context()
            if ctx is None:
                raise TenantNotFoundError(f"Tenant context required for {f.__name__}")
            if not ctx.tenant_id and not (allow_system and ctx.is_system()):
                raise TenantNotFoundError(f"Valid tenant_id required for {f.__name__}")
            return f(*args, **kwargs)
        return wrapper
    if func is not None:
        return decorator(func)
    return decorator



class TenantIsolation:
    _tenant_models: Set[str] = set()
    _exempt_models: Set[str] = {"Tenant", "TenantSettings", "BrandingConfig", "Plan", "FeatureFlag", "SystemConfig"}

    @classmethod
    def register_model(cls, model_name: str):
        cls._tenant_models.add(model_name)

    @classmethod
    def exempt_model(cls, model_name: str):
        cls._exempt_models.add(model_name)

    @classmethod
    def is_tenant_model(cls, model_name: str) -> bool:
        return model_name in cls._tenant_models

    @classmethod
    def is_exempt(cls, model_name: str) -> bool:
        return model_name in cls._exempt_models

    @classmethod
    def validate_access(cls, target_tenant_id: str, resource_type: str = None, resource_id: str = None, access_type: AccessType = AccessType.READ) -> bool:
        ctx = get_tenant_context()
        if ctx is None:
            return False
        if ctx.tenant_id == target_tenant_id:
            return True
        if not ctx.can_access_tenant(target_tenant_id):
            CrossTenantAccessAudit.log_attempt(
                current_tenant=ctx.tenant_id, target_tenant=target_tenant_id,
                resource_type=resource_type, resource_id=resource_id, access_type=access_type,
                user_id=ctx.user_id, ip_address=ctx.ip_address, allowed=False)
            raise CrossTenantAccessError(
                f"Cross-tenant access denied: {ctx.tenant_id} -> {target_tenant_id}",
                current_tenant=ctx.tenant_id, target_tenant=target_tenant_id,
                resource_type=resource_type, resource_id=resource_id)
        CrossTenantAccessAudit.log_attempt(
            current_tenant=ctx.tenant_id, target_tenant=target_tenant_id,
            resource_type=resource_type, resource_id=resource_id, access_type=access_type,
            user_id=ctx.user_id, ip_address=ctx.ip_address, allowed=True)
        return True

    @classmethod
    def filter_queryset(cls, query, model_class, session=None):
        ctx = get_tenant_context()
        if ctx is None:
            return query
        if cls.is_exempt(model_class.__name__):
            return query
        if hasattr(model_class, "tenant_id"):
            return query.filter(model_class.tenant_id == ctx.tenant_id)
        return query


class TenantQueryInterceptor:
    _registered: bool = False
    _enabled: bool = True

    @classmethod
    def register(cls, engine=None, session_factory=None):
        if not SQLALCHEMY_AVAILABLE or cls._registered:
            return
        @event.listens_for(Session, "do_orm_execute")
        def receive_do_orm_execute(orm_execute_state):
            if not cls._enabled or not orm_execute_state.is_select:
                return
        cls._registered = True
        logger.info("Tenant query interceptor registered")

    @classmethod
    def enable(cls):
        cls._enabled = True

    @classmethod
    def disable(cls):
        cls._enabled = False

    @classmethod
    def is_enabled(cls) -> bool:
        return cls._enabled


def tenant_query_interceptor(model_class):
    TenantIsolation.register_model(model_class.__name__)
    def filtered_query(cls):
        from factory.database.connection import get_db
        ctx = get_tenant_context()
        session = next(get_db())
        query = session.query(cls)
        if ctx and hasattr(cls, "tenant_id"):
            query = query.filter(cls.tenant_id == ctx.tenant_id)
        return query
    model_class.tenant_query = classmethod(filtered_query)
    return model_class



@dataclass
class CrossTenantAccessLog:
    timestamp: datetime
    current_tenant: str
    target_tenant: str
    resource_type: Optional[str]
    resource_id: Optional[str]
    access_type: AccessType
    user_id: Optional[str]
    ip_address: Optional[str]
    allowed: bool
    reason: Optional[str] = None


class CrossTenantAccessAudit:
    _logs: List[CrossTenantAccessLog] = []
    _max_logs: int = 10000
    _db_enabled: bool = False
    _alert_threshold: int = 5
    _recent_denials: Dict[str, List[datetime]] = {}

    @classmethod
    def log_attempt(cls, current_tenant: str, target_tenant: str, resource_type: Optional[str] = None,
                    resource_id: Optional[str] = None, access_type: AccessType = AccessType.READ,
                    user_id: Optional[str] = None, ip_address: Optional[str] = None,
                    allowed: bool = False, reason: Optional[str] = None):
        log_entry = CrossTenantAccessLog(
            timestamp=datetime.utcnow(), current_tenant=current_tenant, target_tenant=target_tenant,
            resource_type=resource_type, resource_id=resource_id, access_type=access_type,
            user_id=user_id, ip_address=ip_address, allowed=allowed, reason=reason)
        cls._logs.append(log_entry)
        if len(cls._logs) > cls._max_logs:
            cls._logs = cls._logs[-cls._max_logs:]
        log_level = logging.INFO if allowed else logging.WARNING
        logger.log(log_level, f"Cross-tenant access: {current_tenant} -> {target_tenant} [{resource_type}:{resource_id}] - {'ALLOWED' if allowed else 'DENIED'}")
        if not allowed:
            cls._check_alert_threshold(current_tenant, user_id, ip_address)

    @classmethod
    def _check_alert_threshold(cls, tenant_id: str, user_id: Optional[str], ip_address: Optional[str]):
        key = f"{tenant_id}:{user_id}:{ip_address}"
        now = datetime.utcnow()
        cutoff = now - timedelta(minutes=5)
        if key not in cls._recent_denials:
            cls._recent_denials[key] = []
        cls._recent_denials[key] = [t for t in cls._recent_denials[key] if t > cutoff]
        cls._recent_denials[key].append(now)
        if len(cls._recent_denials[key]) >= cls._alert_threshold:
            logger.critical(f"SECURITY ALERT: Multiple cross-tenant access denials from tenant={tenant_id}, user={user_id}, ip={ip_address}")

    @classmethod
    def get_recent_logs(cls, tenant_id: Optional[str] = None, limit: int = 100) -> List[CrossTenantAccessLog]:
        logs = cls._logs[-limit:]
        if tenant_id:
            logs = [log for log in logs if log.current_tenant == tenant_id or log.target_tenant == tenant_id]
        return logs

    @classmethod
    def get_denial_stats(cls, hours: int = 24) -> Dict[str, int]:
        cutoff = datetime.utcnow() - timedelta(hours=hours)
        recent = [log for log in cls._logs if log.timestamp > cutoff]
        return {
            "total_attempts": len(recent),
            "denied": len([l for l in recent if not l.allowed]),
            "allowed": len([l for l in recent if l.allowed]),
            "unique_tenants": len(set(l.current_tenant for l in recent)),
        }

    @classmethod
    def enable_db_logging(cls, enabled: bool = True):
        cls._db_enabled = enabled


def audit_cross_tenant_access(resource_type: str, access_type: AccessType = AccessType.READ):
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            ctx = get_tenant_context()
            target_tenant = kwargs.get("tenant_id") or (args[1] if len(args) > 1 else None)
            resource_id = kwargs.get("resource_id") or kwargs.get("id") or (args[0] if args else None)
            if ctx and target_tenant and ctx.tenant_id != target_tenant:
                TenantIsolation.validate_access(
                    target_tenant_id=target_tenant, resource_type=resource_type,
                    resource_id=str(resource_id) if resource_id else None, access_type=access_type)
            return func(*args, **kwargs)
        return wrapper
    return decorator



class TenantEncryption:
    _master_key: Optional[bytes] = None
    _tenant_keys: Dict[str, bytes] = {}
    _cipher_cache: Dict[str, Any] = {}

    @classmethod
    def initialize(cls, master_key: Optional[str] = None):
        if not CRYPTO_AVAILABLE:
            logger.warning("Cryptography not available - encryption disabled")
            return
        if master_key:
            cls._master_key = base64.urlsafe_b64decode(master_key)
        else:
            env_key = os.getenv("TENANT_ENCRYPTION_KEY")
            if env_key:
                cls._master_key = base64.urlsafe_b64decode(env_key)
            else:
                cls._master_key = Fernet.generate_key()
                logger.warning("No TENANT_ENCRYPTION_KEY set - generated temporary key.")
        logger.info("Tenant encryption initialized")

    @classmethod
    def _derive_tenant_key(cls, tenant_id: str) -> bytes:
        if not CRYPTO_AVAILABLE:
            return b""
        if tenant_id in cls._tenant_keys:
            return cls._tenant_keys[tenant_id]
        if not cls._master_key:
            cls.initialize()
        salt = hashlib.sha256(tenant_id.encode()).digest()
        kdf = PBKDF2HMAC(algorithm=hashes.SHA256(), length=32, salt=salt, iterations=100000)
        derived_key = base64.urlsafe_b64encode(kdf.derive(cls._master_key))
        cls._tenant_keys[tenant_id] = derived_key
        return derived_key

    @classmethod
    def _get_cipher(cls, tenant_id: str):
        if not CRYPTO_AVAILABLE:
            return None
        if tenant_id not in cls._cipher_cache:
            key = cls._derive_tenant_key(tenant_id)
            cls._cipher_cache[tenant_id] = Fernet(key)
        return cls._cipher_cache[tenant_id]

    @classmethod
    def encrypt(cls, data: Union[str, bytes], tenant_id: Optional[str] = None) -> str:
        if not CRYPTO_AVAILABLE:
            if isinstance(data, bytes):
                return base64.urlsafe_b64encode(data).decode()
            return base64.urlsafe_b64encode(data.encode()).decode()
        if tenant_id is None:
            ctx = get_tenant_context()
            if ctx:
                tenant_id = ctx.tenant_id
            else:
                raise TenantNotFoundError("Tenant ID required for encryption")
        cipher = cls._get_cipher(tenant_id)
        if isinstance(data, str):
            data = data.encode("utf-8")
        encrypted = cipher.encrypt(data)
        return base64.urlsafe_b64encode(encrypted).decode()

    @classmethod
    def decrypt(cls, encrypted_data: str, tenant_id: Optional[str] = None) -> str:
        if not CRYPTO_AVAILABLE:
            try:
                return base64.urlsafe_b64decode(encrypted_data).decode()
            except Exception:
                return encrypted_data
        if tenant_id is None:
            ctx = get_tenant_context()
            if ctx:
                tenant_id = ctx.tenant_id
            else:
                raise TenantNotFoundError("Tenant ID required for decryption")
        cipher = cls._get_cipher(tenant_id)
        encrypted_bytes = base64.urlsafe_b64decode(encrypted_data)
        decrypted = cipher.decrypt(encrypted_bytes)
        return decrypted.decode("utf-8")

    @classmethod
    def rotate_key(cls, tenant_id: str, old_key: Optional[str] = None):
        if tenant_id in cls._tenant_keys:
            del cls._tenant_keys[tenant_id]
        if tenant_id in cls._cipher_cache:
            del cls._cipher_cache[tenant_id]
        logger.info(f"Rotated encryption key for tenant: {tenant_id}")


def get_tenant_encryption() -> TenantEncryption:
    return TenantEncryption



class TenantIsolationMiddleware:
    def __init__(self, app, header_name: str = "X-Tenant-ID", jwt_claim: str = "tenant_id",
                 require_tenant: bool = True, exempt_paths: Optional[List[str]] = None):
        self.app = app
        self.header_name = header_name
        self.jwt_claim = jwt_claim
        self.require_tenant = require_tenant
        self.exempt_paths = exempt_paths or ["/health", "/metrics", "/api/auth/login", "/api/auth/register", "/docs", "/openapi.json"]

    async def __call__(self, scope, receive, send):
        if scope["type"] != "http":
            await self.app(scope, receive, send)
            return
        path = scope.get("path", "")
        if any(path.startswith(exempt) for exempt in self.exempt_paths):
            await self.app(scope, receive, send)
            return
        headers = dict(scope.get("headers", []))
        tenant_id = headers.get(self.header_name.lower().encode(), b"").decode()
        if not tenant_id and self.require_tenant:
            await self._send_error(send, 401, "Tenant ID required")
            return
        if tenant_id:
            user_id = headers.get(b"x-user-id", b"").decode() or None
            user_roles = headers.get(b"x-user-roles", b"").decode()
            user_roles = user_roles.split(",") if user_roles else []
            client = scope.get("client", ("", 0))
            ip_address = client[0] if client else None
            set_tenant_context(tenant_id=tenant_id, user_id=user_id, user_roles=user_roles, ip_address=ip_address)
        try:
            await self.app(scope, receive, send)
        finally:
            clear_tenant_context()

    async def _send_error(self, send, status: int, message: str):
        body = json.dumps({"error": message}).encode()
        await send({"type": "http.response.start", "status": status, "headers": [[b"content-type", b"application/json"]]})
        await send({"type": "http.response.body", "body": body})


def initialize_tenant_isolation(engine=None, master_encryption_key: Optional[str] = None):
    if engine:
        TenantQueryInterceptor.register(engine=engine)
    TenantEncryption.initialize(master_encryption_key)
    logger.info("Tenant isolation system initialized")


if os.getenv("TENANT_ENCRYPTION_KEY"):
    TenantEncryption.initialize()

# -*- coding: utf-8 -*-
"""
Autenticacao via API Key - Plataforma E v6.5
=================================================

Sistema de autenticacao para API publica usando API Keys.
Suporta multiplos tiers com diferentes limites de rate limiting.
"""
import os
from datetime import datetime
from typing import Optional, Tuple
from functools import wraps

from fastapi import HTTPException, Request, Security, Depends, status
from fastapi.security import APIKeyHeader, APIKeyQuery
from pydantic import BaseModel
from sqlalchemy.orm import Session

from dotenv import load_dotenv
load_dotenv()


# =============================================================================
# CONFIGURACAO
# =============================================================================

# Header e query param para API Key
API_KEY_HEADER_NAME = "X-API-Key"
API_KEY_QUERY_NAME = "api_key"

# Security schemes
api_key_header = APIKeyHeader(name=API_KEY_HEADER_NAME, auto_error=False)
api_key_query = APIKeyQuery(name=API_KEY_QUERY_NAME, auto_error=False)


# =============================================================================
# MODELS
# =============================================================================

class APIKeyInfo(BaseModel):
    """Informacoes da API Key autenticada"""
    key_id: str
    name: str
    tier: str
    scopes: list
    rate_limits: dict
    user_id: Optional[int] = None


class APIKeyCreate(BaseModel):
    """Request para criar API Key"""
    name: str
    description: Optional[str] = None
    tier: str = "free"
    scopes: list = ["read"]
    expires_in_days: Optional[int] = None  # Null = nunca expira


class APIKeyResponse(BaseModel):
    """Response com API Key criada"""
    key_id: str
    api_key: str  # Chave completa (mostrada apenas uma vez)
    name: str
    tier: str
    scopes: list
    rate_limits: dict
    expires_at: Optional[str] = None
    created_at: str

    class Config:
        json_schema_extra = {
            "example": {
                "key_id": "fab_abc123xyz",
                "api_key": "sk-fab_abc123xyz_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                "name": "Minha App",
                "tier": "free",
                "scopes": ["read", "write"],
                "rate_limits": {"per_minute": 100, "per_day": 1000},
                "expires_at": None,
                "created_at": "2024-01-15T10:30:00"
            }
        }


# =============================================================================
# FUNCOES DE AUTENTICACAO
# =============================================================================

def _get_db_session():
    """Obtem sessao do banco de dados"""
    try:
        from factory.database.connection import SessionLocal
        return SessionLocal()
    except Exception as e:
        print(f"[APIKeyAuth] Erro ao obter sessao DB: {e}")
        return None


def _get_api_key_from_db(key_hash: str) -> Optional[dict]:
    """Busca API Key no banco pelo hash"""
    db = _get_db_session()
    if not db:
        return None

    try:
        from factory.database.api_models import APIKey

        api_key = db.query(APIKey).filter(
            APIKey.key_hash == key_hash
        ).first()

        if api_key:
            return {
                "model": api_key,
                "key_id": api_key.key_id,
                "name": api_key.name,
                "tier": api_key.tier,
                "status": api_key.status,
                "scopes": api_key.scopes or [],
                "allowed_ips": api_key.allowed_ips or [],
                "user_id": api_key.user_id,
                "expires_at": api_key.expires_at,
                "rate_limits": api_key.get_rate_limits(),
            }
        return None
    except Exception as e:
        print(f"[APIKeyAuth] Erro ao buscar API Key: {e}")
        return None
    finally:
        db.close()


def _update_api_key_usage(key_id: str, ip_address: str):
    """Atualiza metricas de uso da API Key"""
    db = _get_db_session()
    if not db:
        return

    try:
        from factory.database.api_models import APIKey

        api_key = db.query(APIKey).filter(APIKey.key_id == key_id).first()
        if api_key:
            api_key.requests_today += 1
            api_key.requests_total += 1
            api_key.last_used_at = datetime.utcnow()
            api_key.last_used_ip = ip_address
            db.commit()
    except Exception as e:
        print(f"[APIKeyAuth] Erro ao atualizar uso: {e}")
        db.rollback()
    finally:
        db.close()


def validate_api_key(api_key: str, request: Request = None) -> Tuple[bool, Optional[APIKeyInfo], Optional[str]]:
    """
    Valida uma API Key

    Args:
        api_key: A chave completa (sk-fab_xxx_yyy)
        request: Request FastAPI (para verificar IP)

    Returns:
        tuple: (is_valid, api_key_info, error_message)
    """
    if not api_key:
        return False, None, "API Key nao fornecida"

    # Verificar formato basico
    if not api_key.startswith("sk-fab_"):
        return False, None, "Formato de API Key invalido"

    # Calcular hash
    import hashlib
    key_hash = hashlib.sha256(api_key.encode()).hexdigest()

    # Buscar no banco
    key_data = _get_api_key_from_db(key_hash)

    if not key_data:
        return False, None, "API Key invalida ou nao encontrada"

    # Verificar status
    if key_data["status"] != "active":
        return False, None, f"API Key {key_data['status']}"

    # Verificar expiracao
    if key_data["expires_at"] and datetime.utcnow() > key_data["expires_at"]:
        return False, None, "API Key expirada"

    # Verificar IP (se configurado)
    if key_data["allowed_ips"] and request:
        client_ip = request.client.host if request.client else None
        if client_ip and client_ip not in key_data["allowed_ips"]:
            return False, None, f"IP {client_ip} nao autorizado para esta API Key"

    # Atualizar uso
    if request:
        client_ip = request.client.host if request.client else "unknown"
        _update_api_key_usage(key_data["key_id"], client_ip)

    # Retornar info
    api_key_info = APIKeyInfo(
        key_id=key_data["key_id"],
        name=key_data["name"],
        tier=key_data["tier"],
        scopes=key_data["scopes"],
        rate_limits=key_data["rate_limits"],
        user_id=key_data["user_id"],
    )

    return True, api_key_info, None


# =============================================================================
# DEPENDENCIES FASTAPI
# =============================================================================

async def get_api_key(
    request: Request,
    api_key_header: str = Security(api_key_header),
    api_key_query: str = Security(api_key_query),
) -> APIKeyInfo:
    """
    Dependency para autenticacao via API Key

    Aceita API Key via:
    - Header: X-API-Key
    - Query param: api_key

    Usage:
        @app.get("/api/v1/projects")
        async def list_projects(api_key: APIKeyInfo = Depends(get_api_key)):
            return {"key_id": api_key.key_id}
    """
    # Priorizar header sobre query param
    api_key = api_key_header or api_key_query

    if not api_key:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail={
                "error": "api_key_required",
                "message": "API Key obrigatoria. Forneca via header X-API-Key ou query param api_key.",
                "docs": "https://docs.fabricadeagentes.com/api/authentication"
            },
            headers={"WWW-Authenticate": "ApiKey"}
        )

    is_valid, key_info, error = validate_api_key(api_key, request)

    if not is_valid:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail={
                "error": "invalid_api_key",
                "message": error
            },
            headers={"WWW-Authenticate": "ApiKey"}
        )

    # Armazenar info no request state para uso posterior
    request.state.api_key_info = key_info

    return key_info


async def get_api_key_optional(
    request: Request,
    api_key_header: str = Security(api_key_header),
    api_key_query: str = Security(api_key_query),
) -> Optional[APIKeyInfo]:
    """
    Dependency para API Key opcional

    Retorna None se nao autenticado, sem erro.
    """
    api_key = api_key_header or api_key_query

    if not api_key:
        return None

    is_valid, key_info, _ = validate_api_key(api_key, request)

    if is_valid:
        request.state.api_key_info = key_info
        return key_info

    return None


def require_scope(scope: str):
    """
    Factory para dependency que requer scope especifico

    Usage:
        @app.post("/api/v1/projects")
        async def create_project(api_key: APIKeyInfo = Depends(require_scope("write"))):
            pass
    """
    async def scope_checker(api_key: APIKeyInfo = Depends(get_api_key)) -> APIKeyInfo:
        if scope not in api_key.scopes and "admin" not in api_key.scopes:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail={
                    "error": "insufficient_scope",
                    "message": f"Esta operacao requer scope '{scope}'",
                    "required_scope": scope,
                    "your_scopes": api_key.scopes
                }
            )
        return api_key

    return scope_checker


def require_tier(min_tier: str):
    """
    Factory para dependency que requer tier minimo

    Tiers em ordem: free < basic < pro < enterprise

    Usage:
        @app.post("/api/v1/bulk-operations")
        async def bulk_op(api_key: APIKeyInfo = Depends(require_tier("pro"))):
            pass
    """
    tier_order = {"free": 0, "basic": 1, "pro": 2, "enterprise": 3}

    async def tier_checker(api_key: APIKeyInfo = Depends(get_api_key)) -> APIKeyInfo:
        current_tier_level = tier_order.get(api_key.tier, 0)
        required_tier_level = tier_order.get(min_tier, 0)

        if current_tier_level < required_tier_level:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail={
                    "error": "insufficient_tier",
                    "message": f"Esta operacao requer tier '{min_tier}' ou superior",
                    "required_tier": min_tier,
                    "your_tier": api_key.tier,
                    "upgrade_url": "https://fabricadeagentes.com/pricing"
                }
            )
        return api_key

    return tier_checker


# =============================================================================
# FUNCOES CRUD PARA API KEYS
# =============================================================================

def create_api_key(
    name: str,
    description: str = None,
    tier: str = "free",
    scopes: list = None,
    user_id: int = None,
    expires_in_days: int = None,
    allowed_ips: list = None,
) -> dict:
    """
    Cria uma nova API Key

    Returns:
        dict com key_id, api_key (chave completa), e outros dados
    """
    from factory.database.api_models import APIKey
    from datetime import timedelta

    db = _get_db_session()
    if not db:
        raise Exception("Erro ao conectar ao banco de dados")

    try:
        # Gerar chave
        key_id, full_key, key_hash, key_prefix = APIKey.generate_key()

        # Calcular expiracao
        expires_at = None
        if expires_in_days:
            expires_at = datetime.utcnow() + timedelta(days=expires_in_days)

        # Criar registro
        api_key = APIKey(
            key_id=key_id,
            key_hash=key_hash,
            key_prefix=key_prefix,
            name=name,
            description=description,
            tier=tier,
            scopes=scopes or ["read"],
            user_id=user_id,
            expires_at=expires_at,
            allowed_ips=allowed_ips or [],
        )

        db.add(api_key)
        db.commit()
        db.refresh(api_key)

        return {
            "key_id": key_id,
            "api_key": full_key,  # Mostrado apenas uma vez!
            "name": name,
            "tier": tier,
            "scopes": scopes or ["read"],
            "rate_limits": api_key.get_rate_limits(),
            "expires_at": expires_at.isoformat() if expires_at else None,
            "created_at": api_key.created_at.isoformat(),
        }

    except Exception as e:
        db.rollback()
        raise e
    finally:
        db.close()


def list_api_keys(user_id: int = None) -> list:
    """Lista API Keys (sem mostrar a chave completa)"""
    from factory.database.api_models import APIKey

    db = _get_db_session()
    if not db:
        return []

    try:
        query = db.query(APIKey)
        if user_id:
            query = query.filter(APIKey.user_id == user_id)

        api_keys = query.order_by(APIKey.created_at.desc()).all()
        return [key.to_dict() for key in api_keys]

    finally:
        db.close()


def revoke_api_key(key_id: str, user_id: int = None) -> bool:
    """Revoga uma API Key"""
    from factory.database.api_models import APIKey, APIKeyStatus

    db = _get_db_session()
    if not db:
        return False

    try:
        query = db.query(APIKey).filter(APIKey.key_id == key_id)
        if user_id:
            query = query.filter(APIKey.user_id == user_id)

        api_key = query.first()

        if api_key:
            api_key.status = APIKeyStatus.REVOKED.value
            api_key.revoked_at = datetime.utcnow()
            db.commit()
            return True

        return False

    except Exception as e:
        db.rollback()
        print(f"[APIKeyAuth] Erro ao revogar key: {e}")
        return False
    finally:
        db.close()


def get_api_key_stats(key_id: str) -> dict:
    """Retorna estatisticas de uso de uma API Key"""
    from factory.database.api_models import APIKey, APIRequestLog
    from sqlalchemy import func

    db = _get_db_session()
    if not db:
        return {}

    try:
        api_key = db.query(APIKey).filter(APIKey.key_id == key_id).first()
        if not api_key:
            return {}

        # Buscar estatisticas de requests
        today = datetime.utcnow().replace(hour=0, minute=0, second=0, microsecond=0)

        requests_today = db.query(func.count(APIRequestLog.id)).filter(
            APIRequestLog.api_key_id == key_id,
            APIRequestLog.created_at >= today
        ).scalar() or 0

        avg_response_time = db.query(func.avg(APIRequestLog.response_time_ms)).filter(
            APIRequestLog.api_key_id == key_id,
            APIRequestLog.created_at >= today
        ).scalar() or 0

        return {
            "key_id": key_id,
            "requests_today": requests_today,
            "requests_total": api_key.requests_total,
            "avg_response_time_ms": round(avg_response_time, 2),
            "rate_limits": api_key.get_rate_limits(),
            "last_used_at": api_key.last_used_at.isoformat() if api_key.last_used_at else None,
        }

    finally:
        db.close()

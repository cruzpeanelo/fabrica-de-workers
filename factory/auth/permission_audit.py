# -*- coding: utf-8 -*-
"""
Permission Audit Logging - Auditoria de Mudancas de Permissoes
Plataforma E v6.5

Sistema de auditoria para rastreabilidade completa de acoes RBAC:
- Criacao/atualizacao/delecao de roles
- Atribuicao/revogacao de roles a usuarios
- Alteracoes de permissoes em roles

Inclui:
- Quem fez a mudanca (actor)
- Quando foi feita (timestamp)
- O que mudou (old_value, new_value)
- Contexto adicional (IP, user agent, session)
"""

import uuid
import json
import asyncio
from datetime import datetime
from enum import Enum
from functools import wraps
from typing import Optional, List, Dict, Any, Callable, Union

from fastapi import Request

# Database imports
from factory.database.connection import SessionLocal


# =============================================================================
# PERMISSION AUDIT TYPES
# =============================================================================

class PermissionAuditAction(str, Enum):
    """Tipos de acoes de auditoria de permissoes"""
    CREATE = "CREATE"
    UPDATE = "UPDATE"
    DELETE = "DELETE"
    ASSIGN = "ASSIGN"
    REVOKE = "REVOKE"


class PermissionResourceType(str, Enum):
    """Tipos de recursos de permissao"""
    ROLE = "role"
    PERMISSION = "permission"
    USER_ROLE = "user_role"


# =============================================================================
# PERMISSION AUDIT LOGGER
# =============================================================================

class PermissionAuditLogger:
    """
    Logger especializado para auditoria de mudancas de permissoes/roles.

    Registra:
    - Criacao, atualizacao e delecao de roles
    - Atribuicao e revogacao de roles a usuarios
    - Alteracoes em permissoes de roles

    Inclui:
    - Quem fez a mudanca (actor)
    - Quando foi feita (timestamp)
    - O que mudou (old_value, new_value)
    - Contexto adicional (IP, user agent, session)
    """

    def __init__(self, db_session=None):
        self.db = db_session

    def _generate_audit_id(self) -> str:
        """Gera ID unico para entrada de audit log"""
        return f"PERM-AUD-{uuid.uuid4().hex[:12].upper()}"

    def log_permission_change(
        self,
        actor_user_id: Optional[int],
        actor_username: Optional[str],
        action: Union[PermissionAuditAction, str],
        resource_type: Union[PermissionResourceType, str],
        resource_id: str,
        old_value: Optional[Dict] = None,
        new_value: Optional[Dict] = None,
        affected_user_id: Optional[int] = None,
        affected_username: Optional[str] = None,
        ip_address: Optional[str] = None,
        user_agent: Optional[str] = None,
        session_id: Optional[str] = None,
        success: bool = True,
        error_message: Optional[str] = None,
        metadata: Optional[Dict] = None
    ) -> Dict:
        """
        Registra uma mudanca de permissao no log de auditoria.

        Args:
            actor_user_id: ID do usuario que fez a mudanca
            actor_username: Username do ator
            action: Tipo de acao (CREATE, UPDATE, DELETE, ASSIGN, REVOKE)
            resource_type: Tipo de recurso (role, permission, user_role)
            resource_id: ID do recurso afetado
            old_value: Valor anterior (para UPDATE)
            new_value: Novo valor
            affected_user_id: ID do usuario afetado (para ASSIGN/REVOKE)
            affected_username: Username do usuario afetado
            ip_address: Endereco IP do ator
            user_agent: User agent do navegador
            session_id: ID da sessao
            success: Se a operacao foi bem sucedida
            error_message: Mensagem de erro se falhou
            metadata: Dados adicionais

        Returns:
            Dicionario com os dados do log criado
        """
        from factory.database.models import AuditLog

        db = self.db or SessionLocal()
        close_db = self.db is None

        try:
            # Converter enums para string
            action_str = action.value if isinstance(action, PermissionAuditAction) else str(action)
            resource_type_str = resource_type.value if isinstance(resource_type, PermissionResourceType) else str(resource_type)

            # Preparar detalhes completos
            details = {
                "audit_id": self._generate_audit_id(),
                "resource_type": resource_type_str,
                "old_value": old_value,
                "new_value": new_value,
                "affected_user_id": affected_user_id,
                "affected_username": affected_username,
                "session_id": session_id,
                "user_agent": user_agent,
                "metadata": metadata or {}
            }

            # Criar log usando o modelo AuditLog existente
            log = AuditLog(
                user_id=actor_user_id,
                username=actor_username or "system",
                action=f"permission:{action_str.lower()}",
                resource=resource_type_str,
                resource_id=resource_id,
                details=details,
                ip_address=ip_address,
                success=success,
                error_message=error_message
            )

            db.add(log)
            db.commit()
            db.refresh(log)

            return log.to_dict()

        except Exception as e:
            db.rollback()
            # Log falha mas nao interrompe operacao principal
            print(f"[AUDIT] Failed to log permission change: {e}")
            return {"error": str(e)}
        finally:
            if close_db:
                db.close()

    def log_role_created(
        self,
        actor_user_id: Optional[int],
        actor_username: Optional[str],
        role_data: Dict,
        ip_address: Optional[str] = None,
        **kwargs
    ) -> Dict:
        """Registra criacao de role"""
        return self.log_permission_change(
            actor_user_id=actor_user_id,
            actor_username=actor_username,
            action=PermissionAuditAction.CREATE,
            resource_type=PermissionResourceType.ROLE,
            resource_id=role_data.get("role_id", ""),
            new_value=role_data,
            ip_address=ip_address,
            **kwargs
        )

    def log_role_updated(
        self,
        actor_user_id: Optional[int],
        actor_username: Optional[str],
        role_id: str,
        old_data: Dict,
        new_data: Dict,
        ip_address: Optional[str] = None,
        **kwargs
    ) -> Dict:
        """Registra atualizacao de role"""
        return self.log_permission_change(
            actor_user_id=actor_user_id,
            actor_username=actor_username,
            action=PermissionAuditAction.UPDATE,
            resource_type=PermissionResourceType.ROLE,
            resource_id=role_id,
            old_value=old_data,
            new_value=new_data,
            ip_address=ip_address,
            **kwargs
        )

    def log_role_deleted(
        self,
        actor_user_id: Optional[int],
        actor_username: Optional[str],
        role_id: str,
        role_data: Dict,
        ip_address: Optional[str] = None,
        **kwargs
    ) -> Dict:
        """Registra delecao de role"""
        return self.log_permission_change(
            actor_user_id=actor_user_id,
            actor_username=actor_username,
            action=PermissionAuditAction.DELETE,
            resource_type=PermissionResourceType.ROLE,
            resource_id=role_id,
            old_value=role_data,
            ip_address=ip_address,
            **kwargs
        )

    def log_role_assigned(
        self,
        actor_user_id: Optional[int],
        actor_username: Optional[str],
        role_id: str,
        affected_user_id: int,
        affected_username: Optional[str] = None,
        assignment_data: Optional[Dict] = None,
        ip_address: Optional[str] = None,
        **kwargs
    ) -> Dict:
        """Registra atribuicao de role a usuario"""
        return self.log_permission_change(
            actor_user_id=actor_user_id,
            actor_username=actor_username,
            action=PermissionAuditAction.ASSIGN,
            resource_type=PermissionResourceType.USER_ROLE,
            resource_id=f"{affected_user_id}:{role_id}",
            new_value=assignment_data,
            affected_user_id=affected_user_id,
            affected_username=affected_username,
            ip_address=ip_address,
            **kwargs
        )

    def log_role_revoked(
        self,
        actor_user_id: Optional[int],
        actor_username: Optional[str],
        role_id: str,
        affected_user_id: int,
        affected_username: Optional[str] = None,
        revocation_data: Optional[Dict] = None,
        ip_address: Optional[str] = None,
        **kwargs
    ) -> Dict:
        """Registra revogacao de role de usuario"""
        return self.log_permission_change(
            actor_user_id=actor_user_id,
            actor_username=actor_username,
            action=PermissionAuditAction.REVOKE,
            resource_type=PermissionResourceType.USER_ROLE,
            resource_id=f"{affected_user_id}:{role_id}",
            old_value=revocation_data,
            affected_user_id=affected_user_id,
            affected_username=affected_username,
            ip_address=ip_address,
            **kwargs
        )

    def get_permission_audit_logs(
        self,
        actor_user_id: Optional[int] = None,
        affected_user_id: Optional[int] = None,
        resource_type: Optional[str] = None,
        action: Optional[str] = None,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        limit: int = 100,
        offset: int = 0
    ) -> List[Dict]:
        """
        Consulta logs de auditoria de permissoes.

        Args:
            actor_user_id: Filtrar por quem fez a acao
            affected_user_id: Filtrar por usuario afetado
            resource_type: Filtrar por tipo de recurso
            action: Filtrar por tipo de acao
            start_date: Data inicial
            end_date: Data final
            limit: Limite de resultados
            offset: Offset para paginacao

        Returns:
            Lista de logs de auditoria
        """
        from factory.database.models import AuditLog

        db = self.db or SessionLocal()
        close_db = self.db is None

        try:
            query = db.query(AuditLog).filter(
                AuditLog.action.like("permission:%")
            )

            if actor_user_id:
                query = query.filter(AuditLog.user_id == actor_user_id)

            if resource_type:
                query = query.filter(AuditLog.resource == resource_type)

            if action:
                query = query.filter(AuditLog.action == f"permission:{action.lower()}")

            if start_date:
                query = query.filter(AuditLog.timestamp >= start_date)

            if end_date:
                query = query.filter(AuditLog.timestamp <= end_date)

            logs = query.order_by(AuditLog.timestamp.desc()).offset(offset).limit(limit).all()

            # Filtrar por affected_user_id nos detalhes se fornecido
            result = []
            for log in logs:
                log_dict = log.to_dict()
                if affected_user_id:
                    details = log_dict.get("details", {})
                    if details.get("affected_user_id") == affected_user_id:
                        result.append(log_dict)
                else:
                    result.append(log_dict)

            return result

        finally:
            if close_db:
                db.close()


# Instancia global do logger de auditoria de permissoes
permission_audit_logger = PermissionAuditLogger()


# =============================================================================
# AUDIT ACTION DECORATOR
# =============================================================================

def audit_action(action: Union[PermissionAuditAction, str], resource_type: Union[PermissionResourceType, str]):
    """
    Decorator para auto-logging de acoes de permissao.

    Uso:
        @audit_action(PermissionAuditAction.CREATE, PermissionResourceType.ROLE)
        def create_role(data, current_user=None):
            ...
            return created_role

    O decorator automaticamente registra a acao no log de auditoria,
    capturando o usuario atual dos kwargs ou do contexto.
    """
    def decorator(func: Callable):
        @wraps(func)
        async def async_wrapper(*args, **kwargs):
            # Extrair contexto do usuario
            current_user = kwargs.get("current_user") or kwargs.get("user")
            request: Optional[Request] = kwargs.get("request")

            actor_user_id = None
            actor_username = "system"
            ip_address = None

            if current_user:
                if hasattr(current_user, "user_id"):
                    actor_user_id = current_user.user_id
                elif hasattr(current_user, "id"):
                    actor_user_id = current_user.id
                if hasattr(current_user, "username"):
                    actor_username = current_user.username

            if request:
                ip_address = request.client.host if request.client else None

            # Executar a funcao
            result = None
            error_msg = None
            success = True

            try:
                result = await func(*args, **kwargs)
            except Exception as e:
                success = False
                error_msg = str(e)
                raise
            finally:
                # Registrar no audit log
                resource_id = ""
                if result and isinstance(result, dict):
                    resource_id = result.get("role_id") or result.get("id") or ""
                elif "id" in kwargs:
                    resource_id = str(kwargs["id"])
                elif "role_id" in kwargs:
                    resource_id = kwargs["role_id"]

                permission_audit_logger.log_permission_change(
                    actor_user_id=actor_user_id,
                    actor_username=actor_username,
                    action=action,
                    resource_type=resource_type,
                    resource_id=str(resource_id),
                    new_value=result if success and isinstance(result, dict) else None,
                    ip_address=ip_address,
                    success=success,
                    error_message=error_msg
                )

            return result

        @wraps(func)
        def sync_wrapper(*args, **kwargs):
            # Extrair contexto do usuario
            current_user = kwargs.get("current_user") or kwargs.get("user")
            request: Optional[Request] = kwargs.get("request")

            actor_user_id = None
            actor_username = "system"
            ip_address = None

            if current_user:
                if hasattr(current_user, "user_id"):
                    actor_user_id = current_user.user_id
                elif hasattr(current_user, "id"):
                    actor_user_id = current_user.id
                if hasattr(current_user, "username"):
                    actor_username = current_user.username

            if request:
                ip_address = request.client.host if request.client else None

            # Executar a funcao
            result = None
            error_msg = None
            success = True

            try:
                result = func(*args, **kwargs)
            except Exception as e:
                success = False
                error_msg = str(e)
                raise
            finally:
                # Registrar no audit log
                resource_id = ""
                if result and isinstance(result, dict):
                    resource_id = result.get("role_id") or result.get("id") or ""
                elif "id" in kwargs:
                    resource_id = str(kwargs["id"])
                elif "role_id" in kwargs:
                    resource_id = kwargs["role_id"]

                permission_audit_logger.log_permission_change(
                    actor_user_id=actor_user_id,
                    actor_username=actor_username,
                    action=action,
                    resource_type=resource_type,
                    resource_id=str(resource_id),
                    new_value=result if success and isinstance(result, dict) else None,
                    ip_address=ip_address,
                    success=success,
                    error_message=error_msg
                )

            return result

        # Retornar wrapper apropriado
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        return sync_wrapper

    return decorator


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "PermissionAuditAction",
    "PermissionResourceType",
    "PermissionAuditLogger",
    "permission_audit_logger",
    "audit_action",
]

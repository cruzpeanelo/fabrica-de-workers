# -*- coding: utf-8 -*-
"""
Unified Authorization Layer
Plataforma E v6.3

Issue #146: Integra os 3 sistemas de permissao (RBAC/Personas/ABAC)
em uma camada unificada.

Esta camada combina:
- RBAC: Controle baseado em roles (ADMIN, MANAGER, DEVELOPER, VIEWER)
- Personas: Perfis com permissoes granulares e heranca
- ABAC: Controle baseado em atributos e politicas flexiveis

Uso:
    from factory.auth.unified_auth import (
        unified_check_permission,
        require_unified_permission,
        get_unified_user_context
    )

    # Verificar permissao
    allowed = await unified_check_permission(user, "stories", "create", project_id="PROJ-001")

    # Decorator para endpoints
    @app.post("/api/stories")
    @require_unified_permission("stories", "create")
    async def create_story(...):
        ...
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from functools import wraps
from typing import Optional, List, Dict, Any, Callable, Union

from fastapi import HTTPException, Depends, Request, APIRouter
from pydantic import BaseModel

# Import dos 3 sistemas
from factory.auth.rbac import (
    RBACManager,
    UserContext as RBACUserContext,
    get_current_user as get_rbac_user,
    DEFAULT_ROLES
)
from factory.auth.personas import (
    PersonaRegistry,
    PersonaType,
    PermissionChecker as PersonaChecker,
    get_persona_for_role,
    persona_registry
)
from factory.auth.abac import (
    ABACEngine,
    ABACContext,
    Effect,
    get_abac_engine,
    EvaluationResult
)

# Database
from factory.database.connection import SessionLocal

logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS E MODELOS
# =============================================================================

class AuthorizationDecision(str, Enum):
    """Decisao de autorizacao"""
    ALLOW = "allow"
    DENY = "deny"
    NOT_APPLICABLE = "not_applicable"


class CombiningAlgorithm(str, Enum):
    """Algoritmo para combinar decisoes dos 3 sistemas"""
    ALL_MUST_ALLOW = "all_must_allow"       # Todos precisam permitir
    ANY_ALLOW = "any_allow"                  # Qualquer um permitindo libera
    RBAC_PRIORITY = "rbac_priority"          # RBAC tem prioridade, ABAC refina
    DENY_OVERRIDES = "deny_overrides"        # Qualquer DENY bloqueia


@dataclass
class UnifiedAuthResult:
    """Resultado da verificacao unificada"""
    decision: AuthorizationDecision
    rbac_decision: Optional[bool] = None
    persona_decision: Optional[bool] = None
    abac_decision: Optional[AuthorizationDecision] = None
    reason: str = ""
    details: Dict[str, Any] = field(default_factory=dict)
    evaluation_time_ms: float = 0

    def to_dict(self) -> Dict[str, Any]:
        return {
            "decision": self.decision.value,
            "rbac_decision": self.rbac_decision,
            "persona_decision": self.persona_decision,
            "abac_decision": self.abac_decision.value if self.abac_decision else None,
            "reason": self.reason,
            "details": self.details,
            "evaluation_time_ms": self.evaluation_time_ms
        }


@dataclass
class UnifiedUserContext:
    """
    Contexto unificado do usuario que combina informacoes dos 3 sistemas.

    Issue #146: Esta classe centraliza todas as informacoes de autorizacao.
    """
    # Identificacao basica
    user_id: Optional[int] = None
    username: Optional[str] = None
    email: Optional[str] = None
    is_authenticated: bool = False

    # RBAC
    roles: List[str] = field(default_factory=list)
    rbac_permissions: List[str] = field(default_factory=list)
    is_admin: bool = False

    # Personas
    persona_type: Optional[PersonaType] = None
    persona_permissions: List[str] = field(default_factory=list)
    dashboard_type: str = "default"
    allowed_features: List[str] = field(default_factory=list)

    # ABAC
    attributes: Dict[str, Any] = field(default_factory=dict)

    # Contexto
    tenant_id: Optional[str] = None
    project_id: Optional[str] = None
    ip_address: Optional[str] = None

    def get_all_permissions(self) -> List[str]:
        """Retorna todas as permissoes combinadas (RBAC + Persona)"""
        return list(set(self.rbac_permissions + self.persona_permissions))

    def has_role(self, role: str) -> bool:
        """Verifica se usuario tem uma role"""
        return role.upper() in [r.upper() for r in self.roles]

    def has_feature(self, feature: str) -> bool:
        """Verifica se usuario tem acesso a feature"""
        return "*" in self.allowed_features or feature in self.allowed_features

    def to_dict(self) -> Dict[str, Any]:
        return {
            "user_id": self.user_id,
            "username": self.username,
            "email": self.email,
            "is_authenticated": self.is_authenticated,
            "roles": self.roles,
            "rbac_permissions": self.rbac_permissions,
            "is_admin": self.is_admin,
            "persona_type": self.persona_type.value if self.persona_type else None,
            "persona_permissions": self.persona_permissions,
            "dashboard_type": self.dashboard_type,
            "allowed_features": self.allowed_features,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id
        }

    def to_abac_subject(self) -> Dict[str, Any]:
        """Converte para atributos de subject ABAC"""
        return {
            "user_id": self.user_id,
            "username": self.username,
            "role": self.roles[0] if self.roles else None,
            "roles": self.roles,
            "permissions": self.get_all_permissions(),
            "is_admin": self.is_admin,
            "persona_type": self.persona_type.value if self.persona_type else None,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            **self.attributes
        }


# =============================================================================
# UNIFIED AUTH ENGINE
# =============================================================================

class UnifiedAuthEngine:
    """
    Motor de autorizacao unificado.

    Issue #146: Combina RBAC, Personas e ABAC em uma unica interface.
    """

    def __init__(
        self,
        combining_algorithm: CombiningAlgorithm = CombiningAlgorithm.RBAC_PRIORITY,
        enable_rbac: bool = True,
        enable_personas: bool = True,
        enable_abac: bool = True
    ):
        """
        Inicializa o motor unificado.

        Args:
            combining_algorithm: Algoritmo para combinar decisoes
            enable_rbac: Habilitar verificacao RBAC
            enable_personas: Habilitar verificacao de Personas
            enable_abac: Habilitar verificacao ABAC
        """
        self.combining_algorithm = combining_algorithm
        self.enable_rbac = enable_rbac
        self.enable_personas = enable_personas
        self.enable_abac = enable_abac

        # Inicializar subsistemas
        self._persona_registry = persona_registry
        self._persona_checker = PersonaChecker(self._persona_registry)

    def _get_rbac_manager(self) -> RBACManager:
        """Obtem RBACManager com nova sessao"""
        db = SessionLocal()
        return RBACManager(db)

    async def check_permission(
        self,
        user: UnifiedUserContext,
        resource: str,
        action: str,
        resource_attrs: Optional[Dict[str, Any]] = None,
        environment: Optional[Dict[str, Any]] = None
    ) -> UnifiedAuthResult:
        """
        Verifica permissao usando todos os sistemas habilitados.

        Args:
            user: Contexto unificado do usuario
            resource: Recurso sendo acessado (stories, projects, etc)
            action: Acao sendo executada (create, read, update, delete)
            resource_attrs: Atributos adicionais do recurso
            environment: Contexto ambiental (IP, hora, etc)

        Returns:
            UnifiedAuthResult com a decisao combinada
        """
        import time
        start_time = time.time()

        rbac_decision = None
        persona_decision = None
        abac_decision = None
        details = {}

        # 1. Verificar RBAC
        if self.enable_rbac and user.user_id:
            rbac_decision = self._check_rbac(user, resource, action)
            details["rbac"] = {
                "checked": True,
                "decision": rbac_decision,
                "roles": user.roles
            }

        # 2. Verificar Personas
        if self.enable_personas and user.persona_type:
            persona_decision = self._check_persona(user, resource, action)
            details["persona"] = {
                "checked": True,
                "decision": persona_decision,
                "persona_type": user.persona_type.value
            }

        # 3. Verificar ABAC
        if self.enable_abac:
            abac_result = self._check_abac(user, resource, action, resource_attrs, environment)
            abac_decision = AuthorizationDecision.ALLOW if abac_result.decision == Effect.ALLOW else AuthorizationDecision.DENY
            details["abac"] = {
                "checked": True,
                "decision": abac_decision.value,
                "policy_id": abac_result.policy_id,
                "reason": abac_result.reason
            }

        # 4. Combinar decisoes
        final_decision, reason = self._combine_decisions(
            rbac_decision,
            persona_decision,
            abac_decision
        )

        eval_time = (time.time() - start_time) * 1000

        result = UnifiedAuthResult(
            decision=final_decision,
            rbac_decision=rbac_decision,
            persona_decision=persona_decision,
            abac_decision=abac_decision,
            reason=reason,
            details=details,
            evaluation_time_ms=eval_time
        )

        # Log da decisao
        log_level = logging.INFO if final_decision == AuthorizationDecision.ALLOW else logging.WARNING
        logger.log(
            log_level,
            f"[UnifiedAuth] {final_decision.value.upper()}: "
            f"user={user.username} resource={resource} action={action} "
            f"reason={reason}"
        )

        return result

    def _check_rbac(self, user: UnifiedUserContext, resource: str, action: str) -> bool:
        """Verifica permissao via RBAC"""
        try:
            rbac = self._get_rbac_manager()
            try:
                return rbac.check_permission(user.user_id, resource, action, user.project_id)
            finally:
                rbac.close()
        except Exception as e:
            logger.error(f"[UnifiedAuth] RBAC check failed: {e}")
            return False

    def _check_persona(self, user: UnifiedUserContext, resource: str, action: str) -> bool:
        """Verifica permissao via Personas"""
        try:
            context = {
                "tenant_id": user.tenant_id,
                "project_id": user.project_id
            }
            return self._persona_checker.check_permission(
                user.persona_type,
                resource,
                action,
                context
            )
        except Exception as e:
            logger.error(f"[UnifiedAuth] Persona check failed: {e}")
            return False

    def _check_abac(
        self,
        user: UnifiedUserContext,
        resource: str,
        action: str,
        resource_attrs: Optional[Dict[str, Any]] = None,
        environment: Optional[Dict[str, Any]] = None
    ) -> EvaluationResult:
        """Verifica permissao via ABAC"""
        try:
            engine = get_abac_engine()

            # Construir contexto ABAC
            res_attrs = {
                "type": resource,
                "path": resource
            }
            if resource_attrs:
                res_attrs.update(resource_attrs)

            env_attrs = environment or {}
            if user.ip_address:
                env_attrs["ip_address"] = user.ip_address

            context = ABACContext(
                subject=user.to_abac_subject(),
                resource=res_attrs,
                action=action,
                environment=env_attrs
            )

            return engine.evaluate(context)
        except Exception as e:
            logger.error(f"[UnifiedAuth] ABAC check failed: {e}")
            return EvaluationResult(
                decision=Effect.DENY,
                reason=f"ABAC evaluation failed: {str(e)}"
            )

    def _combine_decisions(
        self,
        rbac: Optional[bool],
        persona: Optional[bool],
        abac: Optional[AuthorizationDecision]
    ) -> tuple[AuthorizationDecision, str]:
        """
        Combina decisoes dos 3 sistemas usando o algoritmo configurado.

        Returns:
            tuple: (decisao final, razao)
        """
        # Converter para lista de decisoes booleanas
        decisions = []

        if rbac is not None:
            decisions.append(("RBAC", rbac))
        if persona is not None:
            decisions.append(("Persona", persona))
        if abac is not None:
            decisions.append(("ABAC", abac == AuthorizationDecision.ALLOW))

        if not decisions:
            return AuthorizationDecision.DENY, "No authorization system available"

        # Aplicar algoritmo de combinacao
        if self.combining_algorithm == CombiningAlgorithm.ALL_MUST_ALLOW:
            # Todos precisam permitir
            all_allow = all(d[1] for d in decisions)
            if all_allow:
                return AuthorizationDecision.ALLOW, "All systems allowed"
            denied_by = [d[0] for d in decisions if not d[1]]
            return AuthorizationDecision.DENY, f"Denied by: {', '.join(denied_by)}"

        elif self.combining_algorithm == CombiningAlgorithm.ANY_ALLOW:
            # Qualquer um permitindo libera
            any_allow = any(d[1] for d in decisions)
            if any_allow:
                allowed_by = [d[0] for d in decisions if d[1]]
                return AuthorizationDecision.ALLOW, f"Allowed by: {', '.join(allowed_by)}"
            return AuthorizationDecision.DENY, "All systems denied"

        elif self.combining_algorithm == CombiningAlgorithm.DENY_OVERRIDES:
            # Qualquer DENY bloqueia
            any_deny = any(not d[1] for d in decisions)
            if any_deny:
                denied_by = [d[0] for d in decisions if not d[1]]
                return AuthorizationDecision.DENY, f"Denied by: {', '.join(denied_by)}"
            return AuthorizationDecision.ALLOW, "No system denied"

        else:  # RBAC_PRIORITY (default)
            # RBAC tem prioridade, ABAC pode refinar/bloquear
            if rbac is not None:
                if not rbac:
                    return AuthorizationDecision.DENY, "Denied by RBAC"
                # RBAC permitiu, verificar se ABAC bloqueia
                if abac == AuthorizationDecision.DENY:
                    return AuthorizationDecision.DENY, "RBAC allowed but ABAC denied (policy restriction)"
                return AuthorizationDecision.ALLOW, "Allowed by RBAC"

            # RBAC nao disponivel, usar Persona
            if persona is not None:
                if not persona:
                    return AuthorizationDecision.DENY, "Denied by Persona"
                if abac == AuthorizationDecision.DENY:
                    return AuthorizationDecision.DENY, "Persona allowed but ABAC denied (policy restriction)"
                return AuthorizationDecision.ALLOW, "Allowed by Persona"

            # Apenas ABAC disponivel
            if abac is not None:
                if abac == AuthorizationDecision.ALLOW:
                    return AuthorizationDecision.ALLOW, "Allowed by ABAC"
                return AuthorizationDecision.DENY, "Denied by ABAC"

            return AuthorizationDecision.DENY, "No authorization decision available"


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

_unified_engine: Optional[UnifiedAuthEngine] = None


def get_unified_engine() -> UnifiedAuthEngine:
    """Retorna o motor unificado (singleton)"""
    global _unified_engine
    if _unified_engine is None:
        _unified_engine = UnifiedAuthEngine()
    return _unified_engine


def set_unified_engine(engine: UnifiedAuthEngine):
    """Define o motor unificado"""
    global _unified_engine
    _unified_engine = engine


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

async def build_unified_context(
    user: RBACUserContext,
    request: Optional[Request] = None,
    project_id: Optional[str] = None,
    tenant_id: Optional[str] = None
) -> UnifiedUserContext:
    """
    Constroi contexto unificado a partir do usuario RBAC.

    Args:
        user: Contexto do usuario do RBAC
        request: Request FastAPI (opcional)
        project_id: ID do projeto (opcional)
        tenant_id: ID do tenant (opcional)

    Returns:
        UnifiedUserContext completo
    """
    # Determinar persona baseada na role principal
    persona = None
    persona_permissions = []
    dashboard_type = "default"
    allowed_features = []

    if user.roles:
        persona = get_persona_for_role(user.roles[0])
        if persona:
            persona_permissions = list(persona.get_all_permissions())
            dashboard_type = persona.dashboard_type
            allowed_features = persona.allowed_features

    # Extrair IP do request
    ip_address = None
    if request and request.client:
        ip_address = request.client.host

    return UnifiedUserContext(
        user_id=user.user_id,
        username=user.username,
        is_authenticated=user.is_authenticated,
        roles=user.roles,
        rbac_permissions=user.permissions,
        is_admin=user.is_admin,
        persona_type=persona.persona_type if persona else None,
        persona_permissions=persona_permissions,
        dashboard_type=dashboard_type,
        allowed_features=allowed_features,
        tenant_id=tenant_id,
        project_id=project_id,
        ip_address=ip_address
    )


async def unified_check_permission(
    user: Union[RBACUserContext, UnifiedUserContext],
    resource: str,
    action: str,
    project_id: Optional[str] = None,
    resource_attrs: Optional[Dict[str, Any]] = None,
    environment: Optional[Dict[str, Any]] = None
) -> bool:
    """
    Funcao de conveniencia para verificar permissao.

    Args:
        user: Usuario (RBAC ou Unified context)
        resource: Recurso
        action: Acao
        project_id: ID do projeto (opcional)
        resource_attrs: Atributos do recurso (opcional)
        environment: Contexto ambiental (opcional)

    Returns:
        True se permitido, False caso contrario
    """
    # Converter para UnifiedUserContext se necessario
    if isinstance(user, RBACUserContext):
        unified_user = await build_unified_context(user, project_id=project_id)
    else:
        unified_user = user
        if project_id:
            unified_user.project_id = project_id

    engine = get_unified_engine()
    result = await engine.check_permission(
        unified_user,
        resource,
        action,
        resource_attrs,
        environment
    )

    return result.decision == AuthorizationDecision.ALLOW


# =============================================================================
# DECORATORS
# =============================================================================

def require_unified_permission(
    resource: str,
    action: str,
    resource_id_param: Optional[str] = None,
    project_id_param: Optional[str] = None
):
    """
    Decorator para verificar permissao usando o sistema unificado.

    Issue #146: Substitui decorators individuais de RBAC/Personas/ABAC.

    Usage:
        @app.post("/api/stories")
        @require_unified_permission("stories", "create")
        async def create_story(
            data: StoryCreate,
            user: RBACUserContext = Depends(get_current_user)
        ):
            ...

        @app.put("/api/projects/{project_id}/stories/{story_id}")
        @require_unified_permission("stories", "update", resource_id_param="story_id", project_id_param="project_id")
        async def update_story(...):
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            # Obter usuario do contexto
            user = kwargs.get("user") or kwargs.get("current_user")
            request = kwargs.get("request")

            if not user:
                for arg in args:
                    if hasattr(arg, "username") or hasattr(arg, "user_id"):
                        user = arg
                        break

            if not user or (hasattr(user, 'is_authenticated') and not user.is_authenticated):
                raise HTTPException(
                    status_code=401,
                    detail="Authentication required"
                )

            # Extrair project_id se especificado
            project_id = None
            if project_id_param and project_id_param in kwargs:
                project_id = kwargs[project_id_param]

            # Construir atributos do recurso
            resource_attrs = {"type": resource}
            if resource_id_param and resource_id_param in kwargs:
                resource_attrs["id"] = kwargs[resource_id_param]
                resource_attrs["path"] = f"{resource}/{kwargs[resource_id_param]}"

            # Construir contexto ambiental
            environment = {}
            if request:
                environment["ip_address"] = request.client.host if request.client else None
                environment["user_agent"] = request.headers.get("user-agent")
                environment["method"] = request.method

            # Construir contexto unificado
            if isinstance(user, UnifiedUserContext):
                unified_user = user
            else:
                unified_user = await build_unified_context(user, request, project_id)

            # Verificar permissao
            engine = get_unified_engine()
            result = await engine.check_permission(
                unified_user,
                resource,
                action,
                resource_attrs,
                environment
            )

            if result.decision != AuthorizationDecision.ALLOW:
                raise HTTPException(
                    status_code=403,
                    detail={
                        "error": "Permission denied",
                        "resource": resource,
                        "action": action,
                        "reason": result.reason,
                        "details": result.details
                    }
                )

            # Executar funcao original
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            return func(*args, **kwargs)

        return wrapper
    return decorator


def require_any_role(*roles: str):
    """
    Decorator para verificar se usuario tem pelo menos uma das roles.

    Usage:
        @require_any_role("ADMIN", "MANAGER")
        async def admin_or_manager_endpoint(...):
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = kwargs.get("user") or kwargs.get("current_user")

            if not user:
                raise HTTPException(status_code=401, detail="Authentication required")

            # Verificar roles
            user_roles = getattr(user, "roles", [])
            has_role = any(r.upper() in [ur.upper() for ur in user_roles] for r in roles)

            # Admin sempre tem acesso
            if getattr(user, "is_admin", False):
                has_role = True

            if not has_role:
                raise HTTPException(
                    status_code=403,
                    detail=f"One of these roles required: {', '.join(roles)}"
                )

            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            return func(*args, **kwargs)

        return wrapper
    return decorator


def require_feature(feature: str):
    """
    Decorator para verificar se usuario tem acesso a uma feature.

    Usage:
        @require_feature("export_reports")
        async def export_endpoint(...):
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = kwargs.get("user") or kwargs.get("current_user")
            request = kwargs.get("request")

            if not user:
                raise HTTPException(status_code=401, detail="Authentication required")

            # Construir contexto unificado
            if isinstance(user, UnifiedUserContext):
                unified_user = user
            else:
                unified_user = await build_unified_context(user, request)

            # Admin tem acesso a tudo
            if unified_user.is_admin:
                pass
            elif not unified_user.has_feature(feature):
                raise HTTPException(
                    status_code=403,
                    detail=f"Feature not available: {feature}"
                )

            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            return func(*args, **kwargs)

        return wrapper
    return decorator


# =============================================================================
# DEPENDENCY INJECTION
# =============================================================================

async def get_unified_user(
    user: RBACUserContext = Depends(get_rbac_user),
    request: Request = None
) -> UnifiedUserContext:
    """
    Dependency para obter contexto unificado do usuario.

    Usage:
        @app.get("/api/profile")
        async def get_profile(user: UnifiedUserContext = Depends(get_unified_user)):
            return user.to_dict()
    """
    return await build_unified_context(user, request)


# =============================================================================
# API ROUTER
# =============================================================================

unified_auth_router = APIRouter(prefix="/api/auth/unified", tags=["Unified Auth"])


class CheckPermissionRequest(BaseModel):
    """Request para verificar permissao"""
    resource: str
    action: str
    project_id: Optional[str] = None
    resource_attrs: Optional[Dict[str, Any]] = None
    environment: Optional[Dict[str, Any]] = None


class AuthStatusResponse(BaseModel):
    """Resposta de status de autorizacao"""
    enabled_systems: Dict[str, bool]
    combining_algorithm: str
    user_context: Optional[Dict[str, Any]] = None


@unified_auth_router.get("/status")
async def get_auth_status(user: RBACUserContext = Depends(get_rbac_user)) -> AuthStatusResponse:
    """Retorna status do sistema de autorizacao"""
    engine = get_unified_engine()
    unified_user = await build_unified_context(user)

    return AuthStatusResponse(
        enabled_systems={
            "rbac": engine.enable_rbac,
            "personas": engine.enable_personas,
            "abac": engine.enable_abac
        },
        combining_algorithm=engine.combining_algorithm.value,
        user_context=unified_user.to_dict() if user.is_authenticated else None
    )


@unified_auth_router.post("/check")
async def check_permission_endpoint(
    data: CheckPermissionRequest,
    user: RBACUserContext = Depends(get_rbac_user)
):
    """
    Verifica permissao usando sistema unificado.

    Retorna decisao combinada de RBAC, Personas e ABAC.
    """
    if not user.is_authenticated:
        return {
            "decision": "deny",
            "reason": "Not authenticated"
        }

    unified_user = await build_unified_context(user, project_id=data.project_id)
    engine = get_unified_engine()

    result = await engine.check_permission(
        unified_user,
        data.resource,
        data.action,
        data.resource_attrs,
        data.environment
    )

    return result.to_dict()


@unified_auth_router.get("/permissions")
async def get_user_permissions(
    user: RBACUserContext = Depends(get_rbac_user)
):
    """
    Retorna todas as permissoes do usuario (RBAC + Persona).
    """
    if not user.is_authenticated:
        return {"permissions": [], "features": []}

    unified_user = await build_unified_context(user)

    return {
        "user_id": unified_user.user_id,
        "username": unified_user.username,
        "roles": unified_user.roles,
        "persona_type": unified_user.persona_type.value if unified_user.persona_type else None,
        "rbac_permissions": unified_user.rbac_permissions,
        "persona_permissions": unified_user.persona_permissions,
        "all_permissions": unified_user.get_all_permissions(),
        "allowed_features": unified_user.allowed_features,
        "dashboard_type": unified_user.dashboard_type,
        "is_admin": unified_user.is_admin
    }


@unified_auth_router.get("/features")
async def get_available_features(
    user: RBACUserContext = Depends(get_rbac_user)
):
    """
    Retorna features disponiveis para o usuario.
    """
    if not user.is_authenticated:
        return {"features": []}

    unified_user = await build_unified_context(user)

    return {
        "features": unified_user.allowed_features,
        "dashboard_type": unified_user.dashboard_type
    }


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Classes
    "UnifiedAuthEngine",
    "UnifiedUserContext",
    "UnifiedAuthResult",
    "AuthorizationDecision",
    "CombiningAlgorithm",

    # Funcoes
    "get_unified_engine",
    "set_unified_engine",
    "build_unified_context",
    "unified_check_permission",

    # Decorators
    "require_unified_permission",
    "require_any_role",
    "require_feature",

    # Dependencies
    "get_unified_user",

    # Router
    "unified_auth_router",
]

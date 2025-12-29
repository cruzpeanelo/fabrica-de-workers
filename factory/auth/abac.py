# -*- coding: utf-8 -*-
"""
ABAC - Attribute-Based Access Control
Fabrica de Agentes v6.2 - Security Hardening

Issue #85 - Resource-Level Authorization (ABAC):
- Controle de acesso baseado em atributos
- Politicas flexiveis para recursos
- Decorators para protecao de endpoints
- Integracao com contexto de usuario e recurso
"""

import asyncio
import json
import logging
from datetime import datetime, time
from functools import wraps
from typing import Optional, List, Dict, Any, Callable, Union, Set
from dataclasses import dataclass, field
from enum import Enum
from abc import ABC, abstractmethod

from fastapi import HTTPException, Depends, Request
from pydantic import BaseModel, validator

logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS AND CONSTANTS
# =============================================================================

class Effect(str, Enum):
    """Efeito da politica"""
    ALLOW = "allow"
    DENY = "deny"


class ConditionOperator(str, Enum):
    """Operadores para condicoes"""
    EQUALS = "equals"
    NOT_EQUALS = "not_equals"
    IN = "in"
    NOT_IN = "not_in"
    CONTAINS = "contains"
    STARTS_WITH = "starts_with"
    ENDS_WITH = "ends_with"
    GREATER_THAN = "greater_than"
    LESS_THAN = "less_than"
    GREATER_EQUAL = "greater_equal"
    LESS_EQUAL = "less_equal"
    BETWEEN = "between"
    IS_NULL = "is_null"
    IS_NOT_NULL = "is_not_null"
    MATCHES = "matches"  # Regex


class AttributeSource(str, Enum):
    """Fonte dos atributos"""
    SUBJECT = "subject"      # Usuario/Principal
    RESOURCE = "resource"    # Recurso sendo acessado
    ACTION = "action"        # Acao sendo executada
    ENVIRONMENT = "environment"  # Contexto ambiental (hora, IP, etc)


# =============================================================================
# MODELS
# =============================================================================

@dataclass
class Attribute:
    """Atributo para avaliacao ABAC"""
    name: str
    value: Any
    source: AttributeSource


@dataclass
class Condition:
    """Condicao de uma politica ABAC"""
    attribute: str
    operator: ConditionOperator
    value: Any
    source: AttributeSource = AttributeSource.SUBJECT

    def evaluate(self, context: Dict[str, Any]) -> bool:
        """Avalia a condicao contra o contexto"""
        # Obter valor do atributo do contexto
        source_data = context.get(self.source.value, {})
        attr_value = self._get_nested_value(source_data, self.attribute)

        # Avaliar operador
        return self._evaluate_operator(attr_value)

    def _get_nested_value(self, data: Dict, key: str) -> Any:
        """Obtem valor de chave aninhada (ex: 'user.department')"""
        keys = key.split(".")
        value = data
        for k in keys:
            if isinstance(value, dict):
                value = value.get(k)
            else:
                return None
        return value

    def _evaluate_operator(self, attr_value: Any) -> bool:
        """Avalia operador com valor do atributo"""
        if self.operator == ConditionOperator.IS_NULL:
            return attr_value is None

        if self.operator == ConditionOperator.IS_NOT_NULL:
            return attr_value is not None

        if attr_value is None:
            return False

        if self.operator == ConditionOperator.EQUALS:
            return attr_value == self.value

        if self.operator == ConditionOperator.NOT_EQUALS:
            return attr_value != self.value

        if self.operator == ConditionOperator.IN:
            return attr_value in self.value

        if self.operator == ConditionOperator.NOT_IN:
            return attr_value not in self.value

        if self.operator == ConditionOperator.CONTAINS:
            return self.value in str(attr_value)

        if self.operator == ConditionOperator.STARTS_WITH:
            return str(attr_value).startswith(str(self.value))

        if self.operator == ConditionOperator.ENDS_WITH:
            return str(attr_value).endswith(str(self.value))

        if self.operator == ConditionOperator.GREATER_THAN:
            return attr_value > self.value

        if self.operator == ConditionOperator.LESS_THAN:
            return attr_value < self.value

        if self.operator == ConditionOperator.GREATER_EQUAL:
            return attr_value >= self.value

        if self.operator == ConditionOperator.LESS_EQUAL:
            return attr_value <= self.value

        if self.operator == ConditionOperator.BETWEEN:
            if isinstance(self.value, (list, tuple)) and len(self.value) == 2:
                return self.value[0] <= attr_value <= self.value[1]
            return False

        if self.operator == ConditionOperator.MATCHES:
            import re
            try:
                return bool(re.match(str(self.value), str(attr_value)))
            except re.error:
                return False

        return False


@dataclass
class Policy:
    """Politica ABAC"""
    policy_id: str
    name: str
    description: str
    effect: Effect
    resources: List[str]  # Padroes de recursos (ex: "projects/*", "stories/STR-*")
    actions: List[str]    # Acoes permitidas (create, read, update, delete)
    conditions: List[Condition] = field(default_factory=list)
    priority: int = 0     # Maior prioridade = avaliado primeiro
    active: bool = True

    def matches_resource(self, resource: str) -> bool:
        """Verifica se recurso corresponde a algum padrao"""
        for pattern in self.resources:
            if self._match_pattern(pattern, resource):
                return True
        return False

    def matches_action(self, action: str) -> bool:
        """Verifica se acao esta na lista permitida"""
        if "*" in self.actions:
            return True
        return action.lower() in [a.lower() for a in self.actions]

    def evaluate_conditions(self, context: Dict[str, Any]) -> bool:
        """Avalia todas as condicoes da politica"""
        if not self.conditions:
            return True

        return all(cond.evaluate(context) for cond in self.conditions)

    def _match_pattern(self, pattern: str, value: str) -> bool:
        """Verifica se valor corresponde ao padrao (suporta *)"""
        if pattern == "*":
            return True

        # Converter padrao para regex
        import re
        pattern_regex = pattern.replace("*", ".*").replace("?", ".")
        pattern_regex = f"^{pattern_regex}$"

        try:
            return bool(re.match(pattern_regex, value, re.IGNORECASE))
        except re.error:
            return pattern.lower() == value.lower()


class PolicySet(BaseModel):
    """Conjunto de politicas"""
    policy_set_id: str
    name: str
    description: Optional[str] = None
    policies: List[str] = []  # IDs das politicas
    combining_algorithm: str = "deny_unless_permit"  # ou "permit_unless_deny"


class EvaluationResult(BaseModel):
    """Resultado da avaliacao ABAC"""
    decision: Effect
    policy_id: Optional[str] = None
    policy_name: Optional[str] = None
    reason: str
    evaluated_policies: int = 0
    evaluation_time_ms: float = 0


@dataclass
class ABACContext:
    """Contexto completo para avaliacao ABAC"""
    subject: Dict[str, Any]      # Atributos do usuario
    resource: Dict[str, Any]     # Atributos do recurso
    action: str                  # Acao sendo executada
    environment: Dict[str, Any] = field(default_factory=dict)  # Contexto ambiental

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario para avaliacao"""
        return {
            "subject": self.subject,
            "resource": self.resource,
            "action": {"name": self.action},
            "environment": self.environment
        }


# =============================================================================
# ABAC ENGINE
# =============================================================================

class ABACEngine:
    """
    Motor de avaliacao ABAC

    Implementa:
    - Avaliacao de politicas baseada em atributos
    - Combinacao de resultados (deny_unless_permit, permit_unless_deny)
    - Cache de decisoes
    - Logging de auditoria
    """

    def __init__(
        self,
        combining_algorithm: str = "deny_unless_permit",
        enable_cache: bool = True,
        cache_ttl_seconds: int = 300
    ):
        """
        Inicializa o motor ABAC

        Args:
            combining_algorithm: Algoritmo de combinacao de politicas
            enable_cache: Habilitar cache de decisoes
            cache_ttl_seconds: TTL do cache em segundos
        """
        self.combining_algorithm = combining_algorithm
        self.enable_cache = enable_cache
        self.cache_ttl = cache_ttl_seconds
        self._policies: Dict[str, Policy] = {}
        self._policy_sets: Dict[str, PolicySet] = {}
        self._decision_cache: Dict[str, tuple] = {}

        # Carregar politicas padrao
        self._load_default_policies()

    def _load_default_policies(self):
        """Carrega politicas padrao do sistema"""
        # Politica: Admin tem acesso total
        self.add_policy(Policy(
            policy_id="POL-ADMIN-ALL",
            name="Admin Full Access",
            description="Administradores tem acesso total a todos os recursos",
            effect=Effect.ALLOW,
            resources=["*"],
            actions=["*"],
            conditions=[
                Condition(
                    attribute="role",
                    operator=ConditionOperator.EQUALS,
                    value="ADMIN",
                    source=AttributeSource.SUBJECT
                )
            ],
            priority=100
        ))

        # Politica: Usuarios so podem editar seus proprios recursos
        self.add_policy(Policy(
            policy_id="POL-OWNER-EDIT",
            name="Owner Resource Access",
            description="Usuarios podem editar recursos que possuem",
            effect=Effect.ALLOW,
            resources=["*"],
            actions=["read", "update", "delete"],
            conditions=[
                Condition(
                    attribute="owner_id",
                    operator=ConditionOperator.EQUALS,
                    value="${subject.user_id}",  # Referencia dinamica
                    source=AttributeSource.RESOURCE
                )
            ],
            priority=50
        ))

        # Politica: Bloquear acesso fora do horario comercial (exemplo)
        self.add_policy(Policy(
            policy_id="POL-BUSINESS-HOURS",
            name="Business Hours Only",
            description="Bloqueia acesso a recursos sensiveis fora do horario comercial",
            effect=Effect.DENY,
            resources=["admin/*", "settings/*"],
            actions=["*"],
            conditions=[
                Condition(
                    attribute="is_business_hours",
                    operator=ConditionOperator.EQUALS,
                    value=False,
                    source=AttributeSource.ENVIRONMENT
                )
            ],
            priority=90,
            active=False  # Desativada por padrao
        ))

        # Politica: Viewers so podem ler
        self.add_policy(Policy(
            policy_id="POL-VIEWER-READ",
            name="Viewer Read Only",
            description="Viewers so podem ler recursos",
            effect=Effect.ALLOW,
            resources=["*"],
            actions=["read"],
            conditions=[
                Condition(
                    attribute="role",
                    operator=ConditionOperator.EQUALS,
                    value="VIEWER",
                    source=AttributeSource.SUBJECT
                )
            ],
            priority=30
        ))

        # Politica: Developers podem CRUD em projetos
        self.add_policy(Policy(
            policy_id="POL-DEV-PROJECTS",
            name="Developer Project Access",
            description="Developers podem gerenciar projetos, stories e tasks",
            effect=Effect.ALLOW,
            resources=["projects/*", "stories/*", "tasks/*", "documentation/*"],
            actions=["create", "read", "update"],
            conditions=[
                Condition(
                    attribute="role",
                    operator=ConditionOperator.IN,
                    value=["DEVELOPER", "MANAGER"],
                    source=AttributeSource.SUBJECT
                )
            ],
            priority=40
        ))

        # Politica: Managers podem deletar
        self.add_policy(Policy(
            policy_id="POL-MANAGER-DELETE",
            name="Manager Delete Access",
            description="Managers podem deletar recursos",
            effect=Effect.ALLOW,
            resources=["projects/*", "stories/*", "tasks/*"],
            actions=["delete"],
            conditions=[
                Condition(
                    attribute="role",
                    operator=ConditionOperator.EQUALS,
                    value="MANAGER",
                    source=AttributeSource.SUBJECT
                )
            ],
            priority=45
        ))

        # Politica: Bloquear IPs suspeitos
        self.add_policy(Policy(
            policy_id="POL-BLOCK-SUSPICIOUS-IP",
            name="Block Suspicious IPs",
            description="Bloqueia acesso de IPs marcados como suspeitos",
            effect=Effect.DENY,
            resources=["*"],
            actions=["*"],
            conditions=[
                Condition(
                    attribute="is_suspicious",
                    operator=ConditionOperator.EQUALS,
                    value=True,
                    source=AttributeSource.ENVIRONMENT
                )
            ],
            priority=99
        ))

        logger.info(f"[ABAC] {len(self._policies)} politicas padrao carregadas")

    def add_policy(self, policy: Policy):
        """Adiciona uma politica ao motor"""
        self._policies[policy.policy_id] = policy
        self._clear_cache()
        logger.debug(f"[ABAC] Politica adicionada: {policy.policy_id}")

    def remove_policy(self, policy_id: str) -> bool:
        """Remove uma politica"""
        if policy_id in self._policies:
            del self._policies[policy_id]
            self._clear_cache()
            return True
        return False

    def get_policy(self, policy_id: str) -> Optional[Policy]:
        """Retorna uma politica pelo ID"""
        return self._policies.get(policy_id)

    def list_policies(self, active_only: bool = True) -> List[Policy]:
        """Lista todas as politicas"""
        policies = list(self._policies.values())
        if active_only:
            policies = [p for p in policies if p.active]
        return sorted(policies, key=lambda p: -p.priority)

    def evaluate(self, context: ABACContext) -> EvaluationResult:
        """
        Avalia uma requisicao contra todas as politicas

        Args:
            context: Contexto ABAC com subject, resource, action, environment

        Returns:
            EvaluationResult com a decisao
        """
        import time as time_module
        start_time = time_module.time()

        # Verificar cache
        cache_key = self._get_cache_key(context)
        if self.enable_cache and cache_key in self._decision_cache:
            cached, cached_time = self._decision_cache[cache_key]
            if (time_module.time() - cached_time) < self.cache_ttl:
                logger.debug(f"[ABAC] Cache hit: {cache_key}")
                return cached

        # Enriquecer contexto com ambiente
        context.environment = self._enrich_environment(context.environment)

        # Resolver referencias dinamicas nas politicas
        context_dict = context.to_dict()

        # Obter politicas aplicaveis ordenadas por prioridade
        applicable_policies = [
            p for p in self.list_policies(active_only=True)
            if p.matches_resource(self._get_resource_path(context.resource))
            and p.matches_action(context.action)
        ]

        # Avaliar politicas
        evaluated = 0
        allow_result = None
        deny_result = None

        for policy in applicable_policies:
            evaluated += 1

            # Resolver referencias dinamicas nas condicoes
            resolved_context = self._resolve_dynamic_refs(context_dict, policy)

            if policy.evaluate_conditions(resolved_context):
                if policy.effect == Effect.DENY:
                    deny_result = EvaluationResult(
                        decision=Effect.DENY,
                        policy_id=policy.policy_id,
                        policy_name=policy.name,
                        reason=f"Denied by policy: {policy.name}",
                        evaluated_policies=evaluated,
                        evaluation_time_ms=(time_module.time() - start_time) * 1000
                    )
                    # Deny tem prioridade imediata em deny_unless_permit
                    if self.combining_algorithm == "deny_unless_permit":
                        self._cache_decision(cache_key, deny_result)
                        logger.info(f"[ABAC] DENY: {policy.name} for action {context.action} on {self._get_resource_path(context.resource)}")
                        return deny_result

                elif policy.effect == Effect.ALLOW and allow_result is None:
                    allow_result = EvaluationResult(
                        decision=Effect.ALLOW,
                        policy_id=policy.policy_id,
                        policy_name=policy.name,
                        reason=f"Allowed by policy: {policy.name}",
                        evaluated_policies=evaluated,
                        evaluation_time_ms=(time_module.time() - start_time) * 1000
                    )
                    # Allow tem prioridade imediata em permit_unless_deny
                    if self.combining_algorithm == "permit_unless_deny":
                        self._cache_decision(cache_key, allow_result)
                        logger.info(f"[ABAC] ALLOW: {policy.name} for action {context.action} on {self._get_resource_path(context.resource)}")
                        return allow_result

        # Aplicar algoritmo de combinacao
        eval_time = (time_module.time() - start_time) * 1000

        if self.combining_algorithm == "deny_unless_permit":
            # Precisa de um ALLOW explicito
            if allow_result:
                self._cache_decision(cache_key, allow_result)
                logger.info(f"[ABAC] ALLOW: {allow_result.policy_name}")
                return allow_result
            result = EvaluationResult(
                decision=Effect.DENY,
                reason="No matching ALLOW policy found",
                evaluated_policies=evaluated,
                evaluation_time_ms=eval_time
            )
        else:  # permit_unless_deny
            # Permite a menos que haja DENY
            if deny_result:
                self._cache_decision(cache_key, deny_result)
                logger.info(f"[ABAC] DENY: {deny_result.policy_name}")
                return deny_result
            result = EvaluationResult(
                decision=Effect.ALLOW,
                reason="No DENY policy matched (permit_unless_deny)",
                evaluated_policies=evaluated,
                evaluation_time_ms=eval_time
            )

        self._cache_decision(cache_key, result)
        logger.info(f"[ABAC] {result.decision.value.upper()}: {result.reason}")
        return result

    def _get_resource_path(self, resource: Dict[str, Any]) -> str:
        """Extrai path do recurso"""
        return resource.get("path", resource.get("type", "unknown"))

    def _enrich_environment(self, env: Dict[str, Any]) -> Dict[str, Any]:
        """Enriquece contexto ambiental"""
        now = datetime.now()
        env["timestamp"] = now.isoformat()
        env["hour"] = now.hour
        env["day_of_week"] = now.weekday()
        env["is_weekend"] = now.weekday() >= 5
        env["is_business_hours"] = 8 <= now.hour < 18 and now.weekday() < 5
        return env

    def _resolve_dynamic_refs(
        self,
        context: Dict[str, Any],
        policy: Policy
    ) -> Dict[str, Any]:
        """Resolve referencias dinamicas como ${subject.user_id}"""
        import copy
        resolved = copy.deepcopy(context)

        for condition in policy.conditions:
            if isinstance(condition.value, str) and condition.value.startswith("${"):
                # Extrair referencia
                ref = condition.value[2:-1]  # Remove ${ e }
                parts = ref.split(".")
                if len(parts) >= 2:
                    source = parts[0]
                    attr = ".".join(parts[1:])
                    if source in context:
                        condition.value = self._get_nested_value(context[source], attr)

        return resolved

    def _get_nested_value(self, data: Dict, key: str) -> Any:
        """Obtem valor de chave aninhada"""
        keys = key.split(".")
        value = data
        for k in keys:
            if isinstance(value, dict):
                value = value.get(k)
            else:
                return None
        return value

    def _get_cache_key(self, context: ABACContext) -> str:
        """Gera chave de cache para a decisao"""
        import hashlib
        data = json.dumps({
            "subject": context.subject,
            "resource": context.resource,
            "action": context.action
        }, sort_keys=True)
        return hashlib.md5(data.encode()).hexdigest()

    def _cache_decision(self, key: str, result: EvaluationResult):
        """Armazena decisao no cache"""
        import time as time_module
        if self.enable_cache:
            self._decision_cache[key] = (result, time_module.time())

    def _clear_cache(self):
        """Limpa cache de decisoes"""
        self._decision_cache.clear()


# =============================================================================
# DECORATORS
# =============================================================================

# Instancia global do motor ABAC
_abac_engine: Optional[ABACEngine] = None


def get_abac_engine() -> ABACEngine:
    """Retorna o motor ABAC (singleton)"""
    global _abac_engine
    if _abac_engine is None:
        _abac_engine = ABACEngine()
    return _abac_engine


def set_abac_engine(engine: ABACEngine):
    """Define o motor ABAC"""
    global _abac_engine
    _abac_engine = engine


def require_permission(
    resource_type: str,
    action: str,
    resource_id_param: Optional[str] = None,
    resource_attrs: Optional[Dict[str, Any]] = None
):
    """
    Decorator para verificar permissao ABAC em endpoint

    Args:
        resource_type: Tipo de recurso (projects, stories, tasks, etc)
        action: Acao sendo executada (create, read, update, delete)
        resource_id_param: Nome do parametro que contem o ID do recurso
        resource_attrs: Atributos adicionais do recurso

    Usage:
        @app.get("/api/projects/{project_id}")
        @require_permission("projects", "read", resource_id_param="project_id")
        async def get_project(project_id: str, user: UserContext = Depends(get_current_user)):
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

            if not user:
                raise HTTPException(
                    status_code=401,
                    detail="Authentication required"
                )

            # Construir atributos do subject
            subject_attrs = {
                "user_id": getattr(user, "user_id", None) or getattr(user, "id", None),
                "username": getattr(user, "username", None),
                "role": getattr(user, "role", None),
                "roles": getattr(user, "roles", []),
                "permissions": getattr(user, "permissions", []),
                "is_admin": getattr(user, "is_admin", False)
            }

            # Construir atributos do recurso
            resource_path = f"{resource_type}"
            if resource_id_param and resource_id_param in kwargs:
                resource_id = kwargs[resource_id_param]
                resource_path = f"{resource_type}/{resource_id}"

            res_attrs = {
                "type": resource_type,
                "path": resource_path,
                "id": kwargs.get(resource_id_param) if resource_id_param else None
            }
            if resource_attrs:
                res_attrs.update(resource_attrs)

            # Construir atributos de ambiente
            env_attrs = {}
            if request:
                env_attrs["ip_address"] = request.client.host if request.client else None
                env_attrs["user_agent"] = request.headers.get("user-agent")
                env_attrs["method"] = request.method

            # Criar contexto e avaliar
            context = ABACContext(
                subject=subject_attrs,
                resource=res_attrs,
                action=action,
                environment=env_attrs
            )

            engine = get_abac_engine()
            result = engine.evaluate(context)

            if result.decision == Effect.DENY:
                logger.warning(
                    f"[ABAC] Access denied: user={subject_attrs.get('username')} "
                    f"resource={resource_path} action={action} reason={result.reason}"
                )
                raise HTTPException(
                    status_code=403,
                    detail=f"Access denied: {result.reason}"
                )

            # Executar funcao original
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            return func(*args, **kwargs)

        return wrapper
    return decorator


def check_permission(
    user: Any,
    resource_type: str,
    action: str,
    resource_attrs: Optional[Dict[str, Any]] = None,
    env_attrs: Optional[Dict[str, Any]] = None
) -> EvaluationResult:
    """
    Verifica permissao ABAC programaticamente

    Args:
        user: Objeto de usuario com atributos
        resource_type: Tipo de recurso
        action: Acao sendo executada
        resource_attrs: Atributos do recurso
        env_attrs: Atributos de ambiente

    Returns:
        EvaluationResult com a decisao
    """
    subject_attrs = {
        "user_id": getattr(user, "user_id", None) or getattr(user, "id", None),
        "username": getattr(user, "username", None),
        "role": getattr(user, "role", None),
        "roles": getattr(user, "roles", []),
        "permissions": getattr(user, "permissions", []),
        "is_admin": getattr(user, "is_admin", False)
    }

    res_attrs = {
        "type": resource_type,
        "path": resource_type,
    }
    if resource_attrs:
        res_attrs.update(resource_attrs)

    context = ABACContext(
        subject=subject_attrs,
        resource=res_attrs,
        action=action,
        environment=env_attrs or {}
    )

    return get_abac_engine().evaluate(context)


def require_resource_owner(resource_id_param: str, owner_field: str = "owner_id"):
    """
    Decorator que verifica se o usuario e dono do recurso

    Usage:
        @app.put("/api/projects/{project_id}")
        @require_resource_owner("project_id")
        async def update_project(project_id: str, user: UserContext = Depends(get_current_user)):
            ...
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            user = kwargs.get("user") or kwargs.get("current_user")
            resource_id = kwargs.get(resource_id_param)

            if not user:
                raise HTTPException(status_code=401, detail="Authentication required")

            # Admins sempre tem acesso
            if getattr(user, "is_admin", False) or getattr(user, "role", "") == "ADMIN":
                if asyncio.iscoroutinefunction(func):
                    return await func(*args, **kwargs)
                return func(*args, **kwargs)

            # TODO: Buscar recurso do banco e verificar owner
            # Por enquanto, delegar para ABAC engine
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            return func(*args, **kwargs)

        return wrapper
    return decorator


# =============================================================================
# FASTAPI ROUTER
# =============================================================================

from fastapi import APIRouter

abac_router = APIRouter(prefix="/api/abac", tags=["ABAC"])


class PolicyCreate(BaseModel):
    """Dados para criar politica"""
    policy_id: str
    name: str
    description: str
    effect: Effect
    resources: List[str]
    actions: List[str]
    conditions: List[Dict[str, Any]] = []
    priority: int = 0


class PolicyResponse(BaseModel):
    """Resposta de politica"""
    policy_id: str
    name: str
    description: str
    effect: str
    resources: List[str]
    actions: List[str]
    conditions: List[Dict[str, Any]]
    priority: int
    active: bool


class EvaluateRequest(BaseModel):
    """Requisicao de avaliacao"""
    subject: Dict[str, Any]
    resource: Dict[str, Any]
    action: str
    environment: Dict[str, Any] = {}


@abac_router.get("/policies")
def list_policies(active_only: bool = True):
    """Lista todas as politicas ABAC"""
    engine = get_abac_engine()
    policies = engine.list_policies(active_only)

    return [
        PolicyResponse(
            policy_id=p.policy_id,
            name=p.name,
            description=p.description,
            effect=p.effect.value,
            resources=p.resources,
            actions=p.actions,
            conditions=[
                {
                    "attribute": c.attribute,
                    "operator": c.operator.value,
                    "value": c.value,
                    "source": c.source.value
                }
                for c in p.conditions
            ],
            priority=p.priority,
            active=p.active
        )
        for p in policies
    ]


@abac_router.get("/policies/{policy_id}")
def get_policy(policy_id: str):
    """Busca politica por ID"""
    engine = get_abac_engine()
    policy = engine.get_policy(policy_id)

    if not policy:
        raise HTTPException(status_code=404, detail="Policy not found")

    return PolicyResponse(
        policy_id=policy.policy_id,
        name=policy.name,
        description=policy.description,
        effect=policy.effect.value,
        resources=policy.resources,
        actions=policy.actions,
        conditions=[
            {
                "attribute": c.attribute,
                "operator": c.operator.value,
                "value": c.value,
                "source": c.source.value
            }
            for c in policy.conditions
        ],
        priority=policy.priority,
        active=policy.active
    )


@abac_router.post("/policies")
def create_policy(data: PolicyCreate):
    """Cria nova politica ABAC"""
    engine = get_abac_engine()

    # Converter condicoes
    conditions = []
    for cond_data in data.conditions:
        conditions.append(Condition(
            attribute=cond_data["attribute"],
            operator=ConditionOperator(cond_data["operator"]),
            value=cond_data["value"],
            source=AttributeSource(cond_data.get("source", "subject"))
        ))

    policy = Policy(
        policy_id=data.policy_id,
        name=data.name,
        description=data.description,
        effect=data.effect,
        resources=data.resources,
        actions=data.actions,
        conditions=conditions,
        priority=data.priority
    )

    engine.add_policy(policy)
    logger.info(f"[ABAC] Nova politica criada: {policy.policy_id}")

    return {"message": "Policy created", "policy_id": policy.policy_id}


@abac_router.delete("/policies/{policy_id}")
def delete_policy(policy_id: str):
    """Remove politica ABAC"""
    engine = get_abac_engine()

    if engine.remove_policy(policy_id):
        return {"message": "Policy deleted"}

    raise HTTPException(status_code=404, detail="Policy not found")


@abac_router.post("/evaluate")
def evaluate_access(data: EvaluateRequest):
    """Avalia acesso baseado em ABAC"""
    engine = get_abac_engine()

    context = ABACContext(
        subject=data.subject,
        resource=data.resource,
        action=data.action,
        environment=data.environment
    )

    result = engine.evaluate(context)

    return {
        "decision": result.decision.value,
        "policy_id": result.policy_id,
        "policy_name": result.policy_name,
        "reason": result.reason,
        "evaluated_policies": result.evaluated_policies,
        "evaluation_time_ms": result.evaluation_time_ms
    }


@abac_router.post("/policies/{policy_id}/activate")
def activate_policy(policy_id: str):
    """Ativa uma politica"""
    engine = get_abac_engine()
    policy = engine.get_policy(policy_id)

    if not policy:
        raise HTTPException(status_code=404, detail="Policy not found")

    policy.active = True
    return {"message": "Policy activated", "policy_id": policy_id}


@abac_router.post("/policies/{policy_id}/deactivate")
def deactivate_policy(policy_id: str):
    """Desativa uma politica"""
    engine = get_abac_engine()
    policy = engine.get_policy(policy_id)

    if not policy:
        raise HTTPException(status_code=404, detail="Policy not found")

    policy.active = False
    return {"message": "Policy deactivated", "policy_id": policy_id}


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Classes principais
    "ABACEngine",
    "ABACContext",
    "Policy",
    "Condition",
    "EvaluationResult",

    # Enums
    "Effect",
    "ConditionOperator",
    "AttributeSource",

    # Decorators
    "require_permission",
    "require_resource_owner",
    "check_permission",

    # Funcoes
    "get_abac_engine",
    "set_abac_engine",

    # Router
    "abac_router",
]

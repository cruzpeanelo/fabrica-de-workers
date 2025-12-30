# -*- coding: utf-8 -*-
"""
Cache Decorators
================
Decoradores para caching automatico de funcoes.

Issue #365 - Terminal A
"""

import asyncio
import functools
import hashlib
import inspect
import json
import logging
from typing import Any, Callable, List, Optional, TypeVar, Union

from .cache_manager import CacheManager, CacheType, get_cache_manager

logger = logging.getLogger(__name__)

F = TypeVar("F", bound=Callable[..., Any])


def _make_cache_key(
    func: Callable,
    args: tuple,
    kwargs: dict,
    key_prefix: str,
    key_builder: Optional[Callable] = None
) -> str:
    """
    Constroi chave de cache baseada nos argumentos da funcao.

    Args:
        func: Funcao sendo decorada
        args: Argumentos posicionais
        kwargs: Argumentos nomeados
        key_prefix: Prefixo da chave
        key_builder: Funcao customizada para construir chave

    Returns:
        Chave de cache
    """
    if key_builder:
        return key_builder(*args, **kwargs)

    # Constroi chave baseada nos argumentos
    sig = inspect.signature(func)
    bound = sig.bind(*args, **kwargs)
    bound.apply_defaults()

    # Remove 'self' e 'cls' se presentes
    params = dict(bound.arguments)
    params.pop("self", None)
    params.pop("cls", None)

    # Serializa parametros
    try:
        params_str = json.dumps(params, sort_keys=True, default=str)
    except (TypeError, ValueError):
        # Fallback para hash
        params_str = str(params)

    # Hash se muito longo
    if len(params_str) > 100:
        params_hash = hashlib.md5(params_str.encode()).hexdigest()[:16]
        return f"{key_prefix}:{params_hash}"

    return f"{key_prefix}:{params_str}"


def cached(
    ttl: int = 300,
    key_prefix: Optional[str] = None,
    cache_type: CacheType = CacheType.CUSTOM,
    tenant_id: Optional[str] = None,
    tenant_arg: Optional[str] = None,
    key_builder: Optional[Callable[..., str]] = None,
    tags: Optional[List[str]] = None,
    condition: Optional[Callable[..., bool]] = None,
    unless: Optional[Callable[[Any], bool]] = None,
    cache_manager: Optional[CacheManager] = None
):
    """
    Decorator para cache automatico de funcoes async.

    Args:
        ttl: TTL em segundos
        key_prefix: Prefixo da chave (default: nome da funcao)
        cache_type: Tipo de cache
        tenant_id: ID do tenant fixo
        tenant_arg: Nome do argumento que contem o tenant_id
        key_builder: Funcao customizada para construir chave
        tags: Tags adicionais
        condition: Funcao que determina se deve cachear (args, kwargs) -> bool
        unless: Funcao que determina se NAO deve cachear o resultado (result) -> bool
        cache_manager: Instancia de CacheManager (usa global se None)

    Exemplo:
        @cached(ttl=300, key_prefix="jira:issue")
        async def get_jira_issue(issue_key: str):
            ...

        @cached(ttl=60, tenant_arg="tenant_id")
        async def get_user_data(tenant_id: str, user_id: str):
            ...

        @cached(
            ttl=600,
            key_builder=lambda project_id, **kw: f"proj:{project_id}",
            unless=lambda result: result is None
        )
        async def get_project(project_id: str):
            ...
    """
    def decorator(func: F) -> F:
        # Verifica se funcao e async
        if not asyncio.iscoroutinefunction(func):
            raise TypeError("@cached can only be applied to async functions")

        prefix = key_prefix or func.__name__

        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            # Obtem cache manager
            cm = cache_manager or get_cache_manager()

            # Verifica condicao
            if condition and not condition(*args, **kwargs):
                return await func(*args, **kwargs)

            # Determina tenant_id
            effective_tenant = tenant_id
            if tenant_arg and tenant_arg in kwargs:
                effective_tenant = kwargs[tenant_arg]
            elif tenant_arg:
                # Tenta encontrar nos argumentos posicionais
                sig = inspect.signature(func)
                params = list(sig.parameters.keys())
                if tenant_arg in params:
                    idx = params.index(tenant_arg)
                    if idx < len(args):
                        effective_tenant = args[idx]

            # Constroi chave
            cache_key = _make_cache_key(func, args, kwargs, prefix, key_builder)

            # Tenta obter do cache
            try:
                cached_value = await cm.get(
                    cache_key,
                    cache_type=cache_type,
                    tenant_id=effective_tenant
                )

                if cached_value is not None:
                    logger.debug(f"Cache hit: {cache_key}")
                    return cached_value

            except Exception as e:
                logger.warning(f"Cache get error: {e}")

            # Executa funcao
            result = await func(*args, **kwargs)

            # Verifica unless
            if unless and unless(result):
                return result

            # Armazena no cache
            try:
                await cm.set(
                    cache_key,
                    result,
                    cache_type=cache_type,
                    tenant_id=effective_tenant,
                    ttl=ttl,
                    tags=tags
                )
                logger.debug(f"Cached: {cache_key} (TTL: {ttl}s)")

            except Exception as e:
                logger.warning(f"Cache set error: {e}")

            return result

        # Adiciona metodo para invalidar cache da funcao
        async def invalidate(*args, **kwargs):
            cm = cache_manager or get_cache_manager()
            cache_key = _make_cache_key(func, args, kwargs, prefix, key_builder)

            effective_tenant = tenant_id
            if tenant_arg and tenant_arg in kwargs:
                effective_tenant = kwargs[tenant_arg]

            return await cm.delete(
                cache_key,
                cache_type=cache_type,
                tenant_id=effective_tenant
            )

        wrapper.invalidate = invalidate
        wrapper.cache_key_prefix = prefix

        return wrapper

    return decorator


def cached_property(
    ttl: int = 300,
    key_prefix: Optional[str] = None
):
    """
    Decorator para cache de property em classes.

    Cacheia o resultado da property por instancia.

    Exemplo:
        class MyClass:
            @cached_property(ttl=60)
            async def expensive_data(self):
                ...
    """
    def decorator(func: F) -> F:
        attr_name = f"_cached_{func.__name__}"
        time_attr = f"_cached_{func.__name__}_time"

        @functools.wraps(func)
        async def wrapper(self):
            import time as time_module

            # Verifica cache local
            cached_time = getattr(self, time_attr, 0)
            if time_module.time() - cached_time < ttl:
                cached_value = getattr(self, attr_name, None)
                if cached_value is not None:
                    return cached_value

            # Executa e cacheia
            result = await func(self)
            setattr(self, attr_name, result)
            setattr(self, time_attr, time_module.time())

            return result

        # Adiciona metodo para invalidar
        def invalidate(self):
            setattr(self, time_attr, 0)

        wrapper.invalidate = invalidate

        return wrapper

    return decorator


def cache_aside(
    ttl: int = 300,
    key_prefix: Optional[str] = None,
    cache_type: CacheType = CacheType.CUSTOM,
    fallback: Optional[Callable[..., Any]] = None
):
    """
    Decorator implementando pattern Cache-Aside.

    Tenta cache primeiro, se miss chama funcao e cacheia resultado.
    Se funcao falhar, retorna fallback se disponivel.

    Exemplo:
        @cache_aside(ttl=300, fallback=lambda: {"default": True})
        async def get_config(config_id: str):
            ...
    """
    def decorator(func: F) -> F:
        prefix = key_prefix or func.__name__

        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            cm = get_cache_manager()

            cache_key = _make_cache_key(func, args, kwargs, prefix, None)

            # Tenta cache
            try:
                cached_value = await cm.get(cache_key, cache_type=cache_type)
                if cached_value is not None:
                    return cached_value
            except Exception as e:
                logger.warning(f"Cache aside get error: {e}")

            # Tenta funcao
            try:
                result = await func(*args, **kwargs)

                # Cacheia resultado
                try:
                    await cm.set(cache_key, result, cache_type=cache_type, ttl=ttl)
                except Exception as e:
                    logger.warning(f"Cache aside set error: {e}")

                return result

            except Exception as e:
                logger.error(f"Function error in cache_aside: {e}")

                if fallback:
                    if callable(fallback):
                        return fallback(*args, **kwargs)
                    return fallback

                raise

        return wrapper

    return decorator


def write_through(
    ttl: int = 300,
    key_prefix: Optional[str] = None,
    cache_type: CacheType = CacheType.CUSTOM,
    key_arg: str = "id"
):
    """
    Decorator implementando pattern Write-Through.

    Apos gravar dados, atualiza o cache automaticamente.

    Exemplo:
        @write_through(ttl=300, key_arg="user_id")
        async def update_user(user_id: str, data: dict):
            # Atualiza no banco
            return updated_user

        # Resultado sera cacheado automaticamente
    """
    def decorator(func: F) -> F:
        prefix = key_prefix or func.__name__

        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            # Executa funcao primeiro
            result = await func(*args, **kwargs)

            # Obtem chave do resultado ou argumentos
            cache_key = None

            if isinstance(result, dict) and key_arg in result:
                cache_key = f"{prefix}:{result[key_arg]}"
            elif key_arg in kwargs:
                cache_key = f"{prefix}:{kwargs[key_arg]}"
            else:
                # Tenta encontrar nos args
                sig = inspect.signature(func)
                params = list(sig.parameters.keys())
                if key_arg in params:
                    idx = params.index(key_arg)
                    if idx < len(args):
                        cache_key = f"{prefix}:{args[idx]}"

            # Cacheia resultado
            if cache_key and result is not None:
                try:
                    cm = get_cache_manager()
                    await cm.set(cache_key, result, cache_type=cache_type, ttl=ttl)
                    logger.debug(f"Write-through cache: {cache_key}")
                except Exception as e:
                    logger.warning(f"Write-through cache error: {e}")

            return result

        return wrapper

    return decorator


def invalidate_cache(
    key_pattern: Optional[str] = None,
    cache_type: Optional[CacheType] = None,
    tags: Optional[List[str]] = None,
    tenant_arg: Optional[str] = None
):
    """
    Decorator para invalidar cache apos execucao.

    Util para operacoes de update/delete.

    Exemplo:
        @invalidate_cache(key_pattern="user:*", tags=["user"])
        async def delete_user(user_id: str):
            ...
    """
    def decorator(func: F) -> F:
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            # Executa funcao primeiro
            result = await func(*args, **kwargs)

            cm = get_cache_manager()

            # Determina tenant
            effective_tenant = None
            if tenant_arg and tenant_arg in kwargs:
                effective_tenant = kwargs[tenant_arg]

            # Invalida por pattern
            if key_pattern:
                try:
                    pattern = key_pattern
                    # Substitui placeholders
                    for key, value in kwargs.items():
                        pattern = pattern.replace(f"{{{key}}}", str(value))

                    await cm._backend.invalidate_by_pattern(pattern)
                except Exception as e:
                    logger.warning(f"Invalidate by pattern error: {e}")

            # Invalida por tipo
            if cache_type:
                try:
                    await cm.invalidate_type(cache_type)
                except Exception as e:
                    logger.warning(f"Invalidate by type error: {e}")

            # Invalida por tags
            if tags:
                for tag in tags:
                    try:
                        await cm.invalidate_by_tag(tag)
                    except Exception as e:
                        logger.warning(f"Invalidate by tag error: {e}")

            return result

        return wrapper

    return decorator

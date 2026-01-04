# -*- coding: utf-8 -*-
"""
Cache Decorators - Plataforma E
===============================

Decorators for easy caching of functions and methods.
"""

import hashlib
import json
import functools
from typing import Optional, List, Callable, Any


def _make_key(func: Callable, args: tuple, kwargs: dict) -> str:
    """Generate cache key from function and arguments."""
    key_parts = [func.__module__, func.__qualname__]
    
    # Add args
    for arg in args:
        try:
            key_parts.append(str(hash(arg)))
        except TypeError:
            key_parts.append(str(arg))
    
    # Add sorted kwargs
    for k, v in sorted(kwargs.items()):
        try:
            key_parts.append(f"{k}={hash(v)}")
        except TypeError:
            key_parts.append(f"{k}={v}")
    
    key_str = ":".join(key_parts)
    return hashlib.md5(key_str.encode()).hexdigest()


def cached(ttl: Optional[int] = None, 
           category: str = "default",
           key_prefix: str = "",
           tags: List[str] = None):
    """
    Decorator to cache function results.
    
    Args:
        ttl: Time-to-live in seconds
        category: Category for default TTL
        key_prefix: Prefix for cache key
        tags: Tags for grouped invalidation
    
    Usage:
        @cached(ttl=300)
        def get_user(user_id: int):
            return db.query(User).get(user_id)
        
        @cached(category="dashboard")
        def get_metrics():
            return compute_metrics()
    """
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(*args, **kwargs) -> Any:
            from .cache_manager import get_cache
            
            cache = get_cache()
            
            # Generate cache key
            key = _make_key(func, args, kwargs)
            if key_prefix:
                key = f"{key_prefix}:{key}"
            
            # Try to get from cache
            result = cache.get(key)
            if result is not None:
                return result
            
            # Compute and cache
            result = func(*args, **kwargs)
            cache.set(key, result, ttl=ttl, category=category, tags=tags)
            
            return result
        
        # Add cache control methods
        wrapper.cache_clear = lambda: None  # Will be implemented
        wrapper.cache_key = lambda *a, **kw: _make_key(func, a, kw)
        
        return wrapper
    
    return decorator


def cache_invalidate(*tags: str):
    """
    Decorator to invalidate cache tags after function execution.
    
    Usage:
        @cache_invalidate("users", "dashboard")
        def update_user(user_id: int, data: dict):
            db.update(user_id, data)
    """
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(*args, **kwargs) -> Any:
            from .cache_manager import get_cache
            
            result = func(*args, **kwargs)
            
            # Invalidate tags
            cache = get_cache()
            for tag in tags:
                cache.invalidate_tag(tag)
            
            return result
        
        return wrapper
    
    return decorator


def cached_property(ttl: Optional[int] = 300):
    """
    Decorator for cached class properties.
    
    Usage:
        class User:
            @cached_property(ttl=600)
            def full_name(self):
                return f"{self.first_name} {self.last_name}"
    """
    def decorator(func: Callable) -> property:
        @functools.wraps(func)
        def wrapper(self) -> Any:
            from .cache_manager import get_cache
            
            cache = get_cache()
            key = f"prop:{id(self)}:{func.__name__}"
            
            result = cache.get(key)
            if result is not None:
                return result
            
            result = func(self)
            cache.set(key, result, ttl=ttl)
            
            return result
        
        return property(wrapper)
    
    return decorator

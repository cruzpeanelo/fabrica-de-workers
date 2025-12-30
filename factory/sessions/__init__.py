# -*- coding: utf-8 -*-
"""
Sessions Module - Issue #206
============================
Redis-based session management for stateless deployments.
"""

from .redis_session import RedisSessionStore, get_session_store

__all__ = ["RedisSessionStore", "get_session_store"]

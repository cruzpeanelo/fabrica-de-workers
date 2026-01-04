# -*- coding: utf-8 -*-
"""
Tests for Cache Manager
Plataforma E v6.5

Tests for Issue #447: Cache Manager
"""

import pytest
import time


class TestCacheStats:
    def test_stats_creation(self):
        from factory.cache.cache_manager import CacheStats
        stats = CacheStats()
        assert stats.hits == 0

    def test_hit_rate(self):
        from factory.cache.cache_manager import CacheStats
        stats = CacheStats(hits=75, misses=25)
        assert stats.hit_rate == 75.0


class TestCacheEntry:
    def test_entry_creation(self):
        from factory.cache.cache_manager import CacheEntry
        entry = CacheEntry(key="test", value="data")
        assert entry.key == "test"

    def test_not_expired_without_ttl(self):
        from factory.cache.cache_manager import CacheEntry
        entry = CacheEntry(key="test", value="data", ttl=None)
        assert entry.is_expired is False


class TestMemoryBackend:
    @pytest.fixture
    def backend(self):
        from factory.cache.cache_manager import MemoryBackend
        return MemoryBackend()

    def test_set_and_get(self, backend):
        backend.set("key1", "value1")
        assert backend.get("key1") == "value1"

    def test_get_nonexistent(self, backend):
        assert backend.get("nonexistent") is None

    def test_delete(self, backend):
        backend.set("key1", "value1")
        assert backend.delete("key1") is True
        assert backend.get("key1") is None

    def test_exists(self, backend):
        backend.set("key1", "value1")
        assert backend.exists("key1") is True

    def test_clear(self, backend):
        backend.set("key1", "value1")
        backend.clear()
        assert backend.get("key1") is None

    def test_keys(self, backend):
        backend.set("user:1", "data1")
        backend.set("user:2", "data2")
        all_keys = backend.keys()
        assert len(all_keys) == 2

    def test_delete_by_tag(self, backend):
        backend.set("key1", "value1", tags=["tag1"])
        backend.set("key2", "value2", tags=["tag1"])
        count = backend.delete_by_tag("tag1")
        assert count == 2


class TestCacheManager:
    @pytest.fixture
    def cache(self):
        from factory.cache.cache_manager import CacheManager
        return CacheManager(backend="memory")

    def test_set_and_get(self, cache):
        cache.set("user:123", {"name": "John"})
        result = cache.get("user:123")
        assert result == {"name": "John"}

    def test_get_with_default(self, cache):
        result = cache.get("nonexistent", default="default")
        assert result == "default"

    def test_delete(self, cache):
        cache.set("key1", "value1")
        cache.delete("key1")
        assert cache.get("key1") is None

    def test_exists(self, cache):
        cache.set("key1", "value1")
        assert cache.exists("key1") is True

    def test_get_or_set(self, cache):
        call_count = 0
        def factory():
            nonlocal call_count
            call_count += 1
            return "computed"
        
        result1 = cache.get_or_set("key1", factory)
        result2 = cache.get_or_set("key1", factory)
        assert result1 == "computed"
        assert call_count == 1

    def test_stats(self, cache):
        cache.set("key1", "value1")
        cache.get("key1")
        cache.get("missing")
        stats = cache.get_stats()
        assert stats["hits"] == 1
        assert stats["misses"] == 1

    def test_get_info(self, cache):
        cache.set("key1", "value1")
        info = cache.get_info()
        assert "backend" in info
        assert "stats" in info


class TestGlobalCache:
    def test_get_cache_singleton(self):
        from factory.cache.cache_manager import get_cache
        cache1 = get_cache()
        cache2 = get_cache()
        assert cache1 is cache2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

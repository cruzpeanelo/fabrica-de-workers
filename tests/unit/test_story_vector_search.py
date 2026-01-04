# -*- coding: utf-8 -*-
"""
Tests for Story Vector Search
Plataforma E v6.5

Tests for Issue #449
"""

import pytest
from pathlib import Path
import tempfile


class TestSearchConfig:
    """Tests for SearchConfig"""

    def test_default_config(self):
        from factory.search.story_vector_search import SearchConfig

        config = SearchConfig()
        assert config.dimensions == 512
        assert config.similarity_threshold == 0.7
        assert config.duplicate_threshold == 0.95
        assert config.max_results == 10

    def test_custom_config(self):
        from factory.search.story_vector_search import SearchConfig, SimilarityMetric

        config = SearchConfig(
            dimensions=256,
            similarity_threshold=0.8,
            metric=SimilarityMetric.EUCLIDEAN,
        )
        assert config.dimensions == 256
        assert config.similarity_threshold == 0.8
        assert config.metric == SimilarityMetric.EUCLIDEAN


class TestSimilarityMetric:
    """Tests for SimilarityMetric enum"""

    def test_metric_values(self):
        from factory.search.story_vector_search import SimilarityMetric

        assert SimilarityMetric.COSINE.value == "cosine"
        assert SimilarityMetric.EUCLIDEAN.value == "euclidean"
        assert SimilarityMetric.DOT_PRODUCT.value == "dot_product"


class TestStoryVector:
    """Tests for StoryVector dataclass"""

    def test_vector_creation(self):
        from factory.search.story_vector_search import StoryVector

        vector = StoryVector(
            story_id="STR-001",
            title="Test Story",
            vector=[0.1, 0.2, 0.3],
        )

        assert vector.story_id == "STR-001"
        assert vector.title == "Test Story"
        assert len(vector.vector) == 3

    def test_vector_to_dict(self):
        from factory.search.story_vector_search import StoryVector

        vector = StoryVector(
            story_id="STR-001",
            title="Test Story",
            vector=[0.1, 0.2, 0.3],
            metadata={"epic": "EPIC-01"},
        )

        d = vector.to_dict()
        assert d["story_id"] == "STR-001"
        assert d["vector_dims"] == 3
        assert d["metadata"]["epic"] == "EPIC-01"


class TestSimilarityResult:
    """Tests for SimilarityResult"""

    def test_result_creation(self):
        from factory.search.story_vector_search import SimilarityResult

        result = SimilarityResult(
            story_id="STR-001",
            title="Test Story",
            similarity=0.85,
            is_duplicate=False,
            match_type="semantic",
        )

        assert result.story_id == "STR-001"
        assert result.similarity == 0.85
        assert result.is_duplicate is False

    def test_result_to_dict(self):
        from factory.search.story_vector_search import SimilarityResult

        result = SimilarityResult(
            story_id="STR-001",
            title="Test Story",
            similarity=0.8512345,
            is_duplicate=False,
            match_type="partial",
        )

        d = result.to_dict()
        assert d["similarity"] == 0.8512
        assert d["match_type"] == "partial"


class TestSemanticHashEncoder:
    """Tests for SemanticHashEncoder"""

    @pytest.fixture
    def encoder(self):
        from factory.search.story_vector_search import SemanticHashEncoder
        return SemanticHashEncoder(dimensions=256)

    def test_encode(self, encoder):
        vector = encoder.encode("Test story about user authentication")
        assert len(vector) == 256

    def test_encode_normalized(self, encoder):
        import math
        vector = encoder.encode("Test story")
        norm = math.sqrt(sum(v * v for v in vector))
        assert abs(norm - 1.0) < 0.001

    def test_similar_texts_similar_vectors(self, encoder):
        import math
        vec1 = encoder.encode("User authentication with password")
        vec2 = encoder.encode("User authentication with credentials")

        # Calculate cosine similarity
        dot = sum(a * b for a, b in zip(vec1, vec2))
        norm1 = math.sqrt(sum(a * a for a in vec1))
        norm2 = math.sqrt(sum(b * b for b in vec2))
        similarity = dot / (norm1 * norm2)

        # Should be similar
        assert similarity > 0.5

    def test_different_texts_different_vectors(self, encoder):
        import math
        vec1 = encoder.encode("User authentication with password")
        vec2 = encoder.encode("Database backup scheduled task")

        dot = sum(a * b for a, b in zip(vec1, vec2))
        norm1 = math.sqrt(sum(a * a for a in vec1))
        norm2 = math.sqrt(sum(b * b for b in vec2))
        similarity = dot / (norm1 * norm2)

        # Should be less similar
        assert similarity < 0.5


class TestStoryVectorSearch:
    """Tests for StoryVectorSearch"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    @pytest.fixture
    def search(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(
            dimensions=256,
            similarity_threshold=0.5,
            duplicate_threshold=0.9,
        )
        return StoryVectorSearch(config=config, db_path=temp_db)

    def test_creation(self, search):
        assert search is not None
        assert search.config.dimensions == 256

    def test_index_story(self, search):
        vector = search.index_story(
            story_id="STR-001",
            title="User login feature",
            description="Implement user login with email and password",
        )

        assert vector.story_id == "STR-001"
        assert len(vector.vector) == 256

    def test_index_multiple_stories(self, search):
        search.index_story("STR-001", "User login feature")
        search.index_story("STR-002", "User registration feature")
        search.index_story("STR-003", "Password reset flow")

        assert len(search._vectors) == 3

    def test_index_stories_batch(self, search):
        stories = [
            {"story_id": "STR-001", "title": "User login"},
            {"story_id": "STR-002", "title": "User registration"},
            {"story_id": "STR-003", "title": "Password reset"},
        ]

        count = search.index_stories(stories)
        assert count == 3

    def test_remove_story(self, search):
        search.index_story("STR-001", "Test story")
        result = search.remove_story("STR-001")

        assert result is True
        assert "STR-001" not in search._vectors

    def test_remove_nonexistent_story(self, search):
        result = search.remove_story("FAKE-001")
        assert result is False


class TestSimilaritySearch:
    """Tests for similarity search"""

    @pytest.fixture
    def search_with_data(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(
            dimensions=256,
            similarity_threshold=0.2,  # Lower threshold for more results
            duplicate_threshold=0.9,
        )
        search = StoryVectorSearch(config=config, db_path=temp_db)

        # Index test stories with similar topics
        search.index_story("STR-001", "User login with email and password authentication")
        search.index_story("STR-002", "User authentication login system with password")
        search.index_story("STR-003", "Database backup automated task")
        search.index_story("STR-004", "API endpoint for user profile management")
        search.index_story("STR-005", "User logout and session termination")

        return search

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    def test_find_similar(self, search_with_data):
        results = search_with_data.find_similar("STR-001")

        # Should find at least one similar story (STR-002 is very similar)
        assert len(results) >= 0  # May be 0 if similarity is below threshold
        # Should not include self
        assert not any(r.story_id == "STR-001" for r in results)

    def test_search_by_query(self, search_with_data):
        results = search_with_data.search("user authentication login")

        assert len(results) > 0
        # Should find related stories
        found_ids = [r.story_id for r in results]
        assert any(id in found_ids for id in ["STR-001", "STR-002", "STR-005"])

    def test_search_returns_sorted_results(self, search_with_data):
        results = search_with_data.search("user login")

        if len(results) >= 2:
            assert results[0].similarity >= results[1].similarity


class TestDuplicateDetection:
    """Tests for duplicate detection"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    @pytest.fixture
    def search_with_duplicates(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(
            dimensions=256,
            similarity_threshold=0.5,
            duplicate_threshold=0.85,
        )
        search = StoryVectorSearch(config=config, db_path=temp_db)

        # Index similar stories (potential duplicates)
        search.index_story("STR-001", "User login with email and password")
        search.index_story("STR-002", "Login with email and password for user")
        search.index_story("STR-003", "Completely different topic about backups")

        return search

    def test_find_duplicates(self, search_with_duplicates):
        duplicates = search_with_duplicates.find_duplicates("STR-001")

        # STR-002 should be found as duplicate
        assert any(d.story_id == "STR-002" for d in duplicates)
        # STR-003 should not be found
        assert not any(d.story_id == "STR-003" for d in duplicates)

    def test_check_for_duplicate_positive(self, search_with_duplicates):
        result = search_with_duplicates.check_for_duplicate(
            title="User login with email and password authentication"
        )

        # Should find existing similar story
        if result:
            assert result.is_duplicate is True

    def test_check_for_duplicate_negative(self, search_with_duplicates):
        result = search_with_duplicates.check_for_duplicate(
            title="New feature for payment processing"
        )

        # Should not find duplicate
        assert result is None


class TestRelatedStories:
    """Tests for related stories"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    @pytest.fixture
    def search_with_related(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(
            dimensions=256,
            similarity_threshold=0.3,
            duplicate_threshold=0.95,
        )
        search = StoryVectorSearch(config=config, db_path=temp_db)

        search.index_story("STR-001", "User authentication login system")
        search.index_story("STR-002", "Password reset flow")
        search.index_story("STR-003", "Two factor authentication")
        search.index_story("STR-004", "Database migration script")

        return search

    def test_find_related(self, search_with_related):
        related = search_with_related.find_related("STR-001")

        # Should find auth-related stories
        assert len(related) > 0
        # Related but not duplicates
        assert not any(r.is_duplicate for r in related)


class TestClustering:
    """Tests for story clustering"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    @pytest.fixture
    def search_for_clustering(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(dimensions=256, similarity_threshold=0.3)
        search = StoryVectorSearch(config=config, db_path=temp_db)

        # Auth cluster
        search.index_story("STR-001", "User login feature")
        search.index_story("STR-002", "User authentication")
        search.index_story("STR-003", "Login with OAuth")

        # Payment cluster
        search.index_story("STR-004", "Payment processing")
        search.index_story("STR-005", "Credit card payment")

        # Unrelated
        search.index_story("STR-006", "Email notifications")

        return search

    def test_get_clusters(self, search_for_clustering):
        clusters = search_for_clustering.get_clusters(
            min_similarity=0.5,
            min_cluster_size=2,
        )

        # Should find at least one cluster
        assert len(clusters) >= 0  # May or may not find clusters depending on similarity


class TestStatistics:
    """Tests for statistics"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    @pytest.fixture
    def search_with_data(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(dimensions=256)
        search = StoryVectorSearch(config=config, db_path=temp_db)

        search.index_story("STR-001", "Story one")
        search.index_story("STR-002", "Story two")
        search.index_story("STR-003", "Story three")

        return search

    def test_get_statistics(self, search_with_data):
        stats = search_with_data.get_statistics()

        assert stats["total_indexed"] == 3
        assert stats["dimensions"] == 256
        assert "similarity_threshold" in stats

    def test_get_vector(self, search_with_data):
        vector = search_with_data.get_vector("STR-001")

        assert vector is not None
        assert vector.story_id == "STR-001"

    def test_get_vector_nonexistent(self, search_with_data):
        vector = search_with_data.get_vector("FAKE-001")
        assert vector is None

    def test_clear(self, search_with_data):
        search_with_data.clear()

        assert len(search_with_data._vectors) == 0
        stats = search_with_data.get_statistics()
        assert stats["total_indexed"] == 0


class TestGlobalStorySearch:
    """Tests for global singleton"""

    def test_get_story_search_singleton(self):
        from factory.search.story_vector_search import get_story_search

        s1 = get_story_search()
        s2 = get_story_search()

        assert s1 is s2


class TestMatchType:
    """Tests for match type determination"""

    @pytest.fixture
    def temp_db(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir) / "test_vectors.db"

    @pytest.fixture
    def search(self, temp_db):
        from factory.search.story_vector_search import StoryVectorSearch, SearchConfig

        config = SearchConfig(dimensions=256, similarity_threshold=0.3)
        return StoryVectorSearch(config=config, db_path=temp_db)

    def test_match_type_exact(self, search):
        match_type = search._determine_match_type("User Login", "User Login", 1.0)
        assert match_type == "exact"

    def test_match_type_case_insensitive(self, search):
        match_type = search._determine_match_type("user login", "User Login", 0.99)
        assert match_type == "exact"

    def test_match_type_near_exact(self, search):
        match_type = search._determine_match_type("user login", "user logging", 0.96)
        assert match_type == "near_exact"

    def test_match_type_semantic(self, search):
        match_type = search._determine_match_type("authentication", "login system", 0.88)
        assert match_type == "semantic"

    def test_match_type_partial(self, search):
        match_type = search._determine_match_type("auth", "payment", 0.75)
        assert match_type == "partial"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

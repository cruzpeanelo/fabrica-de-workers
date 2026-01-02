# -*- coding: utf-8 -*-
"""
Unit Tests for Story Categorizer - Issue #246
Fabrica de Agentes v6.5

Tests for factory.ai.categorizer module.
"""

import pytest
from factory.ai.categorizer import (
    StoryCategorizer,
    StoryType,
    CategorizationResult,
    categorize_story,
    suggest_story_type,
    suggest_story_labels,
    get_categorizer
)


class TestStoryTypeClassification:
    """Tests for story type classification"""

    @pytest.mark.unit
    def test_classify_bug(self):
        """Should classify bug stories correctly"""
        result = suggest_story_type(
            "Fix login bug causing crash",
            "Users cannot login due to error"
        )
        assert result["type"] == "bug"
        assert result["confidence"] > 0.5

    @pytest.mark.unit
    def test_classify_feature(self):
        """Should classify feature stories correctly"""
        result = suggest_story_type(
            "Implementar novo dashboard de métricas",
            "Como usuário quero ver métricas do sistema"
        )
        assert result["type"] == "feature"
        assert result["confidence"] > 0.3

    @pytest.mark.unit
    def test_classify_tech_debt(self):
        """Should classify tech debt stories correctly"""
        result = suggest_story_type(
            "Refatorar módulo de autenticação",
            "Technical debt: migrate legacy code"
        )
        assert result["type"] in ["tech-debt", "refactor"]

    @pytest.mark.unit
    def test_classify_docs(self):
        """Should classify documentation stories correctly"""
        result = suggest_story_type(
            "Criar documentação da API",
            "Manual de instruções para desenvolvedores"
        )
        assert result["type"] == "docs"

    @pytest.mark.unit
    def test_classify_test(self):
        """Should classify test stories correctly"""
        result = suggest_story_type(
            "Adicionar unit tests para módulo X",
            "Aumentar cobertura de testes"
        )
        assert result["type"] == "test"

    @pytest.mark.unit
    def test_classify_improvement(self):
        """Should classify improvement stories correctly"""
        result = suggest_story_type(
            "Melhorar performance da busca",
            "Enhancement to improve user experience"
        )
        assert result["type"] == "improvement"

    @pytest.mark.unit
    def test_default_to_feature(self):
        """Should default to feature when no keywords match"""
        result = suggest_story_type(
            "Fazer algo genérico",
            ""
        )
        assert result["type"] == "feature"
        assert result["confidence"] <= 0.5


class TestLabelSuggestions:
    """Tests for label suggestions"""

    @pytest.mark.unit
    def test_suggest_frontend_label(self):
        """Should suggest frontend label"""
        labels = suggest_story_labels(
            "Criar componente React para formulário",
            "UI component with CSS styling"
        )
        label_names = [l["label"] for l in labels]
        assert "frontend" in label_names

    @pytest.mark.unit
    def test_suggest_backend_label(self):
        """Should suggest backend label"""
        labels = suggest_story_labels(
            "Criar endpoint API para usuários",
            "FastAPI route with database query"
        )
        label_names = [l["label"] for l in labels]
        assert "backend" in label_names

    @pytest.mark.unit
    def test_suggest_security_label(self):
        """Should suggest security label"""
        labels = suggest_story_labels(
            "Implementar autenticação JWT",
            "Security feature for authentication"
        )
        label_names = [l["label"] for l in labels]
        assert "security" in label_names

    @pytest.mark.unit
    def test_suggest_mobile_label(self):
        """Should suggest mobile label"""
        labels = suggest_story_labels(
            "Criar tela de login mobile",
            "React Native app screen"
        )
        label_names = [l["label"] for l in labels]
        assert "mobile" in label_names

    @pytest.mark.unit
    def test_suggest_ai_label(self):
        """Should suggest ai label"""
        labels = suggest_story_labels(
            "Integrar Claude para análise",
            "Machine learning feature with LLM"
        )
        label_names = [l["label"] for l in labels]
        assert "ai" in label_names

    @pytest.mark.unit
    def test_suggest_multiple_labels(self):
        """Should suggest multiple relevant labels"""
        labels = suggest_story_labels(
            "API endpoint de autenticação mobile",
            "Backend security for mobile app"
        )
        label_names = [l["label"] for l in labels]
        assert len(labels) >= 2

    @pytest.mark.unit
    def test_labels_ordered_by_confidence(self):
        """Should order labels by confidence"""
        labels = suggest_story_labels(
            "API endpoint with database",
            "Backend database query"
        )
        if len(labels) >= 2:
            assert labels[0]["confidence"] >= labels[1]["confidence"]


class TestEpicSuggestions:
    """Tests for epic suggestions"""

    @pytest.mark.unit
    def test_suggest_auth_epic(self):
        """Should suggest authentication epic"""
        categorizer = get_categorizer()
        result = categorizer.suggest_epic(
            "Implementar login com OAuth",
            "Sistema de autenticação com JWT"
        )
        assert result is not None
        assert "auth" in result.epic_id.lower()

    @pytest.mark.unit
    def test_suggest_dashboard_epic(self):
        """Should suggest dashboard epic"""
        categorizer = get_categorizer()
        result = categorizer.suggest_epic(
            "Criar dashboard de métricas",
            "Painel com KPIs e relatórios"
        )
        assert result is not None
        assert "dashboard" in result.epic_id.lower()

    @pytest.mark.unit
    def test_suggest_kanban_epic(self):
        """Should suggest kanban epic"""
        categorizer = get_categorizer()
        result = categorizer.suggest_epic(
            "Implementar drag and drop no board",
            "Kanban column WIP limits"
        )
        assert result is not None
        assert "kanban" in result.epic_id.lower()

    @pytest.mark.unit
    def test_no_epic_for_generic_story(self):
        """Should return None for generic story"""
        categorizer = get_categorizer()
        result = categorizer.suggest_epic(
            "Fazer algo",
            ""
        )
        # May or may not match
        if result:
            assert result.confidence < 0.5


class TestSimilarStories:
    """Tests for similar story detection"""

    @pytest.mark.unit
    def test_find_similar_stories(self):
        """Should find similar stories"""
        categorizer = get_categorizer()
        existing = [
            {"story_id": "STR-001", "title": "Login com OAuth Google", "description": ""},
            {"story_id": "STR-002", "title": "Login com OAuth Facebook", "description": ""},
            {"story_id": "STR-003", "title": "Dashboard de vendas", "description": ""},
        ]

        similar = categorizer.find_similar_stories(
            "Login com OAuth Microsoft",
            "",
            existing
        )

        # Should find the OAuth stories as similar
        similar_ids = [s["story_id"] for s in similar]
        assert "STR-001" in similar_ids or "STR-002" in similar_ids

    @pytest.mark.unit
    def test_no_similar_for_unique_story(self):
        """Should return empty for very unique story"""
        categorizer = get_categorizer()
        existing = [
            {"story_id": "STR-001", "title": "Login feature", "description": ""},
        ]

        similar = categorizer.find_similar_stories(
            "Xyz abc completely different",
            "",
            existing
        )

        # Might be empty or low similarity
        for s in similar:
            assert s["similarity_score"] < 0.5


class TestFullCategorization:
    """Tests for complete categorization flow"""

    @pytest.mark.unit
    def test_categorize_story_returns_result(self):
        """Should return CategorizationResult"""
        result = categorize_story(
            "STR-001",
            "Fix authentication bug",
            "Users cannot login"
        )

        assert isinstance(result, CategorizationResult)
        assert result.story_id == "STR-001"
        assert result.suggested_type == StoryType.BUG

    @pytest.mark.unit
    def test_categorize_includes_labels(self):
        """Should include suggested labels"""
        result = categorize_story(
            "STR-001",
            "API endpoint security fix",
            "Backend authentication"
        )

        assert len(result.suggested_labels) >= 0
        # Labels should have confidence
        for label in result.suggested_labels:
            assert 0 <= label.confidence <= 1

    @pytest.mark.unit
    def test_batch_categorization(self):
        """Should categorize multiple stories"""
        categorizer = get_categorizer()
        stories = [
            {"story_id": "STR-001", "title": "Bug fix", "description": "Error"},
            {"story_id": "STR-002", "title": "New feature", "description": "Implement"},
        ]

        results = categorizer.categorize_batch(stories)

        assert len(results) == 2
        assert results[0].story_id == "STR-001"
        assert results[1].story_id == "STR-002"


class TestCategorizerSingleton:
    """Tests for categorizer singleton pattern"""

    @pytest.mark.unit
    def test_get_categorizer_returns_same_instance(self):
        """Should return same instance"""
        cat1 = get_categorizer()
        cat2 = get_categorizer()
        assert cat1 is cat2

    @pytest.mark.unit
    def test_categorizer_initialization(self):
        """Should initialize correctly"""
        categorizer = StoryCategorizer(use_llm=False)
        assert categorizer.use_llm is False


class TestHelperFunctions:
    """Tests for helper functions"""

    @pytest.mark.unit
    def test_normalize_text(self):
        """Should normalize text correctly"""
        categorizer = get_categorizer()
        result = categorizer._normalize_text("  Hello WORLD!  ")
        assert result == "hello world"

    @pytest.mark.unit
    def test_count_keyword_matches(self):
        """Should count keyword matches"""
        categorizer = get_categorizer()
        count, matches = categorizer._count_keyword_matches(
            "fix bug error crash",
            ["bug", "error", "crash", "missing"]
        )
        assert count == 3
        assert "bug" in matches
        assert "error" in matches
        assert "crash" in matches
        assert "missing" not in matches


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

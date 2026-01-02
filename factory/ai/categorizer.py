# -*- coding: utf-8 -*-
"""
Story Auto-Categorizer Module (Issue #246)
==========================================

Sistema de auto-categorização de stories usando NLP.

Funcionalidades:
- Classificação automática de tipo (feature, bug, tech-debt, docs)
- Sugestão de épico baseado em similaridade
- Aplicação automática de labels
- Agrupamento de stories similares
- Scoring de confiança para cada sugestão

Arquitetura:
- Usa keyword matching como baseline
- Integração com LLM para análise avançada (opcional)
- Cache de embeddings para performance
"""

import re
import logging
from typing import Optional, List, Dict, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime

logger = logging.getLogger(__name__)


# =============================================================================
# ENUMS E TIPOS
# =============================================================================

class StoryType(str, Enum):
    """Tipos de story suportados"""
    FEATURE = "feature"
    BUG = "bug"
    TECH_DEBT = "tech-debt"
    DOCUMENTATION = "docs"
    IMPROVEMENT = "improvement"
    REFACTOR = "refactor"
    TEST = "test"
    UNKNOWN = "unknown"


class CategorizationSource(str, Enum):
    """Fonte da categorização"""
    KEYWORD = "keyword"       # Baseado em palavras-chave
    LLM = "llm"              # Baseado em análise de LLM
    EMBEDDING = "embedding"   # Baseado em embeddings/similaridade
    MANUAL = "manual"        # Definido manualmente


@dataclass
class CategorySuggestion:
    """Sugestão de categoria para uma story"""
    category: str
    confidence: float  # 0.0 a 1.0
    source: CategorizationSource
    reason: str


@dataclass
class EpicSuggestion:
    """Sugestão de épico para uma story"""
    epic_id: str
    epic_title: str
    confidence: float
    similarity_score: float
    reason: str


@dataclass
class LabelSuggestion:
    """Sugestão de label para uma story"""
    label: str
    confidence: float
    source: CategorizationSource


@dataclass
class CategorizationResult:
    """Resultado completo da categorização"""
    story_id: str
    title: str

    # Tipo sugerido
    suggested_type: StoryType
    type_confidence: float
    type_reason: str

    # Labels sugeridas
    suggested_labels: List[LabelSuggestion] = field(default_factory=list)

    # Épico sugerido
    epic_suggestion: Optional[EpicSuggestion] = None

    # Stories similares
    similar_stories: List[Dict[str, Any]] = field(default_factory=list)

    # Metadados
    processed_at: datetime = field(default_factory=datetime.utcnow)
    source: CategorizationSource = CategorizationSource.KEYWORD


# =============================================================================
# KEYWORDS PARA CLASSIFICAÇÃO
# =============================================================================

TYPE_KEYWORDS: Dict[StoryType, List[str]] = {
    StoryType.BUG: [
        "bug", "erro", "error", "fix", "corrigir", "problema", "issue",
        "crash", "falha", "broken", "não funciona", "nao funciona",
        "defeito", "incorreto", "wrong", "failure", "exception"
    ],
    StoryType.FEATURE: [
        "feature", "funcionalidade", "implementar", "criar", "adicionar",
        "novo", "new", "add", "develop", "desenvolver", "construir",
        "build", "como usuário", "como um", "eu quero", "i want"
    ],
    StoryType.TECH_DEBT: [
        "tech debt", "dívida técnica", "technical debt", "refatorar",
        "refactor", "melhorar performance", "otimizar", "optimize",
        "cleanup", "clean up", "legacy", "deprecate", "migrar", "migrate"
    ],
    StoryType.DOCUMENTATION: [
        "doc", "documentação", "documentation", "readme", "wiki",
        "manual", "guia", "guide", "tutorial", "howto", "how to",
        "instruções", "instructions", "api doc"
    ],
    StoryType.IMPROVEMENT: [
        "melhorar", "improve", "enhancement", "melhoria", "aprimorar",
        "upgrade", "atualizar", "update", "evolução", "evolution"
    ],
    StoryType.REFACTOR: [
        "refatorar", "refactor", "reestruturar", "restructure",
        "reorganizar", "reorganize", "simplificar", "simplify"
    ],
    StoryType.TEST: [
        "teste", "test", "testing", "unit test", "integration test",
        "e2e", "coverage", "cobertura", "qa", "quality"
    ]
}

LABEL_KEYWORDS: Dict[str, List[str]] = {
    "frontend": [
        "ui", "interface", "frontend", "react", "vue", "css", "html",
        "componente", "component", "tela", "screen", "página", "page",
        "botão", "button", "modal", "form", "formulário"
    ],
    "backend": [
        "api", "endpoint", "backend", "servidor", "server", "database",
        "banco de dados", "sql", "query", "route", "rota", "fastapi"
    ],
    "security": [
        "segurança", "security", "auth", "autenticação", "authentication",
        "jwt", "token", "permissão", "permission", "rbac", "acl", "criptografia"
    ],
    "performance": [
        "performance", "desempenho", "lento", "slow", "otimização",
        "optimization", "cache", "latência", "latency", "velocidade"
    ],
    "mobile": [
        "mobile", "app", "android", "ios", "react native", "flutter",
        "responsivo", "responsive", "touch", "gesture"
    ],
    "database": [
        "database", "banco", "sql", "migration", "migração", "model",
        "schema", "tabela", "table", "index", "query", "orm"
    ],
    "integration": [
        "integração", "integration", "api", "webhook", "sync",
        "sincronização", "external", "terceiros", "third-party"
    ],
    "ai": [
        "ai", "ia", "machine learning", "ml", "nlp", "gpt", "claude",
        "llm", "inteligência artificial", "neural", "modelo"
    ],
    "urgent": [
        "urgente", "urgent", "crítico", "critical", "asap", "prioridade",
        "priority", "blocker", "blocking", "produção", "production"
    ]
}

EPIC_KEYWORDS: Dict[str, List[str]] = {
    "authentication": ["auth", "login", "logout", "sessão", "session", "jwt", "mfa", "2fa"],
    "user-management": ["usuário", "user", "perfil", "profile", "conta", "account", "registro"],
    "dashboard": ["dashboard", "painel", "métricas", "metrics", "kpi", "relatório", "report"],
    "kanban": ["kanban", "board", "coluna", "column", "card", "drag", "drop", "wip"],
    "stories": ["story", "stories", "user story", "história", "critério", "aceite"],
    "sprints": ["sprint", "velocity", "burndown", "planning", "retrospectiva", "retro"],
    "integrations": ["integração", "integration", "api", "webhook", "slack", "jira", "github"],
    "mobile": ["mobile", "app", "android", "ios", "react native", "responsive"],
    "ai-features": ["ai", "ia", "claude", "gpt", "nlp", "machine learning", "ml"]
}


# =============================================================================
# CATEGORIZER CLASS
# =============================================================================

class StoryCategorizer:
    """
    Categorizador automático de stories.

    Usa uma combinação de:
    1. Análise de palavras-chave (rápido, baseline)
    2. Integração com LLM para análise avançada (opcional)
    3. Embeddings para similaridade (futuro)
    """

    def __init__(self, use_llm: bool = False):
        """
        Inicializa o categorizador.

        Args:
            use_llm: Se True, usa LLM para análise avançada
        """
        self.use_llm = use_llm
        self._llm_client = None
        self._epics_cache: Dict[str, Dict] = {}
        self._stories_cache: Dict[str, Dict] = {}

        logger.info(f"[Categorizer] Initialized (use_llm={use_llm})")

    def _normalize_text(self, text: str) -> str:
        """Normaliza texto para comparação."""
        if not text:
            return ""
        # Lowercase e remove caracteres especiais
        text = text.lower()
        text = re.sub(r'[^\w\s]', ' ', text)
        text = re.sub(r'\s+', ' ', text).strip()
        return text

    def _count_keyword_matches(
        self,
        text: str,
        keywords: List[str]
    ) -> Tuple[int, List[str]]:
        """
        Conta quantas keywords aparecem no texto.

        Returns:
            Tuple de (contagem, lista de keywords encontradas)
        """
        normalized = self._normalize_text(text)
        matches = []

        for keyword in keywords:
            keyword_normalized = self._normalize_text(keyword)
            if keyword_normalized in normalized:
                matches.append(keyword)

        return len(matches), matches

    def classify_type(
        self,
        title: str,
        description: str = ""
    ) -> Tuple[StoryType, float, str]:
        """
        Classifica o tipo da story.

        Args:
            title: Título da story
            description: Descrição da story

        Returns:
            Tuple de (tipo, confiança, razão)
        """
        combined_text = f"{title} {description}"

        best_type = StoryType.UNKNOWN
        best_score = 0
        best_matches: List[str] = []

        for story_type, keywords in TYPE_KEYWORDS.items():
            count, matches = self._count_keyword_matches(combined_text, keywords)
            # Peso maior para matches no título
            title_count, title_matches = self._count_keyword_matches(title, keywords)
            weighted_score = count + (title_count * 2)

            if weighted_score > best_score:
                best_score = weighted_score
                best_type = story_type
                best_matches = matches

        # Calcular confiança (normalizada)
        confidence = min(best_score / 5.0, 1.0)  # 5 matches = 100% confiança

        if best_score == 0:
            return StoryType.FEATURE, 0.3, "Tipo padrão (nenhuma keyword encontrada)"

        reason = f"Keywords encontradas: {', '.join(best_matches[:5])}"
        return best_type, confidence, reason

    def suggest_labels(
        self,
        title: str,
        description: str = ""
    ) -> List[LabelSuggestion]:
        """
        Sugere labels para a story.

        Args:
            title: Título da story
            description: Descrição da story

        Returns:
            Lista de sugestões de labels ordenada por confiança
        """
        combined_text = f"{title} {description}"
        suggestions = []

        for label, keywords in LABEL_KEYWORDS.items():
            count, matches = self._count_keyword_matches(combined_text, keywords)

            if count > 0:
                confidence = min(count / 3.0, 1.0)  # 3 matches = 100%
                suggestions.append(LabelSuggestion(
                    label=label,
                    confidence=confidence,
                    source=CategorizationSource.KEYWORD
                ))

        # Ordenar por confiança
        suggestions.sort(key=lambda x: x.confidence, reverse=True)

        return suggestions[:5]  # Top 5 labels

    def suggest_epic(
        self,
        title: str,
        description: str = "",
        available_epics: Optional[List[Dict]] = None
    ) -> Optional[EpicSuggestion]:
        """
        Sugere um épico para a story.

        Args:
            title: Título da story
            description: Descrição da story
            available_epics: Lista de épicos disponíveis

        Returns:
            Sugestão de épico ou None
        """
        combined_text = f"{title} {description}"

        best_epic_key = None
        best_score = 0
        best_matches: List[str] = []

        for epic_key, keywords in EPIC_KEYWORDS.items():
            count, matches = self._count_keyword_matches(combined_text, keywords)

            if count > best_score:
                best_score = count
                best_epic_key = epic_key
                best_matches = matches

        if best_score == 0:
            return None

        confidence = min(best_score / 3.0, 1.0)

        # Se temos épicos disponíveis, tentar match
        epic_id = best_epic_key
        epic_title = best_epic_key.replace("-", " ").title()

        if available_epics:
            for epic in available_epics:
                epic_name = self._normalize_text(epic.get("title", ""))
                if best_epic_key.replace("-", " ") in epic_name:
                    epic_id = epic.get("epic_id", best_epic_key)
                    epic_title = epic.get("title", epic_title)
                    break

        return EpicSuggestion(
            epic_id=epic_id,
            epic_title=epic_title,
            confidence=confidence,
            similarity_score=best_score / len(EPIC_KEYWORDS.get(best_epic_key, [1])),
            reason=f"Keywords: {', '.join(best_matches[:3])}"
        )

    def find_similar_stories(
        self,
        title: str,
        description: str = "",
        existing_stories: Optional[List[Dict]] = None,
        limit: int = 5
    ) -> List[Dict[str, Any]]:
        """
        Encontra stories similares.

        Args:
            title: Título da story
            description: Descrição da story
            existing_stories: Lista de stories existentes
            limit: Número máximo de resultados

        Returns:
            Lista de stories similares com scores
        """
        if not existing_stories:
            return []

        target_text = self._normalize_text(f"{title} {description}")
        target_words = set(target_text.split())

        similarities = []

        for story in existing_stories:
            story_text = self._normalize_text(
                f"{story.get('title', '')} {story.get('description', '')}"
            )
            story_words = set(story_text.split())

            # Jaccard similarity
            intersection = len(target_words & story_words)
            union = len(target_words | story_words)

            if union > 0:
                similarity = intersection / union

                if similarity > 0.2:  # Threshold de 20%
                    similarities.append({
                        "story_id": story.get("story_id"),
                        "title": story.get("title"),
                        "similarity_score": round(similarity, 3),
                        "common_words": list(target_words & story_words)[:5]
                    })

        # Ordenar por similaridade
        similarities.sort(key=lambda x: x["similarity_score"], reverse=True)

        return similarities[:limit]

    def categorize(
        self,
        story_id: str,
        title: str,
        description: str = "",
        available_epics: Optional[List[Dict]] = None,
        existing_stories: Optional[List[Dict]] = None
    ) -> CategorizationResult:
        """
        Categoriza completamente uma story.

        Args:
            story_id: ID da story
            title: Título da story
            description: Descrição da story
            available_epics: Lista de épicos disponíveis
            existing_stories: Lista de stories existentes

        Returns:
            Resultado completo da categorização
        """
        logger.info(f"[Categorizer] Processing story: {story_id}")

        # Classificar tipo
        story_type, type_confidence, type_reason = self.classify_type(
            title, description
        )

        # Sugerir labels
        labels = self.suggest_labels(title, description)

        # Sugerir épico
        epic = self.suggest_epic(title, description, available_epics)

        # Encontrar similares
        similar = self.find_similar_stories(
            title, description, existing_stories
        )

        result = CategorizationResult(
            story_id=story_id,
            title=title,
            suggested_type=story_type,
            type_confidence=type_confidence,
            type_reason=type_reason,
            suggested_labels=labels,
            epic_suggestion=epic,
            similar_stories=similar,
            source=CategorizationSource.KEYWORD
        )

        logger.info(
            f"[Categorizer] Result: type={story_type.value}, "
            f"confidence={type_confidence:.2f}, labels={len(labels)}"
        )

        return result

    def categorize_batch(
        self,
        stories: List[Dict],
        available_epics: Optional[List[Dict]] = None
    ) -> List[CategorizationResult]:
        """
        Categoriza múltiplas stories em lote.

        Args:
            stories: Lista de stories para categorizar
            available_epics: Lista de épicos disponíveis

        Returns:
            Lista de resultados de categorização
        """
        results = []

        for story in stories:
            result = self.categorize(
                story_id=story.get("story_id", ""),
                title=story.get("title", ""),
                description=story.get("description", ""),
                available_epics=available_epics,
                existing_stories=stories  # Usar outras stories para similaridade
            )
            results.append(result)

        return results


# =============================================================================
# FUNÇÕES DE CONVENIÊNCIA
# =============================================================================

_categorizer: Optional[StoryCategorizer] = None


def get_categorizer(use_llm: bool = False) -> StoryCategorizer:
    """Retorna instância singleton do categorizador."""
    global _categorizer
    if _categorizer is None:
        _categorizer = StoryCategorizer(use_llm=use_llm)
    return _categorizer


def categorize_story(
    story_id: str,
    title: str,
    description: str = "",
    available_epics: Optional[List[Dict]] = None,
    existing_stories: Optional[List[Dict]] = None
) -> CategorizationResult:
    """
    Função de conveniência para categorizar uma story.

    Example:
        result = categorize_story(
            "STR-001",
            "Implementar login com OAuth",
            "Como usuário quero fazer login com Google"
        )
        print(result.suggested_type)  # StoryType.FEATURE
        print(result.suggested_labels)  # [security, frontend]
    """
    categorizer = get_categorizer()
    return categorizer.categorize(
        story_id=story_id,
        title=title,
        description=description,
        available_epics=available_epics,
        existing_stories=existing_stories
    )


def suggest_story_type(title: str, description: str = "") -> Dict[str, Any]:
    """
    Sugere tipo para uma story.

    Returns:
        Dict com type, confidence e reason
    """
    categorizer = get_categorizer()
    story_type, confidence, reason = categorizer.classify_type(title, description)

    return {
        "type": story_type.value,
        "confidence": confidence,
        "reason": reason
    }


def suggest_story_labels(title: str, description: str = "") -> List[Dict[str, Any]]:
    """
    Sugere labels para uma story.

    Returns:
        Lista de dicts com label e confidence
    """
    categorizer = get_categorizer()
    suggestions = categorizer.suggest_labels(title, description)

    return [
        {"label": s.label, "confidence": s.confidence}
        for s in suggestions
    ]

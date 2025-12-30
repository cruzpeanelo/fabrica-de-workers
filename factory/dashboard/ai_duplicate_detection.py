# -*- coding: utf-8 -*-
"""
AI Duplicate Detection Module (Issue #247)
==========================================
Sistema de deteccao de stories duplicadas ou muito similares.

Funcionalidades:
- Deteccao em tempo real ao criar story
- Calculo de similaridade por texto
- Sugestao de stories relacionadas
- Relatorio de duplicatas potenciais
"""

from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict, Tuple
from datetime import datetime
import re
import math
from collections import Counter
from factory.database.connection import SessionLocal
from factory.database.models import Story

router = APIRouter(prefix="/api/ai/duplicates", tags=["AI Duplicate Detection"])


# ==================== MODELS ====================

class StoryCheckInput(BaseModel):
    title: str
    description: Optional[str] = None
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    project_id: Optional[str] = None


class SimilarStory(BaseModel):
    story_id: str
    title: str
    similarity_score: float
    similarity_type: str  # exact, high, medium, low
    matching_keywords: List[str]
    status: str


class DuplicateCheckResult(BaseModel):
    has_potential_duplicates: bool
    similar_stories: List[SimilarStory]
    recommendation: str
    threshold_used: float


class DuplicateReport(BaseModel):
    total_stories: int
    potential_duplicates: int
    duplicate_groups: List[Dict]
    suggestions: List[str]


# ==================== SIMILARITY ENGINE ====================

class TextSimilarityEngine:
    """Motor de similaridade de texto."""

    STOP_WORDS = {
        'o', 'a', 'os', 'as', 'um', 'uma', 'de', 'da', 'do', 'em', 'na', 'no',
        'para', 'por', 'com', 'como', 'que', 'e', 'ou', 'se', 'ao', 'aos',
        'the', 'a', 'an', 'of', 'to', 'in', 'for', 'with', 'as', 'and', 'or',
        'is', 'are', 'be', 'been', 'being', 'have', 'has', 'had', 'do', 'does',
        'did', 'will', 'would', 'could', 'should', 'may', 'might', 'must',
        'shall', 'can', 'need', 'want', 'quero', 'preciso', 'devo', 'posso',
        'usuario', 'user', 'sistema', 'system', 'deve', 'should', 'must'
    }

    @staticmethod
    def tokenize(text: str) -> List[str]:
        """Tokeniza e normaliza texto."""
        if not text:
            return []

        # Normaliza
        text = text.lower()
        text = re.sub(r'[^\w\s]', ' ', text)
        text = re.sub(r'\d+', '', text)

        # Tokeniza
        tokens = text.split()

        # Remove stop words e tokens curtos
        tokens = [t for t in tokens if t not in TextSimilarityEngine.STOP_WORDS and len(t) > 2]

        return tokens

    @staticmethod
    def jaccard_similarity(tokens1: List[str], tokens2: List[str]) -> float:
        """Calcula similaridade de Jaccard entre dois conjuntos de tokens."""
        if not tokens1 or not tokens2:
            return 0.0

        set1 = set(tokens1)
        set2 = set(tokens2)

        intersection = len(set1 & set2)
        union = len(set1 | set2)

        return intersection / union if union > 0 else 0.0

    @staticmethod
    def cosine_similarity(tokens1: List[str], tokens2: List[str]) -> float:
        """Calcula similaridade de cosseno usando TF."""
        if not tokens1 or not tokens2:
            return 0.0

        counter1 = Counter(tokens1)
        counter2 = Counter(tokens2)

        # Todos os termos
        all_terms = set(counter1.keys()) | set(counter2.keys())

        # Vetores
        vec1 = [counter1.get(term, 0) for term in all_terms]
        vec2 = [counter2.get(term, 0) for term in all_terms]

        # Produto escalar
        dot_product = sum(v1 * v2 for v1, v2 in zip(vec1, vec2))

        # Magnitudes
        mag1 = math.sqrt(sum(v ** 2 for v in vec1))
        mag2 = math.sqrt(sum(v ** 2 for v in vec2))

        if mag1 == 0 or mag2 == 0:
            return 0.0

        return dot_product / (mag1 * mag2)

    @staticmethod
    def levenshtein_ratio(s1: str, s2: str) -> float:
        """Calcula ratio de similaridade baseado em distancia de Levenshtein."""
        if not s1 or not s2:
            return 0.0

        s1, s2 = s1.lower(), s2.lower()

        if s1 == s2:
            return 1.0

        len1, len2 = len(s1), len(s2)

        # Matriz de distancias
        matrix = [[0] * (len2 + 1) for _ in range(len1 + 1)]

        for i in range(len1 + 1):
            matrix[i][0] = i
        for j in range(len2 + 1):
            matrix[0][j] = j

        for i in range(1, len1 + 1):
            for j in range(1, len2 + 1):
                cost = 0 if s1[i - 1] == s2[j - 1] else 1
                matrix[i][j] = min(
                    matrix[i - 1][j] + 1,
                    matrix[i][j - 1] + 1,
                    matrix[i - 1][j - 1] + cost
                )

        distance = matrix[len1][len2]
        max_len = max(len1, len2)

        return 1 - (distance / max_len) if max_len > 0 else 0.0

    @classmethod
    def combined_similarity(cls, text1: str, text2: str) -> Tuple[float, List[str]]:
        """Calcula similaridade combinada e retorna keywords em comum."""
        tokens1 = cls.tokenize(text1)
        tokens2 = cls.tokenize(text2)

        # Pesos para cada metrica
        jaccard = cls.jaccard_similarity(tokens1, tokens2) * 0.3
        cosine = cls.cosine_similarity(tokens1, tokens2) * 0.4
        levenshtein = cls.levenshtein_ratio(text1, text2) * 0.3

        combined = jaccard + cosine + levenshtein

        # Keywords em comum
        common_keywords = list(set(tokens1) & set(tokens2))

        return combined, common_keywords

    @staticmethod
    def classify_similarity(score: float) -> str:
        """Classifica o nivel de similaridade."""
        if score >= 0.9:
            return "exact"
        elif score >= 0.7:
            return "high"
        elif score >= 0.5:
            return "medium"
        else:
            return "low"


# ==================== DUPLICATE DETECTOR ====================

class DuplicateDetector:
    """Detector de duplicatas de stories."""

    def __init__(self, threshold: float = 0.6):
        self.threshold = threshold
        self.engine = TextSimilarityEngine()

    def build_story_text(self, story) -> str:
        """Constroi texto completo da story para comparacao."""
        parts = []

        if hasattr(story, 'title') and story.title:
            parts.append(story.title)
        if hasattr(story, 'description') and story.description:
            parts.append(story.description)
        if hasattr(story, 'persona') and story.persona:
            parts.append(story.persona)
        if hasattr(story, 'action') and story.action:
            parts.append(story.action)
        if hasattr(story, 'benefit') and story.benefit:
            parts.append(story.benefit)

        return ' '.join(parts)

    def check_duplicates(self, new_story: StoryCheckInput, existing_stories: List) -> DuplicateCheckResult:
        """Verifica duplicatas para uma nova story."""
        new_text = f"{new_story.title} {new_story.description or ''} {new_story.persona or ''} {new_story.action or ''} {new_story.benefit or ''}"

        similar_stories = []

        for story in existing_stories:
            existing_text = self.build_story_text(story)

            score, keywords = self.engine.combined_similarity(new_text, existing_text)

            if score >= self.threshold:
                status = story.status.value if hasattr(story.status, 'value') else str(story.status)

                similar_stories.append(SimilarStory(
                    story_id=story.story_id,
                    title=story.title,
                    similarity_score=round(score * 100, 1),
                    similarity_type=self.engine.classify_similarity(score),
                    matching_keywords=keywords[:5],
                    status=status
                ))

        # Ordena por score
        similar_stories.sort(key=lambda x: x.similarity_score, reverse=True)

        # Recomendacao
        if not similar_stories:
            recommendation = "Nenhuma duplicata detectada. Story pode ser criada."
        elif similar_stories[0].similarity_type == "exact":
            recommendation = f"ATENCAO: Story muito similar a {similar_stories[0].story_id}. Considere vincular ou mesclar."
        elif similar_stories[0].similarity_type == "high":
            recommendation = f"Story similar a {similar_stories[0].story_id} encontrada. Verifique se sao relacionadas."
        else:
            recommendation = "Stories relacionadas encontradas. Considere adicionar links."

        return DuplicateCheckResult(
            has_potential_duplicates=len(similar_stories) > 0,
            similar_stories=similar_stories[:10],  # Top 10
            recommendation=recommendation,
            threshold_used=self.threshold
        )

    def generate_report(self, stories: List) -> DuplicateReport:
        """Gera relatorio de duplicatas potenciais."""
        duplicate_groups = []
        processed = set()

        for i, story1 in enumerate(stories):
            if story1.story_id in processed:
                continue

            group = {
                "anchor": story1.story_id,
                "anchor_title": story1.title,
                "duplicates": []
            }

            text1 = self.build_story_text(story1)

            for story2 in stories[i + 1:]:
                if story2.story_id in processed:
                    continue

                text2 = self.build_story_text(story2)
                score, keywords = self.engine.combined_similarity(text1, text2)

                if score >= self.threshold:
                    group["duplicates"].append({
                        "story_id": story2.story_id,
                        "title": story2.title,
                        "similarity": round(score * 100, 1),
                        "keywords": keywords[:3]
                    })
                    processed.add(story2.story_id)

            if group["duplicates"]:
                processed.add(story1.story_id)
                duplicate_groups.append(group)

        # Sugestoes
        suggestions = []
        if duplicate_groups:
            suggestions.append(f"Encontrados {len(duplicate_groups)} grupos de stories potencialmente duplicadas.")
            suggestions.append("Revise cada grupo e considere mesclar ou vincular stories relacionadas.")
        else:
            suggestions.append("Nenhuma duplicata significativa encontrada.")

        return DuplicateReport(
            total_stories=len(stories),
            potential_duplicates=sum(len(g["duplicates"]) + 1 for g in duplicate_groups),
            duplicate_groups=duplicate_groups,
            suggestions=suggestions
        )


# ==================== API ENDPOINTS ====================

@router.post("/check", response_model=DuplicateCheckResult)
async def check_for_duplicates(
    story: StoryCheckInput,
    threshold: float = Query(0.6, ge=0.1, le=1.0, description="Limiar de similaridade (0.1-1.0)")
):
    """
    Verifica se uma nova story tem duplicatas ou similares.
    """
    db = SessionLocal()
    try:
        query = db.query(Story)

        if story.project_id:
            query = query.filter(Story.project_id == story.project_id)

        existing_stories = query.limit(500).all()

        detector = DuplicateDetector(threshold=threshold)
        result = detector.check_duplicates(story, existing_stories)

        return result

    finally:
        db.close()


@router.get("/report")
async def generate_duplicate_report(
    project_id: Optional[str] = Query(None),
    threshold: float = Query(0.6, ge=0.1, le=1.0)
):
    """
    Gera relatorio de duplicatas para um projeto ou todos.
    """
    db = SessionLocal()
    try:
        query = db.query(Story)

        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.limit(500).all()

        detector = DuplicateDetector(threshold=threshold)
        report = detector.generate_report(stories)

        return report

    finally:
        db.close()


@router.get("/related/{story_id}")
async def find_related_stories(
    story_id: str,
    threshold: float = Query(0.4, ge=0.1, le=1.0),
    limit: int = Query(10, ge=1, le=50)
):
    """
    Encontra stories relacionadas a uma story especifica.
    """
    db = SessionLocal()
    try:
        # Busca story alvo
        target_story = db.query(Story).filter(Story.story_id == story_id).first()

        if not target_story:
            raise HTTPException(status_code=404, detail="Story nao encontrada")

        # Busca outras stories do mesmo projeto
        query = db.query(Story).filter(Story.story_id != story_id)

        if target_story.project_id:
            query = query.filter(Story.project_id == target_story.project_id)

        other_stories = query.limit(500).all()

        # Calcula similaridade
        engine = TextSimilarityEngine()
        target_text = f"{target_story.title} {target_story.description or ''}"

        related = []
        for story in other_stories:
            story_text = f"{story.title} {story.description or ''}"
            score, keywords = engine.combined_similarity(target_text, story_text)

            if score >= threshold:
                status = story.status.value if hasattr(story.status, 'value') else str(story.status)
                related.append({
                    "story_id": story.story_id,
                    "title": story.title,
                    "similarity": round(score * 100, 1),
                    "relation_type": engine.classify_similarity(score),
                    "common_keywords": keywords[:5],
                    "status": status
                })

        # Ordena e limita
        related.sort(key=lambda x: x["similarity"], reverse=True)

        return {
            "story_id": story_id,
            "title": target_story.title,
            "related_stories": related[:limit],
            "total_found": len(related)
        }

    finally:
        db.close()


@router.post("/compare")
async def compare_two_stories(story1_id: str, story2_id: str):
    """
    Compara duas stories e retorna analise detalhada.
    """
    db = SessionLocal()
    try:
        s1 = db.query(Story).filter(Story.story_id == story1_id).first()
        s2 = db.query(Story).filter(Story.story_id == story2_id).first()

        if not s1 or not s2:
            raise HTTPException(status_code=404, detail="Uma ou ambas stories nao encontradas")

        engine = TextSimilarityEngine()

        text1 = f"{s1.title} {s1.description or ''}"
        text2 = f"{s2.title} {s2.description or ''}"

        # Similaridades individuais
        tokens1 = engine.tokenize(text1)
        tokens2 = engine.tokenize(text2)

        jaccard = engine.jaccard_similarity(tokens1, tokens2)
        cosine = engine.cosine_similarity(tokens1, tokens2)
        levenshtein = engine.levenshtein_ratio(text1, text2)
        combined, keywords = engine.combined_similarity(text1, text2)

        return {
            "story1": {"id": s1.story_id, "title": s1.title},
            "story2": {"id": s2.story_id, "title": s2.title},
            "similarity": {
                "combined": round(combined * 100, 1),
                "jaccard": round(jaccard * 100, 1),
                "cosine": round(cosine * 100, 1),
                "levenshtein": round(levenshtein * 100, 1)
            },
            "classification": engine.classify_similarity(combined),
            "common_keywords": keywords,
            "recommendation": "Considere mesclar" if combined >= 0.7 else "Considere vincular" if combined >= 0.5 else "Stories distintas"
        }

    finally:
        db.close()


@router.get("/settings")
async def get_detection_settings():
    """
    Retorna configuracoes do detector de duplicatas.
    """
    return {
        "default_threshold": 0.6,
        "thresholds": {
            "exact": 0.9,
            "high": 0.7,
            "medium": 0.5,
            "low": 0.3
        },
        "algorithms": ["jaccard", "cosine", "levenshtein"],
        "weights": {
            "jaccard": 0.3,
            "cosine": 0.4,
            "levenshtein": 0.3
        },
        "max_stories_per_check": 500
    }


# ==================== HTML COMPONENT ====================

def get_duplicate_detection_component() -> str:
    """Retorna componente Vue.js para deteccao de duplicatas."""
    return """
    <!-- AI Duplicate Detection Component -->
    <div id="duplicate-detection">
        <!-- Inline Check (ao criar story) -->
        <div v-if="duplicateCheckResult && duplicateCheckResult.has_potential_duplicates"
             class="mt-3 p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
            <div class="flex items-start gap-3">
                <svg class="w-6 h-6 text-yellow-500 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                          d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                </svg>
                <div class="flex-1">
                    <h4 class="font-medium text-yellow-800">Duplicatas Potenciais Detectadas</h4>
                    <p class="text-sm text-yellow-700 mt-1">{{ duplicateCheckResult.recommendation }}</p>

                    <div class="mt-3 space-y-2">
                        <div v-for="similar in duplicateCheckResult.similar_stories.slice(0, 3)"
                             :key="similar.story_id"
                             class="flex items-center justify-between p-2 bg-white rounded border">
                            <div>
                                <span class="font-mono text-xs text-gray-500">{{ similar.story_id }}</span>
                                <span class="ml-2 text-sm">{{ similar.title }}</span>
                            </div>
                            <div class="flex items-center gap-2">
                                <span :class="{
                                    'bg-red-100 text-red-700': similar.similarity_type === 'exact',
                                    'bg-orange-100 text-orange-700': similar.similarity_type === 'high',
                                    'bg-yellow-100 text-yellow-700': similar.similarity_type === 'medium'
                                }" class="px-2 py-0.5 rounded text-xs font-medium">
                                    {{ similar.similarity_score }}%
                                </span>
                                <button @click="viewStory(similar.story_id)"
                                        class="text-blue-600 hover:text-blue-800 text-xs">
                                    Ver
                                </button>
                            </div>
                        </div>
                    </div>

                    <div class="mt-3 flex gap-2">
                        <button @click="ignoreDuplicateWarning" class="text-sm text-gray-600 hover:text-gray-800">
                            Ignorar e Criar Mesmo Assim
                        </button>
                        <button @click="showDuplicateDetails = true" class="text-sm text-blue-600 hover:text-blue-800">
                            Ver Detalhes
                        </button>
                    </div>
                </div>
            </div>
        </div>

        <!-- Duplicate Report Modal -->
        <div v-if="showDuplicateReport" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center p-4">
            <div class="bg-white rounded-xl w-full max-w-4xl max-h-[90vh] overflow-hidden shadow-2xl">
                <div class="bg-gradient-to-r from-amber-500 to-orange-500 text-white p-4">
                    <div class="flex items-center justify-between">
                        <div class="flex items-center gap-3">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                            </svg>
                            <div>
                                <h2 class="text-lg font-semibold">Relatorio de Duplicatas</h2>
                                <p class="text-amber-100 text-sm">Analise de stories potencialmente duplicadas</p>
                            </div>
                        </div>
                        <button @click="showDuplicateReport = false" class="p-1 hover:bg-white/20 rounded">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 140px);">
                    <!-- Stats -->
                    <div class="grid grid-cols-3 gap-4 mb-6">
                        <div class="bg-gray-50 p-4 rounded-lg text-center">
                            <div class="text-3xl font-bold text-gray-800">{{ duplicateReport?.total_stories || 0 }}</div>
                            <div class="text-sm text-gray-600">Total de Stories</div>
                        </div>
                        <div class="bg-amber-50 p-4 rounded-lg text-center">
                            <div class="text-3xl font-bold text-amber-600">{{ duplicateReport?.potential_duplicates || 0 }}</div>
                            <div class="text-sm text-gray-600">Potenciais Duplicatas</div>
                        </div>
                        <div class="bg-orange-50 p-4 rounded-lg text-center">
                            <div class="text-3xl font-bold text-orange-600">{{ duplicateReport?.duplicate_groups?.length || 0 }}</div>
                            <div class="text-sm text-gray-600">Grupos</div>
                        </div>
                    </div>

                    <!-- Duplicate Groups -->
                    <div v-if="duplicateReport?.duplicate_groups?.length > 0" class="space-y-4">
                        <div v-for="(group, index) in duplicateReport.duplicate_groups" :key="index"
                             class="border rounded-lg overflow-hidden">
                            <div class="bg-gray-100 px-4 py-2 font-medium flex items-center justify-between">
                                <span>Grupo {{ index + 1 }}</span>
                                <span class="text-sm text-gray-500">{{ group.duplicates.length + 1 }} stories</span>
                            </div>
                            <div class="p-4 space-y-2">
                                <!-- Anchor story -->
                                <div class="flex items-center gap-3 p-2 bg-blue-50 rounded">
                                    <span class="font-mono text-xs text-blue-600">{{ group.anchor }}</span>
                                    <span class="text-sm flex-1">{{ group.anchor_title }}</span>
                                    <span class="text-xs text-blue-600">Principal</span>
                                </div>
                                <!-- Duplicates -->
                                <div v-for="dup in group.duplicates" :key="dup.story_id"
                                     class="flex items-center gap-3 p-2 bg-gray-50 rounded">
                                    <span class="font-mono text-xs text-gray-500">{{ dup.story_id }}</span>
                                    <span class="text-sm flex-1">{{ dup.title }}</span>
                                    <span class="text-xs px-2 py-0.5 bg-amber-100 text-amber-700 rounded">
                                        {{ dup.similarity }}% similar
                                    </span>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div v-else class="text-center py-8 text-gray-500">
                        <svg class="w-12 h-12 mx-auto mb-3 text-green-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                        </svg>
                        <p>Nenhuma duplicata encontrada!</p>
                    </div>
                </div>

                <div class="border-t p-4 flex justify-end gap-2 bg-gray-50">
                    <button @click="refreshDuplicateReport" class="px-4 py-2 border rounded-lg hover:bg-gray-100">
                        Atualizar
                    </button>
                    <button @click="showDuplicateReport = false" class="px-4 py-2 bg-gray-800 text-white rounded-lg hover:bg-gray-700">
                        Fechar
                    </button>
                </div>
            </div>
        </div>
    </div>

    <script>
    // Vue.js integration for Duplicate Detection
    const duplicateDetection = {
        data() {
            return {
                showDuplicateReport: false,
                showDuplicateDetails: false,
                duplicateCheckResult: null,
                duplicateReport: null,
                checkingDuplicates: false
            };
        },
        methods: {
            async checkForDuplicates(storyData) {
                this.checkingDuplicates = true;
                try {
                    const response = await fetch('/api/ai/duplicates/check', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(storyData)
                    });
                    this.duplicateCheckResult = await response.json();
                } catch (error) {
                    console.error('Error checking duplicates:', error);
                } finally {
                    this.checkingDuplicates = false;
                }
            },
            async loadDuplicateReport(projectId = null) {
                try {
                    const url = projectId
                        ? `/api/ai/duplicates/report?project_id=${projectId}`
                        : '/api/ai/duplicates/report';
                    const response = await fetch(url);
                    this.duplicateReport = await response.json();
                    this.showDuplicateReport = true;
                } catch (error) {
                    console.error('Error loading report:', error);
                }
            },
            refreshDuplicateReport() {
                this.loadDuplicateReport();
            },
            ignoreDuplicateWarning() {
                this.duplicateCheckResult = null;
            },
            viewStory(storyId) {
                // Navigate to story or open modal
                this.$emit('view-story', storyId);
            }
        }
    };
    </script>
    """


def register_duplicate_detection_routes(app):
    """Registra as rotas de deteccao de duplicatas."""
    app.include_router(router)
    print("[AI] Duplicate Detection registrado: /api/ai/duplicates/*")

# -*- coding: utf-8 -*-
"""
Effort Estimator - Estimativa Inteligente de Esforco
=====================================================

Usa IA (Claude) para estimar story points e esforco com base em:
- Analise de requisitos (NLP)
- Numero de criterios de aceite
- Complexidade tecnica
- Similaridade com stories anteriores
- Historico do projeto

Ref: GitHub Issue #63
"""

import json
import os
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field, asdict
from enum import Enum

# Fibonacci sequence for story points
FIBONACCI_POINTS = [1, 2, 3, 5, 8, 13, 21]

# Complexity mapping
COMPLEXITY_MAPPING = {
    "low": {"min_points": 1, "max_points": 3, "hours_multiplier": 2},
    "medium": {"min_points": 3, "max_points": 8, "hours_multiplier": 4},
    "high": {"min_points": 5, "max_points": 13, "hours_multiplier": 8},
    "very_high": {"min_points": 8, "max_points": 21, "hours_multiplier": 16}
}

# Risk factors
RISK_FACTORS = {
    "new_technology": 1.3,
    "external_dependency": 1.2,
    "unclear_requirements": 1.4,
    "database_changes": 1.2,
    "security_sensitive": 1.3,
    "performance_critical": 1.2,
    "integration_required": 1.25,
    "ui_heavy": 1.15
}


class ConfidenceLevel(str, Enum):
    """Nivel de confianca da estimativa"""
    HIGH = "high"       # 90%+ - Muitos dados similares
    MEDIUM = "medium"   # 70-90% - Alguns dados
    LOW = "low"         # <70% - Pouco historico


@dataclass
class RiskFactor:
    """Fator de risco identificado"""
    name: str
    description: str
    impact: float  # Multiplicador de impacto (1.0 = neutro)
    severity: str  # low, medium, high


@dataclass
class SimilarStory:
    """Story similar encontrada no historico"""
    story_id: str
    title: str
    story_points: int
    actual_hours: float
    similarity_score: float  # 0-100
    completed_at: Optional[str] = None


@dataclass
class ComplexityBreakdown:
    """Decomposicao da complexidade"""
    requirements_complexity: str  # low, medium, high
    technical_complexity: str
    integration_complexity: str
    testing_complexity: str
    overall: str


@dataclass
class EffortEstimate:
    """Resultado da estimativa de esforco"""
    story_id: str

    # Estimativas principais
    suggested_points: int
    suggested_hours: float
    complexity: str

    # Confianca
    confidence: str  # high, medium, low
    confidence_score: int  # 0-100

    # Breakdown
    complexity_breakdown: ComplexityBreakdown
    risk_factors: List[RiskFactor]

    # Comparacao
    similar_stories: List[SimilarStory]

    # Justificativa
    reasoning: str
    suggestions: List[str]

    # Metadados
    estimated_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())
    estimator_version: str = "1.0.0"

    def to_dict(self) -> Dict[str, Any]:
        return {
            "story_id": self.story_id,
            "suggested_points": self.suggested_points,
            "suggested_hours": self.suggested_hours,
            "complexity": self.complexity,
            "confidence": self.confidence,
            "confidence_score": self.confidence_score,
            "complexity_breakdown": asdict(self.complexity_breakdown),
            "risk_factors": [asdict(r) for r in self.risk_factors],
            "similar_stories": [asdict(s) for s in self.similar_stories],
            "reasoning": self.reasoning,
            "suggestions": self.suggestions,
            "estimated_at": self.estimated_at,
            "estimator_version": self.estimator_version
        }


class EffortEstimator:
    """
    Estimador inteligente de esforco usando IA

    Analisa stories e fornece estimativas de:
    - Story points (Fibonacci: 1,2,3,5,8,13,21)
    - Horas estimadas
    - Complexidade
    - Fatores de risco
    - Comparacao com stories similares
    """

    def __init__(self, db_session=None):
        """
        Inicializa o estimador

        Args:
            db_session: Sessao do banco de dados para buscar historico
        """
        self.db = db_session
        self.claude_client = None
        self._init_claude()

    def _init_claude(self):
        """Inicializa cliente Claude"""
        try:
            from factory.ai.claude_integration import get_claude_client
            self.claude_client = get_claude_client()
        except ImportError:
            print("[EffortEstimator] Claude integration not available")
            self.claude_client = None

    def estimate(self, story: Dict[str, Any], project_context: Dict[str, Any] = None) -> EffortEstimate:
        """
        Estima esforco para uma story

        Args:
            story: Dicionario com dados da story
            project_context: Contexto do projeto (historico, metricas, etc)

        Returns:
            EffortEstimate com estimativas detalhadas
        """
        story_id = story.get("story_id", "UNKNOWN")

        # Coleta dados para analise
        title = story.get("title", "")
        description = story.get("description", "")
        persona = story.get("persona", "")
        action = story.get("action", "")
        benefit = story.get("benefit", "")
        acceptance_criteria = story.get("acceptance_criteria", [])
        definition_of_done = story.get("definition_of_done", [])
        technical_notes = story.get("technical_notes", "")
        tags = story.get("tags", [])
        category = story.get("category", "feature")

        # Busca stories similares
        similar_stories = self._find_similar_stories(story, project_context)

        # Analise com IA se disponivel
        if self.claude_client and self.claude_client.is_available():
            return self._estimate_with_ai(
                story_id=story_id,
                title=title,
                description=description,
                narrative=f"Como {persona}, eu quero {action}, para que {benefit}",
                acceptance_criteria=acceptance_criteria,
                definition_of_done=definition_of_done,
                technical_notes=technical_notes,
                tags=tags,
                category=category,
                similar_stories=similar_stories,
                project_context=project_context
            )
        else:
            # Fallback: estimativa heuristica
            return self._estimate_heuristic(
                story_id=story_id,
                title=title,
                description=description,
                acceptance_criteria=acceptance_criteria,
                definition_of_done=definition_of_done,
                technical_notes=technical_notes,
                tags=tags,
                category=category,
                similar_stories=similar_stories
            )

    def _estimate_with_ai(
        self,
        story_id: str,
        title: str,
        description: str,
        narrative: str,
        acceptance_criteria: List[str],
        definition_of_done: List[str],
        technical_notes: str,
        tags: List[str],
        category: str,
        similar_stories: List[SimilarStory],
        project_context: Dict[str, Any] = None
    ) -> EffortEstimate:
        """Estimativa usando Claude AI"""

        system_prompt = """Voce e um Scrum Master e Tech Lead experiente especializado em estimativas ageis.
Analise a User Story fornecida e forneca uma estimativa detalhada.

Responda SEMPRE em JSON valido com a seguinte estrutura:
{
    "suggested_points": <int 1-21 Fibonacci>,
    "suggested_hours": <float>,
    "complexity": "<low|medium|high|very_high>",
    "confidence_score": <int 0-100>,
    "complexity_breakdown": {
        "requirements_complexity": "<low|medium|high>",
        "technical_complexity": "<low|medium|high>",
        "integration_complexity": "<low|medium|high>",
        "testing_complexity": "<low|medium|high>",
        "overall": "<low|medium|high|very_high>"
    },
    "risk_factors": [
        {
            "name": "<nome do risco>",
            "description": "<descricao>",
            "impact": <float 1.0-2.0>,
            "severity": "<low|medium|high>"
        }
    ],
    "reasoning": "<justificativa detalhada da estimativa>",
    "suggestions": ["<sugestao 1>", "<sugestao 2>"]
}

Considere:
- Story Points seguem Fibonacci: 1, 2, 3, 5, 8, 13, 21
- 1 ponto ~= 2-4 horas de trabalho
- Mais criterios de aceite = mais complexidade
- Integracao externa aumenta risco
- Novas tecnologias aumentam incerteza"""

        # Formata stories similares para contexto
        similar_context = ""
        if similar_stories:
            similar_context = "\n\nStories Similares Completadas:\n"
            for s in similar_stories[:3]:
                similar_context += f"- {s.story_id}: {s.title} ({s.story_points} pts, {s.actual_hours}h real)\n"

        message = f"""Analise esta User Story e forneca estimativa de esforco:

**Titulo:** {title}

**Narrativa:** {narrative}

**Descricao:** {description or 'Nao fornecida'}

**Criterios de Aceite ({len(acceptance_criteria)}):**
{chr(10).join(f"- {c}" for c in acceptance_criteria) if acceptance_criteria else "Nenhum definido"}

**Definition of Done ({len(definition_of_done)}):**
{chr(10).join(f"- {d}" for d in definition_of_done) if definition_of_done else "Padrao do projeto"}

**Notas Tecnicas:** {technical_notes or 'Nenhuma'}

**Categoria:** {category}
**Tags:** {', '.join(tags) if tags else 'Nenhuma'}
{similar_context}

Forneca a estimativa em JSON."""

        try:
            response = self.claude_client.chat(message, system_prompt)

            if response.success:
                # Parse JSON da resposta
                json_str = response.content
                # Limpa markdown se presente
                if "```json" in json_str:
                    json_str = json_str.split("```json")[1].split("```")[0]
                elif "```" in json_str:
                    json_str = json_str.split("```")[1].split("```")[0]

                data = json.loads(json_str.strip())

                # Constroi breakdown
                breakdown_data = data.get("complexity_breakdown", {})
                complexity_breakdown = ComplexityBreakdown(
                    requirements_complexity=breakdown_data.get("requirements_complexity", "medium"),
                    technical_complexity=breakdown_data.get("technical_complexity", "medium"),
                    integration_complexity=breakdown_data.get("integration_complexity", "low"),
                    testing_complexity=breakdown_data.get("testing_complexity", "medium"),
                    overall=breakdown_data.get("overall", data.get("complexity", "medium"))
                )

                # Constroi fatores de risco
                risk_factors = []
                for rf in data.get("risk_factors", []):
                    risk_factors.append(RiskFactor(
                        name=rf.get("name", "Unknown"),
                        description=rf.get("description", ""),
                        impact=rf.get("impact", 1.0),
                        severity=rf.get("severity", "low")
                    ))

                # Determina nivel de confianca
                confidence_score = data.get("confidence_score", 70)
                if confidence_score >= 90:
                    confidence = ConfidenceLevel.HIGH.value
                elif confidence_score >= 70:
                    confidence = ConfidenceLevel.MEDIUM.value
                else:
                    confidence = ConfidenceLevel.LOW.value

                # Valida story points (deve ser Fibonacci)
                suggested_points = data.get("suggested_points", 5)
                if suggested_points not in FIBONACCI_POINTS:
                    # Encontra Fibonacci mais proximo
                    suggested_points = min(FIBONACCI_POINTS, key=lambda x: abs(x - suggested_points))

                return EffortEstimate(
                    story_id=story_id,
                    suggested_points=suggested_points,
                    suggested_hours=data.get("suggested_hours", suggested_points * 3),
                    complexity=data.get("complexity", "medium"),
                    confidence=confidence,
                    confidence_score=confidence_score,
                    complexity_breakdown=complexity_breakdown,
                    risk_factors=risk_factors,
                    similar_stories=similar_stories,
                    reasoning=data.get("reasoning", "Estimativa baseada em analise de IA"),
                    suggestions=data.get("suggestions", [])
                )
            else:
                # Fallback se AI falhar
                return self._estimate_heuristic(
                    story_id, title, description, acceptance_criteria,
                    definition_of_done, technical_notes, tags, category, similar_stories
                )

        except (json.JSONDecodeError, KeyError) as e:
            print(f"[EffortEstimator] Error parsing AI response: {e}")
            return self._estimate_heuristic(
                story_id, title, description, acceptance_criteria,
                definition_of_done, technical_notes, tags, category, similar_stories
            )

    def _estimate_heuristic(
        self,
        story_id: str,
        title: str,
        description: str,
        acceptance_criteria: List[str],
        definition_of_done: List[str],
        technical_notes: str,
        tags: List[str],
        category: str,
        similar_stories: List[SimilarStory]
    ) -> EffortEstimate:
        """Estimativa heuristica (fallback sem IA)"""

        # Calcula complexidade baseada em fatores
        complexity_score = 0
        risk_factors = []

        # Fator 1: Numero de criterios de aceite
        ac_count = len(acceptance_criteria)
        if ac_count <= 2:
            complexity_score += 1
        elif ac_count <= 5:
            complexity_score += 2
        elif ac_count <= 8:
            complexity_score += 3
        else:
            complexity_score += 4
            risk_factors.append(RiskFactor(
                name="many_acceptance_criteria",
                description=f"Story tem {ac_count} criterios de aceite, o que pode indicar escopo grande demais",
                impact=1.2,
                severity="medium"
            ))

        # Fator 2: Tamanho da descricao
        desc_len = len(description or "")
        if desc_len > 500:
            complexity_score += 1

        # Fator 3: Notas tecnicas
        if technical_notes:
            complexity_score += 1
            # Detecta palavras-chave de risco
            tech_lower = technical_notes.lower()
            if "integracao" in tech_lower or "api externa" in tech_lower:
                risk_factors.append(RiskFactor(
                    name="external_integration",
                    description="Requer integracao com sistemas externos",
                    impact=1.25,
                    severity="medium"
                ))
                complexity_score += 1
            if "migracao" in tech_lower or "migration" in tech_lower:
                risk_factors.append(RiskFactor(
                    name="data_migration",
                    description="Envolve migracao de dados",
                    impact=1.3,
                    severity="high"
                ))
                complexity_score += 2
            if "seguranca" in tech_lower or "security" in tech_lower:
                risk_factors.append(RiskFactor(
                    name="security_sensitive",
                    description="Requisitos de seguranca especiais",
                    impact=1.3,
                    severity="high"
                ))
                complexity_score += 1

        # Fator 4: Categoria
        category_weights = {
            "feature": 0,
            "bug": -1,
            "tech_debt": 1,
            "spike": 2,
            "improvement": 0
        }
        complexity_score += category_weights.get(category, 0)

        # Fator 5: Tags
        high_complexity_tags = ["database", "security", "performance", "integration", "refactoring"]
        for tag in tags:
            if tag.lower() in high_complexity_tags:
                complexity_score += 1

        # Determina complexidade geral
        if complexity_score <= 2:
            complexity = "low"
            base_points = 2
        elif complexity_score <= 4:
            complexity = "medium"
            base_points = 5
        elif complexity_score <= 6:
            complexity = "high"
            base_points = 8
        else:
            complexity = "very_high"
            base_points = 13

        # Ajusta baseado em stories similares
        if similar_stories:
            avg_points = sum(s.story_points for s in similar_stories) / len(similar_stories)
            # Pondera com a estimativa heuristica
            base_points = int((base_points + avg_points) / 2)

        # Arredonda para Fibonacci
        suggested_points = min(FIBONACCI_POINTS, key=lambda x: abs(x - base_points))

        # Calcula horas
        hours_per_point = COMPLEXITY_MAPPING[complexity]["hours_multiplier"] / 2
        suggested_hours = suggested_points * hours_per_point

        # Aplica multiplicadores de risco
        total_risk_impact = 1.0
        for rf in risk_factors:
            total_risk_impact *= rf.impact
        suggested_hours *= total_risk_impact

        # Determina confianca
        if similar_stories and len(similar_stories) >= 3:
            confidence = ConfidenceLevel.HIGH.value
            confidence_score = 85
        elif similar_stories:
            confidence = ConfidenceLevel.MEDIUM.value
            confidence_score = 75
        else:
            confidence = ConfidenceLevel.LOW.value
            confidence_score = 60

        # Monta breakdown
        complexity_breakdown = ComplexityBreakdown(
            requirements_complexity="high" if ac_count > 5 else "medium" if ac_count > 2 else "low",
            technical_complexity="high" if len(risk_factors) > 2 else "medium" if risk_factors else "low",
            integration_complexity="high" if any(r.name == "external_integration" for r in risk_factors) else "low",
            testing_complexity="high" if ac_count > 5 else "medium",
            overall=complexity
        )

        # Sugestoes
        suggestions = []
        if ac_count > 8:
            suggestions.append("Considere quebrar esta story em stories menores")
        if not acceptance_criteria:
            suggestions.append("Adicione criterios de aceite para melhor estimativa")
        if not technical_notes:
            suggestions.append("Adicione notas tecnicas para identificar riscos")
        if confidence == ConfidenceLevel.LOW.value:
            suggestions.append("Realize Planning Poker com o time para validar estimativa")

        return EffortEstimate(
            story_id=story_id,
            suggested_points=suggested_points,
            suggested_hours=round(suggested_hours, 1),
            complexity=complexity,
            confidence=confidence,
            confidence_score=confidence_score,
            complexity_breakdown=complexity_breakdown,
            risk_factors=risk_factors,
            similar_stories=similar_stories,
            reasoning=self._generate_reasoning(
                complexity_score, ac_count, len(risk_factors), similar_stories
            ),
            suggestions=suggestions
        )

    def _find_similar_stories(
        self,
        story: Dict[str, Any],
        project_context: Dict[str, Any] = None
    ) -> List[SimilarStory]:
        """Busca stories similares no historico"""
        similar = []

        if not self.db:
            return similar

        try:
            from factory.database.models import Story, StoryStatus

            # Busca stories concluidas do mesmo projeto
            project_id = story.get("project_id")
            if not project_id:
                return similar

            completed_stories = self.db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == StoryStatus.DONE.value,
                Story.story_points > 0
            ).all()

            # Calcula similaridade simples baseada em tags e categoria
            story_tags = set(story.get("tags", []))
            story_category = story.get("category", "feature")
            story_title_words = set(story.get("title", "").lower().split())

            for cs in completed_stories:
                similarity_score = 0

                # Mesma categoria
                if cs.category == story_category:
                    similarity_score += 30

                # Tags em comum
                cs_tags = set(cs.tags or [])
                common_tags = story_tags & cs_tags
                if common_tags:
                    similarity_score += len(common_tags) * 10

                # Palavras do titulo em comum
                cs_title_words = set(cs.title.lower().split())
                common_words = story_title_words & cs_title_words
                # Remove palavras comuns
                common_words -= {"a", "o", "de", "da", "do", "para", "como", "um", "uma", "e"}
                if common_words:
                    similarity_score += len(common_words) * 5

                # Numero similar de criterios de aceite
                cs_ac_count = len(cs.acceptance_criteria or [])
                story_ac_count = len(story.get("acceptance_criteria", []))
                if abs(cs_ac_count - story_ac_count) <= 2:
                    similarity_score += 15

                if similarity_score >= 30:  # Threshold minimo
                    # Calcula horas reais se disponivel
                    actual_hours = 0
                    if cs.story_tasks:
                        actual_hours = sum(t.actual_hours or 0 for t in cs.story_tasks)
                    if actual_hours == 0:
                        actual_hours = cs.estimated_hours or (cs.story_points * 3)

                    similar.append(SimilarStory(
                        story_id=cs.story_id,
                        title=cs.title,
                        story_points=cs.story_points,
                        actual_hours=actual_hours,
                        similarity_score=min(similarity_score, 100),
                        completed_at=cs.completed_at.isoformat() if cs.completed_at else None
                    ))

            # Ordena por similaridade e retorna top 5
            similar.sort(key=lambda x: x.similarity_score, reverse=True)
            return similar[:5]

        except Exception as e:
            print(f"[EffortEstimator] Error finding similar stories: {e}")
            return []

    def _generate_reasoning(
        self,
        complexity_score: int,
        ac_count: int,
        risk_count: int,
        similar_stories: List[SimilarStory]
    ) -> str:
        """Gera justificativa da estimativa"""
        parts = []

        parts.append(f"Score de complexidade: {complexity_score}/10")

        if ac_count > 5:
            parts.append(f"Alto numero de criterios de aceite ({ac_count}) indica escopo amplo")
        elif ac_count == 0:
            parts.append("Sem criterios de aceite definidos - estimativa menos precisa")

        if risk_count > 0:
            parts.append(f"{risk_count} fator(es) de risco identificado(s)")

        if similar_stories:
            avg_points = sum(s.story_points for s in similar_stories) / len(similar_stories)
            parts.append(f"Media de {avg_points:.1f} pontos em {len(similar_stories)} stories similares")
        else:
            parts.append("Sem historico de stories similares para comparacao")

        return ". ".join(parts) + "."

    def calibrate(self, story_id: str, actual_points: int, actual_hours: float):
        """
        Calibra o modelo com dados reais apos conclusao

        Args:
            story_id: ID da story concluida
            actual_points: Pontos reais (pos-retrospectiva)
            actual_hours: Horas reais gastas
        """
        # TODO: Implementar calibracao continua
        # - Salvar dados de calibracao
        # - Ajustar fatores de complexidade
        # - Melhorar similaridade
        pass


# Factory function
def create_estimator(db_session=None) -> EffortEstimator:
    """Cria instancia do estimador de esforco"""
    return EffortEstimator(db_session)


# Export
__all__ = [
    "EffortEstimator",
    "EffortEstimate",
    "ComplexityBreakdown",
    "RiskFactor",
    "SimilarStory",
    "ConfidenceLevel",
    "FIBONACCI_POINTS",
    "create_estimator"
]

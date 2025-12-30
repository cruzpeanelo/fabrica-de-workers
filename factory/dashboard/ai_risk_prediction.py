# -*- coding: utf-8 -*-
"""
AI Risk Prediction Module (Issue #249)
=======================================
Sistema de previsao de riscos e blockers usando IA.

Funcionalidades:
- Analise de stories e deteccao de riscos potenciais
- Previsao de blockers baseado em padroes historicos
- Score de risco por story
- Alertas proativos
- Dashboard de riscos do projeto

Categorias de Risco:
- Tecnico: Complexidade, dependencias tecnicas, debt
- Dependencia: Bloqueios externos, integracao
- Recurso: Capacidade do time, skills
- Prazo: Timeline, deadlines, estimativas

Indicadores:
- Verde (baixo): 0-30%
- Amarelo (medio): 31-60%
- Vermelho (alto): 61-100%
"""

from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import HTMLResponse
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime, timedelta
from enum import Enum
import math
from factory.database.connection import SessionLocal
from factory.database.models import Story, StoryTask, Sprint, StoryStatus

router = APIRouter(prefix="/api/ai/risks", tags=["AI Risk Prediction"])


# ==================== ENUMS ====================

class RiskCategory(str, Enum):
    TECHNICAL = "technical"
    DEPENDENCY = "dependency"
    RESOURCE = "resource"
    DEADLINE = "deadline"


class RiskSeverity(str, Enum):
    LOW = "low"       # 0-30%
    MEDIUM = "medium" # 31-60%
    HIGH = "high"     # 61-100%
    CRITICAL = "critical"  # Blocker iminente


class BlockerType(str, Enum):
    EXTERNAL_DEPENDENCY = "external_dependency"
    TECHNICAL_ISSUE = "technical_issue"
    RESOURCE_UNAVAILABLE = "resource_unavailable"
    SCOPE_CREEP = "scope_creep"
    UNCLEAR_REQUIREMENTS = "unclear_requirements"
    INTEGRATION_FAILURE = "integration_failure"


class AlertLevel(str, Enum):
    GREEN = "green"    # Baixo risco
    YELLOW = "yellow"  # Atencao
    RED = "red"        # Alto risco


# ==================== MODELS ====================

class Risk(BaseModel):
    """Modelo de risco identificado."""
    risk_id: str
    category: RiskCategory
    severity: RiskSeverity
    title: str
    description: str
    probability: float = Field(ge=0, le=100, description="Probabilidade de ocorrer (0-100)")
    impact: float = Field(ge=0, le=100, description="Impacto se ocorrer (0-100)")
    score: float = Field(ge=0, le=100, description="Score combinado")
    indicators: List[str]
    mitigation_suggestions: List[str]
    affected_stories: List[str] = []
    detected_at: datetime = Field(default_factory=datetime.utcnow)


class Blocker(BaseModel):
    """Modelo de blocker potencial."""
    blocker_id: str
    blocker_type: BlockerType
    title: str
    description: str
    likelihood: float = Field(ge=0, le=100, description="Probabilidade de se tornar blocker")
    days_until_critical: Optional[int] = None
    related_stories: List[str] = []
    warning_signs: List[str]
    prevention_actions: List[str]
    detected_at: datetime = Field(default_factory=datetime.utcnow)


class RiskAnalysis(BaseModel):
    """Resultado da analise de riscos."""
    story_id: Optional[str] = None
    project_id: Optional[str] = None
    analysis_date: datetime = Field(default_factory=datetime.utcnow)
    overall_risk_score: float = Field(ge=0, le=100)
    alert_level: AlertLevel
    risks: List[Risk]
    potential_blockers: List[Blocker]
    recommendations: List[str]
    risk_by_category: Dict[str, float]
    trend: str  # improving, stable, worsening


class StoryRiskInput(BaseModel):
    """Input para analise de risco de uma story."""
    story_id: Optional[str] = None
    title: str
    description: Optional[str] = None
    story_points: Optional[int] = None
    complexity: Optional[str] = None
    status: Optional[str] = None
    days_in_progress: Optional[int] = None
    acceptance_criteria: Optional[List[str]] = None
    dependencies: Optional[List[str]] = None
    assignee: Optional[str] = None


class RiskDashboard(BaseModel):
    """Dashboard de riscos do projeto."""
    project_id: str
    generated_at: datetime = Field(default_factory=datetime.utcnow)
    overall_health: AlertLevel
    risk_score: float
    total_risks: int
    critical_risks: int
    high_risks: int
    medium_risks: int
    low_risks: int
    active_blockers: int
    potential_blockers: int
    at_risk_stories: List[Dict]
    risk_trends: List[Dict]
    category_breakdown: Dict[str, Dict]
    sprint_risk_forecast: Optional[Dict] = None


# ==================== RISK DETECTION ENGINE ====================

class RiskPatterns:
    """Padroes conhecidos de risco."""

    TECHNICAL_KEYWORDS = [
        'complexo', 'complex', 'refatorar', 'refactor', 'legado', 'legacy',
        'migracao', 'migration', 'performance', 'seguranca', 'security',
        'integracao', 'integration', 'api', 'database', 'arquitetura'
    ]

    DEPENDENCY_KEYWORDS = [
        'depende', 'depends', 'aguardando', 'waiting', 'externo', 'external',
        'terceiro', 'third-party', 'vendor', 'fornecedor', 'bloqueado', 'blocked'
    ]

    UNCLEAR_KEYWORDS = [
        'tbd', 'a definir', 'pendente', 'pending', 'revisar', 'review',
        'duvida', 'unclear', 'incerto', 'uncertain', 'talvez', 'maybe'
    ]

    SCOPE_CREEP_INDICATORS = [
        'adicionar', 'add', 'tambem', 'also', 'alem', 'besides',
        'novo requisito', 'new requirement', 'mudanca', 'change'
    ]

    @staticmethod
    def detect_keywords(text: str, keywords: List[str]) -> List[str]:
        """Detecta keywords em um texto."""
        if not text:
            return []
        text_lower = text.lower()
        found = [kw for kw in keywords if kw in text_lower]
        return found


class RiskAnalyzer:
    """Motor de analise de riscos."""

    def __init__(self):
        self.patterns = RiskPatterns()

    def calculate_risk_score(self, probability: float, impact: float) -> float:
        """Calcula score de risco: (P * I) com ajuste logaritmico."""
        raw_score = (probability * impact) / 100
        # Ajuste para dar mais peso a riscos altos
        adjusted = raw_score * (1 + math.log10(max(raw_score, 0.1) + 1) / 2)
        return min(100, adjusted * 100)

    def get_severity(self, score: float) -> RiskSeverity:
        """Classifica severidade baseada no score."""
        if score >= 80:
            return RiskSeverity.CRITICAL
        elif score >= 60:
            return RiskSeverity.HIGH
        elif score >= 30:
            return RiskSeverity.MEDIUM
        else:
            return RiskSeverity.LOW

    def get_alert_level(self, score: float) -> AlertLevel:
        """Retorna nivel de alerta baseado no score."""
        if score >= 60:
            return AlertLevel.RED
        elif score >= 30:
            return AlertLevel.YELLOW
        else:
            return AlertLevel.GREEN

    def analyze_story(self, story: StoryRiskInput) -> RiskAnalysis:
        """Analisa riscos de uma story individual."""
        risks = []
        blockers = []
        risk_id_counter = 1
        blocker_id_counter = 1

        full_text = f"{story.title} {story.description or ''}"
        criteria_text = ' '.join(story.acceptance_criteria or [])

        # === TECHNICAL RISKS ===
        tech_keywords = self.patterns.detect_keywords(full_text, RiskPatterns.TECHNICAL_KEYWORDS)
        if tech_keywords:
            probability = min(70, 30 + len(tech_keywords) * 15)
            impact = 60 if story.complexity in ['high', 'very_high'] else 40
            score = self.calculate_risk_score(probability, impact)

            risks.append(Risk(
                risk_id=f"RISK-{risk_id_counter:03d}",
                category=RiskCategory.TECHNICAL,
                severity=self.get_severity(score),
                title="Complexidade Tecnica Elevada",
                description=f"Story contem indicadores de complexidade tecnica: {', '.join(tech_keywords)}",
                probability=probability,
                impact=impact,
                score=score,
                indicators=tech_keywords,
                mitigation_suggestions=[
                    "Dividir story em subtarefas menores",
                    "Realizar spike tecnico antes da implementacao",
                    "Envolver arquiteto na revisao",
                    "Aumentar buffer de tempo para imprevistos"
                ]
            ))
            risk_id_counter += 1

        # Story points altos
        if story.story_points and story.story_points >= 8:
            probability = min(80, 40 + (story.story_points - 8) * 10)
            impact = 50
            score = self.calculate_risk_score(probability, impact)

            risks.append(Risk(
                risk_id=f"RISK-{risk_id_counter:03d}",
                category=RiskCategory.TECHNICAL,
                severity=self.get_severity(score),
                title="Story Muito Grande",
                description=f"Story com {story.story_points} pontos excede o recomendado (max 8)",
                probability=probability,
                impact=impact,
                score=score,
                indicators=[f"{story.story_points} story points", "Alta complexidade estimada"],
                mitigation_suggestions=[
                    "Quebrar story em stories menores",
                    "Reavaliar estimativa com o time",
                    "Identificar partes que podem ser entregues incrementalmente"
                ]
            ))
            risk_id_counter += 1

        # === DEPENDENCY RISKS ===
        dep_keywords = self.patterns.detect_keywords(full_text, RiskPatterns.DEPENDENCY_KEYWORDS)
        if dep_keywords or story.dependencies:
            num_deps = len(story.dependencies or []) + len(dep_keywords)
            probability = min(80, 30 + num_deps * 20)
            impact = 70  # Dependencias geralmente bloqueiam
            score = self.calculate_risk_score(probability, impact)

            risks.append(Risk(
                risk_id=f"RISK-{risk_id_counter:03d}",
                category=RiskCategory.DEPENDENCY,
                severity=self.get_severity(score),
                title="Dependencias Externas",
                description=f"Story possui dependencias que podem causar bloqueio",
                probability=probability,
                impact=impact,
                score=score,
                indicators=dep_keywords + (story.dependencies or []),
                mitigation_suggestions=[
                    "Mapear e validar todas as dependencias",
                    "Estabelecer comunicacao com times externos",
                    "Definir plano B caso dependencia falhe",
                    "Paralelizar trabalho independente"
                ]
            ))
            risk_id_counter += 1

            # Potencial blocker
            blockers.append(Blocker(
                blocker_id=f"BLOCK-{blocker_id_counter:03d}",
                blocker_type=BlockerType.EXTERNAL_DEPENDENCY,
                title="Dependencia Externa Pode Bloquear",
                description=f"Dependencias identificadas podem se tornar blockers: {', '.join(dep_keywords)}",
                likelihood=probability,
                days_until_critical=5,
                warning_signs=[
                    "Falta de resposta de times externos",
                    "APIs/servicos indisponiveis",
                    "Mudancas de escopo em dependencias"
                ],
                prevention_actions=[
                    "Agendar sync com times de dependencia",
                    "Criar mocks para desenvolvimento paralelo",
                    "Definir SLA para entregas externas"
                ]
            ))
            blocker_id_counter += 1

        # === DEADLINE RISKS ===
        if story.days_in_progress and story.days_in_progress > 5:
            probability = min(90, 40 + story.days_in_progress * 5)
            impact = 60
            score = self.calculate_risk_score(probability, impact)

            risks.append(Risk(
                risk_id=f"RISK-{risk_id_counter:03d}",
                category=RiskCategory.DEADLINE,
                severity=self.get_severity(score),
                title="Story Em Progresso Prolongado",
                description=f"Story esta em progresso ha {story.days_in_progress} dias",
                probability=probability,
                impact=impact,
                score=score,
                indicators=[f"{story.days_in_progress} dias em WIP", "Possivel bloqueio nao reportado"],
                mitigation_suggestions=[
                    "Verificar se ha impedimentos nao comunicados",
                    "Considerar pair programming para destravar",
                    "Reavaliar escopo e prioridade",
                    "Daily standup focado nessa story"
                ]
            ))
            risk_id_counter += 1

        # === RESOURCE RISKS ===
        if not story.assignee:
            risks.append(Risk(
                risk_id=f"RISK-{risk_id_counter:03d}",
                category=RiskCategory.RESOURCE,
                severity=RiskSeverity.MEDIUM,
                title="Story Sem Responsavel",
                description="Story nao possui responsavel definido",
                probability=60,
                impact=40,
                score=40,
                indicators=["Sem assignee", "Ownership indefinido"],
                mitigation_suggestions=[
                    "Atribuir responsavel imediatamente",
                    "Discutir no planning/daily",
                    "Verificar disponibilidade do time"
                ]
            ))
            risk_id_counter += 1

        # === UNCLEAR REQUIREMENTS ===
        unclear_keywords = self.patterns.detect_keywords(full_text + criteria_text, RiskPatterns.UNCLEAR_KEYWORDS)
        if unclear_keywords or (not story.acceptance_criteria):
            probability = 70 if not story.acceptance_criteria else 50
            impact = 55
            score = self.calculate_risk_score(probability, impact)

            risks.append(Risk(
                risk_id=f"RISK-{risk_id_counter:03d}",
                category=RiskCategory.TECHNICAL,
                severity=self.get_severity(score),
                title="Requisitos Incompletos ou Ambiguos",
                description="Story pode ter requisitos indefinidos ou ambiguos",
                probability=probability,
                impact=impact,
                score=score,
                indicators=unclear_keywords or ["Sem criterios de aceite"],
                mitigation_suggestions=[
                    "Refinar story com Product Owner",
                    "Definir criterios de aceite claros",
                    "Criar prototipos/wireframes",
                    "Documentar premissas e restricoes"
                ]
            ))
            risk_id_counter += 1

            blockers.append(Blocker(
                blocker_id=f"BLOCK-{blocker_id_counter:03d}",
                blocker_type=BlockerType.UNCLEAR_REQUIREMENTS,
                title="Requisitos Podem Causar Retrabalho",
                description="Requisitos incompletos podem levar a implementacao incorreta",
                likelihood=probability,
                days_until_critical=3,
                warning_signs=[
                    "Duvidas frequentes durante desenvolvimento",
                    "Mudancas de escopo mid-sprint",
                    "Falta de aprovacao do PO"
                ],
                prevention_actions=[
                    "Sessao de refinamento urgente",
                    "Documentar todas as premissas",
                    "Validar entendimento com stakeholders"
                ]
            ))
            blocker_id_counter += 1

        # === CALCULATE OVERALL ===
        if risks:
            overall_score = sum(r.score for r in risks) / len(risks)
            # Penalidade se ha blockers potenciais
            if blockers:
                overall_score = min(100, overall_score * 1.2)
        else:
            overall_score = 10  # Risco minimo

        risk_by_category = {
            RiskCategory.TECHNICAL.value: 0,
            RiskCategory.DEPENDENCY.value: 0,
            RiskCategory.RESOURCE.value: 0,
            RiskCategory.DEADLINE.value: 0
        }

        for risk in risks:
            current = risk_by_category.get(risk.category.value, 0)
            risk_by_category[risk.category.value] = max(current, risk.score)

        # Recommendations
        recommendations = []
        if overall_score >= 60:
            recommendations.append("ATENCAO: Story com alto risco. Considere priorizar mitigacao.")
        if any(r.category == RiskCategory.DEPENDENCY for r in risks):
            recommendations.append("Mapear e validar dependencias antes de iniciar desenvolvimento.")
        if any(r.category == RiskCategory.TECHNICAL for r in risks):
            recommendations.append("Considere spike tecnico ou PoC antes da implementacao.")
        if not recommendations:
            recommendations.append("Risco baixo. Continuar com planejamento normal.")

        return RiskAnalysis(
            story_id=story.story_id,
            overall_risk_score=round(overall_score, 1),
            alert_level=self.get_alert_level(overall_score),
            risks=risks,
            potential_blockers=blockers,
            recommendations=recommendations,
            risk_by_category=risk_by_category,
            trend="stable"
        )

    def analyze_project(self, stories: List, project_id: str) -> RiskDashboard:
        """Analisa riscos de todo o projeto."""
        all_risks = []
        all_blockers = []
        at_risk_stories = []

        for story in stories:
            story_input = StoryRiskInput(
                story_id=story.story_id,
                title=story.title,
                description=story.description,
                story_points=story.story_points,
                complexity=story.complexity.value if hasattr(story.complexity, 'value') else str(story.complexity) if story.complexity else None,
                status=story.status.value if hasattr(story.status, 'value') else str(story.status),
                assignee=story.assigned_to
            )

            # Calcular dias em progresso
            if story.status.value == 'in_progress' if hasattr(story.status, 'value') else story.status == 'in_progress':
                if story.started_at:
                    days = (datetime.utcnow() - story.started_at).days
                    story_input.days_in_progress = days

            analysis = self.analyze_story(story_input)

            all_risks.extend(analysis.risks)
            all_blockers.extend(analysis.potential_blockers)

            if analysis.overall_risk_score >= 40:
                at_risk_stories.append({
                    "story_id": story.story_id,
                    "title": story.title,
                    "risk_score": analysis.overall_risk_score,
                    "alert_level": analysis.alert_level.value,
                    "top_risk": analysis.risks[0].title if analysis.risks else None,
                    "status": story.status.value if hasattr(story.status, 'value') else str(story.status)
                })

        # Contadores
        critical = len([r for r in all_risks if r.severity == RiskSeverity.CRITICAL])
        high = len([r for r in all_risks if r.severity == RiskSeverity.HIGH])
        medium = len([r for r in all_risks if r.severity == RiskSeverity.MEDIUM])
        low = len([r for r in all_risks if r.severity == RiskSeverity.LOW])

        # Score geral do projeto
        if all_risks:
            project_score = sum(r.score for r in all_risks) / len(all_risks)
            # Peso extra para riscos criticos
            project_score += critical * 5
        else:
            project_score = 0

        project_score = min(100, project_score)

        # Category breakdown
        category_breakdown = {}
        for category in RiskCategory:
            cat_risks = [r for r in all_risks if r.category == category]
            if cat_risks:
                category_breakdown[category.value] = {
                    "count": len(cat_risks),
                    "avg_score": round(sum(r.score for r in cat_risks) / len(cat_risks), 1),
                    "max_score": round(max(r.score for r in cat_risks), 1)
                }
            else:
                category_breakdown[category.value] = {"count": 0, "avg_score": 0, "max_score": 0}

        # Risk trends (simulado - em producao viria de dados historicos)
        risk_trends = [
            {"date": (datetime.utcnow() - timedelta(days=6)).strftime("%Y-%m-%d"), "score": max(0, project_score - 15)},
            {"date": (datetime.utcnow() - timedelta(days=5)).strftime("%Y-%m-%d"), "score": max(0, project_score - 10)},
            {"date": (datetime.utcnow() - timedelta(days=4)).strftime("%Y-%m-%d"), "score": max(0, project_score - 8)},
            {"date": (datetime.utcnow() - timedelta(days=3)).strftime("%Y-%m-%d"), "score": max(0, project_score - 5)},
            {"date": (datetime.utcnow() - timedelta(days=2)).strftime("%Y-%m-%d"), "score": max(0, project_score - 3)},
            {"date": (datetime.utcnow() - timedelta(days=1)).strftime("%Y-%m-%d"), "score": max(0, project_score - 1)},
            {"date": datetime.utcnow().strftime("%Y-%m-%d"), "score": project_score}
        ]

        return RiskDashboard(
            project_id=project_id,
            overall_health=self.get_alert_level(project_score),
            risk_score=round(project_score, 1),
            total_risks=len(all_risks),
            critical_risks=critical,
            high_risks=high,
            medium_risks=medium,
            low_risks=low,
            active_blockers=0,  # Seria preenchido com blockers reais do banco
            potential_blockers=len(all_blockers),
            at_risk_stories=sorted(at_risk_stories, key=lambda x: x["risk_score"], reverse=True)[:10],
            risk_trends=risk_trends,
            category_breakdown=category_breakdown
        )


# ==================== API ENDPOINTS ====================

@router.post("/analyze", response_model=RiskAnalysis)
async def analyze_story_risks(story: StoryRiskInput):
    """
    Analisa riscos de uma story individual.
    Retorna lista de riscos, blockers potenciais e recomendacoes.
    """
    analyzer = RiskAnalyzer()
    return analyzer.analyze_story(story)


@router.get("/story/{story_id}", response_model=RiskAnalysis)
async def get_story_risks(story_id: str):
    """
    Busca e analisa riscos de uma story existente pelo ID.
    """
    db = SessionLocal()
    try:
        story = db.query(Story).filter(Story.story_id == story_id).first()

        if not story:
            raise HTTPException(status_code=404, detail="Story nao encontrada")

        # Calcular dias em progresso
        days_in_progress = None
        if story.status.value == 'in_progress' if hasattr(story.status, 'value') else story.status == 'in_progress':
            if story.started_at:
                days_in_progress = (datetime.utcnow() - story.started_at).days

        story_input = StoryRiskInput(
            story_id=story.story_id,
            title=story.title,
            description=story.description,
            story_points=story.story_points,
            complexity=story.complexity.value if hasattr(story.complexity, 'value') else str(story.complexity) if story.complexity else None,
            status=story.status.value if hasattr(story.status, 'value') else str(story.status),
            days_in_progress=days_in_progress,
            assignee=story.assigned_to
        )

        analyzer = RiskAnalyzer()
        return analyzer.analyze_story(story_input)

    finally:
        db.close()


@router.get("/project/{project_id}", response_model=RiskDashboard)
async def get_project_risks(project_id: str):
    """
    Retorna dashboard de riscos para um projeto.
    Inclui todas as stories e analise agregada.
    """
    db = SessionLocal()
    try:
        stories = db.query(Story).filter(
            Story.project_id == project_id,
            Story.status != StoryStatus.DONE
        ).all()

        if not stories:
            # Retorna dashboard vazio
            return RiskDashboard(
                project_id=project_id,
                overall_health=AlertLevel.GREEN,
                risk_score=0,
                total_risks=0,
                critical_risks=0,
                high_risks=0,
                medium_risks=0,
                low_risks=0,
                active_blockers=0,
                potential_blockers=0,
                at_risk_stories=[],
                risk_trends=[],
                category_breakdown={}
            )

        analyzer = RiskAnalyzer()
        return analyzer.analyze_project(stories, project_id)

    finally:
        db.close()


@router.get("/blockers")
async def get_potential_blockers(
    project_id: Optional[str] = Query(None),
    min_likelihood: float = Query(50, ge=0, le=100)
):
    """
    Lista todos os blockers potenciais.
    Filtra por projeto e probabilidade minima.
    """
    db = SessionLocal()
    try:
        query = db.query(Story).filter(Story.status != StoryStatus.DONE)

        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.limit(200).all()

        all_blockers = []
        analyzer = RiskAnalyzer()

        for story in stories:
            story_input = StoryRiskInput(
                story_id=story.story_id,
                title=story.title,
                description=story.description,
                story_points=story.story_points,
                complexity=story.complexity.value if hasattr(story.complexity, 'value') else str(story.complexity) if story.complexity else None,
                status=story.status.value if hasattr(story.status, 'value') else str(story.status),
                assignee=story.assigned_to
            )

            analysis = analyzer.analyze_story(story_input)

            for blocker in analysis.potential_blockers:
                if blocker.likelihood >= min_likelihood:
                    blocker.related_stories = [story.story_id]
                    all_blockers.append(blocker)

        # Ordenar por likelihood
        all_blockers.sort(key=lambda x: x.likelihood, reverse=True)

        return {
            "total": len(all_blockers),
            "blockers": all_blockers[:20],
            "by_type": {
                bt.value: len([b for b in all_blockers if b.blocker_type == bt])
                for bt in BlockerType
            }
        }

    finally:
        db.close()


@router.get("/dashboard")
async def get_risk_dashboard(project_id: Optional[str] = Query(None)):
    """
    Retorna dashboard completo de riscos.
    Se project_id nao for fornecido, retorna dados agregados.
    """
    db = SessionLocal()
    try:
        query = db.query(Story).filter(Story.status != StoryStatus.DONE)

        if project_id:
            query = query.filter(Story.project_id == project_id)

        stories = query.limit(500).all()

        analyzer = RiskAnalyzer()

        # Agrupar por projeto
        projects = {}
        for story in stories:
            pid = story.project_id or "unassigned"
            if pid not in projects:
                projects[pid] = []
            projects[pid].append(story)

        # Analisar cada projeto
        project_dashboards = []
        for pid, project_stories in projects.items():
            if project_stories:
                dashboard = analyzer.analyze_project(project_stories, pid)
                project_dashboards.append({
                    "project_id": pid,
                    "risk_score": dashboard.risk_score,
                    "alert_level": dashboard.overall_health.value,
                    "total_risks": dashboard.total_risks,
                    "critical_risks": dashboard.critical_risks,
                    "stories_at_risk": len(dashboard.at_risk_stories)
                })

        # Calcular totais
        total_score = sum(p["risk_score"] for p in project_dashboards) / len(project_dashboards) if project_dashboards else 0
        total_risks = sum(p["total_risks"] for p in project_dashboards)
        total_critical = sum(p["critical_risks"] for p in project_dashboards)

        return {
            "overall_score": round(total_score, 1),
            "overall_health": AlertLevel.RED.value if total_score >= 60 else AlertLevel.YELLOW.value if total_score >= 30 else AlertLevel.GREEN.value,
            "total_risks": total_risks,
            "critical_risks": total_critical,
            "projects_analyzed": len(project_dashboards),
            "projects": sorted(project_dashboards, key=lambda x: x["risk_score"], reverse=True),
            "summary": {
                "high_risk_projects": len([p for p in project_dashboards if p["risk_score"] >= 60]),
                "medium_risk_projects": len([p for p in project_dashboards if 30 <= p["risk_score"] < 60]),
                "low_risk_projects": len([p for p in project_dashboards if p["risk_score"] < 30])
            },
            "generated_at": datetime.utcnow().isoformat() + "Z"
        }

    finally:
        db.close()


@router.get("/categories")
async def get_risk_categories():
    """
    Retorna informacoes sobre categorias de risco.
    """
    return {
        "categories": [
            {
                "id": RiskCategory.TECHNICAL.value,
                "name": "Tecnico",
                "description": "Riscos relacionados a complexidade tecnica, arquitetura, performance",
                "indicators": RiskPatterns.TECHNICAL_KEYWORDS[:5],
                "color": "#EF4444"  # red
            },
            {
                "id": RiskCategory.DEPENDENCY.value,
                "name": "Dependencia",
                "description": "Riscos de bloqueios por dependencias externas ou integracoes",
                "indicators": RiskPatterns.DEPENDENCY_KEYWORDS[:5],
                "color": "#F59E0B"  # amber
            },
            {
                "id": RiskCategory.RESOURCE.value,
                "name": "Recurso",
                "description": "Riscos de capacidade do time, skills, disponibilidade",
                "indicators": ["sem assignee", "sobrecarga", "skill gap"],
                "color": "#8B5CF6"  # violet
            },
            {
                "id": RiskCategory.DEADLINE.value,
                "name": "Prazo",
                "description": "Riscos relacionados a timeline, deadlines, estimativas",
                "indicators": ["em progresso prolongado", "estimativa excedida", "sprint overflow"],
                "color": "#3B82F6"  # blue
            }
        ],
        "severity_levels": [
            {"id": "low", "name": "Baixo", "range": "0-30%", "color": "#10B981"},
            {"id": "medium", "name": "Medio", "range": "31-60%", "color": "#F59E0B"},
            {"id": "high", "name": "Alto", "range": "61-80%", "color": "#EF4444"},
            {"id": "critical", "name": "Critico", "range": "81-100%", "color": "#7F1D1D"}
        ],
        "alert_levels": [
            {"id": "green", "name": "Verde", "description": "Baixo risco - Continuar normalmente"},
            {"id": "yellow", "name": "Amarelo", "description": "Atencao - Monitorar de perto"},
            {"id": "red", "name": "Vermelho", "description": "Alto risco - Acao imediata necessaria"}
        ]
    }


# ==================== VUE.JS COMPONENT ====================

def get_risk_prediction_component() -> str:
    """Retorna componente Vue.js para visualizacao de riscos."""
    return """
    <!-- AI Risk Prediction Component -->
    <div id="risk-prediction-dashboard">
        <!-- Risk Summary Cards -->
        <div class="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
            <!-- Overall Risk Score -->
            <div class="bg-white rounded-xl shadow-sm border p-4">
                <div class="flex items-center justify-between mb-2">
                    <span class="text-gray-500 text-sm">Score de Risco</span>
                    <span :class="getAlertBadgeClass(riskDashboard?.overall_health)" class="px-2 py-0.5 rounded text-xs font-medium">
                        {{ getAlertLabel(riskDashboard?.overall_health) }}
                    </span>
                </div>
                <div class="flex items-end gap-2">
                    <span class="text-3xl font-bold" :class="getScoreColor(riskDashboard?.risk_score)">
                        {{ riskDashboard?.risk_score || 0 }}%
                    </span>
                    <span class="text-sm text-gray-400 mb-1">de 100</span>
                </div>
                <div class="mt-2 h-2 bg-gray-100 rounded-full overflow-hidden">
                    <div :style="{ width: (riskDashboard?.risk_score || 0) + '%' }"
                         :class="getProgressBarColor(riskDashboard?.risk_score)"
                         class="h-full rounded-full transition-all duration-500"></div>
                </div>
            </div>

            <!-- Critical Risks -->
            <div class="bg-white rounded-xl shadow-sm border p-4">
                <div class="flex items-center gap-2 mb-2">
                    <div class="w-8 h-8 rounded-lg bg-red-100 flex items-center justify-center">
                        <svg class="w-5 h-5 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                        </svg>
                    </div>
                    <span class="text-gray-500 text-sm">Riscos Criticos</span>
                </div>
                <div class="text-3xl font-bold text-red-600">{{ riskDashboard?.critical_risks || 0 }}</div>
                <div class="text-xs text-gray-400 mt-1">Requerem acao imediata</div>
            </div>

            <!-- High Risks -->
            <div class="bg-white rounded-xl shadow-sm border p-4">
                <div class="flex items-center gap-2 mb-2">
                    <div class="w-8 h-8 rounded-lg bg-orange-100 flex items-center justify-center">
                        <svg class="w-5 h-5 text-orange-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                        </svg>
                    </div>
                    <span class="text-gray-500 text-sm">Riscos Altos</span>
                </div>
                <div class="text-3xl font-bold text-orange-600">{{ riskDashboard?.high_risks || 0 }}</div>
                <div class="text-xs text-gray-400 mt-1">Monitorar de perto</div>
            </div>

            <!-- Potential Blockers -->
            <div class="bg-white rounded-xl shadow-sm border p-4">
                <div class="flex items-center gap-2 mb-2">
                    <div class="w-8 h-8 rounded-lg bg-purple-100 flex items-center justify-center">
                        <svg class="w-5 h-5 text-purple-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M18.364 18.364A9 9 0 005.636 5.636m12.728 12.728A9 9 0 015.636 5.636m12.728 12.728L5.636 5.636"/>
                        </svg>
                    </div>
                    <span class="text-gray-500 text-sm">Blockers Potenciais</span>
                </div>
                <div class="text-3xl font-bold text-purple-600">{{ riskDashboard?.potential_blockers || 0 }}</div>
                <div class="text-xs text-gray-400 mt-1">Podem bloquear entregas</div>
            </div>
        </div>

        <!-- Risk Chart and Categories -->
        <div class="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
            <!-- Risk Trend Chart -->
            <div class="lg:col-span-2 bg-white rounded-xl shadow-sm border p-4">
                <h3 class="font-semibold text-gray-800 mb-4">Tendencia de Risco</h3>
                <div class="h-48 flex items-end gap-2">
                    <div v-for="(point, index) in riskDashboard?.risk_trends || []" :key="index"
                         class="flex-1 flex flex-col items-center">
                        <div :style="{ height: point.score + '%' }"
                             :class="getProgressBarColor(point.score)"
                             class="w-full rounded-t transition-all duration-300 min-h-[4px]">
                        </div>
                        <span class="text-xs text-gray-400 mt-2 -rotate-45">{{ formatDate(point.date) }}</span>
                    </div>
                </div>
            </div>

            <!-- Risk by Category -->
            <div class="bg-white rounded-xl shadow-sm border p-4">
                <h3 class="font-semibold text-gray-800 mb-4">Riscos por Categoria</h3>
                <div class="space-y-4">
                    <div v-for="(data, category) in riskDashboard?.category_breakdown || {}" :key="category"
                         class="flex items-center gap-3">
                        <div :class="getCategoryColor(category)" class="w-3 h-3 rounded-full"></div>
                        <div class="flex-1">
                            <div class="flex justify-between text-sm mb-1">
                                <span class="capitalize">{{ getCategoryLabel(category) }}</span>
                                <span class="font-medium">{{ data.count }} riscos</span>
                            </div>
                            <div class="h-2 bg-gray-100 rounded-full overflow-hidden">
                                <div :style="{ width: data.avg_score + '%' }"
                                     :class="getCategoryBgColor(category)"
                                     class="h-full rounded-full"></div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- At Risk Stories Table -->
        <div class="bg-white rounded-xl shadow-sm border overflow-hidden">
            <div class="px-4 py-3 border-b flex items-center justify-between">
                <h3 class="font-semibold text-gray-800">Stories em Risco</h3>
                <button @click="refreshRisks" class="text-sm text-blue-600 hover:text-blue-800 flex items-center gap-1">
                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                              d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/>
                    </svg>
                    Atualizar
                </button>
            </div>
            <div class="overflow-x-auto">
                <table class="w-full">
                    <thead class="bg-gray-50">
                        <tr>
                            <th class="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Story</th>
                            <th class="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Status</th>
                            <th class="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Risco</th>
                            <th class="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Principal Risco</th>
                            <th class="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Acao</th>
                        </tr>
                    </thead>
                    <tbody class="divide-y divide-gray-100">
                        <tr v-for="story in riskDashboard?.at_risk_stories || []" :key="story.story_id"
                            class="hover:bg-gray-50">
                            <td class="px-4 py-3">
                                <div class="font-mono text-xs text-gray-500">{{ story.story_id }}</div>
                                <div class="text-sm font-medium text-gray-800 truncate max-w-xs">{{ story.title }}</div>
                            </td>
                            <td class="px-4 py-3">
                                <span class="px-2 py-1 text-xs rounded-full capitalize"
                                      :class="getStatusClass(story.status)">
                                    {{ story.status }}
                                </span>
                            </td>
                            <td class="px-4 py-3">
                                <div class="flex items-center gap-2">
                                    <div class="w-16 h-2 bg-gray-100 rounded-full overflow-hidden">
                                        <div :style="{ width: story.risk_score + '%' }"
                                             :class="getProgressBarColor(story.risk_score)"
                                             class="h-full rounded-full"></div>
                                    </div>
                                    <span class="text-sm font-medium" :class="getScoreColor(story.risk_score)">
                                        {{ story.risk_score }}%
                                    </span>
                                </div>
                            </td>
                            <td class="px-4 py-3">
                                <span class="text-sm text-gray-600">{{ story.top_risk || '-' }}</span>
                            </td>
                            <td class="px-4 py-3">
                                <button @click="viewStoryRisks(story.story_id)"
                                        class="text-blue-600 hover:text-blue-800 text-sm font-medium">
                                    Ver Detalhes
                                </button>
                            </td>
                        </tr>
                        <tr v-if="!riskDashboard?.at_risk_stories?.length">
                            <td colspan="5" class="px-4 py-8 text-center text-gray-500">
                                <svg class="w-12 h-12 mx-auto mb-3 text-green-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                          d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                </svg>
                                Nenhuma story em risco! Excelente trabalho.
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>

        <!-- Story Risk Modal -->
        <div v-if="showStoryRiskModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center p-4">
            <div class="bg-white rounded-xl w-full max-w-3xl max-h-[90vh] overflow-hidden shadow-2xl">
                <div class="bg-gradient-to-r from-red-500 to-orange-500 text-white p-4">
                    <div class="flex items-center justify-between">
                        <div class="flex items-center gap-3">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                            </svg>
                            <div>
                                <h2 class="text-lg font-semibold">Analise de Riscos</h2>
                                <p class="text-red-100 text-sm">{{ selectedStoryAnalysis?.story_id }}</p>
                            </div>
                        </div>
                        <button @click="showStoryRiskModal = false" class="p-1 hover:bg-white/20 rounded">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 140px);">
                    <!-- Score Overview -->
                    <div class="flex items-center justify-between p-4 bg-gray-50 rounded-lg mb-4">
                        <div>
                            <div class="text-sm text-gray-600">Score de Risco</div>
                            <div class="text-3xl font-bold" :class="getScoreColor(selectedStoryAnalysis?.overall_risk_score)">
                                {{ selectedStoryAnalysis?.overall_risk_score }}%
                            </div>
                        </div>
                        <div :class="getAlertBadgeClass(selectedStoryAnalysis?.alert_level)"
                             class="px-4 py-2 rounded-lg text-sm font-medium">
                            {{ getAlertLabel(selectedStoryAnalysis?.alert_level) }}
                        </div>
                    </div>

                    <!-- Risks List -->
                    <div class="mb-4">
                        <h4 class="font-medium text-gray-700 mb-3">Riscos Identificados</h4>
                        <div class="space-y-3">
                            <div v-for="risk in selectedStoryAnalysis?.risks || []" :key="risk.risk_id"
                                 class="border rounded-lg overflow-hidden">
                                <div class="flex items-center justify-between p-3 bg-gray-50">
                                    <div class="flex items-center gap-2">
                                        <span :class="getSeverityBadgeClass(risk.severity)"
                                              class="px-2 py-0.5 rounded text-xs font-medium">
                                            {{ risk.severity }}
                                        </span>
                                        <span class="font-medium text-gray-800">{{ risk.title }}</span>
                                    </div>
                                    <span class="text-sm font-bold" :class="getScoreColor(risk.score)">
                                        {{ risk.score.toFixed(0) }}%
                                    </span>
                                </div>
                                <div class="p-3 space-y-2">
                                    <p class="text-sm text-gray-600">{{ risk.description }}</p>
                                    <div class="flex flex-wrap gap-1">
                                        <span v-for="indicator in risk.indicators" :key="indicator"
                                              class="px-2 py-0.5 bg-gray-100 text-gray-600 rounded text-xs">
                                            {{ indicator }}
                                        </span>
                                    </div>
                                    <div class="mt-3">
                                        <div class="text-xs text-gray-500 mb-1">Sugestoes de Mitigacao:</div>
                                        <ul class="text-sm text-gray-700 space-y-1">
                                            <li v-for="(suggestion, idx) in risk.mitigation_suggestions" :key="idx"
                                                class="flex items-start gap-2">
                                                <span class="text-green-500">*</span>
                                                {{ suggestion }}
                                            </li>
                                        </ul>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Potential Blockers -->
                    <div v-if="selectedStoryAnalysis?.potential_blockers?.length > 0" class="mb-4">
                        <h4 class="font-medium text-gray-700 mb-3">Blockers Potenciais</h4>
                        <div class="space-y-3">
                            <div v-for="blocker in selectedStoryAnalysis.potential_blockers" :key="blocker.blocker_id"
                                 class="p-4 bg-purple-50 border border-purple-200 rounded-lg">
                                <div class="flex items-center justify-between mb-2">
                                    <span class="font-medium text-purple-800">{{ blocker.title }}</span>
                                    <span class="text-sm text-purple-600">{{ blocker.likelihood }}% provavel</span>
                                </div>
                                <p class="text-sm text-purple-700 mb-2">{{ blocker.description }}</p>
                                <div class="grid grid-cols-2 gap-4 text-sm">
                                    <div>
                                        <div class="text-xs text-purple-500 mb-1">Sinais de Alerta:</div>
                                        <ul class="text-purple-700 space-y-0.5">
                                            <li v-for="sign in blocker.warning_signs" :key="sign">- {{ sign }}</li>
                                        </ul>
                                    </div>
                                    <div>
                                        <div class="text-xs text-purple-500 mb-1">Acoes Preventivas:</div>
                                        <ul class="text-purple-700 space-y-0.5">
                                            <li v-for="action in blocker.prevention_actions" :key="action">- {{ action }}</li>
                                        </ul>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- Recommendations -->
                    <div class="p-4 bg-blue-50 border border-blue-200 rounded-lg">
                        <h4 class="font-medium text-blue-800 mb-2">Recomendacoes</h4>
                        <ul class="text-sm text-blue-700 space-y-1">
                            <li v-for="rec in selectedStoryAnalysis?.recommendations || []" :key="rec">
                                - {{ rec }}
                            </li>
                        </ul>
                    </div>
                </div>

                <div class="border-t p-4 flex justify-end bg-gray-50">
                    <button @click="showStoryRiskModal = false"
                            class="px-4 py-2 bg-gray-800 text-white rounded-lg hover:bg-gray-700">
                        Fechar
                    </button>
                </div>
            </div>
        </div>
    </div>

    <script>
    // Vue.js integration for Risk Prediction Dashboard
    const riskPrediction = {
        data() {
            return {
                riskDashboard: null,
                selectedStoryAnalysis: null,
                showStoryRiskModal: false,
                loadingRisks: false
            };
        },
        methods: {
            async loadRiskDashboard(projectId = null) {
                this.loadingRisks = true;
                try {
                    const url = projectId
                        ? `/api/ai/risks/project/${projectId}`
                        : '/api/ai/risks/dashboard';
                    const response = await fetch(url);
                    this.riskDashboard = await response.json();
                } catch (error) {
                    console.error('Error loading risk dashboard:', error);
                } finally {
                    this.loadingRisks = false;
                }
            },
            async viewStoryRisks(storyId) {
                try {
                    const response = await fetch(`/api/ai/risks/story/${storyId}`);
                    this.selectedStoryAnalysis = await response.json();
                    this.showStoryRiskModal = true;
                } catch (error) {
                    console.error('Error loading story risks:', error);
                }
            },
            refreshRisks() {
                this.loadRiskDashboard();
            },
            getScoreColor(score) {
                if (score >= 60) return 'text-red-600';
                if (score >= 30) return 'text-yellow-600';
                return 'text-green-600';
            },
            getProgressBarColor(score) {
                if (score >= 60) return 'bg-red-500';
                if (score >= 30) return 'bg-yellow-500';
                return 'bg-green-500';
            },
            getAlertBadgeClass(level) {
                const classes = {
                    'red': 'bg-red-100 text-red-800',
                    'yellow': 'bg-yellow-100 text-yellow-800',
                    'green': 'bg-green-100 text-green-800'
                };
                return classes[level] || 'bg-gray-100 text-gray-800';
            },
            getAlertLabel(level) {
                const labels = {
                    'red': 'Alto Risco',
                    'yellow': 'Atencao',
                    'green': 'Baixo Risco'
                };
                return labels[level] || 'Desconhecido';
            },
            getSeverityBadgeClass(severity) {
                const classes = {
                    'critical': 'bg-red-600 text-white',
                    'high': 'bg-orange-100 text-orange-800',
                    'medium': 'bg-yellow-100 text-yellow-800',
                    'low': 'bg-green-100 text-green-800'
                };
                return classes[severity] || 'bg-gray-100 text-gray-800';
            },
            getCategoryLabel(category) {
                const labels = {
                    'technical': 'Tecnico',
                    'dependency': 'Dependencia',
                    'resource': 'Recurso',
                    'deadline': 'Prazo'
                };
                return labels[category] || category;
            },
            getCategoryColor(category) {
                const colors = {
                    'technical': 'bg-red-500',
                    'dependency': 'bg-amber-500',
                    'resource': 'bg-violet-500',
                    'deadline': 'bg-blue-500'
                };
                return colors[category] || 'bg-gray-500';
            },
            getCategoryBgColor(category) {
                const colors = {
                    'technical': 'bg-red-400',
                    'dependency': 'bg-amber-400',
                    'resource': 'bg-violet-400',
                    'deadline': 'bg-blue-400'
                };
                return colors[category] || 'bg-gray-400';
            },
            getStatusClass(status) {
                const classes = {
                    'backlog': 'bg-gray-100 text-gray-800',
                    'ready': 'bg-blue-100 text-blue-800',
                    'in_progress': 'bg-yellow-100 text-yellow-800',
                    'review': 'bg-purple-100 text-purple-800',
                    'testing': 'bg-indigo-100 text-indigo-800',
                    'done': 'bg-green-100 text-green-800'
                };
                return classes[status] || 'bg-gray-100 text-gray-800';
            },
            formatDate(dateStr) {
                if (!dateStr) return '';
                const date = new Date(dateStr);
                return date.toLocaleDateString('pt-BR', { day: '2-digit', month: '2-digit' });
            }
        },
        mounted() {
            this.loadRiskDashboard();
        }
    };
    </script>
    """


def register_risk_prediction_routes(app):
    """Registra as rotas do modulo de previsao de riscos."""
    app.include_router(router)
    print("[AI] Risk Prediction registrado: /api/ai/risks/*")

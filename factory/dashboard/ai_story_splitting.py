# -*- coding: utf-8 -*-
"""
AI Story Splitting Module (Issue #248)
======================================
Sistema de IA para sugestao de divisao de stories grandes.

Funcionalidades:
- Analise de stories para detectar se sao muito grandes (>13 pontos ou muitos criterios)
- Sugestao de divisao em stories menores aplicando principios INVEST
- Templates de divisao por tipo (CRUD, workflow, integration)
- Geracao automatica de stories filhas
"""

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from datetime import datetime
from enum import Enum
import uuid
import re

router = APIRouter(prefix="/api/ai/splitting", tags=["AI Story Splitting"])


# ==================== ENUMS ====================

class SplitPatternType(str, Enum):
    """Tipos de padroes de divisao."""
    CRUD = "crud"               # Create, Read, Update, Delete
    WORKFLOW = "workflow"       # Etapas de um fluxo
    INTEGRATION = "integration" # Integracao com sistemas externos
    USER_ROLES = "user_roles"   # Por diferentes usuarios/perfis
    DATA_TYPES = "data_types"   # Por tipos de dados
    PLATFORMS = "platforms"     # Por plataformas (web, mobile, API)
    COMPLEXITY = "complexity"   # Por niveis de complexidade


class ComplexityIndicator(str, Enum):
    """Indicadores de complexidade."""
    MANY_ACCEPTANCE_CRITERIA = "many_acceptance_criteria"
    HIGH_STORY_POINTS = "high_story_points"
    MULTIPLE_USER_TYPES = "multiple_user_types"
    MULTIPLE_INTEGRATIONS = "multiple_integrations"
    CRUD_OPERATIONS = "crud_operations"
    WORKFLOW_STEPS = "workflow_steps"
    MULTIPLE_PLATFORMS = "multiple_platforms"


# ==================== MODELS ====================

class StoryAnalysisInput(BaseModel):
    """Input para analise de story."""
    story_id: Optional[str] = None
    title: str
    description: Optional[str] = None
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: Optional[List[str]] = []
    story_points: Optional[int] = 0
    complexity: Optional[str] = "medium"
    technical_notes: Optional[str] = None
    business_rules: Optional[List[str]] = []


class ComplexityAnalysis(BaseModel):
    """Resultado da analise de complexidade."""
    is_too_large: bool
    total_score: int = Field(ge=0, le=100, description="Score de complexidade 0-100")
    indicators: List[Dict[str, Any]]
    suggested_points: int
    recommendation: str


class SuggestedChildStory(BaseModel):
    """Story filha sugerida."""
    temp_id: str
    title: str
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    acceptance_criteria: List[str] = []
    suggested_points: int
    complexity: str
    category: str
    order: int


class SplitSuggestion(BaseModel):
    """Sugestao de divisao completa."""
    pattern_type: str
    pattern_name: str
    pattern_description: str
    child_stories: List[SuggestedChildStory]
    total_points: int
    invest_compliance: Dict[str, bool]
    rationale: str


class StoryAnalysisResult(BaseModel):
    """Resultado completo da analise."""
    original_story: Dict[str, Any]
    complexity_analysis: ComplexityAnalysis
    should_split: bool
    split_suggestions: List[SplitSuggestion]
    alternative_suggestions: List[str]
    analyzed_at: str


class SplitExecutionInput(BaseModel):
    """Input para executar a divisao."""
    story_id: str
    project_id: str
    pattern_type: str
    child_stories: List[Dict[str, Any]]
    archive_original: bool = False


class SplitExecutionResult(BaseModel):
    """Resultado da execucao da divisao."""
    original_story_id: str
    created_stories: List[Dict[str, Any]]
    success: bool
    message: str


class SplitPattern(BaseModel):
    """Padrao de divisao disponivel."""
    type: str
    name: str
    description: str
    applicable_when: List[str]
    example_splits: List[Dict[str, str]]


# ==================== INVEST PRINCIPLES ====================

INVEST_PRINCIPLES = {
    "independent": {
        "name": "Independent",
        "description": "A story pode ser desenvolvida independentemente de outras",
        "check": lambda s: len(s.get('acceptance_criteria', [])) <= 5 and s.get('suggested_points', 0) <= 8
    },
    "negotiable": {
        "name": "Negotiable",
        "description": "O escopo pode ser negociado sem impactar a entrega",
        "check": lambda s: s.get('action') is not None
    },
    "valuable": {
        "name": "Valuable",
        "description": "Entrega valor claro para o usuario/negocio",
        "check": lambda s: s.get('benefit') is not None and len(s.get('benefit', '')) > 10
    },
    "estimable": {
        "name": "Estimable",
        "description": "Pode ser estimada com confianca",
        "check": lambda s: s.get('suggested_points', 0) in [1, 2, 3, 5, 8]
    },
    "small": {
        "name": "Small",
        "description": "Pequena o suficiente para caber em uma sprint",
        "check": lambda s: s.get('suggested_points', 0) <= 8
    },
    "testable": {
        "name": "Testable",
        "description": "Criterios de aceite claros e testaveis",
        "check": lambda s: len(s.get('acceptance_criteria', [])) >= 2
    }
}


# ==================== SPLIT PATTERNS ====================

SPLIT_PATTERNS = {
    SplitPatternType.CRUD: {
        "name": "CRUD Operations",
        "description": "Divide em operacoes Create, Read, Update, Delete",
        "applicable_when": [
            "Gerenciamento de entidades",
            "Cadastro de dados",
            "CRUD basico",
            "Operacoes de banco de dados"
        ],
        "templates": [
            {
                "suffix": "- Criar",
                "action_template": "criar novo(a) {entity}",
                "criteria_templates": [
                    "Usuario pode preencher formulario de criacao",
                    "Sistema valida campos obrigatorios",
                    "Sistema persiste dados e confirma sucesso"
                ],
                "points": 3
            },
            {
                "suffix": "- Listar/Visualizar",
                "action_template": "visualizar lista de {entity}",
                "criteria_templates": [
                    "Sistema exibe lista paginada",
                    "Usuario pode buscar e filtrar",
                    "Usuario pode ver detalhes de cada item"
                ],
                "points": 3
            },
            {
                "suffix": "- Editar",
                "action_template": "editar {entity} existente",
                "criteria_templates": [
                    "Usuario pode editar campos permitidos",
                    "Sistema valida alteracoes",
                    "Sistema salva e confirma atualizacao"
                ],
                "points": 2
            },
            {
                "suffix": "- Excluir",
                "action_template": "excluir {entity}",
                "criteria_templates": [
                    "Usuario recebe confirmacao antes de excluir",
                    "Sistema realiza soft delete",
                    "Sistema confirma exclusao"
                ],
                "points": 2
            }
        ]
    },
    SplitPatternType.WORKFLOW: {
        "name": "Workflow Steps",
        "description": "Divide em etapas de um fluxo/processo",
        "applicable_when": [
            "Processo de aprovacao",
            "Fluxo de trabalho",
            "Wizard ou steps",
            "Pipeline de processamento"
        ],
        "templates": [
            {
                "suffix": "- Iniciacao",
                "action_template": "iniciar {process}",
                "criteria_templates": [
                    "Usuario pode iniciar o processo",
                    "Sistema valida pre-requisitos",
                    "Sistema cria registro inicial"
                ],
                "points": 3
            },
            {
                "suffix": "- Processamento",
                "action_template": "processar etapas de {process}",
                "criteria_templates": [
                    "Sistema executa regras de negocio",
                    "Sistema atualiza status em cada etapa",
                    "Usuario acompanha progresso"
                ],
                "points": 5
            },
            {
                "suffix": "- Validacao/Revisao",
                "action_template": "revisar e validar {process}",
                "criteria_templates": [
                    "Revisor pode aprovar ou rejeitar",
                    "Sistema registra decisao e justificativa",
                    "Sistema notifica interessados"
                ],
                "points": 3
            },
            {
                "suffix": "- Finalizacao",
                "action_template": "finalizar {process}",
                "criteria_templates": [
                    "Sistema gera documentos finais",
                    "Sistema atualiza status para concluido",
                    "Sistema notifica conclusao"
                ],
                "points": 2
            }
        ]
    },
    SplitPatternType.INTEGRATION: {
        "name": "System Integration",
        "description": "Divide por sistemas/APIs a integrar",
        "applicable_when": [
            "Integracao com API externa",
            "Sincronizacao de dados",
            "Comunicacao entre sistemas",
            "Importacao/Exportacao"
        ],
        "templates": [
            {
                "suffix": "- Configuracao da Integracao",
                "action_template": "configurar conexao com {system}",
                "criteria_templates": [
                    "Admin pode configurar credenciais",
                    "Sistema valida conexao",
                    "Sistema armazena configuracao segura"
                ],
                "points": 3
            },
            {
                "suffix": "- Sincronizacao de Dados",
                "action_template": "sincronizar dados com {system}",
                "criteria_templates": [
                    "Sistema envia/recebe dados no formato esperado",
                    "Sistema trata erros de comunicacao",
                    "Sistema registra logs de sincronizacao"
                ],
                "points": 5
            },
            {
                "suffix": "- Tratamento de Erros",
                "action_template": "tratar falhas de integracao com {system}",
                "criteria_templates": [
                    "Sistema implementa retry automatico",
                    "Sistema notifica admin em caso de falha persistente",
                    "Sistema mantem fila de operacoes pendentes"
                ],
                "points": 3
            },
            {
                "suffix": "- Monitoramento",
                "action_template": "monitorar integracao com {system}",
                "criteria_templates": [
                    "Sistema exibe status da integracao",
                    "Sistema gera alertas de problemas",
                    "Sistema exibe metricas de uso"
                ],
                "points": 2
            }
        ]
    },
    SplitPatternType.USER_ROLES: {
        "name": "User Roles",
        "description": "Divide por diferentes perfis de usuario",
        "applicable_when": [
            "Multiplos tipos de usuario",
            "Diferentes niveis de permissao",
            "Funcionalidades por perfil",
            "Admin vs Usuario comum"
        ],
        "templates": [
            {
                "suffix": "- Visao do Usuario",
                "action_template": "{action} (visao usuario)",
                "criteria_templates": [
                    "Usuario comum acessa funcionalidade basica",
                    "Sistema respeita permissoes de usuario",
                    "Interface adequada ao perfil"
                ],
                "points": 3
            },
            {
                "suffix": "- Visao do Gestor",
                "action_template": "{action} (visao gestor)",
                "criteria_templates": [
                    "Gestor acessa visao consolidada",
                    "Gestor pode aprovar/rejeitar",
                    "Gestor visualiza metricas da equipe"
                ],
                "points": 3
            },
            {
                "suffix": "- Visao do Admin",
                "action_template": "{action} (visao admin)",
                "criteria_templates": [
                    "Admin configura parametros do sistema",
                    "Admin gerencia permissoes",
                    "Admin acessa logs e auditoria"
                ],
                "points": 3
            }
        ]
    },
    SplitPatternType.DATA_TYPES: {
        "name": "Data Types",
        "description": "Divide por tipos diferentes de dados",
        "applicable_when": [
            "Multiplos formatos de dados",
            "Diferentes tipos de arquivos",
            "Variedade de entidades"
        ],
        "templates": [
            {
                "suffix": "- Dados Basicos",
                "action_template": "processar dados basicos de {entity}",
                "criteria_templates": [
                    "Sistema processa campos obrigatorios",
                    "Sistema valida formato basico",
                    "Sistema persiste dados"
                ],
                "points": 3
            },
            {
                "suffix": "- Dados Complementares",
                "action_template": "processar dados complementares de {entity}",
                "criteria_templates": [
                    "Sistema processa campos opcionais",
                    "Sistema valida consistencia",
                    "Sistema atualiza registro"
                ],
                "points": 2
            },
            {
                "suffix": "- Anexos/Midias",
                "action_template": "gerenciar anexos de {entity}",
                "criteria_templates": [
                    "Usuario pode anexar arquivos",
                    "Sistema valida tipo e tamanho",
                    "Sistema armazena de forma segura"
                ],
                "points": 3
            }
        ]
    },
    SplitPatternType.PLATFORMS: {
        "name": "Platforms",
        "description": "Divide por plataformas (Web, Mobile, API)",
        "applicable_when": [
            "Multi-plataforma",
            "Responsividade",
            "API publica",
            "Apps nativos"
        ],
        "templates": [
            {
                "suffix": "- Interface Web",
                "action_template": "{action} via interface web",
                "criteria_templates": [
                    "Interface funciona em navegadores modernos",
                    "Layout responsivo para desktop/tablet",
                    "Acessibilidade WCAG 2.1 AA"
                ],
                "points": 5
            },
            {
                "suffix": "- Interface Mobile",
                "action_template": "{action} via app mobile",
                "criteria_templates": [
                    "Interface nativa iOS/Android",
                    "Funciona offline quando possivel",
                    "Usa recursos do dispositivo"
                ],
                "points": 5
            },
            {
                "suffix": "- API REST",
                "action_template": "{action} via API",
                "criteria_templates": [
                    "Endpoint RESTful documentado",
                    "Autenticacao via JWT/OAuth",
                    "Rate limiting implementado"
                ],
                "points": 3
            }
        ]
    },
    SplitPatternType.COMPLEXITY: {
        "name": "Complexity Layers",
        "description": "Divide em MVP, Melhorias e Avancado",
        "applicable_when": [
            "Funcionalidade complexa",
            "Entrega incremental",
            "Prototipacao",
            "Validacao de hipoteses"
        ],
        "templates": [
            {
                "suffix": "- MVP/Basico",
                "action_template": "{action} (versao basica)",
                "criteria_templates": [
                    "Funcionalidade minima viavel implementada",
                    "Fluxo principal funciona",
                    "Interface simples e funcional"
                ],
                "points": 5
            },
            {
                "suffix": "- Melhorias UX",
                "action_template": "{action} (melhorias de UX)",
                "criteria_templates": [
                    "Interface refinada conforme feedback",
                    "Validacoes em tempo real",
                    "Mensagens de erro amigaveis"
                ],
                "points": 3
            },
            {
                "suffix": "- Avancado/Otimizacao",
                "action_template": "{action} (funcoes avancadas)",
                "criteria_templates": [
                    "Funcionalidades avancadas implementadas",
                    "Performance otimizada",
                    "Metricas e analytics"
                ],
                "points": 5
            }
        ]
    }
}


# ==================== ANALYSIS ENGINE ====================

class StoryComplexityAnalyzer:
    """Analisador de complexidade de stories."""

    # Limiares para deteccao
    MAX_STORY_POINTS = 13
    MAX_ACCEPTANCE_CRITERIA = 8
    MAX_BUSINESS_RULES = 5

    def __init__(self):
        self.indicators = []

    def analyze(self, story: StoryAnalysisInput) -> ComplexityAnalysis:
        """Analisa a complexidade de uma story."""
        self.indicators = []
        total_score = 0

        # 1. Verificar story points
        if story.story_points and story.story_points > self.MAX_STORY_POINTS:
            score = min(30, (story.story_points - self.MAX_STORY_POINTS) * 5)
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.HIGH_STORY_POINTS.value,
                "message": f"Story points ({story.story_points}) acima do recomendado ({self.MAX_STORY_POINTS})",
                "score_impact": score,
                "severity": "high"
            })

        # 2. Verificar quantidade de criterios de aceite
        criteria_count = len(story.acceptance_criteria or [])
        if criteria_count > self.MAX_ACCEPTANCE_CRITERIA:
            score = min(25, (criteria_count - self.MAX_ACCEPTANCE_CRITERIA) * 5)
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.MANY_ACCEPTANCE_CRITERIA.value,
                "message": f"Muitos criterios de aceite ({criteria_count}). Recomendado: ate {self.MAX_ACCEPTANCE_CRITERIA}",
                "score_impact": score,
                "severity": "high"
            })

        # 3. Detectar operacoes CRUD
        crud_keywords = ['criar', 'editar', 'excluir', 'listar', 'visualizar', 'atualizar',
                        'create', 'update', 'delete', 'read', 'list', 'view']
        text = f"{story.title} {story.description or ''} {story.action or ''}"
        crud_found = sum(1 for kw in crud_keywords if kw in text.lower())
        if crud_found >= 3:
            score = 15
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.CRUD_OPERATIONS.value,
                "message": f"Detectadas {crud_found} operacoes CRUD. Considere dividir por operacao.",
                "score_impact": score,
                "severity": "medium",
                "suggested_pattern": SplitPatternType.CRUD.value
            })

        # 4. Detectar multiplos usuarios/perfis
        user_keywords = ['admin', 'usuario', 'gestor', 'operador', 'cliente', 'visitante',
                        'administrador', 'manager', 'supervisor']
        users_found = [kw for kw in user_keywords if kw in text.lower()]
        if len(users_found) >= 2:
            score = 15
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.MULTIPLE_USER_TYPES.value,
                "message": f"Multiplos perfis detectados: {', '.join(users_found)}",
                "score_impact": score,
                "severity": "medium",
                "suggested_pattern": SplitPatternType.USER_ROLES.value
            })

        # 5. Detectar integracoes
        integration_keywords = ['api', 'integracao', 'integrar', 'sincronizar', 'importar',
                               'exportar', 'webhook', 'sap', 'erp', 'crm']
        integrations_found = [kw for kw in integration_keywords if kw in text.lower()]
        if len(integrations_found) >= 2:
            score = 15
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.MULTIPLE_INTEGRATIONS.value,
                "message": f"Multiplas integracoes detectadas: {', '.join(integrations_found)}",
                "score_impact": score,
                "severity": "medium",
                "suggested_pattern": SplitPatternType.INTEGRATION.value
            })

        # 6. Detectar workflow/steps
        workflow_keywords = ['aprovar', 'rejeitar', 'revisar', 'validar', 'etapa', 'step',
                            'fluxo', 'workflow', 'processo', 'pipeline']
        workflows_found = sum(1 for kw in workflow_keywords if kw in text.lower())
        if workflows_found >= 2:
            score = 10
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.WORKFLOW_STEPS.value,
                "message": "Workflow com multiplas etapas detectado",
                "score_impact": score,
                "severity": "medium",
                "suggested_pattern": SplitPatternType.WORKFLOW.value
            })

        # 7. Detectar multiplas plataformas
        platform_keywords = ['web', 'mobile', 'api', 'app', 'desktop', 'responsivo']
        platforms_found = [kw for kw in platform_keywords if kw in text.lower()]
        if len(platforms_found) >= 2:
            score = 10
            total_score += score
            self.indicators.append({
                "type": ComplexityIndicator.MULTIPLE_PLATFORMS.value,
                "message": f"Multiplas plataformas: {', '.join(platforms_found)}",
                "score_impact": score,
                "severity": "medium",
                "suggested_pattern": SplitPatternType.PLATFORMS.value
            })

        # 8. Verificar regras de negocio
        rules_count = len(story.business_rules or [])
        if rules_count > self.MAX_BUSINESS_RULES:
            score = 10
            total_score += score
            self.indicators.append({
                "type": "many_business_rules",
                "message": f"Muitas regras de negocio ({rules_count})",
                "score_impact": score,
                "severity": "medium"
            })

        # Calcular pontos sugeridos
        suggested_points = self._calculate_suggested_points(story, total_score)

        # Determinar se deve dividir
        is_too_large = total_score >= 30 or (story.story_points or 0) > self.MAX_STORY_POINTS

        # Gerar recomendacao
        recommendation = self._generate_recommendation(total_score, self.indicators)

        return ComplexityAnalysis(
            is_too_large=is_too_large,
            total_score=min(100, total_score),
            indicators=self.indicators,
            suggested_points=suggested_points,
            recommendation=recommendation
        )

    def _calculate_suggested_points(self, story: StoryAnalysisInput, complexity_score: int) -> int:
        """Calcula pontos sugeridos baseado na complexidade."""
        base_points = story.story_points or 5

        if complexity_score >= 50:
            return min(21, base_points)
        elif complexity_score >= 30:
            return min(13, base_points)
        elif complexity_score >= 15:
            return min(8, base_points)
        else:
            return min(5, base_points)

    def _generate_recommendation(self, score: int, indicators: List[Dict]) -> str:
        """Gera recomendacao textual."""
        if score >= 50:
            return "Story muito complexa. Divisao altamente recomendada para melhor gerenciamento."
        elif score >= 30:
            return "Story complexa. Considere dividir para facilitar estimativa e desenvolvimento."
        elif score >= 15:
            return "Story moderadamente complexa. Divisao pode melhorar clareza e entrega."
        else:
            return "Story com complexidade adequada. Divisao e opcional."


class StorySplitter:
    """Gerador de sugestoes de divisao."""

    def __init__(self):
        self.analyzer = StoryComplexityAnalyzer()

    def suggest_splits(self, story: StoryAnalysisInput, analysis: ComplexityAnalysis) -> List[SplitSuggestion]:
        """Gera sugestoes de divisao baseadas na analise."""
        suggestions = []

        # Identificar padroes aplicaveis
        applicable_patterns = self._identify_applicable_patterns(story, analysis)

        for pattern_type in applicable_patterns:
            pattern = SPLIT_PATTERNS[pattern_type]
            child_stories = self._generate_child_stories(story, pattern, pattern_type)

            # Verificar INVEST compliance
            invest_compliance = self._check_invest_compliance(child_stories)

            # Calcular total de pontos
            total_points = sum(cs.suggested_points for cs in child_stories)

            suggestions.append(SplitSuggestion(
                pattern_type=pattern_type.value,
                pattern_name=pattern["name"],
                pattern_description=pattern["description"],
                child_stories=child_stories,
                total_points=total_points,
                invest_compliance=invest_compliance,
                rationale=self._generate_rationale(pattern_type, story, child_stories)
            ))

        return suggestions

    def _identify_applicable_patterns(self, story: StoryAnalysisInput,
                                      analysis: ComplexityAnalysis) -> List[SplitPatternType]:
        """Identifica padroes aplicaveis baseados na analise."""
        patterns = []

        # Padroes sugeridos pelos indicadores
        for indicator in analysis.indicators:
            if "suggested_pattern" in indicator:
                pattern = SplitPatternType(indicator["suggested_pattern"])
                if pattern not in patterns:
                    patterns.append(pattern)

        # Se nenhum padrao especifico, usar COMPLEXITY (MVP approach)
        if not patterns:
            patterns.append(SplitPatternType.COMPLEXITY)

        # Sempre adicionar CRUD se for gerenciamento de entidade
        text = f"{story.title} {story.description or ''}"
        if any(kw in text.lower() for kw in ['cadastro', 'gerenciar', 'gestao', 'manage', 'crud']):
            if SplitPatternType.CRUD not in patterns:
                patterns.append(SplitPatternType.CRUD)

        return patterns[:3]  # Maximo 3 sugestoes

    def _generate_child_stories(self, story: StoryAnalysisInput, pattern: Dict,
                                pattern_type: SplitPatternType) -> List[SuggestedChildStory]:
        """Gera stories filhas baseadas no padrao."""
        children = []

        # Extrair entidade/processo do titulo
        entity = self._extract_entity(story)

        for i, template in enumerate(pattern["templates"]):
            # Gerar titulo
            base_title = story.title.rstrip('.')
            title = f"{base_title} {template['suffix']}"

            # Gerar action
            action = template["action_template"].format(
                entity=entity,
                process=entity,
                action=story.action or "realizar a operacao",
                system=entity
            )

            # Gerar criterios
            criteria = [c.format(entity=entity) for c in template["criteria_templates"]]

            # Determinar complexidade
            points = template["points"]
            complexity = "low" if points <= 3 else "medium" if points <= 5 else "high"

            child = SuggestedChildStory(
                temp_id=f"SPLIT-{uuid.uuid4().hex[:8].upper()}",
                title=title,
                persona=story.persona,
                action=action,
                benefit=story.benefit,
                acceptance_criteria=criteria,
                suggested_points=points,
                complexity=complexity,
                category=pattern_type.value,
                order=i + 1
            )
            children.append(child)

        return children

    def _extract_entity(self, story: StoryAnalysisInput) -> str:
        """Extrai a entidade principal da story."""
        text = story.title

        # Remover prefixos comuns
        prefixes = ['implementar', 'criar', 'desenvolver', 'adicionar', 'implement', 'create', 'develop', 'add']
        for prefix in prefixes:
            text = re.sub(f'^{prefix}\\s+', '', text, flags=re.IGNORECASE)

        # Pegar primeiras 3 palavras significativas
        words = text.split()[:3]
        return ' '.join(words) if words else "item"

    def _check_invest_compliance(self, children: List[SuggestedChildStory]) -> Dict[str, bool]:
        """Verifica aderencia aos principios INVEST."""
        compliance = {}

        for principle, config in INVEST_PRINCIPLES.items():
            # Verificar se todas as stories filhas atendem
            all_comply = all(
                config["check"]({
                    "acceptance_criteria": cs.acceptance_criteria,
                    "suggested_points": cs.suggested_points,
                    "action": cs.action,
                    "benefit": cs.benefit
                })
                for cs in children
            )
            compliance[principle] = all_comply

        return compliance

    def _generate_rationale(self, pattern_type: SplitPatternType,
                           story: StoryAnalysisInput,
                           children: List[SuggestedChildStory]) -> str:
        """Gera justificativa para a divisao."""
        total_points = sum(cs.suggested_points for cs in children)
        original_points = story.story_points or 0

        rationales = {
            SplitPatternType.CRUD: f"Dividir por operacoes CRUD permite entrega incremental e testes isolados.",
            SplitPatternType.WORKFLOW: f"Separar etapas do workflow facilita acompanhamento e validacao de cada fase.",
            SplitPatternType.INTEGRATION: f"Isolar integracoes permite desenvolvimento paralelo e troubleshooting.",
            SplitPatternType.USER_ROLES: f"Dividir por perfil permite foco nas necessidades especificas de cada usuario.",
            SplitPatternType.DATA_TYPES: f"Separar por tipo de dado simplifica validacoes e testes.",
            SplitPatternType.PLATFORMS: f"Dividir por plataforma permite equipes especializadas trabalharem em paralelo.",
            SplitPatternType.COMPLEXITY: f"Abordagem MVP permite validar rapidamente e iterar com feedback."
        }

        base = rationales.get(pattern_type, "Divisao melhora gerenciabilidade.")
        return f"{base} Total: {len(children)} stories ({total_points} pontos vs {original_points} original)."


# ==================== API ENDPOINTS ====================

@router.post("/analyze", response_model=StoryAnalysisResult)
async def analyze_story(story: StoryAnalysisInput):
    """
    Analisa uma story e sugere divisao se for muito grande.
    """
    analyzer = StoryComplexityAnalyzer()
    splitter = StorySplitter()

    # Analisar complexidade
    complexity = analyzer.analyze(story)

    # Gerar sugestoes se necessario
    suggestions = []
    if complexity.is_too_large:
        suggestions = splitter.suggest_splits(story, complexity)

    # Sugestoes alternativas
    alternatives = []
    if complexity.total_score >= 15 and not complexity.is_too_large:
        alternatives = [
            "Considere adicionar mais detalhes nos criterios de aceite",
            "Revise se todos os criterios sao realmente necessarios na primeira versao",
            "Converse com o PO sobre prioridade dos requisitos"
        ]

    return StoryAnalysisResult(
        original_story={
            "story_id": story.story_id,
            "title": story.title,
            "story_points": story.story_points,
            "acceptance_criteria_count": len(story.acceptance_criteria or [])
        },
        complexity_analysis=complexity,
        should_split=complexity.is_too_large,
        split_suggestions=suggestions,
        alternative_suggestions=alternatives,
        analyzed_at=datetime.utcnow().isoformat()
    )


@router.post("/split", response_model=SplitExecutionResult)
async def execute_split(split_input: SplitExecutionInput):
    """
    Executa a divisao de uma story em stories menores.
    """
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Story
        import uuid as uuid_lib

        db = SessionLocal()

        try:
            # Buscar story original
            original = db.query(Story).filter(Story.story_id == split_input.story_id).first()

            if not original:
                raise HTTPException(status_code=404, detail="Story original nao encontrada")

            created_stories = []

            for child_data in split_input.child_stories:
                # Gerar novo story_id
                new_story_id = f"STR-{uuid_lib.uuid4().hex[:8].upper()}"

                new_story = Story(
                    story_id=new_story_id,
                    project_id=split_input.project_id,
                    tenant_id=original.tenant_id,
                    title=child_data.get("title"),
                    description=f"Dividida de {split_input.story_id}",
                    persona=child_data.get("persona") or original.persona,
                    action=child_data.get("action"),
                    benefit=child_data.get("benefit") or original.benefit,
                    acceptance_criteria=child_data.get("acceptance_criteria", []),
                    story_points=child_data.get("suggested_points", 3),
                    complexity=child_data.get("complexity", "medium"),
                    epic_id=original.epic_id,
                    sprint_id=original.sprint_id,
                    status="backlog",
                    priority=original.priority,
                    parent_story_id=split_input.story_id
                )

                db.add(new_story)
                created_stories.append({
                    "story_id": new_story_id,
                    "title": child_data.get("title"),
                    "story_points": child_data.get("suggested_points", 3)
                })

            # Atualizar story original se solicitado
            if split_input.archive_original:
                original.status = "archived"
                original.description = f"{original.description or ''}\n\n[Dividida em: {', '.join(s['story_id'] for s in created_stories)}]"

            db.commit()

            return SplitExecutionResult(
                original_story_id=split_input.story_id,
                created_stories=created_stories,
                success=True,
                message=f"Story dividida com sucesso em {len(created_stories)} stories menores."
            )

        except Exception as e:
            db.rollback()
            raise HTTPException(status_code=500, detail=str(e))
        finally:
            db.close()

    except ImportError:
        # Fallback se DB nao disponivel
        return SplitExecutionResult(
            original_story_id=split_input.story_id,
            created_stories=[
                {"story_id": f"STR-{i:04d}", "title": c.get("title"), "story_points": c.get("suggested_points", 3)}
                for i, c in enumerate(split_input.child_stories, 1)
            ],
            success=True,
            message="Divisao simulada (banco de dados nao disponivel)."
        )


@router.get("/patterns", response_model=List[SplitPattern])
async def get_split_patterns():
    """
    Retorna todos os padroes de divisao disponiveis.
    """
    patterns = []

    for pattern_type, config in SPLIT_PATTERNS.items():
        patterns.append(SplitPattern(
            type=pattern_type.value,
            name=config["name"],
            description=config["description"],
            applicable_when=config["applicable_when"],
            example_splits=[
                {"suffix": t["suffix"], "points": str(t["points"])}
                for t in config["templates"]
            ]
        ))

    return patterns


@router.get("/patterns/{pattern_type}")
async def get_pattern_details(pattern_type: str):
    """
    Retorna detalhes de um padrao especifico.
    """
    try:
        pt = SplitPatternType(pattern_type)
    except ValueError:
        raise HTTPException(status_code=404, detail=f"Padrao '{pattern_type}' nao encontrado")

    config = SPLIT_PATTERNS[pt]

    return {
        "type": pt.value,
        "name": config["name"],
        "description": config["description"],
        "applicable_when": config["applicable_when"],
        "templates": config["templates"],
        "invest_principles": {
            k: {"name": v["name"], "description": v["description"]}
            for k, v in INVEST_PRINCIPLES.items()
        }
    }


@router.get("/invest-principles")
async def get_invest_principles():
    """
    Retorna descricao dos principios INVEST.
    """
    return {
        "principles": [
            {
                "key": k,
                "name": v["name"],
                "description": v["description"]
            }
            for k, v in INVEST_PRINCIPLES.items()
        ],
        "recommendation": "Uma boa User Story deve atender a todos os 6 principios INVEST."
    }


# ==================== VUE.JS COMPONENT ====================

def get_story_splitting_component() -> str:
    """Retorna componente Vue.js para divisao de stories."""
    return """
    <!-- AI Story Splitting Component -->
    <div id="story-splitting-assistant">
        <!-- Trigger Button -->
        <button
            @click="openSplitAssistant"
            class="flex items-center gap-2 px-3 py-1.5 bg-gradient-to-r from-orange-500 to-amber-500 text-white rounded-lg hover:from-orange-600 hover:to-amber-600 text-sm shadow-md"
            title="Assistente de Divisao de Stories"
        >
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                      d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
            </svg>
            Dividir Story
        </button>

        <!-- Analysis/Split Modal -->
        <div v-if="showSplitModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center p-4">
            <div class="bg-white rounded-xl w-full max-w-5xl max-h-[90vh] overflow-hidden shadow-2xl">
                <!-- Header -->
                <div class="bg-gradient-to-r from-orange-500 to-amber-500 text-white p-4">
                    <div class="flex items-center justify-between">
                        <div class="flex items-center gap-3">
                            <div class="p-2 bg-white/20 rounded-lg">
                                <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                          d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                                </svg>
                            </div>
                            <div>
                                <h2 class="text-lg font-semibold">Assistente de Divisao de Stories</h2>
                                <p class="text-orange-100 text-sm">IA analisa e sugere divisao seguindo principios INVEST</p>
                            </div>
                        </div>
                        <button @click="closeSplitModal" class="p-1 hover:bg-white/20 rounded">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <!-- Content -->
                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 180px);">
                    <!-- Step 1: Input Story -->
                    <div v-if="currentStep === 1" class="space-y-4">
                        <div class="bg-orange-50 p-4 rounded-lg">
                            <h3 class="font-medium text-orange-900 mb-3">Informacoes da Story</h3>

                            <div class="space-y-3">
                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Titulo da Story *</label>
                                    <input v-model="splitInput.title" type="text"
                                           class="w-full px-3 py-2 border rounded-lg"
                                           placeholder="Ex: Implementar gerenciamento de usuarios">
                                </div>

                                <div class="grid grid-cols-2 gap-4">
                                    <div>
                                        <label class="block text-sm text-gray-600 mb-1">Story Points</label>
                                        <select v-model="splitInput.story_points" class="w-full px-3 py-2 border rounded-lg">
                                            <option :value="0">Nao estimado</option>
                                            <option :value="1">1</option>
                                            <option :value="2">2</option>
                                            <option :value="3">3</option>
                                            <option :value="5">5</option>
                                            <option :value="8">8</option>
                                            <option :value="13">13</option>
                                            <option :value="21">21</option>
                                        </select>
                                    </div>
                                    <div>
                                        <label class="block text-sm text-gray-600 mb-1">Complexidade</label>
                                        <select v-model="splitInput.complexity" class="w-full px-3 py-2 border rounded-lg">
                                            <option value="low">Baixa</option>
                                            <option value="medium">Media</option>
                                            <option value="high">Alta</option>
                                            <option value="very_high">Muito Alta</option>
                                        </select>
                                    </div>
                                </div>

                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Descricao</label>
                                    <textarea v-model="splitInput.description" rows="2"
                                              class="w-full px-3 py-2 border rounded-lg"
                                              placeholder="Descreva a funcionalidade..."></textarea>
                                </div>

                                <div class="grid grid-cols-3 gap-3">
                                    <div>
                                        <label class="block text-sm text-gray-600 mb-1">Persona</label>
                                        <input v-model="splitInput.persona" type="text"
                                               class="w-full px-3 py-2 border rounded-lg text-sm"
                                               placeholder="administrador">
                                    </div>
                                    <div>
                                        <label class="block text-sm text-gray-600 mb-1">Acao</label>
                                        <input v-model="splitInput.action" type="text"
                                               class="w-full px-3 py-2 border rounded-lg text-sm"
                                               placeholder="gerenciar usuarios">
                                    </div>
                                    <div>
                                        <label class="block text-sm text-gray-600 mb-1">Beneficio</label>
                                        <input v-model="splitInput.benefit" type="text"
                                               class="w-full px-3 py-2 border rounded-lg text-sm"
                                               placeholder="controlar acessos">
                                    </div>
                                </div>

                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Criterios de Aceite (um por linha)</label>
                                    <textarea v-model="criteriaText" rows="4"
                                              class="w-full px-3 py-2 border rounded-lg font-mono text-sm"
                                              placeholder="Usuario pode criar novo registro&#10;Sistema valida campos obrigatorios&#10;Sistema exibe confirmacao de sucesso"></textarea>
                                </div>
                            </div>
                        </div>

                        <button
                            @click="analyzeStory"
                            :disabled="!splitInput.title || isAnalyzing"
                            class="w-full py-3 bg-gradient-to-r from-orange-500 to-amber-500 text-white rounded-lg hover:from-orange-600 hover:to-amber-600 disabled:opacity-50 flex items-center justify-center gap-2"
                        >
                            <svg v-if="isAnalyzing" class="w-5 h-5 animate-spin" fill="none" viewBox="0 0 24 24">
                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                            </svg>
                            <span>{{ isAnalyzing ? 'Analisando...' : 'Analisar Story com IA' }}</span>
                        </button>
                    </div>

                    <!-- Step 2: Analysis Result -->
                    <div v-if="currentStep === 2 && analysisResult" class="space-y-4">
                        <!-- Complexity Score -->
                        <div class="flex items-center gap-6 p-4 bg-gray-50 rounded-lg">
                            <div class="text-center">
                                <div class="text-3xl font-bold" :class="getScoreColor(analysisResult.complexity_analysis.total_score)">
                                    {{ analysisResult.complexity_analysis.total_score }}
                                </div>
                                <div class="text-sm text-gray-500">Score</div>
                            </div>
                            <div class="flex-1">
                                <div class="h-3 bg-gray-200 rounded-full overflow-hidden">
                                    <div class="h-full transition-all duration-500"
                                         :class="getScoreBarColor(analysisResult.complexity_analysis.total_score)"
                                         :style="{width: analysisResult.complexity_analysis.total_score + '%'}"></div>
                                </div>
                                <p class="mt-2 text-sm text-gray-600">{{ analysisResult.complexity_analysis.recommendation }}</p>
                            </div>
                            <div class="text-center">
                                <div :class="analysisResult.should_split ? 'text-orange-600' : 'text-green-600'" class="font-semibold">
                                    {{ analysisResult.should_split ? 'Divisao Recomendada' : 'Tamanho Adequado' }}
                                </div>
                            </div>
                        </div>

                        <!-- Complexity Indicators -->
                        <div v-if="analysisResult.complexity_analysis.indicators.length > 0" class="space-y-2">
                            <h4 class="font-medium text-gray-700">Indicadores de Complexidade</h4>
                            <div class="grid grid-cols-2 gap-2">
                                <div v-for="indicator in analysisResult.complexity_analysis.indicators" :key="indicator.type"
                                     class="flex items-start gap-2 p-3 rounded-lg"
                                     :class="indicator.severity === 'high' ? 'bg-red-50 border border-red-200' : 'bg-yellow-50 border border-yellow-200'">
                                    <svg class="w-5 h-5 flex-shrink-0" :class="indicator.severity === 'high' ? 'text-red-500' : 'text-yellow-500'" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                                    </svg>
                                    <div class="flex-1">
                                        <div class="text-sm font-medium" :class="indicator.severity === 'high' ? 'text-red-800' : 'text-yellow-800'">
                                            {{ indicator.message }}
                                        </div>
                                        <div v-if="indicator.suggested_pattern" class="text-xs text-gray-500 mt-1">
                                            Padrao sugerido: {{ indicator.suggested_pattern }}
                                        </div>
                                    </div>
                                    <span class="text-xs px-2 py-0.5 rounded" :class="indicator.severity === 'high' ? 'bg-red-200 text-red-700' : 'bg-yellow-200 text-yellow-700'">
                                        +{{ indicator.score_impact }}
                                    </span>
                                </div>
                            </div>
                        </div>

                        <!-- Split Suggestions -->
                        <div v-if="analysisResult.split_suggestions.length > 0">
                            <h4 class="font-medium text-gray-700 mb-3">Sugestoes de Divisao</h4>

                            <!-- Tabs for each pattern -->
                            <div class="border-b mb-4">
                                <div class="flex gap-2">
                                    <button v-for="(suggestion, index) in analysisResult.split_suggestions"
                                            :key="suggestion.pattern_type"
                                            @click="selectedSuggestionIndex = index"
                                            class="px-4 py-2 text-sm font-medium border-b-2 transition-colors"
                                            :class="selectedSuggestionIndex === index ? 'border-orange-500 text-orange-600' : 'border-transparent text-gray-500 hover:text-gray-700'">
                                        {{ suggestion.pattern_name }}
                                    </button>
                                </div>
                            </div>

                            <!-- Selected Suggestion Details -->
                            <div v-if="selectedSuggestion" class="space-y-4">
                                <div class="p-3 bg-orange-50 rounded-lg">
                                    <p class="text-sm text-orange-800">{{ selectedSuggestion.rationale }}</p>
                                </div>

                                <!-- INVEST Compliance -->
                                <div class="flex items-center gap-3 flex-wrap">
                                    <span class="text-sm text-gray-600">INVEST:</span>
                                    <span v-for="(passed, principle) in selectedSuggestion.invest_compliance" :key="principle"
                                          class="px-2 py-1 rounded text-xs font-medium"
                                          :class="passed ? 'bg-green-100 text-green-700' : 'bg-red-100 text-red-700'">
                                        {{ principle.charAt(0).toUpperCase() }}
                                        <svg v-if="passed" class="w-3 h-3 inline ml-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
                                        </svg>
                                        <svg v-else class="w-3 h-3 inline ml-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                                        </svg>
                                    </span>
                                </div>

                                <!-- Child Stories -->
                                <div class="space-y-3">
                                    <div v-for="child in selectedSuggestion.child_stories" :key="child.temp_id"
                                         class="border rounded-lg overflow-hidden"
                                         :class="selectedChildIds.includes(child.temp_id) ? 'border-orange-400 bg-orange-50' : 'border-gray-200'">
                                        <div class="flex items-center gap-3 p-3">
                                            <input type="checkbox"
                                                   :value="child.temp_id"
                                                   v-model="selectedChildIds"
                                                   class="w-5 h-5 text-orange-500 rounded">
                                            <div class="flex-1">
                                                <div class="font-medium text-gray-800">{{ child.title }}</div>
                                                <div class="text-sm text-gray-500 mt-1">
                                                    <span class="inline-flex items-center gap-1">
                                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"/>
                                                        </svg>
                                                        {{ child.persona || splitInput.persona }}
                                                    </span>
                                                    <span class="mx-2">|</span>
                                                    {{ child.action }}
                                                </div>
                                            </div>
                                            <div class="text-center">
                                                <div class="text-xl font-bold text-orange-600">{{ child.suggested_points }}</div>
                                                <div class="text-xs text-gray-500">pontos</div>
                                            </div>
                                        </div>
                                        <div v-if="selectedChildIds.includes(child.temp_id)" class="border-t p-3 bg-white">
                                            <div class="text-sm text-gray-600 mb-2">Criterios de Aceite:</div>
                                            <ul class="text-sm space-y-1">
                                                <li v-for="(criterion, i) in child.acceptance_criteria" :key="i"
                                                    class="flex items-start gap-2">
                                                    <svg class="w-4 h-4 text-green-500 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
                                                    </svg>
                                                    {{ criterion }}
                                                </li>
                                            </ul>
                                        </div>
                                    </div>
                                </div>

                                <!-- Total Points -->
                                <div class="flex items-center justify-between p-3 bg-gray-100 rounded-lg">
                                    <span class="text-gray-600">Total de Stories Selecionadas:</span>
                                    <span class="font-bold text-lg">{{ selectedChildIds.length }} stories | {{ calculateSelectedPoints() }} pontos</span>
                                </div>
                            </div>
                        </div>

                        <!-- No Split Needed -->
                        <div v-else class="text-center py-8">
                            <svg class="w-16 h-16 mx-auto text-green-500 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                            </svg>
                            <h4 class="text-lg font-medium text-gray-800">Story com tamanho adequado!</h4>
                            <p class="text-gray-600 mt-2">Esta story atende aos principios INVEST e pode ser desenvolvida como esta.</p>
                        </div>
                    </div>

                    <!-- Step 3: Confirmation -->
                    <div v-if="currentStep === 3" class="space-y-4">
                        <div class="text-center py-4">
                            <svg class="w-16 h-16 mx-auto text-green-500 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                            </svg>
                            <h4 class="text-xl font-medium text-gray-800">Divisao Concluida!</h4>
                            <p class="text-gray-600 mt-2">{{ splitResult?.message }}</p>
                        </div>

                        <div v-if="splitResult?.created_stories" class="space-y-2">
                            <h5 class="font-medium text-gray-700">Stories Criadas:</h5>
                            <div v-for="story in splitResult.created_stories" :key="story.story_id"
                                 class="flex items-center justify-between p-3 bg-green-50 border border-green-200 rounded-lg">
                                <div>
                                    <span class="font-mono text-sm text-green-600">{{ story.story_id }}</span>
                                    <span class="ml-2 text-gray-800">{{ story.title }}</span>
                                </div>
                                <span class="text-sm text-green-600">{{ story.story_points }} pts</span>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Footer -->
                <div class="border-t p-4 flex justify-between items-center bg-gray-50">
                    <button v-if="currentStep > 1" @click="goBack"
                            class="px-4 py-2 text-gray-600 hover:text-gray-800 flex items-center gap-2">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7"/>
                        </svg>
                        Voltar
                    </button>
                    <div v-else></div>

                    <div class="flex gap-2">
                        <button @click="closeSplitModal" class="px-4 py-2 border rounded-lg hover:bg-gray-100">
                            {{ currentStep === 3 ? 'Fechar' : 'Cancelar' }}
                        </button>
                        <button v-if="currentStep === 2 && selectedChildIds.length > 0"
                                @click="executeSplit"
                                :disabled="isExecutingSplit"
                                class="px-4 py-2 bg-gradient-to-r from-orange-500 to-amber-500 text-white rounded-lg hover:from-orange-600 hover:to-amber-600 disabled:opacity-50 flex items-center gap-2">
                            <svg v-if="isExecutingSplit" class="w-4 h-4 animate-spin" fill="none" viewBox="0 0 24 24">
                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                            </svg>
                            Criar {{ selectedChildIds.length }} Stories
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script>
    // Vue.js integration for Story Splitting Assistant
    const storySplittingAssistant = {
        data() {
            return {
                showSplitModal: false,
                currentStep: 1,
                isAnalyzing: false,
                isExecutingSplit: false,
                splitInput: {
                    story_id: null,
                    title: '',
                    description: '',
                    persona: '',
                    action: '',
                    benefit: '',
                    story_points: 0,
                    complexity: 'medium',
                    acceptance_criteria: [],
                    technical_notes: '',
                    business_rules: []
                },
                criteriaText: '',
                analysisResult: null,
                selectedSuggestionIndex: 0,
                selectedChildIds: [],
                splitResult: null
            };
        },
        computed: {
            selectedSuggestion() {
                if (!this.analysisResult?.split_suggestions) return null;
                return this.analysisResult.split_suggestions[this.selectedSuggestionIndex];
            }
        },
        methods: {
            openSplitAssistant(story = null) {
                this.showSplitModal = true;
                this.currentStep = 1;
                this.analysisResult = null;
                this.selectedChildIds = [];
                this.splitResult = null;

                if (story) {
                    this.splitInput = {
                        story_id: story.story_id,
                        title: story.title,
                        description: story.description || '',
                        persona: story.persona || '',
                        action: story.action || '',
                        benefit: story.benefit || '',
                        story_points: story.story_points || 0,
                        complexity: story.complexity || 'medium',
                        acceptance_criteria: story.acceptance_criteria || [],
                        technical_notes: story.technical_notes || '',
                        business_rules: story.business_rules || []
                    };
                    this.criteriaText = (story.acceptance_criteria || []).join('\\n');
                } else {
                    this.splitInput = {
                        story_id: null,
                        title: '',
                        description: '',
                        persona: '',
                        action: '',
                        benefit: '',
                        story_points: 0,
                        complexity: 'medium',
                        acceptance_criteria: [],
                        technical_notes: '',
                        business_rules: []
                    };
                    this.criteriaText = '';
                }
            },
            closeSplitModal() {
                this.showSplitModal = false;
            },
            async analyzeStory() {
                this.isAnalyzing = true;
                this.splitInput.acceptance_criteria = this.criteriaText.split('\\n').filter(c => c.trim());

                try {
                    const response = await fetch('/api/ai/splitting/analyze', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(this.splitInput)
                    });
                    this.analysisResult = await response.json();
                    this.currentStep = 2;

                    // Pre-select all child stories from first suggestion
                    if (this.analysisResult.split_suggestions?.[0]?.child_stories) {
                        this.selectedChildIds = this.analysisResult.split_suggestions[0].child_stories.map(c => c.temp_id);
                    }
                } catch (error) {
                    console.error('Error analyzing story:', error);
                    alert('Erro ao analisar story. Tente novamente.');
                } finally {
                    this.isAnalyzing = false;
                }
            },
            async executeSplit() {
                if (!this.selectedSuggestion) return;

                this.isExecutingSplit = true;

                const selectedStories = this.selectedSuggestion.child_stories
                    .filter(c => this.selectedChildIds.includes(c.temp_id));

                try {
                    const response = await fetch('/api/ai/splitting/split', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({
                            story_id: this.splitInput.story_id || 'NEW-STORY',
                            project_id: this.currentProjectId || 'default',
                            pattern_type: this.selectedSuggestion.pattern_type,
                            child_stories: selectedStories,
                            archive_original: false
                        })
                    });
                    this.splitResult = await response.json();
                    this.currentStep = 3;

                    // Emit event for parent to refresh
                    this.$emit('stories-created', this.splitResult.created_stories);
                } catch (error) {
                    console.error('Error executing split:', error);
                    alert('Erro ao dividir story. Tente novamente.');
                } finally {
                    this.isExecutingSplit = false;
                }
            },
            goBack() {
                if (this.currentStep > 1) {
                    this.currentStep--;
                }
            },
            getScoreColor(score) {
                if (score >= 50) return 'text-red-600';
                if (score >= 30) return 'text-orange-600';
                if (score >= 15) return 'text-yellow-600';
                return 'text-green-600';
            },
            getScoreBarColor(score) {
                if (score >= 50) return 'bg-red-500';
                if (score >= 30) return 'bg-orange-500';
                if (score >= 15) return 'bg-yellow-500';
                return 'bg-green-500';
            },
            calculateSelectedPoints() {
                if (!this.selectedSuggestion) return 0;
                return this.selectedSuggestion.child_stories
                    .filter(c => this.selectedChildIds.includes(c.temp_id))
                    .reduce((sum, c) => sum + c.suggested_points, 0);
            }
        },
        watch: {
            selectedSuggestionIndex() {
                // Reset selection when changing pattern
                if (this.analysisResult?.split_suggestions?.[this.selectedSuggestionIndex]?.child_stories) {
                    this.selectedChildIds = this.analysisResult.split_suggestions[this.selectedSuggestionIndex]
                        .child_stories.map(c => c.temp_id);
                }
            }
        }
    };
    </script>
    """


def register_story_splitting_routes(app):
    """Registra as rotas do assistente de divisao de stories."""
    app.include_router(router)
    print("[AI] Story Splitting Assistant registrado: /api/ai/splitting/*")

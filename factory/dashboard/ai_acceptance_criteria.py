# -*- coding: utf-8 -*-
"""
AI Acceptance Criteria Assistant (Issue #250)
=============================================
IA que ajuda a escrever criterios de aceite completos e testaveis.

Funcionalidades:
- Sugestoes automaticas baseadas no titulo/descricao
- Validacao de completude
- Geracao de casos de teste
- Templates por tipo de story
"""

from fastapi import APIRouter, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict
from datetime import datetime

router = APIRouter(prefix="/api/ai/acceptance-criteria", tags=["AI Acceptance Criteria"])


# ==================== MODELS ====================

class StoryInput(BaseModel):
    title: str
    description: Optional[str] = None
    persona: Optional[str] = None
    action: Optional[str] = None
    benefit: Optional[str] = None
    story_type: Optional[str] = "feature"  # feature, bug, tech_debt, spike


class AcceptanceCriterion(BaseModel):
    id: str
    description: str
    category: str  # functional, non_functional, edge_case, validation
    testable: bool
    test_suggestion: Optional[str] = None


class CriteriaSuggestion(BaseModel):
    criteria: List[AcceptanceCriterion]
    test_cases: List[Dict]
    completeness_score: int  # 0-100
    missing_areas: List[str]
    recommendations: List[str]


class ValidationResult(BaseModel):
    is_valid: bool
    score: int
    issues: List[Dict]
    suggestions: List[str]


# ==================== TEMPLATES ====================

CRITERIA_TEMPLATES = {
    "feature": {
        "functional": [
            "O usuario pode {action} com sucesso",
            "O sistema exibe confirmacao apos {action}",
            "Os dados sao persistidos corretamente no banco",
            "O usuario recebe feedback visual durante o processamento"
        ],
        "non_functional": [
            "A operacao completa em menos de 3 segundos",
            "A interface e responsiva em dispositivos moveis",
            "O sistema mantem logs de auditoria da operacao"
        ],
        "edge_cases": [
            "O sistema trata entradas vazias adequadamente",
            "O sistema limita o tamanho maximo de entrada",
            "O sistema trata caracteres especiais corretamente"
        ],
        "validation": [
            "Campos obrigatorios sao validados antes do envio",
            "Mensagens de erro sao claras e orientam o usuario",
            "O formulario preserva dados apos erro de validacao"
        ]
    },
    "bug": {
        "functional": [
            "O comportamento incorreto nao ocorre mais",
            "O comportamento correto e consistente em todos os cenarios",
            "Nenhuma regressao e introduzida em funcionalidades relacionadas"
        ],
        "non_functional": [
            "A correcao nao impacta negativamente a performance",
            "Testes de regressao passam apos a correcao"
        ],
        "edge_cases": [
            "O bug nao ocorre em casos limites relacionados",
            "O bug nao ocorre com dados antigos/legados"
        ],
        "validation": [
            "A correcao e verificavel no ambiente de teste",
            "Logs de erro sao gerados corretamente"
        ]
    },
    "tech_debt": {
        "functional": [
            "O comportamento externo permanece inalterado",
            "Todos os testes existentes continuam passando",
            "A documentacao e atualizada conforme necessario"
        ],
        "non_functional": [
            "O codigo segue os padroes do projeto",
            "A cobertura de testes e mantida ou aumentada",
            "A performance e mantida ou melhorada"
        ],
        "edge_cases": [
            "Migracao de dados (se aplicavel) e reversivel",
            "Compatibilidade com versoes anteriores e mantida"
        ],
        "validation": [
            "Code review aprovado por pelo menos 2 revisores",
            "Metricas de qualidade de codigo sao mantidas"
        ]
    },
    "spike": {
        "functional": [
            "Documentacao tecnica da investigacao e criada",
            "Recomendacoes claras sao apresentadas",
            "Prova de conceito (se aplicavel) e funcional"
        ],
        "non_functional": [
            "Riscos e limitacoes sao documentados",
            "Estimativas de esforco sao atualizadas"
        ],
        "edge_cases": [
            "Alternativas investigadas sao documentadas",
            "Criterios de decisao sao claros"
        ],
        "validation": [
            "Stakeholders revisam e aprovam conclusoes",
            "Proximos passos sao definidos"
        ]
    }
}

TEST_CASE_TEMPLATES = {
    "happy_path": {
        "name": "Caminho feliz - {action}",
        "steps": [
            "Dado que o usuario esta autenticado",
            "Quando o usuario realiza {action}",
            "Entao o sistema {expected_result}"
        ]
    },
    "validation_error": {
        "name": "Erro de validacao - {field}",
        "steps": [
            "Dado que o usuario esta no formulario",
            "Quando o usuario deixa {field} vazio/invalido",
            "Entao o sistema exibe mensagem de erro"
        ]
    },
    "permission_denied": {
        "name": "Acesso negado - sem permissao",
        "steps": [
            "Dado que o usuario nao tem permissao para {action}",
            "Quando o usuario tenta {action}",
            "Entao o sistema exibe mensagem de acesso negado"
        ]
    },
    "concurrent_access": {
        "name": "Acesso concorrente",
        "steps": [
            "Dado que dois usuarios acessam o mesmo recurso",
            "Quando ambos tentam modificar simultaneamente",
            "Entao o sistema trata o conflito adequadamente"
        ]
    }
}


# ==================== AI LOGIC ====================

def extract_keywords(text: str) -> List[str]:
    """Extrai palavras-chave relevantes do texto."""
    if not text:
        return []

    stop_words = {'o', 'a', 'os', 'as', 'um', 'uma', 'de', 'da', 'do', 'em', 'na', 'no',
                  'para', 'por', 'com', 'como', 'que', 'e', 'ou', 'se', 'ao', 'aos',
                  'the', 'a', 'an', 'of', 'to', 'in', 'for', 'with', 'as', 'and', 'or'}

    words = text.lower().replace(',', ' ').replace('.', ' ').split()
    keywords = [w for w in words if len(w) > 2 and w not in stop_words]
    return list(set(keywords))[:10]


def detect_story_type(title: str, description: str = "") -> str:
    """Detecta o tipo de story baseado no conteudo."""
    text = f"{title} {description}".lower()

    bug_indicators = ['bug', 'erro', 'fix', 'corrigir', 'problema', 'falha', 'crash', 'nao funciona']
    tech_debt_indicators = ['refatorar', 'refactor', 'cleanup', 'tech debt', 'melhorar codigo', 'otimizar']
    spike_indicators = ['investigar', 'pesquisar', 'spike', 'poc', 'prova de conceito', 'avaliar']

    for indicator in bug_indicators:
        if indicator in text:
            return "bug"

    for indicator in tech_debt_indicators:
        if indicator in text:
            return "tech_debt"

    for indicator in spike_indicators:
        if indicator in text:
            return "spike"

    return "feature"


def generate_criteria(story: StoryInput) -> List[AcceptanceCriterion]:
    """Gera criterios de aceite baseados na story."""
    story_type = story.story_type or detect_story_type(story.title, story.description or "")
    templates = CRITERIA_TEMPLATES.get(story_type, CRITERIA_TEMPLATES["feature"])

    keywords = extract_keywords(f"{story.title} {story.description or ''} {story.action or ''}")
    action = story.action or story.title

    criteria = []
    criterion_id = 1

    for category, template_list in templates.items():
        for template in template_list:
            # Personaliza o template com keywords da story
            description = template.format(action=action)

            # Gera sugestao de teste
            test_suggestion = None
            if "usuario" in description.lower() or "sistema" in description.lower():
                test_suggestion = f"Teste automatizado: Verificar que {description.lower()}"

            criteria.append(AcceptanceCriterion(
                id=f"AC-{criterion_id:03d}",
                description=description,
                category=category,
                testable=True,
                test_suggestion=test_suggestion
            ))
            criterion_id += 1

    return criteria


def generate_test_cases(story: StoryInput, criteria: List[AcceptanceCriterion]) -> List[Dict]:
    """Gera casos de teste baseados na story e criterios."""
    action = story.action or story.title
    keywords = extract_keywords(story.title)

    test_cases = []

    # Happy path
    test_cases.append({
        "id": "TC-001",
        "name": f"Caminho feliz - {action[:50]}",
        "type": "happy_path",
        "priority": "high",
        "steps": [
            {"step": 1, "action": "Usuario acessa a funcionalidade", "expected": "Interface carrega corretamente"},
            {"step": 2, "action": f"Usuario realiza {action}", "expected": "Sistema processa a requisicao"},
            {"step": 3, "action": "Usuario confirma a acao", "expected": "Sistema exibe confirmacao de sucesso"}
        ],
        "related_criteria": ["AC-001", "AC-002"]
    })

    # Validation test
    test_cases.append({
        "id": "TC-002",
        "name": "Validacao de campos obrigatorios",
        "type": "validation",
        "priority": "high",
        "steps": [
            {"step": 1, "action": "Usuario acessa o formulario", "expected": "Formulario exibido"},
            {"step": 2, "action": "Usuario submete sem preencher campos", "expected": "Sistema exibe erros de validacao"},
            {"step": 3, "action": "Usuario corrige e submete", "expected": "Sistema aceita e processa"}
        ],
        "related_criteria": ["AC-009", "AC-010"]
    })

    # Edge case test
    test_cases.append({
        "id": "TC-003",
        "name": "Teste de limites e casos extremos",
        "type": "edge_case",
        "priority": "medium",
        "steps": [
            {"step": 1, "action": "Usuario insere dados no limite maximo", "expected": "Sistema aceita"},
            {"step": 2, "action": "Usuario insere dados acima do limite", "expected": "Sistema rejeita com mensagem clara"},
            {"step": 3, "action": "Usuario insere caracteres especiais", "expected": "Sistema trata adequadamente"}
        ],
        "related_criteria": ["AC-005", "AC-006", "AC-007"]
    })

    # Performance test
    test_cases.append({
        "id": "TC-004",
        "name": "Teste de performance",
        "type": "non_functional",
        "priority": "medium",
        "steps": [
            {"step": 1, "action": "Usuario realiza a operacao", "expected": "Resposta em menos de 3s"},
            {"step": 2, "action": "Multiplos usuarios simultaneos", "expected": "Sistema mantem performance"},
            {"step": 3, "action": "Verificar logs de auditoria", "expected": "Logs gerados corretamente"}
        ],
        "related_criteria": ["AC-004", "AC-008"]
    })

    return test_cases


def validate_criteria(criteria_list: List[str]) -> ValidationResult:
    """Valida uma lista de criterios de aceite."""
    issues = []
    suggestions = []
    score = 100

    # Verificacoes
    if len(criteria_list) < 3:
        issues.append({
            "type": "warning",
            "message": "Poucos criterios de aceite (menos de 3)",
            "impact": -15
        })
        score -= 15
        suggestions.append("Adicione mais criterios para cobrir diferentes cenarios")

    vague_words = ['adequadamente', 'corretamente', 'rapido', 'bom', 'melhor']
    for i, criterion in enumerate(criteria_list):
        criterion_lower = criterion.lower()

        # Criterio muito curto
        if len(criterion) < 20:
            issues.append({
                "type": "warning",
                "message": f"Criterio {i+1} muito curto",
                "impact": -5
            })
            score -= 5

        # Palavras vagas
        for vague in vague_words:
            if vague in criterion_lower:
                issues.append({
                    "type": "info",
                    "message": f"Criterio {i+1} usa termo vago: '{vague}'",
                    "impact": -3
                })
                score -= 3
                suggestions.append(f"Especifique o que significa '{vague}' no criterio {i+1}")

        # Sem verbo de acao
        action_verbs = ['deve', 'precisa', 'pode', 'exibe', 'permite', 'valida', 'processa']
        has_action = any(verb in criterion_lower for verb in action_verbs)
        if not has_action:
            issues.append({
                "type": "info",
                "message": f"Criterio {i+1} pode nao ser verificavel (sem verbo de acao)",
                "impact": -2
            })
            score -= 2

    # Verificar cobertura de categorias
    has_functional = any('usuario' in c.lower() or 'sistema' in c.lower() for c in criteria_list)
    has_validation = any('erro' in c.lower() or 'valida' in c.lower() for c in criteria_list)
    has_edge = any('limite' in c.lower() or 'maximo' in c.lower() or 'vazio' in c.lower() for c in criteria_list)

    if not has_functional:
        suggestions.append("Adicione criterios funcionais (o que o sistema deve fazer)")
    if not has_validation:
        suggestions.append("Adicione criterios de validacao (como tratar erros)")
    if not has_edge:
        suggestions.append("Adicione criterios para casos limite")

    score = max(0, min(100, score))

    return ValidationResult(
        is_valid=score >= 60,
        score=score,
        issues=issues,
        suggestions=suggestions
    )


# ==================== API ENDPOINTS ====================

@router.post("/suggest", response_model=CriteriaSuggestion)
async def suggest_criteria(story: StoryInput):
    """
    Sugere criterios de aceite baseados na story.
    """
    criteria = generate_criteria(story)
    test_cases = generate_test_cases(story, criteria)

    # Calcular score de completude
    categories_covered = len(set(c.category for c in criteria))
    completeness_score = min(100, categories_covered * 25)

    # Areas faltantes
    missing_areas = []
    categories = [c.category for c in criteria]
    if "edge_cases" not in categories:
        missing_areas.append("Casos limite (edge cases)")
    if "non_functional" not in categories:
        missing_areas.append("Requisitos nao-funcionais")

    # Recomendacoes
    recommendations = []
    if story.persona:
        recommendations.append(f"Criterios focados na perspectiva de: {story.persona}")
    if not story.description:
        recommendations.append("Adicione uma descricao para criterios mais especificos")

    return CriteriaSuggestion(
        criteria=criteria,
        test_cases=test_cases,
        completeness_score=completeness_score,
        missing_areas=missing_areas,
        recommendations=recommendations
    )


@router.post("/validate", response_model=ValidationResult)
async def validate_acceptance_criteria(criteria: List[str]):
    """
    Valida uma lista de criterios de aceite existentes.
    """
    if not criteria:
        raise HTTPException(status_code=400, detail="Lista de criterios vazia")

    return validate_criteria(criteria)


@router.post("/improve")
async def improve_criterion(criterion: str, context: Optional[str] = None):
    """
    Sugere melhorias para um criterio de aceite.
    """
    improvements = []

    # Analise do criterio
    criterion_lower = criterion.lower()

    # Verificar e sugerir melhorias
    if len(criterion) < 30:
        improvements.append({
            "type": "expand",
            "original": criterion,
            "improved": f"{criterion}. O sistema deve exibir feedback visual e registrar a operacao em log."
        })

    vague_replacements = {
        'adequadamente': 'conforme especificacao X',
        'corretamente': 'sem erros e com validacao completa',
        'rapido': 'em menos de 3 segundos',
        'bom': 'atendendo aos criterios de qualidade definidos'
    }

    improved = criterion
    for vague, specific in vague_replacements.items():
        if vague in criterion_lower:
            improved = criterion.replace(vague, specific)
            improvements.append({
                "type": "specificity",
                "original": criterion,
                "improved": improved,
                "reason": f"Substituido '{vague}' por termo mais especifico"
            })

    # Sugerir formato Given-When-Then
    if not any(word in criterion_lower for word in ['dado', 'quando', 'entao', 'given', 'when', 'then']):
        improvements.append({
            "type": "format",
            "original": criterion,
            "improved": f"Dado que o usuario esta na interface, quando {criterion.lower()}, entao o sistema confirma a operacao",
            "reason": "Formato Gherkin (Given-When-Then) facilita automacao de testes"
        })

    return {
        "original": criterion,
        "improvements": improvements,
        "score_improvement": len(improvements) * 10
    }


@router.get("/templates")
async def get_templates():
    """
    Retorna templates de criterios por tipo de story.
    """
    return {
        "story_types": list(CRITERIA_TEMPLATES.keys()),
        "templates": CRITERIA_TEMPLATES,
        "test_case_templates": TEST_CASE_TEMPLATES
    }


@router.get("/templates/{story_type}")
async def get_template_by_type(story_type: str):
    """
    Retorna templates para um tipo especifico de story.
    """
    if story_type not in CRITERIA_TEMPLATES:
        raise HTTPException(status_code=404, detail=f"Tipo '{story_type}' nao encontrado")

    return {
        "story_type": story_type,
        "categories": CRITERIA_TEMPLATES[story_type]
    }


# ==================== HTML COMPONENT ====================

def get_acceptance_criteria_component() -> str:
    """Retorna componente Vue.js para assistente de criterios."""
    return """
    <!-- AI Acceptance Criteria Assistant Component -->
    <div id="ai-criteria-assistant">
        <!-- Trigger Button -->
        <button
            @click="openCriteriaAssistant"
            class="flex items-center gap-2 px-3 py-1.5 bg-purple-100 text-purple-700 rounded-lg hover:bg-purple-200 text-sm"
            title="Assistente IA de Criterios"
        >
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                      d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"/>
            </svg>
            Sugerir Criterios
        </button>

        <!-- Modal -->
        <div v-if="showCriteriaModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center p-4">
            <div class="bg-white rounded-xl w-full max-w-4xl max-h-[90vh] overflow-hidden shadow-2xl">
                <!-- Header -->
                <div class="bg-gradient-to-r from-purple-600 to-indigo-600 text-white p-4">
                    <div class="flex items-center justify-between">
                        <div class="flex items-center gap-3">
                            <div class="p-2 bg-white/20 rounded-lg">
                                <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                          d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"/>
                                </svg>
                            </div>
                            <div>
                                <h2 class="text-lg font-semibold">Assistente de Criterios de Aceite</h2>
                                <p class="text-purple-200 text-sm">IA sugere criterios baseados na sua story</p>
                            </div>
                        </div>
                        <button @click="showCriteriaModal = false" class="p-1 hover:bg-white/20 rounded">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <!-- Content -->
                <div class="p-4 overflow-y-auto" style="max-height: calc(90vh - 180px);">
                    <!-- Story Input -->
                    <div v-if="!criteriaSuggestions" class="space-y-4">
                        <div class="bg-purple-50 p-4 rounded-lg">
                            <h3 class="font-medium text-purple-900 mb-2">Informacoes da Story</h3>
                            <div class="grid grid-cols-2 gap-4">
                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Titulo *</label>
                                    <input v-model="criteriaInput.title" type="text"
                                           class="w-full px-3 py-2 border rounded-lg"
                                           placeholder="Ex: Implementar login com Google">
                                </div>
                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Tipo</label>
                                    <select v-model="criteriaInput.story_type" class="w-full px-3 py-2 border rounded-lg">
                                        <option value="feature">Feature</option>
                                        <option value="bug">Bug Fix</option>
                                        <option value="tech_debt">Tech Debt</option>
                                        <option value="spike">Spike/Pesquisa</option>
                                    </select>
                                </div>
                            </div>
                            <div class="mt-3">
                                <label class="block text-sm text-gray-600 mb-1">Descricao</label>
                                <textarea v-model="criteriaInput.description" rows="2"
                                          class="w-full px-3 py-2 border rounded-lg"
                                          placeholder="Descreva a funcionalidade..."></textarea>
                            </div>
                            <div class="grid grid-cols-3 gap-3 mt-3">
                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Persona</label>
                                    <input v-model="criteriaInput.persona" type="text"
                                           class="w-full px-3 py-2 border rounded-lg text-sm"
                                           placeholder="usuario final">
                                </div>
                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Acao</label>
                                    <input v-model="criteriaInput.action" type="text"
                                           class="w-full px-3 py-2 border rounded-lg text-sm"
                                           placeholder="fazer login">
                                </div>
                                <div>
                                    <label class="block text-sm text-gray-600 mb-1">Beneficio</label>
                                    <input v-model="criteriaInput.benefit" type="text"
                                           class="w-full px-3 py-2 border rounded-lg text-sm"
                                           placeholder="acessar o sistema">
                                </div>
                            </div>
                        </div>

                        <button
                            @click="generateCriteriaSuggestions"
                            :disabled="!criteriaInput.title || isGeneratingCriteria"
                            class="w-full py-3 bg-purple-600 text-white rounded-lg hover:bg-purple-700 disabled:opacity-50 flex items-center justify-center gap-2"
                        >
                            <svg v-if="isGeneratingCriteria" class="w-5 h-5 animate-spin" fill="none" viewBox="0 0 24 24">
                                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                            </svg>
                            <span>{{ isGeneratingCriteria ? 'Gerando...' : 'Gerar Sugestoes com IA' }}</span>
                        </button>
                    </div>

                    <!-- Suggestions Result -->
                    <div v-else class="space-y-4">
                        <!-- Completeness Score -->
                        <div class="flex items-center justify-between p-4 bg-gray-50 rounded-lg">
                            <div>
                                <div class="text-sm text-gray-600">Completude dos Criterios</div>
                                <div class="text-2xl font-bold" :class="criteriaSuggestions.completeness_score >= 75 ? 'text-green-600' : criteriaSuggestions.completeness_score >= 50 ? 'text-yellow-600' : 'text-red-600'">
                                    {{ criteriaSuggestions.completeness_score }}%
                                </div>
                            </div>
                            <div class="w-32 h-32">
                                <svg viewBox="0 0 36 36" class="w-full h-full">
                                    <path d="M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831"
                                          fill="none" stroke="#eee" stroke-width="3"/>
                                    <path d="M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831"
                                          fill="none"
                                          :stroke="criteriaSuggestions.completeness_score >= 75 ? '#10B981' : criteriaSuggestions.completeness_score >= 50 ? '#F59E0B' : '#EF4444'"
                                          stroke-width="3"
                                          :stroke-dasharray="criteriaSuggestions.completeness_score + ', 100'"/>
                                </svg>
                            </div>
                        </div>

                        <!-- Criteria by Category -->
                        <div v-for="category in ['functional', 'non_functional', 'edge_cases', 'validation']" :key="category">
                            <div v-if="getCriteriaByCategory(category).length > 0" class="mb-4">
                                <h4 class="font-medium text-gray-700 mb-2 flex items-center gap-2">
                                    <span :class="getCategoryColor(category)" class="w-3 h-3 rounded-full"></span>
                                    {{ getCategoryLabel(category) }}
                                </h4>
                                <div class="space-y-2">
                                    <div v-for="criterion in getCriteriaByCategory(category)" :key="criterion.id"
                                         class="flex items-start gap-3 p-3 bg-white border rounded-lg hover:border-purple-300">
                                        <input type="checkbox" v-model="selectedCriteria" :value="criterion.id"
                                               class="mt-1 w-4 h-4 text-purple-600">
                                        <div class="flex-1">
                                            <div class="text-sm">{{ criterion.description }}</div>
                                            <div v-if="criterion.test_suggestion" class="text-xs text-gray-500 mt-1">
                                                💡 {{ criterion.test_suggestion }}
                                            </div>
                                        </div>
                                        <span class="text-xs px-2 py-0.5 bg-gray-100 rounded">{{ criterion.id }}</span>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Test Cases -->
                        <div class="mt-6">
                            <h4 class="font-medium text-gray-700 mb-2 flex items-center gap-2">
                                <svg class="w-5 h-5 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                          d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                </svg>
                                Casos de Teste Sugeridos
                            </h4>
                            <div class="space-y-2">
                                <div v-for="tc in criteriaSuggestions.test_cases" :key="tc.id"
                                     class="p-3 bg-green-50 border border-green-200 rounded-lg">
                                    <div class="flex items-center justify-between mb-2">
                                        <span class="font-medium text-green-800">{{ tc.name }}</span>
                                        <span class="text-xs px-2 py-0.5 bg-green-200 text-green-800 rounded">{{ tc.type }}</span>
                                    </div>
                                    <div class="text-sm text-green-700">
                                        <div v-for="step in tc.steps" :key="step.step" class="flex gap-2">
                                            <span class="text-green-500">{{ step.step }}.</span>
                                            <span>{{ step.action }} → {{ step.expected }}</span>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Missing Areas -->
                        <div v-if="criteriaSuggestions.missing_areas.length > 0" class="p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
                            <h4 class="font-medium text-yellow-800 mb-2">Areas para Considerar</h4>
                            <ul class="text-sm text-yellow-700 space-y-1">
                                <li v-for="area in criteriaSuggestions.missing_areas" :key="area">• {{ area }}</li>
                            </ul>
                        </div>
                    </div>
                </div>

                <!-- Footer -->
                <div class="border-t p-4 flex justify-between items-center bg-gray-50">
                    <button v-if="criteriaSuggestions" @click="criteriaSuggestions = null"
                            class="px-4 py-2 text-gray-600 hover:text-gray-800">
                        ← Voltar
                    </button>
                    <div class="flex gap-2">
                        <button @click="showCriteriaModal = false" class="px-4 py-2 border rounded-lg hover:bg-gray-100">
                            Cancelar
                        </button>
                        <button v-if="criteriaSuggestions && selectedCriteria.length > 0"
                                @click="applyCriteria"
                                class="px-4 py-2 bg-purple-600 text-white rounded-lg hover:bg-purple-700">
                            Aplicar {{ selectedCriteria.length }} Criterios
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script>
    // Vue.js integration for AI Criteria Assistant
    const aiCriteriaAssistant = {
        data() {
            return {
                showCriteriaModal: false,
                isGeneratingCriteria: false,
                criteriaInput: {
                    title: '',
                    description: '',
                    persona: '',
                    action: '',
                    benefit: '',
                    story_type: 'feature'
                },
                criteriaSuggestions: null,
                selectedCriteria: []
            };
        },
        methods: {
            openCriteriaAssistant() {
                this.showCriteriaModal = true;
                this.criteriaSuggestions = null;
                this.selectedCriteria = [];
            },
            async generateCriteriaSuggestions() {
                this.isGeneratingCriteria = true;
                try {
                    const response = await fetch('/api/ai/acceptance-criteria/suggest', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(this.criteriaInput)
                    });
                    this.criteriaSuggestions = await response.json();
                    // Pre-select all criteria
                    this.selectedCriteria = this.criteriaSuggestions.criteria.map(c => c.id);
                } catch (error) {
                    console.error('Error generating criteria:', error);
                } finally {
                    this.isGeneratingCriteria = false;
                }
            },
            getCriteriaByCategory(category) {
                if (!this.criteriaSuggestions) return [];
                return this.criteriaSuggestions.criteria.filter(c => c.category === category);
            },
            getCategoryLabel(category) {
                const labels = {
                    'functional': 'Funcionais',
                    'non_functional': 'Nao-Funcionais',
                    'edge_cases': 'Casos Limite',
                    'validation': 'Validacao'
                };
                return labels[category] || category;
            },
            getCategoryColor(category) {
                const colors = {
                    'functional': 'bg-blue-500',
                    'non_functional': 'bg-purple-500',
                    'edge_cases': 'bg-orange-500',
                    'validation': 'bg-green-500'
                };
                return colors[category] || 'bg-gray-500';
            },
            applyCriteria() {
                const selected = this.criteriaSuggestions.criteria
                    .filter(c => this.selectedCriteria.includes(c.id))
                    .map(c => c.description);

                // Emit event to parent component
                this.$emit('criteria-selected', selected);
                this.showCriteriaModal = false;
            }
        }
    };
    </script>
    """


def register_acceptance_criteria_routes(app):
    """Registra as rotas do assistente de criterios."""
    app.include_router(router)
    print("[AI] Acceptance Criteria Assistant registrado: /api/ai/acceptance-criteria/*")

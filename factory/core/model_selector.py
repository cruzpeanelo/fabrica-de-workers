# -*- coding: utf-8 -*-
"""
Model Selector - Selecao Inteligente de Modelos Claude v1.0
Plataforma E

Permite selecao automatica ou manual de modelos Claude baseado em:
- Complexidade da tarefa
- Tipo de operacao
- Custo/beneficio
- Fallback automatico em caso de erro

Modelos suportados:
- claude-opus-4-5-20251101: Tarefas complexas, raciocinio avancado
- claude-sonnet-4-20250514: Balanco entre qualidade e velocidade (default)
- claude-haiku-3-5-20241022: Tarefas rapidas e simples, economia
"""
import os
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional, Dict, Any, List, Callable
from datetime import datetime


# =============================================================================
# ENUMS E CONSTANTES
# =============================================================================

class ClaudeModel(str, Enum):
    """Modelos Claude disponiveis"""
    OPUS = "claude-opus-4-5-20251101"
    SONNET = "claude-sonnet-4-20250514"
    HAIKU = "claude-haiku-3-5-20241022"

    @classmethod
    def from_string(cls, value: str) -> "ClaudeModel":
        """Converte string para enum, com fallback para SONNET"""
        value_lower = value.lower() if value else ""

        # Mapeamento flexivel
        if "opus" in value_lower:
            return cls.OPUS
        elif "haiku" in value_lower:
            return cls.HAIKU
        elif "sonnet" in value_lower:
            return cls.SONNET

        # Tenta match exato
        for model in cls:
            if model.value == value:
                return model

        # Default
        return cls.SONNET

    @property
    def display_name(self) -> str:
        """Nome amigavel para exibicao"""
        names = {
            self.OPUS: "Claude Opus 4.5 (Avancado)",
            self.SONNET: "Claude Sonnet 4 (Padrao)",
            self.HAIKU: "Claude Haiku 3.5 (Rapido)"
        }
        return names.get(self, self.value)

    @property
    def short_name(self) -> str:
        """Nome curto para UI"""
        names = {
            self.OPUS: "Opus 4.5",
            self.SONNET: "Sonnet 4",
            self.HAIKU: "Haiku 3.5"
        }
        return names.get(self, self.value)


class TaskComplexity(str, Enum):
    """Niveis de complexidade de tarefa"""
    SIMPLE = "simple"       # Formatacao, conversao, tarefas triviais
    MEDIUM = "medium"       # Desenvolvimento padrao, correcoes
    COMPLEX = "complex"     # Arquitetura, refatoracao grande
    VERY_COMPLEX = "very_complex"  # Raciocinio avancado, decisoes criticas


class TaskType(str, Enum):
    """Tipos de tarefa para selecao de modelo"""
    # Tarefas simples (Haiku)
    FORMAT_CODE = "format_code"
    SIMPLE_FIX = "simple_fix"
    DOCUMENTATION = "documentation"
    TRANSLATION = "translation"
    SIMPLE_GENERATION = "simple_generation"

    # Tarefas medias (Sonnet - default)
    DEVELOPMENT = "development"
    CODE_REVIEW = "code_review"
    TEST_GENERATION = "test_generation"
    REFACTORING = "refactoring"
    BUG_FIX = "bug_fix"
    API_DESIGN = "api_design"

    # Tarefas complexas (Opus)
    ARCHITECTURE = "architecture"
    SECURITY_AUDIT = "security_audit"
    COMPLEX_REFACTORING = "complex_refactoring"
    SYSTEM_DESIGN = "system_design"
    CRITICAL_FIX = "critical_fix"
    MULTI_STEP_REASONING = "multi_step_reasoning"


# =============================================================================
# CONFIGURACAO DE MODELOS
# =============================================================================

@dataclass
class ModelConfig:
    """Configuracao de um modelo Claude"""
    model_id: str
    display_name: str
    short_name: str
    max_tokens: int
    context_limit: int
    cost_per_1k_input: float   # USD por 1K tokens de entrada
    cost_per_1k_output: float  # USD por 1K tokens de saida
    speed_rating: int          # 1-5 (5 = mais rapido)
    quality_rating: int        # 1-5 (5 = melhor qualidade)
    recommended_for: List[TaskType] = field(default_factory=list)

    @property
    def cost_rating(self) -> int:
        """Rating de custo (1-5, 5 = mais economico)"""
        avg_cost = (self.cost_per_1k_input + self.cost_per_1k_output) / 2
        if avg_cost < 0.005:
            return 5
        elif avg_cost < 0.01:
            return 4
        elif avg_cost < 0.05:
            return 3
        elif avg_cost < 0.15:
            return 2
        return 1


# Configuracoes dos modelos (precos aproximados - verificar documentacao oficial)
MODEL_CONFIGS: Dict[ClaudeModel, ModelConfig] = {
    ClaudeModel.OPUS: ModelConfig(
        model_id=ClaudeModel.OPUS.value,
        display_name="Claude Opus 4.5 (Avancado)",
        short_name="Opus 4.5",
        max_tokens=8192,
        context_limit=200000,
        cost_per_1k_input=0.015,   # Estimativa
        cost_per_1k_output=0.075,  # Estimativa
        speed_rating=2,
        quality_rating=5,
        recommended_for=[
            TaskType.ARCHITECTURE,
            TaskType.SECURITY_AUDIT,
            TaskType.COMPLEX_REFACTORING,
            TaskType.SYSTEM_DESIGN,
            TaskType.CRITICAL_FIX,
            TaskType.MULTI_STEP_REASONING
        ]
    ),
    ClaudeModel.SONNET: ModelConfig(
        model_id=ClaudeModel.SONNET.value,
        display_name="Claude Sonnet 4 (Padrao)",
        short_name="Sonnet 4",
        max_tokens=8192,
        context_limit=200000,
        cost_per_1k_input=0.003,
        cost_per_1k_output=0.015,
        speed_rating=4,
        quality_rating=4,
        recommended_for=[
            TaskType.DEVELOPMENT,
            TaskType.CODE_REVIEW,
            TaskType.TEST_GENERATION,
            TaskType.REFACTORING,
            TaskType.BUG_FIX,
            TaskType.API_DESIGN
        ]
    ),
    ClaudeModel.HAIKU: ModelConfig(
        model_id=ClaudeModel.HAIKU.value,
        display_name="Claude Haiku 3.5 (Rapido)",
        short_name="Haiku 3.5",
        max_tokens=4096,
        context_limit=200000,
        cost_per_1k_input=0.0008,
        cost_per_1k_output=0.004,
        speed_rating=5,
        quality_rating=3,
        recommended_for=[
            TaskType.FORMAT_CODE,
            TaskType.SIMPLE_FIX,
            TaskType.DOCUMENTATION,
            TaskType.TRANSLATION,
            TaskType.SIMPLE_GENERATION
        ]
    )
}


# Mapeamento de complexidade para modelo recomendado
COMPLEXITY_MODEL_MAP: Dict[TaskComplexity, ClaudeModel] = {
    TaskComplexity.SIMPLE: ClaudeModel.HAIKU,
    TaskComplexity.MEDIUM: ClaudeModel.SONNET,
    TaskComplexity.COMPLEX: ClaudeModel.SONNET,
    TaskComplexity.VERY_COMPLEX: ClaudeModel.OPUS
}


# =============================================================================
# MODEL SELECTOR
# =============================================================================

@dataclass
class ModelSelection:
    """Resultado da selecao de modelo"""
    model: ClaudeModel
    reason: str
    confidence: float  # 0-1
    alternatives: List[ClaudeModel] = field(default_factory=list)
    estimated_cost: float = 0.0  # Custo estimado em USD

    def to_dict(self) -> Dict[str, Any]:
        return {
            "model": self.model.value,
            "model_name": self.model.display_name,
            "reason": self.reason,
            "confidence": self.confidence,
            "alternatives": [m.value for m in self.alternatives],
            "estimated_cost": self.estimated_cost
        }


class ModelSelector:
    """
    Seletor inteligente de modelos Claude.

    Determina o melhor modelo baseado em:
    - Tipo de tarefa
    - Complexidade
    - Preferencia do usuario
    - Custo/beneficio

    Tambem gerencia fallback automatico em caso de erro.
    """

    def __init__(
        self,
        default_model: ClaudeModel = ClaudeModel.SONNET,
        enable_auto_selection: bool = True,
        prefer_economy: bool = False,
        prefer_quality: bool = False
    ):
        """
        Inicializa o seletor de modelos.

        Args:
            default_model: Modelo padrao quando nao ha indicacao
            enable_auto_selection: Habilita selecao automatica baseada em contexto
            prefer_economy: Prioriza modelos mais baratos
            prefer_quality: Prioriza modelos de maior qualidade
        """
        self.default_model = default_model
        self.enable_auto_selection = enable_auto_selection
        self.prefer_economy = prefer_economy
        self.prefer_quality = prefer_quality

        # Historico de selecoes para analytics
        self._selection_history: List[Dict] = []

        # Ordem de fallback
        self._fallback_order = [
            ClaudeModel.SONNET,  # Tenta Sonnet primeiro
            ClaudeModel.HAIKU,  # Depois Haiku (mais barato)
            ClaudeModel.OPUS    # Por ultimo Opus (mais caro)
        ]

    def select_model(
        self,
        task_type: Optional[TaskType] = None,
        complexity: Optional[TaskComplexity] = None,
        description: Optional[str] = None,
        story_points: Optional[int] = None,
        override_model: Optional[str] = None,
        estimated_tokens: int = 4000
    ) -> ModelSelection:
        """
        Seleciona o modelo mais apropriado para a tarefa.

        Args:
            task_type: Tipo da tarefa
            complexity: Complexidade da tarefa
            description: Descricao textual da tarefa
            story_points: Story points (se aplicavel)
            override_model: Modelo especifico solicitado pelo usuario
            estimated_tokens: Tokens estimados para calcular custo

        Returns:
            ModelSelection com o modelo escolhido e justificativa
        """
        # Se usuario especificou modelo, usar esse
        if override_model:
            model = ClaudeModel.from_string(override_model)
            return ModelSelection(
                model=model,
                reason=f"Modelo especificado pelo usuario: {model.display_name}",
                confidence=1.0,
                alternatives=self._get_alternatives(model),
                estimated_cost=self._estimate_cost(model, estimated_tokens)
            )

        # Se auto-selecao desabilitada, usar default
        if not self.enable_auto_selection:
            return ModelSelection(
                model=self.default_model,
                reason=f"Modelo padrao: {self.default_model.display_name}",
                confidence=0.8,
                alternatives=self._get_alternatives(self.default_model),
                estimated_cost=self._estimate_cost(self.default_model, estimated_tokens)
            )

        # Determinar complexidade se nao fornecida
        if complexity is None:
            complexity = self._infer_complexity(description, story_points, task_type)

        # Selecionar baseado em tipo de tarefa
        if task_type:
            model = self._select_by_task_type(task_type)
            if model:
                return ModelSelection(
                    model=model,
                    reason=f"Recomendado para {task_type.value}: {model.display_name}",
                    confidence=0.9,
                    alternatives=self._get_alternatives(model),
                    estimated_cost=self._estimate_cost(model, estimated_tokens)
                )

        # Selecionar baseado em complexidade
        model = COMPLEXITY_MODEL_MAP.get(complexity, self.default_model)

        # Ajustar por preferencias
        if self.prefer_economy and model == ClaudeModel.OPUS:
            model = ClaudeModel.SONNET
        elif self.prefer_quality and model == ClaudeModel.HAIKU:
            model = ClaudeModel.SONNET

        return ModelSelection(
            model=model,
            reason=f"Baseado em complexidade ({complexity.value}): {model.display_name}",
            confidence=0.85,
            alternatives=self._get_alternatives(model),
            estimated_cost=self._estimate_cost(model, estimated_tokens)
        )

    def select_for_story(
        self,
        story_points: int,
        complexity: str,
        category: str,
        has_technical_notes: bool = False,
        override_model: Optional[str] = None
    ) -> ModelSelection:
        """
        Seleciona modelo especificamente para uma User Story.

        Args:
            story_points: Pontos da story (Fibonacci)
            complexity: Complexidade (low, medium, high, very_high)
            category: Categoria (feature, bug, tech_debt, etc)
            has_technical_notes: Se tem notas tecnicas detalhadas
            override_model: Modelo especifico solicitado

        Returns:
            ModelSelection apropriada para a story
        """
        if override_model:
            return self.select_model(override_model=override_model)

        # Determinar tipo de tarefa pela categoria
        category_task_map = {
            "feature": TaskType.DEVELOPMENT,
            "bug": TaskType.BUG_FIX,
            "tech_debt": TaskType.REFACTORING,
            "spike": TaskType.ARCHITECTURE,
            "improvement": TaskType.REFACTORING
        }
        task_type = category_task_map.get(category, TaskType.DEVELOPMENT)

        # Ajustar complexidade por story points
        if story_points >= 13:
            complexity_enum = TaskComplexity.VERY_COMPLEX
        elif story_points >= 8:
            complexity_enum = TaskComplexity.COMPLEX
        elif story_points >= 3:
            complexity_enum = TaskComplexity.MEDIUM
        else:
            complexity_enum = TaskComplexity.SIMPLE

        # Override por complexidade explicita
        complexity_map = {
            "low": TaskComplexity.SIMPLE,
            "medium": TaskComplexity.MEDIUM,
            "high": TaskComplexity.COMPLEX,
            "very_high": TaskComplexity.VERY_COMPLEX
        }
        if complexity in complexity_map:
            # Usar o maior entre story points e complexidade explicita
            explicit = complexity_map[complexity]
            if list(TaskComplexity).index(explicit) > list(TaskComplexity).index(complexity_enum):
                complexity_enum = explicit

        # Spike sempre usa Opus
        if category == "spike":
            return ModelSelection(
                model=ClaudeModel.OPUS,
                reason="Spike requer analise profunda - usando Opus",
                confidence=0.95,
                alternatives=[ClaudeModel.SONNET],
                estimated_cost=self._estimate_cost(ClaudeModel.OPUS, 8000)
            )

        # Bug critico usa Opus
        if category == "bug" and story_points >= 8:
            return ModelSelection(
                model=ClaudeModel.OPUS,
                reason="Bug complexo requer analise detalhada - usando Opus",
                confidence=0.9,
                alternatives=[ClaudeModel.SONNET],
                estimated_cost=self._estimate_cost(ClaudeModel.OPUS, 6000)
            )

        return self.select_model(
            task_type=task_type,
            complexity=complexity_enum,
            story_points=story_points
        )

    def get_fallback_model(self, failed_model: ClaudeModel) -> Optional[ClaudeModel]:
        """
        Retorna o proximo modelo na ordem de fallback.

        Args:
            failed_model: Modelo que falhou

        Returns:
            Proximo modelo para tentar, ou None se nao houver
        """
        try:
            current_idx = self._fallback_order.index(failed_model)
            if current_idx < len(self._fallback_order) - 1:
                return self._fallback_order[current_idx + 1]
        except ValueError:
            # Modelo nao esta na lista de fallback, usar ordem padrao
            return self._fallback_order[0] if self._fallback_order else None
        return None

    def _select_by_task_type(self, task_type: TaskType) -> Optional[ClaudeModel]:
        """Seleciona modelo baseado no tipo de tarefa"""
        for model, config in MODEL_CONFIGS.items():
            if task_type in config.recommended_for:
                return model
        return None

    def _infer_complexity(
        self,
        description: Optional[str],
        story_points: Optional[int],
        task_type: Optional[TaskType]
    ) -> TaskComplexity:
        """Infere complexidade baseado em contexto"""

        # Por story points
        if story_points:
            if story_points >= 13:
                return TaskComplexity.VERY_COMPLEX
            elif story_points >= 8:
                return TaskComplexity.COMPLEX
            elif story_points >= 3:
                return TaskComplexity.MEDIUM
            return TaskComplexity.SIMPLE

        # Por tipo de tarefa
        if task_type:
            complex_tasks = [
                TaskType.ARCHITECTURE,
                TaskType.SECURITY_AUDIT,
                TaskType.COMPLEX_REFACTORING,
                TaskType.SYSTEM_DESIGN,
                TaskType.MULTI_STEP_REASONING
            ]
            simple_tasks = [
                TaskType.FORMAT_CODE,
                TaskType.SIMPLE_FIX,
                TaskType.DOCUMENTATION,
                TaskType.TRANSLATION
            ]

            if task_type in complex_tasks:
                return TaskComplexity.COMPLEX
            elif task_type in simple_tasks:
                return TaskComplexity.SIMPLE

        # Por palavras-chave na descricao
        if description:
            desc_lower = description.lower()

            complex_keywords = [
                "arquitetura", "architecture", "seguranca", "security",
                "refatoracao", "refactoring", "sistema", "system",
                "complexo", "complex", "critico", "critical",
                "integracao", "integration", "migracao", "migration"
            ]
            simple_keywords = [
                "simples", "simple", "rapido", "quick", "pequeno", "small",
                "formatacao", "format", "documentacao", "documentation",
                "correcao", "fix", "ajuste", "adjustment"
            ]

            if any(kw in desc_lower for kw in complex_keywords):
                return TaskComplexity.COMPLEX
            elif any(kw in desc_lower for kw in simple_keywords):
                return TaskComplexity.SIMPLE

        # Default
        return TaskComplexity.MEDIUM

    def _get_alternatives(self, selected: ClaudeModel) -> List[ClaudeModel]:
        """Retorna modelos alternativos ao selecionado"""
        return [m for m in ClaudeModel if m != selected]

    def _estimate_cost(self, model: ClaudeModel, estimated_tokens: int) -> float:
        """Estima custo em USD para uma operacao"""
        config = MODEL_CONFIGS.get(model)
        if not config:
            return 0.0

        # Assumindo 60% input, 40% output
        input_tokens = estimated_tokens * 0.6
        output_tokens = estimated_tokens * 0.4

        input_cost = (input_tokens / 1000) * config.cost_per_1k_input
        output_cost = (output_tokens / 1000) * config.cost_per_1k_output

        return round(input_cost + output_cost, 6)

    def get_model_info(self, model: Optional[ClaudeModel] = None) -> Dict[str, Any]:
        """Retorna informacoes sobre um ou todos os modelos"""
        if model:
            config = MODEL_CONFIGS.get(model)
            if config:
                return {
                    "model_id": config.model_id,
                    "display_name": config.display_name,
                    "short_name": config.short_name,
                    "max_tokens": config.max_tokens,
                    "context_limit": config.context_limit,
                    "cost_per_1k_input": config.cost_per_1k_input,
                    "cost_per_1k_output": config.cost_per_1k_output,
                    "speed_rating": config.speed_rating,
                    "quality_rating": config.quality_rating,
                    "cost_rating": config.cost_rating,
                    "recommended_for": [t.value for t in config.recommended_for]
                }
            return {}

        # Retorna todos
        return {
            model.value: self.get_model_info(model)
            for model in ClaudeModel
        }

    def get_available_models(self) -> List[Dict[str, Any]]:
        """Retorna lista de modelos disponiveis para UI"""
        return [
            {
                "id": model.value,
                "name": model.display_name,
                "short_name": model.short_name,
                "speed": MODEL_CONFIGS[model].speed_rating,
                "quality": MODEL_CONFIGS[model].quality_rating,
                "cost": MODEL_CONFIGS[model].cost_rating
            }
            for model in ClaudeModel
        ]


# =============================================================================
# SINGLETON E HELPERS
# =============================================================================

_selector_instance: Optional[ModelSelector] = None


def get_model_selector(
    default_model: str = None,
    prefer_economy: bool = False,
    prefer_quality: bool = False
) -> ModelSelector:
    """
    Retorna instancia do seletor de modelos.

    Args:
        default_model: Modelo padrao (string)
        prefer_economy: Priorizar economia
        prefer_quality: Priorizar qualidade

    Returns:
        Instancia de ModelSelector
    """
    global _selector_instance

    if _selector_instance is None:
        default = ClaudeModel.from_string(default_model) if default_model else ClaudeModel.SONNET
        _selector_instance = ModelSelector(
            default_model=default,
            prefer_economy=prefer_economy,
            prefer_quality=prefer_quality
        )

    return _selector_instance


def select_model_for_task(
    task_type: str = None,
    complexity: str = None,
    description: str = None,
    story_points: int = None,
    override_model: str = None
) -> Dict[str, Any]:
    """
    Funcao helper para selecionar modelo.

    Args:
        task_type: Tipo de tarefa (string)
        complexity: Complexidade (string)
        description: Descricao da tarefa
        story_points: Story points
        override_model: Modelo especifico

    Returns:
        Dict com informacoes da selecao
    """
    selector = get_model_selector()

    # Converter strings para enums
    task_enum = None
    if task_type:
        try:
            task_enum = TaskType(task_type)
        except ValueError:
            pass

    complexity_enum = None
    if complexity:
        try:
            complexity_enum = TaskComplexity(complexity)
        except ValueError:
            pass

    selection = selector.select_model(
        task_type=task_enum,
        complexity=complexity_enum,
        description=description,
        story_points=story_points,
        override_model=override_model
    )

    return selection.to_dict()


def get_available_models() -> List[Dict[str, Any]]:
    """Retorna lista de modelos disponiveis"""
    return get_model_selector().get_available_models()


def get_model_config(model_id: str) -> Dict[str, Any]:
    """Retorna configuracao de um modelo especifico"""
    model = ClaudeModel.from_string(model_id)
    return get_model_selector().get_model_info(model)


# =============================================================================
# CLI TEST
# =============================================================================

if __name__ == "__main__":
    print("\n=== Model Selector - Plataforma E ===\n")

    selector = ModelSelector()

    # Mostrar modelos disponiveis
    print("Modelos disponiveis:")
    for model_info in selector.get_available_models():
        print(f"  - {model_info['name']}")
        print(f"    ID: {model_info['id']}")
        print(f"    Velocidade: {'*' * model_info['speed']}")
        print(f"    Qualidade: {'*' * model_info['quality']}")
        print(f"    Economia: {'*' * model_info['cost']}")
        print()

    # Testar selecao por tipo de tarefa
    print("\nSelecao por tipo de tarefa:")
    for task_type in [TaskType.SIMPLE_FIX, TaskType.DEVELOPMENT, TaskType.ARCHITECTURE]:
        selection = selector.select_model(task_type=task_type)
        print(f"  {task_type.value}: {selection.model.short_name} ({selection.reason})")

    # Testar selecao por story points
    print("\nSelecao por story points:")
    for points in [1, 3, 8, 13, 21]:
        selection = selector.select_for_story(
            story_points=points,
            complexity="medium",
            category="feature"
        )
        print(f"  {points} pts: {selection.model.short_name} - Custo estimado: ${selection.estimated_cost:.4f}")

    print("\n=== Teste concluido ===")

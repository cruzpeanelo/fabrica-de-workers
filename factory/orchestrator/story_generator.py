"""
Gerador de Historias - Plataforma E
==========================================

Gera historias automaticamente seguindo melhores praticas Agile/Kanban.
Historias sao geradas no formato padrao e passam por validacao.
"""

import re
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field, asdict
from enum import Enum
import uuid


class StoryType(Enum):
    FEATURE = "feature"
    BUG = "bug"
    TECH_DEBT = "tech_debt"
    SPIKE = "spike"
    CHORE = "chore"


class StoryPriority(Enum):
    CRITICAL = 1
    HIGH = 2
    MEDIUM = 3
    LOW = 4
    NICE_TO_HAVE = 5


@dataclass
class AcceptanceCriteria:
    """Criterio de aceite de uma historia"""
    criteria_id: str
    description: str
    is_automated: bool = False
    is_verified: bool = False


@dataclass
class UserStory:
    """
    Historia de Usuario no formato Agile

    Formato: Como [persona], eu quero [funcionalidade], para que [beneficio]
    """
    story_id: str
    title: str

    # Formato Agile
    as_a: str  # Persona
    i_want: str  # Funcionalidade desejada
    so_that: str  # Beneficio/valor

    # Detalhamento
    description: str
    acceptance_criteria: List[AcceptanceCriteria]
    technical_notes: Optional[str] = None

    # Classificacao
    story_type: StoryType = StoryType.FEATURE
    priority: StoryPriority = StoryPriority.MEDIUM
    story_points: int = 3
    sprint: int = 1

    # Rastreabilidade
    source: str = "manual"  # manual, transcription, video, document
    source_reference: Optional[str] = None
    source_timestamp: Optional[str] = None

    # Status
    status: str = "draft"  # draft, pending_review, approved, rejected, in_progress, done
    assigned_to: Optional[str] = None
    reviewer: Optional[str] = None

    # Validacao
    is_validated: bool = False
    validation_score: float = 0.0
    validation_notes: Optional[str] = None

    # Metadados
    created_at: datetime = field(default_factory=datetime.now)
    created_by: str = "system"
    tags: List[str] = field(default_factory=list)
    dependencies: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict:
        """Converte para dicionario"""
        return {
            "story_id": self.story_id,
            "title": self.title,
            "as_a": self.as_a,
            "i_want": self.i_want,
            "so_that": self.so_that,
            "description": self.description,
            "acceptance_criteria": [
                {"id": ac.criteria_id, "description": ac.description,
                 "is_automated": ac.is_automated, "is_verified": ac.is_verified}
                for ac in self.acceptance_criteria
            ],
            "technical_notes": self.technical_notes,
            "story_type": self.story_type.value,
            "priority": self.priority.value,
            "story_points": self.story_points,
            "sprint": self.sprint,
            "source": self.source,
            "source_reference": self.source_reference,
            "status": self.status,
            "assigned_to": self.assigned_to,
            "reviewer": self.reviewer,
            "is_validated": self.is_validated,
            "validation_score": self.validation_score,
            "validation_notes": self.validation_notes,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "created_by": self.created_by,
            "tags": self.tags,
            "dependencies": self.dependencies
        }

    def get_full_description(self) -> str:
        """Retorna descricao completa no formato Agile"""
        return f"""**Como** {self.as_a}
**Eu quero** {self.i_want}
**Para que** {self.so_that}

## Descricao
{self.description}

## Criterios de Aceite
{chr(10).join([f'- [ ] {ac.description}' for ac in self.acceptance_criteria])}

{f'## Notas Tecnicas{chr(10)}{self.technical_notes}' if self.technical_notes else ''}
"""


class StoryGenerator:
    """
    Gerador de Historias

    Analisa conteudo extraido e gera historias automaticamente
    seguindo melhores praticas de Agile e Kanban.
    """

    def __init__(self, project_id: str):
        self.project_id = project_id
        self.stories: List[UserStory] = []
        self.story_counter = 0

        # Templates de historias por dominio
        self.templates = self._load_templates()

        # Palavras-chave para classificacao
        self.priority_keywords = {
            StoryPriority.CRITICAL: ['urgente', 'critico', 'bloqueante', 'imediato'],
            StoryPriority.HIGH: ['importante', 'prioritario', 'essencial', 'necessario'],
            StoryPriority.MEDIUM: ['deveria', 'seria bom', 'melhorar'],
            StoryPriority.LOW: ['eventualmente', 'futuro', 'nice to have'],
        }

        self.type_keywords = {
            StoryType.BUG: ['erro', 'bug', 'falha', 'problema', 'corrigir'],
            StoryType.TECH_DEBT: ['refatorar', 'melhorar codigo', 'tecnicamente'],
            StoryType.SPIKE: ['investigar', 'pesquisar', 'avaliar', 'poc'],
        }

    def _load_templates(self) -> Dict:
        """Carrega templates de historias"""
        return {
            "bpm": {
                "visualization": {
                    "as_a": "usuario de negocios",
                    "i_want": "visualizar {process_name} em formato grafico",
                    "so_that": "possa entender o fluxo atual do processo"
                },
                "editing": {
                    "as_a": "analista de processos",
                    "i_want": "editar {process_name} diretamente na interface",
                    "so_that": "possa atualizar a documentacao de forma agil"
                },
                "comparison": {
                    "as_a": "gestor",
                    "i_want": "comparar AS-IS com TO-BE de {process_name}",
                    "so_that": "possa entender as mudancas propostas"
                }
            },
            "crud": {
                "create": {
                    "as_a": "usuario",
                    "i_want": "criar {entity}",
                    "so_that": "possa registrar novos dados no sistema"
                },
                "read": {
                    "as_a": "usuario",
                    "i_want": "visualizar {entity}",
                    "so_that": "possa consultar informacoes existentes"
                },
                "update": {
                    "as_a": "usuario",
                    "i_want": "editar {entity}",
                    "so_that": "possa manter os dados atualizados"
                },
                "delete": {
                    "as_a": "administrador",
                    "i_want": "remover {entity}",
                    "so_that": "possa manter a base de dados limpa"
                }
            }
        }

    def generate_story_id(self) -> str:
        """Gera ID unico para historia"""
        self.story_counter += 1
        return f"US-{self.project_id}-{self.story_counter:03d}"

    def generate_from_text(self, text: str, source: str = "document") -> List[UserStory]:
        """
        Gera historias a partir de texto extraido

        Args:
            text: Texto extraido de documento/video/audio
            source: Origem do texto

        Returns:
            Lista de historias geradas
        """
        generated_stories = []

        # Analisa o texto e identifica requisitos
        requirements = self._extract_requirements(text)

        for req in requirements:
            story = self._requirement_to_story(req, source)
            if story:
                generated_stories.append(story)
                self.stories.append(story)

        return generated_stories

    def generate_from_requirements(self, requirements: List[str],
                                   source: str = "extracted") -> List[UserStory]:
        """Gera historias a partir de lista de requisitos"""
        generated_stories = []

        for req in requirements:
            story = self._requirement_to_story(req, source)
            if story:
                generated_stories.append(story)
                self.stories.append(story)

        return generated_stories

    def _extract_requirements(self, text: str) -> List[Dict]:
        """Extrai requisitos do texto"""
        requirements = []

        # Patterns para identificar requisitos
        patterns = [
            r'(?:o sistema|a aplicacao|o usuario|devemos?|precisamos?|queremos?)\s+(.+?)(?:\.|$)',
            r'(?:funcionalidade|feature|requisito):\s*(.+?)(?:\.|$)',
            r'(?:como|quando|para)\s+(.+?),?\s+(?:eu quero|preciso|deve)',
        ]

        lines = text.split('\n')
        for line in lines:
            line = line.strip()
            if len(line) < 20:
                continue

            for pattern in patterns:
                matches = re.findall(pattern, line.lower())
                if matches:
                    requirements.append({
                        "text": line,
                        "match": matches[0],
                        "confidence": 0.7
                    })
                    break

            # Tambem adiciona linhas que contem palavras-chave
            keywords = ['cadastro', 'consulta', 'relatorio', 'dashboard',
                       'processo', 'workflow', 'aprovacao', 'integracao']

            for kw in keywords:
                if kw in line.lower() and len(line) > 30:
                    requirements.append({
                        "text": line,
                        "keyword": kw,
                        "confidence": 0.5
                    })
                    break

        # Remove duplicatas
        seen = set()
        unique_reqs = []
        for req in requirements:
            if req['text'] not in seen:
                seen.add(req['text'])
                unique_reqs.append(req)

        return unique_reqs[:30]  # Limita a 30 requisitos

    def _requirement_to_story(self, requirement: Dict, source: str) -> Optional[UserStory]:
        """Converte um requisito em historia"""
        text = requirement.get('text', '')

        if not text or len(text) < 20:
            return None

        # Determina tipo e prioridade
        story_type = self._classify_type(text)
        priority = self._classify_priority(text)

        # Gera titulo
        title = self._generate_title(text)

        # Gera formato Agile
        as_a, i_want, so_that = self._generate_agile_format(text)

        # Gera criterios de aceite
        acceptance_criteria = self._generate_acceptance_criteria(text)

        # Estima pontos
        points = self._estimate_points(text, acceptance_criteria)

        story = UserStory(
            story_id=self.generate_story_id(),
            title=title,
            as_a=as_a,
            i_want=i_want,
            so_that=so_that,
            description=text,
            acceptance_criteria=acceptance_criteria,
            story_type=story_type,
            priority=priority,
            story_points=points,
            source=source,
            source_reference=text[:100],
            status="pending_review",
            tags=self._extract_tags(text)
        )

        return story

    def _classify_type(self, text: str) -> StoryType:
        """Classifica o tipo da historia"""
        text_lower = text.lower()

        for story_type, keywords in self.type_keywords.items():
            for kw in keywords:
                if kw in text_lower:
                    return story_type

        return StoryType.FEATURE

    def _classify_priority(self, text: str) -> StoryPriority:
        """Classifica a prioridade da historia"""
        text_lower = text.lower()

        for priority, keywords in self.priority_keywords.items():
            for kw in keywords:
                if kw in text_lower:
                    return priority

        return StoryPriority.MEDIUM

    def _generate_title(self, text: str) -> str:
        """Gera titulo para a historia"""
        # Pega as primeiras palavras significativas
        words = text.split()[:10]
        title = ' '.join(words)

        if len(title) > 60:
            title = title[:57] + '...'

        return title.capitalize()

    def _generate_agile_format(self, text: str) -> Tuple[str, str, str]:
        """Gera formato As a / I want / So that"""
        text_lower = text.lower()

        # Tenta extrair persona
        personas = ['usuario', 'administrador', 'gestor', 'analista',
                   'desenvolvedor', 'cliente', 'vendedor']
        as_a = "usuario do sistema"
        for persona in personas:
            if persona in text_lower:
                as_a = persona
                break

        # I want - usa o texto principal
        i_want = text[:150] if len(text) > 150 else text

        # So that - infere o beneficio
        benefits = {
            'cadastro': 'possa registrar informacoes no sistema',
            'consulta': 'possa acessar informacoes quando necessario',
            'relatorio': 'possa analisar dados e tomar decisoes',
            'processo': 'possa executar o fluxo de trabalho corretamente',
            'aprovacao': 'possa validar e autorizar acoes',
            'integracao': 'possa conectar diferentes sistemas'
        }

        so_that = "possa realizar minhas atividades de forma eficiente"
        for key, benefit in benefits.items():
            if key in text_lower:
                so_that = benefit
                break

        return as_a, i_want, so_that

    def _generate_acceptance_criteria(self, text: str) -> List[AcceptanceCriteria]:
        """Gera criterios de aceite"""
        criteria = []

        # Criterios padrao baseados em boas praticas
        base_criteria = [
            "Funcionalidade implementada conforme especificado",
            "Interface seguindo identidade visual do projeto",
            "Dados persistidos corretamente no banco",
            "Testes unitarios implementados",
            "Documentacao atualizada"
        ]

        for i, desc in enumerate(base_criteria[:4]):
            criteria.append(AcceptanceCriteria(
                criteria_id=f"AC-{i+1:02d}",
                description=desc
            ))

        # Adiciona criterios especificos baseados no texto
        if 'validacao' in text.lower() or 'valida' in text.lower():
            criteria.append(AcceptanceCriteria(
                criteria_id=f"AC-{len(criteria)+1:02d}",
                description="Validacoes de campos implementadas"
            ))

        if 'erro' in text.lower() or 'excecao' in text.lower():
            criteria.append(AcceptanceCriteria(
                criteria_id=f"AC-{len(criteria)+1:02d}",
                description="Tratamento de erros implementado"
            ))

        return criteria

    def _estimate_points(self, text: str, criteria: List[AcceptanceCriteria]) -> int:
        """Estima story points baseado na complexidade"""
        base_points = 3

        # Ajusta baseado no tamanho do texto
        if len(text) > 200:
            base_points += 2
        elif len(text) > 100:
            base_points += 1

        # Ajusta baseado na quantidade de criterios
        base_points += len(criteria) - 3

        # Ajusta baseado em palavras de complexidade
        complexity_words = ['integracao', 'complexo', 'multiplos', 'relatorio',
                          'dashboard', 'workflow', 'automacao']

        for word in complexity_words:
            if word in text.lower():
                base_points += 1

        return min(max(base_points, 1), 21)  # Fibonacci-like: 1-21

    def _extract_tags(self, text: str) -> List[str]:
        """Extrai tags do texto"""
        tags = []
        tag_keywords = {
            'frontend': ['tela', 'interface', 'ui', 'ux', 'visual'],
            'backend': ['api', 'servico', 'banco', 'dados'],
            'integracao': ['integracao', 'conectar', 'sincronizar'],
            'relatorio': ['relatorio', 'dashboard', 'grafico'],
            'processo': ['processo', 'workflow', 'fluxo', 'bpm']
        }

        text_lower = text.lower()
        for tag, keywords in tag_keywords.items():
            for kw in keywords:
                if kw in text_lower:
                    tags.append(tag)
                    break

        return list(set(tags))

    def create_manual_story(self, title: str, as_a: str, i_want: str,
                           so_that: str, acceptance_criteria: List[str],
                           **kwargs) -> UserStory:
        """
        Cria historia manualmente com todos os detalhes

        Args:
            title: Titulo da historia
            as_a: Persona (Como um...)
            i_want: Funcionalidade (Eu quero...)
            so_that: Beneficio (Para que...)
            acceptance_criteria: Lista de criterios de aceite
            **kwargs: Campos opcionais (points, priority, sprint, tags, etc)

        Returns:
            Historia criada
        """
        criteria_objects = [
            AcceptanceCriteria(
                criteria_id=f"AC-{i+1:02d}",
                description=desc
            )
            for i, desc in enumerate(acceptance_criteria)
        ]

        story = UserStory(
            story_id=self.generate_story_id(),
            title=title,
            as_a=as_a,
            i_want=i_want,
            so_that=so_that,
            description=kwargs.get('description', ''),
            acceptance_criteria=criteria_objects,
            story_type=StoryType(kwargs.get('story_type', 'feature')),
            priority=StoryPriority(kwargs.get('priority', 3)),
            story_points=kwargs.get('story_points', 3),
            sprint=kwargs.get('sprint', 1),
            source="manual",
            status="draft",
            created_by=kwargs.get('created_by', 'user'),
            tags=kwargs.get('tags', []),
            technical_notes=kwargs.get('technical_notes')
        )

        self.stories.append(story)
        return story

    def validate_story(self, story: UserStory, validator_id: str) -> Tuple[bool, float, str]:
        """
        Valida uma historia usando criterios de qualidade

        Returns:
            (is_valid, score, notes)
        """
        score = 0.0
        issues = []

        # Verifica titulo
        if len(story.title) >= 10:
            score += 15
        else:
            issues.append("Titulo muito curto")

        # Verifica formato Agile
        if story.as_a and len(story.as_a) >= 3:
            score += 15
        else:
            issues.append("Persona nao definida")

        if story.i_want and len(story.i_want) >= 10:
            score += 20
        else:
            issues.append("Funcionalidade nao clara")

        if story.so_that and len(story.so_that) >= 10:
            score += 15
        else:
            issues.append("Beneficio nao definido")

        # Verifica criterios de aceite
        if len(story.acceptance_criteria) >= 3:
            score += 20
        elif len(story.acceptance_criteria) >= 1:
            score += 10
            issues.append("Poucos criterios de aceite")
        else:
            issues.append("Sem criterios de aceite")

        # Verifica estimativa
        if 1 <= story.story_points <= 21:
            score += 15
        else:
            issues.append("Story points invalidos")

        # Determina se e valida
        is_valid = score >= 70

        notes = "Historia aprovada" if is_valid else f"Problemas: {', '.join(issues)}"

        # Atualiza a historia
        story.is_validated = is_valid
        story.validation_score = score
        story.validation_notes = notes
        story.reviewer = validator_id
        story.status = "approved" if is_valid else "pending_review"

        return is_valid, score, notes

    def get_stories_by_status(self, status: str) -> List[UserStory]:
        """Retorna historias por status"""
        return [s for s in self.stories if s.status == status]

    def get_stories_by_sprint(self, sprint: int) -> List[UserStory]:
        """Retorna historias de um sprint"""
        return [s for s in self.stories if s.sprint == sprint]

    def export_to_json(self) -> str:
        """Exporta todas as historias para JSON"""
        return json.dumps(
            [s.to_dict() for s in self.stories],
            indent=2,
            ensure_ascii=False
        )

    def get_backlog_summary(self) -> Dict:
        """Retorna resumo do backlog"""
        total = len(self.stories)
        by_status = {}
        by_sprint = {}
        total_points = 0

        for story in self.stories:
            by_status[story.status] = by_status.get(story.status, 0) + 1
            by_sprint[story.sprint] = by_sprint.get(story.sprint, 0) + 1
            total_points += story.story_points

        return {
            "total_stories": total,
            "by_status": by_status,
            "by_sprint": by_sprint,
            "total_points": total_points,
            "pending_validation": len(self.get_stories_by_status("pending_review")),
            "ready_for_dev": len(self.get_stories_by_status("approved"))
        }

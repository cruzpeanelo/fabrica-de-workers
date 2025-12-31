# -*- coding: utf-8 -*-
"""
Story Generator - Gerador de User Stories Completas
====================================================

Gera User Stories no padrao de mercado com:
- Narrativa completa (Como/Quero/Para)
- Criterios de aceite detalhados
- Definition of Done
- Tasks por agente
- Estimativas e complexidade
- Dependencias
"""

import json
import uuid
from datetime import datetime, timezone
from typing import List, Dict, Optional, Any
from dataclasses import dataclass, field, asdict

# Mapeamento de agentes por especialidade
AGENT_SPECIALTIES = {
    # Management
    "AGT-01": {"name": "Gestao Estrategica", "skills": ["planning", "coordination", "decisions"]},
    "AGT-02": {"name": "Product Manager", "skills": ["roadmap", "prioritization", "stakeholders"]},
    "AGT-03": {"name": "Product Owner", "skills": ["backlog", "stories", "acceptance_criteria"]},
    "AGT-04": {"name": "Project Manager", "skills": ["sprints", "tracking", "risks"]},

    # Data
    "AGT-05": {"name": "Analista de Dados", "skills": ["sql", "analytics", "visualization"]},
    "AGT-06": {"name": "Engenheiro de Dados", "skills": ["etl", "pipelines", "data_quality"]},
    "AGT-07": {"name": "DBA", "skills": ["database", "schema", "performance", "indexes"]},

    # Development
    "AGT-08": {"name": "Backend Developer", "skills": ["api", "fastapi", "python", "business_logic"]},
    "AGT-09": {"name": "Frontend Developer", "skills": ["vue", "react", "css", "components", "ui"]},
    "AGT-10": {"name": "Security Specialist", "skills": ["auth", "encryption", "vulnerabilities"]},

    # Design
    "AGT-11": {"name": "UX Designer", "skills": ["wireframes", "flows", "research", "usability"]},
    "AGT-12": {"name": "UI Designer", "skills": ["design_system", "visual", "tokens", "figma"]},

    # Quality
    "AGT-13": {"name": "Code Reviewer", "skills": ["code_review", "best_practices", "refactoring"]},
    "AGT-15": {"name": "QA Engineer", "skills": ["testing", "test_cases", "automation"]},
    "AGT-16": {"name": "E2E Tester", "skills": ["playwright", "e2e", "browser_tests"]},

    # Infrastructure
    "AGT-14": {"name": "DevOps", "skills": ["docker", "ci_cd", "deploy", "monitoring"]},
    "AGT-17": {"name": "Tech Writer", "skills": ["documentation", "guides", "api_docs"]},
    "AGT-18": {"name": "Architect", "skills": ["architecture", "patterns", "design", "decisions"]},
    "AGT-19": {"name": "Integrator", "skills": ["apis", "webhooks", "connectors", "integrations"]},
}

# Mapeamento de categoria para agentes responsaveis
CATEGORY_AGENTS = {
    "database": ["AGT-07", "AGT-06"],
    "backend": ["AGT-08", "AGT-10"],
    "frontend": ["AGT-09", "AGT-11", "AGT-12"],
    "api": ["AGT-08", "AGT-19"],
    "integration": ["AGT-19", "AGT-08"],
    "security": ["AGT-10", "AGT-08"],
    "infrastructure": ["AGT-14", "AGT-18"],
    "documentation": ["AGT-17"],
    "testing": ["AGT-15", "AGT-16"],
    "design": ["AGT-11", "AGT-12"],
    "architecture": ["AGT-18", "AGT-08"],
}

# Definition of Done padrao
DEFAULT_DOD = [
    "Codigo implementado e funcionando",
    "Testes unitarios escritos e passando",
    "Code review aprovado",
    "Documentacao atualizada",
    "Deploy em ambiente de desenvolvimento",
    "Criterios de aceite validados"
]

# Status Kanban
KANBAN_STATUS = ["BACKLOG", "TODO", "IN_PROGRESS", "CODE_REVIEW", "TESTING", "DONE"]


@dataclass
class AgentTask:
    """Task especifica para um agente"""
    task_id: str
    agent_id: str
    agent_name: str
    title: str
    description: str
    task_type: str  # development, review, test, documentation, design
    estimated_hours: float
    skills_required: List[str]
    dependencies: List[str] = field(default_factory=list)
    acceptance_criteria: List[str] = field(default_factory=list)
    order: int = 0


@dataclass
class DetailedStory:
    """User Story completa e detalhada"""
    story_id: str
    project_id: str
    title: str
    description: str

    # Narrativa
    persona: str
    action: str
    benefit: str

    # Planejamento
    epic: str
    sprint: int
    priority: str  # CRITICAL, HIGH, MEDIUM, LOW
    points: int
    complexity: str  # low, medium, high, very_high
    business_value: int

    # Criterios
    acceptance_criteria: List[str]
    definition_of_done: List[str]
    business_rules: List[str]
    technical_notes: List[str]

    # Agentes e Tasks
    assigned_to: str  # Agente principal
    agents: List[str]  # Todos os agentes envolvidos
    tasks: List[AgentTask]

    # Dependencias
    dependencies: List[str]
    blocked_by: Optional[str] = None

    # Estimativas
    estimated_hours: float = 0
    risk_level: str = "low"

    # Categoria
    category: str = "feature"
    component: str = ""
    tags: List[str] = field(default_factory=list)


def generate_story_id() -> str:
    """Gera ID unico para story"""
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
    # Issue #355: Aumentar para 8 caracteres para evitar colisoes sob carga
    unique = uuid.uuid4().hex[:8].upper()
    return f"US-{timestamp}-{unique}"


def generate_task_id(story_id: str, agent_id: str, order: int) -> str:
    """Gera ID unico para task"""
    return f"TASK-{story_id.replace('US-', '')}-{agent_id.replace('AGT-', '')}-{order:02d}"


def get_agents_for_category(category: str) -> List[str]:
    """Retorna agentes recomendados para uma categoria"""
    return CATEGORY_AGENTS.get(category, ["AGT-08"])


def calculate_points(complexity: str, tasks_count: int) -> int:
    """Calcula story points baseado em complexidade e numero de tasks"""
    base_points = {
        "low": 2,
        "medium": 5,
        "high": 8,
        "very_high": 13
    }
    base = base_points.get(complexity, 5)
    # Ajusta baseado no numero de tasks
    if tasks_count > 5:
        return min(base + 3, 21)
    elif tasks_count > 3:
        return min(base + 2, 13)
    return base


def create_tasks_for_story(
    story_id: str,
    story_title: str,
    category: str,
    agents: List[str],
    complexity: str
) -> List[AgentTask]:
    """Cria tasks detalhadas para cada agente envolvido na story"""
    tasks = []
    order = 1

    # Horas base por complexidade
    hours_map = {"low": 2, "medium": 4, "high": 8, "very_high": 16}
    base_hours = hours_map.get(complexity, 4)

    for agent_id in agents:
        agent_info = AGENT_SPECIALTIES.get(agent_id, {})
        agent_name = agent_info.get("name", "Agente")
        agent_skills = agent_info.get("skills", [])

        # Determinar tipo de task baseado no agente
        if agent_id in ["AGT-07", "AGT-06"]:
            task_type = "database"
            title = f"Modelagem e implementacao de dados - {story_title}"
            description = f"Criar/ajustar modelos de dados, migrations, indices e queries necessarias"
            criteria = [
                "Modelo de dados criado/atualizado",
                "Migration executada com sucesso",
                "Indices otimizados",
                "Queries performaticas"
            ]
        elif agent_id == "AGT-08":
            task_type = "backend"
            title = f"Desenvolvimento backend - {story_title}"
            description = f"Implementar API endpoints, logica de negocio e integracao com banco"
            criteria = [
                "Endpoints implementados",
                "Validacoes de entrada",
                "Tratamento de erros",
                "Testes unitarios"
            ]
        elif agent_id == "AGT-09":
            task_type = "frontend"
            title = f"Desenvolvimento frontend - {story_title}"
            description = f"Implementar componentes Vue.js, telas e integracao com API"
            criteria = [
                "Componentes implementados",
                "Responsividade",
                "Integracao com API",
                "Estados de loading/erro"
            ]
        elif agent_id in ["AGT-11", "AGT-12"]:
            task_type = "design"
            title = f"Design UI/UX - {story_title}"
            description = f"Criar wireframes, prototipos e especificacoes visuais"
            criteria = [
                "Wireframes aprovados",
                "Design system aplicado",
                "Especificacoes de UI",
                "Assets exportados"
            ]
        elif agent_id == "AGT-13":
            task_type = "review"
            title = f"Code Review - {story_title}"
            description = f"Revisar codigo, verificar padroes e boas praticas"
            criteria = [
                "Codigo revisado",
                "Padroes verificados",
                "Sugestoes documentadas",
                "Aprovacao final"
            ]
        elif agent_id in ["AGT-15", "AGT-16"]:
            task_type = "testing"
            title = f"Testes - {story_title}"
            description = f"Criar e executar testes automatizados"
            criteria = [
                "Casos de teste criados",
                "Testes executados",
                "Bugs reportados",
                "Cobertura adequada"
            ]
        elif agent_id == "AGT-17":
            task_type = "documentation"
            title = f"Documentacao - {story_title}"
            description = f"Documentar funcionalidade, API e guias de uso"
            criteria = [
                "Documentacao tecnica",
                "Exemplos de uso",
                "API documentada"
            ]
        elif agent_id == "AGT-18":
            task_type = "architecture"
            title = f"Arquitetura - {story_title}"
            description = f"Definir arquitetura, padroes e decisoes tecnicas"
            criteria = [
                "Diagrama de arquitetura",
                "Decisoes documentadas",
                "Padroes definidos"
            ]
        elif agent_id == "AGT-19":
            task_type = "integration"
            title = f"Integracao - {story_title}"
            description = f"Implementar integracoes com sistemas externos"
            criteria = [
                "Conectores implementados",
                "Mapeamento de dados",
                "Tratamento de erros"
            ]
        else:
            task_type = "general"
            title = f"Task - {story_title}"
            description = f"Executar atividades relacionadas a story"
            criteria = ["Task concluida"]

        # Calcular horas estimadas
        estimated_hours = base_hours
        if task_type in ["backend", "frontend"]:
            estimated_hours = base_hours * 1.5
        elif task_type in ["testing", "review"]:
            estimated_hours = base_hours * 0.5

        task = AgentTask(
            task_id=generate_task_id(story_id, agent_id, order),
            agent_id=agent_id,
            agent_name=agent_name,
            title=title,
            description=description,
            task_type=task_type,
            estimated_hours=round(estimated_hours, 1),
            skills_required=agent_skills,
            acceptance_criteria=criteria,
            order=order
        )

        tasks.append(task)
        order += 1

    return tasks


def story_to_db_dict(story: DetailedStory) -> Dict[str, Any]:
    """Converte DetailedStory para dicionario compativel com o modelo Story do banco"""
    return {
        "story_id": story.story_id,
        "project_id": story.project_id,
        "title": story.title,
        "description": story.description,
        "epic_id": story.epic,
        "status": "BACKLOG",
        "sprint": story.sprint,
        "points": story.points,
        "priority": story.priority,
        "business_value": story.business_value,
        "narrative_persona": story.persona,
        "narrative_action": story.action,
        "narrative_benefit": story.benefit,
        "acceptance_criteria": story.acceptance_criteria,
        "business_rules": story.business_rules,
        "definition_of_done": story.definition_of_done,
        "technical_notes": story.technical_notes,
        "dependencies": story.dependencies,
        "assigned_to": story.assigned_to,
        "agents": story.agents,
        "estimated_hours": story.estimated_hours,
        "complexity": story.complexity,
        "risk_level": story.risk_level,
        "tags": story.tags,
        "category": story.category,
        "component": story.component,
        "source": "auto_generated"
    }


def task_to_db_dict(task: AgentTask, story_id: str, project_id: str) -> Dict[str, Any]:
    """Converte AgentTask para dicionario compativel com o modelo Task do banco"""
    return {
        "task_id": task.task_id,
        "task_type": task.task_type,
        "project_id": project_id,
        "story_id": story_id,
        "agent_id": task.agent_id,
        "title": task.title,
        "description": task.description,
        "priority": task.order,
        "payload": {
            "agent_name": task.agent_name,
            "estimated_hours": task.estimated_hours,
            "acceptance_criteria": task.acceptance_criteria
        },
        "dependencies": task.dependencies,
        "skills_required": task.skills_required,
        "status": "pending"
    }


# Exportar classes e funcoes principais
__all__ = [
    "DetailedStory",
    "AgentTask",
    "generate_story_id",
    "generate_task_id",
    "get_agents_for_category",
    "calculate_points",
    "create_tasks_for_story",
    "story_to_db_dict",
    "task_to_db_dict",
    "AGENT_SPECIALTIES",
    "CATEGORY_AGENTS",
    "DEFAULT_DOD",
    "KANBAN_STATUS"
]

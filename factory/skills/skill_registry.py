"""
Skill Registry - Registro de skills disponiveis.

Define a estrutura de uma skill e mantem o registro das skills built-in.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Callable
from enum import Enum


class SkillCategory(Enum):
    """Categorias de skills."""
    GIT = "git"
    TEST = "test"
    DOCS = "docs"
    BUILD = "build"
    DEPLOY = "deploy"
    REVIEW = "review"
    UTILITY = "utility"


@dataclass
class SkillInfo:
    """Informacoes de uma skill."""
    name: str
    description: str
    category: SkillCategory
    module: str  # Caminho do modulo Python
    function: str = "execute"  # Funcao a ser chamada
    args: List[str] = field(default_factory=list)  # Argumentos suportados
    examples: List[str] = field(default_factory=list)  # Exemplos de uso
    requires_confirmation: bool = False  # Requer confirmacao do usuario
    enabled: bool = True


# Skills built-in
BUILTIN_SKILLS: Dict[str, SkillInfo] = {
    "commit": SkillInfo(
        name="commit",
        description="Cria um commit com mensagem formatada seguindo o padrao do projeto",
        category=SkillCategory.GIT,
        module="factory.skills.builtin.commit_skill",
        args=["-m", "--message", "--amend", "--no-verify"],
        examples=[
            "/commit -m 'feat: adiciona nova feature'",
            "/commit --amend"
        ],
        requires_confirmation=False
    ),
    "test": SkillInfo(
        name="test",
        description="Executa testes do projeto com pytest",
        category=SkillCategory.TEST,
        module="factory.skills.builtin.test_skill",
        args=["--coverage", "--watch", "--file", "-k"],
        examples=[
            "/test",
            "/test --coverage",
            "/test -k test_login"
        ]
    ),
    "review-pr": SkillInfo(
        name="review-pr",
        description="Faz review de um Pull Request",
        category=SkillCategory.REVIEW,
        module="factory.skills.builtin.review_pr_skill",
        args=["--pr", "--approve", "--request-changes"],
        examples=[
            "/review-pr 123",
            "/review-pr --approve 123"
        ]
    ),
    "docs": SkillInfo(
        name="docs",
        description="Gera ou atualiza documentacao",
        category=SkillCategory.DOCS,
        module="factory.skills.builtin.docs_skill",
        args=["--type", "--file", "--output"],
        examples=[
            "/docs --type api",
            "/docs --file factory/api/routes.py"
        ]
    ),
    "lint": SkillInfo(
        name="lint",
        description="Executa linting no codigo",
        category=SkillCategory.BUILD,
        module="factory.skills.builtin.lint_skill",
        args=["--fix", "--file"],
        examples=[
            "/lint",
            "/lint --fix"
        ]
    ),
    "format": SkillInfo(
        name="format",
        description="Formata codigo com black/prettier",
        category=SkillCategory.BUILD,
        module="factory.skills.builtin.format_skill",
        args=["--check", "--file"],
        examples=[
            "/format",
            "/format --check"
        ]
    ),
    "create-pr": SkillInfo(
        name="create-pr",
        description="Cria um Pull Request",
        category=SkillCategory.GIT,
        module="factory.skills.builtin.create_pr_skill",
        args=["--title", "--body", "--base", "--draft"],
        examples=[
            "/create-pr --title 'Nova feature'",
            "/create-pr --draft"
        ],
        requires_confirmation=True
    ),
    "deploy": SkillInfo(
        name="deploy",
        description="Faz deploy para ambiente especificado",
        category=SkillCategory.DEPLOY,
        module="factory.skills.builtin.deploy_skill",
        args=["--env", "--dry-run"],
        examples=[
            "/deploy --env staging",
            "/deploy --env prod --dry-run"
        ],
        requires_confirmation=True
    ),
    "status": SkillInfo(
        name="status",
        description="Mostra status do projeto (git, testes, etc)",
        category=SkillCategory.UTILITY,
        module="factory.skills.builtin.status_skill",
        args=["--verbose"],
        examples=[
            "/status",
            "/status --verbose"
        ]
    ),
    "issue": SkillInfo(
        name="issue",
        description="Gerencia issues do GitHub",
        category=SkillCategory.UTILITY,
        module="factory.skills.builtin.issue_skill",
        args=["--create", "--close", "--list", "--label"],
        examples=[
            "/issue --list",
            "/issue --create 'Bug: erro no login'",
            "/issue --close 123"
        ]
    ),
}


def get_skill(name: str) -> Optional[SkillInfo]:
    """Retorna informacoes de uma skill pelo nome."""
    return BUILTIN_SKILLS.get(name)


def list_skills_by_category(category: SkillCategory) -> List[SkillInfo]:
    """Lista skills de uma categoria."""
    return [s for s in BUILTIN_SKILLS.values() if s.category == category]


def get_all_skill_names() -> List[str]:
    """Retorna lista de nomes de todas as skills."""
    return list(BUILTIN_SKILLS.keys())

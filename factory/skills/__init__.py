"""
Skills Package - Plataforma E
Habilidades reutilizaveis pelos agentes
"""
from .skill_manager import SkillManager
from .github_skill import GitHubSkill, get_github_skill
from .real_skills import RealSkills, AgentMemory, SkillResult, get_real_skills

__all__ = [
    "SkillManager",
    "GitHubSkill",
    "get_github_skill",
    "RealSkills",
    "AgentMemory",
    "SkillResult",
    "get_real_skills"
]

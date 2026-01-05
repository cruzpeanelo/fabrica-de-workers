# -*- coding: utf-8 -*-
"""
Seed Lookups - Plataforma E
============================

Script para popular tabelas de lookup com dados de referencia.

Uso:
    python scripts/seed_lookups.py           # Popular tudo
    python scripts/seed_lookups.py --status  # Apenas status
    python scripts/seed_lookups.py --clean   # Limpar e repopular

Author: Plataforma E
"""

import sys
import argparse
import logging
from pathlib import Path

# Adicionar diretorio raiz ao path
sys.path.insert(0, str(Path(__file__).parent.parent))

from factory.database.connection import get_session, engine
from factory.database.lookup_models import (
    StatusLookup, PriorityLookup, ComplexityLookup, StoryPointsLookup,
    TaskTypeLookup, RoleLookup, SystemConfig, AgentSkillLookup, WipLimitLookup
)
from factory.constants.lookups import (
    STORY_STATUS, TASK_STATUS, PROJECT_STATUS,
    PRIORITIES, COMPLEXITY, FIBONACCI_POINTS, FIBONACCI_LABELS,
    TASK_TYPES, ROLES, WIP_LIMITS, AGENT_SKILLS, SYSTEM_CONFIG_DEFAULTS
)

# Configurar logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s"
)
logger = logging.getLogger(__name__)


def create_tables():
    """Cria todas as tabelas de lookup se nao existirem."""
    from factory.database.lookup_models import (
        StatusLookup, PriorityLookup, ComplexityLookup, StoryPointsLookup,
        TaskTypeLookup, RoleLookup, SystemConfig, AgentSkillLookup, WipLimitLookup
    )
    from factory.database.connection import Base

    # Criar apenas as tabelas de lookup (nao todas)
    tables = [
        StatusLookup.__table__,
        PriorityLookup.__table__,
        ComplexityLookup.__table__,
        StoryPointsLookup.__table__,
        TaskTypeLookup.__table__,
        RoleLookup.__table__,
        SystemConfig.__table__,
        AgentSkillLookup.__table__,
        WipLimitLookup.__table__
    ]

    for table in tables:
        try:
            table.create(engine, checkfirst=True)
            logger.info(f"Tabela {table.name} criada/verificada")
        except Exception as e:
            logger.warning(f"Erro ao criar tabela {table.name}: {e}")


def seed_status(session, clean: bool = False):
    """Popula tabela status_lookup."""
    logger.info("Populando status_lookup...")

    if clean:
        session.query(StatusLookup).delete()

    # Status de Story
    for status in STORY_STATUS:
        existing = session.query(StatusLookup).filter(
            StatusLookup.entity_type == "story",
            StatusLookup.status_code == status["code"],
            StatusLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(StatusLookup(
                entity_type="story",
                status_code=status["code"],
                label=status["label"],
                label_en=status.get("label_en"),
                color=status.get("color"),
                sort_order=status.get("sort_order", 0),
                is_initial=status.get("is_initial", False),
                is_final=status.get("is_final", False),
                is_active=True
            ))

    # Status de Task
    for status in TASK_STATUS:
        existing = session.query(StatusLookup).filter(
            StatusLookup.entity_type == "task",
            StatusLookup.status_code == status["code"],
            StatusLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(StatusLookup(
                entity_type="task",
                status_code=status["code"],
                label=status["label"],
                label_en=status.get("label_en"),
                color=status.get("color"),
                sort_order=status.get("sort_order", 0),
                is_initial=status.get("is_initial", False),
                is_final=status.get("is_final", False),
                is_active=True
            ))

    # Status de Project
    for status in PROJECT_STATUS:
        existing = session.query(StatusLookup).filter(
            StatusLookup.entity_type == "project",
            StatusLookup.status_code == status["code"],
            StatusLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(StatusLookup(
                entity_type="project",
                status_code=status["code"],
                label=status["label"],
                label_en=status.get("label_en"),
                color=status.get("color"),
                sort_order=status.get("sort_order", 0),
                is_initial=status.get("is_initial", False),
                is_final=status.get("is_final", False),
                is_active=True
            ))

    session.commit()
    count = session.query(StatusLookup).count()
    logger.info(f"  -> {count} status inseridos")


def seed_priorities(session, clean: bool = False):
    """Popula tabela priority_lookup."""
    logger.info("Populando priority_lookup...")

    if clean:
        session.query(PriorityLookup).delete()

    for priority in PRIORITIES:
        existing = session.query(PriorityLookup).filter(
            PriorityLookup.priority_code == priority["code"],
            PriorityLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(PriorityLookup(
                priority_code=priority["code"],
                label=priority["label"],
                label_en=priority.get("label_en"),
                color=priority.get("color"),
                numeric_value=priority["numeric_value"],
                sort_order=priority.get("sort_order", 0),
                is_default=priority.get("is_default", False),
                is_active=True
            ))

    session.commit()
    count = session.query(PriorityLookup).count()
    logger.info(f"  -> {count} prioridades inseridas")


def seed_complexity(session, clean: bool = False):
    """Popula tabela complexity_lookup."""
    logger.info("Populando complexity_lookup...")

    if clean:
        session.query(ComplexityLookup).delete()

    for complexity in COMPLEXITY:
        existing = session.query(ComplexityLookup).filter(
            ComplexityLookup.complexity_code == complexity["code"],
            ComplexityLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(ComplexityLookup(
                complexity_code=complexity["code"],
                label=complexity["label"],
                label_en=complexity.get("label_en"),
                color=complexity.get("color"),
                min_points=complexity.get("min_points"),
                max_points=complexity.get("max_points"),
                estimated_hours_min=complexity.get("estimated_hours_min"),
                estimated_hours_max=complexity.get("estimated_hours_max"),
                sort_order=complexity.get("sort_order", 0),
                is_active=True
            ))

    session.commit()
    count = session.query(ComplexityLookup).count()
    logger.info(f"  -> {count} complexidades inseridas")


def seed_story_points(session, clean: bool = False):
    """Popula tabela story_points_lookup."""
    logger.info("Populando story_points_lookup...")

    if clean:
        session.query(StoryPointsLookup).delete()

    for i, points in enumerate(FIBONACCI_POINTS):
        existing = session.query(StoryPointsLookup).filter(
            StoryPointsLookup.points_value == points,
            StoryPointsLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(StoryPointsLookup(
                points_value=points,
                label=FIBONACCI_LABELS.get(points),
                sort_order=i,
                is_active=True
            ))

    session.commit()
    count = session.query(StoryPointsLookup).count()
    logger.info(f"  -> {count} story points inseridos")


def seed_task_types(session, clean: bool = False):
    """Popula tabela task_type_lookup."""
    logger.info("Populando task_type_lookup...")

    if clean:
        session.query(TaskTypeLookup).delete()

    for task_type in TASK_TYPES:
        existing = session.query(TaskTypeLookup).filter(
            TaskTypeLookup.type_code == task_type["code"],
            TaskTypeLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(TaskTypeLookup(
                type_code=task_type["code"],
                label=task_type["label"],
                label_en=task_type.get("label_en"),
                color=task_type.get("color"),
                icon=task_type.get("icon"),
                sort_order=task_type.get("sort_order", 0),
                is_default=task_type.get("is_default", False),
                is_active=True
            ))

    session.commit()
    count = session.query(TaskTypeLookup).count()
    logger.info(f"  -> {count} tipos de task inseridos")


def seed_roles(session, clean: bool = False):
    """Popula tabela role_lookup."""
    logger.info("Populando role_lookup...")

    if clean:
        session.query(RoleLookup).delete()

    import json

    for role in ROLES:
        existing = session.query(RoleLookup).filter(
            RoleLookup.role_code == role["code"],
            RoleLookup.tenant_id.is_(None)
        ).first()

        if not existing:
            permissions = role.get("permissions", [])
            if isinstance(permissions, list):
                permissions = json.dumps(permissions)

            session.add(RoleLookup(
                role_code=role["code"],
                label=role["label"],
                label_en=role.get("label_en"),
                description=role.get("description"),
                permissions=permissions,
                is_system=role.get("is_system", False),
                sort_order=role.get("sort_order", 0),
                is_active=True
            ))

    session.commit()
    count = session.query(RoleLookup).count()
    logger.info(f"  -> {count} roles inseridos")


def seed_wip_limits(session, clean: bool = False):
    """Popula tabela wip_limit_lookup."""
    logger.info("Populando wip_limit_lookup...")

    if clean:
        session.query(WipLimitLookup).delete()

    for entity_type, limits in WIP_LIMITS.items():
        for column_code, limit_value in limits.items():
            existing = session.query(WipLimitLookup).filter(
                WipLimitLookup.entity_type == entity_type,
                WipLimitLookup.column_code == column_code,
                WipLimitLookup.tenant_id.is_(None)
            ).first()

            if not existing:
                session.add(WipLimitLookup(
                    entity_type=entity_type,
                    column_code=column_code,
                    limit_value=limit_value,
                    warn_at=limit_value - 1 if limit_value > 1 else None
                ))

    session.commit()
    count = session.query(WipLimitLookup).count()
    logger.info(f"  -> {count} WIP limits inseridos")


def seed_agent_skills(session, clean: bool = False):
    """Popula tabela agent_skill_lookup."""
    logger.info("Populando agent_skill_lookup...")

    if clean:
        session.query(AgentSkillLookup).delete()

    for agent_code, keywords in AGENT_SKILLS.items():
        for i, keyword in enumerate(keywords):
            existing = session.query(AgentSkillLookup).filter(
                AgentSkillLookup.agent_code == agent_code,
                AgentSkillLookup.keyword == keyword
            ).first()

            if not existing:
                session.add(AgentSkillLookup(
                    agent_code=agent_code,
                    keyword=keyword,
                    priority=len(keywords) - i,  # Primeiros tem maior prioridade
                    is_active=True
                ))

    session.commit()
    count = session.query(AgentSkillLookup).count()
    logger.info(f"  -> {count} agent skills inseridos")


def seed_system_config(session, clean: bool = False):
    """Popula tabela system_config."""
    logger.info("Populando system_config...")

    if clean:
        session.query(SystemConfig).delete()

    for config_key, config_def in SYSTEM_CONFIG_DEFAULTS.items():
        existing = session.query(SystemConfig).filter(
            SystemConfig.config_key == config_key,
            SystemConfig.tenant_id.is_(None)
        ).first()

        if not existing:
            session.add(SystemConfig(
                config_key=config_key,
                config_value=config_def.get("value"),
                data_type=config_def.get("data_type", "string"),
                description=config_def.get("description"),
                category=config_def.get("category"),
                is_public=False,
                is_readonly=False
            ))

    session.commit()
    count = session.query(SystemConfig).count()
    logger.info(f"  -> {count} configuracoes inseridas")


def seed_all(clean: bool = False):
    """Popula todas as tabelas de lookup."""
    logger.info("=" * 60)
    logger.info("SEED LOOKUPS - Plataforma E")
    logger.info("=" * 60)

    # Criar tabelas primeiro
    create_tables()

    with get_session() as session:
        seed_status(session, clean)
        seed_priorities(session, clean)
        seed_complexity(session, clean)
        seed_story_points(session, clean)
        seed_task_types(session, clean)
        seed_roles(session, clean)
        seed_wip_limits(session, clean)
        seed_agent_skills(session, clean)
        seed_system_config(session, clean)

    logger.info("=" * 60)
    logger.info("Seed concluido com sucesso!")
    logger.info("=" * 60)


def main():
    """Ponto de entrada principal."""
    parser = argparse.ArgumentParser(
        description="Popular tabelas de lookup da Plataforma E"
    )
    parser.add_argument(
        "--clean",
        action="store_true",
        help="Limpar tabelas antes de popular"
    )
    parser.add_argument(
        "--status",
        action="store_true",
        help="Popular apenas status"
    )
    parser.add_argument(
        "--priorities",
        action="store_true",
        help="Popular apenas prioridades"
    )
    parser.add_argument(
        "--complexity",
        action="store_true",
        help="Popular apenas complexidade"
    )
    parser.add_argument(
        "--story-points",
        action="store_true",
        help="Popular apenas story points"
    )
    parser.add_argument(
        "--task-types",
        action="store_true",
        help="Popular apenas tipos de task"
    )
    parser.add_argument(
        "--roles",
        action="store_true",
        help="Popular apenas roles"
    )
    parser.add_argument(
        "--wip-limits",
        action="store_true",
        help="Popular apenas WIP limits"
    )
    parser.add_argument(
        "--agent-skills",
        action="store_true",
        help="Popular apenas agent skills"
    )
    parser.add_argument(
        "--config",
        action="store_true",
        help="Popular apenas configuracoes"
    )

    args = parser.parse_args()

    # Se nenhum argumento especifico, popular tudo
    any_specific = any([
        args.status, args.priorities, args.complexity,
        args.story_points, args.task_types, args.roles,
        args.wip_limits, args.agent_skills, args.config
    ])

    if not any_specific:
        seed_all(args.clean)
        return

    # Criar tabelas primeiro
    create_tables()

    # Popular apenas os selecionados
    with get_session() as session:
        if args.status:
            seed_status(session, args.clean)
        if args.priorities:
            seed_priorities(session, args.clean)
        if args.complexity:
            seed_complexity(session, args.clean)
        if args.story_points:
            seed_story_points(session, args.clean)
        if args.task_types:
            seed_task_types(session, args.clean)
        if args.roles:
            seed_roles(session, args.clean)
        if args.wip_limits:
            seed_wip_limits(session, args.clean)
        if args.agent_skills:
            seed_agent_skills(session, args.clean)
        if args.config:
            seed_system_config(session, args.clean)

    logger.info("Seed seletivo concluido!")


if __name__ == "__main__":
    main()

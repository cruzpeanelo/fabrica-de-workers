"""
Script para Limpar Dados Sensiveis do Banco de Dados
=====================================================

Execute este script antes de demonstracoes ou compartilhamento
para garantir que nenhum dado de cliente esteja visivel.

Uso:
    python factory/database/clean_sensitive_data.py
"""

import sys
import os

# Adiciona o diretorio raiz ao path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.database.connection import SessionLocal
from factory.database.models import Project, Story, Agent
import re


def clean_text(text: str, replacements: dict) -> str:
    """Substitui termos sensiveis por genericos."""
    if not text:
        return text

    result = text
    for old, new in replacements.items():
        # Case insensitive replacement
        pattern = re.compile(re.escape(old), re.IGNORECASE)
        result = pattern.sub(new, result)

    return result


def clean_projects(db):
    """Limpa nomes e descricoes de projetos."""

    # Mapeamento de termos sensiveis para genericos
    replacements = {
        "Belgo": "Cliente",
        "Belgo Bekaert": "Cliente Corp",
        "Belgo Arames": "Cliente SA",
        "Belgo BPM": "BPM Platform",
        "belgo": "cliente",
        "belgo.com.br": "empresa.com.br",
        "belgo.com": "empresa.com",
    }

    projects = db.query(Project).all()
    updated = 0

    for project in projects:
        original_name = project.name
        original_desc = project.description

        project.name = clean_text(project.name, replacements)
        project.description = clean_text(project.description, replacements)

        if project.name != original_name or project.description != original_desc:
            updated += 1
            print(f"  Projeto atualizado: {original_name} -> {project.name}")

    db.commit()
    return updated


def clean_stories(db):
    """Limpa titulos e descricoes de stories."""

    replacements = {
        "Belgo": "Cliente",
        "Belgo Bekaert": "Cliente Corp",
        "Belgo Arames": "Cliente SA",
        "belgo": "cliente",
    }

    stories = db.query(Story).all()
    updated = 0

    for story in stories:
        original_title = story.title
        original_desc = story.description

        story.title = clean_text(story.title, replacements)
        story.description = clean_text(story.description, replacements)

        if story.title != original_title or story.description != original_desc:
            updated += 1

    db.commit()
    return updated


def clean_agent_configs(db):
    """Limpa configuracoes dos agentes que possam ter dados sensiveis."""

    # Agentes geralmente nao tem dados sensiveis, mas verificamos
    agents = db.query(Agent).all()
    updated = 0

    replacements = {
        "Belgo": "Cliente",
        "belgo": "cliente",
    }

    for agent in agents:
        if agent.description:
            original = agent.description
            agent.description = clean_text(agent.description, replacements)
            if agent.description != original:
                updated += 1

    db.commit()
    return updated


def main():
    """Executa limpeza completa do banco de dados."""

    print("=" * 60)
    print("LIMPEZA DE DADOS SENSIVEIS")
    print("=" * 60)
    print()

    db = SessionLocal()

    try:
        print("1. Limpando projetos...")
        projects_updated = clean_projects(db)
        print(f"   {projects_updated} projeto(s) atualizado(s)")
        print()

        print("2. Limpando stories...")
        stories_updated = clean_stories(db)
        print(f"   {stories_updated} story(ies) atualizada(s)")
        print()

        print("3. Limpando agentes...")
        agents_updated = clean_agent_configs(db)
        print(f"   {agents_updated} agente(s) atualizado(s)")
        print()

        print("=" * 60)
        print("LIMPEZA CONCLUIDA COM SUCESSO!")
        print("=" * 60)
        print()
        print(f"Total de registros atualizados: {projects_updated + stories_updated + agents_updated}")

    except Exception as e:
        print(f"ERRO: {e}")
        db.rollback()
        return 1
    finally:
        db.close()

    return 0


if __name__ == "__main__":
    exit(main())

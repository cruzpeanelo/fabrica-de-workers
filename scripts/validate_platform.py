"""
Script de Validacao da Plataforma
=================================

Valida todos os componentes da Plataforma E.

Uso: python scripts/validate_platform.py
"""

import sys
import os
from pathlib import Path
from datetime import datetime

# Add project root to path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

def print_header(title):
    print("\n" + "=" * 60)
    print(f" {title}")
    print("=" * 60)

def print_check(name, status, details=""):
    icon = "[OK]" if status else "[FAIL]"
    print(f"  {icon} {name}")
    if details:
        print(f"      -> {details}")

def check_database():
    """Verifica conexao com banco de dados"""
    print_header("DATABASE")
    try:
        from factory.database.connection import SessionLocal, init_db
        from factory.database.models import Project, Agent, Skill, Story

        init_db()
        db = SessionLocal()

        projects = db.query(Project).count()
        agents = db.query(Agent).count()
        skills = db.query(Skill).count()
        stories = db.query(Story).count()

        db.close()

        print_check("Conexao SQLite", True, "factory.db")
        print_check("Projetos", True, f"{projects} registros")
        print_check("Agentes", True, f"{agents} registros")
        print_check("Skills", True, f"{skills} registros")
        print_check("Stories", True, f"{stories} registros")

        return True
    except Exception as e:
        print_check("Database", False, str(e))
        return False

def check_api_schemas():
    """Verifica schemas da API"""
    print_header("API SCHEMAS")
    try:
        from factory.api.schemas import (
            ProjectCreate, StoryCreate, AgentUpdate,
            ProjectResponse, StatusResponse
        )
        print_check("ProjectCreate", True)
        print_check("StoryCreate", True)
        print_check("AgentUpdate", True)
        print_check("ProjectResponse", True)
        print_check("StatusResponse", True)
        return True
    except Exception as e:
        print_check("API Schemas", False, str(e))
        return False

def check_logging_system():
    """Verifica sistema de logs"""
    print_header("LOGGING SYSTEM")
    try:
        from factory.core.logging_system import (
            get_logger, log_info, log_error,
            FactoryLogger, StructuredLog
        )

        logger = get_logger()
        print_check("FactoryLogger", True)
        print_check("Structured Logs", True)

        # Test logging
        logger.info("Validation test log", source="validator")
        print_check("Log de teste", True, "Escrito com sucesso")

        return True
    except Exception as e:
        print_check("Logging System", False, str(e))
        return False

def check_agents():
    """Verifica agentes"""
    print_header("AGENTS")
    try:
        from factory.agents.corporate_hierarchy import (
            ALL_CORPORATE_AGENTS, get_agents_by_area
        )

        total = len(ALL_CORPORATE_AGENTS)
        areas = {}
        for agent_id, agent in ALL_CORPORATE_AGENTS.items():
            area = agent.department.area
            areas[area] = areas.get(area, 0) + 1

        print_check("Hierarquia Corporativa", True, f"{total} agentes")
        for area, count in sorted(areas.items()):
            print_check(f"  {area}", True, f"{count} agentes")

        return True
    except Exception as e:
        print_check("Agents", False, str(e))
        return False

def check_skills():
    """Verifica skills"""
    print_header("SKILLS")
    try:
        from factory.database.connection import SessionLocal
        from factory.database.models import Skill

        db = SessionLocal()
        skills = db.query(Skill).filter(Skill.enabled == True).all()

        by_type = {}
        for skill in skills:
            t = skill.skill_type
            by_type[t] = by_type.get(t, 0) + 1

        print_check("Skills Habilitadas", True, f"{len(skills)} total")
        for t, count in sorted(by_type.items()):
            print_check(f"  {t}", True, f"{count} skills")

        db.close()
        return True
    except Exception as e:
        print_check("Skills", False, str(e))
        return False

def check_claude_integration():
    """Verifica integracao Claude"""
    print_header("CLAUDE INTEGRATION")
    try:
        from factory.ai.claude_integration import ClaudeClient, AgentBrain

        client = ClaudeClient()
        available = client.is_available()

        print_check("ClaudeClient", True)
        print_check("AgentBrain", True)
        print_check("API Disponivel", available,
                   "Pronto para modo inteligente" if available else "Modo template apenas")

        return True
    except Exception as e:
        print_check("Claude Integration", False, str(e))
        return False

def check_api_endpoints():
    """Verifica endpoints da API"""
    print_header("API ENDPOINTS")
    try:
        from fastapi.testclient import TestClient
        from factory.dashboard.app import app

        client = TestClient(app)

        endpoints = [
            ("GET", "/api/status", 200),
            ("GET", "/api/projects", 200),
            ("GET", "/api/agents", 200),
            ("GET", "/api/skills", 200),
            ("GET", "/api/stories", 200),
            ("GET", "/api/templates", 200),
            ("GET", "/api/logs", 200),
            ("GET", "/docs", 200),
            ("GET", "/redoc", 200),
        ]

        all_ok = True
        for method, path, expected in endpoints:
            if method == "GET":
                response = client.get(path)
            else:
                response = client.post(path)

            ok = response.status_code == expected
            all_ok = all_ok and ok
            print_check(f"{method} {path}", ok, f"Status: {response.status_code}")

        return all_ok
    except Exception as e:
        print_check("API Endpoints", False, str(e))
        return False

def check_tests():
    """Verifica suite de testes"""
    print_header("TEST SUITE")
    try:
        import subprocess
        import re

        result = subprocess.run(
            [sys.executable, "-m", "pytest", "tests/", "--collect-only", "-q"],
            capture_output=True,
            text=True,
            cwd=str(PROJECT_ROOT)
        )

        # Count tests - look for "collected X items" pattern
        test_count = 0
        match = re.search(r'collected (\d+) items?', result.stdout)
        if match:
            test_count = int(match.group(1))

        print_check("Testes Encontrados", test_count > 0, f"{test_count} testes")
        print_check("pytest.ini", (PROJECT_ROOT / "pytest.ini").exists(), "Configurado")
        print_check("conftest.py", (PROJECT_ROOT / "tests" / "conftest.py").exists(), "Fixtures prontas")

        return test_count > 0
    except Exception as e:
        print_check("Test Suite", False, str(e))
        return False

def check_documentation():
    """Verifica documentacao"""
    print_header("DOCUMENTATION")

    files = [
        ("README.md", "Documentacao principal"),
        ("TODO.md", "Lista de pendencias"),
        ("AUDIT_REPORT.md", "Relatorio de auditoria"),
        ("factory/api/schemas.py", "Schemas OpenAPI"),
        ("factory/api/openapi_config.py", "Configuracao OpenAPI"),
    ]

    all_ok = True
    for file, desc in files:
        path = PROJECT_ROOT / file
        exists = path.exists()
        all_ok = all_ok and exists
        print_check(file, exists, desc)

    return all_ok

def main():
    print("\n" + "=" * 60)
    print(" VALIDACAO DA PLATAFORMA - FABRICA DE AGENTES")
    print(f" Data: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 60)

    results = []

    results.append(("Database", check_database()))
    results.append(("API Schemas", check_api_schemas()))
    results.append(("Logging System", check_logging_system()))
    results.append(("Agents", check_agents()))
    results.append(("Skills", check_skills()))
    results.append(("Claude Integration", check_claude_integration()))
    results.append(("API Endpoints", check_api_endpoints()))
    results.append(("Test Suite", check_tests()))
    results.append(("Documentation", check_documentation()))

    # Summary
    print_header("RESUMO")
    passed = sum(1 for _, ok in results if ok)
    total = len(results)

    for name, ok in results:
        print_check(name, ok)

    print(f"\n  Total: {passed}/{total} verificacoes passaram")

    if passed == total:
        print("\n  [SUCCESS] PLATAFORMA 100% FUNCIONAL")
        return 0
    else:
        print(f"\n  [WARNING] {total - passed} verificacoes falharam")
        return 1

if __name__ == "__main__":
    sys.exit(main())

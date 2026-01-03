# -*- coding: utf-8 -*-
"""
Testing Routes - API REST para controle do sistema de testes.

Endpoints:
- POST /api/testing/smoke           - Executar smoke tests
- POST /api/testing/critical        - Executar critical path tests
- POST /api/testing/regression      - Executar full regression
- POST /api/testing/persona/{type}  - Testar como persona
- POST /api/testing/screen/{name}   - Testar tela especifica
- POST /api/testing/whitelabel      - Testar white-label
- GET  /api/testing/status          - Status atual dos testes
- GET  /api/testing/report          - Relatorio completo
- GET  /api/testing/screens         - Listar telas disponiveis
- GET  /api/testing/personas        - Listar personas disponiveis
"""

from flask import Blueprint, request, jsonify
import sys
import os
import asyncio
from datetime import datetime
from pathlib import Path

# Adicionar path do projeto
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

testing_bp = Blueprint('testing', __name__, url_prefix='/api/testing')

# Estado global dos testes
_test_state = {
    "current_suite": None,
    "last_report": None,
    "running": False,
    "started_at": None
}


def run_async(coro):
    """Helper para executar coroutines em contexto sincrono."""
    try:
        loop = asyncio.get_event_loop()
    except RuntimeError:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
    return loop.run_until_complete(coro)


@testing_bp.route('/smoke', methods=['POST'])
def run_smoke_tests():
    """
    Executa smoke tests (< 5 min).

    Testa funcionalidades basicas:
    - Login funciona
    - Dashboard carrega
    - API responde
    """
    global _test_state

    if _test_state["running"]:
        return jsonify({
            "success": False,
            "error": "Testes ja estao em execucao",
            "current_suite": _test_state["current_suite"]
        }), 409

    try:
        from factory.testing import get_regression_runner, get_qa_bridge

        _test_state["running"] = True
        _test_state["current_suite"] = "smoke"
        _test_state["started_at"] = datetime.utcnow().isoformat()

        # Notificar bridge
        bridge = get_qa_bridge()
        bridge.notify_test_start("Smoke Tests", "smoke")

        # Executar testes
        runner = get_regression_runner()
        report = run_async(runner.run_smoke_tests())
        runner.current_report = report

        # Notificar conclusao
        bridge.notify_test_complete(report.to_dict())

        _test_state["running"] = False
        _test_state["last_report"] = report.to_dict()

        return jsonify({
            "success": True,
            "report": report.to_dict()
        })

    except Exception as e:
        _test_state["running"] = False
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/critical', methods=['POST'])
def run_critical_path():
    """
    Executa critical path tests (< 15 min).

    Testa caminhos criticos:
    - CRUD de stories
    - Kanban drag-drop
    - Sprint management
    """
    global _test_state

    if _test_state["running"]:
        return jsonify({
            "success": False,
            "error": "Testes ja estao em execucao"
        }), 409

    try:
        from factory.testing import get_regression_runner, get_qa_bridge

        _test_state["running"] = True
        _test_state["current_suite"] = "critical_path"
        _test_state["started_at"] = datetime.utcnow().isoformat()

        bridge = get_qa_bridge()
        bridge.notify_test_start("Critical Path Tests", "critical_path")

        runner = get_regression_runner()
        report = run_async(runner.run_critical_path())
        runner.current_report = report

        bridge.notify_test_complete(report.to_dict())

        _test_state["running"] = False
        _test_state["last_report"] = report.to_dict()

        return jsonify({
            "success": True,
            "report": report.to_dict()
        })

    except Exception as e:
        _test_state["running"] = False
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/regression', methods=['POST'])
def run_full_regression():
    """
    Executa suite completa de regressao (< 1 hora).

    Testa toda a plataforma:
    - Todas as telas
    - Todas as personas
    - Todos os fluxos
    - Integracoes
    """
    global _test_state

    if _test_state["running"]:
        return jsonify({
            "success": False,
            "error": "Testes ja estao em execucao"
        }), 409

    try:
        from factory.testing import get_regression_runner, get_qa_bridge, get_issue_generator

        _test_state["running"] = True
        _test_state["current_suite"] = "full_regression"
        _test_state["started_at"] = datetime.utcnow().isoformat()

        bridge = get_qa_bridge()
        bridge.notify_test_start("Full Regression Tests", "full_regression")

        runner = get_regression_runner()
        report = run_async(runner.run_full_regression())
        runner.current_report = report

        bridge.notify_test_complete(report.to_dict())

        # Gerar issues para falhas
        issues_created = []
        if report.failures:
            generator = get_issue_generator()
            for failure_data in runner.get_issues_for_creation():
                from factory.testing.issue_generator import TestFailure, ErrorType
                failure = TestFailure(
                    test_id=failure_data.get("test_id", ""),
                    test_name=failure_data.get("test_name", ""),
                    suite=failure_data.get("suite", ""),
                    screen=failure_data.get("screen"),
                    persona=failure_data.get("persona"),
                    error_type=ErrorType.UNKNOWN,
                    error_message=failure_data.get("error_message", ""),
                    steps_to_reproduce=failure_data.get("steps_to_reproduce", [])
                )
                # issue = generator.create_issue_from_failure(failure)
                # if issue:
                #     issues_created.append(issue.to_dict())

        _test_state["running"] = False
        _test_state["last_report"] = report.to_dict()

        return jsonify({
            "success": True,
            "report": report.to_dict(),
            "issues_created": issues_created
        })

    except Exception as e:
        _test_state["running"] = False
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/persona/<persona_type>', methods=['POST'])
def run_persona_tests(persona_type: str):
    """
    Executa testes para uma persona especifica.

    Args:
        persona_type: Tipo da persona (ADMIN, DEVELOPER, etc)
    """
    global _test_state

    if _test_state["running"]:
        return jsonify({
            "success": False,
            "error": "Testes ja estao em execucao"
        }), 409

    try:
        from factory.testing import get_regression_runner, get_qa_bridge

        _test_state["running"] = True
        _test_state["current_suite"] = f"persona_{persona_type}"
        _test_state["started_at"] = datetime.utcnow().isoformat()

        bridge = get_qa_bridge()
        bridge.notify_test_start(f"Persona Tests - {persona_type}", "persona")

        runner = get_regression_runner()
        report = run_async(runner.run_persona_tests(persona_type.upper()))
        runner.current_report = report

        bridge.notify_test_complete(report.to_dict())

        _test_state["running"] = False
        _test_state["last_report"] = report.to_dict()

        return jsonify({
            "success": True,
            "report": report.to_dict()
        })

    except Exception as e:
        _test_state["running"] = False
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/screen/<screen_name>', methods=['POST'])
def run_screen_tests(screen_name: str):
    """
    Executa testes para uma tela especifica.

    Args:
        screen_name: Nome da tela (login, dashboard, kanban, etc)
    """
    global _test_state

    if _test_state["running"]:
        return jsonify({
            "success": False,
            "error": "Testes ja estao em execucao"
        }), 409

    try:
        from factory.testing import get_regression_runner, get_screen_registry

        # Verificar se tela existe
        registry = get_screen_registry()
        screen = registry.get_screen(screen_name)
        if not screen:
            return jsonify({
                "success": False,
                "error": f"Tela '{screen_name}' nao encontrada",
                "available_screens": registry.get_all_screens()
            }), 404

        _test_state["running"] = True
        _test_state["current_suite"] = f"screen_{screen_name}"
        _test_state["started_at"] = datetime.utcnow().isoformat()

        runner = get_regression_runner()
        report = run_async(runner.run_screen_tests(screen_name))
        runner.current_report = report

        _test_state["running"] = False
        _test_state["last_report"] = report.to_dict()

        return jsonify({
            "success": True,
            "report": report.to_dict()
        })

    except Exception as e:
        _test_state["running"] = False
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/whitelabel', methods=['POST'])
def run_whitelabel_tests():
    """
    Executa testes de white-label/branding.

    Testa:
    - Upload de logo
    - Esquema de cores
    - Dominio customizado
    - Templates de email
    - Branding em PDFs
    """
    global _test_state

    if _test_state["running"]:
        return jsonify({
            "success": False,
            "error": "Testes ja estao em execucao"
        }), 409

    try:
        from factory.testing import get_whitelabel_suite
        from factory.testing.whitelabel_tests import TEST_BRANDING_CONFIGS

        _test_state["running"] = True
        _test_state["current_suite"] = "whitelabel"
        _test_state["started_at"] = datetime.utcnow().isoformat()

        suite = get_whitelabel_suite()

        # Pegar tenant do request ou usar primeiro padrao
        data = request.get_json() or {}
        tenant_id = data.get("tenant_id")

        if tenant_id:
            # Encontrar config do tenant
            config = next((c for c in TEST_BRANDING_CONFIGS if c.tenant_id == tenant_id), None)
            if not config:
                return jsonify({
                    "success": False,
                    "error": f"Tenant '{tenant_id}' nao encontrado"
                }), 404
            report = run_async(suite.run_full_branding_suite(config))
            reports = {tenant_id: report.to_dict()}
        else:
            # Testar todos os tenants
            all_reports = run_async(suite.run_all_tenants())
            reports = {tid: r.to_dict() for tid, r in all_reports.items()}

        _test_state["running"] = False
        _test_state["last_report"] = {"whitelabel_reports": reports}

        return jsonify({
            "success": True,
            "reports": reports
        })

    except Exception as e:
        _test_state["running"] = False
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/status', methods=['GET'])
def get_test_status():
    """Retorna status atual dos testes."""
    return jsonify({
        "success": True,
        "data": {
            "running": _test_state["running"],
            "current_suite": _test_state["current_suite"],
            "started_at": _test_state["started_at"],
            "has_report": _test_state["last_report"] is not None
        }
    })


@testing_bp.route('/report', methods=['GET'])
def get_last_report():
    """Retorna ultimo relatorio de testes."""
    if not _test_state["last_report"]:
        return jsonify({
            "success": False,
            "error": "Nenhum relatorio disponivel. Execute testes primeiro."
        }), 404

    return jsonify({
        "success": True,
        "data": _test_state["last_report"]
    })


@testing_bp.route('/screens', methods=['GET'])
def list_screens():
    """Lista todas as telas disponiveis para teste."""
    try:
        from factory.testing import get_screen_registry

        registry = get_screen_registry()
        screens = []

        for name in registry.get_all_screens():
            info = registry.get_screen(name)
            screens.append({
                "name": name,
                "title": info.title,
                "url": info.url,
                "personas": info.personas,
                "test_cases_count": len(info.test_cases)
            })

        return jsonify({
            "success": True,
            "data": screens,
            "total": len(screens),
            "critical_screens": registry.get_critical_screens()
        })

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/personas', methods=['GET'])
def list_personas():
    """Lista todas as personas disponiveis para teste."""
    try:
        from factory.testing import get_persona_fixtures

        fixtures = get_persona_fixtures()
        personas = []

        for persona_type, user in fixtures.get_all_users().items():
            personas.append({
                "type": persona_type,
                "username": user.username,
                "role": user.role,
                "dashboard_type": user.dashboard_type,
                "expected_screens": fixtures.get_expected_screens(persona_type)
            })

        return jsonify({
            "success": True,
            "data": personas,
            "total": len(personas)
        })

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/data/seed', methods=['POST'])
def seed_test_data():
    """
    Popula banco com dados de teste.

    Body:
        {
            "scenario": "standard"  // empty, minimal, standard, enterprise, regression
        }
    """
    try:
        from factory.testing import get_data_factory

        data = request.get_json() or {}
        scenario = data.get("scenario", "standard")

        factory = get_data_factory()
        result = factory.seed_database(scenario)

        return jsonify({
            "success": True,
            "data": {
                "scenario": result["scenario"],
                "description": result["description"],
                "created": {
                    "tenants": len(result["tenants"]),
                    "users": len(result["users"]),
                    "projects": len(result["projects"]),
                    "stories": len(result["stories"]),
                    "integrations": len(result["integrations"])
                }
            }
        })

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/issues/pending', methods=['GET'])
def get_pending_issues():
    """Retorna issues pendentes de criacao."""
    try:
        from factory.testing import get_regression_runner

        runner = get_regression_runner()
        issues = runner.get_issues_for_creation()

        return jsonify({
            "success": True,
            "data": issues,
            "total": len(issues)
        })

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/issues/create', methods=['POST'])
def create_issues():
    """
    Cria issues no GitHub para falhas pendentes.

    Body:
        {
            "failures": [...]  // Lista opcional de falhas especificas
        }
    """
    try:
        from factory.testing import get_regression_runner, get_issue_generator
        from factory.testing.issue_generator import TestFailure, ErrorType

        data = request.get_json() or {}

        runner = get_regression_runner()
        generator = get_issue_generator()

        # Usar falhas do request ou do runner
        failures_data = data.get("failures") or runner.get_issues_for_creation()

        created = []
        for f in failures_data:
            failure = TestFailure(
                test_id=f.get("test_id", ""),
                test_name=f.get("test_name", ""),
                suite=f.get("suite", ""),
                screen=f.get("screen"),
                persona=f.get("persona"),
                error_type=ErrorType(f.get("error_type", "unknown")),
                error_message=f.get("error_message", ""),
                steps_to_reproduce=f.get("steps_to_reproduce", [])
            )
            issue = generator.create_issue_from_failure(failure)
            if issue:
                created.append(issue.to_dict())

        return jsonify({
            "success": True,
            "created": created,
            "total": len(created)
        })

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


@testing_bp.route('/qa/report', methods=['GET'])
def get_qa_report():
    """Retorna relatorio completo do QA."""
    try:
        from factory.testing import get_qa_bridge

        bridge = get_qa_bridge()
        report = bridge.generate_qa_report()

        return jsonify({
            "success": True,
            "data": report
        })

    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500


def register_testing_routes(app):
    """Registra as rotas de testing no app Flask."""
    app.register_blueprint(testing_bp)
    print("[Testing] Routes registered at /api/testing")

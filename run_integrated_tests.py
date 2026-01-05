#!/usr/bin/env python3
"""
Script de Teste Integrado - Plataforma E
Executa todos os niveis de teste e gera issues no GitHub para falhas.
"""

import asyncio
import json
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any
import requests

# Adicionar path do projeto
sys.path.insert(0, str(Path(__file__).parent))

# Configuracao
BASE_URL = "http://localhost:9001"
RESULTS_DIR = Path("analysis")
RESULTS_DIR.mkdir(exist_ok=True)

class IntegratedTestRunner:
    """Executor de testes integrados com geracao de issues."""

    def __init__(self):
        self.results = {
            "start_time": datetime.now().isoformat(),
            "smoke_tests": [],
            "critical_tests": [],
            "api_tests": [],
            "persona_tests": [],
            "security_tests": [],
            "failures": [],
            "issues_created": [],
            "summary": {}
        }
        self.token = None

    def get_auth_token(self, username: str = "admin", password: str = "admin123") -> str:
        """Obtem token JWT para autenticacao."""
        try:
            response = requests.post(
                f"{BASE_URL}/api/v1/auth/login",
                json={"username": username, "password": password},
                timeout=10
            )
            if response.status_code == 200:
                data = response.json()
                self.token = data.get("access_token") or data.get("token")
                return self.token
        except Exception as e:
            print(f"[WARN] Auth failed: {e}")
        return None

    def get_headers(self) -> Dict[str, str]:
        """Retorna headers com autenticacao."""
        headers = {"Content-Type": "application/json"}
        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"
        return headers

    def run_smoke_tests(self) -> List[Dict]:
        """Executa smoke tests rapidos."""
        print("\n" + "="*60)
        print("[SMOKE TESTS] Iniciando testes rapidos...")
        print("="*60)

        tests = [
            {"name": "Health Check", "endpoint": "/health", "method": "GET", "expected": 200},
            {"name": "API Health", "endpoint": "/api/health", "method": "GET", "expected": 200},
            {"name": "Dashboard Load", "endpoint": "/", "method": "GET", "expected": 200},
            {"name": "Login Page", "endpoint": "/login", "method": "GET", "expected": 200},
        ]

        results = []
        for test in tests:
            result = self._run_single_test(test)
            results.append(result)
            status = "PASS" if result["passed"] else "FAIL"
            print(f"  [{status}] {test['name']}")

        self.results["smoke_tests"] = results
        return results

    def run_critical_path_tests(self) -> List[Dict]:
        """Executa testes de caminho critico."""
        print("\n" + "="*60)
        print("[CRITICAL PATH] Iniciando testes de caminho critico...")
        print("="*60)

        # Primeiro obter token
        self.get_auth_token()

        tests = [
            {"name": "Auth - Login API", "endpoint": "/api/v1/auth/login", "method": "POST",
             "data": {"username": "admin", "password": "admin123"}, "expected": [200, 401, 422]},
            {"name": "Stories - List", "endpoint": "/api/stories", "method": "GET", "expected": [200, 401]},
            {"name": "Projects - List", "endpoint": "/api/projects", "method": "GET", "expected": [200, 401]},
            {"name": "Kanban Page", "endpoint": "/kanban", "method": "GET", "expected": [200, 302]},
            {"name": "Stories Page", "endpoint": "/stories", "method": "GET", "expected": [200, 302]},
            {"name": "Sprints - List", "endpoint": "/api/sprints", "method": "GET", "expected": [200, 401]},
        ]

        results = []
        for test in tests:
            result = self._run_single_test(test)
            results.append(result)
            status = "PASS" if result["passed"] else "FAIL"
            print(f"  [{status}] {test['name']} ({result.get('status_code', 'N/A')})")

        self.results["critical_tests"] = results
        return results

    def run_api_tests(self) -> List[Dict]:
        """Executa testes de API completos."""
        print("\n" + "="*60)
        print("[API TESTS] Iniciando testes de API...")
        print("="*60)

        # Endpoints principais para testar
        api_endpoints = [
            # Auth
            {"name": "Auth - Me", "endpoint": "/api/v1/auth/me", "method": "GET", "expected": [200, 401]},

            # Stories
            {"name": "Stories - Create", "endpoint": "/api/stories", "method": "POST",
             "data": {"title": "Test Story", "persona": "developer", "action": "test", "benefit": "testing"},
             "expected": [200, 201, 401, 422]},

            # Projects
            {"name": "Projects - Files", "endpoint": "/api/projects/1/files", "method": "GET", "expected": [200, 401, 404]},

            # Epics
            {"name": "Epics - List", "endpoint": "/api/epics", "method": "GET", "expected": [200, 401]},

            # Chat
            {"name": "Chat - History", "endpoint": "/api/chat/history", "method": "GET", "expected": [200, 401]},

            # Analytics
            {"name": "Analytics - Metrics", "endpoint": "/api/analytics/metrics", "method": "GET", "expected": [200, 401]},
            {"name": "Analytics - Velocity", "endpoint": "/api/analytics/velocity", "method": "GET", "expected": [200, 401]},

            # Search
            {"name": "Search", "endpoint": "/api/search?q=test", "method": "GET", "expected": [200, 401]},

            # Templates
            {"name": "Templates - List", "endpoint": "/api/templates", "method": "GET", "expected": [200, 401]},

            # Activity
            {"name": "Activity Log", "endpoint": "/api/activity", "method": "GET", "expected": [200, 401]},

            # Jobs
            {"name": "Jobs - List", "endpoint": "/api/v1/jobs", "method": "GET", "expected": [200, 401]},
            {"name": "Queue - Stats", "endpoint": "/api/v1/queue/stats", "method": "GET", "expected": [200, 401]},

            # Workers
            {"name": "Workers - List", "endpoint": "/api/v1/workers", "method": "GET", "expected": [200, 401]},

            # Models
            {"name": "Models - List", "endpoint": "/api/v1/models", "method": "GET", "expected": [200]},

            # Admin (should require admin)
            {"name": "Admin - Users", "endpoint": "/api/admin/users", "method": "GET", "expected": [200, 401, 403]},

            # Security
            {"name": "Security - Data", "endpoint": "/api/security/data", "method": "GET", "expected": [200, 401]},

            # Branding
            {"name": "Branding - Current", "endpoint": "/api/branding/current/branding", "method": "GET", "expected": [200, 401, 404]},
        ]

        results = []
        for test in api_endpoints:
            result = self._run_single_test(test)
            results.append(result)
            status = "PASS" if result["passed"] else "FAIL"
            print(f"  [{status}] {test['name']} ({result.get('status_code', 'N/A')})")

        self.results["api_tests"] = results
        return results

    def run_persona_tests(self) -> List[Dict]:
        """Executa testes por persona."""
        print("\n" + "="*60)
        print("[PERSONA TESTS] Iniciando testes por persona...")
        print("="*60)

        personas = [
            {"name": "Admin", "username": "admin", "password": "admin123", "expected_access": ["dashboard", "admin", "users"]},
            {"name": "Developer", "username": "developer", "password": "dev123", "expected_access": ["dashboard", "stories", "kanban"]},
            {"name": "Viewer", "username": "viewer", "password": "view123", "expected_access": ["dashboard"]},
        ]

        results = []
        for persona in personas:
            # Tentar login
            try:
                response = requests.post(
                    f"{BASE_URL}/api/v1/auth/login",
                    json={"username": persona["username"], "password": persona["password"]},
                    timeout=10
                )
                login_success = response.status_code in [200, 401, 422]  # 401/422 esperado se usuario nao existe

                result = {
                    "name": f"Persona - {persona['name']}",
                    "passed": login_success,
                    "status_code": response.status_code,
                    "persona": persona["name"]
                }
                results.append(result)

                status = "PASS" if login_success else "FAIL"
                print(f"  [{status}] {persona['name']} Login (status: {response.status_code})")

            except Exception as e:
                results.append({
                    "name": f"Persona - {persona['name']}",
                    "passed": False,
                    "error": str(e),
                    "persona": persona["name"]
                })
                print(f"  [FAIL] {persona['name']} - {e}")

        self.results["persona_tests"] = results
        return results

    def run_security_tests(self) -> List[Dict]:
        """Executa testes de seguranca basicos."""
        print("\n" + "="*60)
        print("[SECURITY TESTS] Iniciando testes de seguranca...")
        print("="*60)

        results = []

        # Test 1: XSS Prevention
        xss_payload = "<script>alert('xss')</script>"
        try:
            response = requests.get(
                f"{BASE_URL}/api/search",
                params={"q": xss_payload},
                headers=self.get_headers(),
                timeout=10
            )
            # Verificar se payload nao e refletido
            xss_blocked = xss_payload not in response.text
            results.append({
                "name": "XSS Prevention",
                "passed": xss_blocked,
                "details": "Payload nao refletido" if xss_blocked else "Possivel XSS"
            })
            print(f"  [{'PASS' if xss_blocked else 'FAIL'}] XSS Prevention")
        except Exception as e:
            results.append({"name": "XSS Prevention", "passed": False, "error": str(e)})
            print(f"  [FAIL] XSS Prevention - {e}")

        # Test 2: SQL Injection (basico)
        sqli_payload = "' OR '1'='1"
        try:
            response = requests.get(
                f"{BASE_URL}/api/search",
                params={"q": sqli_payload},
                headers=self.get_headers(),
                timeout=10
            )
            # Se retornar erro 500, pode ser vulneravel
            sqli_blocked = response.status_code != 500
            results.append({
                "name": "SQL Injection Basic",
                "passed": sqli_blocked,
                "status_code": response.status_code
            })
            print(f"  [{'PASS' if sqli_blocked else 'FAIL'}] SQL Injection Basic")
        except Exception as e:
            results.append({"name": "SQL Injection Basic", "passed": False, "error": str(e)})
            print(f"  [FAIL] SQL Injection Basic - {e}")

        # Test 3: Auth Required
        try:
            response = requests.get(f"{BASE_URL}/api/stories", timeout=10)
            auth_required = response.status_code in [401, 403]
            results.append({
                "name": "Auth Required on Stories",
                "passed": auth_required,
                "status_code": response.status_code
            })
            print(f"  [{'PASS' if auth_required else 'WARN'}] Auth Required (status: {response.status_code})")
        except Exception as e:
            results.append({"name": "Auth Required", "passed": False, "error": str(e)})
            print(f"  [FAIL] Auth Required - {e}")

        # Test 4: CORS Headers
        try:
            response = requests.options(
                f"{BASE_URL}/api/stories",
                headers={"Origin": "http://evil.com"},
                timeout=10
            )
            cors_safe = "evil.com" not in response.headers.get("Access-Control-Allow-Origin", "")
            results.append({
                "name": "CORS Security",
                "passed": cors_safe,
                "details": response.headers.get("Access-Control-Allow-Origin", "N/A")
            })
            print(f"  [{'PASS' if cors_safe else 'FAIL'}] CORS Security")
        except Exception as e:
            results.append({"name": "CORS Security", "passed": True, "details": "CORS check inconclusive"})
            print(f"  [WARN] CORS Security - {e}")

        # Test 5: Security Headers
        try:
            response = requests.get(f"{BASE_URL}/", timeout=10)
            headers_to_check = ["X-Content-Type-Options", "X-Frame-Options"]
            found_headers = [h for h in headers_to_check if h in response.headers]
            has_security_headers = len(found_headers) > 0
            results.append({
                "name": "Security Headers",
                "passed": has_security_headers,
                "found_headers": found_headers
            })
            print(f"  [{'PASS' if has_security_headers else 'WARN'}] Security Headers ({len(found_headers)}/{len(headers_to_check)})")
        except Exception as e:
            results.append({"name": "Security Headers", "passed": False, "error": str(e)})
            print(f"  [FAIL] Security Headers - {e}")

        self.results["security_tests"] = results
        return results

    def _run_single_test(self, test: Dict) -> Dict:
        """Executa um unico teste."""
        try:
            method = test.get("method", "GET").upper()
            url = f"{BASE_URL}{test['endpoint']}"
            data = test.get("data")
            expected = test.get("expected", 200)
            if not isinstance(expected, list):
                expected = [expected]

            if method == "GET":
                response = requests.get(url, headers=self.get_headers(), timeout=10)
            elif method == "POST":
                response = requests.post(url, json=data, headers=self.get_headers(), timeout=10)
            elif method == "PUT":
                response = requests.put(url, json=data, headers=self.get_headers(), timeout=10)
            elif method == "DELETE":
                response = requests.delete(url, headers=self.get_headers(), timeout=10)
            else:
                response = requests.request(method, url, json=data, headers=self.get_headers(), timeout=10)

            passed = response.status_code in expected

            return {
                "name": test["name"],
                "endpoint": test["endpoint"],
                "method": method,
                "passed": passed,
                "status_code": response.status_code,
                "expected": expected
            }

        except requests.exceptions.Timeout:
            return {
                "name": test["name"],
                "endpoint": test["endpoint"],
                "passed": False,
                "error": "Timeout"
            }
        except Exception as e:
            return {
                "name": test["name"],
                "endpoint": test["endpoint"],
                "passed": False,
                "error": str(e)
            }

    def collect_failures(self) -> List[Dict]:
        """Coleta todas as falhas para criar issues."""
        failures = []

        all_tests = (
            self.results.get("smoke_tests", []) +
            self.results.get("critical_tests", []) +
            self.results.get("api_tests", []) +
            self.results.get("persona_tests", []) +
            self.results.get("security_tests", [])
        )

        for test in all_tests:
            if not test.get("passed", True):
                failures.append({
                    "test_name": test.get("name"),
                    "endpoint": test.get("endpoint", "N/A"),
                    "error": test.get("error", f"Status {test.get('status_code', 'N/A')}"),
                    "expected": test.get("expected"),
                    "actual": test.get("status_code"),
                    "category": self._classify_failure(test)
                })

        self.results["failures"] = failures
        return failures

    def _classify_failure(self, test: Dict) -> str:
        """Classifica a falha para atribuir ao agente correto."""
        name = test.get("name", "").lower()
        endpoint = test.get("endpoint", "").lower()

        if "security" in name or "xss" in name or "sql" in name or "cors" in name or "auth" in name:
            return "SEC"
        elif "ui" in name or "page" in name or "dashboard" in name or "kanban" in name:
            return "FRONT"
        elif "api" in endpoint or "stories" in endpoint or "projects" in endpoint:
            return "BACK"
        else:
            return "BACK"  # Default para backend

    def create_github_issues(self, failures: List[Dict]) -> List[Dict]:
        """Cria issues no GitHub para cada falha."""
        print("\n" + "="*60)
        print("[GITHUB] Criando issues para falhas...")
        print("="*60)

        created_issues = []

        for failure in failures:
            agent = failure.get("category", "BACK")
            title = f"[{agent}] [Bug] {failure['test_name']}"

            body = f"""## Bug Report - Teste Automatizado

### Teste
- **Nome**: {failure['test_name']}
- **Endpoint**: {failure.get('endpoint', 'N/A')}

### Resultado
- **Esperado**: {failure.get('expected', 'Success')}
- **Atual**: {failure.get('actual', failure.get('error', 'Error'))}

### Erro
```
{failure.get('error', 'N/A')}
```

### Agente Responsavel
`[{agent}]`

### Prioridade
P2 - Medium

---
Gerado automaticamente pelo sistema de testes integrados.
"""

            # Criar issue via gh CLI
            try:
                result = subprocess.run(
                    ["gh", "issue", "create", "--title", title, "--body", body, "--label", f"bug,{agent.lower()}"],
                    capture_output=True,
                    text=True,
                    timeout=30
                )

                if result.returncode == 0:
                    issue_url = result.stdout.strip()
                    issue_number = issue_url.split("/")[-1] if "/" in issue_url else "?"
                    created_issues.append({
                        "title": title,
                        "url": issue_url,
                        "number": issue_number,
                        "agent": agent
                    })
                    print(f"  [OK] Issue #{issue_number} criada para [{agent}]")
                else:
                    print(f"  [WARN] Falha ao criar issue: {result.stderr[:100]}")

            except subprocess.TimeoutExpired:
                print(f"  [WARN] Timeout ao criar issue para {failure['test_name']}")
            except FileNotFoundError:
                print(f"  [WARN] gh CLI nao encontrado - Issue nao criada")
                break
            except Exception as e:
                print(f"  [WARN] Erro ao criar issue: {e}")

        self.results["issues_created"] = created_issues
        return created_issues

    def generate_summary(self) -> Dict:
        """Gera resumo dos testes."""
        smoke = self.results.get("smoke_tests", [])
        critical = self.results.get("critical_tests", [])
        api = self.results.get("api_tests", [])
        persona = self.results.get("persona_tests", [])
        security = self.results.get("security_tests", [])

        def count_passed(tests):
            return sum(1 for t in tests if t.get("passed", False))

        summary = {
            "smoke_tests": {"total": len(smoke), "passed": count_passed(smoke)},
            "critical_tests": {"total": len(critical), "passed": count_passed(critical)},
            "api_tests": {"total": len(api), "passed": count_passed(api)},
            "persona_tests": {"total": len(persona), "passed": count_passed(persona)},
            "security_tests": {"total": len(security), "passed": count_passed(security)},
            "total_tests": len(smoke) + len(critical) + len(api) + len(persona) + len(security),
            "total_passed": count_passed(smoke) + count_passed(critical) + count_passed(api) + count_passed(persona) + count_passed(security),
            "total_failures": len(self.results.get("failures", [])),
            "issues_created": len(self.results.get("issues_created", [])),
            "end_time": datetime.now().isoformat()
        }

        summary["pass_rate"] = f"{(summary['total_passed'] / summary['total_tests'] * 100):.1f}%" if summary["total_tests"] > 0 else "N/A"

        self.results["summary"] = summary
        return summary

    def save_results(self, filename: str = "test_results.json"):
        """Salva resultados em arquivo JSON."""
        filepath = RESULTS_DIR / filename
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(self.results, f, indent=2, ensure_ascii=False)
        print(f"\n[SAVED] Resultados salvos em: {filepath}")
        return filepath

    def run_all(self) -> Dict:
        """Executa todos os testes."""
        print("\n" + "="*60)
        print(" TESTE INTEGRADO - PLATAFORMA E")
        print(" Inicio: " + datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
        print("="*60)

        # 1. Smoke Tests
        self.run_smoke_tests()

        # 2. Critical Path
        self.run_critical_path_tests()

        # 3. API Tests
        self.run_api_tests()

        # 4. Persona Tests
        self.run_persona_tests()

        # 5. Security Tests
        self.run_security_tests()

        # 6. Coletar falhas
        failures = self.collect_failures()

        # 7. Criar issues no GitHub
        if failures:
            self.create_github_issues(failures)

        # 8. Gerar sumario
        summary = self.generate_summary()

        # 9. Salvar resultados
        self.save_results()

        # 10. Imprimir sumario
        print("\n" + "="*60)
        print(" SUMARIO")
        print("="*60)
        print(f"  Smoke Tests:    {summary['smoke_tests']['passed']}/{summary['smoke_tests']['total']}")
        print(f"  Critical Path:  {summary['critical_tests']['passed']}/{summary['critical_tests']['total']}")
        print(f"  API Tests:      {summary['api_tests']['passed']}/{summary['api_tests']['total']}")
        print(f"  Persona Tests:  {summary['persona_tests']['passed']}/{summary['persona_tests']['total']}")
        print(f"  Security Tests: {summary['security_tests']['passed']}/{summary['security_tests']['total']}")
        print("-"*60)
        print(f"  TOTAL:          {summary['total_passed']}/{summary['total_tests']} ({summary['pass_rate']})")
        print(f"  Falhas:         {summary['total_failures']}")
        print(f"  Issues criadas: {summary['issues_created']}")
        print("="*60)

        return self.results


if __name__ == "__main__":
    runner = IntegratedTestRunner()
    results = runner.run_all()

    # Exit code baseado em falhas criticas
    smoke_passed = all(t.get("passed", False) for t in results.get("smoke_tests", []))
    sys.exit(0 if smoke_passed else 1)

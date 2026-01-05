"""
Sync Instructions - Sincronizacao de Conhecimento com GitHub Issues

Este modulo sincroniza o conhecimento dos agentes com os issues do GitHub.
Atualiza automaticamente a base de conhecimento quando issues mudam de status.

Uso:
    python -m factory.agents.sync_instructions          # Sync manual
    python -m factory.agents.sync_instructions --auto   # Modo watch (a cada 5 min)
    python -m factory.agents.sync_instructions --force  # Forca atualizacao completa

Versao: 1.0
"""

import subprocess
import json
import os
import sys
import time
import argparse
import re
from datetime import datetime
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum

# Adiciona o diretorio raiz ao path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from factory.agents.agent_knowledge import (
    PLATFORM_KNOWLEDGE,
    AGENT_KNOWLEDGE,
    LEARNING_HISTORY,
    save_knowledge_to_file,
    AgentType
)


# ==============================================================================
# CONFIGURACOES
# ==============================================================================

SYNC_INTERVAL = 300  # 5 minutos em segundos
GH_COMMAND = "gh"
REPO_PATH = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# ==============================================================================
# MAPEAMENTO DE LABELS PARA AGENTES
# ==============================================================================

LABEL_TO_AGENT: Dict[str, str] = {
    # Labels diretos
    "backend": "BACK",
    "frontend": "FRONT",
    "devops": "DEVOPS",
    "security": "SEC",
    "qa": "QA",
    "product": "PROD",
    "innovation": "INOV",
    "finops": "FIN",
    "growth": "GROWTH",
    "architecture": "ARCH",
    "orchestration": "ORCH",

    # Labels de feature
    "api": "BACK",
    "database": "BACK",
    "models": "BACK",
    "ui": "FRONT",
    "ux": "FRONT",
    "dashboard": "FRONT",
    "docker": "DEVOPS",
    "kubernetes": "DEVOPS",
    "ci/cd": "DEVOPS",
    "infra": "DEVOPS",
    "auth": "SEC",
    "rbac": "SEC",
    "audit": "SEC",
    "test": "QA",
    "testing": "QA",
    "e2e": "QA",
    "feature": "PROD",
    "story": "PROD",
    "research": "INOV",
    "poc": "INOV",
    "billing": "FIN",
    "pricing": "FIN",
    "marketing": "GROWTH",
    "marketplace": "GROWTH",

    # Prefixos de issue
    "[BACK]": "BACK",
    "[FRONT]": "FRONT",
    "[DEVOPS]": "DEVOPS",
    "[SEC]": "SEC",
    "[QA]": "QA",
    "[PROD]": "PROD",
    "[INOV]": "INOV",
    "[FIN]": "FIN",
    "[GROWTH]": "GROWTH",
    "[ARCH]": "ARCH",
    "[ORCH]": "ORCH"
}


# ==============================================================================
# CLASSES DE DADOS
# ==============================================================================

@dataclass
class Issue:
    """Representa um issue do GitHub."""
    number: int
    title: str
    state: str
    labels: List[str]
    body: str
    created_at: str
    closed_at: Optional[str]
    assignees: List[str] = None
    milestone: Optional[str] = None

    @property
    def is_open(self) -> bool:
        return self.state.upper() == "OPEN"

    @property
    def is_closed(self) -> bool:
        return self.state.upper() == "CLOSED"

    def get_agent(self) -> Optional[str]:
        """Determina qual agente e responsavel por este issue."""
        # Primeiro, verifica prefixo no titulo
        for prefix, agent in LABEL_TO_AGENT.items():
            if prefix.startswith("[") and prefix in self.title:
                return agent

        # Depois, verifica labels
        for label in self.labels:
            label_lower = label.lower()
            if label_lower in LABEL_TO_AGENT:
                return LABEL_TO_AGENT[label_lower]

        # Analisa palavras-chave no titulo e body
        text = f"{self.title} {self.body}".lower()
        keyword_scores = {agent: 0 for agent in AgentType}

        keyword_map = {
            "BACK": ["api", "endpoint", "route", "database", "model", "query", "rest", "crud"],
            "FRONT": ["ui", "ux", "dashboard", "component", "frontend", "css", "html", "vue", "react"],
            "DEVOPS": ["docker", "kubernetes", "k8s", "deploy", "ci", "cd", "pipeline", "infra"],
            "SEC": ["auth", "security", "login", "password", "token", "jwt", "rbac", "permission"],
            "QA": ["test", "testing", "e2e", "unit", "integration", "coverage", "pytest"],
            "PROD": ["feature", "story", "user", "requirement", "backlog", "sprint"],
            "INOV": ["research", "poc", "experiment", "ai", "ml", "innovation"],
            "FIN": ["billing", "pricing", "cost", "subscription", "payment"],
            "GROWTH": ["marketing", "launch", "campaign", "analytics", "growth"],
            "ARCH": ["architecture", "design", "pattern", "refactor", "structure"],
            "ORCH": ["coordinate", "review", "approve", "distribute", "team"]
        }

        for agent, keywords in keyword_map.items():
            for kw in keywords:
                if kw in text:
                    keyword_scores[agent] += 1

        # Retorna o agente com maior score (se houver)
        max_score = max(keyword_scores.values())
        if max_score > 0:
            for agent, score in keyword_scores.items():
                if score == max_score:
                    return agent

        return None

    def get_story_points(self) -> int:
        """Extrai story points do titulo ou body."""
        # Procura padrao "X pts" ou "X points"
        patterns = [
            r"(\d+)\s*pts?",
            r"(\d+)\s*points?",
            r"\[(\d+)\]",
            r"SP:\s*(\d+)"
        ]

        text = f"{self.title} {self.body}"
        for pattern in patterns:
            match = re.search(pattern, text, re.IGNORECASE)
            if match:
                points = int(match.group(1))
                # Valida fibonacci
                if points in [1, 2, 3, 5, 8, 13, 21]:
                    return points

        return 0

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario."""
        return {
            "number": self.number,
            "title": self.title,
            "state": self.state,
            "points": self.get_story_points(),
            "agent": self.get_agent()
        }


# ==============================================================================
# FUNCOES DE SYNC
# ==============================================================================

def run_gh_command(args: List[str]) -> Tuple[bool, str]:
    """Executa um comando gh e retorna resultado."""
    try:
        result = subprocess.run(
            [GH_COMMAND] + args,
            capture_output=True,
            cwd=REPO_PATH,
            timeout=60,
            encoding='utf-8',
            errors='replace'
        )
        if result.returncode == 0:
            return True, result.stdout
        else:
            return False, result.stderr
    except subprocess.TimeoutExpired:
        return False, "Timeout executando comando gh"
    except FileNotFoundError:
        return False, "gh CLI nao encontrado. Instale: https://cli.github.com/"
    except Exception as e:
        return False, str(e)


def fetch_all_issues() -> List[Issue]:
    """Busca todos os issues do repositorio."""
    print("Buscando issues do GitHub...")

    success, output = run_gh_command([
        "issue", "list",
        "--state", "all",
        "--limit", "500",
        "--json", "number,title,state,labels,body,createdAt,closedAt,assignees,milestone"
    ])

    if not success:
        print(f"Erro ao buscar issues: {output}")
        return []

    try:
        data = json.loads(output)
        issues = []

        for item in data:
            issue = Issue(
                number=item.get("number", 0),
                title=item.get("title", ""),
                state=item.get("state", ""),
                labels=[l.get("name", "") for l in item.get("labels", [])],
                body=item.get("body", "") or "",
                created_at=item.get("createdAt", ""),
                closed_at=item.get("closedAt"),
                assignees=[a.get("login", "") for a in item.get("assignees", [])],
                milestone=item.get("milestone", {}).get("title") if item.get("milestone") else None
            )
            issues.append(issue)

        print(f"Encontrados {len(issues)} issues")
        return issues

    except json.JSONDecodeError as e:
        print(f"Erro ao parsear JSON: {e}")
        return []


def categorize_issues(issues: List[Issue]) -> Dict[str, Dict[str, List[Dict]]]:
    """Categoriza issues por status e agente."""
    result = {
        "implemented": [],
        "pending": [],
        "by_agent": {agent.value: {"implemented": [], "pending": []} for agent in AgentType}
    }

    for issue in issues:
        issue_dict = issue.to_dict()
        agent = issue.get_agent()

        if issue.is_closed:
            result["implemented"].append(issue_dict)
            if agent and agent in result["by_agent"]:
                result["by_agent"][agent]["implemented"].append(issue_dict)
        else:
            result["pending"].append(issue_dict)
            if agent and agent in result["by_agent"]:
                result["by_agent"][agent]["pending"].append(issue_dict)

    return result


def update_platform_knowledge(categorized: Dict[str, Any]) -> None:
    """Atualiza PLATFORM_KNOWLEDGE com dados dos issues."""
    global PLATFORM_KNOWLEDGE

    PLATFORM_KNOWLEDGE["issues"]["implemented"] = categorized["implemented"]
    PLATFORM_KNOWLEDGE["issues"]["pending"] = categorized["pending"]
    PLATFORM_KNOWLEDGE["last_sync"] = datetime.now().isoformat()

    print(f"Atualizados {len(categorized['implemented'])} issues implementados")
    print(f"Atualizados {len(categorized['pending'])} issues pendentes")


def update_agent_knowledge(categorized: Dict[str, Any]) -> None:
    """Atualiza AGENT_KNOWLEDGE com issues especificos de cada agente."""
    global AGENT_KNOWLEDGE

    for agent_id, data in categorized["by_agent"].items():
        if agent_id in AGENT_KNOWLEDGE:
            # Atualiza issues implementados
            AGENT_KNOWLEDGE[agent_id]["implemented_features"] = [
                f"#{i['number']} {i['title']}" for i in data["implemented"]
            ]

            # Atualiza issues pendentes
            AGENT_KNOWLEDGE[agent_id]["pending_features"] = [
                f"#{i['number']} {i['title']}" for i in data["pending"]
            ]

    print("Conhecimento dos agentes atualizado")


def scan_codebase_for_endpoints() -> List[str]:
    """Escaneia o codebase para encontrar endpoints existentes."""
    endpoints = []
    api_dir = os.path.join(REPO_PATH, "factory", "api")

    if not os.path.exists(api_dir):
        return endpoints

    # Regex para encontrar decorators de rota
    route_patterns = [
        r'@\w+\.(?:get|post|put|delete|patch)\s*\(\s*["\']([^"\']+)["\']',
        r'@router\.(?:get|post|put|delete|patch)\s*\(\s*["\']([^"\']+)["\']',
        r'@app\.(?:get|post|put|delete|patch)\s*\(\s*["\']([^"\']+)["\']'
    ]

    for filename in os.listdir(api_dir):
        if filename.endswith(".py"):
            filepath = os.path.join(api_dir, filename)
            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.read()
                    for pattern in route_patterns:
                        matches = re.findall(pattern, content)
                        endpoints.extend(matches)
            except Exception:
                continue

    return list(set(endpoints))


def scan_codebase_for_models() -> List[str]:
    """Escaneia o codebase para encontrar modelos SQLAlchemy."""
    models = []
    models_file = os.path.join(REPO_PATH, "factory", "database", "models.py")

    if not os.path.exists(models_file):
        return models

    try:
        with open(models_file, 'r', encoding='utf-8') as f:
            content = f.read()
            # Procura classes que herdam de Base
            pattern = r'class\s+(\w+)\s*\([^)]*(?:Base|Model)[^)]*\)'
            matches = re.findall(pattern, content)
            models.extend(matches)
    except Exception:
        pass

    return list(set(models))


def scan_codebase_for_tests() -> Dict[str, Any]:
    """Escaneia o codebase para encontrar status de testes."""
    tests_dir = os.path.join(REPO_PATH, "tests")

    if not os.path.exists(tests_dir):
        return {"files": [], "count": 0}

    test_files = []
    for root, dirs, files in os.walk(tests_dir):
        for f in files:
            if f.startswith("test_") and f.endswith(".py"):
                rel_path = os.path.relpath(os.path.join(root, f), REPO_PATH)
                test_files.append(rel_path)

    return {
        "files": test_files,
        "count": len(test_files)
    }


def run_full_sync(force: bool = False) -> bool:
    """Executa sincronizacao completa."""
    print("=" * 60)
    print(f"Sincronizacao iniciada em {datetime.now().isoformat()}")
    print("=" * 60)

    # 1. Busca issues do GitHub
    issues = fetch_all_issues()
    if not issues and not force:
        print("Nenhum issue encontrado, abortando")
        return False

    # 2. Categoriza issues
    categorized = categorize_issues(issues)

    # 3. Atualiza conhecimento da plataforma
    update_platform_knowledge(categorized)

    # 4. Atualiza conhecimento dos agentes
    update_agent_knowledge(categorized)

    # 5. Escaneia endpoints
    print("\nEscaneando endpoints...")
    endpoints = scan_codebase_for_endpoints()
    print(f"Encontrados {len(endpoints)} endpoints")

    # 6. Escaneia modelos
    print("\nEscaneando modelos...")
    models = scan_codebase_for_models()
    print(f"Encontrados {len(models)} modelos")

    # 7. Escaneia testes
    print("\nEscaneando testes...")
    tests = scan_codebase_for_tests()
    print(f"Encontrados {tests['count']} arquivos de teste")

    # 8. Atualiza conhecimento com dados do codebase
    PLATFORM_KNOWLEDGE["codebase_scan"] = {
        "endpoints": endpoints,
        "models": models,
        "tests": tests,
        "scanned_at": datetime.now().isoformat()
    }

    # 9. Salva em arquivo
    print("\nSalvando conhecimento...")
    save_knowledge_to_file()

    print("\n" + "=" * 60)
    print("Sincronizacao concluida!")
    print(f"- Issues implementados: {len(categorized['implemented'])}")
    print(f"- Issues pendentes: {len(categorized['pending'])}")
    print(f"- Endpoints encontrados: {len(endpoints)}")
    print(f"- Modelos encontrados: {len(models)}")
    print(f"- Arquivos de teste: {tests['count']}")
    print("=" * 60)

    return True


def watch_mode(interval: int = SYNC_INTERVAL) -> None:
    """Executa em modo watch, sincronizando periodicamente."""
    print(f"Modo watch ativado. Sincronizando a cada {interval} segundos.")
    print("Pressione Ctrl+C para parar.\n")

    try:
        while True:
            run_full_sync()
            print(f"\nProxima sincronizacao em {interval} segundos...")
            time.sleep(interval)
    except KeyboardInterrupt:
        print("\nWatch mode encerrado.")


def print_summary() -> None:
    """Imprime resumo do conhecimento atual."""
    print("\n" + "=" * 60)
    print("RESUMO DO CONHECIMENTO ATUAL")
    print("=" * 60)

    print(f"\nVersao: {PLATFORM_KNOWLEDGE.get('version', 'N/A')}")
    print(f"Ultimo sync: {PLATFORM_KNOWLEDGE.get('last_sync', 'Nunca')}")

    issues = PLATFORM_KNOWLEDGE.get("issues", {})
    print(f"\nIssues:")
    print(f"  - Implementados: {len(issues.get('implemented', []))}")
    print(f"  - Pendentes: {len(issues.get('pending', []))}")

    print("\nConhecimento por agente:")
    for agent_id, data in AGENT_KNOWLEDGE.items():
        impl = len(data.get("implemented_features", []))
        pend = len(data.get("pending_features", []))
        print(f"  - {agent_id}: {impl} implementados, {pend} pendentes")


# ==============================================================================
# MAIN
# ==============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Sincroniza conhecimento dos agentes com GitHub Issues"
    )
    parser.add_argument(
        "--auto", "-a",
        action="store_true",
        help="Modo watch: sincroniza periodicamente"
    )
    parser.add_argument(
        "--force", "-f",
        action="store_true",
        help="Forca atualizacao mesmo sem mudancas"
    )
    parser.add_argument(
        "--interval", "-i",
        type=int,
        default=SYNC_INTERVAL,
        help=f"Intervalo de sync em segundos (default: {SYNC_INTERVAL})"
    )
    parser.add_argument(
        "--summary", "-s",
        action="store_true",
        help="Mostra resumo do conhecimento atual"
    )

    args = parser.parse_args()

    if args.summary:
        print_summary()
        return

    if args.auto:
        watch_mode(args.interval)
    else:
        run_full_sync(args.force)


if __name__ == "__main__":
    main()

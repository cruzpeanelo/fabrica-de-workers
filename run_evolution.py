#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Plataforma E - Autonomous Evolution Runner
==========================================

Este script executa os 11 agentes de forma autonoma por 8 horas.

Fluxo:
1. PROD e INOV criam novas issues (features, melhorias, inovacoes)
2. ARCH revisa e planeja a arquitetura
3. BACK, FRONT, DEVOPS implementam
4. SEC faz revisao de seguranca
5. QA testa e valida
6. Loop continuo por 8 horas

Uso:
    python run_evolution.py                    # 8 horas (padrao)
    python run_evolution.py --duration 4h     # 4 horas
    python run_evolution.py --duration 12h    # 12 horas
"""

import os
import sys
import json
import time
import asyncio
import subprocess
import logging
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field, asdict

# Setup path
BASE_DIR = Path(__file__).parent
sys.path.insert(0, str(BASE_DIR))

# Logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S"
)
logger = logging.getLogger("Evolution")

# State directory
STATE_DIR = BASE_DIR / "factory" / "state"
STATE_DIR.mkdir(exist_ok=True)


# =============================================================================
# CONFIGURATION
# =============================================================================

@dataclass
class EvolutionConfig:
    """Configuracao da evolucao autonoma."""
    duration_hours: float = 8.0
    poll_interval: int = 60  # segundos entre ciclos
    max_concurrent_agents: int = 3
    issues_per_cycle_prod: int = 2  # issues criadas por PROD por ciclo
    issues_per_cycle_inov: int = 1  # issues criadas por INOV por ciclo
    min_issues_to_work: int = 3  # minimo de issues antes de implementar
    auto_commit: bool = True
    auto_close: bool = True


@dataclass
class EvolutionState:
    """Estado da evolucao."""
    started_at: str = ""
    end_time: str = ""
    cycles_completed: int = 0
    issues_created: int = 0
    issues_implemented: int = 0
    issues_tested: int = 0
    agents_active: Dict[str, str] = field(default_factory=dict)
    last_prod_cycle: int = 0
    last_inov_cycle: int = 0
    errors: List[str] = field(default_factory=list)

    def save(self):
        path = STATE_DIR / "evolution_state.json"
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(asdict(self), f, indent=2)

    @classmethod
    def load(cls) -> "EvolutionState":
        path = STATE_DIR / "evolution_state.json"
        if path.exists():
            with open(path, 'r', encoding='utf-8') as f:
                return cls(**json.load(f))
        return cls()


# =============================================================================
# EVOLUTION IDEAS - Templates for PROD and INOV
# =============================================================================

PROD_FEATURE_TEMPLATES = [
    # Dashboard Features
    "[FRONT] Dashboard de {area} com graficos interativos e filtros avancados",
    "[FRONT] Componente de {component} reutilizavel com temas dark/light",
    "[FRONT] Modal de {action} com validacao em tempo real",

    # API Features
    "[BACK] API de {resource} com paginacao, filtros e ordenacao",
    "[BACK] Endpoint de {action} com validacao e rate limiting",
    "[BACK] Webhook para notificacao de {event}",

    # Integration
    "[BACK] Integracao com {service} para {purpose}",
    "[DEVOPS] Pipeline CI/CD para {environment}",

    # User Experience
    "[FRONT] Onboarding wizard para novos usuarios",
    "[FRONT] Sistema de notificacoes em tempo real",
    "[FRONT] Exportacao de relatorios em PDF/Excel",
]

PROD_AREAS = ["metricas", "analytics", "performance", "usuarios", "projetos", "sprints"]
PROD_COMPONENTS = ["tabela", "formulario", "card", "chart", "timeline", "kanban"]
PROD_ACTIONS = ["criar", "editar", "deletar", "importar", "exportar", "configurar"]
PROD_RESOURCES = ["relatorios", "alertas", "templates", "workflows", "integrações"]
PROD_EVENTS = ["story_completed", "sprint_started", "deployment_done", "test_failed"]
PROD_SERVICES = ["Slack", "Teams", "Jira", "GitHub", "AWS S3", "SendGrid"]
PROD_PURPOSES = ["notificacoes", "sincronizacao", "backup", "monitoramento"]
PROD_ENVIRONMENTS = ["staging", "production", "preview"]

INOV_RESEARCH_TEMPLATES = [
    "[INOV] POC: Implementar {technology} para {use_case}",
    "[INOV] Research: Avaliar {framework} vs {alternative} para {purpose}",
    "[INOV] Benchmark: Performance de {component} com {approach}",
    "[INOV] Experiment: {ai_feature} usando {ml_tech}",
]

INOV_TECHNOLOGIES = ["WebSockets", "GraphQL", "gRPC", "Redis Streams", "Elasticsearch"]
INOV_USE_CASES = ["real-time updates", "busca avancada", "cache distribuido", "filas de mensagem"]
INOV_FRAMEWORKS = ["FastAPI", "Django", "Flask", "Starlette"]
INOV_ALTERNATIVES = ["Express", "NestJS", "Spring Boot", "Go Fiber"]
INOV_AI_FEATURES = ["estimativa de story points", "classificacao de bugs", "sugestao de assignee"]
INOV_ML_TECHS = ["scikit-learn", "transformers", "sentence-transformers", "OpenAI embeddings"]


# =============================================================================
# AGENT RUNNER
# =============================================================================

class AgentRunner:
    """Executa agentes via Claude CLI."""

    def __init__(self, base_dir: Path):
        self.base_dir = base_dir
        self.prompts_dir = base_dir / "prompts"
        self.active_processes: Dict[str, subprocess.Popen] = {}

    def get_agent_prompt(self, agent_type: str) -> str:
        """Carrega prompt do agente."""
        prompt_map = {
            "PROD": "agente_produto.md",
            "INOV": "agente_inovacao.md",
            "ARCH": "agente_arquiteto.md",
            "BACK": "agente_backend.md",
            "FRONT": "agente_frontend.md",
            "DEVOPS": "agente_devops.md",
            "SEC": "agente_security.md",
            "QA": "agente_qa.md",
            "FIN": "agente_financeiro.md",
            "GROWTH": "agente_growth.md",
            "ORCH": "agente_orquestrador.md",
        }

        filename = prompt_map.get(agent_type)
        if not filename:
            return ""

        path = self.prompts_dir / filename
        if path.exists():
            return path.read_text(encoding='utf-8')
        return ""

    async def run_agent_task(self, agent_type: str, task: str, timeout: int = 300) -> Dict[str, Any]:
        """Executa tarefa com um agente."""
        logger.info(f"[{agent_type}] Iniciando: {task[:60]}...")

        prompt = f"""Voce e o agente [{agent_type}] da Plataforma E.

{self.get_agent_prompt(agent_type)}

---

## TAREFA ATUAL

{task}

---

## INSTRUCOES

1. Execute a tarefa acima
2. Use git para commitar suas mudancas
3. Se criar uma issue, use: gh issue create --title "..." --body "..."
4. Se fechar uma issue, use: gh issue close <numero> -c "Implementado"
5. Seja conciso e eficiente
"""

        # Salvar prompt temporario
        prompt_file = STATE_DIR / f"task_{agent_type}.md"
        prompt_file.write_text(prompt, encoding='utf-8')

        try:
            # Executar Claude
            result = subprocess.run(
                ["claude", "--dangerously-skip-permissions", "-p", str(prompt_file), "--max-turns", "50"],
                capture_output=True,
                text=True,
                timeout=timeout,
                cwd=str(self.base_dir)
            )

            return {
                "success": result.returncode == 0,
                "output": result.stdout[:2000] if result.stdout else "",
                "error": result.stderr[:500] if result.stderr else ""
            }

        except subprocess.TimeoutExpired:
            logger.warning(f"[{agent_type}] Timeout apos {timeout}s")
            return {"success": False, "error": "Timeout"}
        except Exception as e:
            logger.error(f"[{agent_type}] Erro: {e}")
            return {"success": False, "error": str(e)}


# =============================================================================
# EVOLUTION ORCHESTRATOR
# =============================================================================

class EvolutionOrchestrator:
    """Orquestrador da evolucao autonoma."""

    def __init__(self, config: EvolutionConfig):
        self.config = config
        self.state = EvolutionState.load()
        self.runner = AgentRunner(BASE_DIR)
        self.running = False

        # Contadores para templates
        self._prod_idx = 0
        self._inov_idx = 0

    def _get_next_prod_idea(self) -> str:
        """Gera proxima ideia de produto."""
        import random
        template = PROD_FEATURE_TEMPLATES[self._prod_idx % len(PROD_FEATURE_TEMPLATES)]
        self._prod_idx += 1

        # Substituir placeholders
        idea = template.format(
            area=random.choice(PROD_AREAS),
            component=random.choice(PROD_COMPONENTS),
            action=random.choice(PROD_ACTIONS),
            resource=random.choice(PROD_RESOURCES),
            event=random.choice(PROD_EVENTS),
            service=random.choice(PROD_SERVICES),
            purpose=random.choice(PROD_PURPOSES),
            environment=random.choice(PROD_ENVIRONMENTS),
        )
        return idea

    def _get_next_inov_idea(self) -> str:
        """Gera proxima ideia de inovacao."""
        import random
        template = INOV_RESEARCH_TEMPLATES[self._inov_idx % len(INOV_RESEARCH_TEMPLATES)]
        self._inov_idx += 1

        idea = template.format(
            technology=random.choice(INOV_TECHNOLOGIES),
            use_case=random.choice(INOV_USE_CASES),
            framework=random.choice(INOV_FRAMEWORKS),
            alternative=random.choice(INOV_ALTERNATIVES),
            purpose=random.choice(PROD_PURPOSES),
            component=random.choice(PROD_COMPONENTS),
            approach="async" if random.random() > 0.5 else "sync",
            ai_feature=random.choice(INOV_AI_FEATURES),
            ml_tech=random.choice(INOV_ML_TECHS),
        )
        return idea

    async def get_open_issues(self) -> List[Dict]:
        """Busca issues abertas no GitHub."""
        try:
            result = subprocess.run(
                ["gh", "issue", "list", "--state", "open", "--limit", "50",
                 "--json", "number,title,labels"],
                capture_output=True, text=True, cwd=str(BASE_DIR)
            )
            if result.returncode == 0:
                return json.loads(result.stdout)
        except Exception as e:
            logger.error(f"Erro ao buscar issues: {e}")
        return []

    async def run_prod_cycle(self):
        """Ciclo do agente PROD - cria issues de features."""
        logger.info("[PROD] Iniciando ciclo de criacao de features...")

        for i in range(self.config.issues_per_cycle_prod):
            idea = self._get_next_prod_idea()

            task = f"""Crie uma nova issue no GitHub para a seguinte feature:

{idea}

Use o comando:
gh issue create --title "{idea}" --body "## Descricao
[Descreva a feature em detalhes]

## Criterios de Aceite
- [ ] ...

## Estimativa
Story Points: X

## Prioridade
Media"

Seja criativo e detalhado na descricao.
"""
            result = await self.runner.run_agent_task("PROD", task, timeout=180)

            if result["success"]:
                self.state.issues_created += 1
                logger.info(f"[PROD] Issue criada: {idea[:50]}...")
            else:
                logger.warning(f"[PROD] Falha ao criar issue: {result.get('error', 'unknown')}")

        self.state.last_prod_cycle = self.state.cycles_completed
        self.state.save()

    async def run_inov_cycle(self):
        """Ciclo do agente INOV - cria issues de inovacao."""
        logger.info("[INOV] Iniciando ciclo de pesquisa e inovacao...")

        for i in range(self.config.issues_per_cycle_inov):
            idea = self._get_next_inov_idea()

            task = f"""Crie uma nova issue no GitHub para a seguinte pesquisa/POC:

{idea}

Use o comando:
gh issue create --title "{idea}" --body "## Objetivo
[Descreva o objetivo da pesquisa]

## Motivacao
[Por que isso e importante para a Plataforma E?]

## Abordagem
1. ...
2. ...

## Resultados Esperados
- ...

## Recursos
- Tempo estimado: X horas
- Dependencias: ..."

Seja tecnico e detalhado.
"""
            result = await self.runner.run_agent_task("INOV", task, timeout=180)

            if result["success"]:
                self.state.issues_created += 1
                logger.info(f"[INOV] Issue criada: {idea[:50]}...")

        self.state.last_inov_cycle = self.state.cycles_completed
        self.state.save()

    async def run_implementation_cycle(self, issues: List[Dict]):
        """Ciclo de implementacao - BACK, FRONT, DEVOPS trabalham nas issues."""
        if not issues:
            logger.info("Nenhuma issue para implementar")
            return

        # Classificar issues por tipo
        back_issues = [i for i in issues if "[BACK]" in i["title"]]
        front_issues = [i for i in issues if "[FRONT]" in i["title"]]
        devops_issues = [i for i in issues if "[DEVOPS]" in i["title"]]
        inov_issues = [i for i in issues if "[INOV]" in i["title"]]

        # Executar em paralelo (limitado)
        tasks = []

        if back_issues:
            issue = back_issues[0]
            tasks.append(self._implement_issue("BACK", issue))

        if front_issues:
            issue = front_issues[0]
            tasks.append(self._implement_issue("FRONT", issue))

        if devops_issues:
            issue = devops_issues[0]
            tasks.append(self._implement_issue("DEVOPS", issue))

        if inov_issues:
            issue = inov_issues[0]
            tasks.append(self._implement_issue("INOV", issue))

        if tasks:
            await asyncio.gather(*tasks[:self.config.max_concurrent_agents])

    async def _implement_issue(self, agent_type: str, issue: Dict):
        """Implementa uma issue especifica."""
        number = issue["number"]
        title = issue["title"]

        logger.info(f"[{agent_type}] Implementando issue #{number}: {title[:40]}...")

        task = f"""Implemente a issue #{number}: {title}

Primeiro, leia os detalhes da issue:
gh issue view {number}

Depois:
1. Analise os requisitos
2. Implemente a solucao
3. Crie testes se aplicavel
4. Faca commit das mudancas
5. Feche a issue quando concluido:
   gh issue close {number} -c "Implementado: [breve descricao]"

Seja eficiente e siga as melhores praticas.
"""

        result = await self.runner.run_agent_task(agent_type, task, timeout=600)

        if result["success"]:
            self.state.issues_implemented += 1
            logger.info(f"[{agent_type}] Issue #{number} implementada")
        else:
            logger.warning(f"[{agent_type}] Falha na issue #{number}")

        self.state.save()

    async def run_qa_cycle(self):
        """Ciclo de QA - testa implementacoes recentes."""
        logger.info("[QA] Iniciando ciclo de testes...")

        task = """Execute os testes da Plataforma E:

1. Rode os testes unitarios:
   python -m pytest tests/unit/ -v --tb=short

2. Verifique a cobertura atual

3. Se houver falhas, crie issues para os bugs encontrados:
   gh issue create --title "[QA] Bug: ..." --body "..."

4. Faca um resumo do estado dos testes
"""

        result = await self.runner.run_agent_task("QA", task, timeout=300)

        if result["success"]:
            self.state.issues_tested += 1
            logger.info("[QA] Ciclo de testes concluido")

        self.state.save()

    async def run_security_cycle(self):
        """Ciclo de SEC - revisao de seguranca."""
        logger.info("[SEC] Iniciando revisao de seguranca...")

        task = """Faca uma revisao de seguranca do codigo recente:

1. Verifique os commits recentes:
   git log --oneline -10

2. Analise arquivos criticos:
   - factory/api/auth.py
   - factory/security/

3. Execute bandit se disponivel:
   bandit -r factory/ -ll

4. Se encontrar problemas, crie issues:
   gh issue create --title "[SEC] Vulnerabilidade: ..." --body "..."

Seja rigoroso mas pratico.
"""

        await self.runner.run_agent_task("SEC", task, timeout=240)
        self.state.save()

    def print_status(self):
        """Imprime status atual."""
        elapsed = datetime.now() - datetime.fromisoformat(self.state.started_at)
        elapsed_str = str(elapsed).split('.')[0]

        remaining = datetime.fromisoformat(self.state.end_time) - datetime.now()
        remaining_str = str(remaining).split('.')[0] if remaining.total_seconds() > 0 else "0:00:00"

        print("\n" + "=" * 60)
        print(" PLATAFORMA E - EVOLUCAO AUTONOMA")
        print("=" * 60)
        print(f"  Ciclo: {self.state.cycles_completed}")
        print(f"  Tempo decorrido: {elapsed_str}")
        print(f"  Tempo restante: {remaining_str}")
        print(f"  ---")
        print(f"  Issues criadas: {self.state.issues_created}")
        print(f"  Issues implementadas: {self.state.issues_implemented}")
        print(f"  Ciclos de teste: {self.state.issues_tested}")
        print("=" * 60 + "\n")

    async def run(self):
        """Loop principal de evolucao."""
        # Inicializar estado
        self.running = True
        self.state.started_at = datetime.now().isoformat()
        end_time = datetime.now() + timedelta(hours=self.config.duration_hours)
        self.state.end_time = end_time.isoformat()
        self.state.save()

        logger.info(f"Iniciando evolucao autonoma por {self.config.duration_hours} horas")
        logger.info(f"Termino previsto: {end_time.strftime('%Y-%m-%d %H:%M:%S')}")

        try:
            while self.running and datetime.now() < end_time:
                self.state.cycles_completed += 1
                cycle = self.state.cycles_completed

                logger.info(f"\n{'='*50}")
                logger.info(f"CICLO #{cycle}")
                logger.info(f"{'='*50}")

                # A cada 3 ciclos, PROD cria features
                if cycle % 3 == 1:
                    await self.run_prod_cycle()

                # A cada 5 ciclos, INOV pesquisa
                if cycle % 5 == 1:
                    await self.run_inov_cycle()

                # Buscar issues abertas
                issues = await self.get_open_issues()
                logger.info(f"Issues abertas: {len(issues)}")

                # Se houver issues suficientes, implementar
                if len(issues) >= self.config.min_issues_to_work:
                    await self.run_implementation_cycle(issues)

                # A cada 4 ciclos, QA testa
                if cycle % 4 == 0:
                    await self.run_qa_cycle()

                # A cada 6 ciclos, SEC revisa
                if cycle % 6 == 0:
                    await self.run_security_cycle()

                # Status
                self.print_status()

                # Aguardar proximo ciclo
                await asyncio.sleep(self.config.poll_interval)

        except KeyboardInterrupt:
            logger.info("\nInterrompido pelo usuario")

        except Exception as e:
            logger.error(f"Erro no loop principal: {e}")
            self.state.errors.append(str(e))

        finally:
            self.running = False
            self.state.save()
            self._print_summary()

    def _print_summary(self):
        """Imprime resumo final."""
        print("\n" + "=" * 60)
        print(" RESUMO DA EVOLUCAO")
        print("=" * 60)
        print(f"  Inicio: {self.state.started_at}")
        print(f"  Fim: {datetime.now().isoformat()}")
        print(f"  Ciclos completados: {self.state.cycles_completed}")
        print(f"  Issues criadas: {self.state.issues_created}")
        print(f"  Issues implementadas: {self.state.issues_implemented}")
        print(f"  Erros: {len(self.state.errors)}")
        print("=" * 60)


# =============================================================================
# MAIN
# =============================================================================

def parse_duration(value: str) -> float:
    """Parse duracao em horas."""
    value = value.lower().strip()

    if value.endswith('h'):
        return float(value[:-1])
    elif value.endswith('m'):
        return float(value[:-1]) / 60
    elif value.isdigit():
        return float(value)

    return 8.0  # default


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="Plataforma E - Evolucao Autonoma",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos:
  python run_evolution.py                    # 8 horas (padrao)
  python run_evolution.py --duration 4h     # 4 horas
  python run_evolution.py --duration 12h    # 12 horas
  python run_evolution.py --duration 30m    # 30 minutos (teste)
"""
    )
    parser.add_argument(
        "--duration", "-d",
        type=str,
        default="8h",
        help="Duracao da evolucao (ex: 8h, 4h, 30m)"
    )
    parser.add_argument(
        "--poll-interval", "-p",
        type=int,
        default=60,
        help="Intervalo entre ciclos em segundos (default: 60)"
    )
    parser.add_argument(
        "--max-agents", "-m",
        type=int,
        default=3,
        help="Maximo de agentes simultaneos (default: 3)"
    )

    args = parser.parse_args()

    # Configuracao
    config = EvolutionConfig(
        duration_hours=parse_duration(args.duration),
        poll_interval=args.poll_interval,
        max_concurrent_agents=args.max_agents
    )

    # Banner
    print("""
+---------------------------------------------------------------+
|                                                               |
|   PLATAFORMA E - EVOLUCAO AUTONOMA                            |
|                                                               |
|   11 Agentes trabalhando juntos para evoluir a plataforma     |
|                                                               |
|   PROD & INOV: Criam features e pesquisas                     |
|   BACK & FRONT & DEVOPS: Implementam                          |
|   SEC: Revisa seguranca                                       |
|   QA: Testa tudo                                              |
|                                                               |
+---------------------------------------------------------------+
""")
    print(f"  Duracao: {config.duration_hours} horas")
    print(f"  Intervalo: {config.poll_interval}s entre ciclos")
    print(f"  Agentes simultaneos: {config.max_agents}")
    print()

    # Confirmar
    response = input("Iniciar evolucao? [S/n]: ").strip().lower()
    if response == 'n':
        print("Cancelado.")
        return

    # Executar
    orchestrator = EvolutionOrchestrator(config)
    asyncio.run(orchestrator.run())


if __name__ == "__main__":
    main()

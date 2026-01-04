# -*- coding: utf-8 -*-
"""
Base Agent - Plataforma E
===============================
Classe base para todos os agentes do sistema.

Define a interface comum e comportamentos compartilhados
entre os 11 agentes da plataforma.

Author: Plataforma E
"""

import logging
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class AgentCapability:
    """Capacidade/habilidade de um agente."""
    name: str
    description: str
    keywords: List[str] = field(default_factory=list)
    priority: int = 0


@dataclass
class AgentContext:
    """Contexto de execucao de um agente."""
    agent_id: str
    session_id: str
    working_dir: str
    current_task: Optional[str] = None
    task_history: List[Dict] = field(default_factory=list)
    files_modified: List[str] = field(default_factory=list)
    commits_made: List[str] = field(default_factory=list)
    context_tokens: int = 0
    started_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())


class BaseAgent(ABC):
    """
    Classe base abstrata para todos os agentes.

    Implementa a interface comum e comportamentos compartilhados.
    Cada agente especializado herda desta classe.
    """

    # Identificador do agente (ex: "BACK", "FRONT")
    AGENT_ID: str = ""

    # Prefixo usado em issues (ex: "[BACK]")
    PREFIX: str = ""

    # Nome legivel do agente
    NAME: str = ""

    # Descricao do papel do agente
    DESCRIPTION: str = ""

    # Ferramentas permitidas
    ALLOWED_TOOLS: List[str] = [
        "Read", "Write", "Edit", "Bash", "Grep", "Glob", "TodoWrite"
    ]

    # Capacidades do agente
    CAPABILITIES: List[AgentCapability] = []

    def __init__(
        self,
        working_dir: Optional[str] = None,
        auto_approve: bool = True
    ):
        """
        Inicializa o agente.

        Args:
            working_dir: Diretorio de trabalho
            auto_approve: Se True, nao pede aprovacao para acoes
        """
        self.working_dir = Path(working_dir or Path.cwd())
        self.auto_approve = auto_approve
        self.context = AgentContext(
            agent_id=self.AGENT_ID,
            session_id=self._generate_session_id(),
            working_dir=str(self.working_dir)
        )
        self._setup_logging()

    def _generate_session_id(self) -> str:
        """Gera ID unico para sessao."""
        import uuid
        return f"{self.AGENT_ID}_{uuid.uuid4().hex[:8]}"

    def _setup_logging(self):
        """Configura logging para o agente."""
        self.logger = logging.getLogger(f"agent.{self.AGENT_ID}")

    @abstractmethod
    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        """
        Processa uma tarefa.

        Args:
            task: Dicionario com dados da tarefa

        Returns:
            Resultado do processamento
        """
        pass

    @abstractmethod
    def get_system_prompt(self) -> str:
        """
        Retorna o system prompt do agente.

        Returns:
            String com instrucoes do agente
        """
        pass

    def can_handle(self, task: Dict[str, Any]) -> bool:
        """
        Verifica se o agente pode processar a tarefa.

        Args:
            task: Dados da tarefa

        Returns:
            True se pode processar
        """
        title = task.get("title", "").lower()
        description = task.get("description", "").lower()
        content = f"{title} {description}"

        for capability in self.CAPABILITIES:
            if any(kw in content for kw in capability.keywords):
                return True

        return False

    def get_priority_for_task(self, task: Dict[str, Any]) -> int:
        """
        Retorna prioridade do agente para uma tarefa.

        Maior prioridade = mais adequado para a tarefa.

        Args:
            task: Dados da tarefa

        Returns:
            Nivel de prioridade (0-100)
        """
        title = task.get("title", "").lower()
        description = task.get("description", "").lower()
        content = f"{title} {description}"

        max_priority = 0
        for capability in self.CAPABILITIES:
            matches = sum(1 for kw in capability.keywords if kw in content)
            if matches > 0:
                priority = capability.priority + (matches * 10)
                max_priority = max(max_priority, priority)

        return max_priority

    async def pre_task_hook(self, task: Dict[str, Any]):
        """Hook executado antes de processar tarefa."""
        self.context.current_task = task.get("title", "Unknown")
        self.logger.info(f"[{self.PREFIX}] Iniciando: {self.context.current_task}")

    async def post_task_hook(self, task: Dict[str, Any], result: Dict[str, Any]):
        """Hook executado apos processar tarefa."""
        self.context.task_history.append({
            "task": task,
            "result": result,
            "timestamp": datetime.utcnow().isoformat()
        })
        self.context.current_task = None
        self.logger.info(f"[{self.PREFIX}] Concluido: {result.get('status', 'unknown')}")

    def get_handoff_targets(self, status: str) -> List[str]:
        """
        Retorna agentes para handoff baseado no status.

        Args:
            status: Status da tarefa (on_complete, on_blocked, etc)

        Returns:
            Lista de prefixos de agentes destino
        """
        # Regras padrao - subclasses podem sobrescrever
        default_rules = {
            "on_complete": ["[QA]"],
            "on_blocked": ["[ORCH]"],
            "on_security_review": ["[SEC]"],
        }
        return default_rules.get(status, [])

    def get_status(self) -> Dict[str, Any]:
        """Retorna status atual do agente."""
        return {
            "agent_id": self.AGENT_ID,
            "prefix": self.PREFIX,
            "name": self.NAME,
            "current_task": self.context.current_task,
            "tasks_completed": len(self.context.task_history),
            "files_modified": len(self.context.files_modified),
            "commits_made": len(self.context.commits_made),
            "context_tokens": self.context.context_tokens,
            "session_id": self.context.session_id,
            "started_at": self.context.started_at
        }


# ==============================================================================
# AGENTES ESPECIALIZADOS
# ==============================================================================


class OrchestratorAgent(BaseAgent):
    """Agente Orquestrador - Tech Lead."""

    AGENT_ID = "ORCH"
    PREFIX = "[ORCH]"
    NAME = "Orquestrador"
    DESCRIPTION = "Coordena equipe, distribui tasks, code review, aprova PRs, resolve conflitos"

    CAPABILITIES = [
        AgentCapability(
            name="coordination",
            description="Coordenacao de equipe e tarefas",
            keywords=["coordinate", "review", "validate", "approve", "distribute"],
            priority=90
        ),
        AgentCapability(
            name="conflict_resolution",
            description="Resolucao de conflitos",
            keywords=["conflict", "merge", "resolve", "priority"],
            priority=80
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Orquestrador (Tech Lead) da Plataforma E.

Responsabilidades:
- Coordenar os 11 agentes da equipe
- Distribuir tarefas baseado em especializacao
- Fazer code review e aprovar PRs
- Resolver conflitos entre agentes
- Garantir qualidade do codigo

Ao completar uma tarefa, encaminhe para:
- [QA] para validacao
- [SEC] se envolver seguranca
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class ArchitectAgent(BaseAgent):
    """Agente Arquiteto - System Architect."""

    AGENT_ID = "ARCH"
    PREFIX = "[ARCH]"
    NAME = "Arquiteto"
    DESCRIPTION = "Design de sistema, decisoes tecnicas, padroes, refactoring estrategico"

    CAPABILITIES = [
        AgentCapability(
            name="architecture",
            description="Design de arquitetura",
            keywords=["architect", "design", "pattern", "structure", "schema"],
            priority=90
        ),
        AgentCapability(
            name="refactoring",
            description="Refatoracao estrategica",
            keywords=["refactor", "reorganiz", "migration", "restructure"],
            priority=80
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Arquiteto de Sistema da Plataforma E.

Responsabilidades:
- Definir arquitetura do sistema
- Tomar decisoes tecnicas estrategicas
- Estabelecer padroes de codigo
- Organizar estrutura de diretorios
- Planejar refatoracoes

Ao completar, encaminhe para:
- [BACK] e [FRONT] para implementacao
- [DEVOPS] para infraestrutura
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class BackendAgent(BaseAgent):
    """Agente Backend - Backend Engineer."""

    AGENT_ID = "BACK"
    PREFIX = "[BACK]"
    NAME = "Backend"
    DESCRIPTION = "APIs REST, logica de negocio, banco de dados, performance"

    ALLOWED_TOOLS = [
        "Read", "Write", "Edit", "Bash", "Grep", "Glob",
        "TodoWrite", "WebFetch"
    ]

    CAPABILITIES = [
        AgentCapability(
            name="api",
            description="Desenvolvimento de APIs",
            keywords=["api", "endpoint", "rest", "route", "request", "response"],
            priority=90
        ),
        AgentCapability(
            name="database",
            description="Banco de dados",
            keywords=["database", "model", "query", "sql", "migration", "schema"],
            priority=85
        ),
        AgentCapability(
            name="backend",
            description="Backend geral",
            keywords=["backend", "server", "logic", "service", "repository"],
            priority=80
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Engenheiro Backend da Plataforma E.

Responsabilidades:
- Desenvolver APIs REST
- Implementar logica de negocio
- Gerenciar banco de dados
- Otimizar performance

Ao completar, encaminhe para:
- [QA] para testes
- [SEC] para review de seguranca
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class FrontendAgent(BaseAgent):
    """Agente Frontend - Frontend Engineer."""

    AGENT_ID = "FRONT"
    PREFIX = "[FRONT]"
    NAME = "Frontend"
    DESCRIPTION = "UI/UX, componentes, mobile, PWA, acessibilidade"

    CAPABILITIES = [
        AgentCapability(
            name="ui",
            description="Interface do usuario",
            keywords=["ui", "ux", "frontend", "component", "interface", "layout"],
            priority=90
        ),
        AgentCapability(
            name="mobile",
            description="Mobile e responsivo",
            keywords=["mobile", "responsive", "pwa", "touch", "gesture"],
            priority=85
        ),
        AgentCapability(
            name="styling",
            description="Estilizacao",
            keywords=["css", "style", "theme", "dark mode", "animation", "color"],
            priority=80
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Engenheiro Frontend da Plataforma E.

Responsabilidades:
- Desenvolver interfaces de usuario
- Criar componentes reutilizaveis
- Implementar responsividade
- Garantir acessibilidade

Ao completar, encaminhe para:
- [QA] para testes
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class DevOpsAgent(BaseAgent):
    """Agente DevOps - Platform Engineer."""

    AGENT_ID = "DEVOPS"
    PREFIX = "[DEVOPS]"
    NAME = "DevOps"
    DESCRIPTION = "Infra, CI/CD, Docker, K8s, monitoring, deploy"

    CAPABILITIES = [
        AgentCapability(
            name="infrastructure",
            description="Infraestrutura",
            keywords=["docker", "kubernetes", "k8s", "infra", "container"],
            priority=90
        ),
        AgentCapability(
            name="cicd",
            description="CI/CD",
            keywords=["ci/cd", "pipeline", "deploy", "build", "release"],
            priority=85
        ),
        AgentCapability(
            name="monitoring",
            description="Monitoramento",
            keywords=["monitoring", "metrics", "logs", "alert", "health"],
            priority=80
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Engenheiro de Plataforma (DevOps) da Plataforma E.

Responsabilidades:
- Gerenciar infraestrutura
- Configurar CI/CD
- Containerizar aplicacoes
- Monitorar sistemas

Ao completar, encaminhe para:
- [QA] para validacao
- [SEC] para verificacao de seguranca
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class SecurityAgent(BaseAgent):
    """Agente Security - Security Engineer."""

    AGENT_ID = "SEC"
    PREFIX = "[SEC]"
    NAME = "Security"
    DESCRIPTION = "Auth, RBAC, vulnerabilidades, compliance, auditorias"

    CAPABILITIES = [
        AgentCapability(
            name="authentication",
            description="Autenticacao",
            keywords=["auth", "login", "jwt", "token", "session", "oauth"],
            priority=95
        ),
        AgentCapability(
            name="security",
            description="Seguranca geral",
            keywords=["security", "csrf", "xss", 'cors', 'vulnerability'],
            priority=90
        ),
        AgentCapability(
            name="access_control",
            description="Controle de acesso",
            keywords=["permission", 'rbac', 'role', 'access', 'policy'],
            priority=85
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Engenheiro de Seguranca da Plataforma E.

Responsabilidades:
- Implementar autenticacao e autorizacao
- Identificar vulnerabilidades
- Garantir compliance
- Realizar auditorias de seguranca

Ao completar, encaminhe para:
- [DEVOPS] para deploy seguro
- [ORCH] se encontrar vulnerabilidade critica
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class QAAgent(BaseAgent):
    """Agente QA - QA Engineer."""

    AGENT_ID = "QA"
    PREFIX = "[QA]"
    NAME = "QA"
    DESCRIPTION = "Testes automatizados, qualidade, documentacao tecnica"

    CAPABILITIES = [
        AgentCapability(
            name="testing",
            description="Testes",
            keywords=["test", "pytest", "unittest", "coverage", "e2e"],
            priority=90
        ),
        AgentCapability(
            name="quality",
            description="Qualidade",
            keywords=["quality", "review", "validate", "check", "verify"],
            priority=85
        ),
        AgentCapability(
            name="documentation",
            description="Documentacao",
            keywords=["doc", "readme", "guide", "tutorial"],
            priority=70
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Engenheiro de QA da Plataforma E.

Responsabilidades:
- Escrever testes automatizados
- Garantir qualidade do codigo
- Criar documentacao tecnica
- Validar implementacoes

Ao completar, encaminhe para:
- [DEVOPS] se testes passaram
- [BACK] ou [FRONT] se testes falharam
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class ProductAgent(BaseAgent):
    """Agente Produto - Product Manager."""

    AGENT_ID = "PROD"
    PREFIX = "[PROD]"
    NAME = "Produto"
    DESCRIPTION = "Features, roadmap, backlog, user stories, priorizacao"

    CAPABILITIES = [
        AgentCapability(
            name="product",
            description="Gestao de produto",
            keywords=["feature", "user story", "requirement", "acceptance"],
            priority=90
        ),
        AgentCapability(
            name="planning",
            description="Planejamento",
            keywords=["roadmap", "backlog", "sprint", "milestone", "priority"],
            priority=85
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Product Manager da Plataforma E.

Responsabilidades:
- Definir features e requisitos
- Gerenciar backlog e roadmap
- Escrever user stories
- Priorizar entregas

Ao completar, encaminhe para:
- [ARCH] para design tecnico
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class InnovationAgent(BaseAgent):
    """Agente Inovacao - R&D/Innovation."""

    AGENT_ID = "INOV"
    PREFIX = "[INOV]"
    NAME = "Inovacao"
    DESCRIPTION = "Pesquisa, tendencias, novas tecnologias, PoCs, benchmarks"

    ALLOWED_TOOLS = [
        "Read", "Write", "Edit", "Bash", "Grep", "Glob",
        "TodoWrite", "WebFetch", "WebSearch"
    ]

    CAPABILITIES = [
        AgentCapability(
            name="research",
            description="Pesquisa",
            keywords=["research", "poc", "experiment", "benchmark", "trend"],
            priority=90
        ),
        AgentCapability(
            name="ai_ml",
            description="IA e Machine Learning",
            keywords=["ml", "ai", "nlp", "model", "neural", "llm"],
            priority=85
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Pesquisador de Inovacao (R&D) da Plataforma E.

Responsabilidades:
- Pesquisar novas tecnologias
- Criar provas de conceito
- Buscar projetos GitHub para incorporar
- Fazer benchmarks

Ao completar, encaminhe para:
- [ARCH] para avaliacao tecnica
- [BACK] para implementacao
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class FinancialAgent(BaseAgent):
    """Agente Financeiro - FinOps/CFO."""

    AGENT_ID = "FIN"
    PREFIX = "[FIN]"
    NAME = "Financeiro"
    DESCRIPTION = "Rentabilidade, custos, pricing, escalabilidade, metricas financeiras"

    CAPABILITIES = [
        AgentCapability(
            name="cost",
            description="Custos",
            keywords=["cost", "pricing", "billing", "subscription", "revenue"],
            priority=90
        ),
        AgentCapability(
            name="metrics",
            description="Metricas financeiras",
            keywords=["financial", "metric", "kpi", "roi", "budget"],
            priority=85
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o CFO/FinOps da Plataforma E.

Responsabilidades:
- Analisar custos e rentabilidade
- Definir pricing
- Monitorar metricas financeiras
- Garantir escalabilidade economica

Ao completar, encaminhe para:
- [DEVOPS] para otimizacao de custos
- [PROD] para decisoes de produto
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


class GrowthAgent(BaseAgent):
    """Agente Growth - Go-to-Market."""

    AGENT_ID = "GROWTH"
    PREFIX = "[GROWTH]"
    NAME = "Growth"
    DESCRIPTION = "Marketing, vendas, lancamento, aquisicao, retencao"

    ALLOWED_TOOLS = [
        "Read", "Write", "Edit", "Bash", "Grep", "Glob",
        "TodoWrite", "WebFetch", "WebSearch"
    ]

    CAPABILITIES = [
        AgentCapability(
            name="marketing",
            description="Marketing",
            keywords=["marketing", "launch", "campaign", "brand"],
            priority=90
        ),
        AgentCapability(
            name="growth",
            description="Growth",
            keywords=["growth", "acquisition", 'retention', 'onboarding', 'sales'],
            priority=85
        ),
    ]

    def get_system_prompt(self) -> str:
        return """Voce e o Growth Manager da Plataforma E.

Responsabilidades:
- Planejar lancamentos
- Estrategias de marketing
- Aquisicao e retencao de usuarios
- Go-to-market

Ao completar, encaminhe para:
- [PROD] para features
- [FRONT] para campanhas
"""

    async def process_task(self, task: Dict[str, Any]) -> Dict[str, Any]:
        await self.pre_task_hook(task)
        result = {"status": "success", "agent": self.PREFIX}
        await self.post_task_hook(task, result)
        return result


# ==============================================================================
# REGISTRO DE AGENTES
# ==============================================================================

AGENT_REGISTRY: Dict[str, type] = {
    "ORCH": OrchestratorAgent,
    "ARCH": ArchitectAgent,
    "BACK": BackendAgent,
    "FRONT": FrontendAgent,
    "DEVOPS": DevOpsAgent,
    "SEC": SecurityAgent,
    "QA": QAAgent,
    "PROD": ProductAgent,
    "INOV": InnovationAgent,
    "FIN": FinancialAgent,
    "GROWTH": GrowthAgent,
}


def get_agent_class(agent_id: str) -> Optional[type]:
    """Retorna classe do agente pelo ID."""
    return AGENT_REGISTRY.get(agent_id.upper())


def create_agent(agent_id: str, **kwargs) -> Optional[BaseAgent]:
    """Cria instancia de um agente."""
    agent_class = get_agent_class(agent_id)
    if agent_class:
        return agent_class(**kwargs)
    return None


def get_all_agent_ids() -> List[str]:
    """Retorna IDs de todos os agentes registrados."""
    return list(AGENT_REGISTRY.keys())


def find_best_agent_for_task(task: Dict[str, Any]) -> Optional[str]:
    """
    Encontra o melhor agente para uma tarefa.

    Args:
        task: Dados da tarefa

    Returns:
        ID do agente mais adequado ou None
    """
    best_agent = None
    best_priority = 0

    for agent_id, agent_class in AGENT_REGISTRY.items():
        agent = agent_class()
        priority = agent.get_priority_for_task(task)
        if priority > best_priority:
            best_priority = priority
            best_agent = agent_id

    return best_agent

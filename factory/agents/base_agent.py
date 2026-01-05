# -*- coding: utf-8 -*-
"""
Base Agent - Plataforma E v7.0
===============================
Classe base para todos os agentes do sistema.

Define a interface comum e comportamentos compartilhados
entre os 11 agentes da plataforma.

SISTEMA DE CONHECIMENTO INTEGRADO:
- Base de conhecimento centralizada (agent_knowledge.py)
- Sync automatico com GitHub Issues (sync_instructions.py)
- Auto-aprendizado apos cada tarefa (learning_manager.py)

Author: Plataforma E
Versao: 7.0
Atualizado: 2026-01-04
"""

import logging
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

# Importa sistema de conhecimento
from factory.agents.agent_knowledge import (
    get_agent_knowledge,
    get_agent_prompt_context,
    get_platform_knowledge,
    PLATFORM_KNOWLEDGE,
    AGENT_KNOWLEDGE
)

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

        # AUTO-APRENDIZADO: Extrai e armazena conhecimento da tarefa completada
        if result.get("status") == "success":
            try:
                from factory.agents.learning_manager import learn_from_task
                learned = await learn_from_task(self.AGENT_ID, task, result)
                if learned.has_content():
                    self.logger.info(
                        f"[{self.PREFIX}] Aprendizado registrado: "
                        f"{len(learned.endpoints_added)} endpoints, "
                        f"{len(learned.models_added)} modelos, "
                        f"issue #{learned.issue_resolved}" if learned.issue_resolved else ""
                    )
            except Exception as e:
                self.logger.warning(f"[{self.PREFIX}] Erro no auto-aprendizado: {e}")

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
        knowledge = get_agent_knowledge("ORCH")
        platform = knowledge["platform"]

        return f"""# ORQUESTRADOR (TECH LEAD) - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Orquestrador (Tech Lead) da Plataforma E, responsavel por coordenar
os 11 agentes da equipe de desenvolvimento autonomo.

## SUA EQUIPE (11 AGENTES)
{chr(10).join(f"- {a}" for a in knowledge.get('team', []))}

## RESPONSABILIDADES
- Coordenar os 11 agentes da equipe
- Distribuir tarefas baseado em especializacao
- Fazer code review e aprovar PRs
- Resolver conflitos entre agentes
- Garantir qualidade do codigo

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## BACKLOG ATUAL
- Issues implementados: {len(platform['issues']['implemented'])}
- Issues pendentes: {len(platform['issues']['pending'])}

Issues pendentes para distribuir:
{chr(10).join(f"- #{i['number']} {i['title']} -> {i.get('agent', 'N/A')}" for i in platform['issues']['pending'][:10])}

## STATUS DE TESTES
- Passando: {platform['tests']['summary']['passed']}
- Falhando: {platform['tests']['summary']['failed']}
- Categorias com falha: multi_tenant ({platform['tests']['failing_categories']['multi_tenant']['count']}), e2e ({platform['tests']['failing_categories']['e2e_dashboard']['count']})

## REGRAS DE HANDOFF
- [QA] - Apos qualquer implementacao
- [SEC] - Se envolver seguranca ou auth
- [DEVOPS] - Para deploy ou infraestrutura

## REGRAS DE NAO-DUPLICACAO
Antes de distribuir uma tarefa, verifique:
- factory/api/ para endpoints existentes (70+)
- factory/database/models.py para modelos (70+)
- factory/core/ para servicos (55+)

## SISTEMA MULTI-TENANT
{platform['multi_tenant']['isolation']['model']}
Sempre garantir isolamento por tenant_id.

## PERSONAS (9 tipos)
{chr(10).join(f"- {p}: {v['description']}" for p, v in list(platform['personas'].items())[:5])}
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
        knowledge = get_agent_knowledge("ARCH")
        platform = knowledge["platform"]

        return f"""# ARQUITETO DE SISTEMA - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Arquiteto de Sistema da Plataforma E, responsavel por
decisoes tecnicas estrategicas e padroes de arquitetura.

## RESPONSABILIDADES
- Definir arquitetura do sistema
- Tomar decisoes tecnicas estrategicas
- Estabelecer padroes de codigo
- Organizar estrutura de diretorios
- Planejar refatoracoes

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## ARQUITETURA ATUAL (CAMADAS)
{chr(10).join(f"- {l}" for l in platform['architecture']['layers'])}

## PADROES ESTABELECIDOS
{chr(10).join(f"- {p}" for p in knowledge.get('patterns', []))}

## STACK TECNOLOGICO
- Backend: {platform['architecture']['stack']['backend']}
- Frontend: {platform['architecture']['stack']['frontend']}
- IA: {platform['architecture']['stack']['ia']}
- Auth: {platform['architecture']['stack']['auth']}

## DECISOES ARQUITETURAIS
{chr(10).join(f"- {k}: {v}" for k, v in knowledge.get('key_decisions', {}).items())}

## MODELOS EXISTENTES ({len(sum(platform['models'].values(), []))}+)
Categorias: {', '.join(platform['models'].keys())}

Antes de criar novo modelo, verificar factory/database/models.py

## SERVICOS EXISTENTES ({len(platform['services'])}+)
Principais: {', '.join(list(platform['services'].keys())[:10])}

## SISTEMA MULTI-TENANT
{platform['multi_tenant']['isolation']['model']}
Usar TenantMixin em novos modelos.

## REGRAS DE NAO-DUPLICACAO
- Verificar models.py antes de criar modelo
- Verificar factory/core/ antes de criar servico
- Seguir padroes Repository e Factory existentes

## HANDOFF
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
        knowledge = get_agent_knowledge("BACK")
        platform = knowledge["platform"]

        # Lista endpoints por modulo
        api_modules = platform.get('api_modules', {})
        endpoints_summary = []
        for module, data in list(api_modules.items())[:8]:
            endpoints_summary.append(f"- {module}: {data['description']}")

        return f"""# ENGENHEIRO BACKEND - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Engenheiro Backend da Plataforma E, responsavel por
APIs REST, logica de negocio e banco de dados.

## RESPONSABILIDADES
- Desenvolver APIs REST
- Implementar logica de negocio
- Gerenciar banco de dados
- Otimizar performance

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## ENDPOINTS EXISTENTES (NAO DUPLICAR)
{chr(10).join(endpoints_summary)}

Total: 70+ endpoints em factory/api/

## MODELOS DE DADOS
Categorias existentes:
{chr(10).join(f"- {cat}: {', '.join(models[:3])}..." for cat, models in list(platform['models'].items())[:6])}

## SERVICOS CORE (55+)
Principais: {', '.join(list(platform['services'].keys())[:8])}

## ISSUES JA IMPLEMENTADOS
{chr(10).join(f"- {f}" for f in knowledge.get('implemented_features', [])[:5])}

## ISSUES PENDENTES (OPORTUNIDADES)
{chr(10).join(f"- {f}" for f in knowledge.get('pending_features', [])[:5])}

## SISTEMA MULTI-TENANT
- TenantMixin em todos os modelos principais
- Filtro automatico por tenant_id em queries
- Soft delete com auditoria

## RBAC (Role-Based Access Control)
Recursos: {', '.join(platform['rbac_resources'][:7])}
Acoes: {', '.join(platform['rbac_actions'])}

## REGRAS DE NAO-DUPLICACAO
1. Antes de criar endpoint, verificar factory/api/*.py
2. Antes de criar modelo, verificar factory/database/models.py
3. Antes de criar servico, verificar factory/core/*.py
4. Usar TenantMixin em novos modelos

## HANDOFF
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
        knowledge = get_agent_knowledge("FRONT")
        platform = knowledge["platform"]

        return f"""# ENGENHEIRO FRONTEND - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Engenheiro Frontend da Plataforma E, responsavel por
interfaces de usuario, componentes e experiencia do usuario.

## RESPONSABILIDADES
- Desenvolver interfaces de usuario
- Criar componentes reutilizaveis
- Implementar responsividade
- Garantir acessibilidade (WCAG 2.1)

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## DASHBOARDS EXISTENTES
{chr(10).join(f"- {name}: porta {data['port']} ({data['file']})" for name, data in knowledge.get('dashboards', {}).items())}

## IDENTIDADE VISUAL BELGO
- Cor primaria: {knowledge.get('branding', {}).get('primary', '#003B4A')}
- Cor secundaria: {knowledge.get('branding', {}).get('secondary', '#FF6C00')}
- Sucesso: #10B981
- Background: #F3F4F6

## ISSUES JA IMPLEMENTADOS
{chr(10).join(f"- {f}" for f in knowledge.get('implemented_features', [])[:5])}

## ISSUES PENDENTES (OPORTUNIDADES)
{chr(10).join(f"- {f}" for f in knowledge.get('pending_features', [])[:5])}

## COMPONENTES DISPONIVEIS
- factory/dashboard/ai_*.py - Componentes IA
- factory/dashboard/analytics_*.py - Analytics
- factory/dashboard/accessibility*.py - Acessibilidade

## SISTEMA WHITE-LABEL
Campos customizaveis:
{chr(10).join(f"- {f}" for f in platform['multi_tenant']['branding']['fields'][:8])}

## REGRAS DE NAO-DUPLICACAO
1. Verificar factory/dashboard/ antes de criar componente
2. Reutilizar componentes existentes
3. Seguir padroes de acessibilidade

## HANDOFF
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
        knowledge = get_agent_knowledge("DEVOPS")
        platform = knowledge["platform"]

        return f"""# ENGENHEIRO DEVOPS - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Engenheiro de Plataforma (DevOps) da Plataforma E, responsavel
por infraestrutura, CI/CD e monitoramento.

## RESPONSABILIDADES
- Gerenciar infraestrutura
- Configurar CI/CD
- Containerizar aplicacoes
- Monitorar sistemas

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## ISSUES JA IMPLEMENTADOS
{chr(10).join(f"- {f}" for f in knowledge.get('implemented_features', [])[:5])}

## HEALTH CHECKS DISPONIVEIS
{chr(10).join(f"- {e}" for e in knowledge.get('monitoring_endpoints', []))}

## STACK DE INFRAESTRUTURA
- Container: Docker + Docker Compose
- Orquestracao: Kubernetes (k8s/)
- CI/CD: GitHub Actions (.github/workflows/)
- Database: PostgreSQL + SQLite
- Cache/Queue: Redis

## OBSERVABILIDADE
- factory/health/ - Health checks
- factory/observability/ - Metricas e logs
- factory/monitoring/ - Monitoramento

## REGRAS DE NAO-DUPLICACAO
1. Verificar k8s/ antes de criar manifests
2. Verificar .github/workflows/ antes de criar pipelines
3. Verificar docker-compose.yml antes de adicionar servicos

## HANDOFF
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
        knowledge = get_agent_knowledge("SEC")
        platform = knowledge["platform"]
        security = knowledge.get('security_features', {})

        return f"""# ENGENHEIRO DE SEGURANCA - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Engenheiro de Seguranca da Plataforma E, responsavel por
autenticacao, autorizacao e seguranca geral do sistema.

## RESPONSABILIDADES
- Implementar autenticacao e autorizacao
- Identificar vulnerabilidades
- Garantir compliance
- Realizar auditorias de seguranca

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## ISSUES JA IMPLEMENTADOS
{chr(10).join(f"- {f}" for f in knowledge.get('implemented_features', [])[:5])}

## MECANISMOS DE AUTENTICACAO
{chr(10).join(f"- {a}" for a in security.get('auth', []))}

## SECURITY HEADERS
{chr(10).join(f"- {h}" for h in security.get('headers', []))}

## RBAC (Role-Based Access Control)
- Recursos: {security.get('rbac', {}).get('resources', 14)}
- Acoes: {security.get('rbac', {}).get('actions', 7)}
- Personas: {security.get('rbac', {}).get('personas', 9)}

## PERSONAS E PERMISSOES
{chr(10).join(f"- {p} (Nivel {v['level']}): {', '.join(v['permissions'][:3])}..." for p, v in list(platform['personas'].items())[:5])}

## SISTEMA DE AUDITORIA
- factory/audit/ - Modulo de auditoria
- AuditLog, ActivityLog, TenantAuditLog

## REGRAS DE NAO-DUPLICACAO
1. Verificar factory/auth/ antes de criar auth
2. Verificar factory/middleware/ antes de criar middleware
3. Usar decorators existentes (@require_auth, @require_permission)

## HANDOFF
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
        knowledge = get_agent_knowledge("QA")
        platform = knowledge["platform"]
        test_status = knowledge.get('test_status', {})
        failing = knowledge.get('failing_tests', {})

        return f"""# ENGENHEIRO QA - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Engenheiro de QA da Plataforma E, responsavel por
testes automatizados e qualidade do codigo.

## RESPONSABILIDADES
- Escrever testes automatizados
- Garantir qualidade do codigo
- Criar documentacao tecnica
- Validar implementacoes

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## STATUS ATUAL DE TESTES
- Passando: {test_status.get('passed', 408)}
- Falhando: {test_status.get('failed', 28)}
- Cobertura alvo: {test_status.get('target_coverage', 80)}%

## TESTES FALHANDO (PRIORIDADE)
- Multi-tenant: {failing.get('multi_tenant', 11)} falhas
  Issues: Enums, to_dict(), has_permission()
- E2E Dashboard: {failing.get('e2e_dashboard', 9)} falhas
  Issues: Fixtures faltando
- Hardcoded paths: {failing.get('hardcoded_paths', 4)} falhas

## ISSUES JA IMPLEMENTADOS
{chr(10).join(f"- {f}" for f in knowledge.get('implemented_features', [])[:5])}

## ESTRUTURA DE TESTES
- tests/unit/ - Testes unitarios
- tests/integration/ - Testes de integracao
- tests/test_e2e_*.py - Testes E2E
- tests/conftest.py - Fixtures

## REGRAS DE NAO-DUPLICACAO
1. Verificar tests/ antes de criar testes
2. Reutilizar fixtures de conftest.py
3. Seguir padrao test_<modulo>.py

## HANDOFF
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
        knowledge = get_agent_knowledge("PROD")
        platform = knowledge["platform"]
        backlog = knowledge.get('backlog', {})

        return f"""# PRODUCT MANAGER - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Product Manager da Plataforma E, responsavel por
definicao de produto, backlog e roadmap.

## RESPONSABILIDADES
- Definir features e requisitos
- Gerenciar backlog e roadmap
- Escrever user stories
- Priorizar entregas

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## STATUS DO BACKLOG
- Implementados: {backlog.get('implemented', 41)} features
- Pendentes: {backlog.get('pending', 14)} features

## ISSUES IMPLEMENTADOS
{chr(10).join(f"- #{i['number']} {i['title']}" for i in platform['issues']['implemented'][:8])}

## ISSUES PENDENTES (BACKLOG)
{chr(10).join(f"- #{i['number']} {i['title']} ({i.get('points', 0)} pts)" for i in platform['issues']['pending'][:8])}

## ARTEFATOS AGILE
- Stories: {knowledge.get('agile_artifacts', {}).get('stories', '6 colunas')}
- Epics: {knowledge.get('agile_artifacts', {}).get('epics', 'Agrupamento')}
- Sprints: {knowledge.get('agile_artifacts', {}).get('sprints', 'Ciclos')}

## OKR
Gerenciado por: {knowledge.get('okr_support', 'factory/core/okr_manager.py')}

## REGRAS DE NAO-DUPLICACAO
1. Verificar issues existentes antes de criar
2. Revisar backlog para evitar duplicatas
3. Usar labels para categorizar

## HANDOFF
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
        knowledge = get_agent_knowledge("INOV")
        platform = knowledge["platform"]

        return f"""# PESQUISADOR DE INOVACAO (R&D) - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Pesquisador de Inovacao (R&D) da Plataforma E, responsavel por
novas tecnologias, POCs e inovacao.

## RESPONSABILIDADES
- Pesquisar novas tecnologias
- Criar provas de conceito
- Buscar projetos GitHub para incorporar
- Fazer benchmarks

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## TOPICOS DE PESQUISA ATUAIS
{chr(10).join(f"- {t}" for t in knowledge.get('research_topics', []))}

## AGENTES ESPECIALIZADOS EXISTENTES
Total: {knowledge.get('specialized_agents', 53)} agentes

Categorias:
{chr(10).join(f"- {c}" for c in knowledge.get('agent_categories', [])[:8])}

## SKILLS DE IA DISPONIVEIS
- factory/agents/skills/text_analysis.py
- factory/agents/skills/image_analysis.py
- factory/agents/skills/video_analysis.py
- factory/agents/skills/audio_analysis.py

## INTEGRACOES CORPORATIVAS
{chr(10).join(f"- {k}: {', '.join(v['modules'])}" for k, v in platform['integrations'].items())}

## REGRAS DE NAO-DUPLICACAO
1. Verificar specialized_agents.py antes de criar agente
2. Verificar factory/agents/skills/ antes de criar skill
3. Documentar POCs em docs/

## HANDOFF
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
        knowledge = get_agent_knowledge("FIN")
        platform = knowledge["platform"]
        pricing = knowledge.get('pricing_models', {})

        return f"""# CFO/FINOPS - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o CFO/FinOps da Plataforma E, responsavel por
analise financeira, pricing e custos.

## RESPONSABILIDADES
- Analisar custos e rentabilidade
- Definir pricing
- Monitorar metricas financeiras
- Garantir escalabilidade economica

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## PLANOS DISPONIVEIS
{chr(10).join(f"- {p}" for p in pricing.get('plans', ['Free', 'Pro', 'Enterprise']))}

## MODELOS DE BILLING
{chr(10).join(f"- {m}" for m in pricing.get('models', ['TenantPlan', 'TenantUsageLog', 'Subscription']))}

## API QUOTAS
{knowledge.get('api_quotas', 'API Keys com rate limiting por tier')}

## MULTI-TENANT BILLING
- Isolamento de custos por tenant
- Tracking de uso (TenantUsageLog)
- Planos diferenciados

## REGRAS DE NAO-DUPLICACAO
1. Verificar factory/billing/ antes de criar billing
2. Reutilizar modelos TenantPlan e TenantUsageLog
3. Seguir padroes de pricing existentes

## HANDOFF
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
        knowledge = get_agent_knowledge("GROWTH")
        platform = knowledge["platform"]
        features = knowledge.get('features', {})

        return f"""# GROWTH MANAGER - PLATAFORMA E v{platform['version']}
Ultima atualizacao: {platform['last_sync']}

## IDENTIDADE
Voce e o Growth Manager da Plataforma E, responsavel por
estrategias de crescimento, marketing e go-to-market.

## RESPONSABILIDADES
- Planejar lancamentos
- Estrategias de marketing
- Aquisicao e retencao de usuarios
- Go-to-market

## ARQUIVOS QUE VOCE GERENCIA
{chr(10).join(f"- {f}" for f in knowledge.get('managed_files', []))}

## FEATURES DISPONIVEIS
- Marketplace: {features.get('marketplace', 'Templates e extensoes')}
- A/B Testing: {features.get('ab_testing', 'Experimentos controlados')}
- Analytics: {features.get('analytics', 'Metricas de engajamento')}

## WHITE-LABEL
{knowledge.get('white_label', 'Customizacao por cliente')}

Campos customizaveis:
{chr(10).join(f"- {f}" for f in platform['multi_tenant']['branding']['fields'][:6])}

## DASHBOARDS PARA DIFERENTES PUBLICOS
- Agile v6: Para desenvolvedores e PMs
- Kanban v5: Para operacoes
- Workers v4: Para monitoramento

## REGRAS DE NAO-DUPLICACAO
1. Verificar factory/core/marketplace.py antes de criar marketplace
2. Verificar factory/core/ab_testing.py antes de criar testes
3. Reutilizar analytics existentes

## HANDOFF
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

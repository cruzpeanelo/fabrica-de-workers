# 🏗️ Arquitetura da Plataforma E

> Documentação técnica completa com decisões arquiteturais, motivos e benefícios

**Versão:** 7.0
**Última Atualização:** Janeiro 2026
**Baseado em:** Análise de 430+ issues implementados

---

## 📋 Índice

1. [Visão Geral](#visão-geral)
2. [Princípios Arquiteturais](#princípios-arquiteturais)
3. [Estrutura de Módulos](#estrutura-de-módulos)
4. [Padrões de Design](#padrões-de-design)
5. [Decisões Técnicas](#decisões-técnicas)
6. [Fluxos de Dados](#fluxos-de-dados)
7. [Segurança](#segurança)
8. [Multi-Tenancy](#multi-tenancy)
9. [Integrações](#integrações)
10. [Observabilidade](#observabilidade)
11. [Deployment](#deployment)

---

## 🎯 Visão Geral

A **Plataforma E** é uma plataforma de desenvolvimento autônomo que utiliza Inteligência Artificial (Claude da Anthropic) para automatizar o ciclo completo de desenvolvimento de software.

### Diagrama de Alto Nível

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           FÁBRICA DE AGENTES v7.0                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                    CAMADA DE APRESENTAÇÃO                             │  │
│  │  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────┐   │  │
│  │  │ Dashboard Agile │  │   REST API      │  │    WebSocket        │   │  │
│  │  │   (Vue.js)      │  │   (FastAPI)     │  │   (Real-time)       │   │  │
│  │  │   Porta 9001    │  │   50+ endpoints │  │   Notificações      │   │  │
│  │  └─────────────────┘  └─────────────────┘  └─────────────────────┘   │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                      │                                      │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                    CAMADA DE APLICAÇÃO                                │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐    │  │
│  │  │ Autonomous  │ │    Job      │ │   Project   │ │   Tenant    │    │  │
│  │  │    Loop     │ │   Queue     │ │   Manager   │ │   Service   │    │  │
│  │  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘    │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐    │  │
│  │  │    RBAC     │ │   Audit     │ │  Analytics  │ │  Workflow   │    │  │
│  │  │   Service   │ │   Logger    │ │   Service   │ │   Engine    │    │  │
│  │  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘    │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                      │                                      │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                      CAMADA DE DOMÍNIO                                │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐    │  │
│  │  │   Project   │ │    Story    │ │    Task     │ │    User     │    │  │
│  │  │   Entity    │ │   Entity    │ │   Entity    │ │   Entity    │    │  │
│  │  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘    │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐    │  │
│  │  │   Tenant    │ │    Job      │ │   Worker    │ │   Sprint    │    │  │
│  │  │   Entity    │ │   Entity    │ │   Entity    │ │   Entity    │    │  │
│  │  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘    │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                      │                                      │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │                   CAMADA DE INFRAESTRUTURA                            │  │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐    │  │
│  │  │ PostgreSQL  │ │    Redis    │ │  Claude AI  │ │ Integrações │    │  │
│  │  │  (SQLite)   │ │   (Cache)   │ │   (LLM)     │ │  Externas   │    │  │
│  │  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘    │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Estatísticas do Projeto

| Métrica | Valor |
|---------|-------|
| **Arquivos Python** | 200+ |
| **Módulos** | 24 principais |
| **Endpoints API** | 50+ |
| **Modelos de Dados** | 50+ |
| **Integrações** | 15+ |
| **Issues Resolvidos** | 430+ |

---

## 🎯 Princípios Arquiteturais

### 1. Clean Architecture (Arquitetura Limpa)

**O que é:** Separação em 4 camadas independentes com dependências direcionadas para o centro.

**Por que escolhemos:**
- **Testabilidade**: Cada camada pode ser testada isoladamente
- **Manutenibilidade**: Mudanças em uma camada não afetam outras
- **Flexibilidade**: Pode trocar banco de dados ou framework sem reescrever lógica de negócio
- **Escalabilidade**: Times podem trabalhar em camadas diferentes simultaneamente

**Benefícios obtidos:**
- Migração de SQLite para PostgreSQL sem alterar código de negócio
- Adição de Redis cache sem modificar repositories
- Troca de templates por Vue.js sem afetar API

```
┌─────────────────────────────────────────────────────────────────┐
│                    PRESENTATION                                  │
│           (FastAPI, Vue.js, WebSocket)                          │
│    Responsabilidade: Interface com usuário                      │
├─────────────────────────────────────────────────────────────────┤
│                    APPLICATION                                   │
│           (Services, Managers, Use Cases)                       │
│    Responsabilidade: Orquestração de fluxos                     │
├─────────────────────────────────────────────────────────────────┤
│                      DOMAIN                                      │
│           (Entities, Value Objects, Interfaces)                 │
│    Responsabilidade: Regras de negócio                          │
├─────────────────────────────────────────────────────────────────┤
│                   INFRASTRUCTURE                                 │
│           (Database, Cache, External APIs)                      │
│    Responsabilidade: Detalhes técnicos                          │
└─────────────────────────────────────────────────────────────────┘
```

### 2. Domain-Driven Design (DDD)

**O que é:** Modelagem do software baseada no domínio do negócio.

**Por que escolhemos:**
- Software reflete a linguagem do negócio (Story, Sprint, Epic)
- Desenvolvedores e POs falam a mesma língua
- Mudanças no negócio são fáceis de mapear para código

**Benefícios obtidos:**
- API intuitiva: `POST /api/stories` em vez de `POST /api/items`
- Modelos refletem conceitos Agile reais
- Onboarding de novos devs mais rápido

### 3. Separation of Concerns (SoC)

**O que é:** Cada módulo tem uma responsabilidade única e bem definida.

**Por que escolhemos:**
- Código mais fácil de entender
- Bugs isolados em módulos específicos
- Reutilização de componentes

**Exemplo prático:**
```
factory/
├── auth/           # APENAS autenticação e autorização
├── database/       # APENAS persistência de dados
├── integrations/   # APENAS conexão com sistemas externos
└── dashboard/      # APENAS interface web
```

### 4. Fail-Fast & Auto-Healing

**O que é:** Sistema detecta erros rapidamente e tenta corrigi-los automaticamente.

**Por que escolhemos:**
- Desenvolvimento autônomo requer auto-correção
- Reduz intervenção humana
- Melhora qualidade do código gerado

**Implementação:**
```python
# Autonomous Loop com Auto-Healing
for attempt in range(MAX_RETRIES):
    result = generate_code()
    if lint_errors := run_linter():
        result = fix_errors(lint_errors)  # Claude corrige
    if test_errors := run_tests():
        result = fix_errors(test_errors)  # Claude corrige
    else:
        break  # Sucesso!
```

---

## 📁 Estrutura de Módulos

### Mapa Completo

```
factory/
│
├── admin/                      # Administração de usuários
│   └── user_admin.py           # CRUD de usuários admin
│
├── agents/                     # Sistema de Agentes IA
│   ├── agent_factory.py        # Factory para criação de agentes
│   ├── core/                   # Runtime e execução
│   │   ├── autonomous_agent.py # Agentes autônomos
│   │   ├── agent_runtime.py    # Runtime de execução
│   │   └── task_executor.py    # Executor de tarefas
│   ├── knowledge/              # Base de conhecimento
│   │   ├── knowledge_base.py   # Armazenamento de conhecimento
│   │   └── retrieval.py        # RAG - Retrieval Augmented Generation
│   ├── learning/               # Aprendizado contínuo
│   │   ├── learning_engine.py  # Engine de aprendizado
│   │   └── feedback.py         # Sistema de feedback
│   ├── memory/                 # Sistemas de memória
│   │   ├── episodic_memory.py  # Memória de longo prazo
│   │   └── working_memory.py   # Memória de curto prazo
│   └── skills/                 # Habilidades dos agentes
│       ├── text/               # Processamento de texto
│       ├── image/              # Análise de imagens
│       ├── video/              # Processamento de vídeo
│       └── audio/              # Processamento de áudio
│
├── ai/                         # Integração com LLMs
│   ├── claude_client.py        # Cliente Claude (principal)
│   ├── azure_openai.py         # Azure OpenAI (fallback)
│   ├── bedrock.py              # AWS Bedrock (fallback)
│   └── vertex.py               # Google Vertex (fallback)
│
├── api/                        # REST API (FastAPI)
│   ├── routes.py               # Rotas principais
│   ├── auth.py                 # Endpoints de autenticação
│   ├── v1/                     # API versionada v1
│   │   ├── stories.py          # CRUD Stories
│   │   ├── projects.py         # CRUD Projects
│   │   └── oauth.py            # OAuth endpoints
│   ├── session_routes.py       # Gestão de sessões (#409)
│   ├── csrf_routes.py          # CSRF protection (#411)
│   ├── brute_force_routes.py   # Proteção força bruta (#402)
│   ├── cors_routes.py          # CORS por tenant (#399)
│   └── rate_limit_routes.py    # Rate limiting (#393)
│
├── audit/                      # Auditoria SOC2/GDPR
│   ├── models.py               # Modelo AuditLog
│   ├── service.py              # Serviço de auditoria
│   ├── decorators.py           # @audit_log decorator
│   └── routes.py               # Endpoints de audit
│
├── auth/                       # Autenticação & Autorização
│   ├── unified_auth.py         # Sistema unificado
│   ├── rbac.py                 # Role-Based Access Control
│   ├── abac.py                 # Attribute-Based Access Control
│   ├── personas.py             # Perfis de usuário (9 personas)
│   ├── oauth2.py               # OAuth2 + JWT
│   ├── sso.py                  # Single Sign-On
│   ├── mfa.py                  # Multi-Factor Authentication (#340)
│   ├── password_policy.py      # Políticas de senha (#342)
│   ├── token_blacklist.py      # Revogação de tokens (#358)
│   ├── session_manager.py      # Gestão de sessões (#409)
│   └── api_keys/               # API Key Management (#341)
│       ├── models.py
│       ├── service.py
│       └── routes.py
│
├── billing/                    # Gestão de Planos
│   ├── models.py               # Planos e limites
│   ├── service.py              # Lógica de billing
│   └── middleware.py           # Verificação de limites
│
├── cache/                      # Camada de Cache
│   ├── redis_cache.py          # Redis implementation
│   └── memory_cache.py         # In-memory fallback
│
├── cloud/                      # Multi-Cloud
│   ├── aws/                    # Amazon Web Services
│   │   ├── ec2.py              # EC2 instances
│   │   ├── lambda_deploy.py    # Lambda functions
│   │   ├── s3.py               # S3 storage
│   │   └── rds.py              # RDS databases
│   ├── azure/                  # Microsoft Azure
│   │   ├── functions.py        # Azure Functions
│   │   ├── storage.py          # Blob Storage
│   │   └── database.py         # Azure Database
│   ├── gcp/                    # Google Cloud Platform
│   └── terraform/              # Infrastructure as Code
│       └── generator.py        # Gerador de Terraform
│
├── config/                     # Configurações
│   ├── __init__.py             # Constantes globais
│   ├── settings.py             # Settings por ambiente
│   └── environments/           # Dev, Staging, Production
│
├── core/                       # Lógica Central
│   ├── autonomous_loop.py      # Loop Generate→Lint→Test→Fix
│   ├── job_queue.py            # Redis job queue
│   ├── project_manager.py      # Gestão de projetos
│   ├── story_generator.py      # Geração de stories
│   ├── app_generator.py        # Gerador de apps testáveis
│   ├── sandbox_executor.py     # Execução isolada (#381)
│   ├── tenant_isolation.py     # Isolamento multi-tenant
│   ├── analytics_service.py    # Análise e BI
│   ├── okr_manager.py          # Gestão de OKRs
│   └── ab_test_manager.py      # A/B Testing
│
├── dashboard/                  # Interfaces Web
│   ├── app_v6_agile.py         # Dashboard Agile v6.5 (principal)
│   ├── app_v5_kanban.py        # Kanban simples
│   ├── app.py                  # Dashboard Workers
│   ├── executive_dashboard.py  # Dashboard Executivo
│   ├── admin_portal.py         # Portal Admin
│   ├── tenant_admin_portal.py  # Admin por Tenant (#288)
│   ├── platform_portal.py      # Super Admin (#287)
│   ├── planning_poker.py       # Planning Poker (#244)
│   ├── sprint_retrospective.py # Retrospectivas (#240)
│   ├── dark_mode.py            # Dark Mode (#217)
│   ├── skeleton_loaders.py     # Skeleton Loaders (#218)
│   ├── accessibility.py        # Acessibilidade (#270)
│   └── static/                 # CSS, JS, Assets
│
├── database/                   # Persistência
│   ├── connection.py           # SQLAlchemy + Redis
│   ├── models.py               # 50+ modelos
│   ├── repositories.py         # Data Access Layer
│   ├── tenant_models.py        # Modelos multi-tenant
│   └── migrations/             # Migrações de schema
│
├── integrations/               # Integrações Externas
│   ├── base.py                 # IntegrationBase + OAuthTokenManager
│   ├── config.py               # Configurações globais
│   ├── gateway.py              # API Gateway
│   ├── jira/                   # Atlassian Jira (#310, #311)
│   ├── azure_devops/           # Azure DevOps (#312, #313)
│   ├── salesforce/             # Salesforce CRM
│   ├── sap_s4/                 # SAP S/4HANA (#19)
│   ├── sap_ecc/                # SAP ECC (#315)
│   ├── sap_cpi/                # SAP CPI
│   ├── teams/                  # Microsoft Teams (#22)
│   ├── email/                  # Email SMTP/Graph (#23)
│   ├── sharepoint/             # SharePoint (#298)
│   ├── calendar/               # Google/Outlook Calendar (#264)
│   ├── github/                 # GitHub
│   ├── gitlab/                 # GitLab
│   ├── deploy/                 # Deploy Manager (#332)
│   ├── monitoring/             # Health Check (#333)
│   └── webhooks/               # Webhook Handlers (#303)
│
├── middleware/                 # HTTP Middleware
│   ├── tenant_middleware.py    # Injeção de tenant
│   ├── auth_middleware.py      # Validação JWT
│   ├── rate_limit_middleware.py # Rate limiting
│   └── security_headers.py     # Security headers (#396)
│
├── notifications/              # Notificações
│   ├── notification_service.py # Serviço central
│   └── channels/               # Canais
│       ├── email_channel.py    # Email
│       ├── slack_channel.py    # Slack
│       └── teams_channel.py    # Microsoft Teams
│
├── observability/              # Monitoramento
│   ├── sentry.py               # Error tracking
│   ├── tracing.py              # Distributed tracing
│   └── metrics.py              # Métricas Prometheus
│
├── security/                   # Segurança
│   ├── encryption.py           # Encryption at Rest (#344)
│   ├── ip_policy.py            # IP Whitelisting (#343)
│   ├── csrf.py                 # CSRF Protection (#411)
│   ├── brute_force.py          # Brute Force Protection (#402)
│   ├── cors_config.py          # CORS por Tenant (#399)
│   ├── rate_limiter.py         # Rate Limiting (#393)
│   ├── security_headers.py     # Headers Seguros (#396)
│   └── event_notifications.py  # Alertas de Segurança (#359)
│
├── websocket/                  # Real-time
│   ├── connection_manager.py   # Gestão de conexões
│   └── routes.py               # WebSocket endpoints
│
└── white_label/                # White Label
    ├── branding.py             # Customização visual
    └── tenant_config.py        # Config por tenant
```

---

## 🎨 Padrões de Design

### 1. Repository Pattern

**O que é:** Abstração da camada de dados que esconde detalhes de persistência.

**Por que usamos:**
- Isolamento do banco de dados
- Facilita testes com mocks
- Centraliza queries complexas
- Suporta multi-tenancy transparente

**Implementação:**

```python
# factory/database/repositories.py

class BaseRepository:
    """Repositório base com operações CRUD + tenant isolation"""

    def __init__(self, model_class: Type[Base], db: Session):
        self.model = model_class
        self.db = db

    def _apply_tenant_filter(self, query):
        """Aplica filtro de tenant automaticamente"""
        tenant_id = get_current_tenant()
        if tenant_id and hasattr(self.model, 'tenant_id'):
            return query.filter(self.model.tenant_id == tenant_id)
        return query

    def get_by_id(self, id: str) -> Optional[Model]:
        query = self.db.query(self.model).filter(self.model.id == id)
        query = self._apply_tenant_filter(query)
        return query.first()

    def get_all(self, **filters) -> List[Model]:
        query = self.db.query(self.model)
        query = self._apply_tenant_filter(query)
        for key, value in filters.items():
            query = query.filter(getattr(self.model, key) == value)
        return query.all()


class StoryRepository(BaseRepository):
    """Repositório especializado para Stories"""

    def get_by_status(self, status: str) -> List[Story]:
        return self.get_all(status=status)

    def get_by_sprint(self, sprint_id: str) -> List[Story]:
        return self.get_all(sprint_id=sprint_id)

    def move_to_column(self, story_id: str, new_status: str):
        story = self.get_by_id(story_id)
        story.status = new_status
        story.updated_at = datetime.utcnow()
        self.db.commit()
```

**Benefícios:**
- `_apply_tenant_filter()` garante isolamento automático
- Queries complexas ficam no repositório, não espalhadas
- Fácil adicionar cache ou auditoria centralmente

### 2. Factory Pattern

**O que é:** Criação de objetos complexos através de uma interface unificada.

**Por que usamos:**
- Múltiplos tipos de agentes/workers
- Seleção dinâmica de modelos LLM
- Configuração por tenant

**Implementação:**

```python
# factory/agents/agent_factory.py

class AgentFactory:
    """Factory para criação de agentes especializados"""

    AGENT_TYPES = {
        "code_generator": CodeGeneratorAgent,
        "test_writer": TestWriterAgent,
        "doc_writer": DocumentationAgent,
        "code_reviewer": CodeReviewerAgent,
        "security_scanner": SecurityScannerAgent,
    }

    @classmethod
    def create(cls, agent_type: str, config: dict = None) -> Agent:
        """Cria agente do tipo especificado"""
        if agent_type not in cls.AGENT_TYPES:
            raise ValueError(f"Unknown agent type: {agent_type}")

        agent_class = cls.AGENT_TYPES[agent_type]
        return agent_class(config or {})

    @classmethod
    def create_for_task(cls, task: StoryTask) -> Agent:
        """Seleciona agente apropriado baseado no tipo de task"""
        mapping = {
            "development": "code_generator",
            "test": "test_writer",
            "documentation": "doc_writer",
            "review": "code_reviewer",
        }
        agent_type = mapping.get(task.task_type, "code_generator")
        return cls.create(agent_type)
```

**Benefícios:**
- Criação centralizada e consistente
- Fácil adicionar novos tipos de agentes
- Seleção automática baseada no contexto

### 3. Strategy Pattern

**O que é:** Família de algoritmos intercambiáveis em runtime.

**Por que usamos:**
- Múltiplos provedores de LLM
- Diferentes estratégias de cache
- Seleção por complexidade/custo

**Implementação:**

```python
# factory/ai/llm_provider.py

class LLMProvider(ABC):
    """Interface abstrata para provedores de LLM"""

    @abstractmethod
    async def generate(self, prompt: str, max_tokens: int) -> str:
        pass

    @abstractmethod
    def get_cost_per_token(self) -> float:
        pass


class ClaudeProvider(LLMProvider):
    """Claude (Anthropic) - Principal"""

    async def generate(self, prompt: str, max_tokens: int = 4096) -> str:
        response = await self.client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=max_tokens,
            messages=[{"role": "user", "content": prompt}]
        )
        return response.content[0].text

    def get_cost_per_token(self) -> float:
        return 0.003  # $3 per 1M tokens


class AzureOpenAIProvider(LLMProvider):
    """Azure OpenAI - Fallback"""
    ...


class LLMSelector:
    """Seleciona provedor baseado em critérios"""

    def __init__(self):
        self.providers = {
            "claude": ClaudeProvider(),
            "azure": AzureOpenAIProvider(),
            "bedrock": BedrockProvider(),
        }

    def get_provider(self, complexity: str = "medium") -> LLMProvider:
        """Seleciona provedor por complexidade"""
        if complexity == "high":
            return self.providers["claude"]  # Melhor reasoning
        elif complexity == "low":
            return self.providers["azure"]   # Mais barato
        return self.providers["claude"]      # Default
```

**Benefícios:**
- Fallback automático se um provedor falhar
- Otimização de custo por complexidade
- Adicionar novos provedores sem alterar código existente

### 4. Observer Pattern

**O que é:** Objetos notificados automaticamente sobre mudanças de estado.

**Por que usamos:**
- Notificações em tempo real (WebSocket)
- Auditoria automática de eventos
- Integração com sistemas externos

**Implementação:**

```python
# factory/websocket/connection_manager.py

class ConnectionManager:
    """Gerencia conexões WebSocket para notificações real-time"""

    def __init__(self):
        self.active_connections: Dict[str, List[WebSocket]] = {}

    async def connect(self, websocket: WebSocket, tenant_id: str):
        await websocket.accept()
        if tenant_id not in self.active_connections:
            self.active_connections[tenant_id] = []
        self.active_connections[tenant_id].append(websocket)

    async def broadcast(self, tenant_id: str, message: dict):
        """Notifica todos os clientes de um tenant"""
        connections = self.active_connections.get(tenant_id, [])
        for connection in connections:
            await connection.send_json(message)


# Uso: quando story muda de status
async def on_story_status_change(story: Story, old_status: str, new_status: str):
    await connection_manager.broadcast(
        tenant_id=story.tenant_id,
        message={
            "type": "story_update",
            "story_id": story.story_id,
            "old_status": old_status,
            "new_status": new_status,
            "timestamp": datetime.utcnow().isoformat()
        }
    )
```

**Benefícios:**
- Dashboard atualiza em tempo real sem polling
- Múltiplos observadores (WebSocket, Audit, Integrations)
- Desacoplamento entre produtor e consumidor de eventos

### 5. Decorator Pattern

**O que é:** Adiciona comportamento a objetos dinamicamente.

**Por que usamos:**
- Auditoria automática em endpoints
- Rate limiting transparente
- Validação de permissões

**Implementação:**

```python
# factory/audit/decorators.py

def audit_log(action: str, resource: str):
    """Decorator que registra ação no audit log"""

    def decorator(func):
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            # Captura estado anterior
            old_value = await get_current_state(resource, kwargs)

            # Executa função
            result = await func(*args, **kwargs)

            # Registra no audit log
            await AuditService.log(
                action=action,
                resource_type=resource,
                resource_id=kwargs.get('id'),
                old_value=old_value,
                new_value=result,
                user_id=get_current_user().id,
                tenant_id=get_current_tenant()
            )

            return result
        return wrapper
    return decorator


# Uso
@audit_log(action="UPDATE", resource="story")
async def update_story(id: str, data: StoryUpdate):
    story = repository.get_by_id(id)
    story.update(data)
    return story
```

**Benefícios:**
- Auditoria sem poluir código de negócio
- Consistência em todos os endpoints
- Fácil adicionar/remover comportamentos

---

## 🔧 Decisões Técnicas

### Backend: FastAPI

| Critério | FastAPI | Flask | Django |
|----------|---------|-------|--------|
| **Performance** | ⭐⭐⭐ Async nativo | ⭐⭐ WSGI | ⭐⭐ WSGI |
| **Documentação** | ⭐⭐⭐ Auto (OpenAPI) | ⭐ Manual | ⭐⭐ Admin |
| **Type Safety** | ⭐⭐⭐ Pydantic | ⭐ Opcional | ⭐⭐ Forms |
| **Curva Aprendizado** | ⭐⭐⭐ Simples | ⭐⭐⭐ Simples | ⭐ Complexo |

**Por que FastAPI:**
- Async nativo permite centenas de conexões simultâneas
- Validação automática com Pydantic
- Documentação Swagger/OpenAPI gerada automaticamente
- Dependency Injection nativo

**Benefícios obtidos:**
- API documentada automaticamente em `/docs`
- Validação de entrada sem código boilerplate
- Performance 3x maior que Flask em benchmarks

### Banco de Dados: PostgreSQL + SQLite

| Ambiente | Banco | Motivo |
|----------|-------|--------|
| Desenvolvimento | SQLite | Zero setup, arquivo único, portátil |
| Produção | PostgreSQL | ACID, performance, escalável, JSON nativo |

**Por que essa combinação:**
- SQLite permite iniciar sem Docker/instalações
- PostgreSQL escala para milhões de registros
- SQLAlchemy abstrai diferenças entre ambos

**Benefícios:**
- `python factory/dashboard/app_v6_agile.py` funciona imediatamente
- Mesma codebase, diferente banco via `DATABASE_URL`
- Migração transparente quando escalar

### Cache: Redis

**Por que Redis:**
- Performance (operações em <1ms)
- Estruturas de dados ricas (strings, hashes, lists, sets)
- Pub/Sub para notificações
- Job Queue nativo

**Uso no projeto:**
- Cache de queries frequentes
- Session storage
- Job queue para workers
- Rate limiting counters
- WebSocket pub/sub

### LLM: Claude (Anthropic)

| Critério | Claude | GPT-4 | Gemini |
|----------|--------|-------|--------|
| **Reasoning** | ⭐⭐⭐ Excelente | ⭐⭐⭐ Excelente | ⭐⭐ Bom |
| **Context Window** | 200K tokens | 128K tokens | 1M tokens |
| **Code Quality** | ⭐⭐⭐ Alto | ⭐⭐⭐ Alto | ⭐⭐ Médio |
| **Custo** | $3/1M tokens | $10/1M tokens | $7/1M tokens |
| **Latência** | ⭐⭐⭐ Baixa | ⭐⭐ Média | ⭐⭐ Média |

**Por que Claude:**
- Melhor reasoning para código complexo
- Janela de contexto grande (200K) para projetos inteiros
- Custo-benefício superior
- Suporte a tools/function calling

**Benefícios obtidos:**
- Código gerado com menos erros
- Capacidade de "ver" projeto inteiro no contexto
- Auto-fix mais eficiente

### Frontend: Vue.js 3

**Por que Vue.js:**
- Composition API moderna
- Reatividade simples e intuitiva
- Ecossistema maduro
- Curva de aprendizado suave

**Integração com FastAPI:**
- Templates Jinja2 com Vue components inline
- APIs REST consumidas via fetch/axios
- WebSocket para real-time updates

---

## 🔄 Fluxos de Dados

### Fluxo Principal: Desenvolvimento Autônomo

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        FLUXO DE DESENVOLVIMENTO AUTÔNOMO                     │
└─────────────────────────────────────────────────────────────────────────────┘

   USUÁRIO                    SISTEMA                         CLAUDE AI
      │                          │                                │
      │  1. Cria Story           │                                │
      │ ─────────────────────►   │                                │
      │  "Login com email/senha" │                                │
      │                          │                                │
      │                          │  2. Gera Tasks automaticamente │
      │                          │ ◄──────────────────────────────│
      │                          │  - Task: Implementation        │
      │                          │  - Task: Tests                 │
      │                          │  - Task: Documentation         │
      │                          │                                │
      │  3. Move para READY      │                                │
      │ ─────────────────────►   │                                │
      │                          │                                │
      │                          │  4. Kanban Watcher detecta     │
      │                          │ ─────────────────────────────► │
      │                          │                                │
      │                          │  5. GENERATE: Cria código      │
      │                          │ ◄───────────────────────────── │
      │                          │                                │
      │                          │  6. LINT: Valida sintaxe       │
      │                          │ ─────────────────────────────► │
      │                          │     └─► Erro? FIX automático   │
      │                          │ ◄───────────────────────────── │
      │                          │                                │
      │                          │  7. TEST: Executa testes       │
      │                          │ ─────────────────────────────► │
      │                          │     └─► Falha? FIX automático  │
      │                          │ ◄───────────────────────────── │
      │                          │                                │
      │                          │  8. COMMIT: Git commit         │
      │                          │                                │
      │  9. WebSocket notifica   │                                │
      │ ◄─────────────────────   │                                │
      │  "Story DONE"            │                                │
      │                          │                                │
      │  10. Testa no Swagger    │                                │
      │ ─────────────────────►   │                                │
      │  (App Generator)         │                                │
      │                          │                                │
```

### Fluxo de Autenticação JWT

```
┌──────────────────────────────────────────────────────────────────┐
│                    FLUXO DE AUTENTICAÇÃO JWT                      │
└──────────────────────────────────────────────────────────────────┘

  CLIENTE                     API                        DATABASE
     │                         │                            │
     │  POST /api/auth/login   │                            │
     │  {email, password}      │                            │
     │ ────────────────────►   │                            │
     │                         │  Busca usuário             │
     │                         │ ─────────────────────────► │
     │                         │                            │
     │                         │  user + password_hash      │
     │                         │ ◄───────────────────────── │
     │                         │                            │
     │                         │  Verifica MFA?             │
     │                         │  (se habilitado)           │
     │                         │                            │
     │  {access_token,         │                            │
     │   refresh_token,        │                            │
     │   expires_in}           │                            │
     │ ◄────────────────────   │                            │
     │                         │                            │
     │  GET /api/stories       │                            │
     │  Authorization: Bearer  │                            │
     │ ────────────────────►   │                            │
     │                         │                            │
     │                         │  Middleware:               │
     │                         │  1. Valida JWT             │
     │                         │  2. Extrai tenant_id       │
     │                         │  3. Injeta contexto        │
     │                         │                            │
     │                         │  SELECT * FROM stories     │
     │                         │  WHERE tenant_id = ?       │
     │                         │ ─────────────────────────► │
     │                         │                            │
     │  [stories]              │  stories                   │
     │ ◄────────────────────   │ ◄───────────────────────── │
     │                         │                            │
```

### Fluxo Multi-Tenant

```
┌──────────────────────────────────────────────────────────────────┐
│                    FLUXO MULTI-TENANT                             │
└──────────────────────────────────────────────────────────────────┘

                           REQUEST
                              │
                              ▼
              ┌───────────────────────────────┐
              │   Tenant Middleware           │
              │                               │
              │  1. Extrai tenant_id de:      │
              │     - X-Tenant-ID header      │
              │     - JWT token claims        │
              │     - Subdomínio URL          │
              │                               │
              │  2. Valida tenant existe      │
              │                               │
              │  3. Injeta no ContextVar      │
              │     (thread-safe)             │
              └───────────────────────────────┘
                              │
                              ▼
              ┌───────────────────────────────┐
              │   Repository Layer            │
              │                               │
              │  def _apply_tenant_filter():  │
              │      tenant = get_context()   │
              │      return query.filter(     │
              │          tenant_id == tenant  │
              │      )                        │
              └───────────────────────────────┘
                              │
                              ▼
              ┌───────────────────────────────┐
              │   Database Query              │
              │                               │
              │  SELECT * FROM stories        │
              │  WHERE tenant_id = 'tenant_a' │
              │  -- NUNCA vê dados de outros  │
              └───────────────────────────────┘
                              │
                              ▼
              ┌───────────────────────────────┐
              │   Encryption Layer            │
              │                               │
              │  Dados sensíveis encriptados  │
              │  com chave do tenant          │
              │  (AES-256-GCM)               │
              └───────────────────────────────┘
```

---

## 🔐 Segurança

### Camadas de Proteção

```
┌─────────────────────────────────────────────────────────────────┐
│                     CAMADAS DE SEGURANÇA                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  CAMADA 1: REDE                                                  │
│  ├── Rate Limiting (100 req/min por IP)                         │
│  ├── IP Whitelisting por tenant                                 │
│  ├── Geo-blocking (países permitidos)                           │
│  └── WAF (Web Application Firewall)                             │
│                                                                  │
│  CAMADA 2: TRANSPORTE                                            │
│  ├── HTTPS obrigatório (TLS 1.3)                                │
│  ├── HSTS (HTTP Strict Transport Security)                      │
│  └── Certificate pinning (mobile)                               │
│                                                                  │
│  CAMADA 3: APLICAÇÃO                                             │
│  ├── JWT com refresh tokens                                     │
│  ├── MFA/2FA (TOTP)                                             │
│  ├── CSRF protection                                            │
│  ├── Brute force protection (5 tentativas)                      │
│  ├── Session management (timeout, concurrent limit)             │
│  └── Security headers (CSP, X-Frame-Options, etc)               │
│                                                                  │
│  CAMADA 4: AUTORIZAÇÃO                                           │
│  ├── RBAC (Role-Based Access Control)                           │
│  ├── ABAC (Attribute-Based Access Control)                      │
│  ├── 9 personas com permissões distintas                        │
│  └── Resource-level permissions                                 │
│                                                                  │
│  CAMADA 5: DADOS                                                 │
│  ├── Encryption at Rest (AES-256-GCM)                           │
│  ├── Per-tenant encryption keys                                 │
│  ├── Key rotation (90 dias)                                     │
│  └── Soft delete com audit trail                                │
│                                                                  │
│  CAMADA 6: AUDITORIA                                             │
│  ├── Audit log imutável (HMAC chain)                            │
│  ├── SOC2/GDPR compliance                                       │
│  ├── Security event notifications                               │
│  └── SIEM export (Elasticsearch, Splunk)                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Implementações de Segurança (Issues)

| Issue | Feature | Arquivo |
|-------|---------|---------|
| #340 | Two-Factor Authentication (2FA/MFA) | `factory/auth/mfa.py` |
| #341 | API Key Management | `factory/auth/api_keys/` |
| #342 | Password Policies | `factory/auth/password_policy.py` |
| #343 | IP Whitelisting e Geo-blocking | `factory/security/ip_policy.py` |
| #344 | Encryption at Rest | `factory/security/encryption.py` |
| #345 | Security Headers e CSP | `factory/security/security_headers.py` |
| #357 | Input Validation Middleware | `factory/middleware/` |
| #358 | JWT Blacklist e Token Revocation | `factory/auth/token_blacklist.py` |
| #359 | Security Event Notifications | `factory/security/event_notifications.py` |
| #393 | Rate Limiting por Tenant | `factory/security/rate_limiter.py` |
| #396 | Security Headers Middleware | `factory/middleware/security_headers.py` |
| #399 | CORS Multi-tenant | `factory/security/cors_config.py` |
| #402 | Brute Force Protection | `factory/security/brute_force.py` |
| #409 | Session Management | `factory/auth/session_manager.py` |
| #411 | CSRF Protection | `factory/security/csrf.py` |

---

## 🏢 Multi-Tenancy

### Arquitetura de Isolamento

```
┌─────────────────────────────────────────────────────────────────┐
│                  ARQUITETURA MULTI-TENANT                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  NÍVEL 1: QUERY ISOLATION                                        │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │  Middleware injeta tenant_id em TODAS as queries          │  │
│  │  Repository._apply_tenant_filter() é automático           │  │
│  │                                                            │  │
│  │  SELECT * FROM stories WHERE tenant_id = 'TENANT_A'       │  │
│  │  -- NUNCA executa query sem filtro de tenant              │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
│  NÍVEL 2: DATABASE ISOLATION                                     │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │  Índices compostos garantem performance                    │  │
│  │  Foreign keys validam tenant_id em cascata                 │  │
│  │  Row-Level Security (RLS) em PostgreSQL                    │  │
│  │                                                            │  │
│  │  CREATE INDEX idx_stories_tenant ON stories(tenant_id);   │  │
│  │  ALTER TABLE stories ENABLE ROW LEVEL SECURITY;           │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
│  NÍVEL 3: ENCRYPTION ISOLATION                                   │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │  Cada tenant tem sua própria chave de criptografia        │  │
│  │  Master key encripta tenant keys                          │  │
│  │  Rotação automática a cada 90 dias                        │  │
│  │                                                            │  │
│  │  tenant_a_key = decrypt(master_key, encrypted_tenant_key) │  │
│  │  data = decrypt(tenant_a_key, encrypted_data)             │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Modelo de Dados Multi-Tenant

```python
# factory/database/models.py

class TenantMixin:
    """Mixin que adiciona tenant_id a qualquer modelo"""

    tenant_id = Column(
        String(50),
        nullable=True,  # None para dados globais
        index=True
    )

    @declared_attr
    def __table_args__(cls):
        return (
            Index(f'idx_{cls.__tablename__}_tenant', 'tenant_id'),
        )


class Story(Base, TenantMixin, SoftDeleteMixin, AuditMixin):
    """User Story com isolamento de tenant"""

    __tablename__ = 'stories'

    story_id = Column(String(20), primary_key=True)
    title = Column(String(500), nullable=False)
    # ... outros campos

    # Índice composto para queries frequentes
    __table_args__ = (
        Index('idx_story_tenant_status', 'tenant_id', 'status'),
        Index('idx_story_tenant_project', 'tenant_id', 'project_id'),
    )
```

---

## 🔗 Integrações

### Mapa de Integrações

```
┌─────────────────────────────────────────────────────────────────┐
│                    INTEGRAÇÕES EXTERNAS                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  PROJECT MANAGEMENT                                              │
│  ├── Jira (Issues #310, #311, #335)                             │
│  │   ├── Boards, Sprints, Epics                                 │
│  │   ├── Sincronização bidirecional                             │
│  │   └── Webhooks                                                │
│  └── Azure DevOps (Issues #312, #313)                           │
│      ├── Work Items, Repos, Pipelines                           │
│      └── Pull Requests                                           │
│                                                                  │
│  ERP/CRM                                                         │
│  ├── SAP S/4HANA (#19)                                          │
│  │   ├── OData v4 API                                           │
│  │   ├── Business Graph                                         │
│  │   └── CDS, RAP, Fiori analyzers                              │
│  ├── SAP ECC (#315)                                             │
│  │   ├── RFC/BAPI                                                │
│  │   ├── OData (Gateway)                                         │
│  │   └── IDocs                                                   │
│  └── Salesforce                                                  │
│      ├── REST API                                                │
│      ├── Bulk API                                                │
│      └── Metadata API                                            │
│                                                                  │
│  COMMUNICATION                                                   │
│  ├── Microsoft Teams (#22)                                      │
│  │   ├── Bot integration                                        │
│  │   ├── Notifications                                           │
│  │   └── Adaptive Cards                                          │
│  ├── Slack (#263)                                               │
│  │   ├── Webhooks                                                │
│  │   └── Slash commands                                          │
│  └── Email (#23)                                                │
│      ├── SMTP                                                    │
│      ├── Microsoft Graph                                         │
│      └── Templates                                               │
│                                                                  │
│  MICROSOFT 365                                                   │
│  ├── SharePoint (#298)                                          │
│  │   ├── Documents                                               │
│  │   └── Lists                                                   │
│  └── Calendar (#264)                                            │
│      ├── Google Calendar                                         │
│      └── Outlook Calendar                                        │
│                                                                  │
│  SOURCE CONTROL                                                  │
│  ├── GitHub                                                      │
│  │   ├── Repositories                                            │
│  │   ├── Actions                                                 │
│  │   └── Webhooks                                                │
│  └── GitLab                                                      │
│      ├── Repositories                                            │
│      └── CI/CD Pipelines                                         │
│                                                                  │
│  BI/ANALYTICS                                                    │
│  ├── Power BI                                                    │
│  └── Tableau                                                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Padrão Base para Integrações

```python
# factory/integrations/base.py

class IntegrationBase(ABC):
    """Classe base abstrata para todas as integrações"""

    def __init__(self, config: IntegrationConfig):
        self.config = config
        self.token_manager = OAuthTokenManager(config)

    @abstractmethod
    async def connect(self) -> bool:
        """Estabelece conexão com o sistema externo"""
        pass

    @abstractmethod
    async def disconnect(self) -> bool:
        """Encerra conexão"""
        pass

    @abstractmethod
    async def test_connection(self) -> HealthCheckResult:
        """Testa se a conexão está funcionando"""
        pass

    @abstractmethod
    async def sync(self, direction: SyncDirection) -> SyncResult:
        """Sincroniza dados"""
        pass

    async def with_retry(self, func, max_retries: int = 3):
        """Executa função com retry e backoff exponencial"""
        for attempt in range(max_retries):
            try:
                return await func()
            except RateLimitError:
                wait_time = 2 ** attempt
                await asyncio.sleep(wait_time)
        raise MaxRetriesExceeded()


class OAuthTokenManager:
    """Gerencia tokens OAuth com refresh automático"""

    async def get_valid_token(self) -> str:
        """Retorna token válido, refresh se necessário"""
        if self._is_token_expired():
            await self._refresh_token()
        return self.access_token

    async def _refresh_token(self):
        """Atualiza access token usando refresh token"""
        response = await self.client.post(
            self.token_url,
            data={
                "grant_type": "refresh_token",
                "refresh_token": self.refresh_token,
                "client_id": self.client_id,
                "client_secret": self.client_secret,
            }
        )
        self.access_token = response["access_token"]
        self.expires_at = datetime.utcnow() + timedelta(seconds=response["expires_in"])
```

---

## 📊 Observabilidade

### Stack de Monitoramento

```
┌─────────────────────────────────────────────────────────────────┐
│                    OBSERVABILIDADE                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  LOGS                                                            │
│  ├── Formato: JSON estruturado                                  │
│  ├── Níveis: DEBUG, INFO, WARNING, ERROR, CRITICAL              │
│  ├── Contexto: request_id, tenant_id, user_id                   │
│  └── Destino: Console + File + Elasticsearch                    │
│                                                                  │
│  MÉTRICAS                                                        │
│  ├── Prometheus metrics endpoint (/metrics)                     │
│  ├── Custom metrics:                                             │
│  │   ├── request_duration_seconds                               │
│  │   ├── active_jobs_total                                      │
│  │   ├── stories_by_status                                      │
│  │   └── llm_tokens_used_total                                  │
│  └── Grafana dashboards pré-configurados                        │
│                                                                  │
│  TRACING                                                         │
│  ├── OpenTelemetry integration                                  │
│  ├── Distributed tracing (Jaeger)                               │
│  ├── Span context propagation                                   │
│  └── Performance profiling                                       │
│                                                                  │
│  ERROR TRACKING                                                  │
│  ├── Sentry integration                                         │
│  ├── Error grouping e deduplication                             │
│  ├── Release tracking                                            │
│  └── Performance monitoring                                      │
│                                                                  │
│  HEALTH CHECKS                                                   │
│  ├── GET /health - Status geral                                 │
│  ├── GET /health/db - Database                                  │
│  ├── GET /health/redis - Cache                                  │
│  └── GET /api/v1/integrations/health - Integrações              │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Audit Logging (SOC2/GDPR)

```python
# factory/audit/service.py

class AuditService:
    """Serviço de auditoria compliant com SOC2/GDPR"""

    AUDITABLE_ACTIONS = [
        # Authentication
        "LOGIN", "LOGOUT", "LOGIN_FAILED", "MFA_ENABLED", "MFA_DISABLED",
        "PASSWORD_CHANGED", "TOKEN_REFRESH", "TOKEN_REVOKED",

        # Authorization
        "ROLE_ASSIGNED", "ROLE_REVOKED", "PERMISSION_DENIED",

        # Data Operations
        "CREATE", "READ", "UPDATE", "DELETE", "EXPORT", "IMPORT",

        # Configuration
        "SETTINGS_CHANGED", "INTEGRATION_CONFIGURED", "API_KEY_CREATED",
    ]

    @classmethod
    async def log(
        cls,
        action: str,
        resource_type: str,
        resource_id: str = None,
        old_value: dict = None,
        new_value: dict = None,
        user_id: str = None,
        tenant_id: str = None,
        ip_address: str = None,
        user_agent: str = None,
    ):
        """Registra evento de auditoria"""

        # Calcula hash para integridade (chain)
        previous_log = await cls._get_last_log(tenant_id)
        hash_input = f"{previous_log.hash if previous_log else 'genesis'}{action}{resource_type}{datetime.utcnow().isoformat()}"
        integrity_hash = hmac.new(
            AUDIT_SECRET.encode(),
            hash_input.encode(),
            hashlib.sha256
        ).hexdigest()

        log_entry = AuditLog(
            tenant_id=tenant_id,
            user_id=user_id,
            action=action,
            resource_type=resource_type,
            resource_id=resource_id,
            old_value=old_value,
            new_value=new_value,
            ip_address=ip_address,
            user_agent=user_agent,
            integrity_hash=integrity_hash,
            created_at=datetime.utcnow(),
        )

        await cls.repository.create(log_entry)

        # Notifica se evento crítico
        if action in cls.CRITICAL_ACTIONS:
            await SecurityEventNotifier.notify(log_entry)
```

---

## 🚀 Deployment

### Ambientes

| Ambiente | Database | Cache | LLM | Porta |
|----------|----------|-------|-----|-------|
| **Development** | SQLite | Memory | Claude Haiku | 9001 |
| **Staging** | PostgreSQL | Redis | Claude Sonnet | 9001 |
| **Production** | PostgreSQL (HA) | Redis Cluster | Claude Opus | 9001 |

### Docker Compose

```yaml
# docker-compose.yml

version: '3.9'

services:
  # Banco de dados principal
  postgres:
    image: postgres:16-alpine
    environment:
      POSTGRES_DB: factory
      POSTGRES_USER: factory
      POSTGRES_PASSWORD: ${DB_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U factory"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Cache e Queue
  redis:
    image: redis:7-alpine
    command: redis-server --appendonly yes
    volumes:
      - redis_data:/data
    ports:
      - "6379:6379"
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Aplicação principal
  app:
    build:
      context: .
      dockerfile: Dockerfile
    environment:
      DATABASE_URL: postgresql://factory:${DB_PASSWORD}@postgres:5432/factory
      REDIS_URL: redis://redis:6379
      ANTHROPIC_API_KEY: ${ANTHROPIC_API_KEY}
      JWT_SECRET_KEY: ${JWT_SECRET_KEY}
      ENCRYPTION_MASTER_KEY: ${ENCRYPTION_MASTER_KEY}
    ports:
      - "9001:9001"
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:9001/health"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  postgres_data:
  redis_data:
```

### Kubernetes (Helm)

```yaml
# k8s/values.yaml

replicaCount: 3

image:
  repository: fabricadeagentes/app
  tag: "7.0"
  pullPolicy: IfNotPresent

service:
  type: ClusterIP
  port: 9001

ingress:
  enabled: true
  annotations:
    kubernetes.io/ingress.class: nginx
    cert-manager.io/cluster-issuer: letsencrypt-prod
  hosts:
    - host: app.fabricadeagentes.com
      paths:
        - path: /
          pathType: Prefix

resources:
  limits:
    cpu: 2000m
    memory: 2Gi
  requests:
    cpu: 500m
    memory: 512Mi

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 70

postgresql:
  enabled: true
  auth:
    database: factory
    existingSecret: factory-db-secret

redis:
  enabled: true
  architecture: standalone
```

---

## 📚 Referências

### Issues Principais por Categoria

**Segurança:**
- #340-#345, #357-#359, #393, #396, #399, #402, #409, #411

**Integrações:**
- #310-#315, #326, #332-#335, #360-#366

**UI/UX:**
- #217-#219, #232-#236, #262, #421-#429

**Infraestrutura:**
- #375-#382, #389, #419-#420

**Features Agile:**
- #240, #244, #279-#281

### Arquivos de Configuração

| Arquivo | Propósito |
|---------|-----------|
| `factory/config/__init__.py` | Constantes globais |
| `factory/config/settings.py` | Settings por ambiente |
| `docker-compose.yml` | Infraestrutura local |
| `k8s/` | Manifestos Kubernetes |
| `.env.example` | Template de variáveis |

---

*Documentação gerada em Janeiro 2026 - Plataforma E v7.0*

# Arquitetura Tecnica - Plataforma E

## Visao Geral

A Plataforma E e construida sobre uma arquitetura modular e extensivel que permite o desenvolvimento autonomo de software atraves de agentes inteligentes colaborativos.

---

## Stack Tecnologico

### Backend
| Tecnologia | Versao | Uso |
|------------|--------|-----|
| Python | 3.10+ | Linguagem principal |
| FastAPI | 0.100+ | Framework web/API |
| SQLAlchemy | 2.0+ | ORM |
| Pydantic | 2.0+ | Validacao de dados |
| SQLite | 3.x | Banco de dados (dev) |
| PostgreSQL | 14+ | Banco de dados (prod) |

### Frontend
| Tecnologia | Versao | Uso |
|------------|--------|-----|
| Vue.js | 3.x | Framework frontend |
| Composition API | - | Padrao de componentes |
| TailwindCSS | 3.x | Estilizacao |

### Inteligencia Artificial
| Tecnologia | Uso |
|------------|-----|
| Claude (Anthropic) | Geracao de codigo inteligente |
| Templates | Fallback quando sem API |

---

## Arquitetura de Camadas

```
+------------------------------------------------------------------+
|                        PRESENTATION LAYER                         |
|  +--------------------------------------------------------------+ |
|  |                    Dashboard (Vue.js)                         | |
|  |  - Monitoramento em tempo real                               | |
|  |  - Gestao de projetos e stories                              | |
|  |  - Visualizacao de agentes                                   | |
|  +--------------------------------------------------------------+ |
+------------------------------------------------------------------+
                                |
                                v
+------------------------------------------------------------------+
|                          API LAYER                                |
|  +--------------------------------------------------------------+ |
|  |                   FastAPI Application                         | |
|  |  - REST endpoints                                             | |
|  |  - WebSocket (tempo real)                                     | |
|  |  - Autenticacao JWT                                           | |
|  |  - Documentacao OpenAPI                                       | |
|  +--------------------------------------------------------------+ |
+------------------------------------------------------------------+
                                |
                                v
+------------------------------------------------------------------+
|                      ORCHESTRATION LAYER                          |
|  +----------------+  +----------------+  +--------------------+   |
|  |    Project     |  |     Story      |  |    Intelligent     |   |
|  |  Orchestrator  |  |    Executor    |  |     Developer      |   |
|  |                |  |                |  |                    |   |
|  | - Processa     |  | - Executa      |  | - Gerencia         |   |
|  |   inputs       |  |   stories      |  |   agentes          |   |
|  | - Gera stories |  | - Move         |  | - Distribui        |   |
|  |                |  |   pipeline     |  |   tarefas          |   |
|  +----------------+  +----------------+  +--------------------+   |
+------------------------------------------------------------------+
                                |
                                v
+------------------------------------------------------------------+
|                         AGENT LAYER                               |
|  +--------------------------------------------------------------+ |
|  |                    Agent Runtime                              | |
|  |  - Gerencia ciclo de vida dos agentes                        | |
|  |  - Distribui tarefas (PriorityQueue)                         | |
|  |  - Execucao paralela (ThreadPoolExecutor)                    | |
|  +--------------------------------------------------------------+ |
|                                |                                  |
|  +--------+ +--------+ +--------+ +--------+ +--------+          |
|  |AGT-003 | |AGT-007 | |AGT-008 | |AGT-009 | |AGT-015 |          |
|  |Product | |DB Spec | |Backend | |Frontend| |  QA    |          |
|  | Owner  | |        | |  Dev   | |  Dev   | |        |          |
|  +---+----+ +---+----+ +---+----+ +---+----+ +---+----+          |
|      |          |          |          |          |               |
|      +----------+----------+----------+----------+               |
|                            |                                     |
|  +--------------------------------------------------------------+|
|  |                 AgentBrain (Claude AI)                        ||
|  |  - Pensamento contextual                                      ||
|  |  - Tomada de decisoes                                         ||
|  |  - Aprendizado continuo                                       ||
|  +--------------------------------------------------------------+|
+------------------------------------------------------------------+
                                |
                                v
+------------------------------------------------------------------+
|                         SKILLS LAYER                              |
|  +----------------+  +----------------+  +--------------------+   |
|  |  Intelligent   |  |  Real Skills   |  |   Agent Memory     |   |
|  |    Skills      |  |  (Templates)   |  |   (Persistent)     |   |
|  |                |  |                |  |                    |   |
|  | - Usa Claude   |  | - Templates    |  | - JSON files       |   |
|  | - Contextual   |  |   pre-definidos|  | - Aprendizado      |   |
|  | - Adaptativo   |  | - Rapido       |  | - Historico        |   |
|  +----------------+  +----------------+  +--------------------+   |
+------------------------------------------------------------------+
                                |
                                v
+------------------------------------------------------------------+
|                      PERSISTENCE LAYER                            |
|  +----------------+  +----------------+  +--------------------+   |
|  |  SQLAlchemy    |  |  File System   |  |   Memory Files     |   |
|  |    (ORM)       |  |                |  |                    |   |
|  |                |  |                |  |                    |   |
|  | - Projects     |  | - Codigo       |  | - AGT-XXX          |   |
|  | - Stories      |  |   gerado       |  |   _memory.json     |   |
|  | - Agents       |  | - Templates    |  |                    |   |
|  | - Tasks        |  | - Docs         |  |                    |   |
|  +----------------+  +----------------+  +--------------------+   |
+------------------------------------------------------------------+
```

---

## Componentes Principais

### 1. Dashboard (factory/dashboard/app.py)

Aplicacao FastAPI que serve como interface principal do sistema.

**Endpoints principais:**

```
Projetos:
  GET  /api/projects           - Lista projetos
  POST /api/projects           - Cria projeto
  GET  /api/projects/{id}      - Detalhe projeto

Stories:
  GET  /api/stories            - Lista stories
  POST /api/stories            - Cria story
  PUT  /api/stories/{id}       - Atualiza story

Desenvolvimento Autonomo:
  POST /api/developer/develop/{id}              - Com templates
  POST /api/intelligent-developer/develop/{id}  - Com AI

Monitoramento:
  GET  /api/intelligent-developer/status/{id}   - Status projeto
  GET  /api/intelligent-developer/agent/{id}    - Status agente
```

### 2. Intelligent Developer (factory/orchestrator/intelligent_developer.py)

Orquestrador principal que coordena o desenvolvimento autonomo.

**Responsabilidades:**
- Inicializar cerebros dos agentes (AgentBrain)
- Agendar tarefas na PriorityQueue
- Distribuir tarefas para workers (ThreadPoolExecutor)
- Coordenar fluxo de desenvolvimento

**Configuracao de Agentes:**
```
AGT-003: Product Owner - User stories e backlog
AGT-005: Analista - Analise de requisitos
AGT-007: Especialista BD - Modelos SQLAlchemy
AGT-008: Backend Dev - Routers FastAPI
AGT-009: Frontend Dev - Componentes Vue.js
AGT-011: Revisor - Code review
AGT-013: Arquiteto - Decisoes arquiteturais
AGT-014: Tech Lead - Lideranca tecnica
AGT-015: QA - Testes automatizados
```

### 3. AgentBrain (factory/ai/claude_integration.py)

"Cerebro" inteligente de cada agente, alimentado por Claude AI.

**Capacidades:**
- `think()`: Pensar sobre uma situacao
- `decide()`: Decidir entre opcoes
- `generate_code_intelligent()`: Gerar codigo contextualizado
- `learn()`: Aprender com experiencias
- `should_ask_approval()`: Decidir se precisa aprovacao humana

### 4. Intelligent Skills (factory/skills/intelligent_skills.py)

Skills que usam LLM para geracao de codigo contextualizada.

**Skills disponiveis:**
- `generate_fastapi_router_intelligent()` - Routers com CRUD
- `generate_sqlalchemy_model_intelligent()` - Modelos ORM
- `generate_vue_component_intelligent()` - Componentes Vue
- `generate_tests_intelligent()` - Testes pytest
- `analyze_requirements_intelligent()` - Analise de requisitos
- `review_code_intelligent()` - Code review automatico

### 5. Real Skills (factory/skills/real_skills.py)

Skills baseadas em templates para geracao rapida (fallback).

### 6. Agent Memory (factory/skills/real_skills.py)

Sistema de memoria persistente dos agentes em arquivos JSON.

**Armazena:**
- `skills_executed`: Contadores de execucoes
- `knowledge`: Aprendizados textuais
- `patterns_learned`: Padroes identificados
- `files_created`: Arquivos criados
- `errors_encountered`: Erros encontrados

---

## Fluxo de Desenvolvimento

### Visao Geral

```
1. ENTRADA
   Usuario fornece requisitos:
   - Documentos (PDF, Word)
   - Texto livre
   - Audio/Video

2. PROCESSAMENTO
   Project Orchestrator:
   - Analisa documentos
   - Extrai entidades
   - Gera User Stories

3. DESENVOLVIMENTO
   Intelligent Developer:
   - Carrega projeto e stories
   - Agenda tarefas por prioridade
   - Distribui para workers paralelos

4. EXECUCAO
   Agentes executam em paralelo:
   - AGT-007: Cria modelos
   - AGT-008: Cria routers e schemas
   - AGT-009: Cria componentes Vue
   - AGT-015: Cria testes

5. SAIDA
   Projeto completo:
   - Backend (FastAPI + SQLAlchemy)
   - Frontend (Vue.js)
   - Testes (pytest)
   - Documentacao
```

### Diagrama de Sequencia Simplificado

```
Usuario       Dashboard       Developer       Agentes
   |              |              |              |
   | criar        |              |              |
   | projeto      |              |              |
   |------------->|              |              |
   |              |              |              |
   | desenvolver  |              |              |
   |------------->|------------->|              |
   |              |              |              |
   |              |              | distribuir   |
   |              |              | tarefas      |
   |              |              |------------->|
   |              |              |              |
   |              |              |    gerar     |
   |              |              |    codigo    |
   |              |              |<-------------|
   |              |              |              |
   |   arquivos   |              |              |
   |   gerados    |              |              |
   |<-------------|<-------------|              |
```

---

## Modelo de Dados

### Entidades Principais

```
projects
  - project_id (PK)
  - name
  - description
  - status
  - config (JSON)
  - created_at

stories
  - story_id (PK)
  - project_id (FK)
  - title
  - description
  - acceptance_criteria (JSON)
  - status
  - sprint
  - priority
  - assigned_agent

agents
  - agent_id (PK)
  - name
  - domain
  - description
  - status
  - capabilities (JSON)

tasks
  - task_id (PK)
  - story_id (FK)
  - agent_id (FK)
  - action
  - params (JSON)
  - status
  - result (JSON)

activity_logs
  - id (PK)
  - source
  - agent_id
  - project_id
  - event_type
  - message
  - level
  - timestamp
```

---

## Configuracao

### Variaveis de Ambiente

```
# Obrigatorias
DATABASE_URL=sqlite:///factory.db

# Opcionais
ANTHROPIC_API_KEY=sk-ant-xxxx    # Habilita modo inteligente
DASHBOARD_PORT=9000
DASHBOARD_HOST=0.0.0.0
LOG_LEVEL=INFO
MAX_WORKERS=5
MAX_PARALLEL_PROJECTS=3
```

### Modos de Operacao

1. **Modo Template** (sem ANTHROPIC_API_KEY)
   - Geracao rapida usando templates pre-definidos
   - Funciona 100% offline
   - Codigo consistente mas menos adaptavel

2. **Modo Inteligente** (com ANTHROPIC_API_KEY)
   - Geracao contextualizada usando Claude AI
   - Codigo adaptado aos requisitos especificos
   - Agentes pensam e aprendem

---

## Performance

### Metricas Tipicas

| Operacao | Tempo |
|----------|-------|
| Processar requisitos | 5-10s |
| Gerar 1 router | 2-5s |
| Projeto medio (20 stories) | 30-60s |
| Projeto grande (50 stories) | 2-3min |

### Otimizacoes

1. **Paralelismo**: ThreadPoolExecutor com 5 workers
2. **Fila de prioridade**: PriorityQueue para ordenacao
3. **Cache**: Memoria dos agentes evita reprocessamento
4. **Fallback**: Templates quando sem AI

---

## Extensibilidade

### Adicionar Novo Agente

```python
# Em intelligent_developer.py
AGENT_CONFIG["AGT-XXX"] = {
    "name": "Novo Agente",
    "role": "Descricao do papel",
    "capabilities": ["cap1", "cap2"],
    "stage": DevelopmentStage.BACKEND,
    "requires_approval_from": "AGT-013",
    "can_approve": []
}
```

### Adicionar Nova Skill

```python
# Em intelligent_skills.py
def generate_new_artifact_intelligent(
    self, agent_id, project_path, params
) -> IntelligentSkillResult:
    brain = self.get_brain(agent_id)
    # ... implementacao
    return IntelligentSkillResult(...)
```

---

## Seguranca

- **Autenticacao**: JWT tokens para API
- **Autorizacao**: Hierarquia de aprovacoes entre agentes
- **Codigo Gerado**:
  - Validacao Pydantic
  - SQLAlchemy ORM (previne SQL Injection)
  - CORS configuravel

---

<p align="center">
Documentacao tecnica da <strong>Plataforma E</strong>
</p>

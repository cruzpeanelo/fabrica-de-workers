# Arquitetura Técnica - Fábrica de Agentes

## Documentação para Equipes de TI

Este documento descreve a arquitetura completa, componentes, artefatos e fluxos técnicos da plataforma Fábrica de Agentes.

---

## Indice

1. [Visao Geral da Arquitetura](#1-visao-geral-da-arquitetura)
2. [Stack Tecnologica](#2-stack-tecnologica)
3. [Componentes do Sistema](#3-componentes-do-sistema)
4. [Modelos de Dados](#4-modelos-de-dados)
5. [APIs e Endpoints](#5-apis-e-endpoints)
6. [Integracao com Claude AI](#6-integracao-com-claude-ai)
7. [Fluxos de Processamento](#7-fluxos-de-processamento)
8. [Estrutura de Arquivos](#8-estrutura-de-arquivos)
9. [Configuracao e Deploy](#9-configuracao-e-deploy)
10. [Seguranca](#10-seguranca)
11. [Monitoramento](#11-monitoramento)
12. [Extensibilidade](#12-extensibilidade)

---

## 1. Visao Geral da Arquitetura

### Diagrama de Alto Nivel

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         FABRICA DE AGENTES v4.0                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌────────────────┐     ┌────────────────┐     ┌────────────────┐           │
│  │   NAVEGADOR    │────>│   DASHBOARD    │────>│   FASTAPI      │           │
│  │   (Usuario)    │<────│   Vue.js 3     │<────│   Backend      │           │
│  └────────────────┘     └────────────────┘     └───────┬────────┘           │
│                                                         │                    │
│                              ┌──────────────────────────┼──────────────────┐ │
│                              │                          │                  │ │
│                              v                          v                  │ │
│                    ┌─────────────────┐        ┌─────────────────┐          │ │
│                    │   CLAUDE API    │        │    SQLITE DB    │          │ │
│                    │   (Anthropic)   │        │                 │          │ │
│                    └─────────────────┘        │  ┌───────────┐  │          │ │
│                              │                │  │ Projects  │  │          │ │
│                              │                │  │ Stories   │  │          │ │
│                              v                │  │ Tasks     │  │          │ │
│                    ┌─────────────────┐        │  │ Agents    │  │          │ │
│                    │  STORY WATCHER  │        │  │ Logs      │  │          │ │
│                    │  (Autonomous)   │───────>│  └───────────┘  │          │ │
│                    └─────────────────┘        └─────────────────┘          │ │
│                              │                                              │ │
│                              v                                              │ │
│                    ┌─────────────────┐                                      │ │
│                    │   projects/     │                                      │ │
│                    │  (Codigo)       │                                      │ │
│                    └─────────────────┘                                      │ │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Arquitetura em Camadas

```
┌─────────────────────────────────────────────────────────┐
│                 CAMADA DE APRESENTACAO                   │
│  ┌─────────────────────────────────────────────────┐    │
│  │  Dashboard (Vue.js 3 + Tailwind CSS)            │    │
│  │  - Kanban Board de Stories                      │    │
│  │  - Chat Assistente IA                           │    │
│  │  - Monitoramento de Agentes                     │    │
│  └─────────────────────────────────────────────────┘    │
├─────────────────────────────────────────────────────────┤
│                   CAMADA DE API                          │
│  ┌─────────────────────────────────────────────────┐    │
│  │  FastAPI REST API                               │    │
│  │  - /api/projects, /api/stories, /api/tasks      │    │
│  │  - /api/chat/message (Claude integration)       │    │
│  │  - /api/agents, /api/logs                       │    │
│  └─────────────────────────────────────────────────┘    │
├─────────────────────────────────────────────────────────┤
│                 CAMADA DE NEGOCIOS                       │
│  ┌─────────────────────────────────────────────────┐    │
│  │  Repositories (Data Access Layer)               │    │
│  │  - StoryRepository, TaskRepository              │    │
│  │  - ProjectRepository, AgentRepository           │    │
│  │  - ChatMessageRepository, AttachmentRepository  │    │
│  └─────────────────────────────────────────────────┘    │
├─────────────────────────────────────────────────────────┤
│                   CAMADA DE DADOS                        │
│  ┌─────────────────────────────────────────────────┐    │
│  │  SQLAlchemy ORM + SQLite                        │    │
│  │  - Models: Project, Story, StoryTask, etc.      │    │
│  │  - Migrations: Alembic (opcional)               │    │
│  └─────────────────────────────────────────────────┘    │
├─────────────────────────────────────────────────────────┤
│                CAMADA DE INTEGRACAO                      │
│  ┌─────────────────────────────────────────────────┐    │
│  │  Claude AI Integration                          │    │
│  │  - ClaudeClient: API wrapper                    │    │
│  │  - AgentBrain: Decisoes inteligentes            │    │
│  │  - Story Watcher: Processamento autonomo        │    │
│  └─────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
```

---

## 2. Stack Tecnologica

### Backend

| Tecnologia | Versao | Uso |
|------------|--------|-----|
| **Python** | 3.10+ | Linguagem principal |
| **FastAPI** | 0.104+ | Framework web async |
| **SQLAlchemy** | 2.0+ | ORM para banco de dados |
| **Pydantic** | 2.0+ | Validacao de dados |
| **Uvicorn** | 0.24+ | ASGI server |
| **python-dotenv** | 1.0+ | Variaveis de ambiente |

### Frontend

| Tecnologia | Versao | Uso |
|------------|--------|-----|
| **Vue.js** | 3.x | Framework reativo (CDN) |
| **Tailwind CSS** | 3.x | Estilizacao (CDN) |
| **SortableJS** | 1.15+ | Drag-and-drop Kanban |
| **Marked.js** | - | Renderizacao Markdown |

### Banco de Dados

| Tecnologia | Uso |
|------------|-----|
| **SQLite** | Desenvolvimento/Producao leve |
| **PostgreSQL** | Producao escalavel (opcional) |

### Inteligencia Artificial

| Tecnologia | Uso |
|------------|-----|
| **Claude API** | Modelo claude-sonnet-4-20250514 |
| **Anthropic SDK** | Cliente Python oficial |

### Infraestrutura (Opcional)

| Tecnologia | Uso |
|------------|-----|
| **Docker** | Containerizacao |
| **Docker Compose** | Orquestracao local |
| **Redis** | Cache/Filas (v4.0 workers) |

---

## 3. Componentes do Sistema

### 3.1 Dashboard (`factory/dashboard/app_v6_agile.py`)

**Responsabilidade**: Interface web completa para gestao Agile

**Tecnologias**: FastAPI + Vue.js 3 (embedded)

**Funcionalidades**:
- Kanban Board de User Stories
- Painel de detalhes de Story (slide-over)
- Assistente de Chat com Claude AI
- Gerenciamento de projetos, epics, sprints
- Upload de arquivos
- Monitoramento em tempo real

**Porta**: 9001

### 3.2 Modelos de Dados (`factory/database/models.py`)

**Entidades principais**:

```python
# Projeto
class Project(Base):
    project_id: str      # PRJ-XXXX
    name: str
    description: str
    project_type: str    # web-app, api-service, data-analysis
    status: str          # PLANNING, IN_PROGRESS, COMPLETED

# User Story
class Story(Base):
    story_id: str        # STR-XXXX
    project_id: str      # FK -> Project
    title: str
    persona: str         # "Como um [usuario]"
    action: str          # "Eu quero [funcionalidade]"
    benefit: str         # "Para que [beneficio]"
    acceptance_criteria: JSON[]
    definition_of_done: JSON[]
    story_points: int    # Fibonacci: 1,2,3,5,8,13
    status: str          # backlog, ready, in_progress, review, testing, done
    complexity: str      # low, medium, high, very_high
    priority: str        # low, medium, high, urgent

# Task (Subtarefa de Story)
class StoryTask(Base):
    task_id: str         # STSK-XXXX
    story_id: str        # FK -> Story
    title: str
    task_type: str       # development, review, test, documentation
    status: str          # pending, in_progress, completed, blocked
    progress: int        # 0-100
    code_output: Text    # Codigo gerado
    files_created: JSON[]

# Documentacao
class StoryDocumentation(Base):
    doc_id: str
    story_id: str        # FK -> Story
    doc_type: str        # technical, user, test, deployment
    content: Text        # Markdown
    test_instructions: Text

# Chat
class ChatMessage(Base):
    message_id: str
    project_id: str
    story_id: str
    role: str            # user, assistant, system
    content: Text
    actions: JSON[]      # Acoes executadas

# Anexos
class Attachment(Base):
    attachment_id: str
    story_id: str
    filename: str
    file_path: str
    mime_type: str
```

### 3.3 Repositories (`factory/database/repositories.py`)

**Padrao Repository para acesso a dados**:

| Repository | Metodos Principais |
|------------|-------------------|
| `ProjectRepository` | create, get_by_id, get_all, update, delete |
| `StoryRepository` | create, get_by_id, get_by_project, move_story, update_progress |
| `StoryTaskRepository` | create, get_by_id, get_by_story, complete, update |
| `StoryDocumentationRepository` | create, get_by_story, update |
| `ChatMessageRepository` | create, get_history, clear_history |
| `AttachmentRepository` | create, get_by_story, get_by_id |

### 3.4 Claude Integration (`factory/ai/claude_integration.py`)

**ClaudeClient**: Wrapper para API Anthropic

```python
class ClaudeClient:
    def chat(message, system_prompt, context, max_tokens) -> ClaudeResponse
    def analyze_requirements(requirements) -> ClaudeResponse
    def generate_code(specification, language, framework) -> ClaudeResponse
    def review_code(code, language) -> ClaudeResponse
    def create_user_story(requirement) -> ClaudeResponse
```

**AgentBrain**: Cerebro inteligente para agentes

```python
class AgentBrain:
    def think(situation, context) -> ClaudeResponse
    def decide(options, criteria) -> ClaudeResponse
    def generate_code_intelligent(task, language) -> ClaudeResponse
    def learn(experience) -> None
```

### 3.5 Story Watcher (`run_story_watcher.py`)

**Responsabilidade**: Processamento autonomo de stories

**Fluxo**:
1. Monitora stories em status "ready" ou "in_progress"
2. Processa tasks pendentes automaticamente
3. Usa Claude AI para gerar codigo
4. Cria documentacao tecnica
5. Move story para proximo status

**Configuracao**:
- `CHECK_INTERVAL`: 30 segundos
- `PROJECT_PATH`: Diretorio de saida do codigo

---

## 4. Modelos de Dados

### Diagrama ER

```
┌─────────────┐       ┌─────────────┐       ┌─────────────┐
│   PROJECT   │       │    EPIC     │       │   SPRINT    │
├─────────────┤       ├─────────────┤       ├─────────────┤
│ project_id  │<──┐   │ epic_id     │   ┌──>│ sprint_id   │
│ name        │   │   │ project_id  │──>│   │ project_id  │
│ description │   │   │ name        │   │   │ name        │
│ project_type│   │   │ description │   │   │ start_date  │
│ status      │   │   └─────────────┘   │   │ end_date    │
└─────────────┘   │                      │   └─────────────┘
                  │                      │
                  │   ┌─────────────┐    │
                  │   │    STORY    │    │
                  │   ├─────────────┤    │
                  └───│ project_id  │    │
                      │ epic_id     │────┘
                      │ sprint_id   │─────┘
                      │ story_id    │──────────────┐
                      │ title       │              │
                      │ persona     │              │
                      │ action      │              │
                      │ benefit     │              │
                      │ story_points│              │
                      │ status      │              │
                      └─────────────┘              │
                            │                      │
           ┌────────────────┼────────────────┐    │
           │                │                │    │
           v                v                v    │
    ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
    │ STORY_TASK  │  │  STORY_DOC  │  │ ATTACHMENT  │
    ├─────────────┤  ├─────────────┤  ├─────────────┤
    │ task_id     │  │ doc_id      │  │attachment_id│
    │ story_id    │  │ story_id    │  │ story_id    │
    │ title       │  │ doc_type    │  │ filename    │
    │ task_type   │  │ content     │  │ file_path   │
    │ status      │  │ test_instr  │  │ mime_type   │
    │ progress    │  └─────────────┘  └─────────────┘
    │ code_output │
    └─────────────┘
```

### Enums e Constantes

```python
# Status de Story (Kanban)
class StoryStatus(Enum):
    BACKLOG = "backlog"
    READY = "ready"
    IN_PROGRESS = "in_progress"
    REVIEW = "review"
    TESTING = "testing"
    DONE = "done"

# Tipo de Task
class StoryTaskType(Enum):
    DEVELOPMENT = "development"
    REVIEW = "review"
    TEST = "test"
    DOCUMENTATION = "documentation"
    DESIGN = "design"

# Status de Task
class StoryTaskStatus(Enum):
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"

# Complexidade
class StoryComplexity(Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    VERY_HIGH = "very_high"

# Prioridade
class TaskPriority(Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    URGENT = "urgent"
```

---

## 5. APIs e Endpoints

### Base URL: `http://localhost:9001`

### Projetos

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/projects` | Lista todos os projetos |
| POST | `/api/projects` | Cria novo projeto |
| GET | `/api/projects/{id}` | Detalhes do projeto |
| PUT | `/api/projects/{id}` | Atualiza projeto |
| DELETE | `/api/projects/{id}` | Remove projeto |

### Stories

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/stories` | Lista stories (filtros: project_id, status) |
| POST | `/api/stories` | Cria nova story |
| GET | `/api/stories/{id}` | Detalhes da story |
| PUT | `/api/stories/{id}` | Atualiza story |
| DELETE | `/api/stories/{id}` | Remove story |
| PATCH | `/api/stories/{id}/move` | Move no Kanban |
| GET | `/api/projects/{id}/story-board` | Kanban completo |

### Tasks

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/stories/{id}/tasks` | Lista tasks da story |
| POST | `/api/stories/{id}/tasks` | Cria task |
| PUT | `/api/story-tasks/{id}` | Atualiza task |
| DELETE | `/api/story-tasks/{id}` | Remove task |
| PATCH | `/api/story-tasks/{id}/complete` | Marca como completa |

### Chat (Assistente IA)

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/chat/history` | Historico de mensagens |
| POST | `/api/chat/message` | Envia mensagem (Claude processa) |
| DELETE | `/api/chat/history` | Limpa historico |

### Arquivos

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| POST | `/api/upload` | Upload de arquivo |
| GET | `/api/files/{filename}` | Download de arquivo |

### Exemplo de Request/Response

**POST /api/chat/message**

Request:
```json
{
  "project_id": "PRJ-001",
  "content": "criar uma story para autenticacao de usuarios"
}
```

Response:
```json
{
  "user_message": {
    "message_id": "msg-001",
    "role": "user",
    "content": "criar uma story para autenticacao de usuarios"
  },
  "assistant_message": {
    "message_id": "msg-002",
    "role": "assistant",
    "content": "Vou criar a story de autenticacao...",
    "actions": [
      {
        "type": "create_story",
        "result": "Story STR-0010 criada: Autenticacao de Usuarios"
      }
    ]
  }
}
```

---

## 6. Integracao com Claude AI

### Configuracao

```python
# .env
ANTHROPIC_API_KEY=sk-ant-xxxxx

# factory/ai/claude_integration.py
DEFAULT_MODEL = "claude-sonnet-4-20250514"
MAX_TOKENS = 4096
```

### Acoes do Assistente

O Claude AI pode executar as seguintes acoes via chat:

```python
# Stories
{"action": "get_story_details", "story_id": "STR-XXXX"}
{"action": "list_stories", "project_id": "PRJ-XXXX"}
{"action": "move_story", "story_id": "STR-XXXX", "status": "ready"}
{"action": "create_story", "story_data": {...}}
{"action": "update_story", "story_id": "STR-XXXX", "updates": {...}}

# Execucao
{"action": "check_execution_status", "story_id": "STR-XXXX"}
{"action": "force_execute", "story_id": "STR-XXXX"}

# Projetos
{"action": "create_project", "project_data": {...}}
{"action": "list_projects"}
{"action": "get_project_details", "project_id": "PRJ-XXXX"}

# Arquivos
{"action": "list_attachments", "story_id": "STR-XXXX"}
{"action": "read_attachment", "attachment_id": "XXX"}
```

### System Prompt

O assistente recebe contexto completo:
- Lista de todas as stories do projeto
- Status e progresso atual
- Acoes disponiveis
- Instrucoes de formato de resposta

---

## 7. Fluxos de Processamento

### Fluxo 1: Criacao de Story via Chat

```
Usuario                    Assistente              Claude API              Database
   │                           │                       │                      │
   │  "criar story login"      │                       │                      │
   │─────────────────────────>│                       │                      │
   │                           │   [prompt + context]  │                      │
   │                           │──────────────────────>│                      │
   │                           │                       │                      │
   │                           │   [response + action] │                      │
   │                           │<──────────────────────│                      │
   │                           │                       │                      │
   │                           │   CREATE Story        │                      │
   │                           │─────────────────────────────────────────────>│
   │                           │                       │                      │
   │                           │   Story STR-0010      │                      │
   │                           │<─────────────────────────────────────────────│
   │                           │                       │                      │
   │  "Story criada: STR-0010" │                       │                      │
   │<─────────────────────────│                       │                      │
```

### Fluxo 2: Processamento Autonomo

```
Story Watcher              Database              Claude API              File System
     │                        │                      │                       │
     │  GET stories [ready]   │                      │                       │
     │───────────────────────>│                      │                       │
     │                        │                      │                       │
     │  [STR-0001, STR-0002]  │                      │                       │
     │<───────────────────────│                      │                       │
     │                        │                      │                       │
     │  Para cada story:      │                      │                       │
     │  UPDATE status=in_prog │                      │                       │
     │───────────────────────>│                      │                       │
     │                        │                      │                       │
     │  Para cada task:       │                      │                       │
     │  [generate code prompt]│                      │                       │
     │──────────────────────────────────────────────>│                       │
     │                        │                      │                       │
     │  [generated code]      │                      │                       │
     │<──────────────────────────────────────────────│                       │
     │                        │                      │                       │
     │  WRITE file            │                      │                       │
     │─────────────────────────────────────────────────────────────────────>│
     │                        │                      │                       │
     │  UPDATE task completed │                      │                       │
     │───────────────────────>│                      │                       │
     │                        │                      │                       │
     │  CREATE documentation  │                      │                       │
     │───────────────────────>│                      │                       │
```

---

## 8. Estrutura de Arquivos

```
Fabrica de Agentes/
│
├── factory/                          # Core da aplicacao
│   │
│   ├── ai/                           # Integracao com IA
│   │   └── claude_integration.py     # Cliente Claude API
│   │
│   ├── core/                         # Componentes centrais
│   │   ├── project_manager.py        # Gerenciador de projetos
│   │   ├── autonomous_loop.py        # Loop de desenvolvimento
│   │   ├── job_queue.py              # Fila de jobs (v4)
│   │   └── story_generator.py        # Gerador de stories
│   │
│   ├── database/                     # Camada de dados
│   │   ├── connection.py             # Conexao SQLAlchemy
│   │   ├── models.py                 # Modelos ORM
│   │   ├── repositories.py           # Repositorios
│   │   ├── seed.py                   # Dados iniciais
│   │   └── factory.db                # Banco SQLite
│   │
│   ├── dashboard/                    # Interface web
│   │   ├── app.py                    # Dashboard original
│   │   └── app_v6_agile.py           # Dashboard Agile (atual)
│   │
│   ├── api/                          # API REST (v4)
│   │   ├── routes.py                 # Endpoints
│   │   ├── auth.py                   # Autenticacao
│   │   └── schemas.py                # Pydantic schemas
│   │
│   ├── skills/                       # Sistema de skills
│   │   └── skill_manager.py          # Gerenciador
│   │
│   ├── config.py                     # Configuracoes
│   └── log_activity.py               # CLI para logs
│
├── docs/                             # Documentacao
│   └── ARQUITETURA.md                # Este arquivo
│
├── projects/                         # Projetos gerados
│   └── [projeto]/                    # Cada projeto em pasta separada
│       └── src/                      # Codigo fonte
│
├── uploads/                          # Arquivos enviados
│
├── templates/                        # Templates de projetos
│   ├── web-app/
│   ├── api-service/
│   └── data-analysis/
│
├── .claude/                          # Configuracao Claude Code
│
├── run_story_watcher.py              # Watcher de stories
├── run_kanban_watcher.py             # Watcher de tasks (legado)
│
├── requirements.txt                  # Dependencias Python
├── docker-compose.yml                # Infraestrutura Docker
├── .env                              # Variaveis de ambiente
├── .env.example                      # Template de .env
├── CLAUDE.md                         # Instrucoes para Claude Code
└── README.md                         # Documentacao principal
```

---

## 9. Configuracao e Deploy

### Variaveis de Ambiente (.env)

```bash
# Obrigatorio
ANTHROPIC_API_KEY=sk-ant-xxxxx

# Banco de Dados
DATABASE_URL=sqlite:///factory/database/factory.db
# ou para PostgreSQL:
# DATABASE_URL=postgresql://user:pass@localhost:5432/fabrica

# Redis (opcional, para v4 workers)
REDIS_URL=redis://localhost:6379

# Dashboard
DASHBOARD_PORT=9001
DASHBOARD_HOST=0.0.0.0

# Workers (v4)
DEFAULT_WORKERS=2
MAX_WORKERS=5

# Claude
CLAUDE_MODEL=claude-sonnet-4-20250514
CLAUDE_MAX_TOKENS=4096

# Seguranca
JWT_SECRET_KEY=your-secret-key
```

### Inicializacao

```bash
# 1. Instalar dependencias
pip install -r requirements.txt

# 2. Configurar ambiente
cp .env.example .env
# Editar .env com sua ANTHROPIC_API_KEY

# 3. Inicializar banco de dados
python factory/database/seed.py

# 4. Iniciar dashboard
python factory/dashboard/app_v6_agile.py

# 5. (Opcional) Iniciar Story Watcher
python run_story_watcher.py
```

### Docker

```yaml
# docker-compose.yml
version: '3.8'
services:
  fabrica:
    build: .
    ports:
      - "9001:9001"
    environment:
      - ANTHROPIC_API_KEY=${ANTHROPIC_API_KEY}
    volumes:
      - ./projects:/app/projects
      - ./uploads:/app/uploads
```

---

## 10. Seguranca

### Autenticacao

- JWT tokens para API (v4)
- Chave gerada automaticamente se nao configurada
- Expiracao configuravel

### Protecao de Dados

- Senhas hasheadas com bcrypt
- API keys nunca logadas
- Arquivos em diretorio isolado

### Boas Praticas

- Validacao de entrada via Pydantic
- Sanitizacao de paths de arquivo
- Rate limiting (v4)
- CORS configuravel

---

## 11. Monitoramento

### Logs

```python
# Registro de atividades
python factory/log_activity.py -a 08 -t info -m "Mensagem" -p PRJ-001
```

### Metricas Disponiveis

- Status de agentes (GET /api/agents)
- Logs de atividade (GET /api/logs)
- Progresso de stories (GET /api/stories)
- Health check (GET /api/status)

### Dashboard

- Atualizacao automatica a cada 5 segundos
- Indicadores visuais de status
- Historico de atividades

---

## 12. Extensibilidade

### Adicionar Novo Tipo de Acao no Assistente

1. Editar `execute_assistant_action()` em `app_v6_agile.py`
2. Adicionar case para nova acao
3. Atualizar system prompt com documentacao

### Adicionar Novo Modelo de Dados

1. Criar classe em `models.py`
2. Criar repository em `repositories.py`
3. Adicionar endpoints em dashboard
4. Rodar migrations se necessario

### Integrar Nova IA

1. Criar novo cliente em `factory/ai/`
2. Implementar interface similar a ClaudeClient
3. Configurar via variavel de ambiente

---

## 13. App Generator (v6.5)

### Visão Geral

O App Generator é um componente que permite gerar aplicações testáveis automaticamente a partir do código criado pelos workers.

```
┌─────────────────────────────────────────────────────────────────┐
│                      APP GENERATOR FLOW                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │   Projeto    │───>│   Análise    │───>│   Detecção   │       │
│  │  projects/   │    │   de Código  │    │   de Tipo    │       │
│  └──────────────┘    └──────────────┘    └──────────────┘       │
│                              │                    │              │
│                              v                    v              │
│                    ┌──────────────┐    ┌──────────────┐         │
│                    │   Encontra   │    │   Python/    │         │
│                    │   Modelos    │    │   Node.js    │         │
│                    └──────────────┘    └──────────────┘         │
│                              │                                   │
│                              v                                   │
│                    ┌──────────────────────────┐                 │
│                    │    GERA APLICAÇÃO        │                 │
│                    │  - main.py (FastAPI)     │                 │
│                    │  - requirements.txt      │                 │
│                    │  - iniciar_app.bat       │                 │
│                    └──────────────────────────┘                 │
│                              │                                   │
│                              v                                   │
│                    ┌──────────────────────────┐                 │
│                    │    INICIA SERVIDOR       │                 │
│                    │  - uvicorn :8000         │                 │
│                    │  - Swagger UI /docs      │                 │
│                    └──────────────────────────┘                 │
│                              │                                   │
│                              v                                   │
│                    ┌──────────────────────────┐                 │
│                    │    ABRE NAVEGADOR        │                 │
│                    │  - http://localhost:8000 │                 │
│                    └──────────────────────────┘                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Componente: `factory/core/app_generator.py`

```python
class AppGenerator:
    """Gera aplicações testáveis automaticamente."""

    def analyze_project(self) -> Dict:
        """Analisa projeto e retorna status."""
        # - Detecta tipo (Python/Node.js)
        # - Encontra modelos SQLAlchemy
        # - Encontra schemas Pydantic
        # - Encontra rotas FastAPI/Flask

    def generate_testable_app(self) -> Dict:
        """Gera aplicação FastAPI com CRUD."""
        # - Cria main.py com endpoints
        # - Cria requirements.txt
        # - Cria script de inicialização

    def start_app(self) -> Dict:
        """Inicia servidor uvicorn."""
        # - Roda em subprocess
        # - Retorna URL e PID
```

### API Endpoints

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/projects/{id}/app-status` | Analisa e retorna status do projeto |
| POST | `/api/projects/{id}/generate-app` | Gera aplicação testável |
| POST | `/api/projects/{id}/start-app` | Inicia servidor de teste |

### Interface do Usuário

O botão flutuante (FAB) no canto inferior direito indica o status:

| Estado | Cor | Ícone | Ação |
|--------|-----|-------|------|
| Desenvolvendo | Cinza | Relógio | Desabilitado |
| Pode Gerar | Azul | Engrenagem | Gerar App |
| Pronto | Verde | Play | Abrir App |

### Arquivos Gerados

```
projects/{project_id}/
├── main.py           # FastAPI app com CRUD automático
├── requirements.txt  # fastapi, uvicorn, sqlalchemy, pydantic
└── iniciar_app.bat   # Script Windows para iniciar
```

---

## Contato e Suporte

- **GitHub**: https://github.com/cruzpeanelo/fabrica-de-workers
- **Documentação Usuário**: [README.md](../README.md)

---

*Documentação Técnica v6.5 - Última atualização: Dezembro 2025*

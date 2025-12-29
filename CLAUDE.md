# Fábrica de Agentes

## Sistema de Desenvolvimento Autônomo com Agentes IA

A **Fábrica de Agentes** é uma plataforma de desenvolvimento autônomo que combina:
- **Dashboard Agile v6.5**: Gestão de User Stories com Kanban, narrativa Agile, e assistente IA
- **Workers Claude**: Processamento autônomo de tarefas com loop de auto-correção
- **Kanban Watcher**: Monitoramento automático que executa tarefas quando movidas para "To Do"
- **App Generator**: Geração automática de aplicações testáveis com 1 clique

### Dashboards Disponíveis

| Dashboard | Porta | Descrição |
|-----------|-------|-----------|
| **Agile v6.5** | 9001 | Sistema Agile completo com Stories, Tasks, Docs, Chat e App Testing |
| **Kanban v5** | 9001 | Kanban simples de tarefas |
| **Workers v4** | 9000 | Fila de jobs e workers Claude |

## Arquitetura Agile v6.5

```
User Stories → Kanban Board → Tasks → Autonomous Dev → App Generator → Testing
      │              │            │            │              │            │
  Narrativa      Drag/Drop    Subtarefas   Claude AI    Auto-detect   1-Click
  Critérios      Colunas      Progresso    Código       FastAPI       Browser
  DoD            Sprint       Output       Testes       Swagger       Validação
```

## Estrutura do Projeto

```
Fábrica de Agentes/
├── factory/
│   ├── api/                    # API REST
│   │   ├── routes.py           # Endpoints
│   │   └── auth.py             # Autenticação JWT
│   ├── core/                   # Core do sistema
│   │   ├── autonomous_loop.py  # Loop Generate→Lint→Test→Fix
│   │   ├── job_queue.py        # Redis job queue
│   │   ├── story_generator.py  # Gerador de stories
│   │   └── app_generator.py    # 🆕 Gerador de apps testáveis
│   ├── database/               # Banco de dados
│   │   ├── connection.py       # SQLite + SQLAlchemy
│   │   ├── models.py           # Modelos (Story, Task, etc)
│   │   └── repositories.py     # Data access layer
│   ├── dashboard/              # Dashboards web
│   │   ├── app_v6_agile.py     # Dashboard Agile (Stories)
│   │   ├── app_v5_kanban.py    # Dashboard Kanban (Tasks)
│   │   └── app.py              # Dashboard Workers
│   └── config.py               # Configurações
├── projects/                   # Projetos gerados
├── uploads/                    # Arquivos anexados
├── tests/                      # Testes automatizados
│   └── test_e2e_dashboard.py   # Testes E2E do dashboard
├── docs/                       # Documentação
├── run_kanban_watcher.py       # Watcher automático
├── run_kanban_dev.py           # Desenvolvimento manual
└── docker-compose.yml          # PostgreSQL + Redis
```

## Iniciando a Fábrica

### Dashboard Agile (Recomendado)
```bash
# Iniciar Dashboard Agile v6.5
python factory/dashboard/app_v6_agile.py

# Dashboard disponível em: http://localhost:9001
```

### Desenvolvimento Autônomo
```bash
# Watcher automático (monitora Kanban a cada 30s)
python run_kanban_watcher.py

# Desenvolvimento manual
python run_kanban_dev.py
```

## 🆕 App Generator - Teste com 1 Clique

O App Generator permite que usuários não-técnicos testem aplicações geradas pelos workers.

### Como Funciona

1. **Análise Automática** - Detecta tipo de projeto (Python/Node.js)
2. **Encontra Modelos** - Identifica SQLAlchemy models e Pydantic schemas
3. **Gera Aplicação** - Cria FastAPI app com CRUD para todos os modelos
4. **Inicia Servidor** - Roda uvicorn na porta 8000
5. **Abre Navegador** - Exibe Swagger UI para testes

### Botão Flutuante (FAB)

O botão flutuante no canto inferior direito mostra o status:

| Cor | Ícone | Status | Ação |
|-----|-------|--------|------|
| 🔘 Cinza | Relógio | Desenvolvendo | Aguardar |
| 🔵 Azul | Engrenagem | Pode testar | Gerar App |
| 🟢 Verde | Play | Pronto | Abrir App |

### API Endpoints - App Testing

```bash
# Verificar status do projeto
GET /api/projects/{project_id}/app-status

# Gerar aplicação testável
POST /api/projects/{project_id}/generate-app

# Iniciar servidor de teste
POST /api/projects/{project_id}/start-app
```

### Arquivos Gerados

```
projects/{project_id}/
├── main.py           # Aplicação FastAPI gerada
├── requirements.txt  # Dependências
└── iniciar_app.bat   # Script de inicialização (Windows)
```

## Sistema Agile v6.5

### Modelos de Dados

#### Story (User Story)
| Campo | Tipo | Descrição |
|-------|------|-----------|
| story_id | string | ID único (STR-0001) |
| title | string | Título da story |
| persona | string | "Como um [usuário]" |
| action | string | "Eu quero [funcionalidade]" |
| benefit | string | "Para que [benefício]" |
| acceptance_criteria | list | Critérios de aceite |
| definition_of_done | list | Definition of Done |
| story_points | int | Fibonacci (1,2,3,5,8,13,21) |
| complexity | enum | low/medium/high/very_high |
| status | enum | backlog/ready/in_progress/review/testing/done |
| priority | enum | low/medium/high/urgent |
| epic_id | string | Epic associado |
| sprint_id | string | Sprint associado |

#### StoryTask (Subtarefa)
| Campo | Tipo | Descrição |
|-------|------|-----------|
| task_id | string | ID único (STSK-0001) |
| story_id | string | Story pai |
| title | string | Título da task |
| task_type | enum | development/review/test/documentation/design |
| status | enum | pending/in_progress/completed/blocked |
| progress | int | 0-100% |
| files_created | list | Arquivos criados |
| code_output | text | Código gerado |
| test_results | json | Resultados de testes |

#### StoryDocumentation
| Campo | Tipo | Descrição |
|-------|------|-----------|
| doc_id | string | ID único (DOC-0001) |
| story_id | string | Story associada |
| doc_type | enum | technical/user/test/deployment/api |
| content | text | Conteúdo Markdown |
| test_instructions | text | Como testar |
| test_cases | list | Casos de teste |

### API Endpoints - Stories

```bash
# Stories
GET    /api/stories                     # Listar stories
POST   /api/stories                     # Criar story
GET    /api/stories/{id}                # Buscar story com tasks
PUT    /api/stories/{id}                # Atualizar story
DELETE /api/stories/{id}                # Deletar story
PATCH  /api/stories/{id}/move           # Mover no Kanban

# Story Tasks
GET    /api/stories/{id}/tasks          # Listar tasks
POST   /api/stories/{id}/tasks          # Criar task
PUT    /api/story-tasks/{id}            # Atualizar task
PATCH  /api/story-tasks/{id}/complete   # Completar task
POST   /api/story-tasks/{id}/generate-tests  # Gerar testes com IA

# Documentation
GET    /api/stories/{id}/docs           # Listar docs
POST   /api/stories/{id}/docs           # Criar doc

# Chat (Assistente IA)
GET    /api/chat/history                # Histórico
POST   /api/chat/message                # Enviar mensagem

# Upload
POST   /api/upload                      # Upload arquivo

# Épicos & Sprints
GET    /api/projects/{id}/epics         # Listar épicos
POST   /api/epics                       # Criar épico
GET    /api/projects/{id}/sprints       # Listar sprints
POST   /api/sprints                     # Criar sprint

# WebSocket
WS     /ws/notifications                # Notificações em tempo real
```

### Kanban Board

```
┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐
│  BACKLOG   │  │   READY    │  │ IN PROGRESS│  │   REVIEW   │  │  TESTING   │  │    DONE    │
├────────────┤  ├────────────┤  ├────────────┤  ├────────────┤  ├────────────┤  ├────────────┤
│ ┌────────┐ │  │ ┌────────┐ │  │ ┌────────┐ │  │            │  │            │  │            │
│ │ STR-01 │ │  │ │ STR-02 │ │  │ │ STR-03 │ │  │            │  │            │  │            │
│ │ 5 pts  │ │  │ │ 8 pts  │ │  │ │ 13 pts │ │  │            │  │            │  │            │
│ │ [████] │ │  │ │ [██──] │ │  │ │ [█───] │ │  │            │  │            │  │            │
│ └────────┘ │  │ └────────┘ │  │ └────────┘ │  │            │  │            │  │            │
└────────────┘  └────────────┘  └────────────┘  └────────────┘  └────────────┘  └────────────┘
```

### Story Card

```
┌─────────────────────────┐
│ EPIC-01      5 pts  [!] │  ← Epic + Points + Priority
│ Título da Story         │
│ ────────────────────    │
│ [████████░░] 80%        │  ← Progresso das tasks
│ 4/5 tasks | @joao       │  ← Tasks + Assignee
└─────────────────────────┘
```

## Interface de Status do Projeto

A interface mostra o progresso de forma amigável para usuários não-técnicos:

### Barra de Progresso
```
Progresso Geral                                    75%
[██████████████████████████████░░░░░░░░░░░░] 75%
```

### Timeline de Etapas
```
    ✓           ✓           ●           ○           ○
Planejamento → Desenvolvimento → Revisão → Testes → Entrega
```

### Contadores de Stories
```
┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐
│    5     │  │    3     │  │    2     │  │    8     │
│ Backlog  │  │ Em Dev   │  │ Em Teste │  │ Concluídas│
└──────────┘  └──────────┘  └──────────┘  └──────────┘
```

## Watcher Automático

O `run_kanban_watcher.py` monitora o Kanban a cada 30 segundos e processa automaticamente stories/tasks movidas para "To Do":

```bash
python run_kanban_watcher.py
```

**Fluxo:**
1. Story movida para "ready" ou "in_progress"
2. Watcher detecta a mudança
3. Claude AI processa cada task da story
4. Arquivos são gerados em `projects/{project_id}/`
5. Documentação técnica é criada automaticamente
6. Story avança pelo pipeline: in_progress → testing → done
7. 🆕 App Generator prepara aplicação para teste

## Variáveis de Ambiente

```bash
# Claude API (obrigatório)
ANTHROPIC_API_KEY=sk-ant-...

# Database (opcional - usa SQLite por padrão)
DATABASE_URL=sqlite:///factory/database/factory.db

# Dashboard
DASHBOARD_PORT=9001
```

## Identidade Visual - Belgo Arames

| Cor | Hex | Uso |
|-----|-----|-----|
| Azul Belgo | #003B4A | Header, botões primários |
| Laranja Belgo | #FF6C00 | Ações, CTAs |
| Verde Sucesso | #10B981 | Concluído, pronto para teste |
| Cinza Claro | #F3F4F6 | Background |
| Branco | #FFFFFF | Cards, painéis |

## Testes

```bash
# Rodar testes E2E do dashboard
python tests/test_e2e_dashboard.py

# Cobertura esperada: 80%+
```

---

*Fábrica de Agentes v6.5 - Sistema Agile de Desenvolvimento Autônomo com Teste de Aplicações*

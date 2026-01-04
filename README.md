# 🏭 Plataforma E

**Plataforma de Desenvolvimento Autônomo com Inteligência Artificial**

[![Python 3.10+](https://img.shields.io/badge/Python-3.10+-blue.svg)](https://python.org)
[![FastAPI](https://img.shields.io/badge/FastAPI-0.104+-green.svg)](https://fastapi.tiangolo.com)
[![Claude AI](https://img.shields.io/badge/Claude-Opus%204.5-purple.svg)](https://anthropic.com)
[![Vue.js 3](https://img.shields.io/badge/Vue.js-3.x-green.svg)](https://vuejs.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## 📋 Sumário

- [Visão Geral](#-visão-geral)
- [Funcionalidades](#-funcionalidades)
- [Quick Start](#-quick-start)
- [Arquitetura](#-arquitetura)
- [Integrações](#-integrações)
- [Para Colaboradores](#-para-colaboradores)
- [Documentação](#-documentação)
- [Roadmap](#-roadmap)
- [Licença](#-licença)

---

## 🎯 Visão Geral

A **Plataforma E** é uma plataforma enterprise que transforma a forma como software é desenvolvido, combinando:

- **Dashboard Agile v6.5**: Gestão completa de User Stories com Kanban
- **Workers Claude AI**: Processamento autônomo de tarefas com auto-correção
- **Multi-Tenant**: Isolamento total de dados entre organizações
- **Integrações Corporativas**: SAP, Jira, Azure DevOps, GitHub, Slack e mais

### O Problema que Resolvemos

| Desafio | Impacto | Nossa Solução |
|---------|---------|---------------|
| **Falta de Visibilidade** | Gestores não sabem o status real | Dashboard Kanban em tempo real |
| **Comunicação Fragmentada** | Informações perdidas entre equipes | Assistente IA centralizado |
| **Documentação Deficiente** | Conhecimento não capturado | Documentação automática |
| **Processos Manuais** | Tempo desperdiçado | Automação com Claude AI |
| **Time-to-Market Lento** | Concorrentes lançam primeiro | Entregas até 3x mais rápidas |

### ROI Esperado

| Métrica | Antes | Depois | Melhoria |
|---------|-------|--------|----------|
| Tempo por User Story | 13.5 horas | 3 horas | **-78%** |
| Bugs em produção | 15/mês | 4/mês | **-73%** |
| Cobertura de testes | 30% | 85% | **+183%** |
| Documentação atualizada | 20% | 100% | **+400%** |

---

## ✨ Funcionalidades

### 🎛️ Dashboard Agile v6.5

```
┌──────────────────────────────────────────────────────────────────────────┐
│  🏭 Plataforma E       [Projeto ▼] [Sprint ▼] [🔍 Buscar...] [?]  │
├────────────┬─────────────────────────────────────────────────────────────┤
│            │                                                             │
│  ÉPICOS    │  BACKLOG   READY    IN PROGRESS  REVIEW   TESTING   DONE   │
│  + Epic 1  │ ┌───────┐ ┌───────┐ ┌──────────┐         ┌───────┐ ┌─────┐ │
│  + Epic 2  │ │STR-001│ │STR-003│ │ STR-005  │         │STR-007│ │DONE │ │
│            │ │  5pts │ │  8pts │ │  13pts   │         │  3pts │ │     │ │
│  SPRINTS   │ │[████░]│ │[█████]│ │ [███░░░] │         │[█████]│ │[███]│ │
│  + Sprint 1│ └───────┘ └───────┘ └──────────┘         └───────┘ └─────┘ │
│            │                                                             │
│  🤖 CHAT   │                                                             │
└────────────┴─────────────────────────────────────────────────────────────┘
```

**Recursos Principais:**
- ✅ Kanban com Drag & Drop
- ✅ User Stories estruturadas (Persona, Ação, Benefício)
- ✅ Story Points e Complexidade (Fibonacci)
- ✅ Épicos e Sprints
- ✅ Assistente IA integrado
- ✅ Dark Mode persistente
- ✅ Multi-idioma (PT-BR, EN-US)
- ✅ Skeleton Loaders e animações
- ✅ Responsivo (Mobile-first)
- ✅ Atalhos de teclado

### 🔐 Segurança Enterprise

| Feature | Descrição |
|---------|-----------|
| **JWT Avançado** | Refresh tokens com rotação, blacklist |
| **RBAC** | Roles: Admin, Manager, Developer, Analyst, Viewer |
| **Multi-Tenant** | Isolamento completo de dados entre organizações |
| **2FA/MFA** | Autenticação em duas etapas |
| **Audit Log** | Trilha de auditoria completa |
| **Rate Limiting** | Proteção contra abuso por IP/usuário |
| **IP Whitelisting** | Geo-blocking por tenant |
| **Encryption at Rest** | Dados sensíveis criptografados |

### 🔗 Integrações Corporativas

| Sistema | Funcionalidades |
|---------|-----------------|
| **SAP S/4HANA** | OData v4, Business Graph, sincronização bidirecional |
| **Jira** | Boards, Sprints, Issues, Webhooks |
| **Azure DevOps** | Repos, Pipelines, Work Items |
| **GitHub/GitLab** | PRs, Issues, CI/CD |
| **Slack/Teams** | Notificações bidirecionais |
| **Power BI/Tableau** | Conectores para dashboards |
| **Microsoft Graph** | Calendar, SharePoint, OneDrive |

### 🤖 Processamento Autônomo

```
┌─────────────────────────────────────────────────────────────────┐
│                    AUTONOMOUS LOOP                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   [GENERATE] ──► [LINT] ──► [TEST] ──► [COMPLETE]               │
│       │            │           │                                 │
│       │            │           │                                 │
│       └────────────┴───────────┘                                │
│                    │                                             │
│                [FIX] ◄── (se erro, máx 5x)                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Quando uma Story é movida para "Ready":**
1. Sistema detecta automaticamente
2. Workers Claude processam as tasks
3. Código é gerado e validado
4. Testes são executados
5. Documentação é criada
6. Story avança no Kanban

---

## 🚀 Quick Start

### Pré-requisitos

- Python 3.10+
- Git
- Chave API Anthropic (Claude)
- Docker (opcional, para PostgreSQL + Redis)

### Instalação

```bash
# 1. Clone o repositório
git clone https://github.com/seu-usuario/plataforma-e.git
cd plataforma-e

# 2. Crie ambiente virtual
python -m venv venv
source venv/bin/activate  # Linux/Mac
venv\Scripts\activate     # Windows

# 3. Instale dependências
pip install -r requirements.txt

# 4. Configure ambiente
cp .env.example .env
# Edite .env e adicione sua ANTHROPIC_API_KEY

# 5. Inicialize banco de dados
python factory/database/seed.py

# 6. Inicie o dashboard
python factory/dashboard/app_v6_agile.py
```

**Acesse:** http://localhost:9001

**Login padrão:** `admin` / `admin`

### Com Docker (Produção)

```bash
# Infraestrutura completa
docker-compose up -d

# Serviços incluídos:
# - factory-api (FastAPI)
# - factory-dashboard (Vue.js)
# - postgres (PostgreSQL 16)
# - redis (Redis 7)
```

---

## 🏗️ Arquitetura

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        FÁBRICA DE AGENTES v7.0                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────────┐  │
│  │   DASHBOARD     │    │   API REST      │    │  PostgreSQL + Redis │  │
│  │   (Vue.js 3)    │◄──►│   (FastAPI)     │◄──►│   (Persistência)    │  │
│  └─────────────────┘    └────────┬────────┘    └─────────────────────┘  │
│                                  │                                       │
│                         ┌────────▼────────┐                              │
│                         │  WORKER POOL    │                              │
│                         │  (Claude AI)    │                              │
│                         └────────┬────────┘                              │
│                                  │                                       │
│          ┌───────────────────────┼───────────────────────┐              │
│          │                       │                       │              │
│          ▼                       ▼                       ▼              │
│  ┌───────────────┐    ┌──────────────────┐    ┌───────────────────┐    │
│  │  Integrações  │    │  Autonomous Loop │    │    Projetos       │    │
│  │  SAP, Jira... │    │  Generate→Test   │    │    Gerados        │    │
│  └───────────────┘    └──────────────────┘    └───────────────────┘    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Estrutura de Diretórios

```
Plataforma E/
├── factory/
│   ├── api/                    # API REST FastAPI
│   │   ├── routes.py           # Endpoints principais
│   │   ├── auth.py             # Autenticação JWT
│   │   └── middleware/         # Middlewares de segurança
│   ├── core/                   # Core do sistema
│   │   ├── autonomous_loop.py  # Loop Generate→Lint→Test→Fix
│   │   ├── job_queue.py        # Redis job queue
│   │   └── worker.py           # Claude workers
│   ├── database/               # Banco de dados
│   │   ├── connection.py       # PostgreSQL + SQLite fallback
│   │   ├── models.py           # SQLAlchemy models (21 tabelas)
│   │   └── repositories.py     # Camada de acesso a dados
│   ├── dashboard/              # Dashboards web
│   │   ├── app_v6_agile.py     # Dashboard Agile principal
│   │   ├── static/             # CSS, JS, imagens
│   │   └── templates/          # Templates HTML
│   ├── integrations/           # Integrações corporativas
│   │   ├── jira/               # Jira Agile API
│   │   ├── azure_devops/       # Azure DevOps
│   │   ├── sap_s4/             # SAP S/4HANA
│   │   └── ...                 # Outras integrações
│   └── config.py               # Configurações centralizadas
├── projects/                   # Projetos gerados
├── docs/                       # Documentação completa
├── tests/                      # Testes automatizados
└── docker-compose.yml          # Infraestrutura Docker
```

### Stack Tecnológico

| Camada | Tecnologias |
|--------|-------------|
| **Frontend** | Vue.js 3, CSS3, JavaScript ES6+ |
| **Backend** | Python 3.10+, FastAPI, SQLAlchemy |
| **Banco de Dados** | PostgreSQL 16, SQLite (dev), Redis 7 |
| **IA** | Claude API (Anthropic), Opus 4.5 |
| **Infraestrutura** | Docker, Docker Compose |
| **Monitoramento** | Prometheus, Grafana, Loki |

---

## 🔗 Integrações

### Configuração de Integrações

Todas as integrações são configuradas via variáveis de ambiente ou painel admin:

```bash
# SAP S/4HANA
SAP_S4_BASE_URL=https://seu-servidor.sap.com
SAP_S4_CLIENT_ID=seu_client_id
SAP_S4_CLIENT_SECRET=seu_client_secret

# Jira
JIRA_BASE_URL=https://sua-empresa.atlassian.net
JIRA_EMAIL=usuario@empresa.com
JIRA_API_TOKEN=seu_token

# Azure DevOps
AZURE_DEVOPS_ORG=sua-organizacao
AZURE_DEVOPS_PAT=seu_pat

# GitHub
GITHUB_TOKEN=ghp_seu_token
```

### APIs de Integração

| Endpoint | Descrição |
|----------|-----------|
| `GET /api/integrations/jira/boards` | Lista boards do Jira |
| `GET /api/integrations/azure/repos` | Lista repositórios Azure |
| `POST /api/integrations/sap/sync` | Sincroniza com SAP |
| `GET /api/integrations/health` | Status das integrações |

---

## 👥 Para Colaboradores

Queremos sua contribuição! Veja como participar:

### Fork e Clone

```bash
# 1. Faça fork no GitHub

# 2. Clone seu fork
git clone https://github.com/SEU-USUARIO/plataforma-e.git
cd plataforma-e

# 3. Adicione upstream
git remote add upstream https://github.com/cruzpeanelo/plataforma-e.git

# 4. Crie branch para sua feature
git checkout -b feature/minha-feature
```

### Setup de Desenvolvimento

```bash
# Ambiente virtual
python -m venv venv
source venv/bin/activate

# Dependências de desenvolvimento
pip install -r requirements.txt
pip install -r requirements-dev.txt

# Pre-commit hooks
pre-commit install

# Executar testes
python -m pytest tests/ -v
```

### Padrões de Código

- **Python**: PEP 8, type hints obrigatórios
- **Docstrings**: Em português (pt-BR)
- **Commits**: [Conventional Commits](https://conventionalcommits.org/)
  - `feat:` nova funcionalidade
  - `fix:` correção de bug
  - `docs:` documentação
  - `refactor:` refatoração
  - `test:` testes

### Enviando Pull Request

```bash
# 1. Atualize sua branch
git fetch upstream
git rebase upstream/main

# 2. Commit suas mudanças
git add .
git commit -m "feat(modulo): descrição da feature"

# 3. Push para seu fork
git push origin feature/minha-feature

# 4. Abra PR no GitHub
```

📖 **Veja o guia completo:** [CONTRIBUTING.md](docs/CONTRIBUTING.md)

---

## 📚 Documentação

| Documento | Descrição |
|-----------|-----------|
| [ARQUITETURA.md](docs/ARQUITETURA.md) | Arquitetura técnica detalhada |
| [API_REFERENCE.md](docs/API_REFERENCE.md) | Referência completa da API |
| [GUIA_USUARIO.md](docs/GUIA_USUARIO.md) | Manual do usuário |
| [DOCUMENTACAO_NEGOCIOS.md](docs/DOCUMENTACAO_NEGOCIOS.md) | Visão de negócio |
| [DOCUMENTACAO_TECNICA.md](docs/DOCUMENTACAO_TECNICA.md) | Detalhes técnicos |
| [CONTRIBUTING.md](docs/CONTRIBUTING.md) | Guia para contribuidores |
| [SECURITY_HARDENING.md](docs/SECURITY_HARDENING.md) | Segurança e hardening |
| [DISASTER_RECOVERY.md](docs/DISASTER_RECOVERY.md) | Recuperação de desastres |

### Documentação de Integrações

| Integração | Documentação |
|------------|--------------|
| Jira | [docs/integrations/jira.md](docs/integrations/) |
| Azure DevOps | [docs/integrations/azure-devops.md](docs/integrations/) |
| SAP S/4HANA | [docs/integrations/sap.md](docs/integrations/) |
| Microsoft Graph | [MICROSOFT_GRAPH_INTEGRATION.md](docs/MICROSOFT_GRAPH_INTEGRATION.md) |

---

## 🗺️ Roadmap

### ✅ v7.0 (Atual)

- [x] Dashboard Agile v6.5 completo
- [x] Multi-tenant com isolamento total
- [x] RBAC e permissões granulares
- [x] Integrações SAP, Jira, Azure DevOps
- [x] 2FA/MFA e segurança enterprise
- [x] Dark Mode e i18n
- [x] WebSocket para tempo real
- [x] Audit Log detalhado

### 🔄 v7.1 (Em Desenvolvimento)

- [ ] App Mobile React Native
- [ ] Planning Poker integrado
- [ ] Estimativas com ML
- [ ] Time Tracking
- [ ] Dependency Graph visual

### 📋 v8.0 (Planejado)

- [ ] Marketplace de templates
- [ ] Kubernetes deployment
- [ ] Multi-cloud (AWS, Azure, GCP)
- [ ] IA generativa para PRDs

---

## 📄 Licença

Este projeto está sob a licença MIT - veja [LICENSE](LICENSE) para detalhes.

---

## 🤝 Contato

- **Autor**: Luis Cruz
- **GitHub**: [@cruzpeanelo](https://github.com/cruzpeanelo)
- **Issues**: [Reportar Bug / Sugerir Feature](https://github.com/cruzpeanelo/plataforma-e/issues)

---

<p align="center">
  <strong>🏭 Plataforma E</strong><br>
  Desenvolvimento autônomo com Claude AI
</p>

<p align="center">
  <sub>Feito com ❤️ no Brasil</sub>
</p>

# Fabrica de Agentes

**Plataforma de Desenvolvimento Autonomo com Inteligencia Artificial**

[![Python 3.10+](https://img.shields.io/badge/Python-3.10+-blue.svg)](https://python.org)
[![FastAPI](https://img.shields.io/badge/FastAPI-0.104+-green.svg)](https://fastapi.tiangolo.com)
[![Claude AI](https://img.shields.io/badge/Claude-Sonnet%204-purple.svg)](https://anthropic.com)
[![Vue.js 3](https://img.shields.io/badge/Vue.js-3.x-green.svg)](https://vuejs.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## Sumario

- [Visao Executiva](#visao-executiva)
- [Enterprise Features v7.0](#enterprise-features-v70)
- [Beneficios para o Negocio](#beneficios-para-o-negocio)
- [Para Product Owners e Gestores](#para-product-owners-e-gestores)
- [Como Funciona](#como-funciona)
- [Casos de Uso](#casos-de-uso)
- [ROI e Metricas](#roi-e-metricas)
- [Funcionalidades do Dashboard v6.2](#funcionalidades-do-dashboard-v62)
- [Instalacao Rapida](#instalacao-rapida)
- [Documentacao Tecnica](#documentacao-tecnica)
- [Changelog](#para-mais-detalhes)

---

## Enterprise Features v7.0

A versao 7.0 traz funcionalidades enterprise-ready que transformam a Fabrica de Agentes em uma plataforma corporativa completa.

### Captura de Requisitos Multimodal

| Canal | Descricao | Tecnologia |
|-------|-----------|------------|
| **Voz** | Capture requisitos falando naturalmente | Whisper/Azure Speech |
| **Office** | Importe de Word, Excel, PowerPoint | python-docx, openpyxl |
| **WhatsApp** | Bot para captura via mensagens | WhatsApp Business API |
| **Video** | Assistente virtual por video | WebRTC + Transcricao |

### Seguranca Enterprise

| Feature | Descricao |
|---------|-----------|
| **JWT Avancado** | Refresh tokens com rotacao, token families, blacklist |
| **SAML 2.0** | SSO com Azure AD, Okta, OneLogin |
| **ABAC** | Controle de acesso baseado em atributos |
| **Vault** | Secrets management com HashiCorp Vault |
| **WAF** | Web Application Firewall (OWASP Top 10) |
| **Security Scanning** | SAST, DAST, SCA no CI/CD |
| **Tenant Isolation** | Zero data leakage entre tenants |

### RBAC e Personas

```
┌─────────────────────────────────────────────────────────────────────┐
│                         SISTEMA DE RBAC                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ROLES                           PERSONAS                            │
│  ├── Admin (Full access)         ├── Product Owner                  │
│  ├── Manager (Team management)   ├── Scrum Master                   │
│  ├── Developer (Code access)     ├── Dev Lead                       │
│  ├── Analyst (Read-only)         ├── QA Lead                        │
│  └── Viewer (Limited view)       └── Stakeholder                    │
│                                                                      │
│  PERMISSOES GRANULARES                                              │
│  stories:read, stories:write, tasks:manage, projects:admin...       │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### Integracoes Corporativas

| Sistema | Funcionalidades |
|---------|-----------------|
| **SAP S/4HANA** | OData v4, Business Graph, sincronizacao bidirecional |
| **GitHub/GitLab** | Repos, PRs, Issues, Webhooks, CI/CD integration |
| **Power BI** | Conector nativo para dashboards de metricas |
| **Tableau** | Export de dados para visualizacoes |
| **Excel** | Export/Import de stories e tasks |

### Infraestrutura Cloud

```
┌─────────────────────────────────────────────────────────────────────┐
│                    ARQUITETURA MULTI-AZ                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────────┐     ┌──────────────────┐                      │
│  │   AZ-1 (Primary) │     │   AZ-2 (Standby) │                      │
│  ├──────────────────┤     ├──────────────────┤                      │
│  │ ┌──────────────┐ │     │ ┌──────────────┐ │                      │
│  │ │  ECS Tasks   │ │◄───►│ │  ECS Tasks   │ │                      │
│  │ └──────────────┘ │     │ └──────────────┘ │                      │
│  │ ┌──────────────┐ │     │ ┌──────────────┐ │                      │
│  │ │ RDS Primary  │ │────►│ │ RDS Replica  │ │                      │
│  │ └──────────────┘ │     │ └──────────────┘ │                      │
│  │ ┌──────────────┐ │     │ ┌──────────────┐ │                      │
│  │ │ElastiCache   │ │◄───►│ │ElastiCache   │ │                      │
│  │ │  Primary     │ │     │ │  Replica     │ │                      │
│  │ └──────────────┘ │     │ └──────────────┘ │                      │
│  └──────────────────┘     └──────────────────┘                      │
│                    │                                                 │
│              ┌─────▼─────┐                                          │
│              │    ALB    │  (Cross-Zone Load Balancing)             │
│              └───────────┘                                          │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### Ambiente de Desenvolvimento

```bash
# Setup completo com Docker Compose
docker-compose up -d

# Servicos incluidos:
# - factory-api (FastAPI)
# - factory-dashboard (Vue.js)
# - factory-workers (Claude AI)
# - postgres (PostgreSQL 16)
# - redis (Redis 7)
# - minio (Object Storage)
# - vault (Secrets Management)
```

### Feature Flags

```python
# Controle de features por ambiente/tenant
from factory.core.feature_flags import FeatureFlags

flags = FeatureFlags()

if flags.is_enabled("new_dashboard", tenant_id="acme"):
    # Mostra novo dashboard
    pass

# Flags por ambiente
# DEV: all features enabled
# STAGING: beta features enabled
# PROD: stable features only
```

### Acessibilidade WCAG 2.1 AA

- Navegacao por teclado completa
- Leitor de tela compativel (ARIA labels)
- Contraste de cores adequado (4.5:1 minimo)
- Textos redimensionaveis ate 200%
- Indicadores de foco visiveis

---

## Visao Executiva

### O Problema

O desenvolvimento de software tradicional enfrenta desafios que impactam diretamente o negocio:

| Desafio | Impacto |
|---------|---------|
| **Falta de Visibilidade** | Gestores nao sabem o real status dos projetos |
| **Comunicacao Fragmentada** | Informacoes perdidas entre equipes tecnicas e de negocio |
| **Documentacao Deficiente** | Conhecimento nao eh capturado, risco quando pessoas saem |
| **Processos Manuais** | Tempo desperdicado em tarefas repetitivas |
| **Time-to-Market Lento** | Concorrentes lancam primeiro |

### Nossa Solucao

A **Fabrica de Agentes** transforma a maneira como software e desenvolvido:

```
┌─────────────────────────────────────────────────────────────────────┐
│                                                                      │
│   ┌──────────┐      ┌─────────────────┐      ┌─────────────────┐    │
│   │  VOCE    │ ───> │   ASSISTENTE    │ ───> │    SOFTWARE     │    │
│   │ (Ideias) │      │   INTELIGENTE   │      │    PRONTO       │    │
│   └──────────┘      │   (Claude IA)   │      └─────────────────┘    │
│                     └─────────────────┘                              │
│                            │                                         │
│              ┌─────────────┼─────────────┐                          │
│              │             │             │                          │
│              v             v             v                          │
│         ┌────────┐   ┌────────┐   ┌────────┐                       │
│         │ Codigo │   │ Testes │   │  Docs  │                       │
│         └────────┘   └────────┘   └────────┘                       │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

**Converse naturalmente, receba software funcionando.**

---

## Beneficios para o Negocio

### Para Executivos (C-Level)

| Beneficio | Valor Entregue |
|-----------|----------------|
| **Reducao de Custos** | Ate 40% menos horas de desenvolvimento |
| **Time-to-Market** | Entregas ate 3x mais rapidas |
| **Previsibilidade** | Visibilidade total do progresso em tempo real |
| **Qualidade** | Menos bugs em producao (-70%) |
| **Conhecimento** | Documentacao automatica preserva know-how |

### Para Gestores de Projeto

| Beneficio | Como Funciona |
|-----------|---------------|
| **Visibilidade Total** | Dashboard Kanban em tempo real |
| **Metricas Automaticas** | Story points, velocidade, burndown |
| **Rastreabilidade** | Historico completo de decisoes |
| **Comunicacao** | Assistente responde duvidas instantaneamente |

### Para Times de Produto

| Beneficio | Como Funciona |
|-----------|---------------|
| **User Stories Estruturadas** | Formato Agile (Como um... Eu quero... Para que...) |
| **Criterios Claros** | Acceptance criteria e Definition of Done |
| **Priorizacao** | Story points e complexidade para planejamento |
| **Feedback Rapido** | Prototipos funcionais em horas |

---

## Para Product Owners e Gestores

### Gestao Agile Completa

O sistema implementa as melhores praticas de metodologias ageis:

#### User Story Estruturada

```
┌─────────────────────────────────────────────────────────────────┐
│  STR-0001: Autenticacao de Usuarios                   8 pts 🔴  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  NARRATIVA                                                       │
│  Como um VENDEDOR                                                │
│  Eu quero FAZER LOGIN NO SISTEMA                                 │
│  Para que EU POSSA ACESSAR MINHAS VENDAS                        │
│                                                                  │
│  CRITERIOS DE ACEITE                                             │
│  ✓ Usuario pode fazer login com email e senha                   │
│  ✓ Sistema valida credenciais no banco                          │
│  ✓ Token JWT gerado apos autenticacao                           │
│  ✓ Mensagem de erro clara se credenciais invalidas              │
│                                                                  │
│  DEFINITION OF DONE                                              │
│  ✓ Codigo revisado por outro desenvolvedor                      │
│  ✓ Testes unitarios com 80% cobertura                           │
│  ✓ Documentacao da API atualizada                               │
│                                                                  │
│  TASKS (3/4 completas)                                          │
│  ✅ Criar endpoint de login                                      │
│  ✅ Implementar validacao JWT                                    │
│  ✅ Criar testes unitarios                                       │
│  🔄 Documentar API                                               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Kanban Visual

```
┌──────────┬──────────┬──────────┬──────────┬──────────┬──────────┐
│ BACKLOG  │  READY   │IN PROGRESS│  REVIEW  │ TESTING  │   DONE   │
├──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
│ ┌──────┐ │ ┌──────┐ │ ┌──────┐ │          │ ┌──────┐ │ ┌──────┐ │
│ │STR-05│ │ │STR-03│ │ │STR-02│ │          │ │STR-04│ │ │STR-01│ │
│ │ 8pts │ │ │ 5pts │ │ │ 3pts │ │          │ │ 5pts │ │ │ 5pts │ │
│ │[████]│ │ │[██──]│ │ │[███─]│ │          │ │[████]│ │ │[████]│ │
│ └──────┘ │ └──────┘ │ └──────┘ │          │ └──────┘ │ └──────┘ │
│          │          │          │          │          │          │
│ ┌──────┐ │          │          │          │          │ ┌──────┐ │
│ │STR-06│ │          │          │          │          │ │STR-07│ │
│ │13pts │ │          │          │          │          │ │ 3pts │ │
│ └──────┘ │          │          │          │          │ └──────┘ │
└──────────┴──────────┴──────────┴──────────┴──────────┴──────────┘
```

### Assistente Inteligente

Converse naturalmente com o sistema:

```
Voce: "Qual o status do projeto de vendas?"

Assistente: "O projeto de vendas tem 12 stories:
- 5 concluidas (38 pontos)
- 3 em progresso (21 pontos)
- 4 no backlog (34 pontos)

Velocidade atual: 38 pontos entregues.
Proxima entrega prevista: STR-0008 (Relatorio de vendas)"
```

```
Voce: "Criar uma story para notificacoes por email"

Assistente: "Story criada: STR-0015 - Notificacoes por Email
- Persona: Vendedor
- Acao: Receber notificacoes de novas vendas
- Beneficio: Acompanhar vendas em tempo real
- Story Points: 5
- Status: Backlog

Deseja que eu mova para Ready para iniciar o desenvolvimento?"
```

```
Voce: "Forcar execucao da story STR-0015"

Assistente: "Story STR-0015 movida para processamento automatico.
O sistema esta gerando o codigo. Acompanhe o progresso no Kanban.

Status: 1/4 tasks concluidas
- ✅ Criar servico de email
- 🔄 Implementar templates
- ⏳ Criar triggers
- ⏳ Testes"
```

---

## Como Funciona

### Fluxo Simplificado

```
1. VOCE DESCREVE      2. IA ESTRUTURA       3. SISTEMA DESENVOLVE
   o que precisa         em User Stories        automaticamente
        │                      │                      │
        v                      v                      v
   ┌─────────┐           ┌─────────┐           ┌─────────┐
   │"Preciso │           │ STR-001 │           │ Codigo  │
   │ de um   │    ───>   │ STR-002 │    ───>   │ Testes  │
   │ login"  │           │ STR-003 │           │ Docs    │
   └─────────┘           └─────────┘           └─────────┘

4. VOCE VALIDA        5. ENTREGA PRONTA
   e aprova              para producao
        │                      │
        v                      v
   ┌─────────┐           ┌─────────┐
   │ "Aprovado│           │ Deploy! │
   │  funciona│           │         │
   │  perfeito"           │         │
   └─────────┘           └─────────┘
```

### Processamento Autonomo

Quando uma story eh movida para "Ready":

1. **Sistema detecta** a story pronta para desenvolvimento
2. **Agentes iniciam** processamento das tasks
3. **Codigo eh gerado** com Claude AI
4. **Testes sao executados** automaticamente
5. **Documentacao eh criada** sobre o que foi feito
6. **Story avanca** no Kanban automaticamente

---

## Casos de Uso

### Caso 1: Nova Funcionalidade

**Situacao**: PO precisa de uma nova tela de relatorios

**Processo Tradicional**: 2-3 semanas
- Reunioes de refinamento
- Documentacao manual
- Desenvolvimento
- Revisao de codigo
- Testes manuais
- Documentacao tecnica

**Com Fabrica de Agentes**: 2-3 dias
```
PO: "Criar tela de relatorios de vendas com filtros por data e vendedor"

Assistente: Vou criar as stories necessarias:
- STR-001: Backend API de relatorios (5 pts)
- STR-002: Componente de filtros (3 pts)
- STR-003: Visualizacao de dados (5 pts)
- STR-004: Testes e documentacao (2 pts)

[Executa automaticamente em 2-3 dias]
```

### Caso 2: Analise de Documentos

**Situacao**: Cliente envia especificacao em PDF

**Processo**:
```
1. Upload do PDF no sistema
2. IA analisa e extrai requisitos
3. Sistema sugere User Stories estruturadas
4. PO aprova stories sugeridas
5. Desenvolvimento inicia automaticamente
```

### Caso 3: Bug em Producao

**Situacao**: Usuario reporta erro no login

**Processo**:
```
Usuario: "Login nao funciona quando email tem acento"

Assistente: Identifiquei o problema. Criando story de correcao:
- STR-BUG-001: Corrigir encoding de email no login
- Prioridade: Urgente
- Estimativa: 2 pontos

Iniciando correcao automatica...
[30 minutos depois]
Correcao implementada e testada. Pronta para deploy.
```

---

## ROI e Metricas

### Comparativo de Tempo

| Atividade | Tradicional | Com Fabrica | Economia |
|-----------|-------------|-------------|----------|
| Criar User Story | 30 min | 2 min | 93% |
| Desenvolvimento | 8 horas | 3 horas | 62% |
| Documentacao | 2 horas | Automatico | 100% |
| Code Review | 1 hora | Automatico | 100% |
| Testes | 2 horas | Automatico | 100% |
| **Total** | **13.5 horas** | **3 horas** | **78%** |

### Metricas de Qualidade

| Metrica | Antes | Depois | Melhoria |
|---------|-------|--------|----------|
| Bugs em producao | 15/mes | 4/mes | -73% |
| Cobertura de testes | 30% | 85% | +183% |
| Documentacao atualizada | 20% | 100% | +400% |
| Tempo de onboarding | 2 semanas | 2 dias | -86% |

### Exemplo de ROI

**Cenario**: Equipe de 5 desenvolvedores, custo medio R$ 15.000/mes

| Item | Tradicional | Com Fabrica |
|------|-------------|-------------|
| Produtividade | 100% | 160% |
| Custo equivalente | R$ 75.000 | R$ 46.875 |
| **Economia mensal** | - | **R$ 28.125** |
| **Economia anual** | - | **R$ 337.500** |

---

## Funcionalidades do Dashboard v6.2

O Dashboard Agile oferece uma experiencia completa de gestao de projetos com recursos avancados de usabilidade.

### Visao Geral da Interface

```
+--------------------------------------------------------------------------------+
|  FA  Fabrica de Agentes          [Projeto ▼] [Sprint ▼] [🔍 Buscar...] [?] [+] |
+----------------+---------------------------------------------------------------+
|                |                                                               |
|   EPICS        |  [Prioridade ▼] [Assignee ▼] [Filtros ativos] 12 stories     |
|   + Epic 1     |                                                               |
|   + Epic 2     |  BACKLOG   READY    IN PROGRESS  REVIEW   TESTING   DONE     |
|                | +--------+--------+------------+--------+---------+--------+ |
|   SPRINTS      | |STR-001 |STR-003 | STR-005    |        |STR-007  |STR-009 | |
|   + Sprint 1   | |  5pts  |  8pts  |   13pts    |        |  3pts   |  5pts  | |
|   + Sprint 2   | |[████░░]|[██████]| [████░░░░] |        |[██████] |[██████]| |
|                | +--------+--------+------------+--------+---------+--------+ |
|   ASSISTENTE   |                                                               |
|   🤖 Chat      |                                                               |
+----------------+---------------------------------------------------------------+
```

### Busca e Filtros Avancados

| Funcionalidade | Descricao | Atalho |
|----------------|-----------|--------|
| **Busca Global** | Filtra por titulo, ID, descricao, persona, acao | `/` |
| **Filtro Prioridade** | Urgente, Alta, Media, Baixa | - |
| **Filtro Assignee** | Todos, Sem assignee | - |
| **Limpar Filtros** | Remove todos os filtros ativos | - |
| **Contador** | Mostra quantidade de stories filtradas | - |

### Acoes Rapidas nos Cards

Ao passar o mouse sobre um card de story:

```
+---------------------------+
|  [→] [🗑]    EPIC-01  5pts| <- Quick actions no hover
|  Login de Usuarios        |
|  ----------------------   |
|  [████████░░] 80%         |
|  4/5 tasks | @joao        |
+---------------------------+
```

- **[→]** Mover para proxima coluna
- **[🗑]** Excluir story (com confirmacao)

### Menu de Contexto (Clique Direito)

```
+----------------------+
| 📋 Abrir detalhes    |
|----------------------|
| 1️⃣ Mover p/ Backlog  |
| 2️⃣ Mover p/ Ready    |
| 3️⃣ Mover p/ Progress |
| 4️⃣ Mover p/ Review   |
| 5️⃣ Mover p/ Testing  |
| 6️⃣ Mover p/ Done     |
|----------------------|
| 📄 Copiar ID         |
| 🗑️ Excluir          |
+----------------------+
```

### Atalhos de Teclado

| Categoria | Atalho | Acao |
|-----------|--------|------|
| **Navegacao** | `/` | Focar na busca |
| | `Esc` | Fechar modal/painel |
| | `?` | Ver atalhos |
| **Acoes** | `N` | Nova Story |
| | `T` | Nova Task |
| | `E` | Editar story |
| | `Del` | Excluir story |
| **Mover Story** | `1` | Mover para Backlog |
| | `2` | Mover para Ready |
| | `3` | Mover para In Progress |
| | `4` | Mover para Review |
| | `5` | Mover para Testing |
| | `6` | Mover para Done |

### Sistema de Notificacoes

Feedback visual em tempo real para todas as acoes:

| Tipo | Cor | Exemplo |
|------|-----|---------|
| **Sucesso** | Verde | "Story criada: STR-0001" |
| **Erro** | Vermelho | "Erro ao salvar" |
| **Info** | Azul | "Story movida para Done" |
| **Alerta** | Amarelo | "Ja esta na ultima coluna" |

### Confirmacao de Acoes Destrutivas

Antes de excluir uma story ou task, o sistema exibe um dialogo de confirmacao:

```
+----------------------------------------+
|  ⚠️  Excluir Story                      |
|----------------------------------------|
|  Tem certeza que deseja excluir?       |
|  Todas as tasks serao perdidas.        |
|                                        |
|  [ STR-0001: Login de Usuarios ]       |
|                                        |
|  Esta acao nao pode ser desfeita.      |
|                                        |
|          [Cancelar]  [Excluir Story]   |
+----------------------------------------+
```

### Onboarding para Novos Usuarios

Ao acessar sem projeto selecionado:

```
+------------------------------------------+
|          🚀 Bem-vindo a Fabrica!          |
|                                          |
|  1. Crie ou selecione um projeto         |
|  2. Adicione User Stories                |
|  3. Arraste stories pelo Kanban          |
|  4. Use o chat para comandos             |
|                                          |
|  Dica: Pressione [?] para atalhos        |
+------------------------------------------+
```

### Drag and Drop Melhorado

- Arraste stories entre colunas
- Feedback visual durante arraste (card elevado e rotacionado)
- Toast de confirmacao ao soltar

### Para Mais Detalhes

Consulte o [CHANGELOG.md](CHANGELOG.md) para historico completo de versoes e funcionalidades.

---

## Instalacao Rapida

### Pre-requisitos

- Python 3.10+
- Chave API Anthropic (Claude)

### Passos

```bash
# 1. Clone o repositorio
git clone https://github.com/cruzpeanelo/fabrica-de-workers.git
cd fabrica-de-workers

# 2. Crie ambiente virtual
python -m venv venv
source venv/bin/activate  # Linux/Mac
venv\Scripts\activate     # Windows

# 3. Instale dependencias
pip install -r requirements.txt

# 4. Configure ambiente
cp .env.example .env
# Edite .env e adicione sua ANTHROPIC_API_KEY

# 5. Inicialize banco de dados
python factory/database/seed.py

# 6. Inicie o dashboard
python factory/dashboard/app_v6_agile.py
```

**Acesse**: http://localhost:9001

### Iniciar Processamento Automatico

```bash
# Em outro terminal
python run_story_watcher.py
```

---

## Documentacao Tecnica

Para equipes de TI, arquitetos e desenvolvedores:

**[Documentacao de Arquitetura Completa](docs/ARQUITETURA.md)**

Conteudo:
- Diagrama de arquitetura
- Stack tecnologica detalhada
- Modelos de dados e ER
- APIs e endpoints
- Integracao com Claude AI
- Fluxos de processamento
- Estrutura de arquivos
- Configuracao e deploy
- Seguranca
- Monitoramento
- Extensibilidade

---

## Proposta de Valor Resumida

| Para Quem | O Que Entregamos |
|-----------|------------------|
| **Executivos** | Reducao de custos, time-to-market, previsibilidade |
| **Gestores** | Visibilidade total, metricas automaticas, comunicacao |
| **Product Owners** | Gestao agil completa, stories estruturadas |
| **Desenvolvedores** | Menos trabalho repetitivo, mais foco em inovacao |
| **Empresa** | Conhecimento documentado, menos dependencia de pessoas |

---

## Arquitetura v4.0

```
+------------------------------------------------------------------+
|                     FABRICA DE WORKERS v4.0                       |
+------------------------------------------------------------------+
|                                                                    |
|  +------------------+    +------------------+    +---------------+ |
|  |   DASHBOARD      |    |   API REST       |    |   PostgreSQL  | |
|  |   (Vue.js 3)     |<-->|   (FastAPI)      |<-->|   + Redis     | |
|  +------------------+    +------------------+    +---------------+ |
|         ^                        |                                 |
|         |              +---------v----------+                      |
|         |              |    REDIS QUEUE     |                      |
|         |              |   (Job Manager)    |                      |
|         |              +---------+----------+                      |
|         |                        |                                 |
|  +------+------------------------v-------------------------------+ |
|  |                    WORKER POOL (2-5)                          | |
|  |                                                                | |
|  |  +-------------+  +-------------+  +-------------+            | |
|  |  |  Worker 1   |  |  Worker 2   |  |  Worker N   |            | |
|  |  | Claude API  |  | Claude API  |  | Claude API  |            | |
|  |  +------+------+  +------+------+  +------+------+            | |
|  |         |                |                |                    | |
|  |         v                v                v                    | |
|  |  +--------------------------------------------------+         | |
|  |  |           AUTONOMOUS LOOP (por job)              |         | |
|  |  |                                                  |         | |
|  |  |   +----------+    +------+    +------+          |         | |
|  |  |   | Generate |--->| Lint |--->| Test |          |         | |
|  |  |   +----------+    +------+    +--+---+          |         | |
|  |  |        ^                         |              |         | |
|  |  |        |     +-------+           |              |         | |
|  |  |        +-----| Fix   |<----------+              |         | |
|  |  |              +-------+   (max 5x)               |         | |
|  |  +--------------------------------------------------+         | |
|  +---------------------------------------------------------------+ |
|                                |                                   |
|                   +------------v-------------+                     |
|                   |      projects/ folder    |                     |
|                   |   (Codigo Gerado)        |                     |
|                   +--------------------------+                     |
+------------------------------------------------------------------+
```

---

## Componentes

### 1. API REST (`factory/api/`)

| Componente | Arquivo | Responsabilidade |
|------------|---------|------------------|
| **Routes** | `routes.py` | Endpoints REST para jobs, workers e queue |
| **Auth** | `auth.py` | Autenticacao JWT com chave persistente |
| **Rate Limit** | `rate_limit.py` | Limitacao de requisicoes via Redis |

**Endpoints Principais:**
```
POST   /api/v1/jobs           - Criar job de desenvolvimento
GET    /api/v1/jobs/{id}      - Status do job
GET    /api/v1/jobs           - Listar jobs
DELETE /api/v1/jobs/{id}      - Cancelar job
GET    /api/v1/queue/stats    - Estatisticas da fila
GET    /api/v1/workers        - Listar workers ativos
POST   /api/v1/auth/login     - Autenticacao
GET    /api/v1/health         - Health check
```

### 2. Core (`factory/core/`)

| Componente | Arquivo | Responsabilidade |
|------------|---------|------------------|
| **Job Queue** | `job_queue.py` | Fila Redis FIFO para jobs |
| **Worker** | `worker.py` | Claude Worker que processa jobs |
| **Autonomous Loop** | `autonomous_loop.py` | Loop Generate->Lint->Test->Fix |

**Job Queue:**
- Fila FIFO para jobs pendentes
- Pub/Sub para eventos em tempo real
- Fallback para SQLite se Redis indisponivel

**Worker:**
- Consome jobs da fila Redis
- Executa autonomous loop com Claude API
- Heartbeat para monitoramento de saude
- Retries automaticos em caso de falha

**Autonomous Loop:**
```
1. SETUP    - Prepara ambiente do projeto
2. GENERATE - Gera codigo via Claude API
3. LINT     - Executa linter (ruff/eslint)
4. TEST     - Executa testes (pytest/jest)
5. FIX      - Se erro, Claude corrige (max 5x)
6. COMPLETE - Projeto pronto em projects/
```

### 3. Database (`factory/database/`)

| Componente | Arquivo | Responsabilidade |
|------------|---------|------------------|
| **Connection** | `connection.py` | PostgreSQL + Redis + SQLite fallback |
| **Models** | `models.py` | SQLAlchemy models (6 tabelas) |
| **Repositories** | `repositories.py` | Camada de acesso a dados |

**Modelos:**
| Tabela | Descricao |
|--------|-----------|
| `projects` | Metadados de projetos |
| `jobs` | Fila de trabalho (unidade principal) |
| `workers` | Registro de workers ativos |
| `failure_history` | Historico de falhas para analise |
| `users` | Autenticacao de usuarios |
| `activity_logs` | Logs de auditoria |

### 4. Dashboard (`factory/dashboard/`)

| Componente | Arquivo | Responsabilidade |
|------------|---------|------------------|
| **App** | `app_v4.py` | Dashboard Vue.js 3 worker-centric |

**Funcionalidades:**
- Visao geral da fila (pendentes, processando, completos)
- Painel de workers (status, job atual, metricas)
- Lista de jobs com filtros e progresso
- Criacao de jobs via interface
- Atualizacao automatica a cada 5 segundos

### 5. Config (`factory/config.py`)

Configuracoes centralizadas:
- Paths do projeto
- Conexoes de banco (PostgreSQL, Redis, SQLite)
- Workers (min, max, timeouts)
- Claude API (modelo, tokens)
- Rate limiting
- MCP tools

### 6. Scripts (`factory/scripts/`)

| Script | Comando | Descricao |
|--------|---------|-----------|
| `start_workers.py` | `python factory/scripts/start_workers.py -w 3` | Inicia pool de workers |
| `start_all.py` | `python factory/scripts/start_all.py` | Inicia dashboard + workers |
| `init_db.py` | `python factory/scripts/init_db.py --seed` | Inicializa banco de dados |

---

## Instalacao

### Pre-requisitos

- Python 3.10+
- Docker (para PostgreSQL + Redis)
- Chave API Anthropic

### Instalacao Rapida

```bash
# Clone o repositorio
git clone https://github.com/cruzpeanelo/fabrica-de-workers.git
cd fabrica-de-workers

# Ambiente virtual
python -m venv venv
source venv/bin/activate  # Linux/Mac
venv\Scripts\activate     # Windows

# Dependencias
pip install -r requirements.txt

# Configurar ambiente
cp .env.example .env
# Edite .env e adicione sua ANTHROPIC_API_KEY

# Iniciar infraestrutura (PostgreSQL + Redis)
docker-compose up -d

# Inicializar banco de dados
python factory/scripts/init_db.py --seed

# Iniciar tudo (Dashboard + Workers)
python factory/scripts/start_all.py --workers 2
```

**Acesse:** http://localhost:9000

### Sem Docker (SQLite + Redis local)

```bash
# Se Redis instalado localmente
redis-server &

# Ou use apenas SQLite (sem Redis)
# O sistema faz fallback automaticamente

python factory/scripts/start_all.py
```

---

## Uso

### Via Dashboard (Recomendado)

1. Acesse http://localhost:9000
2. Clique em "Novo Job"
3. Preencha descricao e stack tecnologica
4. Acompanhe o progresso em tempo real
5. Projeto gerado em `projects/`

### Via API

```bash
# Autenticar
TOKEN=$(curl -s -X POST http://localhost:9000/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "admin", "password": "admin123"}' | jq -r '.access_token')

# Criar job
curl -X POST http://localhost:9000/api/v1/jobs \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "description": "API REST para gerenciamento de tarefas com autenticacao JWT",
    "tech_stack": "python,fastapi,postgresql",
    "features": ["CRUD de tarefas", "Autenticacao JWT", "Rate limiting"]
  }'

# Verificar status
curl http://localhost:9000/api/v1/jobs/{job_id} \
  -H "Authorization: Bearer $TOKEN"

# Ver estatisticas da fila
curl http://localhost:9000/api/v1/queue/stats \
  -H "Authorization: Bearer $TOKEN"

# Listar workers
curl http://localhost:9000/api/v1/workers \
  -H "Authorization: Bearer $TOKEN"
```

### Via Python

```python
import asyncio
from factory.core.job_queue import get_queue

async def create_job():
    queue = await get_queue()

    job = await queue.enqueue({
        "description": "Sistema de blog com posts e comentarios",
        "tech_stack": "python,fastapi,react",
        "features": ["CRUD posts", "Comentarios", "Busca"]
    })

    print(f"Job criado: {job['job_id']}")

    # Acompanhar status
    while True:
        status = await queue.get_job(job['job_id'])
        print(f"Status: {status['status']} - {status['current_step']}")

        if status['status'] in ['completed', 'failed']:
            break

        await asyncio.sleep(5)

asyncio.run(create_job())
```

---

## Estrutura do Projeto

```
Fabrica de Workers/
├── factory/
│   ├── api/                    # API REST
│   │   ├── routes.py           # Endpoints de jobs/workers
│   │   ├── auth.py             # JWT authentication
│   │   └── rate_limit.py       # Redis rate limiting
│   ├── core/                   # Core do sistema
│   │   ├── job_queue.py        # Redis job queue
│   │   ├── worker.py           # Claude workers
│   │   └── autonomous_loop.py  # Loop de desenvolvimento
│   ├── database/               # Banco de dados
│   │   ├── connection.py       # PostgreSQL + Redis + SQLite
│   │   ├── models.py           # SQLAlchemy models
│   │   └── repositories.py     # Data access layer
│   ├── dashboard/              # Dashboard web
│   │   └── app_v4.py           # FastAPI + Vue.js
│   ├── scripts/                # Scripts de inicializacao
│   │   ├── start_workers.py    # Launcher de workers
│   │   ├── start_all.py        # Launcher completo
│   │   └── init_db.py          # Inicializacao do banco
│   └── config.py               # Configuracoes centralizadas
├── projects/                   # Projetos gerados
├── docker-compose.yml          # PostgreSQL + Redis
├── .env.example                # Template de variaveis
├── requirements.txt            # Dependencias Python
└── README.md
```

---

## Configuracao

### Variaveis de Ambiente

| Variavel | Descricao | Padrao |
|----------|-----------|--------|
| `ANTHROPIC_API_KEY` | Chave API Claude **(obrigatorio)** | - |
| `DATABASE_URL` | PostgreSQL connection string | SQLite local |
| `REDIS_URL` | Redis connection string | redis://localhost:6379 |
| `DEFAULT_WORKERS` | Workers iniciais | 2 |
| `MAX_WORKERS` | Maximo de workers | 5 |
| `CLAUDE_MODEL` | Modelo Claude | claude-sonnet-4-20250514 |
| `RATE_LIMIT_REQUESTS` | Requisicoes por janela | 100 |
| `RATE_LIMIT_WINDOW` | Janela em segundos | 60 |
| `JWT_SECRET_KEY` | Chave JWT (gerada automaticamente) | - |

### docker-compose.yml

```yaml
services:
  postgres:
    image: postgres:16-alpine
    environment:
      POSTGRES_USER: fabrica
      POSTGRES_PASSWORD: fabrica_secret
      POSTGRES_DB: fabrica_db
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
```

---

## Fluxo de Trabalho

```
1. Usuario cria JOB via API/Dashboard
       |
       v
2. Job entra na REDIS QUEUE (FIFO)
       |
       v
3. WORKER disponivel pega o job
       |
       v
4. AUTONOMOUS LOOP executa:

   [GENERATE] --> Claude gera codigo
        |
        v
   [LINT] --> ruff/eslint valida
        |
        +---> Erro? --> [FIX] --> Claude corrige --> volta para LINT
        |
        v
   [TEST] --> pytest/jest executa
        |
        +---> Erro? --> [FIX] --> Claude corrige --> volta para LINT
        |
        v
   [COMPLETE] --> Projeto salvo em projects/

5. Status atualizado em tempo real via Redis Pub/Sub
```

---

## Comparacao: v3.0 vs v4.0

| Aspecto | v3.0 (Agentes) | v4.0 (Workers) |
|---------|----------------|----------------|
| Unidade de trabalho | 19 agentes especializados | 2-5 workers genericos |
| Coordenacao | Complexa entre agentes | Fila simples Redis |
| Escalabilidade | Dificil | Horizontal (mais workers) |
| Banco | SQLite apenas | PostgreSQL + Redis |
| API | 80+ endpoints | ~15 endpoints |
| Dashboard | 5000+ linhas | ~800 linhas |
| Auto-correcao | Limitada | Loop ate 5 tentativas |

---

## Roadmap

### v4.0 (Atual)
- [x] Workers Claude com pool configuravel
- [x] Redis Queue para jobs
- [x] PostgreSQL + Redis infrastructure
- [x] Autonomous loop (Generate -> Lint -> Test -> Fix)
- [x] JWT authentication persistente
- [x] Rate limiting via Redis
- [x] Dashboard worker-centric
- [x] API simplificada

### v4.1 (Planejado)
- [ ] WebSocket para atualizacoes em tempo real
- [ ] Multiplos modelos Claude (Opus, Haiku)
- [ ] MCP tools integration
- [ ] Logs estruturados (ELK stack)

### v5.0 (Futuro)
- [ ] Multi-tenant (SaaS)
- [ ] Kubernetes deployment
- [ ] CI/CD integrado
- [ ] Marketplace de templates

---

## Contribuindo

1. Fork o repositorio
2. Crie uma branch (`git checkout -b feature/nova-feature`)
3. Commit suas mudancas (`git commit -m 'Add nova feature'`)
4. Push para a branch (`git push origin feature/nova-feature`)
5. Abra um Pull Request

---

## Licenca

MIT License - Veja [LICENSE](LICENSE) para detalhes.

---

## Contato

- **Autor**: Luis Cruz
- **GitHub**: [cruzpeanelo](https://github.com/cruzpeanelo)

---

<p align="center">
  <strong>Fabrica de Workers</strong> - Desenvolvimento autonomo com Claude AI
</p>

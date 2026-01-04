# 📋 Funcionalidades da Plataforma E

Documentação completa de todas as funcionalidades disponíveis na plataforma.

---

## Índice

- [Dashboard Agile](#-dashboard-agile)
- [Segurança Enterprise](#-segurança-enterprise)
- [Integrações Corporativas](#-integrações-corporativas)
- [UI/UX](#-uiux)
- [Features Agile](#-features-agile)
- [Infraestrutura](#-infraestrutura)

---

## 🎛️ Dashboard Agile

O Dashboard Agile v6.5 é o coração da Plataforma E.

### Kanban Board

```
┌──────────┬──────────┬────────────┬──────────┬──────────┬──────────┐
│ BACKLOG  │  READY   │ IN PROGRESS│  REVIEW  │ TESTING  │   DONE   │
├──────────┼──────────┼────────────┼──────────┼──────────┼──────────┤
│ ┌──────┐ │ ┌──────┐ │ ┌────────┐ │          │ ┌──────┐ │ ┌──────┐ │
│ │STR-01│ │ │STR-02│ │ │ STR-03 │ │          │ │STR-04│ │ │STR-05│ │
│ │ 5pts │ │ │ 8pts │ │ │ 13pts  │ │          │ │ 3pts │ │ │ 5pts │ │
│ │[████]│ │ │[██──]│ │ │ [███─] │ │          │ │[████]│ │ │[████]│ │
│ └──────┘ │ └──────┘ │ └────────┘ │          │ └──────┘ │ └──────┘ │
└──────────┴──────────┴────────────┴──────────┴──────────┴──────────┘
```

**Recursos:**
- 6 colunas customizáveis
- Drag & Drop entre colunas
- Filtros por épico, sprint, prioridade
- Contadores de stories por coluna
- WIP Limits configuráveis

### User Stories

Formato padrão de narrativa Agile:

```
┌─────────────────────────────────────────────────────────────┐
│  STR-0001: Autenticação de Usuários               8 pts 🔴  │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  NARRATIVA                                                   │
│  Como um VENDEDOR                                            │
│  Eu quero FAZER LOGIN NO SISTEMA                             │
│  Para que EU POSSA ACESSAR MINHAS VENDAS                     │
│                                                              │
│  CRITÉRIOS DE ACEITE                                         │
│  ✓ Usuário pode fazer login com email e senha               │
│  ✓ Sistema valida credenciais no banco                      │
│  ✓ Token JWT gerado após autenticação                       │
│                                                              │
│  DEFINITION OF DONE                                          │
│  ✓ Código revisado                                          │
│  ✓ Testes com 80% cobertura                                 │
│  ✓ Documentação atualizada                                  │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

**Campos:**
- **Narrativa**: Persona, Ação, Benefício
- **Story Points**: Fibonacci (1, 2, 3, 5, 8, 13, 21)
- **Complexidade**: Low, Medium, High, Very High
- **Prioridade**: Low, Medium, High, Urgent
- **Critérios de Aceite**: Lista de validações
- **Definition of Done**: Checklist de conclusão

### Tasks (Subtarefas)

Cada story pode ter múltiplas tasks:

| Tipo | Descrição |
|------|-----------|
| **Development** | Implementação de código |
| **Review** | Code review |
| **Test** | Testes automatizados |
| **Documentation** | Documentação técnica |
| **Design** | Mockups e wireframes |

### Épicos e Sprints

**Épicos:**
- Agrupamento de stories relacionadas
- Cores para identificação visual
- Progresso agregado

**Sprints:**
- Período fixo (1-4 semanas)
- Velocity tracking
- Burndown chart

### Assistente IA (Chat)

Converse naturalmente:

```
Você: "Qual o status do projeto de vendas?"

Assistente: "O projeto de vendas tem 12 stories:
- 5 concluídas (38 pontos)
- 3 em progresso (21 pontos)
- 4 no backlog (34 pontos)

Velocidade atual: 38 pontos/sprint."
```

**Comandos:**
- Criar stories por descrição
- Mover stories no Kanban
- Gerar relatórios
- Responder dúvidas

---

## 🔐 Segurança Enterprise

### Autenticação

| Feature | Descrição |
|---------|-----------|
| **JWT Tokens** | Access token + Refresh token |
| **Token Rotation** | Rotação automática de refresh tokens |
| **Token Blacklist** | Revogação de tokens comprometidos |
| **2FA/MFA** | TOTP com Google Authenticator |

### RBAC (Role-Based Access Control)

```
┌─────────────────────────────────────────────────────────────┐
│                      SISTEMA DE RBAC                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ROLES                        PERMISSÕES                     │
│  ├── Admin (Full access)      stories:*, projects:*, admin:*│
│  ├── Manager (Team mgmt)      stories:rw, projects:rw       │
│  ├── Developer (Code)         stories:rw, tasks:rw          │
│  ├── Analyst (Read-only)      stories:r, projects:r         │
│  └── Viewer (Limited)         stories:r (own)               │
│                                                              │
│  PERSONAS                                                    │
│  ├── Product Owner            stories:*, sprints:*          │
│  ├── Scrum Master             sprints:*, retrospectives:*   │
│  ├── Dev Lead                 stories:rw, code_review:*     │
│  └── Stakeholder              reports:r, dashboards:r       │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Multi-Tenant

| Feature | Descrição |
|---------|-----------|
| **Tenant Isolation** | Dados 100% separados por organização |
| **IP Whitelisting** | IPs permitidos por tenant |
| **Geo-blocking** | Restrição por região geográfica |
| **Custom Branding** | Logo e cores por tenant |

### Outras Features de Segurança

| Feature | Descrição |
|---------|-----------|
| **Password Policies** | Regras configuráveis por tenant |
| **Audit Log** | Trilha de auditoria completa |
| **Encryption at Rest** | Dados sensíveis criptografados |
| **Security Headers** | CSP, HSTS, X-Frame-Options |
| **Input Validation** | Middleware de validação |
| **Rate Limiting** | Proteção contra abuso |
| **API Key Management** | Chaves para integrações |

---

## 🔗 Integrações Corporativas

### SAP S/4HANA

```python
# Exemplo de uso
from factory.integrations.sap_s4 import SAPS4HANAClient

client = SAPS4HANAClient(
    base_url="https://sap.empresa.com",
    client_id="xxx",
    client_secret="yyy"
)

# Buscar dados via OData
projects = await client.get_odata("/sap/opu/odata/sap/API_PROJECT")
```

**Features:**
- OData v4 API
- Business Graph
- Analyzers: CDS, RAP, Fiori
- Sincronização bidirecional

### Jira

```python
from factory.integrations.jira import JiraAgileClient

client = JiraAgileClient(
    base_url="https://empresa.atlassian.net",
    email="user@empresa.com",
    api_token="xxx"
)

# Sincronizar board
board = await client.get_board(board_id=123)
sprints = await client.get_sprints(board_id=123)
```

**Features:**
- Boards, Sprints, Epics
- Issues, Subtasks
- Webhooks bidirecionais
- Velocity tracking

### Azure DevOps

```python
from factory.integrations.azure_devops import AzureDevOpsClient

client = AzureDevOpsClient(
    organization="minha-org",
    pat="xxx"
)

# Listar repos
repos = await client.get_repos(project="MeuProjeto")
pipelines = await client.get_pipelines(project="MeuProjeto")
```

**Features:**
- Repos e Branches
- Work Items
- Pipelines CI/CD
- Pull Requests

### Microsoft Graph

| Integração | Funcionalidades |
|------------|-----------------|
| **Calendar** | Eventos, reuniões, disponibilidade |
| **SharePoint** | Sites, listas, documentos |
| **OneDrive** | Upload/download de arquivos |
| **Teams** | Canais, mensagens, notificações |

### Outras Integrações

| Sistema | Status |
|---------|--------|
| GitHub | ✅ Completo |
| GitLab | ✅ Completo |
| Slack | ✅ Bidirecional |
| Power BI | ✅ Conector |
| Tableau | ✅ Conector |
| Salesforce | ✅ REST + Bulk API |

---

## 🎨 UI/UX

### Dark Mode

Toggle no header para alternar entre temas:
- **Light**: Fundo branco, texto escuro
- **Dark**: Fundo escuro, texto claro
- Preferência salva no localStorage

### Multi-idioma (i18n)

| Idioma | Código |
|--------|--------|
| Português Brasil | pt-BR |
| English (US) | en-US |

### Responsividade

| Breakpoint | Layout |
|------------|--------|
| Mobile (< 768px) | 1 coluna, menu hamburguer |
| Tablet (768-1024px) | 2 colunas, sidebar colapsável |
| Desktop (> 1024px) | Layout completo |

### Componentes Visuais

| Componente | Descrição |
|------------|-----------|
| **Skeleton Loaders** | Shimmer enquanto carrega |
| **Empty States** | Estados vazios com CTAs |
| **Toasts** | Notificações flutuantes |
| **Modals** | Diálogos contextuais |
| **Tooltips** | Dicas de uso |

### Acessibilidade (WCAG 2.1 AA)

- Navegação por teclado
- ARIA labels
- Contraste 4.5:1 mínimo
- Focus visible
- Skip links

---

## ⚡ Features Agile

### Sprint Burndown

```
Points  │
   40 ──┼─────────────────────────────
        │ \
   30 ──┼──\────────────────────────
        │   \   Real
   20 ──┼────\──────────────────────
        │     \
   10 ──┼──────\────────────────────
        │       \_______
    0 ──┼─────────────────\─────────
        └─────────────────────────────
           1  2  3  4  5  6  7  8  9  Days
```

### Velocity Tracking

| Sprint | Planejado | Entregue | Velocity |
|--------|-----------|----------|----------|
| Sprint 1 | 40 pts | 35 pts | 35 |
| Sprint 2 | 38 pts | 40 pts | 40 |
| Sprint 3 | 42 pts | 38 pts | 38 |
| **Média** | - | - | **37.6** |

### WIP Limits

Limites de trabalho em progresso por coluna:
- Evita sobrecarga
- Identifica gargalos
- Alerta visual quando excede

### Bulk Actions

Operações em lote:
- Mover múltiplas stories
- Excluir múltiplas stories
- Alterar prioridade em lote
- Atribuir assignee em lote

### Templates de Stories

| Template | Uso |
|----------|-----|
| Feature | Nova funcionalidade |
| Bug Fix | Correção de bug |
| Tech Debt | Débito técnico |
| Spike | Pesquisa/investigação |
| Melhoria | Aprimoramento |

---

## 🏗️ Infraestrutura

### Banco de Dados

| Ambiente | Database | Descrição |
|----------|----------|-----------|
| Dev | SQLite | Local, sem setup |
| Staging | PostgreSQL | Docker Compose |
| Prod | PostgreSQL + Redis | AWS/Azure |

### Docker

```yaml
# docker-compose.yml
services:
  factory-api:
    build: .
    ports:
      - "9001:9001"

  postgres:
    image: postgres:16-alpine

  redis:
    image: redis:7-alpine
```

### Monitoramento

| Ferramenta | Uso |
|------------|-----|
| Prometheus | Métricas |
| Grafana | Dashboards |
| Loki | Logs |
| Jaeger | Tracing |

### Health Checks

```bash
GET /api/health
{
  "status": "healthy",
  "database": "connected",
  "redis": "connected",
  "integrations": {
    "jira": "healthy",
    "azure_devops": "healthy"
  }
}
```

---

## 📊 Métricas e Analytics

### Métricas de Sprint

- Story Points entregues
- Velocity média
- Lead Time
- Cycle Time
- Throughput

### Métricas de Qualidade

- Bugs por sprint
- Cobertura de testes
- Technical Debt
- Code Churn

### Métricas de Time

- Capacidade planejada vs real
- Disponibilidade
- Férias e ausências

---

## 🔜 Em Desenvolvimento

| Feature | Issue | Previsão |
|---------|-------|----------|
| App Mobile React Native | #262 | v7.1 |
| Planning Poker | #244 | v7.1 |
| Dependency Graph | #243 | v7.1 |
| Estimativas com ML | #245 | v7.2 |
| Colaboração Tempo Real | #242 | v7.2 |

---

*Última atualização: 2025-12-30*

# 🏗️ Visão Técnica Completa - Plataforma E v7.0

> Documentação técnica baseada em análise real do código fonte (30/12/2025)

---

## 📊 Estatísticas do Projeto

| Métrica | Valor |
|---------|-------|
| **Total de Arquivos Python** | 200+ |
| **Módulos de Segurança** | 24 |
| **Módulos de Integrações** | 100+ |
| **Módulos de Dashboard** | 74 |
| **Endpoints API** | 50+ |
| **Modelos de Dados** | 50+ |

---

## 🔐 Módulo de Segurança

### Arquitetura de Autenticação (`factory/auth/`)

```
factory/auth/
├── unified_auth.py         # Sistema unificado (RBAC + Personas + ABAC)
├── rbac.py                 # Role-Based Access Control
├── abac.py                 # Attribute-Based Access Control
├── personas.py             # Perfis de usuário
├── personas_extended.py    # Extensões de perfis
├── oauth2.py               # OAuth2 com JWT
├── sso.py                  # Single Sign-On
├── sso_ui.py               # UI de SSO
├── saml_validator.py       # Validação SAML Enterprise
├── mfa.py                  # Multi-Factor Authentication (Issue #340)
├── password_policy.py      # Políticas de senha (Issue #342)
├── token_blacklist.py      # Revogação de tokens (Issue #358)
├── permission_audit.py     # Auditoria de permissões
└── api_keys/               # Gestão de API Keys (Issue #341)
    ├── models.py
    ├── service.py
    └── routes.py
```

### Implementação MFA (`factory/auth/mfa.py`)

```python
# Configurações
MFA_ISSUER = "Plataforma E"
MFA_SECRET_LENGTH = 32      # 160 bits entropy
BACKUP_CODE_COUNT = 10
TOTP_VALID_WINDOW = 1       # ±30 segundos
MAX_MFA_ATTEMPTS = 5
MFA_LOCKOUT_MINUTES = 15

# Funcionalidades
- Setup TOTP com QR Code
- Validação de código
- Backup codes (10 códigos one-time)
- Rate limiting de tentativas
- Lockout após 5 falhas
```

### Proteção de Dados (`factory/security/`)

```
factory/security/
├── encryption.py           # Encryption at Rest (Issue #344)
├── encryption_routes.py    # Endpoints de criptografia
├── ip_policy.py            # IP Whitelisting (Issue #343)
├── ip_policy_routes.py     # Endpoints de IP policy
├── tenant_isolation.py     # Isolamento multi-tenant
└── event_notifications.py  # Alertas de segurança (Issue #359)
```

### Encryption at Rest (`factory/security/encryption.py`)

```python
# Algoritmo: AES-256-GCM
ENCRYPTION_ALGORITHM = "AES-256-GCM"
KEY_SIZE = 32               # 256 bits
NONCE_SIZE = 12             # 96 bits
KEY_ROTATION_DAYS = 90      # Rotação automática

# Campos criptografados
- API keys e secrets
- Tokens de integração
- Dados pessoais (opcional por tenant)
- Mensagens de chat
```

### IP Policy (`factory/security/ip_policy.py`)

```python
# Modos suportados
- "whitelist": Apenas IPs permitidos
- "blacklist": IPs bloqueados

# Funcionalidades
- CIDR ranges (ex: 192.168.1.0/24)
- Geo-blocking por país (MaxMind GeoLite2)
- Bypass para super_admin
- Log de tentativas bloqueadas
- Notificação por email
```

---

## 🔗 Módulo de Integrações

### Estrutura Geral (`factory/integrations/`)

```
factory/integrations/
├── base.py                 # IntegrationBase + OAuthTokenManager
├── config.py               # Configurações globais
├── gateway.py              # API Gateway
├── routes.py               # Endpoints REST
├── sync_service.py         # Sincronização bidirecional
│
├── jira/                   # Jira (Issue #310, #311)
│   ├── config.py
│   └── skills/
│       ├── jira_read_skill.py
│       ├── jira_agile_skill.py    # Sprints, Boards, Epics
│       └── jira_sync_skill.py     # Issue #335
│
├── azure_devops/           # Azure DevOps (Issue #312, #313)
│   └── skills/
│       ├── azure_devops_read_skill.py
│       ├── azure_devops_pipeline_skill.py
│       └── azure_devops_sync_skill.py
│
├── salesforce/             # Salesforce
│   ├── analyzers/          # 4 analyzers
│   ├── generators/         # 4 generators
│   ├── deployers/          # Deployment
│   └── skills/             # 3 skills
│
├── sap_s4/                 # SAP S/4HANA (Issue #19)
│   ├── apis/               # Sales Order, Material, Invoice, BP
│   ├── fiori/
│   ├── rap/
│   └── cds/
│
├── sap_ecc/                # SAP ECC (Issue #315)
│   ├── rfc_client.py
│   ├── odata_client.py
│   ├── abap/
│   ├── idocs/
│   └── analyzers/          # Table, BADI, Config
│
├── sap_cpi/                # SAP CPI
│   ├── iflow_manager.py
│   ├── groovy/
│   └── mapping/
│
├── teams/                  # Microsoft Teams (Issue #22)
│   ├── bot_handler.py
│   ├── notifications.py
│   ├── graph_client.py
│   └── skills/
│
├── email/                  # Email (Issue #23)
│   ├── smtp_client.py
│   ├── graph_mail.py
│   └── templates/
│
├── sharepoint/             # SharePoint (Issue #298)
│   ├── document_client.py
│   ├── list_client.py
│   └── skills/
│
├── calendar/               # Calendar (Issue #264)
│   ├── google_calendar.py
│   ├── outlook_calendar.py
│   └── sync.py
│
├── git/                    # Git providers
│   ├── github_integration.py
│   └── gitlab_integration.py
│
├── bi/                     # Business Intelligence
│   ├── powerbi_connector.py
│   ├── tableau_connector.py
│   └── excel_connector.py
│
├── deploy/                 # Deploy Manager (Issue #332)
│   ├── deploy_manager.py
│   ├── approval_workflow.py
│   ├── rollback_handler.py
│   └── storage/            # S3, Azure, Local
│
├── monitoring/             # Monitoring (Issue #333)
│   ├── health_checker.py
│   ├── metrics_collector.py
│   ├── alerts.py
│   └── circuit_breaker.py
│
├── secrets/                # Secrets Manager (Issue #299)
│   ├── azure_keyvault.py
│   ├── local_encryption.py
│   └── secrets_manager.py
│
└── webhooks/               # Webhook handlers (Issue #303)
    ├── github_webhook.py
    ├── jira_webhook.py
    ├── azure_webhook.py
    └── signature_validator.py
```

### Jira Agile API (`factory/integrations/jira_agile.py`)

```python
class JiraAgileIntegration(JiraIntegration):
    """
    Endpoints Jira Agile REST API:
    - /rest/agile/1.0/board
    - /rest/agile/1.0/sprint
    - /rest/agile/1.0/epic
    - /rest/agile/1.0/backlog
    """

    # Métodos implementados
    async def get_boards(project_key: str) -> List[Board]
    async def get_board(board_id: int) -> Board
    async def get_sprints(board_id: int, state: str) -> List[Sprint]
    async def get_sprint(sprint_id: int) -> Sprint
    async def get_epics(board_id: int) -> List[Epic]
    async def get_backlog(board_id: int) -> List[Issue]
    async def move_to_sprint(issue_keys: List[str], sprint_id: int)
    async def get_velocity(board_id: int) -> VelocityReport
```

### Health Check (`factory/integrations/monitoring/health_checker.py`)

```python
# Endpoints
GET /api/v1/integrations/health          # Status de todas
GET /api/v1/integrations/{name}/health   # Status específica
GET /api/v1/integrations/metrics         # Métricas agregadas

# Métricas coletadas
- Latência média
- Taxa de erro
- Requests/minuto
- Status do circuit breaker
```

---

## 🎛️ Módulo Dashboard

### Estrutura (`factory/dashboard/`)

Total: **74 arquivos Python**

#### Dashboards Principais

| Arquivo | Versão | Descrição |
|---------|--------|-----------|
| `app_v6_agile.py` | **v6.0** | Dashboard Agile PRINCIPAL |
| `app_v5_kanban.py` | v5.0 | Kanban simples |
| `app_v4.py` | v4.0 | Dashboard Workers |
| `app.py` | - | Dashboard base |

#### Features por Categoria

**Gestão de Projetos:**
```
story_templates.py       # Templates de stories (Issue #44)
sprint_capacity.py       # Capacidade de sprint (Issue #279)
agile_metrics.py         # Burndown, velocity (Issue #42)
bulk_actions.py          # Ações em lote (Issue #43)
custom_fields.py         # Campos personalizados
custom_kanban_columns.py # Colunas customizáveis
```

**Inteligência Artificial:**
```
ai_acceptance_criteria.py  # Gerar critérios com IA
ai_story_splitting.py      # Quebrar stories
ai_duplicate_detection.py  # Detectar duplicatas
ai_risk_prediction.py      # Predizer riscos
ai_chat_advanced.py        # Chat avançado (Issue #280)
```

**Experiência do Usuário:**
```
dark_mode.py             # Dark mode (Issue #217)
skeleton_loaders.py      # Skeleton loaders (Issue #218)
accessibility.py         # WCAG/A11y (Issue #270)
keyboard_shortcuts.py    # Atalhos de teclado (Issue #226)
lazy_loading.py          # Lazy loading (Issue #269)
offline_sync.py          # Modo offline (Issue #260)
tour.py                  # Onboarding tour (Issue #232)
```

**Administração:**
```
admin_portal.py          # Portal admin
tenant_admin_portal.py   # Admin por tenant (Issue #288)
platform_portal.py       # Super admin (Issue #287)
rbac_integration.py      # Integração RBAC
audit_dashboard.py       # Dashboard auditoria (Issue #274)
security_settings.py     # Configurações segurança
```

---

## 📊 Módulo de Auditoria

### Estrutura (`factory/audit/`)

```
factory/audit/
├── models.py           # Modelo AuditLog
├── service.py          # AuditService
├── decorators.py       # @audit_log decorator
└── routes.py           # Endpoints
```

### Modelo de Dados

```python
class AuditLog(Base):
    id: int
    timestamp: datetime
    tenant_id: str
    user_id: str
    action: str           # CREATE, READ, UPDATE, DELETE, LOGIN, LOGOUT
    resource_type: str    # story, project, user, etc
    resource_id: str
    old_value: JSON       # Estado anterior
    new_value: JSON       # Estado novo
    ip_address: str
    user_agent: str
    request_id: str
    status: str           # success, failure
    metadata: JSON
```

### Uso

```python
from factory.audit.decorators import audit_log

@audit_log(action="UPDATE", resource="story")
async def update_story(story_id: str, data: dict):
    ...
```

---

## 🤖 Módulo de Agentes IA

### Estrutura (`factory/agents/`)

```
factory/agents/
├── agent_factory.py        # Factory de agents
├── core/
│   ├── autonomous_agent.py # Agents autônomos
│   ├── agent_runtime.py    # Runtime
│   └── task_executor.py    # Executor de tarefas
├── memory/                 # Sistemas de memória
├── knowledge/              # Knowledge base + embeddings
├── learning/               # Learning engine + feedback
└── skills/                 # Skills multimídia
    ├── text/
    ├── image/
    ├── video/
    └── audio/
```

---

## 🔌 API REST

### Endpoints Principais

```
# Autenticação
POST /api/v1/auth/login
POST /api/v1/auth/logout
POST /api/v1/auth/mfa/setup
POST /api/v1/auth/mfa/verify

# Stories
GET    /api/stories
POST   /api/stories
GET    /api/stories/{id}
PUT    /api/stories/{id}
DELETE /api/stories/{id}
PATCH  /api/stories/{id}/move

# Tasks
GET    /api/stories/{id}/tasks
POST   /api/stories/{id}/tasks
PUT    /api/story-tasks/{id}
PATCH  /api/story-tasks/{id}/complete

# Integrações
GET  /api/v1/integrations/health
GET  /api/v1/integrations/{name}/health
GET  /api/v1/integrations/metrics

# Segurança
GET  /api/tenant/{id}/ip-policy
PUT  /api/tenant/{id}/ip-policy
POST /api/security/keys/rotate
GET  /api/audit-logs

# Chat IA
POST /api/chat/message
GET  /api/chat/history
```

---

## 🗄️ Banco de Dados

### Modelos Principais (`factory/database/models.py`)

```python
# Core
Project, Story, StoryTask, StoryDocumentation

# Segurança
User, UserMFA, Tenant, TenantIPPolicy
EncryptionKey, AuditLog, TokenBlacklist

# Integrações
Integration, IntegrationConfig, Webhook

# Features
Sprint, Epic, Comment, Attachment
```

---

## 📁 Estrutura de Arquivos Completa

```
Plataforma E/
├── factory/
│   ├── auth/               # 17 arquivos - Autenticação
│   ├── security/           # 7 arquivos - Proteção de dados
│   ├── audit/              # 5 arquivos - Auditoria
│   ├── integrations/       # 100+ arquivos - Integrações
│   ├── dashboard/          # 74 arquivos - UI/UX
│   ├── api/                # 50+ arquivos - REST API
│   ├── database/           # ORM e persistência
│   ├── core/               # Lógica de negócio
│   ├── agents/             # 38 arquivos - IA Agents
│   ├── orchestrator/       # 10 arquivos - Orquestração
│   ├── middleware/         # Middlewares FastAPI
│   ├── billing/            # Sistema de billing
│   ├── notifications/      # Notificações
│   ├── websocket/          # Real-time
│   └── config/             # Configurações
├── projects/               # Projetos gerados
├── tests/                  # Testes automatizados
├── docs/                   # Documentação
└── docker-compose.yml      # Infraestrutura
```

---

## 🚀 Issues Implementados (30/12/2025)

### Segurança (Terminal B)
- #339 - Audit Log detalhado
- #340 - Two-Factor Authentication (2FA/MFA)
- #341 - API Key Management
- #342 - Password Policies
- #343 - IP Whitelisting e Geo-blocking
- #344 - Encryption at Rest
- #345 - Security Headers e CSP
- #357 - Input Validation Middleware
- #358 - JWT Blacklist e Token Revocation
- #359 - Security Event Notifications

### Integrações (Terminal A)
- #310 - Jira Skills
- #311 - Jira Agile API
- #312 - Azure DevOps Skills
- #313 - Azure DevOps API
- #315 - SAP ECC Analyzers
- #326 - Testes unitários para integrações
- #332 - Deploy Manager real
- #333 - Health Check e Monitoring
- #335 - Jira Sync Skill

### UI/UX (Terminal C)
- #217 - Dark Mode persistente
- #218 - Skeleton Loaders
- #219 - Empty States
- #232 - Onboarding Tour
- #234 - Animações

---

*Documentação gerada automaticamente em 30/12/2025*

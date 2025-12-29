# Changelog - Fábrica de Agentes

Todas as mudanças notáveis neste projeto serão documentadas neste arquivo.

O formato segue [Keep a Changelog](https://keepachangelog.com/pt-BR/1.0.0/),
e este projeto adere ao [Versionamento Semântico](https://semver.org/lang/pt-BR/).

---

## [7.0.0] - 2025-12-29

### Enterprise Edition - Integrações Corporativas e Multi-Cloud

Esta versão representa uma **evolução major** da Fábrica de Agentes, adicionando suporte completo a integrações corporativas, multi-tenancy, e infraestrutura cloud.

#### Novas Funcionalidades Enterprise

##### Multi-LLM Support (`factory/ai/llm_manager.py`) - #11
- **Interface Unificada** - Trabalhe com múltiplos provedores de LLM
- **Provedores Suportados**:
  - Claude (Anthropic) - claude-opus-4-5, claude-sonnet-4, haiku
  - Azure OpenAI - gpt-4, gpt-4-turbo, gpt-3.5
  - AWS Bedrock - claude, titan, llama
  - Google Vertex AI - gemini-pro, palm-2
- **Model Selector** - Seleção inteligente de modelo baseado em task

##### SSO e RBAC (`factory/auth/`) - #12, #13
- **SSO via SAML 2.0** - Integração com Azure AD, Okta, OneLogin
- **RBAC Completo** - Roles: Admin, Manager, Developer, Viewer
- **Permissões Granulares** - Controle de acesso por projeto/story

##### Multi-Tenancy e Billing (`factory/billing/`) - #15
- **Isolamento de Tenants** - Dados separados por empresa
- **Planos de Assinatura** - Free, Pro, Enterprise
- **Métricas de Uso** - Tracking de API calls, stories, storage
- **Limites por Plano** - Rate limiting inteligente

##### Integrações SAP (`factory/integrations/sap_*/`) - #19, #20, #21
- **SAP S/4HANA** - OData v4, Business Graph, analyzers CDS/RAP/Fiori
- **SAP ECC** - RFC Client, BAPI calls, analyzers ABAP/BAdI
- **SAP CPI** - iFlow Manager, Package deployment

##### Salesforce CRM (`factory/integrations/salesforce/`) - #18
- **REST API Client** - CRUD de objetos
- **Bulk API Client** - Operações em massa
- **Tooling API** - Apex, Flows, Metadata
- **Analyzers** - Object, Flow, Apex analysis

##### Microsoft Integration (`factory/integrations/teams/`, `email/`) - #22, #23
- **Microsoft Teams** - Graph API, Webhooks, Bot Framework, Adaptive Cards
- **Exchange/Outlook** - Graph Mail, SMTP fallback, Email templates

##### DevOps Integration (`factory/integrations/`) - #14, #54
- **Jira** - Sincronização bidirecional, webhooks
- **Azure DevOps** - Work Items, Pipelines, Repos

##### Notifications Multi-Canal (`factory/notifications/`) - #24
- **Canais Suportados** - Email, Teams, Slack, Webhook
- **Rules Engine** - Regras de notificação por evento
- **Templates** - Templates HTML para cada canal

##### WebSocket Real-time (`factory/websocket/`) - #25
- **Notificações em Tempo Real** - Updates instantâneos
- **Event System** - story_updated, task_completed, agent_status
- **Room Management** - Canais por projeto/story

##### MCP Tools (`factory/mcp/`) - #27
- **MCP Server** - Server para Claude Code
- **MCP Client** - Client para integração
- **Tools Built-in** - create_project, create_story, execute_story, run_tests

##### SDK Público (`factory/sdk/`) - #61
- **Python SDK** - Client assíncrono completo
- **Models** - Tipos Pydantic para responses
- **Exceptions** - Error handling customizado

##### Public API (`factory/api/`) - #61, #62
- **API v1** - REST API completa
- **API Key Auth** - Autenticação por chave
- **Rate Limiting v2** - Limites por tier

#### Infraestrutura Cloud

##### Terraform IaC (`terraform/`) - #17
- **Multi-Cloud** - AWS, Azure, GCP modules
- **Environments** - dev, staging, prod configurations
- **Modular** - Componentes reutilizáveis

##### Kubernetes (`k8s/`) - #30
- **Manifests Completos** - Namespace, ConfigMaps, Secrets, Ingress
- **API Deployment** - Deployment, Service, HPA
- **Workers Deployment** - Deployment com autoscaling
- **Storage** - PVC para persistência

##### Helm Charts (`helm/`)
- **Chart Fabrica-Agentes** - Chart completo para deploy
- **Values Customizáveis** - Replicas, resources, autoscaling

##### CI/CD (`/.github/workflows/`) - #9
- **GitHub Actions** - CI/CD automatizado
- **Release Workflow** - Releases automáticos

##### Logging Stack (`config/`, `docker-compose.logging.yml`) - #28
- **Loki + Promtail + Grafana** - Stack de logs moderna
- **Dashboards** - Dashboards Grafana pré-configurados

#### Core Features

##### Test Generator (`factory/core/test_generator.py`) - #53
- **Auto Test Generation** - Gera testes automaticamente
- **Unit Tests** - pytest tests
- **Integration Tests** - API tests

##### Chatbot Builder (`factory/core/chatbot_builder.py`) - #67
- **Chatbot Framework** - Framework para criar chatbots
- **Templates** - Templates de conversação
- **Integrations** - Integração com canais

##### Marketplace (`factory/core/marketplace.py`) - #56
- **Template Marketplace** - Templates de projetos
- **Skills Marketplace** - Skills reutilizáveis
- **Categories** - Organização por categoria

#### Estatísticas

- **177 arquivos** criados/modificados
- **80.001 linhas** de código adicionadas
- **32 issues** fechadas nesta release

---

## [6.5.0] - 2025-12-29

### App Generator - Teste de Aplicações com 1 Clique

Esta versão traz o **App Generator**, permitindo que usuários não-técnicos testem suas aplicações com apenas um clique.

#### Novas Funcionalidades

##### App Generator (`factory/core/app_generator.py`)
- **Auto-detecção de Projeto** - Detecta automaticamente o tipo de projeto (Python/Node.js)
- **Análise de Código** - Encontra modelos SQLAlchemy, Pydantic schemas e rotas
- **Geração Automática de App** - Cria aplicação FastAPI testável a partir dos modelos
- **Arquivos Gerados**:
  - `main.py` - Aplicação FastAPI completa com CRUD para todos os modelos
  - `requirements.txt` - Dependências do projeto
  - `iniciar_app.bat` - Script para iniciar a aplicação no Windows
- **Página Inicial** - Home page mostrando status e modelos disponíveis
- **Documentação API** - Swagger UI automático em `/docs`

##### Botão Flutuante de Teste (FAB)
- **Sempre Visível** - Botão fixo no canto inferior direito da tela
- **Badge de Status** - Mostra estado atual do projeto:
  - 🟡 Amarelo: "Desenvolvendo..." - projeto em fase inicial
  - 🔵 Azul: "Pode testar" - código pronto para gerar app
  - 🟢 Verde: "Pronto!" - aplicação disponível para teste
- **Ação com 1 Clique**:
  - ⚙️ Engrenagem azul: Gerar e iniciar aplicação
  - ▶️ Play verde: Abrir aplicação para teste
- **Feedback Visual** - Loading spinner durante processamento

##### API Endpoints para App Testing
- `GET /api/projects/{id}/app-status` - Analisa projeto e retorna status
- `POST /api/projects/{id}/generate-app` - Gera aplicação testável
- `POST /api/projects/{id}/start-app` - Inicia servidor de teste

##### Interface User-Friendly para Status
- **Barra de Progresso** - Progresso visual do desenvolvimento
- **Timeline de Etapas** - 5 fases: Planejamento → Desenvolvimento → Revisão → Testes → Entrega
- **Contadores de Stories** - Backlog, Em Desenvolvimento, Em Teste, Concluídas
- **Próximos Passos** - Orientações contextuais para o usuário
- **Mensagens Claras** - Status em linguagem não-técnica

#### Melhorias Técnicas
- Nova classe `AppGenerator` com métodos:
  - `analyze_project()` - Analisa estrutura do projeto
  - `generate_testable_app()` - Gera aplicação FastAPI
  - `start_app()` - Inicia servidor uvicorn
- Funções JavaScript para UI:
  - `checkAppStatus()` - Verifica status do projeto
  - `generateAndStartApp()` - Gera e inicia app
  - `startAndOpenApp()` - Inicia e abre no navegador
- WebSocket notifications para eventos de app

#### Testes E2E
- Novo arquivo `tests/test_e2e_dashboard.py`
- Cobertura de 80%+ dos endpoints
- Testes de WebSocket, Mobile CSS, e novas features

---

## [6.4.0] - 2025-12-29

### Dashboard Agile - Features de Desenvolvimento

Esta versão traz **3 novas funcionalidades** focadas em design, documentação e desenvolvimento.

#### Novas Funcionalidades

##### Design Hub com Draw.io (#72)
- **Editor Draw.io Integrado** - Crie e edite diagramas diretamente no dashboard
- **Tipos de Design Suportados**:
  - Wireframe - Esboços de interface
  - Architecture - Diagramas de arquitetura
  - Flow - Fluxogramas e processos
  - Database - Modelos de dados (ERD)
  - UI Mockup - Mockups de interface
  - Sequence - Diagramas de sequência
- **Galeria de Designs** - Visualize todos os designs do projeto
- **Vinculação com Stories** - Associe designs a User Stories específicas
- **Export** - Exporte como PNG, SVG ou PDF
- **Thumbnails** - Preview automático dos diagramas

##### Auto Documentation com IA (#73)
- **Geração Automática** - Claude AI gera documentação baseada na story
- **Tipos de Documentação**:
  - Technical - Documentação técnica de implementação
  - User - Manual do usuário
  - Test - Plano de testes e casos de teste
  - API - Documentação de endpoints
  - Deployment - Guia de deploy
- **Botão "Generate with AI"** - Um clique para gerar docs
- **Edição Markdown** - Edite a documentação gerada
- **Vínculo com Story** - Documentação associada a story específica

##### Terminal Interativo (#74)
- **Terminal Web** - Execute comandos diretamente no dashboard
- **Por Projeto** - Terminal isolado por projeto
- **Comandos Comuns**:
  - npm install, npm run dev
  - python, pip
  - git commands
  - Testes (pytest, npm test)
- **Output em Tempo Real** - Veja a saída dos comandos
- **Stop Process** - Interrompa processos em execução
- **Histórico** - Histórico de comandos executados

#### Melhorias Técnicas
- Novo modelo `StoryDesign` para armazenar diagramas Draw.io (XML)
- Endpoints REST para CRUD de designs
- Integração com Claude API para geração de docs
- Sistema de processos para terminal interativo

#### Issues Fechadas
- #72 - Design Hub - Mockups, Wireframes e Arquitetura (Draw.io)
- #73 - Project Preview Dashboard - Visão Unificada (parcial: Auto Docs)
- #74 - Ambiente de Teste Integrado - Terminal

---

## [6.3.0] - 2025-12-29

### Dashboard Agile - Novos Recursos

Esta versão traz **4 novas funcionalidades** focadas em produtividade e personalização.

#### Novas Funcionalidades

##### Templates de Stories (#44)
- **Dropdown de Templates** - Selecione um template ao criar nova story:
  - **Feature**: Para novas funcionalidades
  - **Bug Fix**: Para correção de bugs
  - **Tech Debt**: Para refatoração e débito técnico
  - **Spike**: Para pesquisas e investigações
  - **Melhoria**: Para aprimoramentos
- Preenche automaticamente: título, persona, ação, benefício, descrição e critérios de aceite

##### Dark Mode (#37)
- **Toggle no Header** - Botão para alternar entre modo claro/escuro
- **Persistência** - Preferência salva no localStorage
- **Suporte Completo** - Todas as telas e modais com tema escuro
- Ícones: 🌙 (modo escuro) / ☀️ (modo claro)

##### Sprint Burndown Chart (#42)
- **Mini Gráfico na Sidebar** - Visualização rápida do progresso
- **Modal Expandido** - Clique em "Expandir" para ver detalhes
- **Métricas**:
  - Total Points
  - Pontos Completos
  - Pontos Restantes
  - Velocity
- **Linhas do Gráfico**:
  - Azul: Linha ideal
  - Verde: Progresso real

##### Bulk Actions (#43)
- **Modo de Seleção** - Clique em "Selecionar" para ativar
- **Checkbox nas Stories** - Selecione múltiplas stories
- **Ações em Lote**:
  - Mover para qualquer coluna
  - Excluir múltiplas stories
- **Contador** - Mostra quantas stories selecionadas

#### Melhorias Técnicas
- CSS variables para tema escuro
- Canvas API para mini gráfico de burndown
- Gerenciamento de estado para seleção múltipla

#### Issues Fechadas
- #37 - Dark Mode
- #42 - Sprint Burndown Chart
- #43 - Bulk Actions (Seleção Múltipla)
- #44 - Templates de Stories

---

## [6.2.0] - 2025-12-28

### Dashboard Agile - Melhorias de UX

#### Novas Funcionalidades

##### Notificações em Tempo Real (WebSocket)
- **Conexão WebSocket** - Notificações push em tempo real
- **Indicador de Status** - Mostra Online/Offline no header
- **Sons de Notificação** - Alerta sonoro para novas atualizações
- **Auto-reconexão** - Reconecta automaticamente se desconectar

##### Mobile Responsive
- **Menu Hamburguer** - Navegação mobile-friendly
- **Bottom Navigation** - Barra de navegação inferior
- **Cards Adaptáveis** - Layout ajustado para telas pequenas
- **Touch Gestures** - Suporte a gestos touch

##### Geração de Testes com IA
- **Botão "Gerar Testes"** - Em cada task de desenvolvimento
- **Tipos de Teste**:
  - Testes unitários
  - Testes de integração
  - Testes E2E
- **Modal de Preview** - Visualize antes de aplicar
- **Copiar/Download** - Exporte os testes gerados

---

## [6.1.0] - 2025-12-27

### Dashboard Agile - Sistema de Stories

#### Novas Funcionalidades

##### User Stories Completas
- **Narrativa Agile** - "Como [persona], eu quero [ação] para [benefício]"
- **Critérios de Aceite** - Lista de critérios para validação
- **Definition of Done** - Checklist de conclusão
- **Story Points** - Estimativa em Fibonacci (1,2,3,5,8,13,21)
- **Complexidade** - Low, Medium, High, Very High

##### Kanban Board
- **6 Colunas**: Backlog → Ready → In Progress → Review → Testing → Done
- **Drag & Drop** - Mova stories entre colunas
- **Progress Bar** - Progresso das tasks na story
- **Filtros** - Por épico, sprint, prioridade, assignee

##### Tasks como Subtarefas
- **Tipos**: Development, Review, Test, Documentation, Design
- **Status Individual** - Pending, In Progress, Completed, Blocked
- **Progresso** - 0-100% por task
- **Output de Código** - Armazena código gerado

##### Documentação Integrada
- **Tipos de Doc**: Technical, User, Test, API, Deployment
- **Markdown Editor** - Edição rica de conteúdo
- **Instruções de Teste** - Como testar cada feature
- **Casos de Teste** - Lista de cenários

---

## [6.0.0] - 2025-12-26

### Fábrica de Agentes v6.0 - Dashboard Agile

Lançamento do novo Dashboard Agile com suporte completo a User Stories, metodologia ágil e integração com Claude AI.

#### Principais Features

- **Dashboard Agile v6** - Sistema completo de gestão ágil
- **User Stories** - Modelo completo com narrativa, critérios e DoD
- **Kanban Board** - Quadro visual com 6 colunas
- **Tasks** - Subtarefas com tipos e progresso
- **Documentação** - Sistema de docs integrado
- **Chat IA** - Assistente Claude integrado
- **Épicos e Sprints** - Organização hierárquica

#### Stack Técnica

- **Backend**: FastAPI + SQLAlchemy + SQLite
- **Frontend**: Vue.js 3 + Tailwind CSS (inline)
- **AI**: Claude API (Anthropic)
- **Real-time**: WebSocket

---

## Versões Anteriores

Para versões anteriores (v1.0 - v5.0), consulte o histórico de commits no repositório.

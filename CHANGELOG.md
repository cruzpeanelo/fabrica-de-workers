# Changelog - Fábrica de Agentes

Todas as mudanças notáveis neste projeto serão documentadas neste arquivo.

O formato segue [Keep a Changelog](https://keepachangelog.com/pt-BR/1.0.0/),
e este projeto adere ao [Versionamento Semântico](https://semver.org/lang/pt-BR/).

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

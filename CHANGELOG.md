# Changelog

Todas as mudancas notaveis neste projeto serao documentadas neste arquivo.

O formato segue [Keep a Changelog](https://keepachangelog.com/pt-BR/1.0.0/),
e este projeto adere ao [Versionamento Semantico](https://semver.org/lang/pt-BR/).

---

## [6.4.0] - 2025-12-29

### Dashboard Agile - Features de Desenvolvimento

Esta versao traz **3 novas funcionalidades** focadas em design, documentacao e desenvolvimento.

#### Novas Funcionalidades

##### Design Hub com Draw.io (#72)
- **Editor Draw.io Integrado** - Crie e edite diagramas diretamente no dashboard
- **Tipos de Design Suportados**:
  - Wireframe - Esbocos de interface
  - Architecture - Diagramas de arquitetura
  - Flow - Fluxogramas e processos
  - Database - Modelos de dados (ERD)
  - UI Mockup - Mockups de interface
  - Sequence - Diagramas de sequencia
- **Galeria de Designs** - Visualize todos os designs do projeto
- **Vinculacao com Stories** - Associe designs a User Stories especificas
- **Export** - Exporte como PNG, SVG ou PDF
- **Thumbnails** - Preview automatico dos diagramas

##### Auto Documentation com IA (#73)
- **Geracao Automatica** - Claude AI gera documentacao baseada na story
- **Tipos de Documentacao**:
  - Technical - Documentacao tecnica de implementacao
  - User - Manual do usuario
  - Test - Plano de testes e casos de teste
  - API - Documentacao de endpoints
  - Deployment - Guia de deploy
- **Botao "Generate with AI"** - Um clique para gerar docs
- **Edicao Markdown** - Edite a documentacao gerada
- **Vinculo com Story** - Documentacao associada a story especifica

##### Terminal Interativo (#74)
- **Terminal Web** - Execute comandos diretamente no dashboard
- **Por Projeto** - Terminal isolado por projeto
- **Comandos Comuns**:
  - npm install, npm run dev
  - python, pip
  - git commands
  - Testes (pytest, npm test)
- **Output em Tempo Real** - Veja a saida dos comandos
- **Stop Process** - Interrompa processos em execucao
- **Historico** - Historico de comandos executados

#### Melhorias Tecnicas
- Novo modelo `StoryDesign` para armazenar diagramas Draw.io (XML)
- Endpoints REST para CRUD de designs
- Integracao com Claude API para geracao de docs
- Sistema de processos para terminal interativo

#### Issues Fechadas
- #72 - Design Hub - Mockups, Wireframes e Arquitetura (Draw.io)
- #73 - Project Preview Dashboard - Visao Unificada (parcial: Auto Docs)
- #74 - Ambiente de Teste Integrado - Terminal

---

## [6.3.0] - 2025-12-29

### Dashboard Agile - Novos Recursos

Esta versao traz **4 novas funcionalidades** focadas em produtividade e personalizacao.

#### Novas Funcionalidades

##### Templates de Stories (#44)
- **Dropdown de Templates** - Selecione um template ao criar nova story:
  - **Feature**: Para novas funcionalidades
  - **Bug Fix**: Para correcao de bugs
  - **Tech Debt**: Para refatoracao e debito tecnico
  - **Spike**: Para pesquisas e investigacoes
  - **Melhoria**: Para aprimoramentos
- Preenche automaticamente: titulo, persona, acao, beneficio, descricao e criterios de aceite

##### Dark Mode (#37)
- **Toggle no Header** - Botao para alternar entre modo claro/escuro
- **Persistencia** - Preferencia salva no localStorage
- **Suporte Completo** - Todas as telas e modais com tema escuro
- Icones: 🌙 (modo escuro) / ☀️ (modo claro)

##### Sprint Burndown Chart (#42)
- **Mini Grafico na Sidebar** - Visualizacao rapida do progresso
- **Modal Expandido** - Clique em "Expandir" para ver detalhes
- **Metricas**:
  - Total Points
  - Pontos Completos
  - Pontos Restantes
  - Velocity
- **Linhas do Grafico**:
  - Azul (tracejada): Linha ideal
  - Laranja: Progresso real

##### Bulk Actions (#40)
- **Modo Selecao** - Botao "Selecionar" na barra de filtros
- **Selecao Multipla** - Checkbox em cada story card
- **Barra de Acoes Flutuante**:
  - Mover para Ready
  - Mover para In Progress
  - Mover para Done
  - Excluir selecionadas
- **Animacao Suave** - Toolbar aparece com slide-up

#### Issues Fechadas
- #44 - Templates de Stories
- #42 - Sprint Burndown Chart
- #37 - Suporte a Dark Mode
- #40 - Bulk actions para multiplas stories

---

## [6.2.0] - 2025-12-18

### Dashboard Agile - Melhorias de UX/UI

Esta versao traz **10 melhorias significativas** de experiencia do usuario no Dashboard Agile,
focadas em produtividade, feedback visual e facilidade de uso.

#### Novas Funcionalidades

##### Busca e Filtros
- **Campo de Busca Global** - Busque stories por titulo, ID, descricao, persona ou acao
  - Atalho de teclado: pressione `/` para focar na busca
  - Resultados em tempo real enquanto digita
- **Filtros Avancados** - Barra de filtros no Kanban com:
  - Filtro por Prioridade (Urgente, Alta, Media, Baixa)
  - Filtro por Assignee (Todos, Sem assignee)
  - Indicador de filtros ativos
  - Botao "Limpar filtros"
  - Contador de stories filtradas

##### Acoes Rapidas
- **Quick Actions no Hover** - Ao passar o mouse sobre um card:
  - Mover para proxima coluna (seta)
  - Excluir story (lixeira)
- **Menu de Contexto** - Clique direito em qualquer story para:
  - Abrir detalhes
  - Mover para qualquer coluna (Backlog, Ready, In Progress, Review, Testing, Done)
  - Copiar ID da story
  - Excluir story

##### Atalhos de Teclado
| Atalho | Acao |
|--------|------|
| `/` | Focar no campo de busca |
| `?` | Mostrar modal de atalhos |
| `N` | Nova Story (com projeto selecionado) |
| `T` | Nova Task (com story aberta) |
| `E` | Editar story selecionada |
| `Del` | Excluir story selecionada |
| `1-6` | Mover story para coluna (1=Backlog, 6=Done) |
| `Esc` | Fechar modal/painel ou limpar busca |

##### Feedback Visual
- **Sistema de Notificacoes Toast** - Feedback visual para todas as acoes:
  - Sucesso (verde): Story criada, movida, excluida
  - Erro (vermelho): Falhas de conexao ou validacao
  - Info (azul): Acoes informativas
  - Alerta (amarelo): Avisos importantes
- **Modal de Confirmacao** - Dialogo de confirmacao para acoes destrutivas:
  - Exibe nome do item a ser excluido
  - Destaque visual vermelho para alertar
  - Mensagem clara sobre irreversibilidade

##### Animacoes e Micro-interacoes
- **Animacao de Entrada** - Cards aparecem com efeito suave (fade + slide)
- **Hover em Botoes** - Efeito de elevacao (translateY) ao passar o mouse
- **Progress Bar Suave** - Transicao animada na barra de progresso
- **Drag and Drop Melhorado** - Feedback visual durante arraste:
  - Card rotacionado e elevado
  - Sombra mais pronunciada
  - Indicador de area de destino

##### Onboarding
- **Tela de Boas-Vindas** - Guia passo a passo para novos usuarios:
  1. Criar ou selecionar projeto
  2. Adicionar User Stories com narrativa Agile
  3. Arrastar stories pelo Kanban
  4. Usar chat para comandos rapidos
- **Dica de Atalhos** - Indicacao do atalho `?` para ajuda

#### CSS e Estilos
- Classes de loading: `.spinner`, `.btn-loading`, `.skeleton`
- Classes de drag: `.sortable-chosen`, `.sortable-ghost`
- Classes de quick actions: `.quick-actions`, `.quick-btn`
- Classes de context menu: `.context-menu`, `.context-menu-item`
- Classes de toast: `.toast`, `.toast-success`, `.toast-error`
- Classes de animacao: `.card-animate`, `.btn-animate`

#### Issues Fechadas
- #32 - Campo de busca para stories no Kanban
- #33 - Sistema de notificacoes toast
- #34 - Loading states e skeleton screens
- #35 - Atalhos de teclado para acoes comuns
- #38 - Drag and drop com melhor feedback visual
- #39 - Quick actions e menu de contexto
- #46 - Onboarding e empty states
- #47 - Confirmacao de acoes destrutivas
- #48 - Animacoes e micro-interacoes
- #49 - Filtros avancados no Kanban

---

## [6.1.0] - 2025-12-17

### Assistente IA Inteligente

#### Novas Funcionalidades
- **Chat Assistant com Claude** - Assistente integrado no dashboard
- **Comandos Inteligentes** - O assistente entende linguagem natural
- **Controle de Execucao** - Aprovar/rejeitar acoes sugeridas pelo assistente
- **Historico de Conversas** - Persistencia do historico por projeto

#### Melhorias
- Integracao do assistente com gestao de projetos
- Documentacao tecnica automatica

---

## [6.0.0] - 2025-12-16

### Dashboard Agile v6

#### Sistema Agile Completo
- **User Stories** - Formato "Como um... Eu quero... Para que..."
- **Acceptance Criteria** - Lista de criterios de aceite
- **Definition of Done** - Checklist de DoD
- **Story Points** - Escala Fibonacci (1, 2, 3, 5, 8, 13, 21)
- **Complexidade** - Baixa, Media, Alta, Muito Alta
- **Prioridade** - Baixa, Media, Alta, Urgente

#### Kanban Board
- 6 colunas: Backlog, Ready, In Progress, Review, Testing, Done
- Drag and drop entre colunas
- Contagem de stories e pontos por coluna
- Filtragem por Sprint e Epic

#### Tasks e Documentacao
- Subtasks por story com tipos: development, review, test, documentation, design
- Documentacao tecnica por story
- Upload de arquivos anexos

---

## [5.0.0] - 2025-12-15

### Dashboard Kanban v5

#### Funcionalidades
- Kanban simples de tarefas
- Processamento autonomo com Claude AI
- Integracao com Watcher automatico

---

## [4.0.0] - 2025-12-14

### Dashboard Workers v4

#### Funcionalidades
- Fila de jobs com Redis
- Workers Claude para processamento
- Loop autonomo: Generate -> Lint -> Test -> Fix

---

## Como Usar Este Changelog

### Para Product Owners
Foque nas secoes de "Novas Funcionalidades" para entender o que mudou e comunicar ao time.

### Para Desenvolvedores
Consulte as secoes tecnicas (CSS, Issues Fechadas) para entender implementacoes.

### Para Stakeholders
Leia a "Visao Geral" de cada versao para entender o valor entregue.

---

*Fabrica de Agentes - Sistema Agile de Desenvolvimento Autonomo*

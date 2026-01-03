# Documentação da Fábrica de Agentes

Bem-vindo à documentação completa da **Fábrica de Agentes v7.0**!

> Plataforma de Desenvolvimento Ágil com Inteligência Artificial

---

## 📋 Índice

### Destaques - Comece Aqui

| Documento | Descrição | Público |
|-----------|-----------|---------|
| [VISAO_NEGOCIOS.md](VISAO_NEGOCIOS.md) | **Visão para Negócios** - ROI, benefícios, casos de sucesso | Executivos, Clientes |
| [ARQUITETURA_DETALHADA.md](ARQUITETURA_DETALHADA.md) | **Arquitetura Detalhada** - Decisões técnicas e justificativas | Arquitetos, TI |
| [TECHNICAL_OVERVIEW.md](TECHNICAL_OVERVIEW.md) | **Visão Técnica Completa** - Arquitetura, módulos, APIs | TI, Desenvolvedores |
| [FUNCIONALIDADES_POR_PERFIL.md](FUNCIONALIDADES_POR_PERFIL.md) | **Por Perfil** - 9 personas com dashboards e permissões distintas | Todos |
| [FEATURES.md](FEATURES.md) | **Funcionalidades** - Catálogo completo de features | Todos |
| [ROADMAP.md](ROADMAP.md) | **Roadmap** - Planejamento futuro (Negócio + Técnico) | Todos |

---

### 🚀 Início Rápido

| Documento | Descrição |
|-----------|-----------|
| [Quick Start](../README.md#-quick-start) | Instalação e primeiros passos |
| [GUIA_USUARIO.md](GUIA_USUARIO.md) | Manual completo do usuário |
| [CONTRIBUTING.md](CONTRIBUTING.md) | Guia para contribuidores |

### 📊 Documentação de Negócio

| Documento | Descrição |
|-----------|-----------|
| [VISAO_NEGOCIOS.md](VISAO_NEGOCIOS.md) | ⭐ **Visão completa para negócios** - ROI, benefícios, pricing |
| [ROADMAP.md](ROADMAP.md) | Planejamento estratégico de evolução |
| [DOCUMENTACAO_NEGOCIOS.md](DOCUMENTACAO_NEGOCIOS.md) | Visão geral de negócio (legado) |
| [BUSINESS_VALUE.md](BUSINESS_VALUE.md) | Proposta de valor (legado) |

### Documentação Técnica

| Documento | Descrição |
|-----------|-----------|
| [ARQUITETURA_DETALHADA.md](ARQUITETURA_DETALHADA.md) | **Arquitetura Detalhada** - Decisões, padrões, justificativas |
| [TECHNICAL_OVERVIEW.md](TECHNICAL_OVERVIEW.md) | **Visão técnica completa** - Módulos, APIs, implementação |
| [FEATURES.md](FEATURES.md) | Catálogo detalhado de funcionalidades |
| [ARQUITETURA.md](ARQUITETURA.md) | Arquitetura básica do sistema |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Architecture overview (EN) |
| [DOCUMENTACAO_TECNICA.md](DOCUMENTACAO_TECNICA.md) | Detalhes técnicos (legado) |
| [API_REFERENCE.md](API_REFERENCE.md) | Referência completa da API REST |
| [API_DESIGN_GUIDE.md](API_DESIGN_GUIDE.md) | Guia de design da API |
| [WEBSOCKET.md](WEBSOCKET.md) | Documentação WebSocket |

### 🔗 Integrações

| Documento | Descrição |
|-----------|-----------|
| [integrations/](integrations/) | Todas as integrações disponíveis |
| [MICROSOFT_GRAPH_INTEGRATION.md](MICROSOFT_GRAPH_INTEGRATION.md) | Microsoft Graph (Calendar, SharePoint) |
| [MULTIMEDIA_SKILLS.md](MULTIMEDIA_SKILLS.md) | Skills multimídia |
| [SPECIALIZED_AGENTS.md](SPECIALIZED_AGENTS.md) | Agentes especializados |

### 🔐 Segurança

| Documento | Descrição |
|-----------|-----------|
| [SECURITY_HARDENING.md](SECURITY_HARDENING.md) | Hardening e boas práticas |

### 🏗️ DevOps e Infraestrutura

| Documento | Descrição |
|-----------|-----------|
| [CI_CD_GUIDE.md](CI_CD_GUIDE.md) | Pipeline de CI/CD |
| [CLOUD_MIGRATION_CHECKLIST.md](CLOUD_MIGRATION_CHECKLIST.md) | Checklist para migração cloud |
| [DISASTER_RECOVERY.md](DISASTER_RECOVERY.md) | Recuperação de desastres |
| [runbooks/](runbooks/) | Runbooks operacionais |

---

## 🎯 Por Onde Começar?

### Se você é **Executivo / Cliente**

1. 📊 Leia a [Visão para Negócios](VISAO_NEGOCIOS.md) - entenda o ROI e benefícios
2. 🗺️ Veja o [Roadmap](ROADMAP.md) - planejamento futuro
3. ✨ Explore as [Funcionalidades](FEATURES.md)

### Se você é **Gestor / Product Owner**

1. 📊 Entenda a [Proposta de Valor](VISAO_NEGOCIOS.md)
2. 📖 Leia o [Guia do Usuário](GUIA_USUARIO.md)
3. ✨ Explore as [Funcionalidades](FEATURES.md)
4. 🗺️ Acompanhe o [Roadmap](ROADMAP.md)

### Se você é **Desenvolvedor / TI**

1. Configure o ambiente seguindo o [Quick Start](../README.md#-quick-start)
2. Leia a [Visão Técnica](TECHNICAL_OVERVIEW.md)
3. Explore a [Arquitetura Detalhada](ARQUITETURA_DETALHADA.md) - decisões e justificativas
4. Consulte a [Referência da API](API_REFERENCE.md)
5. Siga o [Guia de Contribuição](CONTRIBUTING.md)

### Se você é **DevOps / SRE**

1. 📐 Revise a [Visão Técnica](TECHNICAL_OVERVIEW.md)
2. ⚙️ Configure [CI/CD](CI_CD_GUIDE.md)
3. 🔄 Prepare [Disaster Recovery](DISASTER_RECOVERY.md)
4. 📋 Consulte os [Runbooks](runbooks/)

---

## 🏗️ Estrutura da Documentação

```
docs/
├── README.md                        # Este índice
│
├── # Documentação Principal (v7.0)
├── VISAO_NEGOCIOS.md               # Visão para negócios - ROI, casos de sucesso
├── ARQUITETURA_DETALHADA.md        # Arquitetura - decisões e justificativas
├── TECHNICAL_OVERVIEW.md           # Visão técnica completa
├── FUNCIONALIDADES_POR_PERFIL.md   # 9 personas com dashboards distintos
├── FEATURES.md                     # Catálogo de funcionalidades
├── ROADMAP.md                      # Roadmap (negócio + técnico)
│
├── # Documentação de Referência
├── ARQUITETURA.md                   # Arquitetura técnica detalhada
├── ARCHITECTURE.md                  # Architecture (English)
├── API_REFERENCE.md                 # Referência da API
├── API_DESIGN_GUIDE.md              # Guia de design
├── CONTRIBUTING.md                  # Guia de contribuição
├── GUIA_USUARIO.md                  # Manual do usuário
├── SECURITY_HARDENING.md            # Segurança
├── WEBSOCKET.md                     # WebSocket
│
├── # Documentação Legada
├── BUSINESS_VALUE.md                # Valor de negócio (legado)
├── DOCUMENTACAO_NEGOCIOS.md         # Negócio (legado)
├── DOCUMENTACAO_TECNICA.md          # Técnica (legado)
│
├── # DevOps
├── CI_CD_GUIDE.md                   # Pipeline CI/CD
├── CLOUD_MIGRATION_CHECKLIST.md     # Migração cloud
├── DISASTER_RECOVERY.md             # DR e backup
│
├── # Subdiretórios
├── api/                             # Docs específicas da API
├── images/                          # Imagens e screenshots
├── integrations/                    # Integrações (SAP, Jira, etc)
└── runbooks/                        # Runbooks operacionais
```

---

## Versão Atual

| Item | Valor |
|------|-------|
| **Versão** | v7.0 |
| **Data** | Janeiro 2026 |
| **Módulos Python** | 200+ |
| **Integrações** | 15+ |
| **Issues Resolvidos** | 430+ |
| **Personas/Perfis** | 9 |
| **Arquitetura** | Clean Architecture + DDD |

---

## 🔗 Links Úteis

- **Repositório**: [github.com/cruzpeanelo/fabrica-de-agentes](https://github.com/cruzpeanelo/fabrica-de-agentes)
- **Issues**: [Reportar Bug / Sugerir Feature](https://github.com/cruzpeanelo/fabrica-de-agentes/issues)
- **Dashboard**: http://localhost:9001 (local)
- **Changelog**: [CHANGELOG.md](../CHANGELOG.md)

---

## 📝 Contribuindo com a Documentação

Encontrou algo errado ou quer melhorar a documentação?

1. Faça fork do repositório
2. Edite o arquivo Markdown relevante
3. Envie um Pull Request

Veja o [Guia de Contribuição](CONTRIBUTING.md) para mais detalhes.

---

<p align="center">
<strong>🏭 Fábrica de Agentes v7.0</strong><br>
Plataforma de Desenvolvimento Ágil com Inteligência Artificial
</p>

---

*Última atualização: 2026-01-03*

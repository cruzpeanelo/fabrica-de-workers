# Guia de CI/CD - Plataforma E

Este documento descreve a configuracao do pipeline de Integracao Continua (CI) e Deploy Continuo (CD) do projeto Plataforma E.

## Visao Geral

O pipeline CI/CD esta configurado usando GitHub Actions e consiste em dois workflows principais:

| Workflow | Arquivo | Trigger | Descricao |
|----------|---------|---------|-----------|
| **CI** | `.github/workflows/ci.yml` | push/PR para main/develop | Lint, testes, security scan |
| **CD** | `.github/workflows/cd.yml` | push para main, tags v*.*.* | Build e deploy |

## Pipeline CI (Integracao Continua)

### Jobs Executados

```
┌─────────┐    ┌──────────┐    ┌────────┐    ┌─────────┐    ┌────────┐
│  Lint   │    │ Security │    │  Test  │    │  Build  │    │ Docker │
│ (3.10,  │    │   Scan   │    │ (3.10, │ -> │ Verify  │ -> │ Build  │
│  3.11,  │    │          │    │  3.11, │    │         │    │        │
│  3.12)  │    │          │    │  3.12) │    │         │    │        │
└─────────┘    └──────────┘    └────────┘    └─────────┘    └────────┘
     │              │              │              │              │
     └──────────────┴──────────────┴──────────────┴──────────────┘
                                   │
                              ┌────────┐
                              │ Status │
                              │ Report │
                              └────────┘
```

### 1. Lint (Verificacao de Codigo)

Executa em Python 3.10, 3.11 e 3.12:

- **Black**: Formatacao de codigo
- **isort**: Ordenacao de imports
- **Ruff**: Linter rapido
- **flake8**: Verificacao de erros sintaticos e complexidade

### 2. Security (Analise de Seguranca)

- **Bandit**: Analise estatica de seguranca do codigo Python
- **pip-audit**: Verificacao de vulnerabilidades em dependencias

### 3. Test (Testes Automatizados)

Executa em Python 3.10, 3.11 e 3.12:

- **pytest**: Testes unitarios e de integracao
- **pytest-cov**: Cobertura de codigo
- **Codecov**: Upload de relatorio de cobertura

### 4. Build (Verificacao de Build)

- Verificacao de imports do dashboard
- Teste de startup do servidor
- Verificacao de modelos de banco de dados

### 5. Docker (Build de Imagem)

Executado apenas no branch main:

- Build da imagem Docker
- Teste de health check
- Cache de layers com GitHub Actions

## Pipeline CD (Deploy Continuo)

### Ambientes

| Ambiente | Trigger | URL |
|----------|---------|-----|
| **Staging** | Push para main | staging.fabrica-agentes.example.com |
| **Production** | Tag v*.*.* ou manual | fabrica-agentes.example.com |

### Jobs Executados

```
┌─────────┐    ┌─────────┐    ┌─────────────────┐
│ Prepare │ -> │  Build  │ -> │ Deploy Staging  │
│         │    │ & Push  │    │       ou        │
│         │    │ Docker  │    │ Deploy Prod     │
└─────────┘    └─────────┘    └─────────────────┘
                                      │
                              ┌───────────────┐
                              │    Notify     │
                              └───────────────┘
```

## Secrets Necessarios

Configure os seguintes secrets no repositorio GitHub:

### Obrigatorios

| Secret | Descricao |
|--------|-----------|
| `GITHUB_TOKEN` | Automatico, para push de imagem GHCR |
| `CODECOV_TOKEN` | Token do Codecov para upload de cobertura |

### Opcionais (Deploy)

| Secret | Descricao |
|--------|-----------|
| `DOCKERHUB_USERNAME` | Usuario Docker Hub |
| `DOCKERHUB_TOKEN` | Token de acesso Docker Hub |
| `KUBE_CONFIG` | Configuracao kubectl (base64) |
| `AWS_ACCESS_KEY_ID` | Credenciais AWS |
| `AWS_SECRET_ACCESS_KEY` | Credenciais AWS |
| `SLACK_WEBHOOK` | Webhook para notificacoes Slack |
| `DISCORD_WEBHOOK` | Webhook para notificacoes Discord |

## Como Configurar Secrets

1. Va para Settings do repositorio no GitHub
2. Clique em "Secrets and variables" > "Actions"
3. Clique em "New repository secret"
4. Adicione cada secret necessario

### Exemplo: Configurar Codecov

```bash
# 1. Acesse https://codecov.io e faca login com GitHub
# 2. Selecione o repositorio
# 3. Copie o CODECOV_TOKEN
# 4. Adicione como secret no GitHub
```

### Exemplo: Configurar Kubernetes

```bash
# 1. Exporte a configuracao do kubectl
kubectl config view --raw | base64 -w 0

# 2. Adicione como secret KUBE_CONFIG no GitHub
```

## Execucao Manual

### CI

O workflow CI pode ser executado manualmente:

1. Va para Actions no GitHub
2. Selecione o workflow "CI"
3. Clique em "Run workflow"

### CD

O workflow CD pode ser executado manualmente com opcoes:

```bash
# Via GitHub CLI
gh workflow run cd.yml -f environment=staging
gh workflow run cd.yml -f environment=production -f version=1.2.3
```

## Criando uma Release

### Via Tag

```bash
# Criar e enviar tag
git tag -a v1.0.0 -m "Release v1.0.0"
git push origin v1.0.0
```

Isso dispara:
1. Workflow Release: Cria release no GitHub
2. Workflow CD: Deploy para producao

### Via GitHub

1. Va para Releases
2. Clique em "Draft a new release"
3. Escolha a tag ou crie uma nova
4. Adicione notas de release
5. Publique

## Caching

O pipeline usa cache para acelerar builds:

| Cache | Chave | Retencao |
|-------|-------|----------|
| pip | `v1-{os}-pip-{job}-{python}-{hash}` | 7 dias |
| Docker | GitHub Actions Cache | 7 dias |

Para invalidar o cache, incremente `CACHE_VERSION` no workflow.

## Troubleshooting

### Build Falhou no Lint

```bash
# Corrigir formatacao localmente
black factory/ tests/
isort factory/ tests/
```

### Build Falhou nos Testes

```bash
# Executar testes localmente
pytest tests/unit/ -v

# Ver cobertura
pytest tests/unit/ --cov=factory --cov-report=html
```

### Build Docker Falhou

```bash
# Testar build localmente
docker build -t fabrica-agentes:test .
docker run -p 9000:9000 fabrica-agentes:test
```

### Security Scan Encontrou Vulnerabilidades

```bash
# Verificar dependencias
pip-audit

# Atualizar dependencias
pip install --upgrade <pacote>
```

## Badges

Adicione ao README:

```markdown
[![CI](https://github.com/SEU_USUARIO/plataforma-e/actions/workflows/ci.yml/badge.svg)](https://github.com/SEU_USUARIO/plataforma-e/actions/workflows/ci.yml)
[![CD](https://github.com/SEU_USUARIO/plataforma-e/actions/workflows/cd.yml/badge.svg)](https://github.com/SEU_USUARIO/plataforma-e/actions/workflows/cd.yml)
[![codecov](https://codecov.io/gh/SEU_USUARIO/plataforma-e/branch/main/graph/badge.svg)](https://codecov.io/gh/SEU_USUARIO/plataforma-e)
```

## Proximos Passos

1. [ ] Configurar secrets no repositorio
2. [ ] Fazer push para trigger do CI
3. [ ] Verificar execucao do pipeline
4. [ ] Configurar ambiente de staging
5. [ ] Configurar ambiente de producao
6. [ ] Adicionar notificacoes (Slack/Discord)

---

*Documentacao gerada para Issue #9 - Configurar CI/CD com GitHub Actions*

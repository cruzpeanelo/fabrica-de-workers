# 🤝 Guia de Contribuição

Obrigado pelo interesse em contribuir com a **Plataforma E**! Este guia vai te ajudar a configurar o ambiente, entender nossa estrutura e enviar suas contribuições.

---

## 📋 Índice

- [Código de Conduta](#código-de-conduta)
- [Pré-requisitos](#pré-requisitos)
- [Setup do Ambiente](#setup-do-ambiente)
- [Estrutura do Projeto](#estrutura-do-projeto)
- [Fluxo de Contribuição](#fluxo-de-contribuição)
- [Padrões de Código](#padrões-de-código)
- [Testes](#testes)
- [Documentação](#documentação)
- [Pull Requests](#pull-requests)
- [Reportando Issues](#reportando-issues)

---

## Código de Conduta

Este projeto segue um código de conduta aberto e inclusivo. Esperamos que todos os contribuidores:

- Sejam respeitosos e profissionais
- Aceitem feedback construtivo
- Foquem no que é melhor para a comunidade
- Mostrem empatia com outros contribuidores

---

## Pré-requisitos

Antes de começar, certifique-se de ter instalado:

| Ferramenta | Versão Mínima | Download |
|------------|---------------|----------|
| Python | 3.10+ | [python.org](https://python.org) |
| Git | 2.30+ | [git-scm.com](https://git-scm.com) |
| Docker | 20.10+ | [docker.com](https://docker.com) (opcional) |
| Node.js | 18+ | [nodejs.org](https://nodejs.org) (para frontend) |

### Chaves de API (Opcionais para Desenvolvimento)

- **Anthropic API Key**: Para testes com Claude AI
- **Jira API Token**: Para testes de integração Jira
- **Azure DevOps PAT**: Para testes de integração Azure

---

## Setup do Ambiente

### 1. Fork e Clone

```bash
# Faça fork do repositório no GitHub
# Depois clone seu fork:
git clone https://github.com/SEU-USUARIO/plataforma-e.git
cd plataforma-e

# Adicione o repositório original como upstream
git remote add upstream https://github.com/cruzpeanelo/plataforma-e.git

# Verifique os remotes
git remote -v
# origin    https://github.com/SEU-USUARIO/plataforma-e.git (fetch)
# origin    https://github.com/SEU-USUARIO/plataforma-e.git (push)
# upstream  https://github.com/cruzpeanelo/plataforma-e.git (fetch)
# upstream  https://github.com/cruzpeanelo/plataforma-e.git (push)
```

### 2. Ambiente Virtual Python

```bash
# Crie o ambiente virtual
python -m venv venv

# Ative o ambiente
# Linux/macOS:
source venv/bin/activate

# Windows (PowerShell):
.\venv\Scripts\Activate.ps1

# Windows (CMD):
venv\Scripts\activate.bat

# Windows (Git Bash):
source venv/Scripts/activate
```

### 3. Instale Dependências

```bash
# Dependências de produção
pip install -r requirements.txt

# Dependências de desenvolvimento (testes, linting, etc)
pip install -r requirements-dev.txt

# Ou instale tudo de uma vez:
pip install -e ".[dev]"
```

### 4. Configure Variáveis de Ambiente

```bash
# Copie o arquivo de exemplo
cp .env.example .env

# Edite o arquivo .env com suas configurações
# Mínimo necessário para desenvolvimento:
ANTHROPIC_API_KEY=sua_chave_aqui  # Opcional para testes locais
DATABASE_URL=sqlite:///factory/database/factory.db
```

### 5. Inicialize o Banco de Dados

```bash
# Criar estrutura e dados de seed
python factory/database/seed.py
```

### 6. Verifique a Instalação

```bash
# Execute os testes
python -m pytest tests/ -v

# Inicie o dashboard
python factory/dashboard/app_v6_agile.py

# Acesse http://localhost:9001
# Login: admin / admin
```

---

## Estrutura do Projeto

```
Plataforma E/
├── factory/                    # Código principal
│   ├── api/                    # API REST (FastAPI)
│   │   ├── routes.py           # Endpoints principais
│   │   ├── auth.py             # Autenticação JWT
│   │   ├── middleware/         # Middlewares de segurança
│   │   └── v1/                 # Versão 1 da API
│   ├── core/                   # Core do sistema
│   │   ├── autonomous_loop.py  # Loop de auto-correção
│   │   ├── job_queue.py        # Fila de jobs (Redis)
│   │   ├── worker.py           # Workers Claude
│   │   └── story_generator.py  # Gerador de stories
│   ├── database/               # Banco de dados
│   │   ├── connection.py       # Conexões (PostgreSQL/SQLite)
│   │   ├── models.py           # Modelos SQLAlchemy
│   │   └── repositories.py     # Camada de acesso a dados
│   ├── dashboard/              # Dashboards web
│   │   ├── app_v6_agile.py     # Dashboard Agile principal
│   │   ├── static/             # CSS, JS, imagens
│   │   └── templates/          # Templates HTML (Jinja2)
│   ├── integrations/           # Integrações externas
│   │   ├── jira/               # Jira API
│   │   ├── azure_devops/       # Azure DevOps API
│   │   ├── sap_s4/             # SAP S/4HANA
│   │   └── ...                 # Outras integrações
│   ├── auth/                   # Autenticação avançada
│   │   └── api_keys/           # Gestão de API Keys
│   └── config.py               # Configurações centralizadas
├── tests/                      # Testes automatizados
│   ├── test_api.py             # Testes de API
│   ├── test_integrations.py    # Testes de integrações
│   └── conftest.py             # Fixtures pytest
├── docs/                       # Documentação
├── projects/                   # Projetos gerados pelo sistema
├── docker-compose.yml          # Infraestrutura Docker
├── requirements.txt            # Dependências de produção
├── requirements-dev.txt        # Dependências de desenvolvimento
└── pyproject.toml              # Configuração do projeto
```

### Áreas de Responsabilidade

| Área | Diretório | Descrição |
|------|-----------|-----------|
| **API** | `factory/api/` | Endpoints REST, autenticação |
| **Core** | `factory/core/` | Lógica de negócio, workers |
| **Database** | `factory/database/` | Modelos, repositórios |
| **Dashboard** | `factory/dashboard/` | Interface web |
| **Integrações** | `factory/integrations/` | SAP, Jira, Azure, etc |
| **Segurança** | `factory/auth/`, `factory/middleware/` | Autenticação, autorização |

---

## Fluxo de Contribuição

### 1. Sincronize com Upstream

```bash
# Busque as últimas mudanças do upstream
git fetch upstream

# Atualize sua branch main
git checkout main
git merge upstream/main

# Push para seu fork
git push origin main
```

### 2. Crie uma Branch

```bash
# Padrão: tipo/descricao-curta
git checkout -b feature/minha-nova-feature
git checkout -b fix/corrige-bug-login
git checkout -b docs/atualiza-readme
```

**Tipos de branch:**
- `feature/` - Nova funcionalidade
- `fix/` - Correção de bug
- `docs/` - Documentação
- `refactor/` - Refatoração
- `test/` - Testes

### 3. Faça suas Alterações

```bash
# Desenvolva sua feature/fix

# Verifique o status
git status

# Adicione arquivos
git add .

# Faça commit (veja padrões abaixo)
git commit -m "feat(api): adiciona endpoint de métricas"
```

### 4. Mantenha sua Branch Atualizada

```bash
# Rebase com upstream frequentemente
git fetch upstream
git rebase upstream/main
```

### 5. Execute os Testes

```bash
# Todos os testes
python -m pytest tests/ -v

# Testes com cobertura
python -m pytest tests/ --cov=factory --cov-report=term-missing

# Apenas testes específicos
python -m pytest tests/test_api.py -v
```

### 6. Envie para seu Fork

```bash
git push origin feature/minha-nova-feature
```

### 7. Abra um Pull Request

1. Vá para seu fork no GitHub
2. Clique em "Compare & pull request"
3. Preencha o template do PR
4. Aguarde revisão

---

## Padrões de Código

### Python

```python
# Type hints são obrigatórios
def calcular_story_points(story: Story, fatores: list[str]) -> int:
    """
    Calcula story points baseado em fatores de complexidade.

    Args:
        story: A user story para calcular
        fatores: Lista de fatores de complexidade

    Returns:
        Story points calculados (Fibonacci: 1, 2, 3, 5, 8, 13, 21)

    Raises:
        ValueError: Se a story não tiver critérios de aceite
    """
    if not story.acceptance_criteria:
        raise ValueError("Story precisa de critérios de aceite")

    complexidade = len(fatores)
    return _fibonacci_mais_proximo(complexidade)


# Nomes de variáveis em snake_case
user_story = get_story_by_id(story_id)
total_points = sum(s.story_points for s in stories)

# Classes em PascalCase
class StoryRepository:
    """Repositório para operações de User Stories."""

    def __init__(self, session: Session) -> None:
        self.session = session

    async def find_by_status(self, status: StoryStatus) -> list[Story]:
        """Busca stories por status."""
        ...
```

### Docstrings

Use docstrings em **português brasileiro** seguindo o estilo Google:

```python
def processar_story(story_id: str, opcoes: ProcessOptions) -> StoryResult:
    """
    Processa uma user story através do pipeline de desenvolvimento.

    Esta função coordena todo o fluxo de processamento de uma story,
    incluindo geração de código, linting, testes e documentação.

    Args:
        story_id: Identificador único da story (formato: STR-0001)
        opcoes: Opções de processamento (timeout, retries, etc)

    Returns:
        Resultado do processamento contendo:
        - status: sucesso/falha
        - arquivos_gerados: lista de arquivos criados
        - tempo_execucao: tempo em segundos

    Raises:
        StoryNotFoundError: Se a story não existir
        ProcessingError: Se ocorrer erro no processamento
        TimeoutError: Se exceder o tempo limite

    Examples:
        >>> resultado = processar_story("STR-0001", ProcessOptions())
        >>> print(resultado.status)
        'sucesso'
    """
```

### Commits (Conventional Commits)

```bash
# Formato: tipo(escopo): descrição

# Tipos:
feat     # Nova funcionalidade
fix      # Correção de bug
docs     # Documentação
style    # Formatação (não afeta código)
refactor # Refatoração
test     # Testes
chore    # Manutenção (dependências, configs)

# Exemplos:
git commit -m "feat(api): adiciona endpoint de métricas de sprint"
git commit -m "fix(auth): corrige validação de token expirado"
git commit -m "docs(readme): atualiza instruções de instalação"
git commit -m "refactor(core): simplifica lógica do autonomous loop"
git commit -m "test(integrations): adiciona testes para Jira API"
```

### Linting e Formatação

```bash
# Formatação automática com Black
black factory/

# Ordenar imports
isort factory/

# Linting com Ruff
ruff check factory/

# Type checking
mypy factory/
```

---

## Testes

### Estrutura de Testes

```
tests/
├── conftest.py              # Fixtures globais
├── test_api.py              # Testes de endpoints
├── test_auth.py             # Testes de autenticação
├── test_core.py             # Testes do core
├── test_database.py         # Testes de repositórios
├── test_integrations.py     # Testes de integrações
└── integration/             # Testes de integração
    └── test_jira_sync.py
```

### Escrevendo Testes

```python
import pytest
from factory.core.story_generator import StoryGenerator


class TestStoryGenerator:
    """Testes para o gerador de stories."""

    @pytest.fixture
    def generator(self) -> StoryGenerator:
        """Cria instância do gerador para testes."""
        return StoryGenerator()

    def test_gerar_story_valida(self, generator: StoryGenerator) -> None:
        """Deve gerar story com todos os campos obrigatórios."""
        # Arrange
        descricao = "Login com email e senha"

        # Act
        story = generator.gerar(descricao)

        # Assert
        assert story.title is not None
        assert story.persona is not None
        assert story.action is not None
        assert story.benefit is not None

    def test_gerar_story_com_criterios(self, generator: StoryGenerator) -> None:
        """Deve gerar critérios de aceite automaticamente."""
        descricao = "Cadastro de usuário com validação"
        story = generator.gerar(descricao)

        assert len(story.acceptance_criteria) > 0

    @pytest.mark.asyncio
    async def test_gerar_story_async(self, generator: StoryGenerator) -> None:
        """Deve funcionar de forma assíncrona."""
        story = await generator.gerar_async("Nova feature")
        assert story is not None
```

### Executando Testes

```bash
# Todos os testes
python -m pytest tests/ -v

# Com cobertura
python -m pytest tests/ --cov=factory --cov-report=html

# Apenas um arquivo
python -m pytest tests/test_api.py -v

# Apenas um teste específico
python -m pytest tests/test_api.py::test_create_story -v

# Testes marcados
python -m pytest -m "integration" -v

# Parallel (mais rápido)
python -m pytest tests/ -n auto
```

---

## Documentação

### Atualizando Docs

Sempre que adicionar uma nova feature, atualize a documentação relevante:

| Tipo de Mudança | Arquivo a Atualizar |
|-----------------|---------------------|
| Nova API | `docs/API_REFERENCE.md` |
| Nova feature | `docs/GUIA_USUARIO.md` |
| Arquitetura | `docs/ARQUITETURA.md` |
| Integração | `docs/integrations/` |
| Segurança | `docs/SECURITY_HARDENING.md` |

### Estilo da Documentação

- Use Markdown (GitHub Flavored)
- Português brasileiro (pt-BR)
- Inclua exemplos de código
- Use tabelas quando apropriado
- Adicione screenshots se visual

---

## Pull Requests

### Template de PR

```markdown
## Descrição

Breve descrição do que foi alterado e por quê.

## Tipo de Mudança

- [ ] Nova feature
- [ ] Correção de bug
- [ ] Refatoração
- [ ] Documentação
- [ ] Outro: ____

## Issue Relacionada

Closes #123

## Como Testar

1. Passo 1
2. Passo 2
3. Resultado esperado

## Checklist

- [ ] Código segue os padrões do projeto
- [ ] Testes foram adicionados/atualizados
- [ ] Documentação foi atualizada
- [ ] Não há warnings de linting
- [ ] Funciona localmente
```

### Revisão de Código

- PRs precisam de pelo menos 1 aprovação
- Responda a todos os comentários
- Faça as alterações solicitadas
- Mantenha commits organizados (squash se necessário)

---

## Reportando Issues

### Template de Bug

```markdown
## Descrição do Bug

Descreva o bug de forma clara e concisa.

## Passos para Reproduzir

1. Vá para '...'
2. Clique em '...'
3. Role até '...'
4. Veja o erro

## Comportamento Esperado

O que deveria acontecer.

## Screenshots

Se aplicável, adicione screenshots.

## Ambiente

- OS: [ex: Windows 11]
- Python: [ex: 3.10.5]
- Browser: [ex: Chrome 120]
- Versão: [ex: v7.0.0]

## Logs

```
Cole logs relevantes aqui
```
```

### Template de Feature

```markdown
## Descrição da Feature

Descreva a funcionalidade desejada.

## Problema que Resolve

Qual problema essa feature resolve?

## Solução Proposta

Como você imagina a implementação?

## Alternativas Consideradas

Outras abordagens que você considerou.

## Contexto Adicional

Qualquer informação extra.
```

---

## Dúvidas?

- Abra uma issue com a tag `question`
- Consulte a [documentação](docs/)
- Entre em contato via issues

---

**Obrigado por contribuir!** 🎉

---

*Última atualização: 2025-12-30*

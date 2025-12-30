# Integração GitLab - Fábrica de Agentes

## Visão Geral

A integração com GitLab permite sincronização bidirecional entre User Stories da Fábrica de Agentes e Issues do GitLab, além de criação automática de Merge Requests.

## Configuração

### Variáveis de Ambiente

```bash
# Token de acesso do GitLab (obrigatório)
GITLAB_TOKEN=glpat-xxxxxxxxxxxxxxxxxxxx

# URL do GitLab (opcional, padrão: gitlab.com)
GITLAB_URL=https://gitlab.com

# ID do projeto GitLab (obrigatório)
GITLAB_PROJECT_ID=12345678
```

### Criando Token de Acesso

1. Acesse **Settings > Access Tokens** no GitLab
2. Crie um token com os escopos:
   - `api` - Acesso completo à API
   - `read_repository` - Leitura do repositório
   - `write_repository` - Escrita no repositório
3. Copie o token e configure em `GITLAB_TOKEN`

### Obtendo o Project ID

1. Acesse seu projeto no GitLab
2. O ID está visível em **Settings > General** ou na URL do projeto
3. Configure em `GITLAB_PROJECT_ID`

## Funcionalidades

### Sincronização de Issues

| GitLab Label | Status na Fábrica |
|--------------|-------------------|
| `backlog` | Backlog |
| `ready` | Ready |
| `doing`, `in-progress` | In Progress |
| `review` | Review |
| `testing` | Testing |
| `done`, `closed` | Done |

### Criação de Merge Requests

A integração cria MRs automaticamente quando:
- Uma Story é movida para "Review"
- Tasks de desenvolvimento são concluídas

**Campos do MR:**
- **Title**: Título da Story
- **Description**: Descrição + critérios de aceite
- **Source Branch**: `feature/story-{id}`
- **Target Branch**: `main` (configurável)
- **Labels**: Labels baseados na prioridade

### Webhooks

Configure webhooks no GitLab para sincronização em tempo real:

1. Acesse **Settings > Webhooks** no projeto GitLab
2. Configure a URL: `https://sua-fabrica.com/api/webhooks/gitlab`
3. Selecione eventos:
   - Issues events
   - Merge request events
   - Push events
4. Adicione o Secret Token (mesmo valor de `WEBHOOK_SECRET`)

## Uso via API

### Sincronizar Story com GitLab

```bash
# Criar Issue no GitLab a partir de Story
POST /api/stories/{story_id}/sync-gitlab

# Resposta
{
  "success": true,
  "gitlab_issue_id": 123,
  "gitlab_issue_url": "https://gitlab.com/project/-/issues/123"
}
```

### Criar Merge Request

```bash
# Criar MR para uma Story
POST /api/stories/{story_id}/create-mr

# Body (opcional)
{
  "source_branch": "feature/story-001",
  "target_branch": "main"
}

# Resposta
{
  "success": true,
  "mr_id": 456,
  "mr_url": "https://gitlab.com/project/-/merge_requests/456"
}
```

## Uso via Python

```python
from factory.integrations.gitlab_integration import GitLabIntegration

# Inicializar
gitlab = GitLabIntegration(
    token="glpat-xxx",
    project_id="12345678",
    url="https://gitlab.com"
)

# Criar issue
issue = await gitlab.create_issue(
    title="Implementar login",
    description="Como usuário, quero fazer login...",
    labels=["feature", "high-priority"]
)

# Criar merge request
mr = await gitlab.create_merge_request(
    title="feat: implementar login",
    source_branch="feature/login",
    target_branch="main",
    description="Implementação do login de usuários"
)

# Sincronizar story
await gitlab.sync_story_to_issue(story_id="STR-0001")
```

## Mapeamento de Campos

| Campo Story | Campo GitLab Issue |
|-------------|-------------------|
| title | title |
| persona + action + benefit | description |
| priority | labels (priority::*) |
| story_points | weight |
| status | labels (workflow::*) |
| epic_id | epic_id |

## Troubleshooting

### Erro 401 - Unauthorized
- Verifique se o token está correto
- Confirme que o token tem os escopos necessários
- Verifique se o token não expirou

### Erro 404 - Project Not Found
- Confirme o `GITLAB_PROJECT_ID`
- Verifique se o token tem acesso ao projeto

### Webhook não funciona
- Verifique se a URL está acessível externamente
- Confirme que o Secret Token está correto
- Verifique os logs em **Settings > Webhooks > Recent Deliveries**

### Issues não sincronizam
- Verifique se as labels existem no projeto GitLab
- Confirme que o mapeamento de status está correto
- Verifique os logs da aplicação

## Referências

- [GitLab API Documentation](https://docs.gitlab.com/ee/api/)
- [GitLab Webhooks](https://docs.gitlab.com/ee/user/project/integrations/webhooks.html)
- [Personal Access Tokens](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html)

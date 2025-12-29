# Configuracao da Integracao com Microsoft Teams

Este documento descreve como configurar a integracao completa com Microsoft Teams
na Fabrica de Agentes.

## Indice

1. [Visao Geral](#visao-geral)
2. [Metodos de Integracao](#metodos-de-integracao)
3. [Configuracao via Webhook](#configuracao-via-webhook)
4. [Configuracao via Microsoft Graph API](#configuracao-via-microsoft-graph-api)
5. [Configuracao do Bot](#configuracao-do-bot)
6. [Uso no Codigo](#uso-no-codigo)
7. [Comandos do Bot](#comandos-do-bot)
8. [Troubleshooting](#troubleshooting)

---

## Visao Geral

A integracao com Microsoft Teams permite:

- **Notificacoes em Canais**: Enviar mensagens para canais do Teams
- **Cards Adaptativos**: Enviar mensagens ricas com botoes e acoes
- **Mensagens Diretas**: Enviar DMs para usuarios especificos
- **Bot Interativo**: Receber comandos e responder usuarios

### Componentes

| Componente | Descricao | Requer |
|------------|-----------|--------|
| WebhookClient | Envia mensagens via Incoming Webhooks | URL do Webhook |
| GraphClient | Acessa API do Microsoft Graph | Azure AD App |
| ChannelNotifier | Notifica canais | Webhook ou Graph |
| DMNotifier | Envia DMs | Graph API |
| TeamsBotHandler | Processa comandos do bot | Azure Bot |

---

## Metodos de Integracao

### 1. Incoming Webhook (Simples)

**Vantagens:**
- Configuracao rapida
- Sem necessidade de Azure AD
- Sem custo adicional

**Limitacoes:**
- Apenas envia mensagens
- Nao recebe respostas
- Sem mensagens diretas

### 2. Microsoft Graph API (Intermediario)

**Vantagens:**
- Acesso completo a canais e chats
- Mensagens diretas
- Leitura de dados

**Requer:**
- Registro de app no Azure AD
- Permissoes adequadas

### 3. Bot Framework (Completo)

**Vantagens:**
- Interacao bidirecional
- Comandos personalizados
- Aprovacoes e feedback

**Requer:**
- Azure Bot Service
- Endpoint publico (HTTPS)

---

## Configuracao via Webhook

### Passo 1: Criar Incoming Webhook no Teams

1. Abra o Microsoft Teams
2. Va ao canal desejado
3. Clique nos tres pontos (...) > **Conectores**
4. Encontre **Incoming Webhook** e clique **Adicionar**
5. De um nome (ex: "Fabrica de Agentes")
6. Copie a **URL do Webhook**

### Passo 2: Configurar no Projeto

```python
from factory.integrations.teams import TeamsIntegration, TeamsConfig

# Configuracao simples com um webhook
config = TeamsConfig(
    enabled=True,
    webhook_url="https://outlook.office.com/webhook/xxx"
)

# Configuracao com multiplos canais
config = TeamsConfig(
    enabled=True,
    webhooks={
        "dev": "https://outlook.office.com/webhook/dev-xxx",
        "alerts": "https://outlook.office.com/webhook/alerts-yyy",
        "general": "https://outlook.office.com/webhook/general-zzz"
    },
    default_channel="dev"
)

teams = TeamsIntegration(config)
await teams.connect()
```

### Passo 3: Enviar Notificacoes

```python
# Mensagem simples
await teams.notify("Projeto concluido!")

# Com titulo
await teams.notify("Deploy realizado com sucesso", title="Deploy")

# Para canal especifico
await teams.notify("Erro critico!", channel="alerts")

# Tipos pre-definidos
await teams.notify_success("Deploy", "Versao 1.0 publicada")
await teams.notify_warning("Aviso", "Disco em 80%")
await teams.notify_error("Erro", "Falha na conexao")

# Projeto concluido
await teams.notify_project_completed(
    project_id="PROJ-001",
    project_name="Sistema de Vendas",
    files_count=45,
    duration="2 minutos"
)
```

---

## Configuracao via Microsoft Graph API

### Passo 1: Registrar App no Azure AD

1. Acesse o [Portal Azure](https://portal.azure.com)
2. Va em **Azure Active Directory** > **Registros de aplicativo**
3. Clique em **Novo registro**
4. Configure:
   - Nome: "Fabrica de Agentes Teams"
   - Tipo de conta: "Apenas esta organizacao"
5. Apos criar, anote:
   - **ID do aplicativo (cliente)**
   - **ID do diretorio (locatario)**

### Passo 2: Criar Secret

1. No app registrado, va em **Certificados e segredos**
2. Clique em **Novo segredo do cliente**
3. De uma descricao e selecione validade
4. **IMPORTANTE**: Copie o valor do segredo imediatamente

### Passo 3: Configurar Permissoes

1. Va em **Permissoes de API** > **Adicionar uma permissao**
2. Selecione **Microsoft Graph**
3. Escolha **Permissoes de aplicativo**
4. Adicione:
   - `ChannelMessage.Send`
   - `Chat.ReadWrite.All`
   - `User.Read.All`
5. Clique em **Conceder consentimento de administrador**

### Passo 4: Configurar no Projeto

```python
from factory.integrations.teams import TeamsIntegration, TeamsConfig
from factory.integrations.teams.notifications.dm_notifier import UserMapping

config = TeamsConfig(
    enabled=True,
    # Graph API
    graph_tenant_id="seu-tenant-id",
    graph_client_id="seu-client-id",
    graph_client_secret="seu-client-secret",
    # Mapeamento de usuarios para DMs
    users={
        "joao": UserMapping(
            name="Joao Silva",
            user_id="azure-ad-user-id",
            email="joao@empresa.com"
        ),
        "maria": UserMapping(
            name="Maria Santos",
            user_id="azure-ad-user-id-2",
            email="maria@empresa.com"
        )
    }
)

teams = TeamsIntegration(config)
await teams.connect()

# Enviar DM
await teams.send_dm("joao", "Sua tarefa foi concluida!")
```

---

## Configuracao do Bot

### Passo 1: Criar Bot no Azure

1. Acesse o [Portal Azure](https://portal.azure.com)
2. Pesquise por **Azure Bot**
3. Clique em **Criar**
4. Configure:
   - Nome: "fabrica-agentes-bot"
   - Preco: F0 (gratuito)
   - App ID: Use o app registrado anteriormente
5. Apos criar, va em **Canais** e adicione **Microsoft Teams**

### Passo 2: Configurar Endpoint

O bot precisa de um endpoint publico HTTPS para receber mensagens.

**Opcao A: Azure Functions**
```
https://sua-function.azurewebsites.net/api/teams/messages
```

**Opcao B: ngrok (desenvolvimento)**
```bash
ngrok http 9001
# Use a URL HTTPS gerada
```

### Passo 3: Registrar Endpoint no Azure

1. No Azure Bot, va em **Configuracao**
2. Em **Messaging endpoint**, coloque sua URL:
   ```
   https://seu-dominio.com/api/teams/messages
   ```

### Passo 4: Configurar no Projeto

```python
from factory.integrations.teams import TeamsIntegration, TeamsConfig

config = TeamsConfig(
    enabled=True,
    # Bot
    bot_app_id="seu-bot-app-id",
    bot_app_password="seu-bot-password",
    bot_endpoint="/api/teams/messages"
)

teams = TeamsIntegration(config)

# Registrar callbacks para comandos
teams.register_bot_action("get_platform_status", get_status_func)
teams.register_bot_action("list_projects", list_projects_func)
teams.register_bot_action("create_project", create_project_func)
```

### Passo 5: Endpoint no Dashboard

```python
from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route('/api/teams/messages', methods=['POST'])
async def teams_webhook():
    activity = request.json
    response = await teams.process_bot_activity(activity)
    if response:
        return jsonify(response)
    return '', 200
```

---

## Uso no Codigo

### Configuracao Completa

```python
from factory.integrations.teams import TeamsIntegration, TeamsConfig
from factory.integrations.teams.notifications.dm_notifier import UserMapping
from factory.integrations.teams.notifications.channel_notifier import ChannelConfig, NotificationMethod

config = TeamsConfig(
    enabled=True,

    # Webhooks
    webhook_url="https://outlook.office.com/webhook/default",
    webhooks={
        "dev": "https://outlook.office.com/webhook/dev",
        "alerts": "https://outlook.office.com/webhook/alerts"
    },

    # Graph API
    graph_tenant_id="xxx",
    graph_client_id="xxx",
    graph_client_secret="xxx",

    # Bot
    bot_app_id="xxx",
    bot_app_password="xxx",

    # Canais com Graph (opcional)
    channels={
        "principal": ChannelConfig(
            name="Canal Principal",
            team_id="team-id",
            channel_id="channel-id",
            method=NotificationMethod.GRAPH_API
        )
    },

    # Usuarios
    users={
        "admin": UserMapping(
            name="Administrador",
            user_id="user-id",
            email="admin@empresa.com"
        )
    },

    # Configuracoes gerais
    default_channel="dev",
    dashboard_url="https://fabrica.empresa.com",
    quiet_hours_start=22,
    quiet_hours_end=7,
    respect_quiet_hours=True
)

teams = TeamsIntegration(config)
await teams.connect()
```

### Exemplos de Uso

```python
# Notificacoes simples
await teams.notify("Mensagem")
await teams.notify_success("Titulo", "Mensagem de sucesso")
await teams.notify_error("Erro", "Descricao do erro")

# Notificacoes de projeto
await teams.notify_project_started(
    project_id="PROJ-001",
    project_name="Sistema X",
    description="Sistema de gestao"
)

await teams.notify_project_completed(
    project_id="PROJ-001",
    project_name="Sistema X",
    files_count=45,
    duration="2 minutos"
)

await teams.notify_project_error(
    project_id="PROJ-001",
    project_name="Sistema X",
    error_message="Falha na compilacao",
    step="Build"
)

# Notificacoes de story
await teams.notify_story_status_change(
    story_id="STR-001",
    story_title="Implementar login",
    old_status="ready",
    new_status="in_progress"
)

# Notificacoes de task
await teams.notify_task_completed(
    task_id="TSK-001",
    task_title="Criar endpoint",
    story_id="STR-001",
    story_title="Implementar login",
    files_created=["auth.py", "login.html"]
)

# Mensagens diretas
await teams.send_dm("admin", "Voce tem uma aprovacao pendente")

# Resumo diario
await teams.send_daily_summary(
    stories_completed=5,
    stories_in_progress=3,
    tasks_completed=12,
    files_generated=45
)

# Card customizado
card = teams.create_card()
card.add_heading("Titulo Custom")
card.add_text("Descricao")
card.add_fact_set([("Chave", "Valor")])
card.add_action_url("Ver mais", "https://...")
await teams.notify_card(card.build())
```

---

## Comandos do Bot

### Comandos Disponiveis

| Comando | Descricao |
|---------|-----------|
| `/fabrica status` | Status geral da plataforma |
| `/fabrica status [id]` | Status de projeto especifico |
| `/fabrica listar` | Lista projetos |
| `/fabrica listar stories [id]` | Lista stories de um projeto |
| `/fabrica projeto [nome]` | Cria novo projeto |
| `/fabrica desenvolver [id]` | Inicia desenvolvimento |
| `/fabrica story criar [titulo]` | Cria nova story |
| `/fabrica ajuda` | Mostra comandos disponiveis |

### Exemplo de Interacao

```
Usuario: /fabrica status

Bot: [Card com status]
     Status: Online
     Stories Totais: 25
     Tarefas em Andamento: 5
     ...

Usuario: /fabrica projeto Sistema de Vendas

Bot: [Card de confirmacao]
     Confirmar criacao do projeto "Sistema de Vendas"?
     [Confirmar] [Cancelar]
```

---

## Troubleshooting

### Webhook nao envia mensagens

1. Verifique se a URL do webhook esta correta
2. Teste a URL manualmente:
   ```bash
   curl -X POST "URL_DO_WEBHOOK" \
     -H "Content-Type: application/json" \
     -d '{"text": "Teste"}'
   ```
3. Verifique se o conector esta ativo no Teams

### Graph API retorna 401

1. Verifique as credenciais (tenant_id, client_id, client_secret)
2. Confirme que as permissoes foram concedidas pelo admin
3. Verifique se o segredo nao expirou

### Bot nao responde

1. Verifique se o endpoint esta acessivel publicamente
2. Confirme que o endpoint esta configurado no Azure Bot
3. Verifique os logs do servidor
4. Teste com o Bot Framework Emulator

### DMs nao sao enviadas

1. Verifique se o user_id esta correto (Azure AD Object ID)
2. Confirme as permissoes: `Chat.ReadWrite.All`
3. Verifique se o app tem consentimento do admin

---

## Variaveis de Ambiente

Para producao, use variaveis de ambiente:

```bash
# .env
TEAMS_WEBHOOK_URL=https://outlook.office.com/webhook/xxx
TEAMS_WEBHOOK_DEV=https://outlook.office.com/webhook/dev
TEAMS_WEBHOOK_ALERTS=https://outlook.office.com/webhook/alerts

TEAMS_TENANT_ID=xxx
TEAMS_CLIENT_ID=xxx
TEAMS_CLIENT_SECRET=xxx

TEAMS_BOT_APP_ID=xxx
TEAMS_BOT_PASSWORD=xxx
```

```python
import os
from factory.integrations.teams import TeamsConfig

config = TeamsConfig(
    enabled=True,
    webhook_url=os.getenv("TEAMS_WEBHOOK_URL"),
    graph_tenant_id=os.getenv("TEAMS_TENANT_ID"),
    graph_client_id=os.getenv("TEAMS_CLIENT_ID"),
    graph_client_secret=os.getenv("TEAMS_CLIENT_SECRET"),
    bot_app_id=os.getenv("TEAMS_BOT_APP_ID"),
    bot_app_password=os.getenv("TEAMS_BOT_PASSWORD")
)
```

---

## Dependencias

Instale as dependencias necessarias:

```bash
pip install aiohttp msal
```

Para o bot completo:
```bash
pip install botbuilder-core botbuilder-schema
```

---

*Fabrica de Agentes - Integracao Microsoft Teams v1.0*

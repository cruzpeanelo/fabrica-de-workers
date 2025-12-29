# Integracao Microsoft Graph (Exchange/Outlook)

## Visao Geral

A integracao com Microsoft Graph permite:

- **Envio de emails** via Microsoft Graph API
- **Leitura de caixa de entrada** para processar requisitos
- **Gerenciamento de calendario** (reunioes Teams)
- **Processamento de anexos** (PDF, Word, Excel)
- **Templates HTML** personalizados
- **Notificacoes automaticas** por email
- **Relatorios periodicos** (diario, semanal)

## Configuracao

### 1. Registrar Aplicacao no Azure AD

1. Acesse [portal.azure.com](https://portal.azure.com)
2. Va para **Azure Active Directory** > **App registrations** > **New registration**
3. Defina um nome (ex: "Fabrica de Agentes - Email")
4. Tipo de conta: **Single tenant** ou **Multitenant**
5. Clique em **Register**

### 2. Configurar Permissoes

Em **API permissions**, adicione:

| Permissao | Tipo | Descricao |
|-----------|------|-----------|
| Mail.Send | Application | Enviar emails |
| Mail.Read | Application | Ler emails |
| Mail.ReadWrite | Application | Ler/escrever emails |
| Calendars.ReadWrite | Application | Criar reunioes |
| User.Read | Application | Ler perfil usuario |

Clique em **Grant admin consent** para aprovar.

### 3. Criar Client Secret

1. Va para **Certificates & secrets** > **New client secret**
2. Defina uma descricao e validade
3. Copie o valor do secret (so aparece uma vez!)

### 4. Configurar Variaveis de Ambiente

Crie ou edite o arquivo `.env`:

```bash
# Azure AD Application
AZURE_TENANT_ID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
AZURE_CLIENT_ID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
AZURE_CLIENT_SECRET=seu-client-secret

# Email Configuration
GRAPH_SENDER_EMAIL=fabrica@suaempresa.com
GRAPH_DEFAULT_RECIPIENTS=team@suaempresa.com
GRAPH_ADMIN_RECIPIENTS=admin@suaempresa.com
GRAPH_MONITORED_FOLDER=Inbox
GRAPH_ENABLED=true

# SMTP Fallback (opcional)
SMTP_SERVER=smtp.office365.com
SMTP_PORT=587
SMTP_USERNAME=fabrica@suaempresa.com
SMTP_PASSWORD=sua-senha
SMTP_SENDER_NAME=Fabrica de Agentes
SMTP_USE_TLS=true
SMTP_ENABLED=false
```

## Uso Basico

### Inicializacao

```python
from factory.integrations.microsoft_graph import (
    MicrosoftGraphClient,
    EmailConfig,
    init_email_integration
)

# Inicializacao automatica
client = await init_email_integration()

# Ou manual
config = EmailConfig.from_env()
client = MicrosoftGraphClient(config)
await client.connect()
```

### Enviar Email Simples

```python
await client.send_email(
    to=["usuario@empresa.com", "outro@empresa.com"],
    subject="Projeto Concluido",
    body="<h1>Sucesso!</h1><p>Seu projeto foi finalizado.</p>",
    body_type="HTML"
)
```

### Enviar com Template

```python
await client.send_email_with_template(
    to=["usuario@empresa.com"],
    template_name="project_completed",
    template_vars={
        "project_name": "Meu Projeto",
        "project_id": "PROJ-001",
        "files_count": 15,
        "execution_time": "2h 30min"
    }
)
```

### Templates Disponiveis

| Template | Descricao |
|----------|-----------|
| `project_created` | Novo projeto criado |
| `project_completed` | Projeto finalizado |
| `story_status_update` | Story atualizada |
| `error_alert` | Alerta de erro |
| `daily_report` | Relatorio diario |

### Ler Emails

```python
# Emails nao lidos
messages = await client.get_messages(
    folder="Inbox",
    filter_query="isRead eq false",
    top=10
)

# Email especifico
message = await client.get_message(message_id)

# Marcar como lido
await client.mark_as_read(message_id)
```

### Agendar Reuniao

```python
from datetime import datetime, timedelta

event = await client.schedule_meeting(
    subject="Review do Projeto",
    start=datetime.now() + timedelta(days=1),
    duration_minutes=30,
    attendees=["team@empresa.com"],
    body="Reuniao para revisar o projeto",
    location="Teams"
)

# URL da reuniao Teams
teams_url = event.get("onlineMeeting", {}).get("joinUrl")
```

## Componentes

### Readers

```python
from factory.integrations.email.readers import InboxReader, AttachmentProcessor

# Leitor de inbox
reader = InboxReader(client)
emails = await reader.get_project_requests(days_back=7)

for email in emails:
    project_data = await reader.extract_project_requirements(email)
    print(f"Projeto: {project_data['name']}")

# Processador de anexos
processor = AttachmentProcessor(client)
attachments = await processor.get_attachments(message_id)

for att in attachments:
    text = await processor.extract_text(att)
    requirements = processor.extract_requirements(text)
```

### Senders

```python
from factory.integrations.email.senders import NotificationSender, ReportSender

# Notificacoes
notifier = NotificationSender(client)
await notifier.notify_project_completed(project_data)
await notifier.notify_error("Falha", "Descricao do erro")

# Relatorios
reporter = ReportSender(client)
await reporter.send_daily_report()
await reporter.send_project_report(project_data)
```

### Skills para Agentes

```python
from factory.integrations.email.skills import EmailSendSkill, EmailReadSkill

# Skill de envio
send_skill = EmailSendSkill(client)
result = await send_skill.execute({
    "action": "send",
    "to": ["usuario@empresa.com"],
    "subject": "Teste",
    "body": "Conteudo"
})

# Skill de leitura
read_skill = EmailReadSkill(client)
result = await read_skill.execute({
    "action": "get_project_requests"
})
```

## Integracao Facade

Para uso simplificado, use a classe `EmailIntegration`:

```python
from factory.integrations.microsoft_graph import EmailIntegration

integration = EmailIntegration()
await integration.connect()

# Notificacoes
await integration.notify_project_created(project_data)
await integration.notify_project_completed(project_data, execution_data)
await integration.notify_story_updated(story_data)
await integration.notify_error("Tipo", "Mensagem")

# Relatorios
await integration.send_daily_report()
await integration.send_project_report(project_data)

# Leitura
emails = await integration.get_unread_emails()
requests = await integration.get_project_requests()
project_data = await integration.process_email_for_project(email)

# Monitoramento
async def on_new_project(email):
    project = await integration.process_email_for_project(email)
    # Criar projeto...

await integration.start_inbox_monitor(on_new_project)
```

## Fluxo de Email para Projeto

1. Usuario envia email para `fabrica@empresa.com`
2. Assunto: `[PROJETO] Nome do Projeto`
3. Corpo: Descricao dos requisitos
4. Anexos: Documentos PDF/Word (opcional)
5. Sistema processa email e extrai requisitos
6. Projeto criado automaticamente
7. Email de confirmacao enviado ao usuario
8. Ao concluir, email com resultados

## Estrutura de Arquivos

```
factory/integrations/email/
├── __init__.py
├── graph_mail_client.py     # Cliente Microsoft Graph
├── smtp_client.py           # Cliente SMTP (fallback)
├── templates/               # Templates HTML
│   ├── base.html
│   ├── project_created.html
│   ├── project_completed.html
│   ├── story_status_update.html
│   ├── error_alert.html
│   └── daily_report.html
├── readers/
│   ├── __init__.py
│   ├── inbox_reader.py      # Leitor de caixa de entrada
│   └── attachment_processor.py  # Processador de anexos
├── senders/
│   ├── __init__.py
│   ├── notification_sender.py  # Notificacoes
│   └── report_sender.py        # Relatorios
└── skills/
    ├── __init__.py
    ├── email_send_skill.py   # Skill de envio
    └── email_read_skill.py   # Skill de leitura
```

## Dependencias

```bash
pip install msal           # Microsoft Authentication Library
pip install aiohttp        # HTTP async client
pip install jinja2         # Templates
pip install pdfplumber     # Leitura de PDF (opcional)
pip install python-docx    # Leitura de Word (opcional)
pip install openpyxl       # Leitura de Excel (opcional)
pip install beautifulsoup4 # Parsing HTML (opcional)
```

## Troubleshooting

### Erro de Autenticacao

```
AADSTS7000215: Invalid client secret provided
```

**Solucao:** Verifique se o `AZURE_CLIENT_SECRET` esta correto e nao expirou.

### Erro de Permissao

```
Authorization_RequestDenied: Insufficient privileges
```

**Solucao:** Adicione as permissoes necessarias no Azure AD e conceda admin consent.

### Email Nao Enviado

```
The mailbox is either inactive, soft-deleted, or is hosted on-premise
```

**Solucao:** Verifique se `GRAPH_SENDER_EMAIL` e uma caixa de correio valida no Exchange Online.

---

*Integracao Microsoft Graph - Fabrica de Agentes v6.0*

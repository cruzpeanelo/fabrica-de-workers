# SharePoint Integration

**Terminal 5 - Issue #298**

Integração com Microsoft SharePoint via Microsoft Graph API para leitura e sincronização de documentos.

## Visão Geral

Esta integração permite:
- Leitura de sites, listas e bibliotecas de documentos
- Upload/download de arquivos
- Sincronização bidirecional de pastas
- Gerenciamento de versões
- Compartilhamento de arquivos
- Webhooks para notificações em tempo real

## Configuração

### 1. Registrar Aplicativo no Azure AD

1. Acesse [Azure Portal](https://portal.azure.com) > Azure Active Directory > App registrations
2. Clique em "New registration"
3. Configure:
   - Nome: "Fabrica de Agentes - SharePoint"
   - Tipo de conta: Multi-tenant (se necessário)
4. Após criar, anote:
   - Application (client) ID
   - Directory (tenant) ID
5. Em "Certificates & secrets", crie um novo client secret

### 2. Configurar Permissões

Em "API permissions", adicione:

| Permissão | Tipo | Descrição |
|-----------|------|-----------|
| Sites.Read.All | Application | Ler todos os sites |
| Sites.ReadWrite.All | Application | Ler/escrever sites |
| Files.Read.All | Application | Ler todos os arquivos |
| Files.ReadWrite.All | Application | Ler/escrever arquivos |

**Importante**: Clique em "Grant admin consent" para aprovar as permissões.

### 3. Configurar na Fábrica de Agentes

```python
from factory.integrations.sharepoint import SharePointConfig, SharePointReadSkill

config = SharePointConfig(
    tenant_id="seu-tenant-id",
    client_id="seu-client-id",
    client_secret="seu-client-secret",
    integration_tenant_id="TENANT-001"  # Para isolamento de dados
)
```

## Uso

### Leitura de Sites

```python
from factory.integrations.sharepoint import SharePointConfig
from factory.integrations.sharepoint.skills import SharePointReadSkill

config = SharePointConfig(...)
skill = SharePointReadSkill(config)

# Listar sites
result = await skill.read_sites()
if result.success:
    for site in result.data:
        print(f"Site: {site['displayName']}")

# Buscar site específico
result = await skill.read_site_info("site-id")
```

### Leitura de Documentos

```python
# Listar bibliotecas
result = await skill.read_document_libraries(site_id)

# Listar documentos de uma pasta
result = await skill.read_documents(site_id, drive_id, "/Projetos")

# Download de arquivo
result = await skill.read_document_content(site_id, drive_id, "/Projetos/relatorio.pdf")
if result.success:
    content = result.data  # bytes
```

### Sincronização

```python
from factory.integrations.sharepoint import SharePointConfig, SharePointSyncConfig
from factory.integrations.sharepoint.skills import SharePointSyncSkill

config = SharePointConfig(...)
sync_config = SharePointSyncConfig(
    local_path="/projetos/cliente",
    remote_folder="/Documentos/Projeto",
    conflict_resolution="newest_wins"  # ou "remote_wins", "local_wins"
)

skill = SharePointSyncSkill(config, sync_config)

# Sync bidirecional
result = await skill.sync(site_id, drive_id)
print(f"Uploaded: {len(result.uploaded)}")
print(f"Downloaded: {len(result.downloaded)}")
print(f"Conflicts: {len(result.conflicts)}")

# Delta sync (apenas mudanças)
result = await skill.delta_sync(drive_id)
```

### Upload de Arquivos

```python
from factory.integrations.sharepoint import DocumentClient

client = DocumentClient(config)
await client.authenticate()

# Upload simples (até 4MB)
with open("arquivo.pdf", "rb") as f:
    result = await client.upload_file(
        site_id, drive_id,
        "/Documentos/arquivo.pdf",
        f.read()
    )

# Upload grande (até 250MB)
result = await client.upload_large_file(
    site_id, drive_id,
    "/Documentos/arquivo_grande.zip",
    large_content
)
```

### Gerenciamento de Listas

```python
from factory.integrations.sharepoint import ListClient

client = ListClient(config)
await client.authenticate()

# Listar itens
items = await client.list_items(
    site_id, list_id,
    filter_query="fields/Status eq 'Ativo'",
    order_by="fields/Created desc",
    top=50
)

# Criar item
new_item = await client.create_item(
    site_id, list_id,
    {"Title": "Novo Item", "Status": "Pendente"}
)

# Atualizar item
await client.update_item(
    site_id, list_id, item_id,
    {"Status": "Concluído"}
)
```

## Isolamento de Tenant

Todas as operações são isoladas por `integration_tenant_id`:

```python
config = SharePointConfig(
    tenant_id="azure-tenant-id",
    client_id="...",
    client_secret="...",
    integration_tenant_id="CLIENTE-001"  # Isolamento
)
```

O `integration_tenant_id` é usado para:
- Filtrar dados por tenant
- Validar webhooks
- Audit logging
- Rate limiting separado

## Webhooks

### Criar Subscription

```python
from factory.integrations.sharepoint import SiteClient
from datetime import datetime, timedelta

client = SiteClient(config)
await client.authenticate()

subscription = await client.create_subscription(
    resource=f"/sites/{site_id}/lists/{list_id}/items",
    notification_url="https://sua-api.com/webhooks/sharepoint",
    expiration_datetime=datetime.utcnow() + timedelta(days=30),
    change_type="created,updated,deleted"
)
```

### Validar Webhook

```python
# O clientState contém o tenant_id para validação
if request.headers.get("clientState") != f"tenant_{tenant_id}":
    return Response(status=401)
```

## Troubleshooting

### Erro 403: Access Denied

1. Verifique se as permissões foram concedidas com "Admin consent"
2. Confirme que o tenant_id está correto
3. Verifique se o aplicativo tem acesso ao site específico

### Erro 429: Too Many Requests

O cliente implementa retry automático com backoff exponencial. Para ajustar:

```python
config = SharePointConfig(
    ...,
    max_retries=5,
    retry_delay_seconds=2.0
)
```

### Upload Falha para Arquivos Grandes

Para arquivos > 4MB, use `upload_large_file()` que faz upload em chunks:

```python
config = SharePointConfig(
    ...,
    chunk_size=10 * 1024 * 1024  # 10MB por chunk
)
```

## Dependências

```
aiohttp>=3.8.0
```

## Referências

- [Microsoft Graph API - SharePoint](https://docs.microsoft.com/graph/api/resources/sharepoint)
- [SharePoint REST API](https://docs.microsoft.com/sharepoint/dev/sp-add-ins/get-to-know-the-sharepoint-rest-service)
- [Azure AD Authentication](https://docs.microsoft.com/azure/active-directory/develop/)

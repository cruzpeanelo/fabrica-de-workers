# MCP Tools - Fabrica de Agentes

## Visao Geral

O modulo MCP (Model Context Protocol) permite que Claude Desktop e outros clientes MCP interajam diretamente com a Fabrica de Agentes. Atraves dessas ferramentas, e possivel:

- Criar e gerenciar User Stories no formato Agile
- Criar e completar Tasks de desenvolvimento
- Iniciar e monitorar Jobs de processamento
- Consultar projetos e metricas do sistema

## Instalacao

### Requisitos

- Python 3.10+
- Fabrica de Agentes instalada
- Claude Desktop (opcional, para uso via MCP)

### Configuracao para Claude Desktop

1. Localize o arquivo de configuracao do Claude Desktop:
   - Windows: `%APPDATA%\Claude\claude_desktop_config.json`
   - macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
   - Linux: `~/.config/Claude/claude_desktop_config.json`

2. Adicione a configuracao do servidor MCP:

```json
{
    "mcpServers": {
        "fabrica-agentes": {
            "command": "python",
            "args": ["-m", "factory.mcp"],
            "cwd": "C:\\Users\\lcruz\\Fabrica de Agentes"
        }
    }
}
```

3. Reinicie o Claude Desktop

4. Verifique se o servidor esta funcionando: No Claude Desktop, voce devera ver as ferramentas da Fabrica de Agentes disponiveis.

## Ferramentas Disponiveis

### Story Tools (Gerenciamento de User Stories)

| Ferramenta | Descricao |
|------------|-----------|
| `create_story` | Cria uma nova User Story no formato Agile |
| `list_stories` | Lista stories de um projeto com filtros |
| `get_story` | Busca detalhes de uma story com tasks e docs |
| `update_story` | Atualiza campos de uma story |
| `move_story` | Move story no Kanban (muda status) |
| `delete_story` | Deleta uma story e suas tasks |
| `estimate_story` | Estima story points usando IA |

### Task Tools (Gerenciamento de Tasks)

| Ferramenta | Descricao |
|------------|-----------|
| `create_task` | Cria uma nova task em uma story |
| `list_tasks` | Lista tasks de uma story |
| `update_task` | Atualiza campos de uma task |
| `complete_task` | Marca task como completa |

### Worker Tools (Controle de Jobs e Workers)

| Ferramenta | Descricao |
|------------|-----------|
| `create_job` | Cria um novo job para processamento |
| `list_jobs` | Lista jobs na fila |
| `get_job_status` | Consulta status de um job |
| `cancel_job` | Cancela um job pendente |
| `list_workers` | Lista workers registrados |
| `get_queue_stats` | Estatisticas da fila de jobs |

### Project Tools (Gerenciamento de Projetos)

| Ferramenta | Descricao |
|------------|-----------|
| `create_project` | Cria um novo projeto |
| `list_projects` | Lista todos os projetos |
| `get_project` | Busca detalhes de um projeto |
| `update_project` | Atualiza um projeto |

### System Tools (Consultas e Metricas)

| Ferramenta | Descricao |
|------------|-----------|
| `system_health` | Verifica saude do sistema |
| `get_metrics` | Retorna metricas gerais |
| `list_activity_logs` | Lista logs de atividades |

## Exemplos de Uso

### Exemplo 1: Criar uma User Story

```
Crie uma story no projeto PROJ-001:
- Titulo: Implementar sistema de login
- Persona: usuario do sistema
- Acao: fazer login com email e senha
- Beneficio: acessar minhas informacoes de forma segura
- Criterios: validar email, senha minimo 8 caracteres, mensagem de erro clara
- Prioridade: alta
```

O Claude ira usar a ferramenta `create_story` automaticamente.

### Exemplo 2: Criar Tasks para uma Story

```
Para a story STR-20241229-ABCD, crie as seguintes tasks:
1. Criar endpoint de autenticacao (development, 4h)
2. Implementar validacao de senha (development, 2h)
3. Criar testes unitarios (test, 3h)
4. Documentar API (documentation, 1h)
```

### Exemplo 3: Criar um Job de Processamento

```
Crie um job para construir uma API REST de tarefas com:
- Tech stack: Python, FastAPI, SQLite
- Features: CRUD de tarefas, autenticacao JWT, paginacao
```

### Exemplo 4: Consultar Metricas

```
Mostre as metricas atuais do sistema: quantos projetos, stories, tasks e jobs existem?
```

## Uso Programatico (Python)

### Cliente MCP Async

```python
from factory.mcp.client import MCPToolsClient

async def exemplo():
    client = MCPToolsClient()

    # Criar story
    result = await client.create_story(
        project_id="PROJ-001",
        title="Implementar login",
        persona="usuario",
        action="fazer login com email",
        benefit="acessar sistema de forma segura",
        priority="high"
    )
    print(f"Story criada: {result['story_id']}")

    # Listar stories
    stories = await client.list_stories(project_id="PROJ-001")
    for s in stories['stories']:
        print(f"- {s['story_id']}: {s['title']}")

    # Verificar saude do sistema
    health = await client.system_health()
    print(f"Status: {health['status']}")
```

### Cliente MCP Sincrono

```python
from factory.mcp.client import SyncMCPClient

client = SyncMCPClient()

# Criar projeto
result = client.create_project(
    name="Meu Projeto",
    description="Descricao do projeto",
    project_type="web-app"
)
print(f"Projeto criado: {result['project_id']}")
```

### Integracao com Claude API

```python
from anthropic import Anthropic
from factory.mcp.client import (
    get_claude_tools,
    execute_tool_call,
    format_tool_result_for_claude
)

# Obter tools no formato Claude API
tools = get_claude_tools()

# Criar cliente
client = Anthropic()

# Enviar mensagem com tools
response = client.messages.create(
    model="claude-sonnet-4-20250514",
    max_tokens=4096,
    tools=tools,
    messages=[{
        "role": "user",
        "content": "Crie uma story para implementar login no projeto PROJ-001"
    }]
)

# Processar tool_use
for block in response.content:
    if block.type == "tool_use":
        result = await execute_tool_call(block.name, block.input)
        formatted = format_tool_result_for_claude(result)
        print(f"Tool: {block.name}")
        print(f"Result: {formatted}")
```

### Conversa Automatica com Tools

```python
from anthropic import Anthropic
from factory.mcp.client import ClaudeToolsConversation

anthropic = Anthropic()
conv = ClaudeToolsConversation(
    anthropic,
    tool_categories=["story", "task"]  # Apenas story e task tools
)

# Conversa com execucao automatica de tools
result = await conv.chat(
    "Crie uma story e 3 tasks para implementar login no projeto PROJ-001"
)

print(f"Resposta: {result['response']}")
print(f"Tools executadas: {len(result['tool_calls'])}")
for tc in result['tool_calls']:
    print(f"  - {tc['tool']}: {tc['result']['success']}")
```

## CLI

### Testar Ferramentas

```bash
python -m factory.mcp --test
```

### Listar Ferramentas

```bash
python -m factory.mcp --list-tools
```

### Listar em JSON

```bash
python -m factory.mcp --list-tools --json
```

### Iniciar Servidor (modo stdio)

```bash
python -m factory.mcp
```

## Variaveis de Ambiente

| Variavel | Padrao | Descricao |
|----------|--------|-----------|
| `MCP_ENABLED` | `true` | Habilita/desabilita MCP |
| `MCP_STORY_ENABLED` | `true` | Habilita ferramentas de Story |
| `MCP_TASK_ENABLED` | `true` | Habilita ferramentas de Task |
| `MCP_WORKER_ENABLED` | `true` | Habilita ferramentas de Worker |
| `MCP_PROJECT_ENABLED` | `true` | Habilita ferramentas de Project |
| `MCP_SYSTEM_ENABLED` | `true` | Habilita ferramentas de System |

## Arquitetura

```
factory/mcp/
├── __init__.py       # Exports e documentacao do modulo
├── __main__.py       # Entry point CLI
├── server.py         # Servidor MCP (stdio)
├── client.py         # Clientes para uso programatico
├── tools.py          # Definicoes das ferramentas
└── MCP_README.md     # Esta documentacao
```

### Fluxo de Comunicacao

```
Claude Desktop                    MCP Server                      Fabrica de Agentes
     |                                |                                   |
     |--- initialize ---------------->|                                   |
     |<-- capabilities ---------------|                                   |
     |                                |                                   |
     |--- tools/list ---------------->|                                   |
     |<-- tools[] --------------------|                                   |
     |                                |                                   |
     |--- tools/call (create_story)-->|                                   |
     |                                |--- create_story() --------------->|
     |                                |<-- {success, story_id} -----------|
     |<-- result ---------------------|                                   |
```

## Troubleshooting

### Erro: "MCP nao disponivel"

Verifique se o modulo MCP esta instalado corretamente:
```bash
python -c "from factory.mcp import MCPServer; print('OK')"
```

### Erro: "Ferramenta nao encontrada"

Liste as ferramentas disponiveis:
```bash
python -m factory.mcp --list-tools
```

### Erro de conexao com banco de dados

Verifique se o banco SQLite existe:
```bash
python -c "from factory.database.connection import check_db_health; import asyncio; print(asyncio.run(check_db_health()))"
```

### Claude Desktop nao mostra as ferramentas

1. Verifique o arquivo de configuracao
2. Verifique o caminho do projeto no `cwd`
3. Reinicie o Claude Desktop
4. Verifique logs em: `%APPDATA%\Claude\logs\`

## Contribuindo

Para adicionar novas ferramentas:

1. Defina a ferramenta em `tools.py` usando `ToolDefinition`
2. Implemente o handler async
3. Adicione a ferramenta a classe correspondente (`StoryTools`, `TaskTools`, etc.)
4. Atualize esta documentacao

## Licenca

Fabrica de Agentes - Sistema de Desenvolvimento Autonomo com Agentes IA

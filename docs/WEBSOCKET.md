# WebSocket - Plataforma E

Sistema de WebSocket para atualizacoes em tempo real na Plataforma E.

## Visao Geral

O sistema WebSocket permite:
- **Atualizacoes em tempo real** de stories, tasks e notificacoes
- **Grupos por projeto** para receber apenas eventos relevantes
- **Heartbeat automatico** para manter conexoes ativas
- **Reconexao automatica** com backoff exponencial no cliente
- **Streaming de logs** para monitoramento de jobs

## Endpoints WebSocket

### `/ws/notifications`

Endpoint principal para receber todas as notificacoes do sistema.

```javascript
// Conectar
const ws = new WebSocket('ws://localhost:9001/ws/notifications');

// Receber mensagens
ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    console.log('Evento:', data.type, data.data);
};

// Heartbeat
ws.send('ping');

// Inscrever em projeto
ws.send(JSON.stringify({
    action: 'subscribe',
    project_id: 'PROJ-001'
}));
```

### `/ws/project/{project_id}`

Endpoint especifico para eventos de um projeto.

```javascript
const ws = new WebSocket('ws://localhost:9001/ws/project/PROJ-001');

ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    // Recebe apenas eventos do PROJ-001
    console.log('Evento do projeto:', data);
};
```

### `/ws/logs`

Endpoint para streaming de logs em tempo real.

```javascript
const ws = new WebSocket('ws://localhost:9001/ws/logs?level=info');

ws.onmessage = (event) => {
    const log = JSON.parse(event.data);
    if (log.type === 'log_message') {
        console.log(`[${log.data.level}] ${log.data.message}`);
    }
};
```

## Tipos de Eventos

### Eventos de Conexao

| Tipo | Descricao |
|------|-----------|
| `connection` | Conexao estabelecida |
| `heartbeat` | Ping de manutencao do servidor |
| `ping` | Ping enviado pelo cliente |
| `pong` | Resposta ao ping |
| `subscribed` | Inscrito em grupo de projeto |
| `unsubscribed` | Removido de grupo de projeto |

### Eventos de Story

| Tipo | Descricao | Dados |
|------|-----------|-------|
| `story_created` | Nova story criada | `story_id`, `title`, `project_id` |
| `story_updated` | Story atualizada | `story_id`, `title` |
| `story_moved` | Story movida no Kanban | `story_id`, `title`, `to` |
| `story_deleted` | Story removida | `story_id` |

### Eventos de Task

| Tipo | Descricao | Dados |
|------|-----------|-------|
| `task_created` | Nova task criada | `task_id`, `title`, `story_id` |
| `task_updated` | Task atualizada | `task_id`, `title`, `progress` |
| `task_completed` | Task concluida | `task_id`, `title`, `story_id` |
| `task_deleted` | Task removida | `task_id` |

### Eventos de Chat

| Tipo | Descricao | Dados |
|------|-----------|-------|
| `chat_message` | Nova mensagem | `preview`, `project_id`, `story_id` |

### Eventos de Job/Worker

| Tipo | Descricao | Dados |
|------|-----------|-------|
| `job_started` | Job iniciado | `job_id`, `status` |
| `job_progress` | Progresso do job | `job_id`, `progress`, `status`, `output` |
| `job_completed` | Job concluido | `job_id`, `status` |
| `job_failed` | Job falhou | `job_id`, `status`, `error` |

### Eventos de Sistema

| Tipo | Descricao | Dados |
|------|-----------|-------|
| `notification` | Notificacao generica | `title`, `message`, `level` |
| `error` | Erro no sistema | `message`, `code` |
| `log_message` | Mensagem de log | `level`, `message`, `source` |

## Estrutura de Mensagem

Todas as mensagens seguem a estrutura:

```json
{
    "type": "story_created",
    "data": {
        "story_id": "STR-0001",
        "title": "Implementar login",
        "project_id": "PROJ-001"
    },
    "timestamp": "2025-01-15T10:30:00Z",
    "message_id": "abc123",
    "project_id": "PROJ-001",
    "story_id": "STR-0001"
}
```

## Comandos do Cliente

### Ping/Heartbeat

```javascript
// Enviar ping simples
ws.send('ping');

// Ou via JSON
ws.send(JSON.stringify({ action: 'ping' }));
```

### Inscrever em Projeto

```javascript
ws.send(JSON.stringify({
    action: 'subscribe',
    project_id: 'PROJ-001'
}));
```

### Cancelar Inscricao

```javascript
ws.send(JSON.stringify({
    action: 'unsubscribe',
    project_id: 'PROJ-001'
}));
```

### Obter Estatisticas

```javascript
ws.send(JSON.stringify({ action: 'stats' }));
// Resposta: estatisticas do WebSocket manager
```

## Reconexao Automatica (Cliente)

Exemplo de implementacao com backoff exponencial:

```javascript
class WebSocketClient {
    constructor(url) {
        this.url = url;
        this.ws = null;
        this.reconnectDelay = 1000; // Comeca com 1 segundo
        this.maxDelay = 30000; // Maximo 30 segundos
        this.reconnectTimer = null;
    }

    connect() {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
            console.log('Conectado');
            this.reconnectDelay = 1000; // Reset delay
        };

        this.ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            this.handleMessage(data);
        };

        this.ws.onclose = () => {
            console.log('Desconectado, reconectando...');
            this.scheduleReconnect();
        };

        this.ws.onerror = () => {
            this.ws.close();
        };
    }

    scheduleReconnect() {
        if (this.reconnectTimer) {
            clearTimeout(this.reconnectTimer);
        }

        this.reconnectTimer = setTimeout(() => {
            this.connect();
            // Backoff exponencial com limite
            this.reconnectDelay = Math.min(
                this.reconnectDelay * 2,
                this.maxDelay
            );
        }, this.reconnectDelay);
    }

    handleMessage(data) {
        switch (data.type) {
            case 'heartbeat':
                this.ws.send('ping');
                break;
            case 'story_created':
                console.log('Nova story:', data.data);
                break;
            // ... outros eventos
        }
    }

    send(message) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify(message));
        }
    }

    close() {
        if (this.reconnectTimer) {
            clearTimeout(this.reconnectTimer);
        }
        if (this.ws) {
            this.ws.close();
        }
    }
}

// Uso
const client = new WebSocketClient('ws://localhost:9001/ws/notifications');
client.connect();
```

## API REST de WebSocket

### GET `/api/ws/stats`

Retorna estatisticas do WebSocket.

```bash
curl http://localhost:9001/api/ws/stats
```

Resposta:
```json
{
    "active_connections": 5,
    "active_projects": 2,
    "total_connections": 150,
    "total_messages_sent": 1234,
    "total_broadcasts": 89,
    "connections_per_project": {
        "PROJ-001": 3,
        "PROJ-002": 2
    }
}
```

### GET `/api/ws/connections`

Retorna numero de conexoes ativas.

```bash
curl http://localhost:9001/api/ws/connections
```

### GET `/api/ws/project/{project_id}/subscribers`

Retorna numero de inscritos em projeto.

```bash
curl http://localhost:9001/api/ws/project/PROJ-001/subscribers
```

### POST `/api/ws/broadcast`

Envia mensagem broadcast.

```bash
curl -X POST http://localhost:9001/api/ws/broadcast \
  -H "Content-Type: application/json" \
  -d '{
    "event_type": "notification",
    "data": {"title": "Aviso", "message": "Manutencao em 5 minutos"},
    "project_id": "PROJ-001"
  }'
```

## Integracao com Python

### Uso no Backend

```python
from factory.websocket import ws_manager, EventType

# Notificar story criada
ws_manager.notify_story_created(
    story_id="STR-0001",
    title="Implementar login",
    project_id="PROJ-001"
)

# Notificar task completada
ws_manager.notify_task_completed(
    task_id="STSK-0001",
    title="Criar endpoint",
    story_id="STR-0001"
)

# Broadcast para projeto
await ws_manager.send_to_project(
    project_id="PROJ-001",
    event_type=EventType.NOTIFICATION,
    data={"title": "Aviso", "message": "Deploy concluido"}
)

# Log em tempo real
ws_manager.notify_log("info", "Build iniciado", source="CI/CD")
```

### Integracao com FastAPI

```python
from fastapi import FastAPI
from factory.websocket import router as ws_router

app = FastAPI()
app.include_router(ws_router)
```

## Configuracoes

| Variavel | Descricao | Padrao |
|----------|-----------|--------|
| `WEBSOCKET_HEARTBEAT_INTERVAL` | Intervalo de heartbeat (segundos) | 30 |
| `WEBSOCKET_TIMEOUT` | Timeout para desconexao (segundos) | 90 |
| `WEBSOCKET_MAX_CONNECTIONS` | Limite de conexoes simultaneas | 1000 |
| `WEBSOCKET_MAX_MESSAGE_SIZE` | Tamanho maximo de mensagem (bytes) | 65536 |

## Troubleshooting

### Conexao Fecha Imediatamente

1. Verifique se o servidor esta rodando
2. Confirme a URL (ws:// ou wss://)
3. Verifique CORS se usando HTTPS

### Mensagens Nao Chegam

1. Verifique se esta inscrito no projeto correto
2. Confirme o tipo de evento esperado
3. Use `/api/ws/stats` para verificar conexoes

### Performance

1. Use grupos por projeto para reduzir trafego
2. Implemente debounce no cliente para eventos frequentes
3. Monitore `/api/ws/stats` para identificar problemas

---

*Plataforma E v6.5 - WebSocket para Atualizacoes em Tempo Real*

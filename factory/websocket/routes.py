# -*- coding: utf-8 -*-
"""
WebSocket Routes - Fabrica de Agentes
=====================================
Endpoints WebSocket para integracao com FastAPI.

Endpoints disponiveis:
- /ws/notifications: Notificacoes em tempo real
- /ws/project/{project_id}: Eventos de projeto especifico
- /ws/logs: Streaming de logs

Uso:
    from factory.websocket.routes import router
    app.include_router(router)
"""

import json
import asyncio
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, WebSocket, WebSocketDisconnect, Query

from .manager import ws_manager
from .events import (
    EventType,
    create_connection_event,
    create_pong_event,
    create_heartbeat_event,
    WebSocketEvent
)

router = APIRouter(tags=["WebSocket"])


@router.websocket("/ws/notifications")
async def websocket_notifications(
    websocket: WebSocket,
    client_id: Optional[str] = Query(None)
):
    """
    WebSocket endpoint principal para notificacoes em tempo real.

    Conecte-se a este endpoint para receber todas as notificacoes do sistema.

    Mensagens suportadas do cliente:
    - "ping": Responde com pong (heartbeat)
    - {"action": "subscribe", "project_id": "..."}: Inscreve em projeto
    - {"action": "unsubscribe", "project_id": "..."}: Remove inscricao
    - {"action": "stats"}: Retorna estatisticas do WebSocket

    Eventos enviados pelo servidor:
    - connection: Conexao estabelecida
    - heartbeat: Ping de manutencao
    - pong: Resposta ao ping do cliente
    - story_created, story_updated, story_moved, story_deleted
    - task_created, task_updated, task_completed, task_deleted
    - chat_message
    - notification
    - job_progress, job_started, job_completed, job_failed
    - log_message

    Args:
        websocket: Conexao WebSocket
        client_id: ID opcional do cliente (query param)

    Exemplo JavaScript:
        const ws = new WebSocket('ws://localhost:9001/ws/notifications');
        ws.onmessage = (e) => console.log(JSON.parse(e.data));
        ws.send('ping'); // Heartbeat
        ws.send(JSON.stringify({action: 'subscribe', project_id: 'PROJ-001'}));
    """
    # Conectar
    conn_info = await ws_manager.connect(websocket, client_id)
    actual_client_id = conn_info.client_id

    try:
        # Enviar evento de conexao
        event = create_connection_event(
            status="connected",
            message="Conectado ao servidor de notificacoes em tempo real"
        )
        await websocket.send_json(event.to_dict())

        # Loop de mensagens
        while True:
            try:
                # Aguardar mensagem com timeout para heartbeat
                data = await asyncio.wait_for(
                    websocket.receive_text(),
                    timeout=ws_manager.HEARTBEAT_INTERVAL
                )

                # Processar mensagem
                response = await ws_manager.handle_message(actual_client_id, data)

                if response:
                    await websocket.send_json(response)

            except asyncio.TimeoutError:
                # Timeout - enviar heartbeat
                try:
                    event = create_heartbeat_event()
                    await websocket.send_json(event.to_dict())
                except:
                    break

            except WebSocketDisconnect:
                break

            except Exception as e:
                # Log erro mas continua
                print(f"[WebSocket] Erro processando mensagem: {e}")
                continue

    except WebSocketDisconnect:
        pass

    finally:
        ws_manager.disconnect(websocket)


@router.websocket("/ws/project/{project_id}")
async def websocket_project(
    websocket: WebSocket,
    project_id: str,
    client_id: Optional[str] = Query(None)
):
    """
    WebSocket endpoint para eventos de projeto especifico.

    Conecte-se a este endpoint para receber apenas eventos de um projeto.
    Automaticamente inscrito no grupo do projeto.

    Args:
        websocket: Conexao WebSocket
        project_id: ID do projeto
        client_id: ID opcional do cliente

    Eventos enviados:
    - Todos os eventos do projeto especificado
    - connection, heartbeat, pong

    Exemplo JavaScript:
        const ws = new WebSocket('ws://localhost:9001/ws/project/PROJ-001');
        ws.onmessage = (e) => {
            const event = JSON.parse(e.data);
            if (event.project_id === 'PROJ-001') {
                console.log('Evento do projeto:', event);
            }
        };
    """
    # Conectar
    conn_info = await ws_manager.connect(websocket, client_id)
    actual_client_id = conn_info.client_id

    try:
        # Inscrever no projeto automaticamente
        await ws_manager.subscribe_to_project(actual_client_id, project_id)

        # Enviar evento de conexao
        event = WebSocketEvent(
            type=EventType.CONNECTION,
            data={
                "status": "connected",
                "message": f"Conectado ao projeto {project_id}",
                "project_id": project_id
            },
            project_id=project_id
        )
        await websocket.send_json(event.to_dict())

        # Loop de mensagens
        while True:
            try:
                data = await asyncio.wait_for(
                    websocket.receive_text(),
                    timeout=ws_manager.HEARTBEAT_INTERVAL
                )

                # Processar ping
                if data == "ping":
                    await ws_manager.handle_ping(actual_client_id)
                    continue

                # Processar JSON
                try:
                    msg = json.loads(data)
                    action = msg.get("action", "")

                    if action == "ping":
                        await ws_manager.handle_ping(actual_client_id)

                except json.JSONDecodeError:
                    pass

            except asyncio.TimeoutError:
                # Heartbeat
                try:
                    event = create_heartbeat_event()
                    await websocket.send_json(event.to_dict())
                except:
                    break

            except WebSocketDisconnect:
                break

            except:
                continue

    except WebSocketDisconnect:
        pass

    finally:
        # Remover inscricao e desconectar
        await ws_manager.unsubscribe_from_project(actual_client_id, project_id)
        ws_manager.disconnect(websocket)


@router.websocket("/ws/logs")
async def websocket_logs(
    websocket: WebSocket,
    level: Optional[str] = Query("info"),
    source: Optional[str] = Query(None)
):
    """
    WebSocket endpoint para streaming de logs.

    Conecte-se a este endpoint para receber logs em tempo real.
    Util para monitorar jobs, builds e processos.

    Args:
        websocket: Conexao WebSocket
        level: Nivel minimo de log (debug, info, warning, error)
        source: Filtro por fonte de log (opcional)

    Eventos enviados:
    - log_message: Mensagem de log
    - connection: Conexao estabelecida

    Exemplo JavaScript:
        const ws = new WebSocket('ws://localhost:9001/ws/logs?level=info');
        ws.onmessage = (e) => {
            const log = JSON.parse(e.data);
            if (log.type === 'log_message') {
                console.log(`[${log.data.level}] ${log.data.message}`);
            }
        };
    """
    await websocket.accept()

    try:
        # Enviar evento de conexao
        event = WebSocketEvent(
            type=EventType.CONNECTION,
            data={
                "status": "connected",
                "message": "Streaming de logs iniciado",
                "level_filter": level,
                "source_filter": source
            }
        )
        await websocket.send_json(event.to_dict())

        # Loop - apenas mantem conexao aberta
        # Logs sao enviados via broadcast do manager
        while True:
            try:
                data = await asyncio.wait_for(
                    websocket.receive_text(),
                    timeout=60
                )

                if data == "ping":
                    await websocket.send_json(create_pong_event().to_dict())

            except asyncio.TimeoutError:
                # Manter vivo
                try:
                    await websocket.send_json(create_heartbeat_event().to_dict())
                except:
                    break

            except WebSocketDisconnect:
                break

            except:
                continue

    except WebSocketDisconnect:
        pass


# =============================================================================
# REST ENDPOINTS PARA WEBSOCKET
# =============================================================================

from fastapi import HTTPException


@router.get("/api/ws/stats")
async def get_websocket_stats():
    """
    Retorna estatisticas do WebSocket manager.

    Returns:
        Dicionario com:
        - active_connections: Numero de conexoes ativas
        - active_projects: Numero de projetos com inscricoes
        - total_connections: Total de conexoes historicas
        - total_messages_sent: Total de mensagens enviadas
        - total_broadcasts: Total de broadcasts realizados
    """
    return ws_manager.get_stats()


@router.get("/api/ws/connections")
async def get_connections_count():
    """Retorna numero de conexoes ativas."""
    return {"count": ws_manager.get_connection_count()}


@router.get("/api/ws/project/{project_id}/subscribers")
async def get_project_subscribers(project_id: str):
    """Retorna numero de inscritos em projeto."""
    return {
        "project_id": project_id,
        "subscribers": ws_manager.get_project_subscribers(project_id)
    }


@router.post("/api/ws/broadcast")
async def broadcast_message(
    event_type: str,
    data: dict,
    project_id: Optional[str] = None
):
    """
    Envia mensagem broadcast via WebSocket.

    Args:
        event_type: Tipo do evento (ex: notification)
        data: Dados do evento
        project_id: Se especificado, envia apenas para inscritos no projeto

    Returns:
        Numero de clientes que receberam a mensagem
    """
    try:
        event_enum = EventType(event_type)
    except ValueError:
        event_enum = EventType.NOTIFICATION

    count = await ws_manager.broadcast_event(event_enum, data, project_id)
    return {"sent_to": count}

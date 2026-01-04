# -*- coding: utf-8 -*-
"""
WebSocket Routes - Plataforma E
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
from .collaboration import collab_manager, UserStatus, EventType as CollabEventType

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


# =============================================================================
# COLABORACAO EM TEMPO REAL - Issue #242
# =============================================================================

@router.websocket("/ws/collaboration/{project_id}")
async def websocket_collaboration(
    websocket: WebSocket,
    project_id: str,
    user_id: str = Query(...),
    username: str = Query(...),
    avatar_url: Optional[str] = Query(None)
):
    """
    WebSocket endpoint para colaboracao em tempo real estilo Figma/Google Docs.

    Permite:
    - Ver quem esta online no projeto
    - Cursores em tempo real de outros usuarios
    - Indicadores de quem esta visualizando/editando cada story
    - Locks otimistas para evitar conflitos de edicao

    Args:
        websocket: Conexao WebSocket
        project_id: ID do projeto
        user_id: ID do usuario
        username: Nome do usuario
        avatar_url: URL do avatar (opcional)

    Mensagens do cliente:
    - {"type": "cursor_move", "x": 100, "y": 200, "element_id": "story-123"}
    - {"type": "view_start", "entity_type": "story", "entity_id": "STR-001"}
    - {"type": "view_end"}
    - {"type": "edit_start", "entity_type": "story", "entity_id": "STR-001"}
    - {"type": "edit_end", "entity_type": "story", "entity_id": "STR-001"}
    - {"type": "status_change", "status": "away|busy|online"}
    - "ping"

    Eventos do servidor:
    - {"type": "sync", "users": [...], "locks": [...]}
    - {"type": "join", "user": {...}}
    - {"type": "leave", "user": {...}}
    - {"type": "cursor_move", "user_id": "...", "cursor": {...}}
    - {"type": "view_start", "user_id": "...", "entity_type": "...", "entity_id": "..."}
    - {"type": "edit_start", "user_id": "...", "lock": {...}}
    - {"type": "edit_conflict", "lock": {...}, "message": "..."}

    Exemplo JavaScript:
        const ws = new WebSocket(
            'ws://localhost:9001/ws/collaboration/PROJ-001?user_id=user1&username=Joao'
        );
        ws.onmessage = (e) => {
            const msg = JSON.parse(e.data);
            if (msg.type === 'cursor_move') {
                updateCursor(msg.user_id, msg.cursor);
            }
        };
        // Mover cursor
        ws.send(JSON.stringify({type: 'cursor_move', x: 100, y: 200}));
    """
    await websocket.accept()

    try:
        # Entrar no room
        user_presence = await collab_manager.join_room(
            websocket, project_id, user_id, username, avatar_url
        )

        # Loop de mensagens
        while True:
            try:
                data = await asyncio.wait_for(
                    websocket.receive_text(),
                    timeout=30  # Heartbeat a cada 30s
                )

                # Ping simples
                if data == "ping":
                    await websocket.send_json({
                        "type": CollabEventType.PONG.value,
                        "timestamp": datetime.utcnow().isoformat()
                    })
                    continue

                # Parse JSON
                try:
                    msg = json.loads(data)
                    msg_type = msg.get("type", "")

                    if msg_type == "ping":
                        await websocket.send_json({
                            "type": CollabEventType.PONG.value,
                            "timestamp": datetime.utcnow().isoformat()
                        })

                    elif msg_type == "cursor_move":
                        await collab_manager.update_cursor(
                            websocket,
                            x=msg.get("x", 0),
                            y=msg.get("y", 0),
                            element_id=msg.get("element_id")
                        )

                    elif msg_type == "view_start":
                        await collab_manager.start_viewing(
                            websocket,
                            entity_type=msg.get("entity_type", "story"),
                            entity_id=msg.get("entity_id", "")
                        )

                    elif msg_type == "view_end":
                        await collab_manager.stop_viewing(websocket)

                    elif msg_type == "edit_start":
                        success, lock = await collab_manager.acquire_lock(
                            websocket,
                            entity_type=msg.get("entity_type", "story"),
                            entity_id=msg.get("entity_id", "")
                        )
                        if success:
                            await websocket.send_json({
                                "type": "lock_acquired",
                                "lock": lock.to_dict() if lock else None
                            })

                    elif msg_type == "edit_end":
                        await collab_manager.release_lock(
                            websocket,
                            entity_type=msg.get("entity_type", "story"),
                            entity_id=msg.get("entity_id", "")
                        )

                    elif msg_type == "status_change":
                        status_str = msg.get("status", "online")
                        try:
                            status = UserStatus(status_str)
                            await collab_manager.update_status(websocket, status)
                        except ValueError:
                            pass

                except json.JSONDecodeError:
                    pass

            except asyncio.TimeoutError:
                # Enviar heartbeat
                try:
                    await websocket.send_json({
                        "type": CollabEventType.PING.value,
                        "timestamp": datetime.utcnow().isoformat()
                    })
                except:
                    break

            except WebSocketDisconnect:
                break

            except Exception as e:
                print(f"[Collaboration] Erro: {e}")
                continue

    except WebSocketDisconnect:
        pass

    finally:
        await collab_manager.leave_room(websocket)


# =============================================================================
# REST ENDPOINTS PARA COLABORACAO - Issue #242
# =============================================================================

@router.get("/api/collaboration/{project_id}/presence")
async def get_project_presence(project_id: str):
    """
    Retorna lista de usuarios online no projeto.

    Returns:
        Lista com informacoes de presenca de cada usuario
    """
    return {
        "project_id": project_id,
        "users": collab_manager.get_room_presence(project_id)
    }


@router.get("/api/collaboration/{project_id}/viewers/{entity_type}/{entity_id}")
async def get_entity_viewers(project_id: str, entity_type: str, entity_id: str):
    """
    Retorna usuarios visualizando uma entidade especifica.

    Args:
        project_id: ID do projeto
        entity_type: Tipo da entidade (story, task)
        entity_id: ID da entidade

    Returns:
        Lista de usuarios visualizando
    """
    return {
        "entity_type": entity_type,
        "entity_id": entity_id,
        "viewers": collab_manager.get_viewers(entity_type, entity_id)
    }


@router.get("/api/collaboration/{project_id}/lock/{entity_type}/{entity_id}")
async def get_entity_lock(project_id: str, entity_type: str, entity_id: str):
    """
    Retorna lock de edicao de uma entidade.

    Args:
        project_id: ID do projeto
        entity_type: Tipo da entidade (story, task)
        entity_id: ID da entidade

    Returns:
        Lock ativo ou null se nao houver
    """
    lock = collab_manager.get_lock(entity_type, entity_id)
    return {
        "entity_type": entity_type,
        "entity_id": entity_id,
        "lock": lock,
        "is_locked": lock is not None
    }

# -*- coding: utf-8 -*-
"""
WebSocket Manager - Fabrica de Agentes
======================================
Gerenciador de conexoes WebSocket com suporte a:
- Broadcast de eventos para todos os clientes
- Grupos por projeto para mensagens direcionadas
- Heartbeat/ping para manter conexoes ativas
- Reconexao automatica com backoff exponencial
- Estatisticas de conexoes

Uso:
    from factory.websocket import ws_manager, EventType

    # Broadcast para todos
    await ws_manager.broadcast_event(EventType.NOTIFICATION, {"message": "Ola!"})

    # Mensagem para grupo de projeto
    await ws_manager.send_to_project("PROJ-001", EventType.STORY_CREATED, {...})

    # Notificar story criada
    ws_manager.notify_story_created("STR-001", "Titulo", "PROJ-001")
"""

import asyncio
import logging
from datetime import datetime
from typing import Dict, List, Set, Optional, Any
from dataclasses import dataclass, field
from collections import defaultdict

from fastapi import WebSocket, WebSocketDisconnect

from .events import (
    WebSocketEvent,
    EventType,
    create_connection_event,
    create_heartbeat_event,
    create_pong_event,
    create_story_event,
    create_task_event,
    create_chat_event,
    create_notification_event,
    create_error_event,
    create_job_event,
    create_log_event
)

# Configurar logging
logger = logging.getLogger(__name__)


@dataclass
class ConnectionInfo:
    """
    Informacoes sobre uma conexao WebSocket.

    Atributos:
        websocket: Objeto WebSocket
        connected_at: Data/hora da conexao
        last_ping: Ultimo ping recebido
        subscribed_projects: Projetos inscritos
        client_id: ID unico do cliente
        user_agent: User-Agent do cliente
    """
    websocket: WebSocket
    connected_at: datetime = field(default_factory=datetime.utcnow)
    last_ping: datetime = field(default_factory=datetime.utcnow)
    subscribed_projects: Set[str] = field(default_factory=set)
    client_id: str = ""
    user_agent: str = ""
    missed_heartbeats: int = 0

    def __post_init__(self):
        if not self.client_id:
            import uuid
            self.client_id = str(uuid.uuid4())[:8]


class WebSocketManager:
    """
    Gerenciador centralizado de conexoes WebSocket.

    Funcionalidades:
    - Gerencia multiplas conexoes simultaneas
    - Suporta grupos por projeto para broadcast direcionado
    - Heartbeat automatico para detectar conexoes mortas
    - Estatisticas de conexoes e mensagens
    - Thread-safe para uso com asyncio

    Configuracoes:
    - HEARTBEAT_INTERVAL: Intervalo entre heartbeats (segundos)
    - HEARTBEAT_TIMEOUT: Tempo maximo sem resposta antes de desconectar
    - MAX_CONNECTIONS: Limite maximo de conexoes simultaneas
    """

    # Configuracoes
    HEARTBEAT_INTERVAL = 30  # segundos
    HEARTBEAT_TIMEOUT = 90   # segundos (3 heartbeats perdidos)
    MAX_CONNECTIONS = 1000
    MAX_MESSAGE_SIZE = 65536  # 64KB

    def __init__(self):
        """Inicializa o gerenciador de WebSocket."""
        # Mapa de conexoes: client_id -> ConnectionInfo
        self._connections: Dict[str, ConnectionInfo] = {}

        # Grupos por projeto: project_id -> Set[client_id]
        self._project_groups: Dict[str, Set[str]] = defaultdict(set)

        # Lista de conexoes ativas (para compatibilidade)
        self.active_connections: List[WebSocket] = []

        # Estatisticas
        self._stats = {
            "total_connections": 0,
            "total_messages_sent": 0,
            "total_messages_received": 0,
            "total_broadcasts": 0,
            "started_at": datetime.utcnow().isoformat()
        }

        # Lock para operacoes thread-safe
        self._lock = asyncio.Lock()

        # Task de heartbeat
        self._heartbeat_task: Optional[asyncio.Task] = None

        logger.info("[WebSocket] Manager inicializado")

    # =========================================================================
    # GERENCIAMENTO DE CONEXOES
    # =========================================================================

    async def connect(self, websocket: WebSocket, client_id: Optional[str] = None) -> ConnectionInfo:
        """
        Aceita nova conexao WebSocket.

        Args:
            websocket: Objeto WebSocket
            client_id: ID do cliente (opcional, gerado automaticamente)

        Returns:
            ConnectionInfo com dados da conexao

        Raises:
            Exception: Se limite de conexoes atingido
        """
        async with self._lock:
            # Verificar limite de conexoes
            if len(self._connections) >= self.MAX_CONNECTIONS:
                await websocket.close(code=1013, reason="Limite de conexoes atingido")
                raise Exception("Limite de conexoes atingido")

            # Aceitar conexao
            await websocket.accept()

            # Criar info da conexao
            user_agent = ""
            try:
                headers = dict(websocket.headers)
                user_agent = headers.get("user-agent", "")[:200]
            except:
                pass

            conn_info = ConnectionInfo(
                websocket=websocket,
                client_id=client_id or "",
                user_agent=user_agent
            )

            # Registrar conexao
            self._connections[conn_info.client_id] = conn_info
            self.active_connections.append(websocket)
            self._stats["total_connections"] += 1

            logger.info(f"[WebSocket] Nova conexao: {conn_info.client_id} (total: {len(self._connections)})")

            # Enviar evento de conexao
            try:
                event = create_connection_event(
                    status="connected",
                    message="Conectado ao servidor de notificacoes em tempo real"
                )
                await websocket.send_json(event.to_dict())
            except Exception as e:
                logger.warning(f"[WebSocket] Erro ao enviar evento de conexao: {e}")

            return conn_info

    def disconnect(self, websocket: WebSocket) -> Optional[str]:
        """
        Remove conexao WebSocket.

        Args:
            websocket: Objeto WebSocket

        Returns:
            client_id da conexao removida ou None
        """
        client_id = None

        # Encontrar e remover conexao
        for cid, conn_info in list(self._connections.items()):
            if conn_info.websocket == websocket:
                client_id = cid
                break

        if client_id:
            # Remover de grupos de projeto
            for project_id in list(self._project_groups.keys()):
                self._project_groups[project_id].discard(client_id)
                if not self._project_groups[project_id]:
                    del self._project_groups[project_id]

            # Remover conexao
            del self._connections[client_id]
            logger.info(f"[WebSocket] Conexao removida: {client_id} (total: {len(self._connections)})")

        # Remover da lista de conexoes ativas
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)

        return client_id

    async def disconnect_async(self, websocket: WebSocket) -> Optional[str]:
        """Versao async do disconnect para uso em contextos async."""
        async with self._lock:
            return self.disconnect(websocket)

    # =========================================================================
    # SUBSCRICAO EM GRUPOS (PROJETOS)
    # =========================================================================

    async def subscribe_to_project(self, client_id: str, project_id: str) -> bool:
        """
        Inscreve cliente em grupo de projeto.

        Args:
            client_id: ID do cliente
            project_id: ID do projeto

        Returns:
            True se inscrito com sucesso
        """
        async with self._lock:
            if client_id not in self._connections:
                return False

            conn_info = self._connections[client_id]
            conn_info.subscribed_projects.add(project_id)
            self._project_groups[project_id].add(client_id)

            logger.debug(f"[WebSocket] {client_id} inscrito em {project_id}")

            # Enviar confirmacao
            try:
                event = WebSocketEvent(
                    type=EventType.SUBSCRIBED,
                    data={"project_id": project_id, "message": f"Inscrito no projeto {project_id}"},
                    project_id=project_id
                )
                await conn_info.websocket.send_json(event.to_dict())
            except:
                pass

            return True

    async def unsubscribe_from_project(self, client_id: str, project_id: str) -> bool:
        """
        Remove inscricao do cliente em grupo de projeto.

        Args:
            client_id: ID do cliente
            project_id: ID do projeto

        Returns:
            True se removido com sucesso
        """
        async with self._lock:
            if client_id not in self._connections:
                return False

            conn_info = self._connections[client_id]
            conn_info.subscribed_projects.discard(project_id)
            self._project_groups[project_id].discard(client_id)

            if not self._project_groups[project_id]:
                del self._project_groups[project_id]

            logger.debug(f"[WebSocket] {client_id} removido de {project_id}")

            # Enviar confirmacao
            try:
                event = WebSocketEvent(
                    type=EventType.UNSUBSCRIBED,
                    data={"project_id": project_id, "message": f"Removido do projeto {project_id}"},
                    project_id=project_id
                )
                await conn_info.websocket.send_json(event.to_dict())
            except:
                pass

            return True

    # =========================================================================
    # BROADCAST E ENVIO DE MENSAGENS
    # =========================================================================

    async def broadcast(self, message: dict) -> int:
        """
        Envia mensagem para todas as conexoes ativas.
        Compativel com a interface anterior.

        Args:
            message: Dicionario com a mensagem

        Returns:
            Numero de clientes que receberam a mensagem
        """
        sent_count = 0
        failed_connections = []

        for conn_info in list(self._connections.values()):
            try:
                await conn_info.websocket.send_json(message)
                sent_count += 1
            except Exception as e:
                logger.warning(f"[WebSocket] Erro ao enviar para {conn_info.client_id}: {e}")
                failed_connections.append(conn_info.websocket)

        # Remover conexoes com falha
        for ws in failed_connections:
            self.disconnect(ws)

        self._stats["total_messages_sent"] += sent_count
        self._stats["total_broadcasts"] += 1

        return sent_count

    async def broadcast_event(
        self,
        event_type: EventType,
        data: Dict[str, Any],
        project_id: Optional[str] = None,
        exclude_client: Optional[str] = None
    ) -> int:
        """
        Envia evento para todas as conexoes ou grupo de projeto.

        Args:
            event_type: Tipo do evento
            data: Dados do evento
            project_id: Se especificado, envia apenas para inscritos no projeto
            exclude_client: ID do cliente a excluir do broadcast

        Returns:
            Numero de clientes que receberam
        """
        event = WebSocketEvent(
            type=event_type,
            data=data,
            project_id=project_id
        )

        return await self._send_event(event, project_id, exclude_client)

    async def send_to_project(
        self,
        project_id: str,
        event_type: EventType,
        data: Dict[str, Any],
        exclude_client: Optional[str] = None
    ) -> int:
        """
        Envia evento apenas para clientes inscritos no projeto.

        Args:
            project_id: ID do projeto
            event_type: Tipo do evento
            data: Dados do evento
            exclude_client: Cliente a excluir

        Returns:
            Numero de clientes que receberam
        """
        return await self.broadcast_event(event_type, data, project_id, exclude_client)

    async def send_to_client(self, client_id: str, event: WebSocketEvent) -> bool:
        """
        Envia evento para cliente especifico.

        Args:
            client_id: ID do cliente
            event: Evento a enviar

        Returns:
            True se enviado com sucesso
        """
        if client_id not in self._connections:
            return False

        try:
            await self._connections[client_id].websocket.send_json(event.to_dict())
            self._stats["total_messages_sent"] += 1
            return True
        except Exception as e:
            logger.warning(f"[WebSocket] Erro ao enviar para {client_id}: {e}")
            self.disconnect(self._connections[client_id].websocket)
            return False

    async def _send_event(
        self,
        event: WebSocketEvent,
        project_id: Optional[str] = None,
        exclude_client: Optional[str] = None
    ) -> int:
        """Envia evento internamente."""
        sent_count = 0
        failed_connections = []

        # Determinar destinatarios
        if project_id and project_id in self._project_groups:
            # Apenas clientes do projeto
            target_clients = self._project_groups[project_id]
        else:
            # Todos os clientes
            target_clients = set(self._connections.keys())

        # Excluir cliente se especificado
        if exclude_client:
            target_clients = target_clients - {exclude_client}

        # Enviar para cada cliente
        for client_id in target_clients:
            if client_id not in self._connections:
                continue

            conn_info = self._connections[client_id]
            try:
                await conn_info.websocket.send_json(event.to_dict())
                sent_count += 1
            except Exception as e:
                logger.warning(f"[WebSocket] Erro ao enviar para {client_id}: {e}")
                failed_connections.append(conn_info.websocket)

        # Remover conexoes com falha
        for ws in failed_connections:
            self.disconnect(ws)

        self._stats["total_messages_sent"] += sent_count

        return sent_count

    # =========================================================================
    # HEARTBEAT / PING-PONG
    # =========================================================================

    async def start_heartbeat(self):
        """Inicia tarefa de heartbeat em background."""
        if self._heartbeat_task and not self._heartbeat_task.done():
            return

        self._heartbeat_task = asyncio.create_task(self._heartbeat_loop())
        logger.info("[WebSocket] Heartbeat iniciado")

    async def stop_heartbeat(self):
        """Para tarefa de heartbeat."""
        if self._heartbeat_task:
            self._heartbeat_task.cancel()
            try:
                await self._heartbeat_task
            except asyncio.CancelledError:
                pass
            logger.info("[WebSocket] Heartbeat parado")

    async def _heartbeat_loop(self):
        """Loop de heartbeat para verificar conexoes."""
        while True:
            try:
                await asyncio.sleep(self.HEARTBEAT_INTERVAL)
                await self._send_heartbeat()
                await self._check_dead_connections()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"[WebSocket] Erro no heartbeat: {e}")

    async def _send_heartbeat(self):
        """Envia heartbeat para todas as conexoes."""
        event = create_heartbeat_event()
        await self.broadcast(event.to_dict())

    async def _check_dead_connections(self):
        """Remove conexoes que nao respondem."""
        now = datetime.utcnow()
        dead_connections = []

        for client_id, conn_info in list(self._connections.items()):
            # Verificar tempo desde ultimo ping
            time_since_ping = (now - conn_info.last_ping).total_seconds()

            if time_since_ping > self.HEARTBEAT_TIMEOUT:
                conn_info.missed_heartbeats += 1

                if conn_info.missed_heartbeats >= 3:
                    dead_connections.append(conn_info.websocket)
                    logger.info(f"[WebSocket] Conexao morta detectada: {client_id}")

        # Remover conexoes mortas
        for ws in dead_connections:
            try:
                await ws.close(code=1001, reason="Heartbeat timeout")
            except:
                pass
            self.disconnect(ws)

    async def handle_ping(self, client_id: str) -> bool:
        """
        Processa ping recebido do cliente.

        Args:
            client_id: ID do cliente

        Returns:
            True se processado com sucesso
        """
        if client_id not in self._connections:
            return False

        conn_info = self._connections[client_id]
        conn_info.last_ping = datetime.utcnow()
        conn_info.missed_heartbeats = 0

        # Enviar pong
        try:
            event = create_pong_event()
            await conn_info.websocket.send_json(event.to_dict())
            return True
        except:
            return False

    async def handle_message(self, client_id: str, message: str) -> Optional[Dict[str, Any]]:
        """
        Processa mensagem recebida do cliente.

        Args:
            client_id: ID do cliente
            message: Mensagem recebida (texto ou JSON)

        Returns:
            Resposta a enviar ou None
        """
        self._stats["total_messages_received"] += 1

        # Ping simples
        if message == "ping":
            await self.handle_ping(client_id)
            return None

        # Tentar parsear JSON
        try:
            import json
            data = json.loads(message)

            action = data.get("action") or data.get("type", "")

            # Acoes conhecidas
            if action == "ping":
                await self.handle_ping(client_id)
                return None

            elif action == "subscribe":
                project_id = data.get("project_id")
                if project_id:
                    await self.subscribe_to_project(client_id, project_id)
                return None

            elif action == "unsubscribe":
                project_id = data.get("project_id")
                if project_id:
                    await self.unsubscribe_from_project(client_id, project_id)
                return None

            elif action == "stats":
                return self.get_stats()

        except json.JSONDecodeError:
            pass

        return None

    # =========================================================================
    # METODOS DE NOTIFICACAO CONVENIENTES
    # =========================================================================

    def notify(self, notification_type: str, data: dict):
        """
        Metodo de compatibilidade para notificar eventos.
        Cria task asyncio para broadcast.

        Args:
            notification_type: Tipo da notificacao
            data: Dados da notificacao
        """
        message = {
            "type": notification_type,
            "data": data,
            "timestamp": datetime.utcnow().isoformat() + "Z"
        }
        try:
            asyncio.create_task(self.broadcast(message))
        except RuntimeError:
            # Nao ha event loop ativo
            pass

    def notify_story_created(self, story_id: str, title: str, project_id: str):
        """Notifica criacao de story."""
        self.notify("story_created", {
            "story_id": story_id,
            "title": title,
            "project_id": project_id
        })

    def notify_story_updated(self, story_id: str, title: str, project_id: Optional[str] = None):
        """Notifica atualizacao de story."""
        data = {"story_id": story_id, "title": title}
        if project_id:
            data["project_id"] = project_id
        self.notify("story_updated", data)

    def notify_story_moved(self, story_id: str, title: str, to_status: str, project_id: Optional[str] = None):
        """Notifica movimentacao de story no Kanban."""
        data = {"story_id": story_id, "title": title, "to": to_status}
        if project_id:
            data["project_id"] = project_id
        self.notify("story_moved", data)

    def notify_story_deleted(self, story_id: str, project_id: Optional[str] = None):
        """Notifica remocao de story."""
        data = {"story_id": story_id}
        if project_id:
            data["project_id"] = project_id
        self.notify("story_deleted", data)

    def notify_task_created(self, task_id: str, title: str, story_id: str):
        """Notifica criacao de task."""
        self.notify("task_created", {
            "task_id": task_id,
            "title": title,
            "story_id": story_id
        })

    def notify_task_updated(self, task_id: str, title: str, story_id: str, progress: Optional[int] = None):
        """Notifica atualizacao de task."""
        data = {"task_id": task_id, "title": title, "story_id": story_id}
        if progress is not None:
            data["progress"] = progress
        self.notify("task_updated", data)

    def notify_task_completed(self, task_id: str, title: str, story_id: str):
        """Notifica conclusao de task."""
        self.notify("task_completed", {
            "task_id": task_id,
            "title": title,
            "story_id": story_id
        })

    def notify_chat_message(self, preview: str, project_id: Optional[str] = None, story_id: Optional[str] = None):
        """Notifica nova mensagem de chat."""
        data = {"preview": preview[:100]}
        if project_id:
            data["project_id"] = project_id
        if story_id:
            data["story_id"] = story_id
        self.notify("chat_message", data)

    def notify_job_progress(self, job_id: str, progress: int, status: str, output: Optional[str] = None):
        """Notifica progresso de job."""
        data = {"job_id": job_id, "progress": progress, "status": status}
        if output:
            data["output"] = output[:500]
        self.notify("job_progress", data)

    def notify_log(self, level: str, message: str, source: Optional[str] = None):
        """Notifica mensagem de log."""
        data = {"level": level, "message": message}
        if source:
            data["source"] = source
        self.notify("log_message", data)

    # =========================================================================
    # ESTATISTICAS E INFORMACOES
    # =========================================================================

    def get_stats(self) -> Dict[str, Any]:
        """
        Retorna estatisticas do WebSocket manager.

        Returns:
            Dicionario com estatisticas
        """
        return {
            **self._stats,
            "active_connections": len(self._connections),
            "active_projects": len(self._project_groups),
            "connections_per_project": {
                pid: len(clients)
                for pid, clients in self._project_groups.items()
            }
        }

    def get_connection_count(self) -> int:
        """Retorna numero de conexoes ativas."""
        return len(self._connections)

    def get_project_subscribers(self, project_id: str) -> int:
        """Retorna numero de inscritos em projeto."""
        return len(self._project_groups.get(project_id, set()))

    def is_client_connected(self, client_id: str) -> bool:
        """Verifica se cliente esta conectado."""
        return client_id in self._connections


# =============================================================================
# INSTANCIA GLOBAL
# =============================================================================

# Instancia singleton do manager
ws_manager = WebSocketManager()


# =============================================================================
# FUNCAO DE COMPATIBILIDADE
# =============================================================================

def notify(notification_type: str, data: dict):
    """
    Funcao de compatibilidade para notificar eventos.
    Usa o manager global.

    Args:
        notification_type: Tipo da notificacao
        data: Dados da notificacao
    """
    ws_manager.notify(notification_type, data)

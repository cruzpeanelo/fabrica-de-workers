# -*- coding: utf-8 -*-
"""
Colaboracao em Tempo Real - Issue #242
======================================

Implementa colaboracao estilo Figma/Google Docs:
- Indicadores de presenca
- Cursores em tempo real
- Viewing indicators
- Locks otimistas

Autor: Terminal D - Features Agile e AI
"""

import asyncio
import json
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Set
from dataclasses import dataclass, field, asdict
from enum import Enum
import random
import hashlib

from fastapi import WebSocket


# =============================================================================
# ENUMS E TIPOS
# =============================================================================

class UserStatus(str, Enum):
    ONLINE = "online"
    AWAY = "away"
    BUSY = "busy"
    OFFLINE = "offline"


class EventType(str, Enum):
    # Presence
    JOIN = "join"
    LEAVE = "leave"
    STATUS_CHANGE = "status_change"
    # Cursor
    CURSOR_MOVE = "cursor_move"
    # Viewing
    VIEW_START = "view_start"
    VIEW_END = "view_end"
    # Editing
    EDIT_START = "edit_start"
    EDIT_END = "edit_end"
    EDIT_CONFLICT = "edit_conflict"
    # System
    PING = "ping"
    PONG = "pong"
    SYNC = "sync"


# =============================================================================
# DATA CLASSES
# =============================================================================

@dataclass
class CursorPosition:
    x: int
    y: int
    element_id: Optional[str] = None
    timestamp: datetime = field(default_factory=datetime.utcnow)


@dataclass
class UserPresence:
    user_id: str
    username: str
    avatar_url: Optional[str] = None
    color: str = ""
    status: UserStatus = UserStatus.ONLINE
    cursor: Optional[CursorPosition] = None
    viewing: Optional[str] = None  # story_id ou task_id
    editing: Optional[str] = None  # story_id ou task_id
    last_active: datetime = field(default_factory=datetime.utcnow)

    def __post_init__(self):
        if not self.color:
            self.color = self._generate_color()

    def _generate_color(self) -> str:
        """Gera cor unica baseada no user_id"""
        colors = [
            "#EF4444", "#F97316", "#F59E0B", "#84CC16", "#22C55E",
            "#14B8A6", "#06B6D4", "#0EA5E9", "#3B82F6", "#6366F1",
            "#8B5CF6", "#A855F7", "#D946EF", "#EC4899", "#F43F5E"
        ]
        hash_val = int(hashlib.md5(self.user_id.encode()).hexdigest()[:8], 16)
        return colors[hash_val % len(colors)]

    def to_dict(self) -> dict:
        return {
            "user_id": self.user_id,
            "username": self.username,
            "avatar_url": self.avatar_url,
            "color": self.color,
            "status": self.status.value,
            "cursor": asdict(self.cursor) if self.cursor else None,
            "viewing": self.viewing,
            "editing": self.editing,
            "last_active": self.last_active.isoformat()
        }


@dataclass
class EditLock:
    entity_type: str  # "story" ou "task"
    entity_id: str
    user_id: str
    username: str
    acquired_at: datetime = field(default_factory=datetime.utcnow)
    expires_at: datetime = field(default_factory=lambda: datetime.utcnow() + timedelta(minutes=5))

    def is_expired(self) -> bool:
        return datetime.utcnow() > self.expires_at

    def to_dict(self) -> dict:
        return {
            "entity_type": self.entity_type,
            "entity_id": self.entity_id,
            "user_id": self.user_id,
            "username": self.username,
            "acquired_at": self.acquired_at.isoformat(),
            "expires_at": self.expires_at.isoformat()
        }


# =============================================================================
# COLLABORATION MANAGER
# =============================================================================

class CollaborationManager:
    """
    Gerencia colaboracao em tempo real entre usuarios.

    Features:
    - Rooms por projeto
    - Presenca de usuarios
    - Cursores em tempo real
    - Locks otimistas
    """

    def __init__(self):
        # Room -> Set[WebSocket]
        self.rooms: Dict[str, Set[WebSocket]] = {}

        # Room -> User ID -> UserPresence
        self.presence: Dict[str, Dict[str, UserPresence]] = {}

        # WebSocket -> (room_id, user_id)
        self.socket_map: Dict[WebSocket, tuple] = {}

        # Entity ID -> EditLock
        self.locks: Dict[str, EditLock] = {}

        # Throttle de cursor updates (50ms = 20 FPS)
        self.cursor_throttle_ms = 50
        self.last_cursor_update: Dict[str, datetime] = {}

    def _get_room_id(self, project_id: str) -> str:
        """Gera ID do room baseado no projeto"""
        return f"project:{project_id}"

    async def join_room(
        self,
        websocket: WebSocket,
        project_id: str,
        user_id: str,
        username: str,
        avatar_url: Optional[str] = None
    ) -> UserPresence:
        """Usuario entra em um room de projeto"""
        room_id = self._get_room_id(project_id)

        # Inicializa room se nao existe
        if room_id not in self.rooms:
            self.rooms[room_id] = set()
            self.presence[room_id] = {}

        # Adiciona socket ao room
        self.rooms[room_id].add(websocket)
        self.socket_map[websocket] = (room_id, user_id)

        # Cria presenca do usuario
        user_presence = UserPresence(
            user_id=user_id,
            username=username,
            avatar_url=avatar_url
        )
        self.presence[room_id][user_id] = user_presence

        # Notifica outros usuarios
        await self._broadcast_to_room(room_id, {
            "type": EventType.JOIN.value,
            "user": user_presence.to_dict(),
            "timestamp": datetime.utcnow().isoformat()
        }, exclude=websocket)

        # Envia estado atual para o novo usuario
        await self._send_sync(websocket, room_id)

        return user_presence

    async def leave_room(self, websocket: WebSocket):
        """Usuario sai do room"""
        if websocket not in self.socket_map:
            return

        room_id, user_id = self.socket_map[websocket]

        # Remove socket do room
        if room_id in self.rooms:
            self.rooms[room_id].discard(websocket)

        # Remove presenca
        user_data = None
        if room_id in self.presence and user_id in self.presence[room_id]:
            user_data = self.presence[room_id][user_id].to_dict()
            del self.presence[room_id][user_id]

        # Remove locks do usuario
        locks_to_remove = [
            lock_id for lock_id, lock in self.locks.items()
            if lock.user_id == user_id
        ]
        for lock_id in locks_to_remove:
            del self.locks[lock_id]

        # Remove do mapa
        del self.socket_map[websocket]

        # Notifica outros usuarios
        if user_data:
            await self._broadcast_to_room(room_id, {
                "type": EventType.LEAVE.value,
                "user": user_data,
                "timestamp": datetime.utcnow().isoformat()
            })

        # Limpa room vazio
        if room_id in self.rooms and len(self.rooms[room_id]) == 0:
            del self.rooms[room_id]
            if room_id in self.presence:
                del self.presence[room_id]

    async def update_cursor(
        self,
        websocket: WebSocket,
        x: int,
        y: int,
        element_id: Optional[str] = None
    ):
        """Atualiza posicao do cursor (com throttle)"""
        if websocket not in self.socket_map:
            return

        room_id, user_id = self.socket_map[websocket]

        # Throttle check
        throttle_key = f"{room_id}:{user_id}"
        now = datetime.utcnow()
        if throttle_key in self.last_cursor_update:
            elapsed = (now - self.last_cursor_update[throttle_key]).total_seconds() * 1000
            if elapsed < self.cursor_throttle_ms:
                return

        self.last_cursor_update[throttle_key] = now

        # Atualiza presenca
        if room_id in self.presence and user_id in self.presence[room_id]:
            self.presence[room_id][user_id].cursor = CursorPosition(x=x, y=y, element_id=element_id)
            self.presence[room_id][user_id].last_active = now

            # Broadcast para outros
            await self._broadcast_to_room(room_id, {
                "type": EventType.CURSOR_MOVE.value,
                "user_id": user_id,
                "cursor": {"x": x, "y": y, "element_id": element_id},
                "timestamp": now.isoformat()
            }, exclude=websocket)

    async def start_viewing(self, websocket: WebSocket, entity_type: str, entity_id: str):
        """Usuario comeca a visualizar uma entidade"""
        if websocket not in self.socket_map:
            return

        room_id, user_id = self.socket_map[websocket]
        viewing_key = f"{entity_type}:{entity_id}"

        if room_id in self.presence and user_id in self.presence[room_id]:
            self.presence[room_id][user_id].viewing = viewing_key
            self.presence[room_id][user_id].last_active = datetime.utcnow()

            await self._broadcast_to_room(room_id, {
                "type": EventType.VIEW_START.value,
                "user_id": user_id,
                "entity_type": entity_type,
                "entity_id": entity_id,
                "timestamp": datetime.utcnow().isoformat()
            }, exclude=websocket)

    async def stop_viewing(self, websocket: WebSocket):
        """Usuario para de visualizar"""
        if websocket not in self.socket_map:
            return

        room_id, user_id = self.socket_map[websocket]

        if room_id in self.presence and user_id in self.presence[room_id]:
            viewing = self.presence[room_id][user_id].viewing
            self.presence[room_id][user_id].viewing = None

            if viewing:
                entity_type, entity_id = viewing.split(":", 1)
                await self._broadcast_to_room(room_id, {
                    "type": EventType.VIEW_END.value,
                    "user_id": user_id,
                    "entity_type": entity_type,
                    "entity_id": entity_id,
                    "timestamp": datetime.utcnow().isoformat()
                }, exclude=websocket)

    async def acquire_lock(
        self,
        websocket: WebSocket,
        entity_type: str,
        entity_id: str
    ) -> tuple[bool, Optional[EditLock]]:
        """Tenta adquirir lock para edicao"""
        if websocket not in self.socket_map:
            return False, None

        room_id, user_id = self.socket_map[websocket]
        lock_key = f"{entity_type}:{entity_id}"

        # Verifica lock existente
        if lock_key in self.locks:
            existing_lock = self.locks[lock_key]
            if not existing_lock.is_expired() and existing_lock.user_id != user_id:
                # Lock pertence a outro usuario
                await self._send_to_socket(websocket, {
                    "type": EventType.EDIT_CONFLICT.value,
                    "lock": existing_lock.to_dict(),
                    "message": f"{existing_lock.username} esta editando este item"
                })
                return False, existing_lock

        # Adquire lock
        username = self.presence.get(room_id, {}).get(user_id, UserPresence(user_id, "Unknown")).username
        lock = EditLock(
            entity_type=entity_type,
            entity_id=entity_id,
            user_id=user_id,
            username=username
        )
        self.locks[lock_key] = lock

        # Atualiza presenca
        if room_id in self.presence and user_id in self.presence[room_id]:
            self.presence[room_id][user_id].editing = lock_key

        # Notifica outros
        await self._broadcast_to_room(room_id, {
            "type": EventType.EDIT_START.value,
            "user_id": user_id,
            "lock": lock.to_dict(),
            "timestamp": datetime.utcnow().isoformat()
        }, exclude=websocket)

        return True, lock

    async def release_lock(self, websocket: WebSocket, entity_type: str, entity_id: str):
        """Libera lock de edicao"""
        if websocket not in self.socket_map:
            return

        room_id, user_id = self.socket_map[websocket]
        lock_key = f"{entity_type}:{entity_id}"

        # Verifica se o lock pertence ao usuario
        if lock_key in self.locks and self.locks[lock_key].user_id == user_id:
            del self.locks[lock_key]

            # Atualiza presenca
            if room_id in self.presence and user_id in self.presence[room_id]:
                self.presence[room_id][user_id].editing = None

            # Notifica outros
            await self._broadcast_to_room(room_id, {
                "type": EventType.EDIT_END.value,
                "user_id": user_id,
                "entity_type": entity_type,
                "entity_id": entity_id,
                "timestamp": datetime.utcnow().isoformat()
            }, exclude=websocket)

    async def update_status(self, websocket: WebSocket, status: UserStatus):
        """Atualiza status do usuario"""
        if websocket not in self.socket_map:
            return

        room_id, user_id = self.socket_map[websocket]

        if room_id in self.presence and user_id in self.presence[room_id]:
            self.presence[room_id][user_id].status = status
            self.presence[room_id][user_id].last_active = datetime.utcnow()

            await self._broadcast_to_room(room_id, {
                "type": EventType.STATUS_CHANGE.value,
                "user_id": user_id,
                "status": status.value,
                "timestamp": datetime.utcnow().isoformat()
            }, exclude=websocket)

    def get_room_presence(self, project_id: str) -> List[dict]:
        """Retorna lista de usuarios no room"""
        room_id = self._get_room_id(project_id)
        if room_id not in self.presence:
            return []
        return [user.to_dict() for user in self.presence[room_id].values()]

    def get_viewers(self, entity_type: str, entity_id: str) -> List[dict]:
        """Retorna usuarios visualizando uma entidade"""
        viewing_key = f"{entity_type}:{entity_id}"
        viewers = []
        for room_id, users in self.presence.items():
            for user_id, user in users.items():
                if user.viewing == viewing_key:
                    viewers.append(user.to_dict())
        return viewers

    def get_lock(self, entity_type: str, entity_id: str) -> Optional[dict]:
        """Retorna lock de uma entidade"""
        lock_key = f"{entity_type}:{entity_id}"
        if lock_key in self.locks:
            lock = self.locks[lock_key]
            if not lock.is_expired():
                return lock.to_dict()
            else:
                del self.locks[lock_key]
        return None

    async def _send_sync(self, websocket: WebSocket, room_id: str):
        """Envia estado atual para um usuario"""
        users = [user.to_dict() for user in self.presence.get(room_id, {}).values()]
        locks = [lock.to_dict() for lock in self.locks.values() if not lock.is_expired()]

        await self._send_to_socket(websocket, {
            "type": EventType.SYNC.value,
            "users": users,
            "locks": locks,
            "timestamp": datetime.utcnow().isoformat()
        })

    async def _broadcast_to_room(
        self,
        room_id: str,
        message: dict,
        exclude: Optional[WebSocket] = None
    ):
        """Envia mensagem para todos no room"""
        if room_id not in self.rooms:
            return

        dead_sockets = []
        for socket in self.rooms[room_id]:
            if socket != exclude:
                try:
                    await socket.send_json(message)
                except Exception:
                    dead_sockets.append(socket)

        # Remove sockets mortos
        for socket in dead_sockets:
            await self.leave_room(socket)

    async def _send_to_socket(self, websocket: WebSocket, message: dict):
        """Envia mensagem para um socket especifico"""
        try:
            await websocket.send_json(message)
        except Exception:
            await self.leave_room(websocket)


# Singleton global
collab_manager = CollaborationManager()

# -*- coding: utf-8 -*-
"""
Testes para o modulo de Colaboracao em Tempo Real - Issue #242
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import AsyncMock, MagicMock

from factory.websocket.collaboration import (
    CollaborationManager,
    UserPresence,
    EditLock,
    CursorPosition,
    UserStatus,
    EventType
)


class TestUserPresence:
    """Testes para UserPresence"""

    def test_create_user_presence(self):
        """Deve criar presenca com valores padrao"""
        presence = UserPresence(
            user_id="user1",
            username="Joao"
        )

        assert presence.user_id == "user1"
        assert presence.username == "Joao"
        assert presence.status == UserStatus.ONLINE
        assert presence.color != ""  # Cor gerada automaticamente
        assert presence.cursor is None
        assert presence.viewing is None
        assert presence.editing is None

    def test_user_presence_color_consistency(self):
        """Mesma user_id deve gerar mesma cor"""
        presence1 = UserPresence(user_id="user1", username="A")
        presence2 = UserPresence(user_id="user1", username="B")
        presence3 = UserPresence(user_id="user2", username="C")

        assert presence1.color == presence2.color
        assert presence1.color != presence3.color  # Usuarios diferentes tem cores diferentes

    def test_user_presence_to_dict(self):
        """Deve converter para dicionario corretamente"""
        presence = UserPresence(
            user_id="user1",
            username="Joao",
            avatar_url="http://example.com/avatar.jpg"
        )

        data = presence.to_dict()

        assert data["user_id"] == "user1"
        assert data["username"] == "Joao"
        assert data["avatar_url"] == "http://example.com/avatar.jpg"
        assert data["status"] == "online"
        assert "color" in data
        assert "last_active" in data


class TestEditLock:
    """Testes para EditLock"""

    def test_create_edit_lock(self):
        """Deve criar lock com valores padrao"""
        lock = EditLock(
            entity_type="story",
            entity_id="STR-001",
            user_id="user1",
            username="Joao"
        )

        assert lock.entity_type == "story"
        assert lock.entity_id == "STR-001"
        assert lock.user_id == "user1"
        assert lock.username == "Joao"
        assert not lock.is_expired()

    def test_lock_expiration(self):
        """Lock deve expirar apos tempo configurado"""
        lock = EditLock(
            entity_type="story",
            entity_id="STR-001",
            user_id="user1",
            username="Joao",
            expires_at=datetime.utcnow() - timedelta(minutes=1)  # Ja expirado
        )

        assert lock.is_expired()

    def test_lock_to_dict(self):
        """Deve converter para dicionario corretamente"""
        lock = EditLock(
            entity_type="story",
            entity_id="STR-001",
            user_id="user1",
            username="Joao"
        )

        data = lock.to_dict()

        assert data["entity_type"] == "story"
        assert data["entity_id"] == "STR-001"
        assert data["user_id"] == "user1"
        assert data["username"] == "Joao"
        assert "acquired_at" in data
        assert "expires_at" in data


class TestCollaborationManager:
    """Testes para CollaborationManager"""

    @pytest.fixture
    def manager(self):
        """Cria instancia do manager para testes"""
        return CollaborationManager()

    @pytest.fixture
    def mock_websocket(self):
        """Cria mock de WebSocket"""
        ws = AsyncMock()
        ws.send_json = AsyncMock()
        return ws

    @pytest.mark.asyncio
    async def test_join_room(self, manager, mock_websocket):
        """Usuario deve entrar no room corretamente"""
        presence = await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        assert presence.user_id == "user1"
        assert presence.username == "Joao"
        assert mock_websocket in manager.socket_map
        assert "project:PROJ-001" in manager.rooms
        assert mock_websocket in manager.rooms["project:PROJ-001"]

    @pytest.mark.asyncio
    async def test_leave_room(self, manager, mock_websocket):
        """Usuario deve sair do room corretamente"""
        # Entrar primeiro
        await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        # Sair
        await manager.leave_room(mock_websocket)

        assert mock_websocket not in manager.socket_map
        # Room vazio deve ser removido
        assert "project:PROJ-001" not in manager.rooms

    @pytest.mark.asyncio
    async def test_cursor_update_throttle(self, manager, mock_websocket):
        """Cursor updates devem ser throttled"""
        await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        # Primeiro update deve funcionar
        await manager.update_cursor(mock_websocket, x=100, y=100)

        # Updates muito rapidos devem ser ignorados
        await manager.update_cursor(mock_websocket, x=110, y=110)
        await manager.update_cursor(mock_websocket, x=120, y=120)

        # Apenas o primeiro deve ter atualizado o cursor
        presence = manager.presence["project:PROJ-001"]["user1"]
        assert presence.cursor is not None
        assert presence.cursor.x == 100  # Primeiro valor

    @pytest.mark.asyncio
    async def test_viewing_indicator(self, manager, mock_websocket):
        """Deve rastrear quem esta visualizando"""
        await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        await manager.start_viewing(mock_websocket, "story", "STR-001")

        presence = manager.presence["project:PROJ-001"]["user1"]
        assert presence.viewing == "story:STR-001"

        await manager.stop_viewing(mock_websocket)
        assert presence.viewing is None

    @pytest.mark.asyncio
    async def test_acquire_lock(self, manager, mock_websocket):
        """Deve adquirir lock para edicao"""
        await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        success, lock = await manager.acquire_lock(mock_websocket, "story", "STR-001")

        assert success
        assert lock is not None
        assert lock.user_id == "user1"
        assert "story:STR-001" in manager.locks

    @pytest.mark.asyncio
    async def test_lock_conflict(self, manager):
        """Deve detectar conflito de lock"""
        ws1 = AsyncMock()
        ws1.send_json = AsyncMock()
        ws2 = AsyncMock()
        ws2.send_json = AsyncMock()

        # User1 entra e adquire lock
        await manager.join_room(ws1, "PROJ-001", "user1", "Joao")
        success1, _ = await manager.acquire_lock(ws1, "story", "STR-001")
        assert success1

        # User2 entra e tenta adquirir mesmo lock
        await manager.join_room(ws2, "PROJ-001", "user2", "Maria")
        success2, lock2 = await manager.acquire_lock(ws2, "story", "STR-001")

        assert not success2
        assert lock2.user_id == "user1"  # Lock ainda pertence ao user1
        # Deve ter enviado mensagem de conflito para user2
        ws2.send_json.assert_called()

    @pytest.mark.asyncio
    async def test_release_lock(self, manager, mock_websocket):
        """Deve liberar lock de edicao"""
        await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        await manager.acquire_lock(mock_websocket, "story", "STR-001")
        await manager.release_lock(mock_websocket, "story", "STR-001")

        assert "story:STR-001" not in manager.locks

    @pytest.mark.asyncio
    async def test_status_change(self, manager, mock_websocket):
        """Deve atualizar status do usuario"""
        await manager.join_room(
            websocket=mock_websocket,
            project_id="PROJ-001",
            user_id="user1",
            username="Joao"
        )

        await manager.update_status(mock_websocket, UserStatus.AWAY)

        presence = manager.presence["project:PROJ-001"]["user1"]
        assert presence.status == UserStatus.AWAY

    def test_get_room_presence(self, manager):
        """Deve retornar lista de usuarios no room"""
        # Room vazio
        assert manager.get_room_presence("PROJ-001") == []

    def test_get_viewers(self, manager):
        """Deve retornar viewers de uma entidade"""
        viewers = manager.get_viewers("story", "STR-001")
        assert viewers == []

    def test_get_lock(self, manager):
        """Deve retornar lock de uma entidade"""
        lock = manager.get_lock("story", "STR-001")
        assert lock is None


class TestEventTypes:
    """Testes para tipos de evento"""

    def test_user_status_values(self):
        """Valores de status devem ser validos"""
        assert UserStatus.ONLINE.value == "online"
        assert UserStatus.AWAY.value == "away"
        assert UserStatus.BUSY.value == "busy"
        assert UserStatus.OFFLINE.value == "offline"

    def test_event_type_values(self):
        """Valores de tipo de evento devem ser validos"""
        assert EventType.JOIN.value == "join"
        assert EventType.LEAVE.value == "leave"
        assert EventType.CURSOR_MOVE.value == "cursor_move"
        assert EventType.EDIT_START.value == "edit_start"
        assert EventType.EDIT_CONFLICT.value == "edit_conflict"

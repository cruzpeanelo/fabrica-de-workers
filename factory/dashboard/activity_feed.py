# -*- coding: utf-8 -*-
"""
Activity Feed / Activity Log - Issue #233
==========================================
Real-time activity feed showing all actions in the project.

Features:
1. Real-time activity stream via WebSocket
2. Filter by: user, entity type, action type, date range
3. Activity types: created, updated, moved, commented, assigned, completed
4. Entity types: story, task, epic, sprint, comment
5. User avatars and timestamps
6. Grouped by day
7. Infinite scroll pagination
8. WebSocket for real-time updates

Endpoints:
- GET /api/activities - List activities (paginated, with filters)
- GET /api/activities/stats - Activity statistics
- GET /api/projects/{id}/activities - Project-specific activities
- GET /api/users/{id}/activities - User-specific activities
- WS /ws/activities - Real-time activity stream
- GET /activities - Full page activity feed
"""

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, Query, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime, timedelta
from enum import Enum
import asyncio
import json
import uuid


# =============================================================================
# ENUMS & DATA MODEL
# =============================================================================

class ActivityAction(str, Enum):
    CREATED = "created"
    UPDATED = "updated"
    DELETED = "deleted"
    MOVED = "moved"
    COMMENTED = "commented"
    ASSIGNED = "assigned"
    COMPLETED = "completed"


class EntityType(str, Enum):
    STORY = "story"
    TASK = "task"
    EPIC = "epic"
    SPRINT = "sprint"
    COMMENT = "comment"
    PROJECT = "project"
    USER = "user"


class Activity(BaseModel):
    id: str
    project_id: Optional[str] = None
    user_id: str
    user_name: str
    user_avatar: Optional[str] = None
    action: ActivityAction
    entity_type: EntityType
    entity_id: str
    entity_title: str
    details: Dict[str, Any] = {}
    created_at: datetime


# =============================================================================
# IN-MEMORY ACTIVITY STORE
# =============================================================================

activities_store: List[Dict[str, Any]] = []
MAX_ACTIVITIES = 10000

# Sample user avatars (initials-based)
def get_user_avatar(user_name: str) -> str:
    """Generate avatar URL based on user initials"""
    initials = "".join([w[0].upper() for w in user_name.split()[:2]])
    colors = ["3B82F6", "10B981", "8B5CF6", "F59E0B", "EF4444", "06B6D4", "EC4899"]
    color = colors[sum(ord(c) for c in user_name) % len(colors)]
    return f"https://ui-avatars.com/api/?name={initials}&background={color}&color=fff&size=40"


def log_activity(
    action: str,
    entity_type: str,
    entity_id: str,
    entity_title: str,
    user_id: str = "user_1",
    user_name: str = "Sistema",
    project_id: str = None,
    details: Dict[str, Any] = None
) -> Dict[str, Any]:
    """Log a new activity and broadcast to WebSocket subscribers"""
    activity = {
        "id": f"act_{uuid.uuid4().hex[:12]}",
        "project_id": project_id,
        "user_id": user_id,
        "user_name": user_name,
        "user_avatar": get_user_avatar(user_name),
        "action": action,
        "entity_type": entity_type,
        "entity_id": entity_id,
        "entity_title": entity_title,
        "details": details or {},
        "created_at": datetime.utcnow().isoformat()
    }

    activities_store.insert(0, activity)

    # Trim to max size
    while len(activities_store) > MAX_ACTIVITIES:
        activities_store.pop()

    # Broadcast to WebSocket connections
    asyncio.create_task(activity_ws_manager.broadcast(activity))

    return activity


# =============================================================================
# WEBSOCKET MANAGER FOR ACTIVITY FEED
# =============================================================================

class ActivityWebSocketManager:
    def __init__(self):
        self.active_connections: List[WebSocket] = []
        self.connection_filters: Dict[WebSocket, Dict[str, Any]] = {}

    async def connect(self, websocket: WebSocket, filters: Dict[str, Any] = None):
        await websocket.accept()
        self.active_connections.append(websocket)
        self.connection_filters[websocket] = filters or {}

    def disconnect(self, websocket: WebSocket):
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)
        if websocket in self.connection_filters:
            del self.connection_filters[websocket]

    async def broadcast(self, activity: Dict[str, Any]):
        """Broadcast activity to all connected clients that match filters"""
        for connection in self.active_connections:
            try:
                filters = self.connection_filters.get(connection, {})

                # Apply filters
                if filters.get("project_id") and activity.get("project_id") != filters["project_id"]:
                    continue
                if filters.get("user_id") and activity.get("user_id") != filters["user_id"]:
                    continue
                if filters.get("entity_type") and activity.get("entity_type") != filters["entity_type"]:
                    continue
                if filters.get("action") and activity.get("action") != filters["action"]:
                    continue

                await connection.send_json({
                    "type": "activity",
                    "data": activity
                })
            except Exception:
                pass

activity_ws_manager = ActivityWebSocketManager()


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def filter_activities(
    activities: List[Dict],
    user_id: Optional[str] = None,
    project_id: Optional[str] = None,
    entity_type: Optional[str] = None,
    action: Optional[str] = None,
    date_from: Optional[str] = None,
    date_to: Optional[str] = None
) -> List[Dict]:
    """Filter activities based on criteria"""
    result = activities.copy()

    if user_id:
        result = [a for a in result if a.get("user_id") == user_id]

    if project_id:
        result = [a for a in result if a.get("project_id") == project_id]

    if entity_type:
        result = [a for a in result if a.get("entity_type") == entity_type]

    if action:
        result = [a for a in result if a.get("action") == action]

    if date_from:
        try:
            from_dt = datetime.fromisoformat(date_from.replace("Z", "+00:00"))
            result = [a for a in result
                     if datetime.fromisoformat(a["created_at"].replace("Z", "+00:00")) >= from_dt]
        except:
            pass

    if date_to:
        try:
            to_dt = datetime.fromisoformat(date_to.replace("Z", "+00:00"))
            result = [a for a in result
                     if datetime.fromisoformat(a["created_at"].replace("Z", "+00:00")) <= to_dt]
        except:
            pass

    return result


def group_activities_by_day(activities: List[Dict]) -> Dict[str, List[Dict]]:
    """Group activities by day for display"""
    grouped = {}
    today = datetime.utcnow().date()
    yesterday = today - timedelta(days=1)

    for activity in activities:
        try:
            dt = datetime.fromisoformat(activity["created_at"].replace("Z", "+00:00"))
            activity_date = dt.date()

            if activity_date == today:
                key = "Hoje"
            elif activity_date == yesterday:
                key = "Ontem"
            else:
                key = activity_date.strftime("%d de %B, %Y")

            if key not in grouped:
                grouped[key] = []
            grouped[key].append(activity)
        except:
            if "Outras" not in grouped:
                grouped["Outras"] = []
            grouped["Outras"].append(activity)

    return grouped


def get_activity_description(activity: Dict) -> str:
    """Generate human-readable activity description"""
    action_verbs = {
        "created": "criou",
        "updated": "atualizou",
        "deleted": "excluiu",
        "moved": "moveu",
        "commented": "comentou em",
        "assigned": "atribuiu",
        "completed": "completou"
    }

    entity_names = {
        "story": "story",
        "task": "task",
        "epic": "epic",
        "sprint": "sprint",
        "comment": "comentario",
        "project": "projeto",
        "user": "usuario"
    }

    verb = action_verbs.get(activity.get("action", ""), activity.get("action", ""))
    entity = entity_names.get(activity.get("entity_type", ""), activity.get("entity_type", ""))

    return f'{verb} {entity} "{activity.get("entity_title", "")}"'


# =============================================================================
# SEED DEMO DATA
# =============================================================================

def seed_demo_activities():
    """Seed some demo activities for testing"""
    if activities_store:
        return

    demo_users = [
        ("user_1", "Joao Silva"),
        ("user_2", "Maria Santos"),
        ("user_3", "Pedro Costa"),
        ("user_4", "Ana Oliveira"),
    ]

    demo_activities = [
        ("created", "story", "STR-001", "Implementar login com OAuth2", "PRJ-001"),
        ("updated", "story", "STR-001", "Implementar login com OAuth2", "PRJ-001"),
        ("moved", "story", "STR-002", "Dashboard responsivo", "PRJ-001"),
        ("completed", "task", "TSK-001", "Criar componente de formulario", "PRJ-001"),
        ("commented", "story", "STR-003", "API de notificacoes", "PRJ-001"),
        ("assigned", "story", "STR-004", "Sistema de cache", "PRJ-001"),
        ("created", "epic", "EPIC-001", "Autenticacao e Seguranca", "PRJ-001"),
        ("created", "sprint", "SPR-001", "Sprint 1 - Janeiro", "PRJ-001"),
        ("updated", "task", "TSK-002", "Testes unitarios do modulo auth", "PRJ-001"),
        ("completed", "story", "STR-005", "Pagina de perfil do usuario", "PRJ-001"),
    ]

    base_time = datetime.utcnow()

    for i, (action, entity_type, entity_id, title, project_id) in enumerate(demo_activities):
        user_id, user_name = demo_users[i % len(demo_users)]

        activity = {
            "id": f"act_demo_{i:04d}",
            "project_id": project_id,
            "user_id": user_id,
            "user_name": user_name,
            "user_avatar": get_user_avatar(user_name),
            "action": action,
            "entity_type": entity_type,
            "entity_id": entity_id,
            "entity_title": title,
            "details": {},
            "created_at": (base_time - timedelta(hours=i * 2)).isoformat()
        }
        activities_store.append(activity)


# =============================================================================
# REGISTER ACTIVITY FEED ENDPOINTS
# =============================================================================

def register_activity_feed(app: FastAPI):
    """Register all activity feed endpoints"""

    # Seed demo data
    seed_demo_activities()

    # -------------------------------------------------------------------------
    # API Endpoints
    # -------------------------------------------------------------------------

    @app.get("/api/activities")
    async def get_activities(
        user_id: Optional[str] = Query(None, description="Filter by user ID"),
        project_id: Optional[str] = Query(None, description="Filter by project ID"),
        entity_type: Optional[str] = Query(None, description="Filter by entity type"),
        action: Optional[str] = Query(None, description="Filter by action type"),
        date_from: Optional[str] = Query(None, description="Filter from date (ISO format)"),
        date_to: Optional[str] = Query(None, description="Filter to date (ISO format)"),
        limit: int = Query(50, ge=1, le=200, description="Number of activities to return"),
        offset: int = Query(0, ge=0, description="Offset for pagination")
    ):
        """Get paginated list of activities with filters"""
        filtered = filter_activities(
            activities_store,
            user_id=user_id,
            project_id=project_id,
            entity_type=entity_type,
            action=action,
            date_from=date_from,
            date_to=date_to
        )

        total = len(filtered)
        paginated = filtered[offset:offset + limit]

        # Group by day
        grouped = group_activities_by_day(paginated)

        return {
            "activities": paginated,
            "grouped": grouped,
            "total": total,
            "offset": offset,
            "limit": limit,
            "has_more": offset + limit < total
        }

    @app.get("/api/activities/stats")
    async def get_activity_stats(
        project_id: Optional[str] = Query(None),
        days: int = Query(7, ge=1, le=90)
    ):
        """Get activity statistics"""
        cutoff = datetime.utcnow() - timedelta(days=days)

        filtered = activities_store
        if project_id:
            filtered = [a for a in filtered if a.get("project_id") == project_id]

        recent = [a for a in filtered
                  if datetime.fromisoformat(a["created_at"].replace("Z", "+00:00")) >= cutoff]

        # Count by action
        by_action = {}
        for a in recent:
            action = a.get("action", "unknown")
            by_action[action] = by_action.get(action, 0) + 1

        # Count by entity type
        by_entity = {}
        for a in recent:
            entity = a.get("entity_type", "unknown")
            by_entity[entity] = by_entity.get(entity, 0) + 1

        # Count by user (top 10)
        by_user = {}
        for a in recent:
            user = a.get("user_name", "unknown")
            by_user[user] = by_user.get(user, 0) + 1
        top_users = sorted(by_user.items(), key=lambda x: x[1], reverse=True)[:10]

        # Count by day
        by_day = {}
        for a in recent:
            day = a["created_at"][:10]
            by_day[day] = by_day.get(day, 0) + 1

        # Most active hours
        by_hour = {}
        for a in recent:
            try:
                hour = datetime.fromisoformat(a["created_at"].replace("Z", "+00:00")).hour
                by_hour[hour] = by_hour.get(hour, 0) + 1
            except:
                pass

        return {
            "period_days": days,
            "total_activities": len(recent),
            "by_action": by_action,
            "by_entity_type": by_entity,
            "top_users": [{"user": u, "count": c} for u, c in top_users],
            "by_day": dict(sorted(by_day.items())),
            "by_hour": dict(sorted(by_hour.items()))
        }

    @app.get("/api/projects/{project_id}/activities")
    async def get_project_activities(
        project_id: str,
        limit: int = Query(50, ge=1, le=200),
        offset: int = Query(0, ge=0)
    ):
        """Get activities for a specific project"""
        filtered = [a for a in activities_store if a.get("project_id") == project_id]

        total = len(filtered)
        paginated = filtered[offset:offset + limit]
        grouped = group_activities_by_day(paginated)

        return {
            "project_id": project_id,
            "activities": paginated,
            "grouped": grouped,
            "total": total,
            "offset": offset,
            "limit": limit,
            "has_more": offset + limit < total
        }

    @app.get("/api/users/{user_id}/activities")
    async def get_user_activities(
        user_id: str,
        limit: int = Query(50, ge=1, le=200),
        offset: int = Query(0, ge=0)
    ):
        """Get activities for a specific user"""
        filtered = [a for a in activities_store if a.get("user_id") == user_id]

        total = len(filtered)
        paginated = filtered[offset:offset + limit]
        grouped = group_activities_by_day(paginated)

        # Get user info
        user_name = paginated[0].get("user_name", "Usuario") if paginated else "Usuario"
        user_avatar = paginated[0].get("user_avatar", "") if paginated else ""

        return {
            "user_id": user_id,
            "user_name": user_name,
            "user_avatar": user_avatar,
            "activities": paginated,
            "grouped": grouped,
            "total": total,
            "offset": offset,
            "limit": limit,
            "has_more": offset + limit < total
        }

    @app.post("/api/activities")
    async def create_activity(
        action: str,
        entity_type: str,
        entity_id: str,
        entity_title: str,
        user_id: str = "user_1",
        user_name: str = "Sistema",
        project_id: Optional[str] = None,
        details: Optional[Dict[str, Any]] = None
    ):
        """Create a new activity (internal API)"""
        activity = log_activity(
            action=action,
            entity_type=entity_type,
            entity_id=entity_id,
            entity_title=entity_title,
            user_id=user_id,
            user_name=user_name,
            project_id=project_id,
            details=details
        )
        return activity

    # -------------------------------------------------------------------------
    # WebSocket Endpoint
    # -------------------------------------------------------------------------

    @app.websocket("/ws/activities")
    async def websocket_activities(
        websocket: WebSocket,
        project_id: Optional[str] = Query(None),
        user_id: Optional[str] = Query(None),
        entity_type: Optional[str] = Query(None)
    ):
        """WebSocket endpoint for real-time activity stream"""
        filters = {}
        if project_id:
            filters["project_id"] = project_id
        if user_id:
            filters["user_id"] = user_id
        if entity_type:
            filters["entity_type"] = entity_type

        await activity_ws_manager.connect(websocket, filters)

        try:
            # Send initial connection confirmation
            await websocket.send_json({
                "type": "connected",
                "message": "Connected to activity feed",
                "filters": filters
            })

            # Keep connection alive
            while True:
                data = await websocket.receive_text()

                # Handle ping/pong for keep-alive
                if data == "ping":
                    await websocket.send_json({"type": "pong"})

        except WebSocketDisconnect:
            activity_ws_manager.disconnect(websocket)

    # -------------------------------------------------------------------------
    # Activity Feed Page
    # -------------------------------------------------------------------------

    @app.get("/activities", response_class=HTMLResponse)
    async def activity_feed_page():
        """Full page activity feed with filters and real-time updates"""
        return ACTIVITY_FEED_TEMPLATE

    print("[Dashboard] Activity Feed loaded: /activities, /api/activities/*, /ws/activities")


# =============================================================================
# HTML TEMPLATE FOR ACTIVITY FEED PAGE
# =============================================================================

ACTIVITY_FEED_TEMPLATE = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Activity Feed - Plataforma E</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <style>
        * { font-family: 'Inter', sans-serif; }
        :root {
            --belgo-blue: #003B4A;
            --belgo-orange: #FF6C00;
        }

        /* Real-time indicator pulse animation */
        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }
        .pulse-animation {
            animation: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite;
        }

        /* New activity highlight */
        @keyframes slideIn {
            from {
                opacity: 0;
                transform: translateY(-10px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }
        .activity-new {
            animation: slideIn 0.3s ease-out;
            background-color: #FEF3C7;
        }

        /* Activity timeline */
        .activity-timeline::before {
            content: '';
            position: absolute;
            left: 24px;
            top: 0;
            bottom: 0;
            width: 2px;
            background: #E5E7EB;
        }

        /* Infinite scroll loading */
        .loading-indicator {
            display: flex;
            justify-content: center;
            padding: 20px;
        }

        /* Action icons */
        .action-icon {
            width: 32px;
            height: 32px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            flex-shrink: 0;
        }

        /* Scrollbar styling */
        .activity-scroll::-webkit-scrollbar {
            width: 6px;
        }
        .activity-scroll::-webkit-scrollbar-track {
            background: #F3F4F6;
        }
        .activity-scroll::-webkit-scrollbar-thumb {
            background: #CBD5E1;
            border-radius: 3px;
        }
    </style>
</head>
<body class="bg-gray-100 min-h-screen">
    <div id="app">
        <!-- Header -->
        <header class="bg-[#003B4A] text-white py-4 px-6 shadow-lg sticky top-0 z-50">
            <div class="max-w-7xl mx-auto flex justify-between items-center">
                <div class="flex items-center gap-4">
                    <div>
                        <h1 class="text-2xl font-bold">Activity Feed</h1>
                        <p class="text-blue-200 text-sm">Historico de atividades em tempo real</p>
                    </div>
                    <!-- Real-time indicator -->
                    <div v-if="wsConnected" class="flex items-center gap-2 bg-green-500/20 px-3 py-1 rounded-full">
                        <span class="w-2 h-2 bg-green-400 rounded-full pulse-animation"></span>
                        <span class="text-green-300 text-xs font-medium">Tempo Real</span>
                    </div>
                    <div v-else class="flex items-center gap-2 bg-red-500/20 px-3 py-1 rounded-full">
                        <span class="w-2 h-2 bg-red-400 rounded-full"></span>
                        <span class="text-red-300 text-xs font-medium">Desconectado</span>
                    </div>
                </div>
                <div class="flex items-center gap-4">
                    <a href="/" class="bg-[#FF6C00] hover:bg-orange-600 px-4 py-2 rounded-lg font-medium transition flex items-center gap-2">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18"/>
                        </svg>
                        Voltar ao Dashboard
                    </a>
                </div>
            </div>
        </header>

        <main class="max-w-7xl mx-auto py-6 px-6">
            <div class="flex gap-6">
                <!-- Sidebar Filters -->
                <aside class="w-64 flex-shrink-0">
                    <div class="bg-white rounded-xl shadow-sm p-4 sticky top-24">
                        <h3 class="font-semibold text-gray-800 mb-4">Filtros</h3>

                        <!-- Entity Type Filter -->
                        <div class="mb-4">
                            <label class="block text-sm font-medium text-gray-600 mb-2">Tipo de Entidade</label>
                            <select v-model="filters.entity_type" @change="loadActivities(true)"
                                    class="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:ring-2 focus:ring-[#003B4A]/20 focus:border-[#003B4A]">
                                <option value="">Todos</option>
                                <option value="story">Stories</option>
                                <option value="task">Tasks</option>
                                <option value="epic">Epics</option>
                                <option value="sprint">Sprints</option>
                                <option value="comment">Comentarios</option>
                            </select>
                        </div>

                        <!-- Action Filter -->
                        <div class="mb-4">
                            <label class="block text-sm font-medium text-gray-600 mb-2">Tipo de Acao</label>
                            <select v-model="filters.action" @change="loadActivities(true)"
                                    class="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:ring-2 focus:ring-[#003B4A]/20 focus:border-[#003B4A]">
                                <option value="">Todas</option>
                                <option value="created">Criadas</option>
                                <option value="updated">Atualizadas</option>
                                <option value="moved">Movidas</option>
                                <option value="completed">Completadas</option>
                                <option value="commented">Comentadas</option>
                                <option value="assigned">Atribuidas</option>
                                <option value="deleted">Excluidas</option>
                            </select>
                        </div>

                        <!-- User Filter -->
                        <div class="mb-4">
                            <label class="block text-sm font-medium text-gray-600 mb-2">Usuario</label>
                            <select v-model="filters.user_id" @change="loadActivities(true)"
                                    class="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:ring-2 focus:ring-[#003B4A]/20 focus:border-[#003B4A]">
                                <option value="">Todos</option>
                                <option v-for="user in uniqueUsers" :key="user.id" :value="user.id">
                                    {{ user.name }}
                                </option>
                            </select>
                        </div>

                        <!-- Date Range -->
                        <div class="mb-4">
                            <label class="block text-sm font-medium text-gray-600 mb-2">Periodo</label>
                            <div class="space-y-2">
                                <input type="date" v-model="filters.date_from" @change="loadActivities(true)"
                                       class="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:ring-2 focus:ring-[#003B4A]/20 focus:border-[#003B4A]"
                                       placeholder="De">
                                <input type="date" v-model="filters.date_to" @change="loadActivities(true)"
                                       class="w-full px-3 py-2 border border-gray-200 rounded-lg text-sm focus:ring-2 focus:ring-[#003B4A]/20 focus:border-[#003B4A]"
                                       placeholder="Ate">
                            </div>
                        </div>

                        <!-- Clear Filters -->
                        <button @click="clearFilters"
                                class="w-full px-4 py-2 text-sm text-gray-600 hover:text-gray-800 hover:bg-gray-100 rounded-lg transition">
                            Limpar Filtros
                        </button>

                        <!-- Stats Summary -->
                        <div class="mt-6 pt-4 border-t border-gray-100">
                            <h4 class="text-sm font-medium text-gray-600 mb-3">Resumo (7 dias)</h4>
                            <div class="space-y-2">
                                <div class="flex justify-between text-sm">
                                    <span class="text-gray-500">Total</span>
                                    <span class="font-medium text-gray-800">{{ stats.total_activities || 0 }}</span>
                                </div>
                                <div v-for="(count, action) in stats.by_action" :key="action"
                                     class="flex justify-between text-sm">
                                    <span class="text-gray-500 capitalize">{{ action }}</span>
                                    <span class="font-medium text-gray-800">{{ count }}</span>
                                </div>
                            </div>
                        </div>
                    </div>
                </aside>

                <!-- Main Content -->
                <div class="flex-1">
                    <!-- New Activity Alert -->
                    <div v-if="newActivitiesCount > 0"
                         @click="showNewActivities"
                         class="bg-blue-50 border border-blue-200 rounded-xl p-3 mb-4 cursor-pointer hover:bg-blue-100 transition flex items-center justify-center gap-2">
                        <svg class="w-5 h-5 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 10l7-7m0 0l7 7m-7-7v18"/>
                        </svg>
                        <span class="text-blue-700 font-medium">{{ newActivitiesCount }} nova(s) atividade(s)</span>
                    </div>

                    <!-- Activity List -->
                    <div class="bg-white rounded-xl shadow-sm overflow-hidden">
                        <!-- Header -->
                        <div class="px-6 py-4 border-b border-gray-100 flex justify-between items-center">
                            <div>
                                <span class="font-semibold text-gray-800">Atividades</span>
                                <span class="text-sm text-gray-500 ml-2">({{ total }} total)</span>
                            </div>
                            <button @click="loadActivities(true)"
                                    class="p-2 hover:bg-gray-100 rounded-lg transition" title="Atualizar">
                                <svg class="w-5 h-5 text-gray-500" :class="{'animate-spin': loading}" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"/>
                                </svg>
                            </button>
                        </div>

                        <!-- Activity Timeline -->
                        <div class="activity-scroll max-h-[calc(100vh-280px)] overflow-y-auto"
                             @scroll="handleScroll" ref="activityList">

                            <!-- Loading State -->
                            <div v-if="loading && activities.length === 0" class="py-20 text-center">
                                <div class="animate-spin rounded-full h-10 w-10 border-b-2 border-[#003B4A] mx-auto mb-4"></div>
                                <span class="text-gray-500">Carregando atividades...</span>
                            </div>

                            <!-- Empty State -->
                            <div v-else-if="activities.length === 0" class="py-20 text-center">
                                <svg class="w-16 h-16 text-gray-300 mx-auto mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"/>
                                </svg>
                                <p class="text-gray-500">Nenhuma atividade encontrada</p>
                                <p class="text-gray-400 text-sm">Ajuste os filtros ou aguarde novas atividades</p>
                            </div>

                            <!-- Grouped Activities -->
                            <template v-else>
                                <div v-for="(dayActivities, day) in grouped" :key="day">
                                    <!-- Day Header -->
                                    <div class="px-6 py-2 bg-gray-50 border-b border-gray-100 sticky top-0 z-10">
                                        <span class="text-sm font-medium text-gray-600">{{ day }}</span>
                                    </div>

                                    <!-- Day Activities -->
                                    <div class="relative activity-timeline">
                                        <div v-for="activity in dayActivities" :key="activity.id"
                                             :class="['px-6 py-4 border-b border-gray-50 hover:bg-gray-50 transition relative',
                                                      activity.isNew ? 'activity-new' : '']">
                                            <div class="flex gap-4">
                                                <!-- Action Icon -->
                                                <div :class="['action-icon', getActionIconClass(activity.action)]">
                                                    <svg class="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                        <path :d="getActionIconPath(activity.action)" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"/>
                                                    </svg>
                                                </div>

                                                <!-- Content -->
                                                <div class="flex-1 min-w-0">
                                                    <div class="flex items-center gap-2 mb-1">
                                                        <!-- User Avatar -->
                                                        <img :src="activity.user_avatar" :alt="activity.user_name"
                                                             class="w-6 h-6 rounded-full">
                                                        <span class="font-medium text-gray-800">{{ activity.user_name }}</span>
                                                        <span class="text-gray-500">{{ getActionVerb(activity.action) }}</span>
                                                        <span :class="['px-2 py-0.5 rounded text-xs font-medium', getEntityBadgeClass(activity.entity_type)]">
                                                            {{ activity.entity_type }}
                                                        </span>
                                                    </div>

                                                    <!-- Entity Title -->
                                                    <p class="text-gray-700 truncate">{{ activity.entity_title }}</p>

                                                    <!-- Details (if any) -->
                                                    <div v-if="Object.keys(activity.details).length > 0"
                                                         class="mt-2 text-sm text-gray-500 bg-gray-50 rounded p-2">
                                                        <span v-if="activity.details.old_value">
                                                            <span class="line-through text-red-400">{{ activity.details.old_value }}</span>
                                                            <span class="mx-2">-></span>
                                                            <span class="text-green-600">{{ activity.details.new_value }}</span>
                                                        </span>
                                                        <span v-else>{{ JSON.stringify(activity.details) }}</span>
                                                    </div>
                                                </div>

                                                <!-- Timestamp -->
                                                <div class="flex-shrink-0 text-right">
                                                    <span class="text-xs text-gray-400">{{ formatTime(activity.created_at) }}</span>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                </div>

                                <!-- Load More Indicator -->
                                <div v-if="hasMore" class="loading-indicator">
                                    <div v-if="loadingMore" class="flex items-center gap-2">
                                        <div class="animate-spin rounded-full h-5 w-5 border-b-2 border-[#003B4A]"></div>
                                        <span class="text-gray-500 text-sm">Carregando mais...</span>
                                    </div>
                                    <span v-else class="text-gray-400 text-sm">Role para carregar mais</span>
                                </div>
                            </template>
                        </div>
                    </div>
                </div>

                <!-- Right Sidebar - Statistics -->
                <aside class="w-72 flex-shrink-0">
                    <div class="bg-white rounded-xl shadow-sm p-4 sticky top-24">
                        <h3 class="font-semibold text-gray-800 mb-4">Estatisticas</h3>

                        <!-- Top Users -->
                        <div class="mb-6">
                            <h4 class="text-sm font-medium text-gray-600 mb-3">Usuarios Mais Ativos</h4>
                            <div class="space-y-2">
                                <div v-for="(user, index) in stats.top_users" :key="user.user"
                                     class="flex items-center gap-3">
                                    <span class="text-xs text-gray-400 w-4">{{ index + 1 }}.</span>
                                    <img :src="getUserAvatar(user.user)" :alt="user.user" class="w-6 h-6 rounded-full">
                                    <span class="flex-1 text-sm text-gray-700 truncate">{{ user.user }}</span>
                                    <span class="text-sm font-medium text-gray-800">{{ user.count }}</span>
                                </div>
                                <div v-if="!stats.top_users?.length" class="text-sm text-gray-400 text-center py-2">
                                    Sem dados
                                </div>
                            </div>
                        </div>

                        <!-- Activity by Type -->
                        <div class="mb-6">
                            <h4 class="text-sm font-medium text-gray-600 mb-3">Por Tipo de Entidade</h4>
                            <div class="space-y-2">
                                <div v-for="(count, type) in stats.by_entity_type" :key="type"
                                     class="flex items-center gap-2">
                                    <div :class="['w-3 h-3 rounded', getEntityDotClass(type)]"></div>
                                    <span class="flex-1 text-sm text-gray-600 capitalize">{{ type }}</span>
                                    <span class="text-sm font-medium text-gray-800">{{ count }}</span>
                                </div>
                            </div>
                        </div>

                        <!-- Activity by Day Chart -->
                        <div>
                            <h4 class="text-sm font-medium text-gray-600 mb-3">Atividade Diaria</h4>
                            <div class="h-20 flex items-end gap-1">
                                <div v-for="(count, day) in stats.by_day" :key="day"
                                     class="flex-1 bg-[#003B4A] rounded-t"
                                     :style="{ height: getBarHeight(count) + '%' }"
                                     :title="day + ': ' + count + ' atividades'">
                                </div>
                            </div>
                            <div class="text-xs text-gray-400 text-center mt-1">Ultimos 7 dias</div>
                        </div>
                    </div>
                </aside>
            </div>
        </main>
    </div>

    <script>
        const { createApp, ref, reactive, computed, onMounted, onUnmounted, nextTick } = Vue;

        createApp({
            setup() {
                // State
                const activities = ref([]);
                const grouped = ref({});
                const total = ref(0);
                const hasMore = ref(false);
                const loading = ref(true);
                const loadingMore = ref(false);
                const stats = ref({});
                const wsConnected = ref(false);
                const newActivitiesCount = ref(0);
                const newActivities = ref([]);
                const activityList = ref(null);
                let ws = null;
                let offset = 0;
                const limit = 50;

                // Filters
                const filters = reactive({
                    entity_type: '',
                    action: '',
                    user_id: '',
                    date_from: '',
                    date_to: ''
                });

                // Computed
                const uniqueUsers = computed(() => {
                    const users = new Map();
                    activities.value.forEach(a => {
                        if (!users.has(a.user_id)) {
                            users.set(a.user_id, { id: a.user_id, name: a.user_name });
                        }
                    });
                    return Array.from(users.values());
                });

                // Methods
                const loadActivities = async (reset = false) => {
                    if (reset) {
                        offset = 0;
                        activities.value = [];
                        loading.value = true;
                    } else {
                        loadingMore.value = true;
                    }

                    try {
                        const params = new URLSearchParams();
                        params.append('limit', limit);
                        params.append('offset', offset);
                        if (filters.entity_type) params.append('entity_type', filters.entity_type);
                        if (filters.action) params.append('action', filters.action);
                        if (filters.user_id) params.append('user_id', filters.user_id);
                        if (filters.date_from) params.append('date_from', filters.date_from);
                        if (filters.date_to) params.append('date_to', filters.date_to);

                        const res = await fetch('/api/activities?' + params.toString());
                        const data = await res.json();

                        if (reset) {
                            activities.value = data.activities;
                        } else {
                            activities.value = [...activities.value, ...data.activities];
                        }

                        grouped.value = data.grouped;
                        total.value = data.total;
                        hasMore.value = data.has_more;
                        offset = activities.value.length;
                    } catch (e) {
                        console.error('Error loading activities:', e);
                    } finally {
                        loading.value = false;
                        loadingMore.value = false;
                    }
                };

                const loadStats = async () => {
                    try {
                        const res = await fetch('/api/activities/stats?days=7');
                        stats.value = await res.json();
                    } catch (e) {
                        console.error('Error loading stats:', e);
                    }
                };

                const connectWebSocket = () => {
                    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
                    const wsUrl = `${protocol}//${window.location.host}/ws/activities`;

                    ws = new WebSocket(wsUrl);

                    ws.onopen = () => {
                        wsConnected.value = true;
                        console.log('WebSocket connected');
                    };

                    ws.onclose = () => {
                        wsConnected.value = false;
                        console.log('WebSocket disconnected, reconnecting...');
                        setTimeout(connectWebSocket, 3000);
                    };

                    ws.onerror = (error) => {
                        console.error('WebSocket error:', error);
                    };

                    ws.onmessage = (event) => {
                        const message = JSON.parse(event.data);
                        if (message.type === 'activity') {
                            // Check if activity passes current filters
                            const a = message.data;
                            if (filters.entity_type && a.entity_type !== filters.entity_type) return;
                            if (filters.action && a.action !== filters.action) return;
                            if (filters.user_id && a.user_id !== filters.user_id) return;

                            a.isNew = true;
                            newActivities.value.unshift(a);
                            newActivitiesCount.value++;
                        }
                    };

                    // Send ping every 30 seconds to keep connection alive
                    setInterval(() => {
                        if (ws && ws.readyState === WebSocket.OPEN) {
                            ws.send('ping');
                        }
                    }, 30000);
                };

                const showNewActivities = () => {
                    activities.value = [...newActivities.value, ...activities.value];
                    regroupActivities();
                    newActivities.value = [];
                    newActivitiesCount.value = 0;
                    total.value += newActivitiesCount.value;

                    // Scroll to top
                    if (activityList.value) {
                        activityList.value.scrollTop = 0;
                    }

                    // Remove new highlight after 3 seconds
                    setTimeout(() => {
                        activities.value.forEach(a => a.isNew = false);
                    }, 3000);
                };

                const regroupActivities = () => {
                    const today = new Date().toDateString();
                    const yesterday = new Date(Date.now() - 86400000).toDateString();
                    const result = {};

                    activities.value.forEach(a => {
                        const date = new Date(a.created_at).toDateString();
                        let key;
                        if (date === today) key = 'Hoje';
                        else if (date === yesterday) key = 'Ontem';
                        else key = new Date(a.created_at).toLocaleDateString('pt-BR', { day: 'numeric', month: 'long', year: 'numeric' });

                        if (!result[key]) result[key] = [];
                        result[key].push(a);
                    });

                    grouped.value = result;
                };

                const clearFilters = () => {
                    filters.entity_type = '';
                    filters.action = '';
                    filters.user_id = '';
                    filters.date_from = '';
                    filters.date_to = '';
                    loadActivities(true);
                };

                const handleScroll = (e) => {
                    const el = e.target;
                    const threshold = 100;

                    if (el.scrollTop + el.clientHeight >= el.scrollHeight - threshold) {
                        if (hasMore.value && !loadingMore.value) {
                            loadActivities(false);
                        }
                    }
                };

                const formatTime = (isoString) => {
                    const date = new Date(isoString);
                    const now = new Date();
                    const diffMs = now - date;
                    const diffMins = Math.floor(diffMs / 60000);
                    const diffHours = Math.floor(diffMs / 3600000);

                    if (diffMins < 1) return 'Agora';
                    if (diffMins < 60) return `${diffMins}min`;
                    if (diffHours < 24) return `${diffHours}h`;
                    return date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });
                };

                const getActionVerb = (action) => {
                    const verbs = {
                        created: 'criou',
                        updated: 'atualizou',
                        deleted: 'excluiu',
                        moved: 'moveu',
                        commented: 'comentou em',
                        assigned: 'atribuiu',
                        completed: 'completou'
                    };
                    return verbs[action] || action;
                };

                const getActionIconClass = (action) => {
                    const classes = {
                        created: 'bg-green-500',
                        updated: 'bg-blue-500',
                        deleted: 'bg-red-500',
                        moved: 'bg-purple-500',
                        commented: 'bg-yellow-500',
                        assigned: 'bg-cyan-500',
                        completed: 'bg-emerald-500'
                    };
                    return classes[action] || 'bg-gray-500';
                };

                const getActionIconPath = (action) => {
                    const paths = {
                        created: 'M12 4v16m8-8H4',
                        updated: 'M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z',
                        deleted: 'M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16',
                        moved: 'M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4',
                        commented: 'M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z',
                        assigned: 'M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z',
                        completed: 'M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z'
                    };
                    return paths[action] || 'M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z';
                };

                const getEntityBadgeClass = (type) => {
                    const classes = {
                        story: 'bg-blue-100 text-blue-700',
                        task: 'bg-green-100 text-green-700',
                        epic: 'bg-purple-100 text-purple-700',
                        sprint: 'bg-orange-100 text-orange-700',
                        comment: 'bg-yellow-100 text-yellow-700',
                        project: 'bg-gray-100 text-gray-700'
                    };
                    return classes[type] || 'bg-gray-100 text-gray-700';
                };

                const getEntityDotClass = (type) => {
                    const classes = {
                        story: 'bg-blue-500',
                        task: 'bg-green-500',
                        epic: 'bg-purple-500',
                        sprint: 'bg-orange-500',
                        comment: 'bg-yellow-500'
                    };
                    return classes[type] || 'bg-gray-500';
                };

                const getUserAvatar = (userName) => {
                    const initials = userName.split(' ').slice(0, 2).map(w => w[0]).join('').toUpperCase();
                    const colors = ['3B82F6', '10B981', '8B5CF6', 'F59E0B', 'EF4444', '06B6D4', 'EC4899'];
                    const color = colors[userName.split('').reduce((acc, c) => acc + c.charCodeAt(0), 0) % colors.length];
                    return `https://ui-avatars.com/api/?name=${initials}&background=${color}&color=fff&size=40`;
                };

                const getBarHeight = (count) => {
                    const maxCount = Math.max(...Object.values(stats.value.by_day || {}), 1);
                    return (count / maxCount) * 100;
                };

                // Lifecycle
                onMounted(() => {
                    loadActivities(true);
                    loadStats();
                    connectWebSocket();
                });

                onUnmounted(() => {
                    if (ws) {
                        ws.close();
                    }
                });

                return {
                    activities,
                    grouped,
                    total,
                    hasMore,
                    loading,
                    loadingMore,
                    stats,
                    wsConnected,
                    newActivitiesCount,
                    filters,
                    uniqueUsers,
                    activityList,
                    loadActivities,
                    clearFilters,
                    handleScroll,
                    showNewActivities,
                    formatTime,
                    getActionVerb,
                    getActionIconClass,
                    getActionIconPath,
                    getEntityBadgeClass,
                    getEntityDotClass,
                    getUserAvatar,
                    getBarHeight
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
"""


# =============================================================================
# ACTIVITY FEED SIDEBAR COMPONENT (for embedding in main dashboard)
# =============================================================================

ACTIVITY_FEED_SIDEBAR_HTML = '''
        <!-- Activity Feed Sidebar (Issue #233) -->
        <div v-if="showActivityFeed" class="fixed right-0 top-0 h-full w-96 bg-white shadow-xl z-40 flex flex-col">
            <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white">
                <div class="flex items-center gap-2">
                    <h3 class="font-semibold">Activity Feed</h3>
                    <span v-if="activityWsConnected" class="w-2 h-2 bg-green-400 rounded-full pulse-animation"></span>
                </div>
                <div class="flex items-center gap-2">
                    <a href="/activities" target="_blank" class="text-xs text-blue-200 hover:text-white">Ver tudo</a>
                    <button @click="showActivityFeed = false" class="text-white/70 hover:text-white text-xl">&times;</button>
                </div>
            </div>
            <div class="flex-1 overflow-y-auto">
                <div v-for="activity in recentActivities" :key="activity.id"
                     class="px-4 py-3 border-b border-gray-50 hover:bg-gray-50 transition">
                    <div class="flex gap-3">
                        <img :src="activity.user_avatar" :alt="activity.user_name" class="w-8 h-8 rounded-full">
                        <div class="flex-1 min-w-0">
                            <div class="text-sm">
                                <span class="font-medium text-gray-800">{{ activity.user_name }}</span>
                                <span class="text-gray-500">{{ getActivityVerb(activity.action) }}</span>
                            </div>
                            <p class="text-sm text-gray-600 truncate">{{ activity.entity_title }}</p>
                            <span class="text-xs text-gray-400">{{ formatActivityTime(activity.created_at) }}</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>
'''

ACTIVITY_FEED_VUE_DATA = '''
            // Activity Feed (Issue #233)
            const showActivityFeed = ref(false);
            const recentActivities = ref([]);
            const activityWsConnected = ref(false);
            let activityWs = null;
'''

ACTIVITY_FEED_VUE_METHODS = '''
            // Activity Feed Methods (Issue #233)
            const loadRecentActivities = async () => {
                try {
                    const res = await fetch('/api/activities?limit=20');
                    if (res.ok) {
                        const data = await res.json();
                        recentActivities.value = data.activities;
                    }
                } catch (e) { console.error('Error loading activities:', e); }
            };

            const connectActivityWebSocket = () => {
                const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
                const wsUrl = `${protocol}//${window.location.host}/ws/activities`;
                activityWs = new WebSocket(wsUrl);
                activityWs.onopen = () => { activityWsConnected.value = true; };
                activityWs.onclose = () => { activityWsConnected.value = false; setTimeout(connectActivityWebSocket, 3000); };
                activityWs.onmessage = (event) => {
                    const message = JSON.parse(event.data);
                    if (message.type === 'activity') {
                        recentActivities.value.unshift(message.data);
                        if (recentActivities.value.length > 50) recentActivities.value.pop();
                    }
                };
            };

            const getActivityVerb = (action) => {
                const verbs = { created: 'criou', updated: 'atualizou', deleted: 'excluiu', moved: 'moveu', commented: 'comentou em', assigned: 'atribuiu', completed: 'completou' };
                return verbs[action] || action;
            };

            const formatActivityTime = (isoString) => {
                const date = new Date(isoString);
                const now = new Date();
                const diffMs = now - date;
                const diffMins = Math.floor(diffMs / 60000);
                const diffHours = Math.floor(diffMs / 3600000);
                if (diffMins < 1) return 'Agora';
                if (diffMins < 60) return `${diffMins}min`;
                if (diffHours < 24) return `${diffHours}h`;
                return date.toLocaleDateString('pt-BR');
            };
'''

ACTIVITY_FEED_VUE_RETURN = '''
                // Activity Feed (Issue #233)
                showActivityFeed, recentActivities, activityWsConnected,
                loadRecentActivities, connectActivityWebSocket, getActivityVerb, formatActivityTime,
'''

ACTIVITY_FEED_SIDEBAR_BUTTON = '''
                    <!-- Activity Feed Button (Issue #233) -->
                    <button @click="showActivityFeed = !showActivityFeed; if (showActivityFeed) loadRecentActivities()"
                            class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"/>
                        </svg>
                        Activity Feed
                        <span v-if="activityWsConnected" class="w-2 h-2 bg-green-400 rounded-full ml-auto"></span>
                    </button>
'''

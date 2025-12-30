# -*- coding: utf-8 -*-
"""
Notification Center Module (Issue #227)
========================================
Complete notification center with bell icon in header.

Features:
1. Bell icon in header with unread count badge
2. Dropdown panel showing recent notifications
3. Notification types: info, success, warning, error, mention, assignment
4. Mark as read/unread functionality
5. Mark all as read
6. Notification preferences (which types to show)
7. Real-time updates via WebSocket (connects to existing /ws/notifications)
"""

from fastapi import FastAPI, HTTPException, Query, BackgroundTasks
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import json
import uuid


# =============================================================================
# NOTIFICATION STORAGE (In production, use database)
# =============================================================================

# User notifications storage: {user_id: [notification_dict, ...]}
notifications_store: Dict[str, List[dict]] = {}

# User preferences storage: {user_id: preferences_dict}
preferences_store: Dict[str, dict] = {}

# Default preferences
DEFAULT_PREFERENCES = {
    "enabled": True,
    "types": {
        "info": True,
        "success": True,
        "warning": True,
        "error": True,
        "mention": True,
        "assignment": True
    },
    "sound_enabled": True,
    "desktop_notifications": False,
    "email_notifications": False,
    "digest_frequency": "instant"  # instant, hourly, daily
}

# Notification type definitions
NOTIFICATION_TYPES = {
    "info": {
        "label": "Informacao",
        "icon": "info",
        "color": "blue",
        "bg_class": "bg-blue-100",
        "text_class": "text-blue-600",
        "border_class": "border-blue-200"
    },
    "success": {
        "label": "Sucesso",
        "icon": "check-circle",
        "color": "green",
        "bg_class": "bg-green-100",
        "text_class": "text-green-600",
        "border_class": "border-green-200"
    },
    "warning": {
        "label": "Aviso",
        "icon": "exclamation",
        "color": "yellow",
        "bg_class": "bg-yellow-100",
        "text_class": "text-yellow-600",
        "border_class": "border-yellow-200"
    },
    "error": {
        "label": "Erro",
        "icon": "x-circle",
        "color": "red",
        "bg_class": "bg-red-100",
        "text_class": "text-red-600",
        "border_class": "border-red-200"
    },
    "mention": {
        "label": "Mencao",
        "icon": "at-symbol",
        "color": "purple",
        "bg_class": "bg-purple-100",
        "text_class": "text-purple-600",
        "border_class": "border-purple-200"
    },
    "assignment": {
        "label": "Atribuicao",
        "icon": "user-plus",
        "color": "indigo",
        "bg_class": "bg-indigo-100",
        "text_class": "text-indigo-600",
        "border_class": "border-indigo-200"
    }
}


# =============================================================================
# PYDANTIC MODELS
# =============================================================================

class NotificationCreate(BaseModel):
    """Model for creating a notification"""
    type: str = "info"  # info, success, warning, error, mention, assignment
    title: str
    message: str
    url: Optional[str] = None
    data: Optional[Dict[str, Any]] = None
    priority: str = "normal"  # low, normal, high, urgent


class NotificationUpdate(BaseModel):
    """Model for updating notification"""
    read: Optional[bool] = None


class NotificationPreferences(BaseModel):
    """Model for user notification preferences"""
    enabled: bool = True
    types: Dict[str, bool] = None
    sound_enabled: bool = True
    desktop_notifications: bool = False
    email_notifications: bool = False
    digest_frequency: str = "instant"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_user_notifications(user_id: str) -> List[dict]:
    """Get all notifications for a user"""
    return notifications_store.get(user_id, [])


def get_user_preferences(user_id: str) -> dict:
    """Get user preferences or return defaults"""
    return preferences_store.get(user_id, DEFAULT_PREFERENCES.copy())


def create_notification(
    user_id: str,
    notif_type: str,
    title: str,
    message: str,
    url: Optional[str] = None,
    data: Optional[dict] = None,
    priority: str = "normal"
) -> dict:
    """Create a new notification for a user"""
    notif_id = f"notif_{uuid.uuid4().hex[:12]}"

    notification = {
        "id": notif_id,
        "user_id": user_id,
        "type": notif_type,
        "title": title,
        "message": message,
        "url": url,
        "data": data or {},
        "priority": priority,
        "read": False,
        "created_at": datetime.utcnow().isoformat() + "Z",
        "read_at": None
    }

    if user_id not in notifications_store:
        notifications_store[user_id] = []

    # Add to beginning (newest first)
    notifications_store[user_id].insert(0, notification)

    # Keep only last 100 notifications per user
    if len(notifications_store[user_id]) > 100:
        notifications_store[user_id] = notifications_store[user_id][:100]

    return notification


# =============================================================================
# REGISTER NOTIFICATION CENTER ENDPOINTS
# =============================================================================

def register_notification_center(app: FastAPI):
    """Register notification center API endpoints and page"""

    # -------------------------------------------------------------------------
    # API: List Notifications (with pagination)
    # -------------------------------------------------------------------------
    @app.get("/api/notifications")
    async def list_notifications(
        user_id: str = Query("default", description="User ID"),
        page: int = Query(1, ge=1, description="Page number"),
        page_size: int = Query(20, ge=1, le=100, description="Items per page"),
        type_filter: Optional[str] = Query(None, description="Filter by type"),
        unread_only: bool = Query(False, description="Only show unread")
    ):
        """List notifications with pagination"""
        notifications = get_user_notifications(user_id)

        # Apply filters
        if type_filter:
            notifications = [n for n in notifications if n["type"] == type_filter]

        if unread_only:
            notifications = [n for n in notifications if not n["read"]]

        # Calculate pagination
        total = len(notifications)
        start = (page - 1) * page_size
        end = start + page_size
        paginated = notifications[start:end]

        return {
            "notifications": paginated,
            "pagination": {
                "page": page,
                "page_size": page_size,
                "total": total,
                "total_pages": (total + page_size - 1) // page_size if page_size > 0 else 0,
                "has_next": end < total,
                "has_prev": page > 1
            }
        }

    # -------------------------------------------------------------------------
    # API: Get Unread Count
    # -------------------------------------------------------------------------
    @app.get("/api/notifications/unread-count")
    async def get_unread_count(user_id: str = Query("default")):
        """Get count of unread notifications"""
        notifications = get_user_notifications(user_id)
        unread = len([n for n in notifications if not n["read"]])

        # Count by type
        by_type = {}
        for n in notifications:
            if not n["read"]:
                t = n["type"]
                by_type[t] = by_type.get(t, 0) + 1

        return {
            "unread_count": unread,
            "by_type": by_type
        }

    # -------------------------------------------------------------------------
    # API: Create Notification
    # -------------------------------------------------------------------------
    @app.post("/api/notifications")
    async def create_notification_endpoint(
        notification: NotificationCreate,
        user_id: str = Query("default")
    ):
        """Create a new notification"""
        if notification.type not in NOTIFICATION_TYPES:
            raise HTTPException(400, f"Invalid notification type: {notification.type}")

        new_notif = create_notification(
            user_id=user_id,
            notif_type=notification.type,
            title=notification.title,
            message=notification.message,
            url=notification.url,
            data=notification.data,
            priority=notification.priority
        )

        return {
            "success": True,
            "notification": new_notif
        }

    # -------------------------------------------------------------------------
    # API: Mark as Read
    # -------------------------------------------------------------------------
    @app.post("/api/notifications/{notification_id}/read")
    async def mark_notification_read(
        notification_id: str,
        user_id: str = Query("default")
    ):
        """Mark a notification as read"""
        notifications = get_user_notifications(user_id)
        notif = next((n for n in notifications if n["id"] == notification_id), None)

        if not notif:
            raise HTTPException(404, "Notification not found")

        notif["read"] = True
        notif["read_at"] = datetime.utcnow().isoformat() + "Z"

        return {"success": True, "notification": notif}

    # -------------------------------------------------------------------------
    # API: Mark as Unread
    # -------------------------------------------------------------------------
    @app.post("/api/notifications/{notification_id}/unread")
    async def mark_notification_unread(
        notification_id: str,
        user_id: str = Query("default")
    ):
        """Mark a notification as unread"""
        notifications = get_user_notifications(user_id)
        notif = next((n for n in notifications if n["id"] == notification_id), None)

        if not notif:
            raise HTTPException(404, "Notification not found")

        notif["read"] = False
        notif["read_at"] = None

        return {"success": True, "notification": notif}

    # -------------------------------------------------------------------------
    # API: Mark All as Read
    # -------------------------------------------------------------------------
    @app.post("/api/notifications/read-all")
    async def mark_all_notifications_read(user_id: str = Query("default")):
        """Mark all notifications as read"""
        notifications = get_user_notifications(user_id)
        read_at = datetime.utcnow().isoformat() + "Z"
        count = 0

        for notif in notifications:
            if not notif["read"]:
                notif["read"] = True
                notif["read_at"] = read_at
                count += 1

        return {
            "success": True,
            "marked_count": count
        }

    # -------------------------------------------------------------------------
    # API: Delete Notification
    # -------------------------------------------------------------------------
    @app.delete("/api/notifications/{notification_id}")
    async def delete_notification(
        notification_id: str,
        user_id: str = Query("default")
    ):
        """Delete a notification"""
        if user_id not in notifications_store:
            raise HTTPException(404, "Notification not found")

        initial_count = len(notifications_store[user_id])
        notifications_store[user_id] = [
            n for n in notifications_store[user_id]
            if n["id"] != notification_id
        ]

        if len(notifications_store[user_id]) == initial_count:
            raise HTTPException(404, "Notification not found")

        return {"success": True}

    # -------------------------------------------------------------------------
    # API: Get Preferences
    # -------------------------------------------------------------------------
    @app.get("/api/notifications/preferences")
    async def get_notification_preferences(user_id: str = Query("default")):
        """Get user notification preferences"""
        prefs = get_user_preferences(user_id)
        return {
            "preferences": prefs,
            "available_types": NOTIFICATION_TYPES
        }

    # -------------------------------------------------------------------------
    # API: Update Preferences
    # -------------------------------------------------------------------------
    @app.put("/api/notifications/preferences")
    async def update_notification_preferences(
        preferences: NotificationPreferences,
        user_id: str = Query("default")
    ):
        """Update user notification preferences"""
        current = get_user_preferences(user_id)

        # Update fields
        current["enabled"] = preferences.enabled
        if preferences.types:
            current["types"] = preferences.types
        current["sound_enabled"] = preferences.sound_enabled
        current["desktop_notifications"] = preferences.desktop_notifications
        current["email_notifications"] = preferences.email_notifications
        current["digest_frequency"] = preferences.digest_frequency

        preferences_store[user_id] = current

        return {
            "success": True,
            "preferences": current
        }

    # -------------------------------------------------------------------------
    # API: Clear All Notifications
    # -------------------------------------------------------------------------
    @app.delete("/api/notifications")
    async def clear_all_notifications(
        user_id: str = Query("default"),
        read_only: bool = Query(True, description="Only clear read notifications")
    ):
        """Clear notifications"""
        if user_id not in notifications_store:
            return {"success": True, "cleared": 0}

        initial_count = len(notifications_store[user_id])

        if read_only:
            notifications_store[user_id] = [
                n for n in notifications_store[user_id] if not n["read"]
            ]
        else:
            notifications_store[user_id] = []

        cleared = initial_count - len(notifications_store[user_id])

        return {"success": True, "cleared": cleared}

    # -------------------------------------------------------------------------
    # API: Get Notification Types
    # -------------------------------------------------------------------------
    @app.get("/api/notifications/types")
    async def get_notification_types():
        """Get available notification types"""
        return {
            "types": NOTIFICATION_TYPES
        }

    # -------------------------------------------------------------------------
    # Demo: Add Sample Notifications
    # -------------------------------------------------------------------------
    @app.post("/api/notifications/demo")
    async def create_demo_notifications(user_id: str = Query("default")):
        """Create demo notifications for testing"""
        samples = [
            ("assignment", "Nova Story Atribuida", "Voce foi atribuido a story STR-0042: Implementar login social", "/stories/STR-0042"),
            ("mention", "Voce foi mencionado", "@voce foi mencionado em um comentario na story STR-0041", "/stories/STR-0041"),
            ("success", "Sprint Concluido", "Sprint 12 foi concluido com sucesso! 95% das stories entregues.", "/sprints/12"),
            ("warning", "Prazo Proximo", "A story STR-0039 esta com prazo para amanha!", "/stories/STR-0039"),
            ("error", "Build Falhou", "O build do projeto falhou. Verifique os logs.", "/builds/latest"),
            ("info", "Atualizacao Disponivel", "Uma nova versao do sistema esta disponivel.", None),
        ]

        created = []
        for notif_type, title, message, url in samples:
            notif = create_notification(
                user_id=user_id,
                notif_type=notif_type,
                title=title,
                message=message,
                url=url
            )
            created.append(notif)

        return {
            "success": True,
            "created": len(created),
            "notifications": created
        }

    print("[Dashboard] Notification Center endpoints loaded: /api/notifications/*")


# =============================================================================
# NOTIFICATION CENTER HTML COMPONENT
# =============================================================================

def get_notification_center_html():
    """Returns the HTML for the notification center bell icon and dropdown"""
    return '''
    <!-- Notification Center Bell Icon (Issue #227) -->
    <div class="notification-center-wrapper relative" v-if="notificationCenterEnabled">
        <!-- Bell Button with Badge -->
        <button @click="toggleNotificationCenter"
                class="notification-bell-btn relative p-2 hover:bg-white/10 rounded-lg transition-colors"
                :class="{ 'notification-bell-active': showNotificationCenter }"
                title="Notificacoes"
                aria-label="Abrir centro de notificacoes">
            <!-- Bell Icon SVG -->
            <svg class="w-6 h-6 text-white/80 hover:text-white transition-colors"
                 fill="none"
                 stroke="currentColor"
                 viewBox="0 0 24 24"
                 :class="{ 'animate-bell-ring': hasNewNotification }">
                <path stroke-linecap="round"
                      stroke-linejoin="round"
                      stroke-width="2"
                      d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"/>
            </svg>

            <!-- Unread Badge -->
            <span v-if="notificationUnreadCount > 0"
                  class="notification-badge absolute -top-1 -right-1 min-w-[20px] h-5 px-1 bg-red-500 text-white text-xs font-bold rounded-full flex items-center justify-center animate-pulse">
                {{ notificationUnreadCount > 99 ? '99+' : notificationUnreadCount }}
            </span>
        </button>

        <!-- Notification Dropdown Panel -->
        <transition name="notification-dropdown">
            <div v-if="showNotificationCenter"
                 class="notification-dropdown absolute right-0 top-full mt-2 w-96 max-w-[calc(100vw-2rem)] bg-white rounded-xl shadow-2xl z-50 overflow-hidden"
                 @click.stop>

                <!-- Header -->
                <div class="notification-header px-4 py-3 bg-gradient-to-r from-[#003B4A] to-[#004d5c] text-white">
                    <div class="flex items-center justify-between">
                        <h3 class="font-semibold text-lg">Notificacoes</h3>
                        <div class="flex items-center gap-2">
                            <!-- Mark All Read Button -->
                            <button v-if="notificationUnreadCount > 0"
                                    @click="markAllNotificationsRead"
                                    class="text-xs text-white/70 hover:text-white transition-colors px-2 py-1 rounded hover:bg-white/10"
                                    title="Marcar todas como lidas">
                                Marcar todas como lidas
                            </button>
                            <!-- Settings Button -->
                            <button @click="showNotificationSettings = !showNotificationSettings"
                                    class="p-1 hover:bg-white/10 rounded transition-colors"
                                    title="Configuracoes">
                                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"/>
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
                                </svg>
                            </button>
                            <!-- Close Button -->
                            <button @click="showNotificationCenter = false"
                                    class="p-1 hover:bg-white/10 rounded transition-colors"
                                    title="Fechar">
                                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                                </svg>
                            </button>
                        </div>
                    </div>

                    <!-- Unread Count Badge -->
                    <div v-if="notificationUnreadCount > 0" class="mt-1 text-sm text-white/70">
                        {{ notificationUnreadCount }} {{ notificationUnreadCount === 1 ? 'notificacao nao lida' : 'notificacoes nao lidas' }}
                    </div>
                </div>

                <!-- Filter Tabs -->
                <div class="notification-filters px-4 py-2 border-b flex gap-1 overflow-x-auto bg-gray-50">
                    <button @click="notificationFilter = 'all'"
                            :class="['px-3 py-1.5 rounded-lg text-sm font-medium transition-colors whitespace-nowrap',
                                     notificationFilter === 'all' ? 'bg-[#003B4A] text-white' : 'bg-white text-gray-600 hover:bg-gray-100']">
                        Todas
                    </button>
                    <button @click="notificationFilter = 'unread'"
                            :class="['px-3 py-1.5 rounded-lg text-sm font-medium transition-colors whitespace-nowrap',
                                     notificationFilter === 'unread' ? 'bg-[#003B4A] text-white' : 'bg-white text-gray-600 hover:bg-gray-100']">
                        Nao lidas
                    </button>
                    <button @click="notificationFilter = 'mention'"
                            :class="['px-3 py-1.5 rounded-lg text-sm font-medium transition-colors whitespace-nowrap',
                                     notificationFilter === 'mention' ? 'bg-purple-600 text-white' : 'bg-white text-gray-600 hover:bg-gray-100']">
                        Mencoes
                    </button>
                    <button @click="notificationFilter = 'assignment'"
                            :class="['px-3 py-1.5 rounded-lg text-sm font-medium transition-colors whitespace-nowrap',
                                     notificationFilter === 'assignment' ? 'bg-indigo-600 text-white' : 'bg-white text-gray-600 hover:bg-gray-100']">
                        Atribuicoes
                    </button>
                </div>

                <!-- Settings Panel (collapsible) -->
                <transition name="settings-slide">
                    <div v-if="showNotificationSettings" class="notification-settings px-4 py-3 bg-gray-50 border-b">
                        <h4 class="font-medium text-sm text-gray-700 mb-3">Preferencias de Notificacao</h4>

                        <!-- Enable/Disable All -->
                        <label class="flex items-center justify-between py-2 cursor-pointer">
                            <span class="text-sm text-gray-600">Notificacoes ativas</span>
                            <input type="checkbox" v-model="notificationPrefs.enabled"
                                   @change="saveNotificationPrefs"
                                   class="form-checkbox h-5 w-5 text-[#003B4A] rounded">
                        </label>

                        <!-- Sound Toggle -->
                        <label class="flex items-center justify-between py-2 cursor-pointer">
                            <span class="text-sm text-gray-600">Som de notificacao</span>
                            <input type="checkbox" v-model="notificationPrefs.sound_enabled"
                                   @change="saveNotificationPrefs"
                                   class="form-checkbox h-5 w-5 text-[#003B4A] rounded">
                        </label>

                        <!-- Type Filters -->
                        <div class="mt-2 pt-2 border-t">
                            <span class="text-xs text-gray-500 uppercase tracking-wider">Tipos de notificacao</span>
                            <div class="mt-2 grid grid-cols-2 gap-2">
                                <label v-for="(typeInfo, typeKey) in notificationTypes" :key="typeKey"
                                       class="flex items-center gap-2 cursor-pointer">
                                    <input type="checkbox"
                                           v-model="notificationPrefs.types[typeKey]"
                                           @change="saveNotificationPrefs"
                                           class="form-checkbox h-4 w-4 rounded"
                                           :class="'text-' + typeInfo.color + '-600'">
                                    <span class="text-sm text-gray-600">{{ typeInfo.label }}</span>
                                </label>
                            </div>
                        </div>
                    </div>
                </transition>

                <!-- Notifications List -->
                <div class="notification-list max-h-[400px] overflow-y-auto">
                    <!-- Empty State -->
                    <div v-if="filteredNotifications.length === 0"
                         class="empty-state py-12 px-4 text-center">
                        <div class="w-16 h-16 mx-auto mb-4 rounded-full bg-gray-100 flex items-center justify-center">
                            <svg class="w-8 h-8 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"/>
                            </svg>
                        </div>
                        <p class="text-gray-500 font-medium">Nenhuma notificacao</p>
                        <p class="text-gray-400 text-sm mt-1">
                            {{ notificationFilter !== 'all' ? 'Tente outro filtro' : 'Voce esta em dia!' }}
                        </p>
                    </div>

                    <!-- Notification Items -->
                    <div v-for="notif in filteredNotifications"
                         :key="notif.id"
                         @click="handleNotificationClick(notif)"
                         :class="['notification-item px-4 py-3 border-b hover:bg-gray-50 cursor-pointer transition-colors',
                                  !notif.read ? 'bg-blue-50/50' : '']">
                        <div class="flex gap-3">
                            <!-- Type Icon -->
                            <div :class="['notification-icon w-10 h-10 rounded-full flex items-center justify-center flex-shrink-0',
                                          getNotificationBgClass(notif.type)]">
                                <span :class="['text-lg', getNotificationTextClass(notif.type)]">
                                    {{ getNotificationEmoji(notif.type) }}
                                </span>
                            </div>

                            <!-- Content -->
                            <div class="flex-1 min-w-0">
                                <div class="flex items-start justify-between gap-2">
                                    <span class="font-medium text-sm text-gray-900 line-clamp-1">{{ notif.title }}</span>
                                    <span class="text-xs text-gray-400 whitespace-nowrap flex-shrink-0">
                                        {{ formatNotificationTime(notif.created_at) }}
                                    </span>
                                </div>
                                <p class="text-sm text-gray-600 mt-0.5 line-clamp-2">{{ notif.message }}</p>

                                <!-- Priority Badge (for high/urgent) -->
                                <span v-if="notif.priority === 'high' || notif.priority === 'urgent'"
                                      :class="['inline-block mt-1 px-2 py-0.5 rounded text-xs font-medium',
                                               notif.priority === 'urgent' ? 'bg-red-100 text-red-700' : 'bg-orange-100 text-orange-700']">
                                    {{ notif.priority === 'urgent' ? 'Urgente' : 'Alta prioridade' }}
                                </span>
                            </div>

                            <!-- Actions -->
                            <div class="flex flex-col gap-1 flex-shrink-0">
                                <!-- Unread Indicator -->
                                <div v-if="!notif.read"
                                     class="w-2.5 h-2.5 bg-blue-500 rounded-full"
                                     title="Nao lida"></div>

                                <!-- Delete Button -->
                                <button @click.stop="deleteNotification(notif.id)"
                                        class="p-1 text-gray-400 hover:text-red-500 transition-colors opacity-0 group-hover:opacity-100"
                                        title="Remover">
                                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                                    </svg>
                                </button>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Footer -->
                <div class="notification-footer px-4 py-3 bg-gray-50 border-t flex items-center justify-between">
                    <button @click="loadMoreNotifications"
                            v-if="hasMoreNotifications"
                            class="text-sm text-[#003B4A] hover:text-[#004d5c] font-medium">
                        Carregar mais
                    </button>
                    <span v-else class="text-sm text-gray-400">
                        {{ notifications.length }} notificacoes
                    </span>

                    <button @click="clearReadNotifications"
                            class="text-sm text-gray-500 hover:text-red-600 transition-colors">
                        Limpar lidas
                    </button>
                </div>
            </div>
        </transition>

        <!-- Click Outside Overlay -->
        <div v-if="showNotificationCenter"
             class="fixed inset-0 z-40"
             @click="showNotificationCenter = false"></div>
    </div>
    '''


# =============================================================================
# NOTIFICATION CENTER CSS
# =============================================================================

def get_notification_center_css():
    """Returns CSS styles for notification center"""
    return '''
    /* Notification Center Styles (Issue #227) */

    /* Bell Animation */
    @keyframes bell-ring {
        0%, 100% { transform: rotate(0deg); }
        10%, 30%, 50%, 70%, 90% { transform: rotate(10deg); }
        20%, 40%, 60%, 80% { transform: rotate(-10deg); }
    }

    .animate-bell-ring {
        animation: bell-ring 0.5s ease-in-out;
    }

    /* Badge Pulse */
    .notification-badge {
        animation: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite;
    }

    @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.7; }
    }

    /* Dropdown Transition */
    .notification-dropdown-enter-active,
    .notification-dropdown-leave-active {
        transition: all 0.2s ease;
    }

    .notification-dropdown-enter-from,
    .notification-dropdown-leave-to {
        opacity: 0;
        transform: translateY(-10px) scale(0.95);
    }

    /* Settings Slide Transition */
    .settings-slide-enter-active,
    .settings-slide-leave-active {
        transition: all 0.3s ease;
    }

    .settings-slide-enter-from,
    .settings-slide-leave-to {
        opacity: 0;
        max-height: 0;
        padding-top: 0;
        padding-bottom: 0;
    }

    /* Notification Item Hover */
    .notification-item {
        position: relative;
    }

    .notification-item:hover .opacity-0 {
        opacity: 1;
    }

    /* Line Clamp */
    .line-clamp-1 {
        display: -webkit-box;
        -webkit-line-clamp: 1;
        -webkit-box-orient: vertical;
        overflow: hidden;
    }

    .line-clamp-2 {
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
        overflow: hidden;
    }

    /* Custom Scrollbar for Notification List */
    .notification-list::-webkit-scrollbar {
        width: 6px;
    }

    .notification-list::-webkit-scrollbar-track {
        background: #f1f1f1;
    }

    .notification-list::-webkit-scrollbar-thumb {
        background: #cbd5e1;
        border-radius: 3px;
    }

    .notification-list::-webkit-scrollbar-thumb:hover {
        background: #94a3b8;
    }

    /* Mobile Responsive */
    @media (max-width: 640px) {
        .notification-dropdown {
            position: fixed;
            top: 60px;
            left: 0;
            right: 0;
            width: 100%;
            max-width: 100%;
            border-radius: 0;
            max-height: calc(100vh - 60px);
        }

        .notification-list {
            max-height: calc(100vh - 280px);
        }
    }
    '''


# =============================================================================
# NOTIFICATION CENTER JAVASCRIPT
# =============================================================================

def get_notification_center_js():
    """Returns JavaScript/Vue.js code for notification center"""
    return '''
    // Notification Center State (Issue #227)
    notificationCenterEnabled: true,
    showNotificationCenter: false,
    showNotificationSettings: false,
    notifications: [],
    notificationUnreadCount: 0,
    notificationFilter: 'all',
    notificationPage: 1,
    hasMoreNotifications: false,
    hasNewNotification: false,
    notificationTypes: {
        info: { label: 'Informacao', color: 'blue' },
        success: { label: 'Sucesso', color: 'green' },
        warning: { label: 'Aviso', color: 'yellow' },
        error: { label: 'Erro', color: 'red' },
        mention: { label: 'Mencao', color: 'purple' },
        assignment: { label: 'Atribuicao', color: 'indigo' }
    },
    notificationPrefs: {
        enabled: true,
        sound_enabled: true,
        types: {
            info: true,
            success: true,
            warning: true,
            error: true,
            mention: true,
            assignment: true
        }
    },

    // Notification Center Methods
    async loadNotifications() {
        try {
            const userId = this.currentUser || 'default';
            const typeFilter = this.notificationFilter !== 'all' && this.notificationFilter !== 'unread'
                ? `&type_filter=${this.notificationFilter}` : '';
            const unreadOnly = this.notificationFilter === 'unread' ? '&unread_only=true' : '';

            const response = await fetch(
                `/api/notifications?user_id=${userId}&page=${this.notificationPage}&page_size=20${typeFilter}${unreadOnly}`
            );
            const data = await response.json();

            if (this.notificationPage === 1) {
                this.notifications = data.notifications;
            } else {
                this.notifications = [...this.notifications, ...data.notifications];
            }

            this.hasMoreNotifications = data.pagination.has_next;
        } catch (e) {
            console.error('Error loading notifications:', e);
        }
    },

    async loadUnreadCount() {
        try {
            const userId = this.currentUser || 'default';
            const response = await fetch(`/api/notifications/unread-count?user_id=${userId}`);
            const data = await response.json();
            this.notificationUnreadCount = data.unread_count;
        } catch (e) {
            console.error('Error loading unread count:', e);
        }
    },

    async loadNotificationPrefs() {
        try {
            const userId = this.currentUser || 'default';
            const response = await fetch(`/api/notifications/preferences?user_id=${userId}`);
            const data = await response.json();
            this.notificationPrefs = data.preferences;
        } catch (e) {
            console.error('Error loading notification preferences:', e);
        }
    },

    async saveNotificationPrefs() {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/notifications/preferences?user_id=${userId}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(this.notificationPrefs)
            });
        } catch (e) {
            console.error('Error saving notification preferences:', e);
        }
    },

    toggleNotificationCenter() {
        this.showNotificationCenter = !this.showNotificationCenter;
        if (this.showNotificationCenter) {
            this.notificationPage = 1;
            this.loadNotifications();
            this.hasNewNotification = false;
        }
    },

    get filteredNotifications() {
        return this.notifications;
    },

    async handleNotificationClick(notification) {
        // Mark as read if unread
        if (!notification.read) {
            await this.markNotificationRead(notification.id);
        }

        // Navigate to URL if provided
        if (notification.url) {
            this.showNotificationCenter = false;
            // Handle internal navigation
            if (notification.url.startsWith('/stories/')) {
                const storyId = notification.url.split('/stories/')[1];
                this.selectStory({ story_id: storyId });
            } else {
                window.location.href = notification.url;
            }
        }
    },

    async markNotificationRead(notificationId) {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/notifications/${notificationId}/read?user_id=${userId}`, {
                method: 'POST'
            });

            const notif = this.notifications.find(n => n.id === notificationId);
            if (notif) {
                notif.read = true;
                this.notificationUnreadCount = Math.max(0, this.notificationUnreadCount - 1);
            }
        } catch (e) {
            console.error('Error marking notification as read:', e);
        }
    },

    async markAllNotificationsRead() {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/notifications/read-all?user_id=${userId}`, {
                method: 'POST'
            });

            this.notifications.forEach(n => n.read = true);
            this.notificationUnreadCount = 0;
        } catch (e) {
            console.error('Error marking all as read:', e);
        }
    },

    async deleteNotification(notificationId) {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/notifications/${notificationId}?user_id=${userId}`, {
                method: 'DELETE'
            });

            const idx = this.notifications.findIndex(n => n.id === notificationId);
            if (idx >= 0) {
                if (!this.notifications[idx].read) {
                    this.notificationUnreadCount = Math.max(0, this.notificationUnreadCount - 1);
                }
                this.notifications.splice(idx, 1);
            }
        } catch (e) {
            console.error('Error deleting notification:', e);
        }
    },

    async clearReadNotifications() {
        try {
            const userId = this.currentUser || 'default';
            await fetch(`/api/notifications?user_id=${userId}&read_only=true`, {
                method: 'DELETE'
            });

            this.notifications = this.notifications.filter(n => !n.read);
        } catch (e) {
            console.error('Error clearing notifications:', e);
        }
    },

    loadMoreNotifications() {
        this.notificationPage++;
        this.loadNotifications();
    },

    // Helper methods for notification styling
    getNotificationEmoji(type) {
        const emojis = {
            info: 'info',
            success: 'check',
            warning: 'warn',
            error: 'error',
            mention: '@',
            assignment: 'user'
        };
        return emojis[type] || 'bell';
    },

    getNotificationBgClass(type) {
        const classes = {
            info: 'bg-blue-100',
            success: 'bg-green-100',
            warning: 'bg-yellow-100',
            error: 'bg-red-100',
            mention: 'bg-purple-100',
            assignment: 'bg-indigo-100'
        };
        return classes[type] || 'bg-gray-100';
    },

    getNotificationTextClass(type) {
        const classes = {
            info: 'text-blue-600',
            success: 'text-green-600',
            warning: 'text-yellow-600',
            error: 'text-red-600',
            mention: 'text-purple-600',
            assignment: 'text-indigo-600'
        };
        return classes[type] || 'text-gray-600';
    },

    formatNotificationTime(dateStr) {
        const date = new Date(dateStr);
        const now = new Date();
        const diff = Math.floor((now - date) / 1000);

        if (diff < 60) return 'agora';
        if (diff < 3600) return `${Math.floor(diff / 60)}min`;
        if (diff < 86400) return `${Math.floor(diff / 3600)}h`;
        if (diff < 604800) return `${Math.floor(diff / 86400)}d`;
        return date.toLocaleDateString('pt-BR', { day: '2-digit', month: '2-digit' });
    },

    // WebSocket notification handler
    handleWebSocketNotificationCenter(data) {
        if (data.type === 'notification') {
            // Add new notification to the top
            this.notifications.unshift(data.notification);
            this.notificationUnreadCount++;
            this.hasNewNotification = true;

            // Play sound if enabled
            if (this.notificationPrefs.sound_enabled && this.notificationSoundEnabled) {
                try { this.notificationSound.play(); } catch(e) {}
            }

            // Show toast
            this.addToast(data.notification.type, data.notification.title, data.notification.message);
        }
    },

    initNotificationCenter() {
        this.loadUnreadCount();
        this.loadNotificationPrefs();

        // Poll for new notifications every 30 seconds
        setInterval(() => {
            this.loadUnreadCount();
        }, 30000);
    }
    '''


# =============================================================================
# NOTIFICATION CENTER PAGE
# =============================================================================

def get_notification_center_page_html():
    """Returns full HTML page for notifications settings"""
    return '''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Central de Notificacoes - Fabrica de Agentes</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.prod.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <style>
        body { font-family: 'Inter', sans-serif; background: #f8fafc; }
    </style>
</head>
<body>
    <div id="app" class="min-h-screen">
        <!-- Header -->
        <header class="bg-gradient-to-r from-[#003B4A] to-[#004d5c] text-white px-6 py-4 shadow-lg">
            <div class="max-w-4xl mx-auto flex items-center justify-between">
                <div class="flex items-center gap-4">
                    <a href="/" class="text-white/70 hover:text-white">
                        <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18"/>
                        </svg>
                    </a>
                    <h1 class="text-xl font-semibold">Central de Notificacoes</h1>
                </div>
                <div class="flex items-center gap-2">
                    <span class="bg-white/20 px-3 py-1 rounded-full text-sm">
                        {{ unreadCount }} nao lidas
                    </span>
                </div>
            </div>
        </header>

        <!-- Main Content -->
        <main class="max-w-4xl mx-auto p-6">
            <!-- Filters -->
            <div class="bg-white rounded-xl shadow-sm p-4 mb-6">
                <div class="flex flex-wrap gap-2">
                    <button @click="filter = 'all'"
                            :class="['px-4 py-2 rounded-lg text-sm font-medium transition-colors',
                                     filter === 'all' ? 'bg-[#003B4A] text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200']">
                        Todas ({{ notifications.length }})
                    </button>
                    <button @click="filter = 'unread'"
                            :class="['px-4 py-2 rounded-lg text-sm font-medium transition-colors',
                                     filter === 'unread' ? 'bg-[#003B4A] text-white' : 'bg-gray-100 text-gray-600 hover:bg-gray-200']">
                        Nao lidas ({{ unreadCount }})
                    </button>
                    <button v-for="(info, type) in notificationTypes" :key="type"
                            @click="filter = type"
                            :class="['px-4 py-2 rounded-lg text-sm font-medium transition-colors',
                                     filter === type ? `bg-${info.color}-600 text-white` : 'bg-gray-100 text-gray-600 hover:bg-gray-200']">
                        {{ info.label }}
                    </button>
                </div>
            </div>

            <!-- Actions -->
            <div class="flex justify-between items-center mb-4">
                <button @click="markAllRead"
                        v-if="unreadCount > 0"
                        class="text-sm text-[#003B4A] hover:underline">
                    Marcar todas como lidas
                </button>
                <button @click="clearRead"
                        class="text-sm text-gray-500 hover:text-red-600">
                    Limpar lidas
                </button>
            </div>

            <!-- Notifications List -->
            <div class="space-y-3">
                <div v-if="filteredNotifications.length === 0"
                     class="bg-white rounded-xl shadow-sm p-12 text-center">
                    <div class="w-16 h-16 mx-auto mb-4 rounded-full bg-gray-100 flex items-center justify-center">
                        <svg class="w-8 h-8 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"/>
                        </svg>
                    </div>
                    <p class="text-gray-500 font-medium">Nenhuma notificacao encontrada</p>
                </div>

                <div v-for="notif in filteredNotifications" :key="notif.id"
                     :class="['bg-white rounded-xl shadow-sm p-4 transition-all hover:shadow-md cursor-pointer',
                              !notif.read ? 'border-l-4 border-blue-500' : '']"
                     @click="toggleRead(notif)">
                    <div class="flex gap-4">
                        <div :class="['w-12 h-12 rounded-full flex items-center justify-center flex-shrink-0',
                                      getBgClass(notif.type)]">
                            <span class="text-xl">{{ getIcon(notif.type) }}</span>
                        </div>
                        <div class="flex-1">
                            <div class="flex items-start justify-between">
                                <div>
                                    <h3 class="font-semibold text-gray-900">{{ notif.title }}</h3>
                                    <p class="text-gray-600 mt-1">{{ notif.message }}</p>
                                </div>
                                <div class="flex items-center gap-2">
                                    <span class="text-sm text-gray-400">{{ formatTime(notif.created_at) }}</span>
                                    <button @click.stop="deleteNotif(notif.id)"
                                            class="p-1 text-gray-400 hover:text-red-500">
                                        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                                        </svg>
                                    </button>
                                </div>
                            </div>
                            <div class="flex items-center gap-2 mt-2">
                                <span :class="['px-2 py-0.5 rounded text-xs font-medium', getBgClass(notif.type), getTextClass(notif.type)]">
                                    {{ notificationTypes[notif.type]?.label || notif.type }}
                                </span>
                                <span v-if="notif.priority === 'high'" class="px-2 py-0.5 rounded text-xs font-medium bg-orange-100 text-orange-700">
                                    Alta prioridade
                                </span>
                                <span v-if="notif.priority === 'urgent'" class="px-2 py-0.5 rounded text-xs font-medium bg-red-100 text-red-700">
                                    Urgente
                                </span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Load More -->
            <div v-if="hasMore" class="text-center mt-6">
                <button @click="loadMore"
                        class="px-6 py-2 bg-[#003B4A] text-white rounded-lg hover:bg-[#004d5c] transition-colors">
                    Carregar mais
                </button>
            </div>
        </main>
    </div>

    <script>
    const { createApp, ref, computed, onMounted } = Vue;

    createApp({
        setup() {
            const notifications = ref([]);
            const filter = ref('all');
            const page = ref(1);
            const hasMore = ref(false);

            const notificationTypes = {
                info: { label: 'Informacao', color: 'blue' },
                success: { label: 'Sucesso', color: 'green' },
                warning: { label: 'Aviso', color: 'yellow' },
                error: { label: 'Erro', color: 'red' },
                mention: { label: 'Mencao', color: 'purple' },
                assignment: { label: 'Atribuicao', color: 'indigo' }
            };

            const unreadCount = computed(() =>
                notifications.value.filter(n => !n.read).length
            );

            const filteredNotifications = computed(() => {
                if (filter.value === 'all') return notifications.value;
                if (filter.value === 'unread') return notifications.value.filter(n => !n.read);
                return notifications.value.filter(n => n.type === filter.value);
            });

            const loadNotifications = async () => {
                const response = await fetch(`/api/notifications?user_id=default&page=${page.value}`);
                const data = await response.json();
                if (page.value === 1) {
                    notifications.value = data.notifications;
                } else {
                    notifications.value.push(...data.notifications);
                }
                hasMore.value = data.pagination.has_next;
            };

            const loadMore = () => {
                page.value++;
                loadNotifications();
            };

            const toggleRead = async (notif) => {
                const endpoint = notif.read ? 'unread' : 'read';
                await fetch(`/api/notifications/${notif.id}/${endpoint}?user_id=default`, { method: 'POST' });
                notif.read = !notif.read;
            };

            const markAllRead = async () => {
                await fetch('/api/notifications/read-all?user_id=default', { method: 'POST' });
                notifications.value.forEach(n => n.read = true);
            };

            const clearRead = async () => {
                await fetch('/api/notifications?user_id=default&read_only=true', { method: 'DELETE' });
                notifications.value = notifications.value.filter(n => !n.read);
            };

            const deleteNotif = async (id) => {
                await fetch(`/api/notifications/${id}?user_id=default`, { method: 'DELETE' });
                notifications.value = notifications.value.filter(n => n.id !== id);
            };

            const getIcon = (type) => {
                const icons = { info: 'i', success: 'check', warning: '!', error: 'X', mention: '@', assignment: '+' };
                return icons[type] || 'bell';
            };

            const getBgClass = (type) => {
                const classes = { info: 'bg-blue-100', success: 'bg-green-100', warning: 'bg-yellow-100', error: 'bg-red-100', mention: 'bg-purple-100', assignment: 'bg-indigo-100' };
                return classes[type] || 'bg-gray-100';
            };

            const getTextClass = (type) => {
                const classes = { info: 'text-blue-600', success: 'text-green-600', warning: 'text-yellow-600', error: 'text-red-600', mention: 'text-purple-600', assignment: 'text-indigo-600' };
                return classes[type] || 'text-gray-600';
            };

            const formatTime = (dateStr) => {
                const date = new Date(dateStr);
                const now = new Date();
                const diff = Math.floor((now - date) / 1000);
                if (diff < 60) return 'agora';
                if (diff < 3600) return `${Math.floor(diff / 60)}min`;
                if (diff < 86400) return `${Math.floor(diff / 3600)}h`;
                return date.toLocaleDateString('pt-BR');
            };

            onMounted(() => {
                loadNotifications();
            });

            return {
                notifications, filter, hasMore, notificationTypes, unreadCount,
                filteredNotifications, loadMore, toggleRead, markAllRead, clearRead,
                deleteNotif, getIcon, getBgClass, getTextClass, formatTime
            };
        }
    }).mount('#app');
    </script>
</body>
</html>
'''


# =============================================================================
# MAIN REGISTRATION FUNCTION
# =============================================================================

def register_notification_center_page(app: FastAPI):
    """Register the notification center page route"""

    @app.get("/notification-center", response_class=HTMLResponse)
    async def notification_center_page():
        """Notification Center full page"""
        return get_notification_center_page_html()

    print("[Dashboard] Notification Center page loaded: /notification-center")


# =============================================================================
# MODULE ENTRY POINT
# =============================================================================

if __name__ == "__main__":
    # Demo usage
    from fastapi import FastAPI
    import uvicorn

    app = FastAPI(title="Notification Center Demo")
    register_notification_center(app)
    register_notification_center_page(app)

    uvicorn.run(app, host="0.0.0.0", port=8227)

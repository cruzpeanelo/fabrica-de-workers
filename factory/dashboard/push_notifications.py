# -*- coding: utf-8 -*-
"""
Push Notifications Module (Issue #261)
======================================
Sistema de notificacoes push nativas.

Funcionalidades:
- Registro de service worker
- Subscription management
- Notificacoes por evento
- Grouping de notificacoes
- Centro de notificacoes
- Preferencias granulares
"""

from fastapi import APIRouter, HTTPException, Query, BackgroundTasks
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime
import json
import uuid

router = APIRouter(prefix="/api/notifications", tags=["Push Notifications"])

# Notification storage (in production, use database + push service)
user_notifications: Dict[str, List[dict]] = {}
push_subscriptions: Dict[str, dict] = {}

# Notification types
NOTIFICATION_TYPES = {
    "story_assigned": {
        "title": "Nova Story Atribuida",
        "icon": "user-plus",
        "category": "assignment"
    },
    "story_mentioned": {
        "title": "Voce foi Mencionado",
        "icon": "at-symbol",
        "category": "mention"
    },
    "story_status_changed": {
        "title": "Status Alterado",
        "icon": "arrow-right",
        "category": "status"
    },
    "comment_added": {
        "title": "Novo Comentario",
        "icon": "chat",
        "category": "comment"
    },
    "task_completed": {
        "title": "Task Concluida",
        "icon": "check",
        "category": "task"
    },
    "sprint_started": {
        "title": "Sprint Iniciado",
        "icon": "play",
        "category": "sprint"
    },
    "sprint_ended": {
        "title": "Sprint Encerrado",
        "icon": "flag",
        "category": "sprint"
    },
    "due_date_approaching": {
        "title": "Prazo Proximo",
        "icon": "clock",
        "category": "deadline"
    },
    "due_date_passed": {
        "title": "Prazo Expirado",
        "icon": "exclamation",
        "category": "deadline"
    },
    "review_requested": {
        "title": "Revisao Solicitada",
        "icon": "eye",
        "category": "review"
    }
}


class PushSubscription(BaseModel):
    endpoint: str
    keys: Dict[str, str]  # p256dh, auth


class NotificationCreate(BaseModel):
    user_id: str
    type: str
    title: Optional[str] = None
    body: str
    data: Optional[Dict[str, Any]] = None
    url: Optional[str] = None
    priority: str = "normal"  # low, normal, high
    group_id: Optional[str] = None


class NotificationPreferences(BaseModel):
    enabled: bool = True
    categories: Optional[Dict[str, bool]] = None
    quiet_hours: Optional[Dict[str, str]] = None  # start, end
    group_notifications: bool = True


@router.get("/vapid-key")
async def get_vapid_public_key():
    """Retorna chave publica VAPID para subscription."""
    # In production, use proper VAPID keys
    return {
        "public_key": "BGxg9TZ4C9r1w_jSC5xYGD9Tq2G8K3Y5R2H1N7X0V4M6W8A9P1L3K5J7I9O2U4E6Q8R0T2Y4W6Z8X0C2V4B6N8"
    }


@router.post("/subscribe")
async def subscribe_push(subscription: PushSubscription, user_id: str = Query(...)):
    """Registra subscription para push notifications."""
    push_subscriptions[user_id] = {
        "endpoint": subscription.endpoint,
        "keys": subscription.keys,
        "subscribed_at": datetime.now().isoformat()
    }

    return {
        "success": True,
        "message": "Inscrito para notificacoes push"
    }


@router.delete("/subscribe")
async def unsubscribe_push(user_id: str = Query(...)):
    """Remove subscription de push notifications."""
    if user_id in push_subscriptions:
        del push_subscriptions[user_id]

    return {
        "success": True,
        "message": "Desinscrito de notificacoes push"
    }


@router.get("/")
async def get_notifications(
    user_id: str = Query(...),
    unread_only: bool = Query(False),
    category: Optional[str] = Query(None),
    limit: int = Query(50)
):
    """Retorna notificacoes do usuario."""
    notifications = user_notifications.get(user_id, [])

    if unread_only:
        notifications = [n for n in notifications if not n.get("read")]

    if category:
        notifications = [n for n in notifications if n.get("category") == category]

    # Sort by date descending
    notifications.sort(key=lambda n: n.get("created_at", ""), reverse=True)

    # Group notifications by group_id
    grouped = {}
    ungrouped = []

    for n in notifications[:limit]:
        group_id = n.get("group_id")
        if group_id:
            if group_id not in grouped:
                grouped[group_id] = {
                    "group_id": group_id,
                    "notifications": [],
                    "count": 0,
                    "latest": n
                }
            grouped[group_id]["notifications"].append(n)
            grouped[group_id]["count"] += 1
        else:
            ungrouped.append(n)

    return {
        "notifications": ungrouped,
        "grouped": list(grouped.values()),
        "total": len(notifications),
        "unread_count": len([n for n in notifications if not n.get("read")])
    }


@router.get("/unread-count")
async def get_unread_count(user_id: str = Query(...)):
    """Retorna contagem de notificacoes nao lidas."""
    notifications = user_notifications.get(user_id, [])
    unread = len([n for n in notifications if not n.get("read")])

    return {
        "unread_count": unread
    }


@router.post("/")
async def create_notification(
    notification: NotificationCreate,
    background_tasks: BackgroundTasks
):
    """Cria e envia uma notificacao."""
    notif_id = f"notif_{uuid.uuid4().hex[:12]}"

    notif_type = NOTIFICATION_TYPES.get(notification.type, {})

    new_notification = {
        "id": notif_id,
        "user_id": notification.user_id,
        "type": notification.type,
        "title": notification.title or notif_type.get("title", "Notificacao"),
        "body": notification.body,
        "icon": notif_type.get("icon", "bell"),
        "category": notif_type.get("category", "general"),
        "data": notification.data or {},
        "url": notification.url,
        "priority": notification.priority,
        "group_id": notification.group_id,
        "read": False,
        "created_at": datetime.now().isoformat()
    }

    # Store notification
    if notification.user_id not in user_notifications:
        user_notifications[notification.user_id] = []

    user_notifications[notification.user_id].append(new_notification)

    # Keep only last 200 notifications per user
    if len(user_notifications[notification.user_id]) > 200:
        user_notifications[notification.user_id] = user_notifications[notification.user_id][-200:]

    # Send push notification in background
    if notification.user_id in push_subscriptions:
        background_tasks.add_task(
            send_push_notification,
            notification.user_id,
            new_notification
        )

    return {
        "success": True,
        "notification": new_notification
    }


@router.post("/send-bulk")
async def send_bulk_notification(
    user_ids: List[str],
    notification: NotificationCreate,
    background_tasks: BackgroundTasks
):
    """Envia notificacao para multiplos usuarios."""
    sent = 0

    for user_id in user_ids:
        notification.user_id = user_id
        await create_notification(notification, background_tasks)
        sent += 1

    return {
        "success": True,
        "sent": sent
    }


@router.patch("/{notification_id}/read")
async def mark_as_read(notification_id: str, user_id: str = Query(...)):
    """Marca notificacao como lida."""
    notifications = user_notifications.get(user_id, [])
    notif = next((n for n in notifications if n["id"] == notification_id), None)

    if notif:
        notif["read"] = True
        notif["read_at"] = datetime.now().isoformat()

    return {"success": True}


@router.post("/mark-all-read")
async def mark_all_as_read(user_id: str = Query(...)):
    """Marca todas notificacoes como lidas."""
    notifications = user_notifications.get(user_id, [])

    for notif in notifications:
        if not notif.get("read"):
            notif["read"] = True
            notif["read_at"] = datetime.now().isoformat()

    return {
        "success": True,
        "marked": len(notifications)
    }


@router.delete("/{notification_id}")
async def delete_notification(notification_id: str, user_id: str = Query(...)):
    """Remove uma notificacao."""
    notifications = user_notifications.get(user_id, [])
    user_notifications[user_id] = [n for n in notifications if n["id"] != notification_id]

    return {"success": True}


@router.delete("/")
async def clear_notifications(user_id: str = Query(...), read_only: bool = Query(True)):
    """Limpa notificacoes."""
    if user_id not in user_notifications:
        return {"success": True, "cleared": 0}

    if read_only:
        before = len(user_notifications[user_id])
        user_notifications[user_id] = [n for n in user_notifications[user_id] if not n.get("read")]
        cleared = before - len(user_notifications[user_id])
    else:
        cleared = len(user_notifications[user_id])
        user_notifications[user_id] = []

    return {
        "success": True,
        "cleared": cleared
    }


async def send_push_notification(user_id: str, notification: dict):
    """Envia push notification via Web Push."""
    # In production, use pywebpush
    subscription = push_subscriptions.get(user_id)

    if not subscription:
        return

    try:
        # Would use pywebpush here:
        # webpush(
        #     subscription,
        #     json.dumps(notification),
        #     vapid_private_key=VAPID_PRIVATE_KEY,
        #     vapid_claims={"sub": "mailto:..."}
        # )
        print(f"[Push] Would send to {user_id}: {notification['title']}")
    except Exception as e:
        print(f"[Push] Error: {e}")


def get_notifications_html():
    """Retorna o HTML do centro de notificacoes."""
    return '''
    <!-- Notifications Center (Issue #261) -->
    <div v-if="showNotificationCenter"
         class="fixed top-16 right-4 w-96 bg-white rounded-xl shadow-2xl z-50 max-h-[80vh] overflow-hidden flex flex-col">
        <!-- Header -->
        <div class="px-4 py-3 border-b flex items-center justify-between">
            <h3 class="font-semibold">Notificacoes</h3>
            <div class="flex items-center gap-2">
                <button @click="markAllAsRead"
                        v-if="unreadCount > 0"
                        class="text-xs text-blue-600 hover:underline">
                    Marcar todas como lidas
                </button>
                <button @click="showNotificationCenter = false"
                        class="text-gray-400 hover:text-gray-600">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                    </svg>
                </button>
            </div>
        </div>

        <!-- Filters -->
        <div class="px-4 py-2 border-b flex gap-2 overflow-x-auto">
            <button @click="notificationFilter = 'all'"
                    :class="['px-3 py-1 rounded-full text-sm whitespace-nowrap',
                             notificationFilter === 'all' ? 'bg-blue-100 text-blue-700' : 'bg-gray-100']">
                Todas
            </button>
            <button @click="notificationFilter = 'unread'"
                    :class="['px-3 py-1 rounded-full text-sm whitespace-nowrap',
                             notificationFilter === 'unread' ? 'bg-blue-100 text-blue-700' : 'bg-gray-100']">
                Nao lidas ({{ unreadCount }})
            </button>
            <button @click="notificationFilter = 'mention'"
                    :class="['px-3 py-1 rounded-full text-sm whitespace-nowrap',
                             notificationFilter === 'mention' ? 'bg-blue-100 text-blue-700' : 'bg-gray-100']">
                Mencoes
            </button>
        </div>

        <!-- Notifications List -->
        <div class="flex-1 overflow-y-auto">
            <div v-if="filteredNotifications.length === 0"
                 class="text-center py-12 text-gray-400">
                <svg class="w-12 h-12 mx-auto mb-3 text-gray-300" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                          d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"/>
                </svg>
                Nenhuma notificacao
            </div>

            <div v-for="notif in filteredNotifications" :key="notif.id"
                 @click="handleNotificationClick(notif)"
                 :class="['px-4 py-3 border-b hover:bg-gray-50 cursor-pointer transition-colors',
                          !notif.read ? 'bg-blue-50' : '']">
                <div class="flex gap-3">
                    <!-- Icon -->
                    <div :class="['w-10 h-10 rounded-full flex items-center justify-center flex-shrink-0',
                                  getNotificationIconBg(notif.category)]">
                        <span class="text-lg">{{ getNotificationIcon(notif.icon) }}</span>
                    </div>

                    <!-- Content -->
                    <div class="flex-1 min-w-0">
                        <div class="flex items-center justify-between">
                            <span class="font-medium text-sm">{{ notif.title }}</span>
                            <span class="text-xs text-gray-400">{{ formatTimeAgo(notif.created_at) }}</span>
                        </div>
                        <p class="text-sm text-gray-600 truncate">{{ notif.body }}</p>
                    </div>

                    <!-- Unread indicator -->
                    <div v-if="!notif.read" class="w-2 h-2 bg-blue-500 rounded-full flex-shrink-0 mt-2"></div>
                </div>
            </div>

            <!-- Grouped Notifications -->
            <div v-for="group in notificationGroups" :key="group.group_id"
                 class="px-4 py-3 border-b">
                <div @click="toggleGroup(group.group_id)"
                     class="flex items-center justify-between cursor-pointer">
                    <div class="flex items-center gap-2">
                        <span class="font-medium text-sm">{{ group.latest.title }}</span>
                        <span class="text-xs bg-gray-200 px-2 py-0.5 rounded-full">
                            {{ group.count }}
                        </span>
                    </div>
                    <svg class="w-4 h-4 text-gray-400 transform transition-transform"
                         :class="{ 'rotate-180': expandedGroups.includes(group.group_id) }"
                         fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                    </svg>
                </div>
                <div v-if="expandedGroups.includes(group.group_id)" class="mt-2 pl-4 space-y-2">
                    <div v-for="n in group.notifications" :key="n.id"
                         class="text-sm text-gray-600">
                        {{ n.body }}
                    </div>
                </div>
            </div>
        </div>

        <!-- Footer -->
        <div class="px-4 py-2 border-t bg-gray-50 text-center">
            <button @click="clearReadNotifications"
                    class="text-sm text-gray-500 hover:text-gray-700">
                Limpar lidas
            </button>
        </div>
    </div>

    <!-- Notification Bell (Header) -->
    <div class="relative">
        <button @click="toggleNotificationCenter"
                class="relative p-2 hover:bg-gray-100 rounded-lg">
            <svg class="w-6 h-6 text-gray-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                      d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"/>
            </svg>
            <span v-if="unreadCount > 0"
                  class="absolute -top-1 -right-1 w-5 h-5 bg-red-500 text-white text-xs rounded-full flex items-center justify-center">
                {{ unreadCount > 99 ? '99+' : unreadCount }}
            </span>
        </button>
    </div>
    '''


def get_notifications_js():
    """Retorna o JavaScript para notificacoes."""
    return '''
    // Notifications State
    showNotificationCenter: false,
    notifications: [],
    notificationGroups: [],
    unreadCount: 0,
    notificationFilter: 'all',
    expandedGroups: [],
    pushEnabled: false,

    // Notification Methods
    async loadNotifications() {
        const userId = this.currentUser || 'default';
        try {
            const response = await fetch(`/api/notifications/?user_id=${userId}`);
            const data = await response.json();
            this.notifications = data.notifications;
            this.notificationGroups = data.grouped;
            this.unreadCount = data.unread_count;
        } catch (e) {
            console.error('Error loading notifications:', e);
        }
    },

    get filteredNotifications() {
        if (this.notificationFilter === 'all') return this.notifications;
        if (this.notificationFilter === 'unread') return this.notifications.filter(n => !n.read);
        return this.notifications.filter(n => n.category === this.notificationFilter);
    },

    toggleNotificationCenter() {
        this.showNotificationCenter = !this.showNotificationCenter;
        if (this.showNotificationCenter) {
            this.loadNotifications();
        }
    },

    async handleNotificationClick(notification) {
        // Mark as read
        if (!notification.read) {
            await this.markAsRead(notification.id);
        }

        // Navigate to URL if provided
        if (notification.url) {
            this.showNotificationCenter = false;
            // Handle navigation
        }
    },

    async markAsRead(notificationId) {
        const userId = this.currentUser || 'default';
        try {
            await fetch(`/api/notifications/${notificationId}/read?user_id=${userId}`, {
                method: 'PATCH'
            });
            const notif = this.notifications.find(n => n.id === notificationId);
            if (notif) {
                notif.read = true;
                this.unreadCount = Math.max(0, this.unreadCount - 1);
            }
        } catch (e) {
            console.error('Error marking as read:', e);
        }
    },

    async markAllAsRead() {
        const userId = this.currentUser || 'default';
        try {
            await fetch(`/api/notifications/mark-all-read?user_id=${userId}`, {
                method: 'POST'
            });
            this.notifications.forEach(n => n.read = true);
            this.unreadCount = 0;
        } catch (e) {
            console.error('Error marking all as read:', e);
        }
    },

    async clearReadNotifications() {
        const userId = this.currentUser || 'default';
        try {
            await fetch(`/api/notifications/?user_id=${userId}&read_only=true`, {
                method: 'DELETE'
            });
            this.notifications = this.notifications.filter(n => !n.read);
        } catch (e) {
            console.error('Error clearing notifications:', e);
        }
    },

    toggleGroup(groupId) {
        const idx = this.expandedGroups.indexOf(groupId);
        if (idx >= 0) {
            this.expandedGroups.splice(idx, 1);
        } else {
            this.expandedGroups.push(groupId);
        }
    },

    getNotificationIcon(icon) {
        const icons = {
            'user-plus': '👤',
            'at-symbol': '@',
            'arrow-right': '→',
            'chat': '💬',
            'check': '✓',
            'play': '▶',
            'flag': '🏁',
            'clock': '⏰',
            'exclamation': '⚠',
            'eye': '👁',
            'bell': '🔔'
        };
        return icons[icon] || '🔔';
    },

    getNotificationIconBg(category) {
        const colors = {
            'assignment': 'bg-blue-100',
            'mention': 'bg-purple-100',
            'status': 'bg-yellow-100',
            'comment': 'bg-green-100',
            'task': 'bg-emerald-100',
            'sprint': 'bg-indigo-100',
            'deadline': 'bg-red-100',
            'review': 'bg-orange-100'
        };
        return colors[category] || 'bg-gray-100';
    },

    formatTimeAgo(dateStr) {
        const date = new Date(dateStr);
        const now = new Date();
        const diff = Math.floor((now - date) / 1000);

        if (diff < 60) return 'agora';
        if (diff < 3600) return `${Math.floor(diff / 60)}m`;
        if (diff < 86400) return `${Math.floor(diff / 3600)}h`;
        return `${Math.floor(diff / 86400)}d`;
    },

    // Push Notification Setup
    async setupPushNotifications() {
        if (!('serviceWorker' in navigator) || !('PushManager' in window)) {
            console.log('Push notifications not supported');
            return;
        }

        try {
            // Get VAPID key
            const response = await fetch('/api/notifications/vapid-key');
            const { public_key } = await response.json();

            // Register service worker
            const registration = await navigator.serviceWorker.register('/sw.js');

            // Subscribe to push
            const subscription = await registration.pushManager.subscribe({
                userVisibleOnly: true,
                applicationServerKey: this.urlBase64ToUint8Array(public_key)
            });

            // Send subscription to server
            await fetch(`/api/notifications/subscribe?user_id=${this.currentUser}`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(subscription)
            });

            this.pushEnabled = true;
        } catch (e) {
            console.error('Error setting up push:', e);
        }
    },

    urlBase64ToUint8Array(base64String) {
        const padding = '='.repeat((4 - base64String.length % 4) % 4);
        const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
        const rawData = window.atob(base64);
        const outputArray = new Uint8Array(rawData.length);
        for (let i = 0; i < rawData.length; ++i) {
            outputArray[i] = rawData.charCodeAt(i);
        }
        return outputArray;
    }
    '''


def register_push_notifications(app):
    """Registra os endpoints de notificacoes no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Push Notifications endpoints loaded: /api/notifications/*")

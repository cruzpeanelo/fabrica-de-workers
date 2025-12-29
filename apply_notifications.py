# -*- coding: utf-8 -*-
"""Script to apply notification changes to app_v6_agile.py"""

import re

# Read the file
with open('factory/dashboard/app_v6_agile.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Define old text pattern
old_pattern = '''# =============================================================================
# WEBSOCKET CONNECTION MANAGER
# =============================================================================

class ConnectionManager:
    def __init__(self):
        self.active_connections: List[WebSocket] = []

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket):
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)

    async def broadcast(self, message: dict):
        for connection in self.active_connections:
            try:
                await connection.send_json(message)
            except:
                pass

ws_manager = ConnectionManager()

def notify(notification_type: str, data: dict):
    message = {"type": notification_type, "data": data, "timestamp": datetime.utcnow().isoformat() + "Z"}
    try:
        asyncio.create_task(ws_manager.broadcast(message))
    except:
        pass


# =============================================================================
# PYDANTIC SCHEMAS
# ============================================================================='''

new_text = '''# =============================================================================
# NOTIFICATION TYPES
# =============================================================================

class NotificationType:
    """Notification types for real-time updates"""
    STORY_CREATED = "story_created"
    STORY_MOVED = "story_moved"
    STORY_UPDATED = "story_updated"
    STORY_DELETED = "story_deleted"
    TASK_CREATED = "task_created"
    TASK_COMPLETED = "task_completed"
    TASK_UPDATED = "task_updated"
    DOC_CREATED = "doc_created"
    CHAT_MESSAGE = "chat_message"
    APP_READY = "app_ready"
    BUILD_STARTED = "build_started"
    BUILD_COMPLETED = "build_completed"
    BUILD_FAILED = "build_failed"
    CONNECTION = "connection"
    PONG = "pong"


# Slack/Teams Integration Config
SLACK_WEBHOOK_URL = os.getenv("SLACK_WEBHOOK_URL", "")
TEAMS_WEBHOOK_URL = os.getenv("TEAMS_WEBHOOK_URL", "")
NOTIFICATIONS_ENABLED = os.getenv("NOTIFICATIONS_ENABLED", "true").lower() == "true"


# =============================================================================
# WEBSOCKET CONNECTION MANAGER
# =============================================================================

class ConnectionManager:
    def __init__(self):
        self.active_connections: List[WebSocket] = []
        self.notification_history: List[dict] = []
        self.max_history = 100  # Keep last 100 notifications
        self.unread_count = 0

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket):
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)

    def add_notification(self, notification: dict):
        """Add notification to history"""
        notification["id"] = str(uuid.uuid4())[:8]
        notification["read"] = False
        self.notification_history.insert(0, notification)
        self.unread_count += 1
        if len(self.notification_history) > self.max_history:
            self.notification_history = self.notification_history[:self.max_history]

    def mark_as_read(self, notification_id: str = None):
        """Mark notification(s) as read"""
        if notification_id:
            for n in self.notification_history:
                if n.get("id") == notification_id and not n.get("read"):
                    n["read"] = True
                    self.unread_count = max(0, self.unread_count - 1)
                    break
        else:
            for n in self.notification_history:
                n["read"] = True
            self.unread_count = 0

    def get_history(self, limit: int = 50) -> List[dict]:
        """Get notification history"""
        return self.notification_history[:limit]

    def get_unread_count(self) -> int:
        """Get unread notification count"""
        return self.unread_count

    async def broadcast(self, message: dict):
        for connection in self.active_connections:
            try:
                await connection.send_json(message)
            except:
                pass

ws_manager = ConnectionManager()


async def send_slack_notification(notification_type: str, data: dict):
    """Send notification to Slack webhook"""
    if not SLACK_WEBHOOK_URL:
        return
    try:
        import httpx
        title_map = {
            NotificationType.STORY_CREATED: ":sparkles: Nova Story Criada",
            NotificationType.STORY_MOVED: ":arrow_right: Story Movida",
            NotificationType.TASK_COMPLETED: ":white_check_mark: Task Completada",
            NotificationType.APP_READY: ":rocket: App Pronto",
            NotificationType.BUILD_COMPLETED: ":package: Build Completado",
            NotificationType.BUILD_FAILED: ":x: Build Falhou",
        }
        title = title_map.get(notification_type, f":bell: {notification_type}")
        text = ""
        if "title" in data:
            text = f"*{data.get('story_id', data.get('task_id', ''))}*: {data['title']}"
        elif "message" in data:
            text = data["message"]
        else:
            text = str(data)
        payload = {"blocks": [{"type": "section", "text": {"type": "mrkdwn", "text": f"{title}\\n{text}"}}]}
        async with httpx.AsyncClient() as client:
            await client.post(SLACK_WEBHOOK_URL, json=payload, timeout=5.0)
    except Exception as e:
        print(f"[Slack] Error sending notification: {e}")


async def send_teams_notification(notification_type: str, data: dict):
    """Send notification to Microsoft Teams webhook"""
    if not TEAMS_WEBHOOK_URL:
        return
    try:
        import httpx
        title_map = {
            NotificationType.STORY_CREATED: "Nova Story Criada",
            NotificationType.STORY_MOVED: "Story Movida",
            NotificationType.TASK_COMPLETED: "Task Completada",
            NotificationType.APP_READY: "App Pronto",
            NotificationType.BUILD_COMPLETED: "Build Completado",
            NotificationType.BUILD_FAILED: "Build Falhou",
        }
        title = title_map.get(notification_type, notification_type)
        text = ""
        if "title" in data:
            text = f"**{data.get('story_id', data.get('task_id', ''))}**: {data['title']}"
        elif "message" in data:
            text = data["message"]
        else:
            text = str(data)
        payload = {
            "@type": "MessageCard",
            "@context": "http://schema.org/extensions",
            "themeColor": "003B4A",
            "summary": title,
            "sections": [{"activityTitle": title, "activitySubtitle": "Fabrica de Agentes", "text": text, "markdown": True}]
        }
        async with httpx.AsyncClient() as client:
            await client.post(TEAMS_WEBHOOK_URL, json=payload, timeout=5.0)
    except Exception as e:
        print(f"[Teams] Error sending notification: {e}")


def notify(notification_type: str, data: dict):
    """Send notification via WebSocket and optionally to Slack/Teams"""
    message = {"type": notification_type, "data": data, "timestamp": datetime.utcnow().isoformat() + "Z"}
    if notification_type not in [NotificationType.PONG, NotificationType.CONNECTION]:
        ws_manager.add_notification(message.copy())
    try:
        asyncio.create_task(ws_manager.broadcast(message))
        if NOTIFICATIONS_ENABLED and notification_type in [
            NotificationType.STORY_CREATED, NotificationType.STORY_MOVED,
            NotificationType.TASK_COMPLETED, NotificationType.APP_READY,
            NotificationType.BUILD_COMPLETED, NotificationType.BUILD_FAILED
        ]:
            if SLACK_WEBHOOK_URL:
                asyncio.create_task(send_slack_notification(notification_type, data))
            if TEAMS_WEBHOOK_URL:
                asyncio.create_task(send_teams_notification(notification_type, data))
    except:
        pass


# =============================================================================
# PYDANTIC SCHEMAS
# ============================================================================='''

if old_pattern in content:
    content = content.replace(old_pattern, new_text)
    with open('factory/dashboard/app_v6_agile.py', 'w', encoding='utf-8') as f:
        f.write(content)
    print('SUCCESS: ConnectionManager section updated')
else:
    print('WARNING: Pattern not found exactly')
    # Debug: show where it might differ
    import difflib
    # Find the approximate location
    idx = content.find('class ConnectionManager:')
    if idx > 0:
        print(f'Found ConnectionManager at index {idx}')
        snippet = content[idx-200:idx+500]
        print('Snippet around ConnectionManager:')
        print(snippet[:300])

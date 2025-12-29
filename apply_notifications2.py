# -*- coding: utf-8 -*-
"""Script to add notification API endpoints to app_v6_agile.py"""

# Read the file
with open('factory/dashboard/app_v6_agile.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Pattern to find and replace
old_pattern = '''# =============================================================================
# WEBSOCKET ENDPOINT
# =============================================================================

@app.websocket("/ws/notifications")
async def websocket_notifications(websocket: WebSocket):
    """WebSocket endpoint para notificacoes em tempo real"""
    await ws_manager.connect(websocket)
    try:
        await websocket.send_json({
            "type": "connection",
            "data": {"status": "connected", "message": "Conectado ao servidor de notificacoes"},
            "timestamp": datetime.utcnow().isoformat() + "Z"
        })
        while True:
            try:
                data = await websocket.receive_text()
                if data == "ping":
                    await websocket.send_json({"type": "pong", "timestamp": datetime.utcnow().isoformat() + "Z"})
            except WebSocketDisconnect:
                break
            except:
                break
    except WebSocketDisconnect:
        pass
    finally:
        ws_manager.disconnect(websocket)'''

new_text = '''# =============================================================================
# API ENDPOINTS - NOTIFICATIONS
# =============================================================================

@app.get("/api/notifications")
def get_notifications(limit: int = 50):
    """Get notification history"""
    return {
        "notifications": ws_manager.get_history(limit),
        "unread_count": ws_manager.get_unread_count()
    }


@app.post("/api/notifications/mark-read")
def mark_notifications_read(notification_id: Optional[str] = None):
    """Mark notification(s) as read"""
    ws_manager.mark_as_read(notification_id)
    return {"success": True, "unread_count": ws_manager.get_unread_count()}


@app.post("/api/notifications/mark-all-read")
def mark_all_notifications_read():
    """Mark all notifications as read"""
    ws_manager.mark_as_read()
    return {"success": True, "unread_count": 0}


# =============================================================================
# API ENDPOINTS - INTEGRATIONS (SLACK/TEAMS)
# =============================================================================

class WebhookConfig(BaseModel):
    webhook_url: str
    enabled: bool = True


class TestNotification(BaseModel):
    message: str = "Test notification from Fabrica de Agentes"


@app.get("/api/integrations/config")
def get_integrations_config():
    """Get current integration configuration status"""
    return {
        "slack": {
            "configured": bool(SLACK_WEBHOOK_URL),
            "enabled": NOTIFICATIONS_ENABLED
        },
        "teams": {
            "configured": bool(TEAMS_WEBHOOK_URL),
            "enabled": NOTIFICATIONS_ENABLED
        },
        "notifications_enabled": NOTIFICATIONS_ENABLED
    }


@app.post("/api/integrations/slack/webhook")
async def configure_slack_webhook(config: WebhookConfig):
    """Configure Slack webhook URL (runtime only, use env var for persistence)"""
    global SLACK_WEBHOOK_URL
    SLACK_WEBHOOK_URL = config.webhook_url if config.enabled else ""
    return {"success": True, "message": "Slack webhook configured", "configured": bool(SLACK_WEBHOOK_URL)}


@app.post("/api/integrations/slack/test")
async def test_slack_notification(test: TestNotification = TestNotification()):
    """Send a test notification to Slack"""
    if not SLACK_WEBHOOK_URL:
        raise HTTPException(400, "Slack webhook URL not configured. Set SLACK_WEBHOOK_URL environment variable.")
    await send_slack_notification("test", {"message": test.message})
    return {"success": True, "message": "Test notification sent to Slack"}


@app.post("/api/integrations/teams/webhook")
async def configure_teams_webhook(config: WebhookConfig):
    """Configure Teams webhook URL (runtime only, use env var for persistence)"""
    global TEAMS_WEBHOOK_URL
    TEAMS_WEBHOOK_URL = config.webhook_url if config.enabled else ""
    return {"success": True, "message": "Teams webhook configured", "configured": bool(TEAMS_WEBHOOK_URL)}


@app.post("/api/integrations/teams/test")
async def test_teams_notification(test: TestNotification = TestNotification()):
    """Send a test notification to Microsoft Teams"""
    if not TEAMS_WEBHOOK_URL:
        raise HTTPException(400, "Teams webhook URL not configured. Set TEAMS_WEBHOOK_URL environment variable.")
    await send_teams_notification("test", {"message": test.message})
    return {"success": True, "message": "Test notification sent to Teams"}


# =============================================================================
# WEBSOCKET ENDPOINT
# =============================================================================

@app.websocket("/ws/notifications")
async def websocket_notifications(websocket: WebSocket):
    """WebSocket endpoint para notificacoes em tempo real"""
    await ws_manager.connect(websocket)
    try:
        # Send connection message with current unread count
        await websocket.send_json({
            "type": "connection",
            "data": {
                "status": "connected",
                "message": "Conectado ao servidor de notificacoes",
                "unread_count": ws_manager.get_unread_count()
            },
            "timestamp": datetime.utcnow().isoformat() + "Z"
        })
        while True:
            try:
                data = await websocket.receive_text()
                if data == "ping":
                    await websocket.send_json({"type": "pong", "timestamp": datetime.utcnow().isoformat() + "Z"})
                elif data == "get_unread_count":
                    await websocket.send_json({
                        "type": "unread_count",
                        "data": {"count": ws_manager.get_unread_count()},
                        "timestamp": datetime.utcnow().isoformat() + "Z"
                    })
            except WebSocketDisconnect:
                break
            except:
                break
    except WebSocketDisconnect:
        pass
    finally:
        ws_manager.disconnect(websocket)'''

if old_pattern in content:
    content = content.replace(old_pattern, new_text)
    with open('factory/dashboard/app_v6_agile.py', 'w', encoding='utf-8') as f:
        f.write(content)
    print('SUCCESS: API endpoints added')
else:
    print('WARNING: Pattern not found exactly')

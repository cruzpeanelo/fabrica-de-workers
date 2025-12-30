# -*- coding: utf-8 -*-
"""
Audit Dashboard Module (Issue #274)
===================================
Dashboard administrativo para visualizar logs de auditoria.

Funcionalidades:
- Timeline de atividades do sistema
- Filtros por usuario, acao, data
- Export de logs
- Alertas de atividades suspeitas
- Drill-down em eventos
"""

from fastapi import APIRouter, Query
from fastapi.responses import Response
from typing import Optional, List
from datetime import datetime, timedelta
from io import StringIO
import csv

from factory.database.connection import SessionLocal

router = APIRouter(prefix="/api/audit", tags=["Audit Dashboard"])

# In-memory audit log storage (in production, use factory.core.audit_logger)
audit_logs: List[dict] = []

# Action categories
ACTION_CATEGORIES = {
    "auth": ["login", "logout", "password_change", "mfa_enabled", "api_key_created"],
    "data": ["create", "update", "delete", "export", "import"],
    "admin": ["user_created", "user_deleted", "role_changed", "settings_changed"],
    "security": ["failed_login", "suspicious_activity", "permission_denied"]
}


def log_audit_event(
    action: str,
    user: str,
    entity_type: str = None,
    entity_id: str = None,
    details: dict = None,
    ip_address: str = None,
    severity: str = "info"
):
    """Registra um evento de auditoria."""
    event = {
        "id": f"aud_{len(audit_logs) + 1:06d}",
        "timestamp": datetime.now().isoformat(),
        "action": action,
        "user": user,
        "entity_type": entity_type,
        "entity_id": entity_id,
        "details": details or {},
        "ip_address": ip_address,
        "severity": severity,
        "category": get_action_category(action)
    }
    audit_logs.append(event)

    # Keep only last 10000 logs
    while len(audit_logs) > 10000:
        audit_logs.pop(0)

    return event


def get_action_category(action: str) -> str:
    """Determina a categoria de uma acao."""
    for category, actions in ACTION_CATEGORIES.items():
        if action in actions:
            return category
    return "other"


@router.get("/logs")
async def get_audit_logs(
    user: Optional[str] = Query(None),
    action: Optional[str] = Query(None),
    category: Optional[str] = Query(None),
    severity: Optional[str] = Query(None),
    entity_type: Optional[str] = Query(None),
    date_from: Optional[str] = Query(None),
    date_to: Optional[str] = Query(None),
    limit: int = Query(100, ge=1, le=1000),
    offset: int = Query(0, ge=0)
):
    """Retorna logs de auditoria com filtros."""
    filtered_logs = audit_logs.copy()

    if user:
        filtered_logs = [log for log in filtered_logs if user.lower() in log["user"].lower()]

    if action:
        filtered_logs = [log for log in filtered_logs if action.lower() in log["action"].lower()]

    if category:
        filtered_logs = [log for log in filtered_logs if log.get("category") == category]

    if severity:
        filtered_logs = [log for log in filtered_logs if log.get("severity") == severity]

    if entity_type:
        filtered_logs = [log for log in filtered_logs if log.get("entity_type") == entity_type]

    if date_from:
        try:
            from_date = datetime.fromisoformat(date_from)
            filtered_logs = [log for log in filtered_logs
                           if datetime.fromisoformat(log["timestamp"]) >= from_date]
        except:
            pass

    if date_to:
        try:
            to_date = datetime.fromisoformat(date_to)
            filtered_logs = [log for log in filtered_logs
                           if datetime.fromisoformat(log["timestamp"]) <= to_date]
        except:
            pass

    # Sort by timestamp descending
    filtered_logs.sort(key=lambda x: x["timestamp"], reverse=True)

    total = len(filtered_logs)
    paginated = filtered_logs[offset:offset + limit]

    return {
        "logs": paginated,
        "total": total,
        "offset": offset,
        "limit": limit
    }


@router.get("/summary")
async def get_audit_summary(days: int = Query(7, ge=1, le=90)):
    """Retorna resumo de auditoria dos ultimos dias."""
    cutoff = datetime.now() - timedelta(days=days)

    recent_logs = [log for log in audit_logs
                   if datetime.fromisoformat(log["timestamp"]) >= cutoff]

    # Count by category
    by_category = {}
    for log in recent_logs:
        cat = log.get("category", "other")
        by_category[cat] = by_category.get(cat, 0) + 1

    # Count by severity
    by_severity = {}
    for log in recent_logs:
        sev = log.get("severity", "info")
        by_severity[sev] = by_severity.get(sev, 0) + 1

    # Count by user (top 10)
    by_user = {}
    for log in recent_logs:
        user = log.get("user", "unknown")
        by_user[user] = by_user.get(user, 0) + 1
    top_users = sorted(by_user.items(), key=lambda x: x[1], reverse=True)[:10]

    # Count by day
    by_day = {}
    for log in recent_logs:
        day = log["timestamp"][:10]
        by_day[day] = by_day.get(day, 0) + 1

    # Security alerts (high severity)
    alerts = [log for log in recent_logs if log.get("severity") in ["warning", "critical"]]

    return {
        "period_days": days,
        "total_events": len(recent_logs),
        "by_category": by_category,
        "by_severity": by_severity,
        "top_users": top_users,
        "by_day": dict(sorted(by_day.items())),
        "recent_alerts": alerts[-10:][::-1]
    }


@router.get("/users/{user}")
async def get_user_activity(user: str, limit: int = Query(50)):
    """Retorna atividade de um usuario especifico."""
    user_logs = [log for log in audit_logs if log["user"] == user]
    user_logs.sort(key=lambda x: x["timestamp"], reverse=True)

    return {
        "user": user,
        "total_actions": len(user_logs),
        "recent_actions": user_logs[:limit]
    }


@router.get("/entity/{entity_type}/{entity_id}")
async def get_entity_history(entity_type: str, entity_id: str):
    """Retorna historico de alteracoes de uma entidade."""
    entity_logs = [log for log in audit_logs
                   if log.get("entity_type") == entity_type
                   and log.get("entity_id") == entity_id]
    entity_logs.sort(key=lambda x: x["timestamp"], reverse=True)

    return {
        "entity_type": entity_type,
        "entity_id": entity_id,
        "history": entity_logs
    }


@router.get("/export")
async def export_audit_logs(
    format: str = Query("csv", enum=["csv", "json"]),
    days: int = Query(30, ge=1, le=365)
):
    """Exporta logs de auditoria."""
    cutoff = datetime.now() - timedelta(days=days)
    logs_to_export = [log for log in audit_logs
                      if datetime.fromisoformat(log["timestamp"]) >= cutoff]

    if format == "json":
        import json
        content = json.dumps(logs_to_export, indent=2)
        media_type = "application/json"
        filename = f"audit_logs_{datetime.now().strftime('%Y%m%d')}.json"
    else:
        output = StringIO()
        writer = csv.writer(output)
        writer.writerow(["ID", "Timestamp", "User", "Action", "Category",
                        "Severity", "Entity Type", "Entity ID", "IP Address"])

        for log in logs_to_export:
            writer.writerow([
                log.get("id", ""),
                log.get("timestamp", ""),
                log.get("user", ""),
                log.get("action", ""),
                log.get("category", ""),
                log.get("severity", ""),
                log.get("entity_type", ""),
                log.get("entity_id", ""),
                log.get("ip_address", "")
            ])

        content = output.getvalue()
        media_type = "text/csv"
        filename = f"audit_logs_{datetime.now().strftime('%Y%m%d')}.csv"

    return Response(
        content=content,
        media_type=media_type,
        headers={"Content-Disposition": f"attachment; filename={filename}"}
    )


@router.get("/alerts")
async def get_security_alerts(limit: int = Query(20)):
    """Retorna alertas de seguranca recentes."""
    alerts = [log for log in audit_logs if log.get("severity") in ["warning", "critical"]]
    alerts.sort(key=lambda x: x["timestamp"], reverse=True)

    return {
        "alerts": alerts[:limit],
        "total": len(alerts)
    }


def get_audit_dashboard_html():
    """Retorna o HTML do dashboard de auditoria."""
    return '''
    <!-- Audit Dashboard (Issue #274) -->
    <div v-if="currentTab === 'audit'" class="audit-dashboard p-6">
        <!-- Header -->
        <div class="flex items-center justify-between mb-6">
            <div>
                <h2 class="text-2xl font-bold">Auditoria</h2>
                <p class="text-gray-500">Logs e atividades do sistema</p>
            </div>
            <button @click="exportAuditLogs"
                    class="px-4 py-2 bg-gray-100 hover:bg-gray-200 rounded-lg flex items-center gap-2">
                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                          d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"/>
                </svg>
                Exportar
            </button>
        </div>

        <!-- Summary Cards -->
        <div class="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-blue-600">{{ auditSummary.total_events }}</div>
                <div class="text-sm text-gray-500">Eventos (7 dias)</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-green-600">{{ auditSummary.by_severity?.info || 0 }}</div>
                <div class="text-sm text-gray-500">Info</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-orange-600">{{ auditSummary.by_severity?.warning || 0 }}</div>
                <div class="text-sm text-gray-500">Avisos</div>
            </div>
            <div class="bg-white rounded-xl shadow p-4">
                <div class="text-3xl font-bold text-red-600">{{ auditSummary.by_severity?.critical || 0 }}</div>
                <div class="text-sm text-gray-500">Criticos</div>
            </div>
        </div>

        <!-- Filters -->
        <div class="bg-white rounded-xl shadow p-4 mb-6">
            <div class="flex flex-wrap gap-4">
                <input type="text" v-model="auditFilters.user" placeholder="Usuario"
                       class="px-3 py-2 border border-gray-300 rounded-lg text-sm">
                <select v-model="auditFilters.category" class="px-3 py-2 border border-gray-300 rounded-lg text-sm">
                    <option value="">Todas categorias</option>
                    <option value="auth">Autenticacao</option>
                    <option value="data">Dados</option>
                    <option value="admin">Admin</option>
                    <option value="security">Seguranca</option>
                </select>
                <select v-model="auditFilters.severity" class="px-3 py-2 border border-gray-300 rounded-lg text-sm">
                    <option value="">Todas severidades</option>
                    <option value="info">Info</option>
                    <option value="warning">Warning</option>
                    <option value="critical">Critical</option>
                </select>
                <input type="date" v-model="auditFilters.dateFrom"
                       class="px-3 py-2 border border-gray-300 rounded-lg text-sm">
                <input type="date" v-model="auditFilters.dateTo"
                       class="px-3 py-2 border border-gray-300 rounded-lg text-sm">
                <button @click="loadAuditLogs"
                        class="px-4 py-2 bg-blue-600 text-white rounded-lg text-sm hover:bg-blue-700">
                    Filtrar
                </button>
            </div>
        </div>

        <!-- Logs Table -->
        <div class="bg-white rounded-xl shadow overflow-hidden">
            <table class="w-full text-sm">
                <thead class="bg-gray-50">
                    <tr>
                        <th class="px-4 py-3 text-left">Data/Hora</th>
                        <th class="px-4 py-3 text-left">Usuario</th>
                        <th class="px-4 py-3 text-left">Acao</th>
                        <th class="px-4 py-3 text-left">Entidade</th>
                        <th class="px-4 py-3 text-left">Severidade</th>
                    </tr>
                </thead>
                <tbody class="divide-y divide-gray-100">
                    <tr v-for="log in auditLogs" :key="log.id"
                        class="hover:bg-gray-50 cursor-pointer"
                        @click="showAuditDetail(log)">
                        <td class="px-4 py-3">{{ formatDateTime(log.timestamp) }}</td>
                        <td class="px-4 py-3">{{ log.user }}</td>
                        <td class="px-4 py-3">{{ log.action }}</td>
                        <td class="px-4 py-3">
                            <span v-if="log.entity_type">{{ log.entity_type }}: {{ log.entity_id }}</span>
                            <span v-else class="text-gray-400">-</span>
                        </td>
                        <td class="px-4 py-3">
                            <span :class="getSeverityClass(log.severity)">{{ log.severity }}</span>
                        </td>
                    </tr>
                </tbody>
            </table>

            <!-- Pagination -->
            <div class="px-4 py-3 bg-gray-50 flex items-center justify-between">
                <span class="text-sm text-gray-500">{{ auditTotal }} registros</span>
                <div class="flex gap-2">
                    <button @click="auditPage--" :disabled="auditPage <= 0"
                            class="px-3 py-1 text-sm bg-white border rounded hover:bg-gray-50 disabled:opacity-50">
                        Anterior
                    </button>
                    <button @click="auditPage++"
                            class="px-3 py-1 text-sm bg-white border rounded hover:bg-gray-50">
                        Proximo
                    </button>
                </div>
            </div>
        </div>
    </div>
    '''


def register_audit_dashboard(app):
    """Registra os endpoints de auditoria no app FastAPI."""
    app.include_router(router)
    print("[Dashboard] Audit Dashboard endpoints loaded: /api/audit/*")

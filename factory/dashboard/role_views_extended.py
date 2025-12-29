# Role Views Extended - Issue #111
from typing import Dict, List, Optional, Any

EXECUTIVE_WIDGETS = [
    {"id": "portfolio_overview", "title": "Portfolio Overview", "type": "summary", "size": "full"},
    {"id": "strategic_kpis", "title": "Strategic KPIs", "type": "metrics", "size": "full"},
    {"id": "resource_allocation", "title": "Resource Allocation", "type": "chart", "size": "half"},
    {"id": "budget_tracking", "title": "Budget Tracking", "type": "chart", "size": "half"},
    {"id": "risk_dashboard", "title": "Risk Dashboard", "type": "matrix", "size": "half"},
    {"id": "milestone_timeline", "title": "Milestone Timeline", "type": "timeline", "size": "half"},
]

def get_executive_dashboard_data(project_id=None):
    return {
        "portfolio_overview": {"total_projects": 12, "on_track": 9, "at_risk": 2, "delayed": 1},
        "strategic_kpis": [
            {"name": "Time to Market", "value": 45, "target": 60, "unit": "days"},
            {"name": "Quality Score", "value": 98, "target": 95, "unit": "%"},
        ]
    }


class TenantCustomView:
    def __init__(self, tenant_id):
        self.tenant_id = tenant_id
        self.custom_widgets = []
        self.theme_overrides = {}
        self.layout_config = {}
        self.feature_flags = {}

    def add_custom_widget(self, widget):
        self.custom_widgets.append(widget)

    def set_theme(self, theme):
        self.theme_overrides = theme

    def to_dict(self):
        return {
            "tenant_id": self.tenant_id,
            "custom_widgets": self.custom_widgets,
            "theme_overrides": self.theme_overrides,
            "feature_flags": self.feature_flags
        }


class TenantViewRegistry:
    def __init__(self):
        self._views = {}

    def get_or_create(self, tenant_id):
        if tenant_id not in self._views:
            self._views[tenant_id] = TenantCustomView(tenant_id)
        return self._views[tenant_id]

    def get_view(self, tenant_id):
        return self._views.get(tenant_id)

    def list_tenants(self):
        return list(self._views.keys())


tenant_view_registry = TenantViewRegistry()


def get_dashboard_for_role_and_tenant(role, tenant_id=None, user_id=None):
    from factory.dashboard.role_views import get_widgets_for_role, get_dashboard_data, get_user_preferences

    role_upper = role.upper()
    if role_upper == "EXECUTIVE":
        widgets = EXECUTIVE_WIDGETS
        data = get_executive_dashboard_data()
    else:
        widgets = get_widgets_for_role(role)
        data = get_dashboard_data(role)

    if tenant_id:
        tenant_view = tenant_view_registry.get_view(tenant_id)
        if tenant_view:
            widgets = widgets + tenant_view.custom_widgets
            data["theme_overrides"] = tenant_view.theme_overrides
            data["feature_flags"] = tenant_view.feature_flags

    if user_id:
        prefs = get_user_preferences(user_id)
        data["user_preferences"] = prefs

    return {"role": role_upper, "widgets": widgets, "data": data}


def register_enhanced_role_views_endpoints(app):
    from fastapi import APIRouter

    router = APIRouter(prefix="/api/dashboard/v2", tags=["role-views-v2"])

    @router.get("/role/{role}")
    async def get_role_dashboard_v2(role: str, tenant_id: str = None, user_id: int = None):
        return get_dashboard_for_role_and_tenant(role, tenant_id, user_id)

    @router.get("/tenant/{tenant_id}/config")
    async def get_tenant_config(tenant_id: str):
        view = tenant_view_registry.get_view(tenant_id)
        if not view:
            return {"tenant_id": tenant_id, "config": None}
        return {"tenant_id": tenant_id, "config": view.to_dict()}

    @router.post("/tenant/{tenant_id}/config")
    async def set_tenant_config(tenant_id: str, config: dict):
        view = tenant_view_registry.get_or_create(tenant_id)
        if "custom_widgets" in config:
            for widget in config["custom_widgets"]:
                view.add_custom_widget(widget)
        if "theme" in config:
            view.set_theme(config["theme"])
        return {"success": True, "config": view.to_dict()}

    @router.get("/roles")
    async def list_available_roles():
        return {
            "roles": [
                {"id": "admin", "name": "Admin"},
                {"id": "manager", "name": "Manager"},
                {"id": "developer", "name": "Developer"},
                {"id": "executive", "name": "Executive"},
                {"id": "viewer", "name": "Viewer"},
            ]
        }

    app.include_router(router)
    print("[Dashboard] Enhanced Role Views v2 endpoints registered")

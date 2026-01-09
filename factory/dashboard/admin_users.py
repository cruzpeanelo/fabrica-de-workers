# -*- coding: utf-8 -*-
"""
Admin Users Panel - Painel de Administracao de Usuarios
=========================================================

Issue #87 - Painel de Administracao de Usuarios

Este modulo fornece componentes UI para gerenciamento de usuarios:
- Listagem com filtros e paginacao
- Criacao de usuarios
- Edicao de usuarios
- Atribuicao de roles
- Convites por email
- Ativacao/Desativacao

Componentes:
- admin_users_panel(): Painel principal
- users_table(): Tabela de usuarios
- user_form(): Formulario de usuario
- invite_form(): Formulario de convite
"""

from datetime import datetime
from typing import Dict, Any, List, Optional

# Database
from factory.database.connection import SessionLocal
from factory.database.models import User
from factory.database.tenant_models import TenantMember, TenantInvite, MemberRole, InviteStatus


# =============================================================================
# DATA FUNCTIONS
# =============================================================================

def get_users_for_tenant(
    tenant_id: str,
    page: int = 1,
    limit: int = 20,
    status: Optional[str] = None,
    role: Optional[str] = None,
    search: Optional[str] = None
) -> Dict[str, Any]:
    """
    Retorna usuarios do tenant com filtros

    Args:
        tenant_id: ID do tenant
        page: Pagina atual
        limit: Itens por pagina
        status: Filtro de status (active, inactive, pending)
        role: Filtro de role
        search: Termo de busca

    Returns:
        Dict com users, total, pages
    """
    db = SessionLocal()
    try:
        # Buscar membros do tenant
        members_query = db.query(TenantMember).filter(
            TenantMember.tenant_id == tenant_id
        )

        if role:
            members_query = members_query.filter(TenantMember.tenant_role == role)

        members = members_query.all()
        user_ids = [m.user_id for m in members]

        # Buscar usuarios
        users_query = db.query(User).filter(User.id.in_(user_ids))

        if status == "active":
            users_query = users_query.filter(User.active == True)
        elif status == "inactive":
            users_query = users_query.filter(User.active == False)
        elif status == "pending":
            users_query = users_query.filter(User.last_login == None)

        if search:
            search_term = f"%{search}%"
            users_query = users_query.filter(
                (User.username.ilike(search_term)) |
                (User.email.ilike(search_term))
            )

        total = users_query.count()
        offset = (page - 1) * limit
        users = users_query.order_by(User.created_at.desc()).offset(offset).limit(limit).all()

        # Enriquecer com dados de membership
        result = []
        members_map = {m.user_id: m for m in members}

        for user in users:
            user_dict = user.to_dict()
            member = members_map.get(user.id)

            if member:
                user_dict["tenant_role"] = member.tenant_role
                user_dict["member_status"] = member.status
                user_dict["joined_at"] = member.joined_at.isoformat() if member.joined_at else None
                user_dict["last_active_at"] = member.last_active_at.isoformat() if member.last_active_at else None

            result.append(user_dict)

        return {
            "users": result,
            "total": total,
            "page": page,
            "limit": limit,
            "pages": (total + limit - 1) // limit
        }
    finally:
        db.close()


def get_pending_invites(tenant_id: str) -> List[Dict[str, Any]]:
    """Retorna convites pendentes do tenant"""
    db = SessionLocal()
    try:
        invites = db.query(TenantInvite).filter(
            TenantInvite.tenant_id == tenant_id,
            TenantInvite.status == InviteStatus.PENDING.value
        ).order_by(TenantInvite.created_at.desc()).all()

        return [i.to_dict() for i in invites]
    finally:
        db.close()


def get_user_stats_for_tenant(tenant_id: str) -> Dict[str, Any]:
    """Retorna estatisticas de usuarios do tenant"""
    db = SessionLocal()
    try:
        members = db.query(TenantMember).filter(
            TenantMember.tenant_id == tenant_id
        ).all()

        total = len(members)
        active = sum(1 for m in members if m.active)

        # Por role
        by_role = {}
        for m in members:
            role = m.tenant_role
            by_role[role] = by_role.get(role, 0) + 1

        # Convites pendentes
        pending_invites = db.query(TenantInvite).filter(
            TenantInvite.tenant_id == tenant_id,
            TenantInvite.status == InviteStatus.PENDING.value
        ).count()

        return {
            "total": total,
            "active": active,
            "inactive": total - active,
            "by_role": by_role,
            "pending_invites": pending_invites
        }
    finally:
        db.close()


# =============================================================================
# UI COMPONENTS (HTML/JavaScript)
# =============================================================================

def generate_admin_users_html(tenant_id: str) -> str:
    """
    Gera HTML do painel de administracao de usuarios

    Args:
        tenant_id: ID do tenant

    Returns:
        HTML completo
    """
    return f'''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Administracao de Usuarios - Plataforma E</title>
    <style>
        :root {{
            --primary: #003B4A;
            --secondary: #FF6C00;
            --success: #10B981;
            --warning: #F59E0B;
            --danger: #EF4444;
            --info: #3B82F6;
            --bg: #F3F4F6;
            --surface: #FFFFFF;
            --text: #1F2937;
            --text-secondary: #6B7280;
            --border: #E5E7EB;
        }}

        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}

        body {{
            font-family: 'Inter', 'Segoe UI', sans-serif;
            background: var(--bg);
            color: var(--text);
            line-height: 1.5;
        }}

        .dashboard {{
            max-width: 1200px;
            margin: 0 auto;
            padding: 24px;
        }}

        .header {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 24px;
        }}

        .header h1 {{
            font-size: 24px;
            color: var(--primary);
        }}

        .header-actions {{
            display: flex;
            gap: 12px;
        }}

        .btn {{
            display: inline-flex;
            align-items: center;
            gap: 8px;
            padding: 10px 20px;
            border: none;
            border-radius: 8px;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s;
        }}

        .btn-primary {{
            background: var(--primary);
            color: white;
        }}

        .btn-primary:hover {{
            background: #00526A;
        }}

        .btn-secondary {{
            background: var(--secondary);
            color: white;
        }}

        .btn-secondary:hover {{
            background: #E65C00;
        }}

        .btn-outline {{
            background: transparent;
            border: 1px solid var(--border);
            color: var(--text);
        }}

        .btn-outline:hover {{
            background: var(--bg);
        }}

        .btn-danger {{
            background: var(--danger);
            color: white;
        }}

        .btn-danger:hover {{
            background: #DC2626;
        }}

        .btn-sm {{
            padding: 6px 12px;
            font-size: 12px;
        }}

        /* Stats Cards */
        .stats-row {{
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 16px;
            margin-bottom: 24px;
        }}

        .stat-card {{
            background: var(--surface);
            border-radius: 12px;
            padding: 20px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }}

        .stat-value {{
            font-size: 28px;
            font-weight: 700;
            color: var(--primary);
        }}

        .stat-label {{
            font-size: 14px;
            color: var(--text-secondary);
        }}

        /* Filters */
        .filters {{
            display: flex;
            gap: 12px;
            margin-bottom: 16px;
            flex-wrap: wrap;
        }}

        .filter-group {{
            display: flex;
            flex-direction: column;
            gap: 4px;
        }}

        .filter-label {{
            font-size: 12px;
            color: var(--text-secondary);
        }}

        .filter-select,
        .filter-input {{
            padding: 8px 12px;
            border: 1px solid var(--border);
            border-radius: 6px;
            font-size: 14px;
            min-width: 160px;
        }}

        .filter-select:focus,
        .filter-input:focus {{
            outline: none;
            border-color: var(--primary);
        }}

        /* Panel */
        .panel {{
            background: var(--surface);
            border-radius: 12px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            overflow: hidden;
        }}

        .panel-header {{
            padding: 16px 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}

        .panel-title {{
            font-size: 16px;
            font-weight: 600;
        }}

        /* Table */
        .users-table {{
            width: 100%;
            border-collapse: collapse;
        }}

        .users-table th,
        .users-table td {{
            padding: 12px 16px;
            text-align: left;
            border-bottom: 1px solid var(--border);
        }}

        .users-table th {{
            background: var(--bg);
            font-weight: 600;
            font-size: 13px;
            color: var(--text-secondary);
            text-transform: uppercase;
        }}

        .users-table tr:hover {{
            background: var(--bg);
        }}

        .users-table tr.selected {{
            background: #E0F2FE;
        }}

        .user-avatar {{
            width: 36px;
            height: 36px;
            border-radius: 50%;
            background: var(--primary);
            color: white;
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: 600;
            font-size: 14px;
        }}

        .user-info {{
            display: flex;
            align-items: center;
            gap: 12px;
        }}

        .user-name {{
            font-weight: 500;
        }}

        .user-email {{
            font-size: 13px;
            color: var(--text-secondary);
        }}

        /* Badges */
        .badge {{
            display: inline-block;
            padding: 4px 10px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }}

        .badge-active {{
            background: #D1FAE5;
            color: #065F46;
        }}

        .badge-inactive {{
            background: #FEE2E2;
            color: #991B1B;
        }}

        .badge-pending {{
            background: #FEF3C7;
            color: #92400E;
        }}

        .badge-role {{
            background: #E0E7FF;
            color: #3730A3;
        }}

        /* Actions */
        .row-actions {{
            display: flex;
            gap: 8px;
        }}

        .action-btn {{
            padding: 6px;
            border: none;
            border-radius: 4px;
            background: transparent;
            color: var(--text-secondary);
            cursor: pointer;
        }}

        .action-btn:hover {{
            background: var(--bg);
            color: var(--primary);
        }}

        .action-btn.danger:hover {{
            color: var(--danger);
        }}

        /* Modal */
        .modal-overlay {{
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            z-index: 1000;
            align-items: center;
            justify-content: center;
        }}

        .modal-overlay.active {{
            display: flex;
        }}

        .modal {{
            background: var(--surface);
            border-radius: 12px;
            width: 100%;
            max-width: 500px;
            max-height: 90vh;
            overflow-y: auto;
        }}

        .modal-header {{
            padding: 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}

        .modal-title {{
            font-size: 18px;
            font-weight: 600;
        }}

        .modal-close {{
            background: none;
            border: none;
            font-size: 24px;
            cursor: pointer;
            color: var(--text-secondary);
        }}

        .modal-body {{
            padding: 20px;
        }}

        .modal-footer {{
            padding: 16px 20px;
            border-top: 1px solid var(--border);
            display: flex;
            justify-content: flex-end;
            gap: 12px;
        }}

        /* Form */
        .form-group {{
            margin-bottom: 16px;
        }}

        .form-label {{
            display: block;
            margin-bottom: 6px;
            font-size: 14px;
            font-weight: 500;
        }}

        .form-input,
        .form-select {{
            width: 100%;
            padding: 10px 12px;
            border: 1px solid var(--border);
            border-radius: 6px;
            font-size: 14px;
        }}

        .form-input:focus,
        .form-select:focus {{
            outline: none;
            border-color: var(--primary);
        }}

        .form-help {{
            font-size: 12px;
            color: var(--text-secondary);
            margin-top: 4px;
        }}

        /* Pagination */
        .pagination {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 16px 20px;
            border-top: 1px solid var(--border);
        }}

        .pagination-info {{
            font-size: 14px;
            color: var(--text-secondary);
        }}

        .pagination-buttons {{
            display: flex;
            gap: 8px;
        }}

        /* Profiles Multi-select */
        .profile-checkbox {{
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 8px 12px;
            margin: 4px 0;
            border-radius: 6px;
            cursor: pointer;
            transition: background-color 0.2s;
        }}

        .profile-checkbox:hover {{
            background-color: #f3f4f6;
        }}

        .profile-checkbox input[type="checkbox"] {{
            width: 18px;
            height: 18px;
            cursor: pointer;
        }}

        .profile-checkbox span:nth-child(2) {{
            flex: 1;
            font-size: 14px;
            color: #374151;
        }}

        .profile-category {{
            margin-bottom: 16px;
        }}

        /* Profile Level Badges */
        .badge-level-0 {{
            background-color: #ef4444;
            color: white;
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 12px;
            font-weight: 600;
        }}

        .badge-level-10 {{
            background-color: #f97316;
            color: white;
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 12px;
            font-weight: 600;
        }}

        .badge-level-25,
        .badge-level-28,
        .badge-level-30,
        .badge-level-35 {{
            background-color: #3b82f6;
            color: white;
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 12px;
            font-weight: 600;
        }}

        .badge-level-40,
        .badge-level-45,
        .badge-level-50,
        .badge-level-55 {{
            background-color: #8b5cf6;
            color: white;
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 12px;
            font-weight: 600;
        }}

        .badge-level-60,
        .badge-level-65 {{
            background-color: #10b981;
            color: white;
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 12px;
            font-weight: 600;
        }}

        .badge-level-100 {{
            background-color: #6b7280;
            color: white;
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 12px;
            font-weight: 600;
        }}

        /* Responsive */
        @media (max-width: 768px) {{
            .stats-row {{
                grid-template-columns: repeat(2, 1fr);
            }}
            .filters {{
                flex-direction: column;
            }}
            .header {{
                flex-direction: column;
                gap: 16px;
            }}
        }}
    </style>
</head>
<body>
    <div class="dashboard">
        <div class="header">
            <h1>Administracao de Usuarios</h1>
            <div class="header-actions">
                <button class="btn btn-secondary" onclick="openInviteModal()">
                    Convidar Usuario
                </button>
                <!-- Issue #295: Added add-user-btn class and data-testid for test compatibility -->
                <button class="btn btn-primary add-user-btn" id="btn-add-user" data-testid="add-user-btn" onclick="openCreateModal()">
                    + Adicionar Usuario
                </button>
            </div>
        </div>

        <!-- Stats -->
        <div class="stats-row" id="stats-row">
            <div class="stat-card">
                <div class="stat-value" id="stat-total">-</div>
                <div class="stat-label">Total de Usuarios</div>
            </div>
            <div class="stat-card">
                <div class="stat-value" id="stat-active">-</div>
                <div class="stat-label">Ativos</div>
            </div>
            <div class="stat-card">
                <div class="stat-value" id="stat-inactive">-</div>
                <div class="stat-label">Inativos</div>
            </div>
            <div class="stat-card">
                <div class="stat-value" id="stat-invites">-</div>
                <div class="stat-label">Convites Pendentes</div>
            </div>
        </div>

        <!-- Filters -->
        <div class="filters">
            <div class="filter-group">
                <label class="filter-label">Status</label>
                <select class="filter-select" id="filter-status" onchange="applyFilters()">
                    <option value="">Todos</option>
                    <option value="active">Ativos</option>
                    <option value="inactive">Inativos</option>
                    <option value="pending">Pendentes</option>
                </select>
            </div>
            <div class="filter-group">
                <label class="filter-label">Role</label>
                <select class="filter-select" id="filter-role" onchange="applyFilters()">
                    <option value="">Todas</option>
                    <option value="owner">Owner</option>
                    <option value="admin">Admin</option>
                    <option value="member">Member</option>
                    <option value="viewer">Viewer</option>
                </select>
            </div>
            <!-- Issue #295: Added search-users class and data-testid for test compatibility -->
            <div class="filter-group search-container">
                <label class="filter-label" for="search-users">Buscar</label>
                <input type="text" class="filter-input search-input search-users" id="search-users"
                    data-testid="search-users" placeholder="Buscar usuarios..." onkeyup="debounceSearch()">
            </div>
        </div>

        <!-- Users Table -->
        <div class="panel">
            <table class="users-table">
                <thead>
                    <tr>
                        <th>Usuario</th>
                        <th>Role</th>
                        <th>Status</th>
                        <th>Ultimo Acesso</th>
                        <th>Acoes</th>
                    </tr>
                </thead>
                <tbody id="users-tbody">
                    <tr>
                        <td colspan="5" style="text-align: center; padding: 40px; color: var(--text-secondary);">
                            Carregando usuarios...
                        </td>
                    </tr>
                </tbody>
            </table>
            <div class="pagination">
                <div class="pagination-info" id="pagination-info">
                    Mostrando 0 de 0 usuarios
                </div>
                <div class="pagination-buttons">
                    <button class="btn btn-outline btn-sm" id="btn-prev" onclick="prevPage()" disabled>
                        Anterior
                    </button>
                    <button class="btn btn-outline btn-sm" id="btn-next" onclick="nextPage()" disabled>
                        Proximo
                    </button>
                </div>
            </div>
        </div>
    </div>

    <!-- Create User Modal -->
    <div class="modal-overlay" id="create-modal">
        <div class="modal">
            <div class="modal-header">
                <h3 class="modal-title">Criar Usuario</h3>
                <button class="modal-close" onclick="closeCreateModal()">&times;</button>
            </div>
            <div class="modal-body">
                <form id="create-form">
                    <div class="form-group">
                        <label class="form-label">Nome de Usuario *</label>
                        <input type="text" class="form-input" id="create-username" required>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Email *</label>
                        <input type="email" class="form-input" id="create-email" required>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Senha *</label>
                        <input type="password" class="form-input" id="create-password" required>
                        <div class="form-help">Minimo 8 caracteres</div>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Role no Tenant (Legacy)</label>
                        <select class="form-select" id="create-role">
                            <option value="member">Member</option>
                            <option value="admin">Admin</option>
                            <option value="viewer">Viewer</option>
                        </select>
                        <div class="form-help">Mantido por compatibilidade. Use Perfis abaixo.</div>
                    </div>

                    <!-- NOVO: Multi-select de Perfis -->
                    <div class="form-group">
                        <label class="form-label">Perfis * (Multiplos)</label>
                        <div class="form-help">Selecione um ou mais perfis. Permissoes serao acumuladas.</div>

                        <div class="profiles-grid" style="max-height: 400px; overflow-y: auto; border: 1px solid #e5e7eb; border-radius: 8px; padding: 12px;">
                            <!-- Plataforma -->
                            <div class="profile-category">
                                <h4 style="font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; margin: 8px 0;">Plataforma</h4>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="super_admin" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Super Admin</span>
                                    <span class="badge badge-level-0">Level 0</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="admin" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Admin</span>
                                    <span class="badge badge-level-10">Level 10</span>
                                </label>
                            </div>

                            <!-- Gestão -->
                            <div class="profile-category">
                                <h4 style="font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; margin: 8px 0;">Gestao</h4>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="product_manager" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Product Manager</span>
                                    <span class="badge badge-level-25">Level 25</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="product_owner" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Product Owner</span>
                                    <span class="badge badge-level-28">Level 28</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="project_manager" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Project Manager</span>
                                    <span class="badge badge-level-30">Level 30</span>
                                </label>
                            </div>

                            <!-- Desenvolvimento -->
                            <div class="profile-category">
                                <h4 style="font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; margin: 8px 0;">Desenvolvimento</h4>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="dev_frontend" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Dev Frontend</span>
                                    <span class="badge badge-level-50">Level 50</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="dev_backend" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Dev Backend</span>
                                    <span class="badge badge-level-50">Level 50</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="dev_mobile" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Dev Mobile</span>
                                    <span class="badge badge-level-50">Level 50</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="dev_fullstack" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Dev Fullstack</span>
                                    <span class="badge badge-level-50">Level 50</span>
                                </label>
                            </div>

                            <!-- Qualidade -->
                            <div class="profile-category">
                                <h4 style="font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; margin: 8px 0;">Qualidade</h4>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="qa_manual" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>QA Manual</span>
                                    <span class="badge badge-level-60">Level 60</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="qa_automation" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>QA Automation</span>
                                    <span class="badge badge-level-60">Level 60</span>
                                </label>
                            </div>

                            <!-- Processo & Documentação -->
                            <div class="profile-category">
                                <h4 style="font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; margin: 8px 0;">Processo & Documentacao</h4>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="bpm_analyst" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>BPM Analyst</span>
                                    <span class="badge badge-level-45">Level 45</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="documentador" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Documentador</span>
                                    <span class="badge badge-level-65">Level 65</span>
                                </label>
                            </div>

                            <!-- Outros -->
                            <div class="profile-category">
                                <h4 style="font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; margin: 8px 0;">Outros</h4>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="tech_lead" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Tech Lead</span>
                                    <span class="badge badge-level-40">Level 40</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="designer" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Designer</span>
                                    <span class="badge badge-level-55">Level 55</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="business_analyst" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Business Analyst</span>
                                    <span class="badge badge-level-35">Level 35</span>
                                </label>
                                <label class="profile-checkbox">
                                    <input type="checkbox" value="viewer" class="profile-input" onchange="updatePermissionsPreview()">
                                    <span>Viewer</span>
                                    <span class="badge badge-level-100">Level 100</span>
                                </label>
                            </div>
                        </div>
                    </div>

                    <!-- Preview de Permissões -->
                    <div class="form-group" id="permissions-preview-container" style="display: none;">
                        <label class="form-label">Permissoes que serao concedidas:</label>
                        <div id="permissions-preview" style="max-height: 150px; overflow-y: auto; background-color: #f9fafb; border: 1px solid #e5e7eb; border-radius: 4px; padding: 8px; font-size: 12px;">
                        </div>
                    </div>
                </form>
            </div>
            <div class="modal-footer">
                <button class="btn btn-outline" onclick="closeCreateModal()">Cancelar</button>
                <button class="btn btn-primary" onclick="createUser()">Criar Usuario</button>
            </div>
        </div>
    </div>

    <!-- Invite User Modal -->
    <div class="modal-overlay" id="invite-modal">
        <div class="modal">
            <div class="modal-header">
                <h3 class="modal-title">Convidar Usuario</h3>
                <button class="modal-close" onclick="closeInviteModal()">&times;</button>
            </div>
            <div class="modal-body">
                <form id="invite-form">
                    <div class="form-group">
                        <label class="form-label">Email *</label>
                        <input type="email" class="form-input" id="invite-email" required>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Role</label>
                        <select class="form-select" id="invite-role">
                            <option value="member">Member</option>
                            <option value="admin">Admin</option>
                            <option value="viewer">Viewer</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Mensagem (opcional)</label>
                        <textarea class="form-input" id="invite-message" rows="3"
                            placeholder="Adicione uma mensagem personalizada..."></textarea>
                    </div>
                </form>
            </div>
            <div class="modal-footer">
                <button class="btn btn-outline" onclick="closeInviteModal()">Cancelar</button>
                <button class="btn btn-secondary" onclick="sendInvite()">Enviar Convite</button>
            </div>
        </div>
    </div>

    <!-- Edit User Modal -->
    <div class="modal-overlay" id="edit-modal">
        <div class="modal">
            <div class="modal-header">
                <h3 class="modal-title">Editar Usuario</h3>
                <button class="modal-close" onclick="closeEditModal()">&times;</button>
            </div>
            <div class="modal-body">
                <form id="edit-form">
                    <input type="hidden" id="edit-user-id">
                    <div class="form-group">
                        <label class="form-label">Email</label>
                        <input type="email" class="form-input" id="edit-email">
                    </div>
                    <div class="form-group">
                        <label class="form-label">Role no Tenant</label>
                        <select class="form-select" id="edit-role">
                            <option value="member">Member</option>
                            <option value="admin">Admin</option>
                            <option value="viewer">Viewer</option>
                            <option value="owner">Owner</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Nova Senha (deixe em branco para manter)</label>
                        <input type="password" class="form-input" id="edit-password">
                    </div>
                </form>
            </div>
            <div class="modal-footer">
                <button class="btn btn-outline" onclick="closeEditModal()">Cancelar</button>
                <button class="btn btn-primary" onclick="saveUser()">Salvar</button>
            </div>
        </div>
    </div>

    <script>
        const TENANT_ID = "{tenant_id}";
        let currentPage = 1;
        let totalPages = 1;
        let searchTimeout = null;

        // Load data on page load
        document.addEventListener('DOMContentLoaded', () => {{
            loadUsers();
            loadStats();
        }});

        async function loadStats() {{
            try {{
                const res = await fetch(`/api/v1/admin/users/stats?tenant_id=${{TENANT_ID}}`);
                const data = await res.json();
                const stats = data.stats;

                document.getElementById('stat-total').textContent = stats.total || 0;
                document.getElementById('stat-active').textContent = stats.active || 0;
                document.getElementById('stat-inactive').textContent = stats.inactive || 0;
                document.getElementById('stat-invites').textContent = stats.pending_invites || 0;
            }} catch (error) {{
                console.error('Error loading stats:', error);
            }}
        }}

        async function loadUsers() {{
            const status = document.getElementById('filter-status').value;
            const role = document.getElementById('filter-role').value;
            // Issue #295: Updated to use new search-users ID
            const search = document.getElementById('search-users').value;

            const params = new URLSearchParams({{
                tenant_id: TENANT_ID,
                page: currentPage,
                limit: 20
            }});

            if (status) params.append('status', status);
            if (role) params.append('role', role);
            if (search) params.append('search', search);

            try {{
                const res = await fetch(`/api/v1/admin/users?${{params}}`);
                const data = await res.json();

                renderUsers(data.users);
                totalPages = data.pages;
                updatePagination(data);
            }} catch (error) {{
                console.error('Error loading users:', error);
            }}
        }}

        function renderUsers(users) {{
            const tbody = document.getElementById('users-tbody');

            if (!users || users.length === 0) {{
                tbody.innerHTML = `
                    <tr>
                        <td colspan="5" style="text-align: center; padding: 40px; color: var(--text-secondary);">
                            Nenhum usuario encontrado
                        </td>
                    </tr>
                `;
                return;
            }}

            tbody.innerHTML = users.map(user => `
                <tr>
                    <td>
                        <div class="user-info">
                            <div class="user-avatar">
                                ${{(user.username || 'U').charAt(0).toUpperCase()}}
                            </div>
                            <div>
                                <div class="user-name">${{user.username}}</div>
                                <div class="user-email">${{user.email || '-'}}</div>
                            </div>
                        </div>
                    </td>
                    <td>
                        <span class="badge badge-role">${{(user.tenant_role || user.role || 'member').toUpperCase()}}</span>
                    </td>
                    <td>
                        <span class="badge ${{user.active ? 'badge-active' : 'badge-inactive'}}">
                            ${{user.active ? 'Ativo' : 'Inativo'}}
                        </span>
                    </td>
                    <td>
                        ${{user.last_login ? new Date(user.last_login).toLocaleDateString() : 'Nunca'}}
                    </td>
                    <td>
                        <div class="row-actions">
                            <button class="action-btn" title="Editar" onclick="editUser(${{user.id}})">
                                <svg width="16" height="16" fill="currentColor" viewBox="0 0 16 16">
                                    <path d="M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168l10-10zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207 11.207 2.5zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293l6.5-6.5zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325z"/>
                                </svg>
                            </button>
                            <button class="action-btn danger" title="Desativar" onclick="toggleUser(${{user.id}}, ${{user.active}})">
                                <svg width="16" height="16" fill="currentColor" viewBox="0 0 16 16">
                                    <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"/>
                                    <path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/>
                                </svg>
                            </button>
                        </div>
                    </td>
                </tr>
            `).join('');
        }}

        function updatePagination(data) {{
            const info = document.getElementById('pagination-info');
            const start = ((data.page - 1) * data.limit) + 1;
            const end = Math.min(data.page * data.limit, data.total);
            info.textContent = `Mostrando ${{start}}-${{end}} de ${{data.total}} usuarios`;

            document.getElementById('btn-prev').disabled = currentPage <= 1;
            document.getElementById('btn-next').disabled = currentPage >= totalPages;
        }}

        function applyFilters() {{
            currentPage = 1;
            loadUsers();
        }}

        function debounceSearch() {{
            clearTimeout(searchTimeout);
            searchTimeout = setTimeout(() => {{
                currentPage = 1;
                loadUsers();
            }}, 300);
        }}

        function prevPage() {{
            if (currentPage > 1) {{
                currentPage--;
                loadUsers();
            }}
        }}

        function nextPage() {{
            if (currentPage < totalPages) {{
                currentPage++;
                loadUsers();
            }}
        }}

        // Modals
        function openCreateModal() {{
            document.getElementById('create-modal').classList.add('active');
        }}

        function closeCreateModal() {{
            document.getElementById('create-modal').classList.remove('active');
            document.getElementById('create-form').reset();
        }}

        function openInviteModal() {{
            document.getElementById('invite-modal').classList.add('active');
        }}

        function closeInviteModal() {{
            document.getElementById('invite-modal').classList.remove('active');
            document.getElementById('invite-form').reset();
        }}

        function openEditModal() {{
            document.getElementById('edit-modal').classList.add('active');
        }}

        function closeEditModal() {{
            document.getElementById('edit-modal').classList.remove('active');
            document.getElementById('edit-form').reset();
        }}

        async function createUser() {{
            const username = document.getElementById('create-username').value;
            const email = document.getElementById('create-email').value;
            const password = document.getElementById('create-password').value;
            const role = document.getElementById('create-role').value;

            // Obter perfis selecionados
            const selectedProfiles = Array.from(document.querySelectorAll('.profile-input:checked'))
                .map(input => input.value);

            if (!username || !email || !password) {{
                alert('Preencha todos os campos obrigatorios');
                return;
            }}

            if (password.length < 8) {{
                alert('A senha deve ter no minimo 8 caracteres');
                return;
            }}

            if (selectedProfiles.length === 0) {{
                alert('Selecione ao menos um perfil');
                return;
            }}

            try {{
                // 1. Criar usuario
                const res = await fetch(`/api/v1/admin/users?tenant_id=${{TENANT_ID}}`, {{
                    method: 'POST',
                    headers: {{'Content-Type': 'application/json'}},
                    body: JSON.stringify({{ username, email, password, tenant_role: role }})
                }});

                const data = await res.json();

                if (res.ok) {{
                    const userId = data.user.id;

                    // 2. Atribuir perfis
                    const profilesRes = await fetch(`/api/v1/admin/users/${{userId}}/profiles`, {{
                        method: 'POST',
                        headers: {{'Content-Type': 'application/json'}},
                        body: JSON.stringify({{
                            profile_ids: selectedProfiles,
                            scope: 'global'
                        }})
                    }});

                    if (profilesRes.ok) {{
                        alert('Usuario criado com sucesso com ' + selectedProfiles.length + ' perfis!');
                        closeCreateModal();
                        loadUsers();
                        loadStats();
                    }} else {{
                        const profilesData = await profilesRes.json();
                        alert('Usuario criado mas erro ao atribuir perfis: ' + (profilesData.detail || 'Erro desconhecido'));
                        closeCreateModal();
                        loadUsers();
                    }}
                }} else {{
                    alert(data.detail || 'Erro ao criar usuario');
                }}
            }} catch (error) {{
                alert('Erro ao criar usuario');
                console.error(error);
            }}
        }}

        async function sendInvite() {{
            const email = document.getElementById('invite-email').value;
            const role = document.getElementById('invite-role').value;
            const message = document.getElementById('invite-message').value;

            if (!email) {{
                alert('Informe o email');
                return;
            }}

            try {{
                const res = await fetch(`/api/v1/admin/users/invite?tenant_id=${{TENANT_ID}}`, {{
                    method: 'POST',
                    headers: {{'Content-Type': 'application/json'}},
                    body: JSON.stringify({{ email, role, message }})
                }});

                const data = await res.json();

                if (res.ok) {{
                    alert('Convite enviado com sucesso!');
                    closeInviteModal();
                    loadStats();
                }} else {{
                    alert(data.detail || 'Erro ao enviar convite');
                }}
            }} catch (error) {{
                alert('Erro ao enviar convite');
                console.error(error);
            }}
        }}

        async function editUser(userId) {{
            try {{
                const res = await fetch(`/api/v1/admin/users/${{userId}}`);
                const data = await res.json();
                const user = data.user;

                document.getElementById('edit-user-id').value = userId;
                document.getElementById('edit-email').value = user.email || '';
                document.getElementById('edit-role').value = user.tenant_role || user.role || 'member';
                document.getElementById('edit-password').value = '';

                openEditModal();
            }} catch (error) {{
                alert('Erro ao carregar usuario');
                console.error(error);
            }}
        }}

        async function saveUser() {{
            const userId = document.getElementById('edit-user-id').value;
            const email = document.getElementById('edit-email').value;
            const role = document.getElementById('edit-role').value;
            const password = document.getElementById('edit-password').value;

            const body = {{ email }};
            if (password) body.password = password;

            try {{
                // Update user
                const res1 = await fetch(`/api/v1/admin/users/${{userId}}`, {{
                    method: 'PUT',
                    headers: {{'Content-Type': 'application/json'}},
                    body: JSON.stringify(body)
                }});

                // Update role
                const res2 = await fetch(`/api/v1/admin/users/${{userId}}/roles`, {{
                    method: 'POST',
                    headers: {{'Content-Type': 'application/json'}},
                    body: JSON.stringify({{ role, tenant_id: TENANT_ID }})
                }});

                if (res1.ok) {{
                    alert('Usuario atualizado com sucesso!');
                    closeEditModal();
                    loadUsers();
                }} else {{
                    const data = await res1.json();
                    alert(data.detail || 'Erro ao atualizar usuario');
                }}
            }} catch (error) {{
                alert('Erro ao atualizar usuario');
                console.error(error);
            }}
        }}

        async function toggleUser(userId, isActive) {{
            const action = isActive ? 'desativar' : 'ativar';
            if (!confirm(`Deseja ${{action}} este usuario?`)) return;

            try {{
                if (isActive) {{
                    await fetch(`/api/v1/admin/users/${{userId}}`, {{ method: 'DELETE' }});
                }} else {{
                    await fetch(`/api/v1/admin/users/${{userId}}/activate`, {{ method: 'POST' }});
                }}

                loadUsers();
                loadStats();
            }} catch (error) {{
                alert('Erro ao alterar status do usuario');
                console.error(error);
            }}
        }}

        // Mapa de permissões por perfil (simplificado para preview)
        const PROFILE_PERMISSIONS = {{
            'super_admin': ['*:*'],
            'admin': ['*:manage'],
            'product_manager': ['stories:*', 'epics:*', 'roadmap:*', 'analytics:read'],
            'product_owner': ['backlog:manage', 'stories:prioritize', 'stories:create', 'planning_poker:*'],
            'project_manager': ['projects:*', 'sprints:*', 'stories:*', 'tasks:*', 'reports:*'],
            'dev_frontend': ['code:frontend:*', 'stories:read', 'stories:update', 'tasks:*', 'components:*'],
            'dev_backend': ['code:backend:*', 'stories:read', 'stories:update', 'tasks:*', 'api:*', 'database:manage'],
            'dev_mobile': ['code:mobile:*', 'stories:read', 'stories:update', 'tasks:*'],
            'dev_fullstack': ['code:*', 'stories:read', 'stories:update', 'tasks:*', 'api:*'],
            'qa_manual': ['tests:manual:*', 'bugs:*', 'stories:test', 'test_cases:*'],
            'qa_automation': ['tests:automation:*', 'ci_cd:read', 'test_results:*', 'bugs:*'],
            'bpm_analyst': ['processes:*', 'workflows:*', 'automation:*', 'integrations:*'],
            'documentador': ['documentation:*', 'stories:read', 'wiki:*', 'knowledge_base:*'],
            'tech_lead': ['code:*', 'code_review:*', 'architecture:*', 'technical_decisions:*'],
            'designer': ['designs:*', 'prototypes:*', 'stories:read', 'ux_research:*'],
            'business_analyst': ['requirements:*', 'stories:create', 'stories:read', 'analytics:read'],
            'viewer': ['*:read']
        }};

        function updatePermissionsPreview() {{
            const selectedProfiles = Array.from(document.querySelectorAll('.profile-input:checked'))
                .map(input => input.value);

            const previewContainer = document.getElementById('permissions-preview-container');
            const previewDiv = document.getElementById('permissions-preview');

            if (selectedProfiles.length === 0) {{
                previewContainer.style.display = 'none';
                return;
            }}

            // Calcular UNION de permissões
            const allPermissions = new Set();
            selectedProfiles.forEach(profileId => {{
                const perms = PROFILE_PERMISSIONS[profileId] || [];
                perms.forEach(p => allPermissions.add(p));
            }});

            // Renderizar preview
            const permsList = Array.from(allPermissions).sort();
            previewDiv.innerHTML = permsList.map(p => `<code style="background-color: #e5e7eb; padding: 2px 6px; border-radius: 4px; margin: 2px; display: inline-block;">${{p}}</code>`).join(' ');
            previewContainer.style.display = 'block';
        }}
    </script>
</body>
</html>
'''


# =============================================================================
# API ENDPOINTS
# =============================================================================

def register_admin_users_endpoints(app, tenant_id: str = None):
    """
    Registra endpoints do painel de usuarios no app FastAPI

    Args:
        app: Instancia do FastAPI
        tenant_id: ID do tenant (opcional)
    """
    from fastapi import APIRouter
    from fastapi.responses import HTMLResponse

    router = APIRouter(prefix="/admin/users", tags=["admin-users"])

    @router.get("/", response_class=HTMLResponse)
    async def admin_users_page(tenant: str = "default"):
        """Retorna pagina HTML do painel de usuarios"""
        return generate_admin_users_html(tenant)

    @router.get("/stats")
    async def get_stats(tenant_id: str):
        """Retorna estatisticas de usuarios"""
        stats = get_user_stats_for_tenant(tenant_id)
        return {"stats": stats}

    @router.get("/invites")
    async def get_invites(tenant_id: str):
        """Retorna convites pendentes"""
        invites = get_pending_invites(tenant_id)
        return {"invites": invites}

    app.include_router(router)
    print("[Dashboard] Admin Users endpoints registered")


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI
    from fastapi.responses import HTMLResponse

    app = FastAPI(title="Admin Users Panel")

    # Include admin routes
    from factory.api.admin_routes import router as admin_router
    app.include_router(admin_router)

    register_admin_users_endpoints(app)

    @app.get("/", response_class=HTMLResponse)
    async def root():
        return generate_admin_users_html("default")

    print("[Admin Users] Starting standalone server on port 9003...")
    uvicorn.run(app, host="0.0.0.0", port=9003)

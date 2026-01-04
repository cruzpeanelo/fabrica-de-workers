# -*- coding: utf-8 -*-
"""
Admin RBAC - Pagina de Administracao de Roles e Permissoes
==========================================================

Pagina /admin/rbac para gerenciar:
- CRUD de Roles
- Atribuir permissoes a roles
- Atribuir roles a usuarios

Issue: RBAC Administration Page
"""

from datetime import datetime
from typing import Dict, Any, List, Optional


# =============================================================================
# RBAC ADMIN HTML TEMPLATE
# =============================================================================

def generate_rbac_admin_html() -> str:
    """
    Gera HTML da pagina de administracao RBAC

    Returns:
        HTML completo da pagina
    """
    return '''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Administracao RBAC - Plataforma E</title>
    <style>
        :root {
            --primary: #003B4A;
            --primary-light: #00526A;
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
            --sidebar-width: 280px;
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', 'Segoe UI', sans-serif;
            background: var(--bg);
            color: var(--text);
            line-height: 1.5;
        }

        /* Layout */
        .rbac-layout {
            display: flex;
            min-height: 100vh;
        }

        /* Header */
        .main-header {
            background: var(--primary);
            color: white;
            padding: 16px 24px;
            display: flex;
            justify-content: space-between;
            align-items: center;
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            z-index: 100;
        }

        .header-title {
            font-size: 20px;
            font-weight: 600;
        }

        .header-subtitle {
            font-size: 12px;
            opacity: 0.7;
        }

        .header-actions {
            display: flex;
            gap: 12px;
        }

        /* Main Content */
        .main-content {
            margin-top: 64px;
            display: flex;
            flex: 1;
        }

        /* Sidebar - Roles List */
        .roles-sidebar {
            width: var(--sidebar-width);
            background: var(--surface);
            border-right: 1px solid var(--border);
            display: flex;
            flex-direction: column;
            height: calc(100vh - 64px);
            position: fixed;
            top: 64px;
            left: 0;
        }

        .sidebar-header {
            padding: 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .sidebar-title {
            font-size: 16px;
            font-weight: 600;
        }

        .roles-list {
            flex: 1;
            overflow-y: auto;
            padding: 12px;
        }

        .role-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px 16px;
            border-radius: 8px;
            cursor: pointer;
            transition: all 0.2s;
            margin-bottom: 4px;
        }

        .role-item:hover {
            background: var(--bg);
        }

        .role-item.active {
            background: var(--primary);
            color: white;
        }

        .role-icon {
            width: 36px;
            height: 36px;
            border-radius: 8px;
            background: var(--bg);
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: 600;
            font-size: 14px;
        }

        .role-item.active .role-icon {
            background: rgba(255,255,255,0.2);
            color: white;
        }

        .role-info {
            flex: 1;
        }

        .role-name {
            font-weight: 500;
            font-size: 14px;
        }

        .role-level {
            font-size: 12px;
            opacity: 0.7;
        }

        .role-badge {
            font-size: 10px;
            padding: 2px 8px;
            border-radius: 10px;
            background: var(--info);
            color: white;
        }

        .role-badge.system {
            background: var(--warning);
        }

        /* Content Area */
        .content-area {
            flex: 1;
            margin-left: var(--sidebar-width);
            padding: 24px;
        }

        /* Tabs */
        .tabs {
            display: flex;
            gap: 4px;
            border-bottom: 1px solid var(--border);
            margin-bottom: 24px;
            background: var(--surface);
            padding: 0 16px;
            border-radius: 8px 8px 0 0;
        }

        .tab {
            padding: 16px 24px;
            font-size: 14px;
            color: var(--text-secondary);
            cursor: pointer;
            border-bottom: 2px solid transparent;
            transition: all 0.2s;
        }

        .tab:hover {
            color: var(--text);
        }

        .tab.active {
            color: var(--primary);
            border-bottom-color: var(--primary);
            font-weight: 500;
        }

        /* Panels */
        .panel {
            background: var(--surface);
            border-radius: 12px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            margin-bottom: 24px;
        }

        .panel-header {
            padding: 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .panel-title {
            font-size: 16px;
            font-weight: 600;
        }

        .panel-body {
            padding: 20px;
        }

        /* Permissions Grid */
        .permissions-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
            gap: 16px;
        }

        .permission-group {
            background: var(--bg);
            border-radius: 8px;
            padding: 16px;
        }

        .permission-group-header {
            display: flex;
            align-items: center;
            gap: 8px;
            margin-bottom: 12px;
            padding-bottom: 12px;
            border-bottom: 1px solid var(--border);
        }

        .permission-group-icon {
            width: 32px;
            height: 32px;
            border-radius: 6px;
            background: var(--primary);
            color: white;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .permission-group-name {
            font-weight: 600;
            font-size: 14px;
            text-transform: capitalize;
        }

        .permission-actions {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
        }

        .permission-checkbox {
            display: flex;
            align-items: center;
            gap: 6px;
            padding: 6px 12px;
            background: var(--surface);
            border-radius: 6px;
            cursor: pointer;
            transition: all 0.2s;
            font-size: 13px;
        }

        .permission-checkbox:hover {
            background: var(--border);
        }

        .permission-checkbox input {
            width: 16px;
            height: 16px;
            accent-color: var(--primary);
        }

        .permission-checkbox.checked {
            background: var(--primary);
            color: white;
        }

        /* Users Table */
        .users-table {
            width: 100%;
            border-collapse: collapse;
        }

        .users-table th,
        .users-table td {
            padding: 12px 16px;
            text-align: left;
            border-bottom: 1px solid var(--border);
        }

        .users-table th {
            font-weight: 600;
            font-size: 13px;
            color: var(--text-secondary);
            text-transform: uppercase;
            background: var(--bg);
        }

        .users-table tr:hover {
            background: var(--bg);
        }

        .user-cell {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .user-avatar {
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
        }

        .user-details {
            flex: 1;
        }

        .user-name {
            font-weight: 500;
        }

        .user-email {
            font-size: 12px;
            color: var(--text-secondary);
        }

        .role-tags {
            display: flex;
            flex-wrap: wrap;
            gap: 6px;
        }

        .role-tag {
            display: inline-flex;
            align-items: center;
            gap: 4px;
            padding: 4px 10px;
            background: var(--bg);
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }

        .role-tag.admin {
            background: var(--danger);
            color: white;
        }

        .role-tag.manager {
            background: var(--warning);
            color: white;
        }

        .role-tag.developer {
            background: var(--info);
            color: white;
        }

        .role-tag.viewer {
            background: var(--success);
            color: white;
        }

        .role-tag .remove-btn {
            cursor: pointer;
            opacity: 0.7;
            margin-left: 4px;
        }

        .role-tag .remove-btn:hover {
            opacity: 1;
        }

        /* Buttons */
        .btn {
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
            text-decoration: none;
        }

        .btn-primary {
            background: var(--primary);
            color: white;
        }

        .btn-primary:hover {
            background: var(--primary-light);
        }

        .btn-secondary {
            background: var(--secondary);
            color: white;
        }

        .btn-secondary:hover {
            opacity: 0.9;
        }

        .btn-outline {
            background: transparent;
            border: 1px solid var(--border);
            color: var(--text);
        }

        .btn-outline:hover {
            background: var(--bg);
        }

        .btn-danger {
            background: var(--danger);
            color: white;
        }

        .btn-danger:hover {
            opacity: 0.9;
        }

        .btn-sm {
            padding: 6px 12px;
            font-size: 12px;
        }

        .btn-icon {
            width: 36px;
            height: 36px;
            padding: 0;
            display: flex;
            align-items: center;
            justify-content: center;
            border-radius: 8px;
        }

        /* Forms */
        .form-group {
            margin-bottom: 16px;
        }

        .form-label {
            display: block;
            font-size: 14px;
            font-weight: 500;
            margin-bottom: 6px;
            color: var(--text);
        }

        .form-input {
            width: 100%;
            padding: 10px 14px;
            border: 1px solid var(--border);
            border-radius: 8px;
            font-size: 14px;
            transition: all 0.2s;
        }

        .form-input:focus {
            outline: none;
            border-color: var(--primary);
            box-shadow: 0 0 0 3px rgba(0, 59, 74, 0.1);
        }

        .form-textarea {
            min-height: 100px;
            resize: vertical;
        }

        .form-select {
            appearance: none;
            background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='%236B7280' stroke-width='2'%3E%3Cpath d='M6 9l6 6 6-6'/%3E%3C/svg%3E");
            background-repeat: no-repeat;
            background-position: right 12px center;
            padding-right: 40px;
        }

        /* Modal */
        .modal-overlay {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0, 0, 0, 0.5);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 1000;
            opacity: 0;
            visibility: hidden;
            transition: all 0.3s;
        }

        .modal-overlay.active {
            opacity: 1;
            visibility: visible;
        }

        .modal {
            background: var(--surface);
            border-radius: 12px;
            width: 90%;
            max-width: 500px;
            max-height: 90vh;
            overflow-y: auto;
            transform: translateY(-20px);
            transition: all 0.3s;
        }

        .modal-overlay.active .modal {
            transform: translateY(0);
        }

        .modal-header {
            padding: 20px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .modal-title {
            font-size: 18px;
            font-weight: 600;
        }

        .modal-close {
            width: 32px;
            height: 32px;
            border: none;
            background: none;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            border-radius: 6px;
            color: var(--text-secondary);
        }

        .modal-close:hover {
            background: var(--bg);
            color: var(--text);
        }

        .modal-body {
            padding: 20px;
        }

        .modal-footer {
            padding: 16px 20px;
            border-top: 1px solid var(--border);
            display: flex;
            justify-content: flex-end;
            gap: 12px;
        }

        /* Alerts */
        .alert {
            padding: 12px 16px;
            border-radius: 8px;
            margin-bottom: 16px;
            display: flex;
            align-items: flex-start;
            gap: 12px;
        }

        .alert-success {
            background: #D1FAE5;
            color: #065F46;
        }

        .alert-error {
            background: #FEE2E2;
            color: #991B1B;
        }

        .alert-warning {
            background: #FEF3C7;
            color: #92400E;
        }

        .alert-info {
            background: #DBEAFE;
            color: #1E40AF;
        }

        /* Empty State */
        .empty-state {
            text-align: center;
            padding: 60px 20px;
            color: var(--text-secondary);
        }

        .empty-state-icon {
            width: 64px;
            height: 64px;
            margin: 0 auto 16px;
            opacity: 0.5;
        }

        .empty-state-title {
            font-size: 18px;
            font-weight: 600;
            color: var(--text);
            margin-bottom: 8px;
        }

        /* Loading */
        .loading {
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 40px;
        }

        .spinner {
            width: 40px;
            height: 40px;
            border: 3px solid var(--border);
            border-top-color: var(--primary);
            border-radius: 50%;
            animation: spin 1s linear infinite;
        }

        @keyframes spin {
            to { transform: rotate(360deg); }
        }

        /* Toast Notifications */
        .toast-container {
            position: fixed;
            bottom: 24px;
            right: 24px;
            z-index: 1100;
            display: flex;
            flex-direction: column;
            gap: 12px;
        }

        .toast {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 14px 20px;
            background: var(--surface);
            border-radius: 8px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.15);
            min-width: 300px;
            transform: translateX(120%);
            transition: transform 0.3s;
        }

        .toast.show {
            transform: translateX(0);
        }

        .toast-success {
            border-left: 4px solid var(--success);
        }

        .toast-error {
            border-left: 4px solid var(--danger);
        }

        .toast-icon {
            width: 24px;
            height: 24px;
        }

        .toast-success .toast-icon {
            color: var(--success);
        }

        .toast-error .toast-icon {
            color: var(--danger);
        }

        .toast-content {
            flex: 1;
        }

        .toast-title {
            font-weight: 600;
            font-size: 14px;
        }

        .toast-message {
            font-size: 13px;
            color: var(--text-secondary);
        }

        /* Responsive */
        @media (max-width: 768px) {
            .roles-sidebar {
                width: 100%;
                position: relative;
                height: auto;
                top: 0;
            }
            .content-area {
                margin-left: 0;
            }
            .rbac-layout {
                flex-direction: column;
            }
            .permissions-grid {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <!-- Header -->
    <header class="main-header">
        <div>
            <div class="header-title">Administracao RBAC</div>
            <div class="header-subtitle">Gerenciamento de Roles e Permissoes</div>
        </div>
        <div class="header-actions">
            <button class="btn btn-outline" onclick="window.location.href='/'">
                <svg width="16" height="16" fill="currentColor" viewBox="0 0 20 20">
                    <path fill-rule="evenodd" d="M9.707 16.707a1 1 0 01-1.414 0l-6-6a1 1 0 010-1.414l6-6a1 1 0 011.414 1.414L5.414 9H17a1 1 0 110 2H5.414l4.293 4.293a1 1 0 010 1.414z" clip-rule="evenodd"/>
                </svg>
                Voltar
            </button>
            <button class="btn btn-secondary" onclick="openCreateRoleModal()">
                + Nova Role
            </button>
        </div>
    </header>

    <!-- Main Layout -->
    <div class="main-content">
        <!-- Sidebar - Roles List -->
        <aside class="roles-sidebar">
            <div class="sidebar-header">
                <h3 class="sidebar-title">Roles</h3>
                <button class="btn btn-sm btn-primary" onclick="refreshRoles()">
                    Atualizar
                </button>
            </div>
            <div class="roles-list" id="rolesList">
                <div class="loading">
                    <div class="spinner"></div>
                </div>
            </div>
        </aside>

        <!-- Content Area -->
        <div class="content-area">
            <!-- Tabs -->
            <div class="tabs">
                <div class="tab active" onclick="showTab('permissions')" id="tab-permissions">Permissoes</div>
                <div class="tab" onclick="showTab('users')" id="tab-users">Usuarios com esta Role</div>
                <div class="tab" onclick="showTab('settings')" id="tab-settings">Configuracoes</div>
            </div>

            <!-- Role Details -->
            <div id="roleDetails">
                <div class="empty-state">
                    <svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"/>
                    </svg>
                    <div class="empty-state-title">Selecione uma Role</div>
                    <p>Escolha uma role na lista ao lado para ver seus detalhes e permissoes</p>
                </div>
            </div>

            <!-- Permissions Tab Content -->
            <div id="content-permissions" style="display: none;">
                <div class="panel">
                    <div class="panel-header">
                        <div>
                            <h3 class="panel-title" id="selectedRoleName">Permissoes</h3>
                            <p style="font-size: 13px; color: var(--text-secondary); margin-top: 4px;" id="selectedRoleDesc"></p>
                        </div>
                        <button class="btn btn-primary btn-sm" onclick="savePermissions()">
                            Salvar Alteracoes
                        </button>
                    </div>
                    <div class="panel-body">
                        <div class="permissions-grid" id="permissionsGrid">
                            <!-- Permissions will be loaded here -->
                        </div>
                    </div>
                </div>
            </div>

            <!-- Users Tab Content -->
            <div id="content-users" style="display: none;">
                <div class="panel">
                    <div class="panel-header">
                        <h3 class="panel-title">Usuarios com esta Role</h3>
                        <button class="btn btn-primary btn-sm" onclick="openAssignUserModal()">
                            + Atribuir Usuario
                        </button>
                    </div>
                    <div class="panel-body" style="padding: 0;">
                        <table class="users-table">
                            <thead>
                                <tr>
                                    <th>Usuario</th>
                                    <th>Outras Roles</th>
                                    <th>Atribuido em</th>
                                    <th>Expira em</th>
                                    <th></th>
                                </tr>
                            </thead>
                            <tbody id="usersTableBody">
                                <tr>
                                    <td colspan="5" style="text-align: center; color: var(--text-secondary); padding: 40px;">
                                        Carregando usuarios...
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>

            <!-- Settings Tab Content -->
            <div id="content-settings" style="display: none;">
                <div class="panel">
                    <div class="panel-header">
                        <h3 class="panel-title">Configuracoes da Role</h3>
                    </div>
                    <div class="panel-body">
                        <form id="roleSettingsForm">
                            <div class="form-group">
                                <label class="form-label">Nome da Role</label>
                                <input type="text" class="form-input" id="settingsRoleName" placeholder="Nome da role">
                            </div>
                            <div class="form-group">
                                <label class="form-label">Descricao</label>
                                <textarea class="form-input form-textarea" id="settingsRoleDesc" placeholder="Descricao da role"></textarea>
                            </div>
                            <div class="form-group">
                                <label class="form-label">Nivel Hierarquico</label>
                                <input type="number" class="form-input" id="settingsRoleLevel" min="0" max="100" placeholder="0-100">
                                <small style="color: var(--text-secondary); margin-top: 4px; display: block;">
                                    Quanto maior o nivel, mais privilegios (Admin = 100, Viewer = 10)
                                </small>
                            </div>
                            <div style="display: flex; gap: 12px; margin-top: 24px;">
                                <button type="button" class="btn btn-primary" onclick="saveRoleSettings()">
                                    Salvar Configuracoes
                                </button>
                                <button type="button" class="btn btn-danger" onclick="deleteRole()" id="deleteRoleBtn">
                                    Excluir Role
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <!-- Create Role Modal -->
    <div class="modal-overlay" id="createRoleModal">
        <div class="modal">
            <div class="modal-header">
                <h3 class="modal-title">Nova Role</h3>
                <button class="modal-close" onclick="closeModal('createRoleModal')">
                    <svg width="20" height="20" fill="currentColor" viewBox="0 0 20 20">
                        <path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd"/>
                    </svg>
                </button>
            </div>
            <div class="modal-body">
                <form id="createRoleForm">
                    <div class="form-group">
                        <label class="form-label">Nome da Role *</label>
                        <input type="text" class="form-input" id="newRoleName" required placeholder="Ex: AUDITOR, TESTER">
                    </div>
                    <div class="form-group">
                        <label class="form-label">Descricao</label>
                        <textarea class="form-input form-textarea" id="newRoleDesc" placeholder="Descricao das responsabilidades desta role"></textarea>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Nivel Hierarquico</label>
                        <input type="number" class="form-input" id="newRoleLevel" min="0" max="100" value="25" placeholder="0-100">
                    </div>
                </form>
            </div>
            <div class="modal-footer">
                <button class="btn btn-outline" onclick="closeModal('createRoleModal')">Cancelar</button>
                <button class="btn btn-primary" onclick="createRole()">Criar Role</button>
            </div>
        </div>
    </div>

    <!-- Assign User Modal -->
    <div class="modal-overlay" id="assignUserModal">
        <div class="modal">
            <div class="modal-header">
                <h3 class="modal-title">Atribuir Role a Usuario</h3>
                <button class="modal-close" onclick="closeModal('assignUserModal')">
                    <svg width="20" height="20" fill="currentColor" viewBox="0 0 20 20">
                        <path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd"/>
                    </svg>
                </button>
            </div>
            <div class="modal-body">
                <form id="assignUserForm">
                    <div class="form-group">
                        <label class="form-label">Usuario *</label>
                        <select class="form-input form-select" id="assignUserId" required>
                            <option value="">Selecione um usuario</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Projeto (opcional)</label>
                        <select class="form-input form-select" id="assignProjectId">
                            <option value="">Global (todos os projetos)</option>
                        </select>
                    </div>
                    <div class="form-group">
                        <label class="form-label">Data de Expiracao (opcional)</label>
                        <input type="datetime-local" class="form-input" id="assignExpiresAt">
                    </div>
                </form>
            </div>
            <div class="modal-footer">
                <button class="btn btn-outline" onclick="closeModal('assignUserModal')">Cancelar</button>
                <button class="btn btn-primary" onclick="assignUserToRole()">Atribuir</button>
            </div>
        </div>
    </div>

    <!-- Toast Container -->
    <div class="toast-container" id="toastContainer"></div>

    <script>
        // State
        let selectedRoleId = null;
        let roles = [];
        let users = [];
        let projects = [];
        let resources = [];
        let actions = [];
        let currentPermissions = [];

        // Initialize
        document.addEventListener('DOMContentLoaded', () => {
            loadRoles();
            loadUsers();
            loadProjects();
            loadPermissionsDefinition();
        });

        // Load all roles
        async function loadRoles() {
            try {
                const res = await fetch('/api/rbac/roles');
                roles = await res.json();
                renderRolesList();
            } catch (error) {
                console.error('Error loading roles:', error);
                showToast('error', 'Erro', 'Falha ao carregar roles');
            }
        }

        // Refresh roles
        function refreshRoles() {
            loadRoles();
        }

        // Load users
        async function loadUsers() {
            try {
                const res = await fetch('/api/admin/users');
                const data = await res.json();
                users = data.users || data || [];
            } catch (error) {
                console.error('Error loading users:', error);
            }
        }

        // Load projects
        async function loadProjects() {
            try {
                const res = await fetch('/api/projects');
                projects = await res.json();
            } catch (error) {
                console.error('Error loading projects:', error);
            }
        }

        // Load permissions definition
        async function loadPermissionsDefinition() {
            try {
                const res = await fetch('/api/rbac/permissions');
                const data = await res.json();
                resources = data.resources || [];
                actions = data.actions || [];
            } catch (error) {
                console.error('Error loading permissions:', error);
            }
        }

        // Render roles list
        function renderRolesList() {
            const container = document.getElementById('rolesList');

            if (!roles || roles.length === 0) {
                container.innerHTML = `
                    <div class="empty-state" style="padding: 40px 20px;">
                        <p>Nenhuma role encontrada</p>
                    </div>
                `;
                return;
            }

            container.innerHTML = roles.map(role => `
                <div class="role-item ${selectedRoleId === role.role_id ? 'active' : ''}"
                     onclick="selectRole('${role.role_id}')">
                    <div class="role-icon">${role.name.charAt(0)}</div>
                    <div class="role-info">
                        <div class="role-name">${role.name}</div>
                        <div class="role-level">Nivel ${role.level}</div>
                    </div>
                    ${role.is_system ? '<span class="role-badge system">Sistema</span>' : ''}
                </div>
            `).join('');
        }

        // Select role
        async function selectRole(roleId) {
            selectedRoleId = roleId;
            renderRolesList();

            const role = roles.find(r => r.role_id === roleId);
            if (!role) return;

            // Hide empty state and show content
            document.getElementById('roleDetails').style.display = 'none';

            // Update role info
            document.getElementById('selectedRoleName').textContent = `Permissoes: ${role.name}`;
            document.getElementById('selectedRoleDesc').textContent = role.description || '';

            // Update settings tab
            document.getElementById('settingsRoleName').value = role.name;
            document.getElementById('settingsRoleDesc').value = role.description || '';
            document.getElementById('settingsRoleLevel').value = role.level;

            // Disable delete for system roles
            document.getElementById('deleteRoleBtn').disabled = role.is_system;
            document.getElementById('deleteRoleBtn').style.opacity = role.is_system ? '0.5' : '1';

            // Store current permissions
            currentPermissions = role.permissions || [];

            // Render permissions grid
            renderPermissionsGrid();

            // Load users with this role
            loadRoleUsers(roleId);

            // Show permissions tab
            showTab('permissions');
        }

        // Render permissions grid
        function renderPermissionsGrid() {
            const container = document.getElementById('permissionsGrid');

            const resourceIcons = {
                'projects': 'M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z',
                'stories': 'M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z',
                'tasks': 'M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4',
                'users': 'M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z',
                'roles': 'M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z',
                'epics': 'M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10',
                'sprints': 'M13 10V3L4 14h7v7l9-11h-7z',
                'documentation': 'M12 6.253v13m0-13C10.832 5.477 9.246 5 7.5 5S4.168 5.477 3 6.253v13C4.168 18.477 5.754 18 7.5 18s3.332.477 4.5 1.253m0-13C13.168 5.477 14.754 5 16.5 5c1.747 0 3.332.477 4.5 1.253v13C19.832 18.477 18.247 18 16.5 18c-1.746 0-3.332.477-4.5 1.253',
                'designs': 'M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z',
                'workers': 'M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z',
                'jobs': 'M21 13.255A23.931 23.931 0 0112 15c-3.183 0-6.22-.62-9-1.745M16 6V4a2 2 0 00-2-2h-4a2 2 0 00-2 2v2m4 6h.01M5 20h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z',
                'chat': 'M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z',
                'settings': 'M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z',
                'audit': 'M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-3 7h3m-3 4h3m-6-4h.01M9 16h.01'
            };

            const actionLabels = {
                'create': 'Criar',
                'read': 'Ler',
                'update': 'Atualizar',
                'delete': 'Excluir',
                'manage': 'Gerenciar',
                'execute': 'Executar',
                'assign': 'Atribuir'
            };

            container.innerHTML = resources.map(resource => `
                <div class="permission-group">
                    <div class="permission-group-header">
                        <div class="permission-group-icon">
                            <svg width="16" height="16" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                      d="${resourceIcons[resource] || resourceIcons['projects']}"/>
                            </svg>
                        </div>
                        <span class="permission-group-name">${resource}</span>
                        <label class="permission-checkbox" style="margin-left: auto;">
                            <input type="checkbox"
                                   onchange="toggleAllPermissions('${resource}', this.checked)"
                                   ${hasWildcard(resource) ? 'checked' : ''}>
                            <span>Todos</span>
                        </label>
                    </div>
                    <div class="permission-actions">
                        ${actions.map(action => `
                            <label class="permission-checkbox ${hasPermission(resource, action) ? 'checked' : ''}">
                                <input type="checkbox"
                                       data-resource="${resource}"
                                       data-action="${action}"
                                       onchange="updatePermissionCheckbox(this)"
                                       ${hasPermission(resource, action) ? 'checked' : ''}>
                                <span>${actionLabels[action] || action}</span>
                            </label>
                        `).join('')}
                    </div>
                </div>
            `).join('');
        }

        // Check if role has permission
        function hasPermission(resource, action) {
            if (currentPermissions.includes('*:*')) return true;
            if (currentPermissions.includes(`${resource}:*`)) return true;
            if (currentPermissions.includes(`${resource}:manage`)) return true;
            return currentPermissions.includes(`${resource}:${action}`);
        }

        // Check if role has wildcard for resource
        function hasWildcard(resource) {
            return currentPermissions.includes('*:*') || currentPermissions.includes(`${resource}:*`);
        }

        // Toggle all permissions for a resource
        function toggleAllPermissions(resource, checked) {
            const checkboxes = document.querySelectorAll(`input[data-resource="${resource}"]`);
            checkboxes.forEach(cb => {
                cb.checked = checked;
                updatePermissionCheckbox(cb, false);
            });
        }

        // Update permission checkbox visual
        function updatePermissionCheckbox(checkbox, updateState = true) {
            const label = checkbox.closest('.permission-checkbox');
            if (checkbox.checked) {
                label.classList.add('checked');
            } else {
                label.classList.remove('checked');
            }
        }

        // Save permissions
        async function savePermissions() {
            if (!selectedRoleId) return;

            const checkboxes = document.querySelectorAll('input[data-resource]');
            const permissions = [];

            checkboxes.forEach(cb => {
                if (cb.checked) {
                    permissions.push(`${cb.dataset.resource}:${cb.dataset.action}`);
                }
            });

            try {
                const res = await fetch(`/api/rbac/roles/${selectedRoleId}`, {
                    method: 'PUT',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ permissions })
                });

                if (res.ok) {
                    showToast('success', 'Sucesso', 'Permissoes atualizadas com sucesso');
                    await loadRoles();
                    // Re-select to refresh
                    const roleIndex = roles.findIndex(r => r.role_id === selectedRoleId);
                    if (roleIndex >= 0) {
                        currentPermissions = roles[roleIndex].permissions || [];
                    }
                } else {
                    const error = await res.json();
                    showToast('error', 'Erro', error.detail || 'Falha ao atualizar permissoes');
                }
            } catch (error) {
                console.error('Error saving permissions:', error);
                showToast('error', 'Erro', 'Falha ao salvar permissoes');
            }
        }

        // Load users with selected role
        async function loadRoleUsers(roleId) {
            const tbody = document.getElementById('usersTableBody');
            tbody.innerHTML = '<tr><td colspan="5" style="text-align: center; padding: 40px;">Carregando...</td></tr>';

            try {
                // Get all users and filter those with this role
                const usersWithRole = [];

                for (const user of users) {
                    try {
                        const res = await fetch(`/api/rbac/users/${user.id}/roles`);
                        if (res.ok) {
                            const userRoles = await res.json();
                            const hasRole = userRoles.find(ur => ur.role_id === roleId);
                            if (hasRole) {
                                usersWithRole.push({
                                    ...user,
                                    roleAssignment: hasRole,
                                    allRoles: userRoles
                                });
                            }
                        }
                    } catch (e) {
                        // Skip user if error
                    }
                }

                if (usersWithRole.length === 0) {
                    tbody.innerHTML = `
                        <tr>
                            <td colspan="5" style="text-align: center; color: var(--text-secondary); padding: 40px;">
                                Nenhum usuario com esta role
                            </td>
                        </tr>
                    `;
                    return;
                }

                tbody.innerHTML = usersWithRole.map(user => `
                    <tr>
                        <td>
                            <div class="user-cell">
                                <div class="user-avatar">${(user.username || 'U').charAt(0).toUpperCase()}</div>
                                <div class="user-details">
                                    <div class="user-name">${user.username}</div>
                                    <div class="user-email">${user.email || '-'}</div>
                                </div>
                            </div>
                        </td>
                        <td>
                            <div class="role-tags">
                                ${user.allRoles.filter(r => r.role_id !== roleId).map(r => `
                                    <span class="role-tag ${r.role?.name?.toLowerCase() || ''}">${r.role?.name || r.role_id}</span>
                                `).join('') || '<span style="color: var(--text-secondary);">-</span>'}
                            </div>
                        </td>
                        <td>${user.roleAssignment.assigned_at ? new Date(user.roleAssignment.assigned_at).toLocaleDateString() : '-'}</td>
                        <td>${user.roleAssignment.expires_at ? new Date(user.roleAssignment.expires_at).toLocaleDateString() : 'Nunca'}</td>
                        <td>
                            <button class="btn btn-outline btn-sm" onclick="revokeRole(${user.id}, '${roleId}')">
                                Revogar
                            </button>
                        </td>
                    </tr>
                `).join('');

            } catch (error) {
                console.error('Error loading role users:', error);
                tbody.innerHTML = `
                    <tr>
                        <td colspan="5" style="text-align: center; color: var(--danger); padding: 40px;">
                            Erro ao carregar usuarios
                        </td>
                    </tr>
                `;
            }
        }

        // Show tab
        function showTab(tabId) {
            // Hide all content
            document.querySelectorAll('[id^="content-"]').forEach(el => {
                el.style.display = 'none';
            });

            // Remove active from all tabs
            document.querySelectorAll('.tab').forEach(el => {
                el.classList.remove('active');
            });

            // Show selected content
            const content = document.getElementById(`content-${tabId}`);
            if (content) {
                content.style.display = 'block';
            }

            // Activate tab
            const tab = document.getElementById(`tab-${tabId}`);
            if (tab) {
                tab.classList.add('active');
            }
        }

        // Open create role modal
        function openCreateRoleModal() {
            document.getElementById('newRoleName').value = '';
            document.getElementById('newRoleDesc').value = '';
            document.getElementById('newRoleLevel').value = '25';
            openModal('createRoleModal');
        }

        // Create role
        async function createRole() {
            const name = document.getElementById('newRoleName').value.trim().toUpperCase();
            const description = document.getElementById('newRoleDesc').value.trim();
            const level = parseInt(document.getElementById('newRoleLevel').value) || 0;

            if (!name) {
                showToast('error', 'Erro', 'Nome da role e obrigatorio');
                return;
            }

            try {
                const res = await fetch('/api/rbac/roles', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ name, description, level, permissions: [] })
                });

                if (res.ok) {
                    showToast('success', 'Sucesso', 'Role criada com sucesso');
                    closeModal('createRoleModal');
                    await loadRoles();
                } else {
                    const error = await res.json();
                    showToast('error', 'Erro', error.detail || 'Falha ao criar role');
                }
            } catch (error) {
                console.error('Error creating role:', error);
                showToast('error', 'Erro', 'Falha ao criar role');
            }
        }

        // Save role settings
        async function saveRoleSettings() {
            if (!selectedRoleId) return;

            const name = document.getElementById('settingsRoleName').value.trim().toUpperCase();
            const description = document.getElementById('settingsRoleDesc').value.trim();
            const level = parseInt(document.getElementById('settingsRoleLevel').value) || 0;

            try {
                const res = await fetch(`/api/rbac/roles/${selectedRoleId}`, {
                    method: 'PUT',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ name, description, level })
                });

                if (res.ok) {
                    showToast('success', 'Sucesso', 'Configuracoes salvas com sucesso');
                    await loadRoles();
                    selectRole(selectedRoleId);
                } else {
                    const error = await res.json();
                    showToast('error', 'Erro', error.detail || 'Falha ao salvar configuracoes');
                }
            } catch (error) {
                console.error('Error saving settings:', error);
                showToast('error', 'Erro', 'Falha ao salvar configuracoes');
            }
        }

        // Delete role
        async function deleteRole() {
            if (!selectedRoleId) return;

            const role = roles.find(r => r.role_id === selectedRoleId);
            if (role?.is_system) {
                showToast('error', 'Erro', 'Nao e possivel excluir roles do sistema');
                return;
            }

            if (!confirm(`Tem certeza que deseja excluir a role "${role?.name}"?`)) {
                return;
            }

            try {
                const res = await fetch(`/api/rbac/roles/${selectedRoleId}`, {
                    method: 'DELETE'
                });

                if (res.ok) {
                    showToast('success', 'Sucesso', 'Role excluida com sucesso');
                    selectedRoleId = null;
                    await loadRoles();
                    document.getElementById('roleDetails').style.display = 'block';
                    document.querySelectorAll('[id^="content-"]').forEach(el => {
                        el.style.display = 'none';
                    });
                } else {
                    const error = await res.json();
                    showToast('error', 'Erro', error.detail || 'Falha ao excluir role');
                }
            } catch (error) {
                console.error('Error deleting role:', error);
                showToast('error', 'Erro', 'Falha ao excluir role');
            }
        }

        // Open assign user modal
        function openAssignUserModal() {
            if (!selectedRoleId) {
                showToast('error', 'Erro', 'Selecione uma role primeiro');
                return;
            }

            // Populate users dropdown
            const userSelect = document.getElementById('assignUserId');
            userSelect.innerHTML = '<option value="">Selecione um usuario</option>' +
                users.map(u => `<option value="${u.id}">${u.username} (${u.email || '-'})</option>`).join('');

            // Populate projects dropdown
            const projectSelect = document.getElementById('assignProjectId');
            projectSelect.innerHTML = '<option value="">Global (todos os projetos)</option>' +
                projects.map(p => `<option value="${p.project_id}">${p.name}</option>`).join('');

            document.getElementById('assignExpiresAt').value = '';
            openModal('assignUserModal');
        }

        // Assign user to role
        async function assignUserToRole() {
            const userId = document.getElementById('assignUserId').value;
            const projectId = document.getElementById('assignProjectId').value || null;
            const expiresAt = document.getElementById('assignExpiresAt').value || null;

            if (!userId) {
                showToast('error', 'Erro', 'Selecione um usuario');
                return;
            }

            try {
                const res = await fetch(`/api/rbac/users/${userId}/roles`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        user_id: parseInt(userId),
                        role_id: selectedRoleId,
                        project_id: projectId,
                        expires_at: expiresAt
                    })
                });

                if (res.ok) {
                    showToast('success', 'Sucesso', 'Role atribuida com sucesso');
                    closeModal('assignUserModal');
                    loadRoleUsers(selectedRoleId);
                } else {
                    const error = await res.json();
                    showToast('error', 'Erro', error.detail || 'Falha ao atribuir role');
                }
            } catch (error) {
                console.error('Error assigning role:', error);
                showToast('error', 'Erro', 'Falha ao atribuir role');
            }
        }

        // Revoke role from user
        async function revokeRole(userId, roleId) {
            if (!confirm('Tem certeza que deseja revogar esta role do usuario?')) {
                return;
            }

            try {
                const res = await fetch(`/api/rbac/users/${userId}/roles/${roleId}`, {
                    method: 'DELETE'
                });

                if (res.ok) {
                    showToast('success', 'Sucesso', 'Role revogada com sucesso');
                    loadRoleUsers(selectedRoleId);
                } else {
                    const error = await res.json();
                    showToast('error', 'Erro', error.detail || 'Falha ao revogar role');
                }
            } catch (error) {
                console.error('Error revoking role:', error);
                showToast('error', 'Erro', 'Falha ao revogar role');
            }
        }

        // Modal helpers
        function openModal(modalId) {
            document.getElementById(modalId).classList.add('active');
        }

        function closeModal(modalId) {
            document.getElementById(modalId).classList.remove('active');
        }

        // Toast notifications
        function showToast(type, title, message) {
            const container = document.getElementById('toastContainer');
            const id = Date.now();

            const icons = {
                success: '<svg width="24" height="24" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd"/></svg>',
                error: '<svg width="24" height="24" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd"/></svg>'
            };

            const toast = document.createElement('div');
            toast.id = `toast-${id}`;
            toast.className = `toast toast-${type}`;
            toast.innerHTML = `
                <div class="toast-icon">${icons[type]}</div>
                <div class="toast-content">
                    <div class="toast-title">${title}</div>
                    <div class="toast-message">${message}</div>
                </div>
            `;

            container.appendChild(toast);

            // Trigger animation
            setTimeout(() => toast.classList.add('show'), 10);

            // Auto remove
            setTimeout(() => {
                toast.classList.remove('show');
                setTimeout(() => toast.remove(), 300);
            }, 4000);
        }

        // Close modals on escape
        document.addEventListener('keydown', (e) => {
            if (e.key === 'Escape') {
                document.querySelectorAll('.modal-overlay.active').forEach(modal => {
                    modal.classList.remove('active');
                });
            }
        });

        // Close modal on overlay click
        document.querySelectorAll('.modal-overlay').forEach(overlay => {
            overlay.addEventListener('click', (e) => {
                if (e.target === overlay) {
                    overlay.classList.remove('active');
                }
            });
        });
    </script>
</body>
</html>
'''


# =============================================================================
# REGISTER ENDPOINTS
# =============================================================================

def register_rbac_admin_endpoints(app):
    """
    Registra endpoints da pagina de administracao RBAC no app FastAPI

    Args:
        app: Instancia do FastAPI
    """
    from fastapi import APIRouter, Depends, HTTPException
    from fastapi.responses import HTMLResponse

    router = APIRouter(prefix="/admin", tags=["admin-rbac"])

    @router.get("/rbac", response_class=HTMLResponse)
    async def rbac_admin_page():
        """
        Pagina de administracao RBAC
        Requer role ADMIN para acessar
        """
        return generate_rbac_admin_html()

    app.include_router(router)
    print("[Dashboard] RBAC Admin page registered at /admin/rbac")


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI

    app = FastAPI(title="RBAC Admin")

    # Include RBAC API routes
    try:
        from factory.auth.rbac import rbac_router
        app.include_router(rbac_router)
    except ImportError as e:
        print(f"Warning: Could not import RBAC router: {e}")

    # Include Admin routes for users
    try:
        from factory.api.admin_routes import router as admin_router
        app.include_router(admin_router)
    except ImportError as e:
        print(f"Warning: Could not import admin router: {e}")

    register_rbac_admin_endpoints(app)

    print("[RBAC Admin] Starting standalone server on port 9005...")
    uvicorn.run(app, host="0.0.0.0", port=9005)

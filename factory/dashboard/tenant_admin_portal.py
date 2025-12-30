# -*- coding: utf-8 -*-
"""
Tenant Admin Portal Page (Issue #288)
======================================

HTML page for Tenant Administrators to manage their organization.

Author: Fabrica de Agentes - Terminal 4
"""

import logging
from fastapi import APIRouter, Request
from fastapi.responses import HTMLResponse

logger = logging.getLogger(__name__)


def register_tenant_admin_portal(app):
    """Register tenant admin portal pages"""

    @app.get("/tenant-admin", response_class=HTMLResponse)
    async def tenant_admin_page(request: Request):
        """Tenant Admin Portal main page"""
        return get_tenant_admin_html()

    @app.get("/tenant-admin/members", response_class=HTMLResponse)
    async def tenant_members_page(request: Request):
        """Members management page"""
        return get_tenant_admin_html()

    @app.get("/tenant-admin/settings", response_class=HTMLResponse)
    async def tenant_settings_page(request: Request):
        """Tenant settings page"""
        return get_tenant_admin_html()

    @app.get("/tenant-admin/integrations", response_class=HTMLResponse)
    async def tenant_integrations_page(request: Request):
        """Integrations page"""
        return get_tenant_admin_html()

    @app.get("/tenant-admin/billing", response_class=HTMLResponse)
    async def tenant_billing_page(request: Request):
        """Tenant billing page"""
        return get_tenant_admin_html()

    @app.get("/tenant-admin/audit-logs", response_class=HTMLResponse)
    async def tenant_audit_page(request: Request):
        """Audit logs page"""
        return get_tenant_admin_html()

    logger.info("[Tenant Admin Portal] Pages registered at /tenant-admin/*")


def get_tenant_admin_html():
    """Generate Tenant Admin Portal HTML"""
    return """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Admin Portal - Fabrica de Agentes</title>
    <style>
        :root {
            --primary: #003B4A;
            --primary-light: #004d5f;
            --accent: #FF6C00;
            --success: #10B981;
            --warning: #F59E0B;
            --danger: #EF4444;
            --gray-50: #F9FAFB;
            --gray-100: #F3F4F6;
            --gray-200: #E5E7EB;
            --gray-300: #D1D5DB;
            --gray-500: #6B7280;
            --gray-700: #374151;
            --gray-900: #111827;
        }

        * { box-sizing: border-box; margin: 0; padding: 0; }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: var(--gray-100);
            min-height: 100vh;
        }

        .header {
            background: var(--primary);
            color: white;
            padding: 1rem 2rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .header h1 {
            font-size: 1.25rem;
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .header .tenant-badge {
            background: var(--accent);
            padding: 0.25rem 0.75rem;
            border-radius: 6px;
            font-size: 0.75rem;
        }

        .back-link {
            color: white;
            text-decoration: none;
            opacity: 0.9;
        }

        .back-link:hover { opacity: 1; }

        .layout {
            display: flex;
            min-height: calc(100vh - 56px);
        }

        .sidebar {
            width: 240px;
            background: white;
            border-right: 1px solid var(--gray-200);
            padding: 1rem 0;
        }

        .nav-item {
            display: flex;
            align-items: center;
            gap: 0.75rem;
            padding: 0.75rem 1.25rem;
            color: var(--gray-700);
            text-decoration: none;
            font-size: 0.875rem;
            transition: all 0.2s;
        }

        .nav-item:hover {
            background: var(--gray-100);
            color: var(--primary);
        }

        .nav-item.active {
            background: var(--primary);
            color: white;
        }

        .nav-divider {
            height: 1px;
            background: var(--gray-200);
            margin: 0.75rem 0;
        }

        .nav-label {
            font-size: 0.7rem;
            text-transform: uppercase;
            color: var(--gray-500);
            padding: 0.5rem 1.25rem;
            font-weight: 600;
        }

        .main {
            flex: 1;
            padding: 1.5rem;
            overflow-y: auto;
        }

        .page-title {
            font-size: 1.25rem;
            color: var(--gray-900);
            margin-bottom: 0.25rem;
        }

        .page-subtitle {
            color: var(--gray-500);
            font-size: 0.875rem;
            margin-bottom: 1.5rem;
        }

        .stats-row {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
            gap: 1rem;
            margin-bottom: 1.5rem;
        }

        .stat-card {
            background: white;
            border-radius: 8px;
            padding: 1rem;
            box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }

        .stat-card .label {
            color: var(--gray-500);
            font-size: 0.75rem;
            margin-bottom: 0.25rem;
        }

        .stat-card .value {
            font-size: 1.5rem;
            font-weight: 600;
            color: var(--gray-900);
        }

        .section {
            background: white;
            border-radius: 8px;
            padding: 1.25rem;
            margin-bottom: 1rem;
            box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }

        .section-title {
            font-size: 1rem;
            font-weight: 600;
            margin-bottom: 1rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .table {
            width: 100%;
            border-collapse: collapse;
            font-size: 0.875rem;
        }

        .table th, .table td {
            padding: 0.75rem;
            text-align: left;
            border-bottom: 1px solid var(--gray-200);
        }

        .table th {
            color: var(--gray-500);
            font-weight: 500;
            font-size: 0.75rem;
            text-transform: uppercase;
        }

        .role-badge {
            padding: 0.2rem 0.5rem;
            border-radius: 4px;
            font-size: 0.7rem;
            font-weight: 500;
        }

        .role-badge.owner { background: #FEE2E2; color: #991B1B; }
        .role-badge.admin { background: #DBEAFE; color: #1E40AF; }
        .role-badge.member { background: #D1FAE5; color: #065F46; }
        .role-badge.viewer { background: var(--gray-200); color: var(--gray-700); }

        .status-dot {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            display: inline-block;
            margin-right: 0.5rem;
        }

        .status-dot.active { background: var(--success); }
        .status-dot.invited { background: var(--warning); }
        .status-dot.suspended { background: var(--danger); }

        .btn {
            padding: 0.5rem 0.875rem;
            border-radius: 6px;
            font-size: 0.8rem;
            font-weight: 500;
            cursor: pointer;
            border: none;
        }

        .btn-primary {
            background: var(--primary);
            color: white;
        }

        .btn-outline {
            background: transparent;
            border: 1px solid var(--gray-300);
            color: var(--gray-700);
        }

        .btn-danger {
            background: var(--danger);
            color: white;
        }

        .form-group {
            margin-bottom: 1rem;
        }

        .form-group label {
            display: block;
            margin-bottom: 0.25rem;
            font-weight: 500;
            font-size: 0.875rem;
        }

        .form-group input, .form-group select {
            width: 100%;
            padding: 0.5rem;
            border: 1px solid var(--gray-300);
            border-radius: 6px;
            font-size: 0.875rem;
        }

        .integration-card {
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 1rem;
            border: 1px solid var(--gray-200);
            border-radius: 8px;
            margin-bottom: 0.75rem;
        }

        .integration-card .info {
            display: flex;
            align-items: center;
            gap: 0.75rem;
        }

        .integration-card .icon {
            width: 40px;
            height: 40px;
            background: var(--gray-100);
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .toggle {
            position: relative;
            width: 40px;
            height: 22px;
        }

        .toggle input {
            opacity: 0;
            width: 0;
            height: 0;
        }

        .toggle-slider {
            position: absolute;
            cursor: pointer;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background-color: var(--gray-300);
            transition: 0.3s;
            border-radius: 22px;
        }

        .toggle-slider:before {
            position: absolute;
            content: "";
            height: 16px;
            width: 16px;
            left: 3px;
            bottom: 3px;
            background-color: white;
            transition: 0.3s;
            border-radius: 50%;
        }

        .toggle input:checked + .toggle-slider {
            background-color: var(--success);
        }

        .toggle input:checked + .toggle-slider:before {
            transform: translateX(18px);
        }

        .modal {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 1000;
        }

        .modal-content {
            background: white;
            border-radius: 12px;
            padding: 1.5rem;
            width: 100%;
            max-width: 400px;
        }

        .modal-title {
            font-size: 1.125rem;
            font-weight: 600;
            margin-bottom: 1rem;
        }

        .modal-actions {
            display: flex;
            justify-content: flex-end;
            gap: 0.5rem;
            margin-top: 1rem;
        }
    </style>
</head>
<body>
    <div id="app">
        <header class="header">
            <h1>
                <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"/>
                </svg>
                Admin Portal
                <span class="tenant-badge">{{ tenant.name || 'Minha Organizacao' }}</span>
            </h1>
            <a href="/" class="back-link">Voltar ao Dashboard</a>
        </header>

        <div class="layout">
            <nav class="sidebar">
                <div class="nav-label">Geral</div>
                <a href="#" class="nav-item" :class="{ active: tab === 'dashboard' }" @click.prevent="tab = 'dashboard'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <rect x="3" y="3" width="7" height="9"/><rect x="14" y="3" width="7" height="5"/><rect x="14" y="12" width="7" height="9"/><rect x="3" y="16" width="7" height="5"/>
                    </svg>
                    Dashboard
                </a>

                <div class="nav-divider"></div>
                <div class="nav-label">Equipe</div>

                <a href="#" class="nav-item" :class="{ active: tab === 'members' }" @click.prevent="tab = 'members'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M23 21v-2a4 4 0 0 0-3-3.87"/><path d="M16 3.13a4 4 0 0 1 0 7.75"/>
                    </svg>
                    Membros
                </a>

                <a href="#" class="nav-item" :class="{ active: tab === 'invites' }" @click.prevent="tab = 'invites'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z"/><polyline points="22,6 12,13 2,6"/>
                    </svg>
                    Convites
                </a>

                <div class="nav-divider"></div>
                <div class="nav-label">Configuracoes</div>

                <a href="#" class="nav-item" :class="{ active: tab === 'settings' }" @click.prevent="tab = 'settings'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z"/>
                    </svg>
                    Configuracoes
                </a>

                <a href="#" class="nav-item" :class="{ active: tab === 'integrations' }" @click.prevent="tab = 'integrations'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M12 2v4m0 12v4M4.93 4.93l2.83 2.83m8.48 8.48l2.83 2.83M2 12h4m12 0h4M4.93 19.07l2.83-2.83m8.48-8.48l2.83-2.83"/>
                    </svg>
                    Integracoes
                </a>

                <a href="#" class="nav-item" :class="{ active: tab === 'billing' }" @click.prevent="tab = 'billing'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <rect x="1" y="4" width="22" height="16" rx="2" ry="2"/><line x1="1" y1="10" x2="23" y2="10"/>
                    </svg>
                    Billing
                </a>

                <a href="#" class="nav-item" :class="{ active: tab === 'audit' }" @click.prevent="tab = 'audit'">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"/>
                    </svg>
                    Audit Logs
                </a>
            </nav>

            <main class="main">
                <!-- Dashboard -->
                <div v-if="tab === 'dashboard'">
                    <h2 class="page-title">Dashboard do Tenant</h2>
                    <p class="page-subtitle">Visao geral da sua organizacao</p>

                    <div class="stats-row">
                        <div class="stat-card">
                            <div class="label">Membros</div>
                            <div class="value">{{ dashboard.metrics?.members || 0 }}</div>
                        </div>
                        <div class="stat-card">
                            <div class="label">Projetos</div>
                            <div class="value">{{ dashboard.metrics?.projects || 0 }}</div>
                        </div>
                        <div class="stat-card">
                            <div class="label">Stories</div>
                            <div class="value">{{ dashboard.metrics?.stories?.total || 0 }}</div>
                        </div>
                        <div class="stat-card">
                            <div class="label">Conclusao</div>
                            <div class="value">{{ dashboard.metrics?.stories?.completion_rate || 0 }}%</div>
                        </div>
                    </div>

                    <div class="section">
                        <div class="section-title">Plano Atual</div>
                        <p><strong>{{ tenant.plan || 'Professional' }}</strong></p>
                        <p style="color: var(--gray-500); font-size: 0.875rem;">Status: {{ tenant.status || 'Ativo' }}</p>
                    </div>
                </div>

                <!-- Members -->
                <div v-if="tab === 'members'">
                    <h2 class="page-title">Membros da Equipe</h2>
                    <p class="page-subtitle">Gerenciar membros da organizacao</p>

                    <div class="section">
                        <div class="section-title">
                            <span>Membros ({{ members.length }})</span>
                            <button class="btn btn-primary" @click="showInviteModal = true">+ Convidar</button>
                        </div>

                        <table class="table">
                            <thead>
                                <tr>
                                    <th>Usuario</th>
                                    <th>Email</th>
                                    <th>Role</th>
                                    <th>Status</th>
                                    <th>Acoes</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr v-for="member in members" :key="member.id">
                                    <td>{{ member.user?.username || 'Usuario' }}</td>
                                    <td>{{ member.user?.email || '-' }}</td>
                                    <td><span class="role-badge" :class="member.role">{{ member.role }}</span></td>
                                    <td>
                                        <span class="status-dot" :class="member.status"></span>
                                        {{ member.status }}
                                    </td>
                                    <td>
                                        <button class="btn btn-outline" @click="editMember(member)">Editar</button>
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>

                <!-- Invites -->
                <div v-if="tab === 'invites'">
                    <h2 class="page-title">Convites Pendentes</h2>
                    <p class="page-subtitle">Gerenciar convites enviados</p>

                    <div class="section">
                        <div class="section-title">
                            <span>Convites</span>
                            <button class="btn btn-primary" @click="showInviteModal = true">+ Novo Convite</button>
                        </div>

                        <div v-if="invites.length === 0" style="text-align: center; padding: 2rem; color: var(--gray-500);">
                            Nenhum convite pendente
                        </div>

                        <table v-else class="table">
                            <thead>
                                <tr>
                                    <th>Email</th>
                                    <th>Role</th>
                                    <th>Enviado por</th>
                                    <th>Expira em</th>
                                    <th>Acoes</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr v-for="invite in invites" :key="invite.id">
                                    <td>{{ invite.email }}</td>
                                    <td><span class="role-badge" :class="invite.role">{{ invite.role }}</span></td>
                                    <td>{{ invite.invited_by }}</td>
                                    <td>{{ invite.expires_at }}</td>
                                    <td>
                                        <button class="btn btn-danger" @click="revokeInvite(invite.id)">Revogar</button>
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>

                <!-- Settings -->
                <div v-if="tab === 'settings'">
                    <h2 class="page-title">Configuracoes</h2>
                    <p class="page-subtitle">Configuracoes da organizacao</p>

                    <div class="section">
                        <div class="section-title">Informacoes Gerais</div>
                        <div class="form-group">
                            <label>Nome da Organizacao</label>
                            <input type="text" v-model="settings.tenant.name">
                        </div>
                        <div class="form-group">
                            <label>Timezone</label>
                            <select v-model="settings.general.timezone">
                                <option value="UTC">UTC</option>
                                <option value="America/Sao_Paulo">America/Sao_Paulo</option>
                                <option value="America/New_York">America/New_York</option>
                            </select>
                        </div>
                        <button class="btn btn-primary">Salvar</button>
                    </div>
                </div>

                <!-- Integrations -->
                <div v-if="tab === 'integrations'">
                    <h2 class="page-title">Integracoes</h2>
                    <p class="page-subtitle">Conectar servicos externos</p>

                    <div class="section">
                        <div v-for="integration in integrations" :key="integration.type" class="integration-card">
                            <div class="info">
                                <div class="icon">
                                    <svg v-if="integration.type === 'github'" width="24" height="24" viewBox="0 0 24 24" fill="currentColor">
                                        <path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/>
                                    </svg>
                                    <span v-else>{{ integration.name[0] }}</span>
                                </div>
                                <div>
                                    <strong>{{ integration.name }}</strong>
                                    <p style="color: var(--gray-500); font-size: 0.75rem;">{{ integration.description }}</p>
                                </div>
                            </div>
                            <label class="toggle">
                                <input type="checkbox" :checked="integration.enabled">
                                <span class="toggle-slider"></span>
                            </label>
                        </div>
                    </div>
                </div>

                <!-- Billing -->
                <div v-if="tab === 'billing'">
                    <h2 class="page-title">Billing</h2>
                    <p class="page-subtitle">Informacoes de pagamento</p>

                    <div class="section">
                        <div class="section-title">Plano Atual</div>
                        <p><strong>{{ billing.plan?.name || 'Professional' }}</strong> - ${{ billing.plan?.price || 49 }}/mes</p>
                        <button class="btn btn-outline" style="margin-top: 1rem;">Upgrade</button>
                    </div>
                </div>

                <!-- Audit -->
                <div v-if="tab === 'audit'">
                    <h2 class="page-title">Audit Logs</h2>
                    <p class="page-subtitle">Historico de atividades</p>

                    <div class="section">
                        <div v-if="auditLogs.length === 0" style="text-align: center; padding: 2rem; color: var(--gray-500);">
                            Nenhum log disponivel
                        </div>
                        <div v-else v-for="log in auditLogs" :key="log.id" style="padding: 0.75rem 0; border-bottom: 1px solid var(--gray-200);">
                            <div style="display: flex; justify-content: space-between;">
                                <span><strong>{{ log.action }}</strong> - {{ log.entity_type }}</span>
                                <span style="color: var(--gray-500); font-size: 0.75rem;">{{ log.created_at }}</span>
                            </div>
                        </div>
                    </div>
                </div>
            </main>
        </div>

        <!-- Invite Modal -->
        <div v-if="showInviteModal" class="modal" @click.self="showInviteModal = false">
            <div class="modal-content">
                <div class="modal-title">Convidar Membro</div>
                <div class="form-group">
                    <label>Email</label>
                    <input type="email" v-model="inviteForm.email" placeholder="email@exemplo.com">
                </div>
                <div class="form-group">
                    <label>Role</label>
                    <select v-model="inviteForm.role">
                        <option value="admin">Admin</option>
                        <option value="member">Member</option>
                        <option value="viewer">Viewer</option>
                    </select>
                </div>
                <div class="modal-actions">
                    <button class="btn btn-outline" @click="showInviteModal = false">Cancelar</button>
                    <button class="btn btn-primary" @click="sendInvite">Enviar Convite</button>
                </div>
            </div>
        </div>
    </div>

    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script>
        const { createApp, ref, onMounted } = Vue;

        createApp({
            setup() {
                const tab = ref('dashboard');
                const tenant = ref({});
                const dashboard = ref({});
                const members = ref([]);
                const invites = ref([]);
                const settings = ref({ tenant: {}, general: {} });
                const integrations = ref([]);
                const billing = ref({});
                const auditLogs = ref([]);
                const showInviteModal = ref(false);
                const inviteForm = ref({ email: '', role: 'member' });

                async function loadDashboard() {
                    try {
                        const res = await fetch('/api/tenant-admin/dashboard');
                        const data = await res.json();
                        if (data.success) {
                            dashboard.value = data.dashboard;
                            tenant.value = data.dashboard.tenant || {};
                        }
                    } catch (e) { console.error(e); }
                }

                async function loadMembers() {
                    try {
                        const res = await fetch('/api/tenant-admin/members');
                        const data = await res.json();
                        if (data.success) members.value = data.members;
                    } catch (e) { console.error(e); }
                }

                async function loadInvites() {
                    try {
                        const res = await fetch('/api/tenant-admin/members/invites');
                        const data = await res.json();
                        if (data.success) invites.value = data.invites;
                    } catch (e) { console.error(e); }
                }

                async function loadSettings() {
                    try {
                        const res = await fetch('/api/tenant-admin/settings');
                        const data = await res.json();
                        if (data.success) settings.value = data.settings;
                    } catch (e) { console.error(e); }
                }

                async function loadIntegrations() {
                    try {
                        const res = await fetch('/api/tenant-admin/integrations');
                        const data = await res.json();
                        if (data.success) integrations.value = data.integrations;
                    } catch (e) { console.error(e); }
                }

                async function loadBilling() {
                    try {
                        const res = await fetch('/api/tenant-admin/billing');
                        const data = await res.json();
                        if (data.success) billing.value = data.billing;
                    } catch (e) { console.error(e); }
                }

                async function loadAuditLogs() {
                    try {
                        const res = await fetch('/api/tenant-admin/audit-logs');
                        const data = await res.json();
                        if (data.success) auditLogs.value = data.logs;
                    } catch (e) { console.error(e); }
                }

                async function sendInvite() {
                    try {
                        const res = await fetch('/api/tenant-admin/members/invite', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(inviteForm.value)
                        });
                        const data = await res.json();
                        if (data.success) {
                            alert('Convite enviado!');
                            showInviteModal.value = false;
                            loadInvites();
                        }
                    } catch (e) { console.error(e); }
                }

                async function revokeInvite(id) {
                    if (!confirm('Revogar convite?')) return;
                    try {
                        await fetch(`/api/tenant-admin/members/invites/${id}`, { method: 'DELETE' });
                        loadInvites();
                    } catch (e) { console.error(e); }
                }

                function editMember(member) {
                    alert('Editar membro: ' + member.user?.username);
                }

                onMounted(() => {
                    loadDashboard();
                    loadMembers();
                    loadInvites();
                    loadSettings();
                    loadIntegrations();
                    loadBilling();
                    loadAuditLogs();
                });

                return {
                    tab, tenant, dashboard, members, invites, settings,
                    integrations, billing, auditLogs, showInviteModal,
                    inviteForm, sendInvite, revokeInvite, editMember
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
"""

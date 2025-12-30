# -*- coding: utf-8 -*-
"""
Admin Portal - Portal de Administracao Multi-nivel
====================================================

Issue #113 - Portal de Administracao Multi-nivel

Este modulo fornece a UI do portal de administracao com 3 niveis:

1. Platform Admin (Super Admin):
   - Dashboard com metricas globais
   - Gestao de tenants
   - Configuracoes da plataforma

2. Tenant Admin:
   - Dashboard do tenant
   - Gestao de usuarios
   - Configuracoes do tenant

3. Project Admin:
   - Dashboard do projeto
   - Gestao de membros
   - Configuracoes do projeto

Componentes:
- admin_portal_html(): Portal principal com navegacao
- platform_dashboard(): Dashboard do platform admin
- tenant_dashboard(): Dashboard do tenant admin
- project_dashboard(): Dashboard do project admin
"""

from datetime import datetime
from typing import Dict, Any, List, Optional


# =============================================================================
# UI COMPONENTS (HTML/JavaScript)
# =============================================================================

def generate_admin_portal_html(admin_level: str = "platform") -> str:
    """
    Gera HTML do portal de administracao

    Args:
        admin_level: Nivel de admin (platform, tenant, project)

    Returns:
        HTML completo
    """
    return '''
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Portal de Administracao - Fabrica de Agentes</title>
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
            --sidebar-width: 260px;
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
        .admin-layout {
            display: flex;
            min-height: 100vh;
        }

        /* Sidebar */
        .sidebar {
            width: var(--sidebar-width);
            background: var(--primary);
            color: white;
            display: flex;
            flex-direction: column;
            position: fixed;
            top: 0;
            left: 0;
            bottom: 0;
            z-index: 100;
        }

        .sidebar-header {
            padding: 20px;
            border-bottom: 1px solid rgba(255,255,255,0.1);
        }

        .sidebar-logo {
            font-size: 18px;
            font-weight: 700;
        }

        .sidebar-subtitle {
            font-size: 12px;
            opacity: 0.7;
            margin-top: 4px;
        }

        .admin-level-badge {
            display: inline-block;
            padding: 4px 8px;
            background: var(--secondary);
            border-radius: 4px;
            font-size: 11px;
            font-weight: 600;
            margin-top: 8px;
        }

        .sidebar-nav {
            flex: 1;
            padding: 16px 0;
            overflow-y: auto;
        }

        .nav-section {
            margin-bottom: 24px;
        }

        .nav-section-title {
            padding: 0 20px 8px;
            font-size: 11px;
            text-transform: uppercase;
            opacity: 0.5;
            letter-spacing: 0.5px;
        }

        .nav-item {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px 20px;
            color: rgba(255,255,255,0.7);
            text-decoration: none;
            transition: all 0.2s;
            cursor: pointer;
        }

        .nav-item:hover {
            background: rgba(255,255,255,0.1);
            color: white;
        }

        .nav-item.active {
            background: rgba(255,255,255,0.15);
            color: white;
            border-right: 3px solid var(--secondary);
        }

        .nav-icon {
            width: 20px;
            height: 20px;
            opacity: 0.7;
        }

        .nav-item.active .nav-icon {
            opacity: 1;
        }

        .sidebar-footer {
            padding: 16px 20px;
            border-top: 1px solid rgba(255,255,255,0.1);
        }

        .user-info {
            display: flex;
            align-items: center;
            gap: 12px;
        }

        .user-avatar {
            width: 36px;
            height: 36px;
            border-radius: 50%;
            background: var(--secondary);
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: 600;
        }

        .user-details {
            flex: 1;
        }

        .user-name {
            font-size: 14px;
            font-weight: 500;
        }

        .user-role {
            font-size: 12px;
            opacity: 0.7;
        }

        /* Main Content */
        .main-content {
            flex: 1;
            margin-left: var(--sidebar-width);
            min-height: 100vh;
        }

        /* Header */
        .main-header {
            background: var(--surface);
            padding: 16px 24px;
            border-bottom: 1px solid var(--border);
            display: flex;
            justify-content: space-between;
            align-items: center;
            position: sticky;
            top: 0;
            z-index: 50;
        }

        .header-title {
            font-size: 20px;
            font-weight: 600;
        }

        .header-breadcrumb {
            font-size: 14px;
            color: var(--text-secondary);
        }

        .header-actions {
            display: flex;
            gap: 12px;
        }

        /* Content Area */
        .content-area {
            padding: 24px;
        }

        /* Cards Grid */
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 20px;
            margin-bottom: 24px;
        }

        .metric-card {
            background: var(--surface);
            border-radius: 12px;
            padding: 24px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .metric-card-header {
            display: flex;
            justify-content: space-between;
            align-items: flex-start;
            margin-bottom: 16px;
        }

        .metric-icon {
            width: 48px;
            height: 48px;
            border-radius: 10px;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .metric-icon.blue { background: #DBEAFE; color: #2563EB; }
        .metric-icon.green { background: #D1FAE5; color: #059669; }
        .metric-icon.orange { background: #FED7AA; color: #EA580C; }
        .metric-icon.purple { background: #E9D5FF; color: #9333EA; }

        .metric-trend {
            font-size: 12px;
            display: flex;
            align-items: center;
            gap: 4px;
        }

        .metric-trend.up { color: var(--success); }
        .metric-trend.down { color: var(--danger); }

        .metric-value {
            font-size: 32px;
            font-weight: 700;
            color: var(--primary);
            margin-bottom: 4px;
        }

        .metric-label {
            font-size: 14px;
            color: var(--text-secondary);
        }

        /* Panels */
        .panels-grid {
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 24px;
        }

        .panel {
            background: var(--surface);
            border-radius: 12px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
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

        /* Table */
        .data-table {
            width: 100%;
            border-collapse: collapse;
        }

        .data-table th,
        .data-table td {
            padding: 12px 16px;
            text-align: left;
            border-bottom: 1px solid var(--border);
        }

        .data-table th {
            font-weight: 600;
            font-size: 13px;
            color: var(--text-secondary);
            text-transform: uppercase;
            background: var(--bg);
        }

        .data-table tr:hover {
            background: var(--bg);
        }

        /* Status Badge */
        .status-badge {
            display: inline-block;
            padding: 4px 10px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: 500;
        }

        .status-active { background: #D1FAE5; color: #065F46; }
        .status-trial { background: #DBEAFE; color: #1E40AF; }
        .status-suspended { background: #FEE2E2; color: #991B1B; }

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

        .btn-outline {
            background: transparent;
            border: 1px solid var(--border);
            color: var(--text);
        }

        .btn-outline:hover {
            background: var(--bg);
        }

        .btn-sm {
            padding: 6px 12px;
            font-size: 12px;
        }

        .btn-link {
            background: none;
            border: none;
            color: var(--primary);
            padding: 0;
            cursor: pointer;
        }

        .btn-link:hover {
            text-decoration: underline;
        }

        /* Activity List */
        .activity-list {
            list-style: none;
        }

        .activity-item {
            display: flex;
            gap: 12px;
            padding: 12px 0;
            border-bottom: 1px solid var(--border);
        }

        .activity-item:last-child {
            border-bottom: none;
        }

        .activity-dot {
            width: 8px;
            height: 8px;
            border-radius: 50%;
            margin-top: 6px;
        }

        .activity-dot.blue { background: var(--info); }
        .activity-dot.green { background: var(--success); }
        .activity-dot.orange { background: var(--warning); }

        .activity-content {
            flex: 1;
        }

        .activity-text {
            font-size: 14px;
            margin-bottom: 4px;
        }

        .activity-time {
            font-size: 12px;
            color: var(--text-secondary);
        }

        /* Quick Actions */
        .quick-actions {
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 12px;
        }

        .quick-action {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 16px;
            background: var(--bg);
            border-radius: 8px;
            cursor: pointer;
            transition: all 0.2s;
            text-decoration: none;
            color: var(--text);
        }

        .quick-action:hover {
            background: var(--border);
        }

        .quick-action-icon {
            width: 40px;
            height: 40px;
            border-radius: 8px;
            background: var(--primary);
            color: white;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .quick-action-text {
            font-size: 14px;
            font-weight: 500;
        }

        /* Tabs */
        .tabs {
            display: flex;
            gap: 4px;
            border-bottom: 1px solid var(--border);
            margin-bottom: 20px;
        }

        .tab {
            padding: 12px 20px;
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

        /* Responsive */
        @media (max-width: 1200px) {
            .metrics-grid {
                grid-template-columns: repeat(2, 1fr);
            }
            .panels-grid {
                grid-template-columns: 1fr;
            }
        }

        @media (max-width: 768px) {
            .sidebar {
                transform: translateX(-100%);
            }
            .main-content {
                margin-left: 0;
            }
            .metrics-grid {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <div class="admin-layout">
        <!-- Sidebar -->
        <aside class="sidebar">
            <div class="sidebar-header">
                <div class="sidebar-logo">Fabrica de Agentes</div>
                <div class="sidebar-subtitle">Portal de Administracao</div>
                <div class="admin-level-badge" id="admin-level-badge">PLATFORM ADMIN</div>
            </div>

            <nav class="sidebar-nav" id="sidebar-nav">
                <!-- Platform Admin Navigation -->
                <div class="nav-section" id="nav-platform">
                    <div class="nav-section-title">Plataforma</div>
                    <a class="nav-item active" onclick="showSection('dashboard')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path d="M10.707 2.293a1 1 0 00-1.414 0l-7 7a1 1 0 001.414 1.414L4 10.414V17a1 1 0 001 1h2a1 1 0 001-1v-2a1 1 0 011-1h2a1 1 0 011 1v2a1 1 0 001 1h2a1 1 0 001-1v-6.586l.293.293a1 1 0 001.414-1.414l-7-7z"/>
                        </svg>
                        Dashboard
                    </a>
                    <a class="nav-item" onclick="showSection('tenants')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path d="M4 4a2 2 0 012-2h8a2 2 0 012 2v12a1 1 0 110 2h-3a1 1 0 01-1-1v-2a1 1 0 00-1-1H9a1 1 0 00-1 1v2a1 1 0 01-1 1H4a1 1 0 110-2V4zm3 1h2v2H7V5zm2 4H7v2h2V9zm2-4h2v2h-2V5zm2 4h-2v2h2V9z"/>
                        </svg>
                        Tenants
                    </a>
                    <a class="nav-item" onclick="showSection('users')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path d="M9 6a3 3 0 11-6 0 3 3 0 016 0zM17 6a3 3 0 11-6 0 3 3 0 016 0zM12.93 17c.046-.327.07-.66.07-1a6.97 6.97 0 00-1.5-4.33A5 5 0 0119 16v1h-6.07zM6 11a5 5 0 015 5v1H1v-1a5 5 0 015-5z"/>
                        </svg>
                        Usuarios
                    </a>
                    <a class="nav-item" onclick="showSection('workers')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path fill-rule="evenodd" d="M2 5a2 2 0 012-2h12a2 2 0 012 2v10a2 2 0 01-2 2H4a2 2 0 01-2-2V5zm3.293 1.293a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 01-1.414-1.414L7.586 10 5.293 7.707a1 1 0 010-1.414zM11 12a1 1 0 100 2h3a1 1 0 100-2h-3z" clip-rule="evenodd"/>
                        </svg>
                        Workers
                    </a>
                </div>

                <div class="nav-section">
                    <div class="nav-section-title">Sistema</div>
                    <a class="nav-item" onclick="showSection('metrics')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path d="M2 10a8 8 0 018-8v8h8a8 8 0 11-16 0z"/>
                            <path d="M12 2.252A8.014 8.014 0 0117.748 8H12V2.252z"/>
                        </svg>
                        Metricas
                    </a>
                    <a class="nav-item" onclick="showSection('audit')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path fill-rule="evenodd" d="M4 2a1 1 0 011 1v2.101a7.002 7.002 0 0111.601 2.566 1 1 0 11-1.885.666A5.002 5.002 0 005.999 7H9a1 1 0 010 2H4a1 1 0 01-1-1V3a1 1 0 011-1zm.008 9.057a1 1 0 011.276.61A5.002 5.002 0 0014.001 13H11a1 1 0 110-2h5a1 1 0 011 1v5a1 1 0 11-2 0v-2.101a7.002 7.002 0 01-11.601-2.566 1 1 0 01.61-1.276z" clip-rule="evenodd"/>
                        </svg>
                        Auditoria
                    </a>
                    <a class="nav-item" onclick="showSection('settings')">
                        <svg class="nav-icon" fill="currentColor" viewBox="0 0 20 20">
                            <path fill-rule="evenodd" d="M11.49 3.17c-.38-1.56-2.6-1.56-2.98 0a1.532 1.532 0 01-2.286.948c-1.372-.836-2.942.734-2.106 2.106.54.886.061 2.042-.947 2.287-1.561.379-1.561 2.6 0 2.978a1.532 1.532 0 01.947 2.287c-.836 1.372.734 2.942 2.106 2.106a1.532 1.532 0 012.287.947c.379 1.561 2.6 1.561 2.978 0a1.533 1.533 0 012.287-.947c1.372.836 2.942-.734 2.106-2.106a1.533 1.533 0 01.947-2.287c1.561-.379 1.561-2.6 0-2.978a1.532 1.532 0 01-.947-2.287c.836-1.372-.734-2.942-2.106-2.106a1.532 1.532 0 01-2.287-.947zM10 13a3 3 0 100-6 3 3 0 000 6z" clip-rule="evenodd"/>
                        </svg>
                        Configuracoes
                    </a>
                </div>
            </nav>

            <div class="sidebar-footer">
                <div class="user-info">
                    <div class="user-avatar">A</div>
                    <div class="user-details">
                        <div class="user-name">Admin</div>
                        <div class="user-role">Platform Admin</div>
                    </div>
                </div>
            </div>
        </aside>

        <!-- Main Content -->
        <main class="main-content">
            <header class="main-header">
                <div>
                    <div class="header-breadcrumb">Administracao</div>
                    <h1 class="header-title" id="page-title">Dashboard</h1>
                </div>
                <div class="header-actions">
                    <button class="btn btn-outline btn-sm" onclick="refreshData()">
                        Atualizar
                    </button>
                </div>
            </header>

            <div class="content-area" id="content-area">
                <!-- Dashboard Section -->
                <section id="section-dashboard">
                    <!-- Metrics Grid -->
                    <div class="metrics-grid">
                        <div class="metric-card">
                            <div class="metric-card-header">
                                <div class="metric-icon blue">
                                    <svg width="24" height="24" fill="currentColor" viewBox="0 0 20 20">
                                        <path d="M4 4a2 2 0 012-2h8a2 2 0 012 2v12a1 1 0 110 2h-3a1 1 0 01-1-1v-2a1 1 0 00-1-1H9a1 1 0 00-1 1v2a1 1 0 01-1 1H4a1 1 0 110-2V4z"/>
                                    </svg>
                                </div>
                                <div class="metric-trend up">+12%</div>
                            </div>
                            <div class="metric-value" id="metric-tenants">-</div>
                            <div class="metric-label">Tenants Ativos</div>
                        </div>

                        <div class="metric-card">
                            <div class="metric-card-header">
                                <div class="metric-icon green">
                                    <svg width="24" height="24" fill="currentColor" viewBox="0 0 20 20">
                                        <path d="M9 6a3 3 0 11-6 0 3 3 0 016 0zM17 6a3 3 0 11-6 0 3 3 0 016 0zM12.93 17c.046-.327.07-.66.07-1a6.97 6.97 0 00-1.5-4.33A5 5 0 0119 16v1h-6.07zM6 11a5 5 0 015 5v1H1v-1a5 5 0 015-5z"/>
                                    </svg>
                                </div>
                                <div class="metric-trend up">+8%</div>
                            </div>
                            <div class="metric-value" id="metric-users">-</div>
                            <div class="metric-label">Usuarios Ativos</div>
                        </div>

                        <div class="metric-card">
                            <div class="metric-card-header">
                                <div class="metric-icon orange">
                                    <svg width="24" height="24" fill="currentColor" viewBox="0 0 20 20">
                                        <path fill-rule="evenodd" d="M2 5a2 2 0 012-2h12a2 2 0 012 2v10a2 2 0 01-2 2H4a2 2 0 01-2-2V5zm3.293 1.293a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 01-1.414-1.414L7.586 10 5.293 7.707a1 1 0 010-1.414z" clip-rule="evenodd"/>
                                    </svg>
                                </div>
                            </div>
                            <div class="metric-value" id="metric-workers">-</div>
                            <div class="metric-label">Workers Ativos</div>
                        </div>

                        <div class="metric-card">
                            <div class="metric-card-header">
                                <div class="metric-icon purple">
                                    <svg width="24" height="24" fill="currentColor" viewBox="0 0 20 20">
                                        <path d="M8.433 7.418c.155-.103.346-.196.567-.267v1.698a2.305 2.305 0 01-.567-.267C8.07 8.34 8 8.114 8 8c0-.114.07-.34.433-.582zM11 12.849v-1.698c.22.071.412.164.567.267.364.243.433.468.433.582 0 .114-.07.34-.433.582a2.305 2.305 0 01-.567.267z"/>
                                        <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm1-13a1 1 0 10-2 0v.092a4.535 4.535 0 00-1.676.662C6.602 6.234 6 7.009 6 8c0 .99.602 1.765 1.324 2.246.48.32 1.054.545 1.676.662v1.941c-.391-.127-.68-.317-.843-.504a1 1 0 10-1.51 1.31c.562.649 1.413 1.076 2.353 1.253V15a1 1 0 102 0v-.092a4.535 4.535 0 001.676-.662C13.398 13.766 14 12.991 14 12c0-.99-.602-1.765-1.324-2.246A4.535 4.535 0 0011 9.092V7.151c.391.127.68.317.843.504a1 1 0 101.511-1.31c-.563-.649-1.413-1.076-2.354-1.253V5z" clip-rule="evenodd"/>
                                    </svg>
                                </div>
                            </div>
                            <div class="metric-value" id="metric-revenue">-</div>
                            <div class="metric-label">Receita MTD</div>
                        </div>
                    </div>

                    <!-- Panels Grid -->
                    <div class="panels-grid">
                        <!-- Tenants Panel -->
                        <div class="panel">
                            <div class="panel-header">
                                <h3 class="panel-title">Tenants Recentes</h3>
                                <button class="btn btn-link btn-sm" onclick="showSection('tenants')">Ver todos</button>
                            </div>
                            <div class="panel-body" style="padding: 0;">
                                <table class="data-table" id="tenants-table">
                                    <thead>
                                        <tr>
                                            <th>Tenant</th>
                                            <th>Plano</th>
                                            <th>Status</th>
                                            <th></th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        <tr>
                                            <td colspan="4" style="text-align: center; color: var(--text-secondary);">
                                                Carregando...
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            </div>
                        </div>

                        <!-- Activity Panel -->
                        <div class="panel">
                            <div class="panel-header">
                                <h3 class="panel-title">Atividade Recente</h3>
                            </div>
                            <div class="panel-body">
                                <ul class="activity-list" id="activity-list">
                                    <li class="activity-item">
                                        <div class="activity-dot green"></div>
                                        <div class="activity-content">
                                            <div class="activity-text">Carregando atividades...</div>
                                        </div>
                                    </li>
                                </ul>
                            </div>
                        </div>
                    </div>

                    <!-- Quick Actions -->
                    <div class="panel" style="margin-top: 24px;">
                        <div class="panel-header">
                            <h3 class="panel-title">Acoes Rapidas</h3>
                        </div>
                        <div class="panel-body">
                            <div class="quick-actions">
                                <a class="quick-action" onclick="showSection('tenants')">
                                    <div class="quick-action-icon">+</div>
                                    <span class="quick-action-text">Novo Tenant</span>
                                </a>
                                <a class="quick-action" onclick="showSection('users')">
                                    <div class="quick-action-icon">+</div>
                                    <span class="quick-action-text">Novo Usuario</span>
                                </a>
                                <a class="quick-action" onclick="showSection('workers')">
                                    <div class="quick-action-icon">+</div>
                                    <span class="quick-action-text">Iniciar Worker</span>
                                </a>
                                <a class="quick-action" onclick="showSection('metrics')">
                                    <div class="quick-action-icon">+</div>
                                    <span class="quick-action-text">Ver Metricas</span>
                                </a>
                            </div>
                        </div>
                    </div>
                </section>

                <!-- Tenants Section -->
                <section id="section-tenants" style="display: none;">
                    <div class="panel">
                        <div class="panel-header">
                            <h3 class="panel-title">Gestao de Tenants</h3>
                            <button class="btn btn-primary btn-sm">+ Novo Tenant</button>
                        </div>
                        <div class="panel-body" style="padding: 0;">
                            <table class="data-table" id="all-tenants-table">
                                <thead>
                                    <tr>
                                        <th>Tenant</th>
                                        <th>Plano</th>
                                        <th>Membros</th>
                                        <th>Projetos</th>
                                        <th>Status</th>
                                        <th>Criado em</th>
                                        <th></th>
                                    </tr>
                                </thead>
                                <tbody>
                                    <tr>
                                        <td colspan="7" style="text-align: center; color: var(--text-secondary);">
                                            Carregando tenants...
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        </div>
                    </div>
                </section>

                <!-- Users Section -->
                <section id="section-users" style="display: none;">
                    <iframe src="/admin/users/?tenant=default" style="width: 100%; height: calc(100vh - 120px); border: none;"></iframe>
                </section>

                <!-- Workers Section -->
                <section id="section-workers" style="display: none;">
                    <iframe src="/api/monitoring/dashboard" style="width: 100%; height: calc(100vh - 120px); border: none;"></iframe>
                </section>

                <!-- Issue #296: Metrics Section -->
                <section id="section-metrics" style="display: none;" class="config-section">
                    <div class="panel">
                        <div class="panel-header">
                            <h3 class="panel-title">Metricas da Plataforma</h3>
                        </div>
                        <div class="panel-body">
                            <div class="metrics-grid">
                                <div class="metric-card">
                                    <div class="metric-value">0</div>
                                    <div class="metric-label">Requisicoes/hora</div>
                                </div>
                                <div class="metric-card">
                                    <div class="metric-value">0ms</div>
                                    <div class="metric-label">Latencia Media</div>
                                </div>
                                <div class="metric-card">
                                    <div class="metric-value">0%</div>
                                    <div class="metric-label">Taxa de Erro</div>
                                </div>
                                <div class="metric-card">
                                    <div class="metric-value">0%</div>
                                    <div class="metric-label">CPU Uso</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </section>

                <!-- Issue #296: Audit Section -->
                <section id="section-audit" style="display: none;" class="config-section">
                    <div class="panel">
                        <div class="panel-header">
                            <h3 class="panel-title">Logs de Auditoria</h3>
                            <div class="header-actions">
                                <select class="filter-select">
                                    <option value="">Todos os tipos</option>
                                    <option value="login">Login</option>
                                    <option value="create">Criacao</option>
                                    <option value="update">Atualizacao</option>
                                    <option value="delete">Exclusao</option>
                                </select>
                            </div>
                        </div>
                        <div class="panel-body" style="padding: 0;">
                            <table class="data-table">
                                <thead>
                                    <tr>
                                        <th>Data/Hora</th>
                                        <th>Usuario</th>
                                        <th>Acao</th>
                                        <th>Recurso</th>
                                        <th>IP</th>
                                    </tr>
                                </thead>
                                <tbody id="audit-logs">
                                    <tr>
                                        <td colspan="5" style="text-align: center; color: var(--text-secondary);">
                                            Nenhum log encontrado
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        </div>
                    </div>
                </section>

                <!-- Issue #296: Settings Section with Forms and Config Sections -->
                <section id="section-settings" style="display: none;">
                    <div class="tabs">
                        <button class="tab active" onclick="showSettingsTab('general')">Geral</button>
                        <button class="tab" onclick="showSettingsTab('branding')">Branding</button>
                        <button class="tab" onclick="showSettingsTab('notifications')">Notificacoes</button>
                        <button class="tab" onclick="showSettingsTab('integrations')">Integracoes</button>
                    </div>

                    <!-- General Settings -->
                    <div id="settings-general" class="config-section" data-testid="config-section">
                        <div class="panel">
                            <div class="panel-header">
                                <h3 class="panel-title">Configuracoes Gerais</h3>
                            </div>
                            <div class="panel-body">
                                <form id="general-settings-form" class="settings-form" data-testid="settings-form">
                                    <div class="form-group">
                                        <label class="form-label">Nome da Plataforma</label>
                                        <input type="text" class="form-input" name="platform_name" value="Fabrica de Agentes">
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Email de Suporte</label>
                                        <input type="email" class="form-input" name="support_email" placeholder="suporte@exemplo.com">
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Timezone Padrao</label>
                                        <select class="form-select" name="timezone">
                                            <option value="America/Sao_Paulo">America/Sao_Paulo (BRT)</option>
                                            <option value="UTC">UTC</option>
                                            <option value="America/New_York">America/New_York (EST)</option>
                                        </select>
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">
                                            <input type="checkbox" name="maintenance_mode"> Modo de Manutencao
                                        </label>
                                    </div>
                                    <button type="submit" class="btn btn-primary">Salvar Configuracoes</button>
                                </form>
                            </div>
                        </div>
                    </div>

                    <!-- Branding Settings -->
                    <div id="settings-branding" class="config-section" style="display: none;">
                        <div class="panel">
                            <div class="panel-header">
                                <h3 class="panel-title">Branding</h3>
                            </div>
                            <div class="panel-body">
                                <form id="branding-settings-form" class="settings-form">
                                    <div class="form-group">
                                        <label class="form-label">Logo URL</label>
                                        <input type="url" class="form-input" name="logo_url" placeholder="https://...">
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Cor Primaria</label>
                                        <input type="color" name="primary_color" value="#003B4A">
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Cor Secundaria</label>
                                        <input type="color" name="secondary_color" value="#FF6C00">
                                    </div>
                                    <button type="submit" class="btn btn-primary">Salvar Branding</button>
                                </form>
                            </div>
                        </div>
                    </div>

                    <!-- Notification Settings -->
                    <div id="settings-notifications" class="config-section" style="display: none;">
                        <div class="panel">
                            <div class="panel-header">
                                <h3 class="panel-title">Notificacoes</h3>
                            </div>
                            <div class="panel-body">
                                <form id="notification-settings-form" class="settings-form">
                                    <div class="form-group">
                                        <label class="form-label">
                                            <input type="checkbox" name="email_notifications" checked> Notificacoes por Email
                                        </label>
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">
                                            <input type="checkbox" name="slack_notifications"> Notificacoes no Slack
                                        </label>
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Webhook URL</label>
                                        <input type="url" class="form-input" name="webhook_url" placeholder="https://...">
                                    </div>
                                    <button type="submit" class="btn btn-primary">Salvar Notificacoes</button>
                                </form>
                            </div>
                        </div>
                    </div>

                    <!-- Integration Settings -->
                    <div id="settings-integrations" class="config-section" style="display: none;">
                        <div class="panel">
                            <div class="panel-header">
                                <h3 class="panel-title">Integracoes</h3>
                            </div>
                            <div class="panel-body">
                                <form id="integration-settings-form" class="settings-form">
                                    <div class="form-group">
                                        <label class="form-label">GitHub Token</label>
                                        <input type="password" class="form-input" name="github_token" placeholder="ghp_...">
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Jira URL</label>
                                        <input type="url" class="form-input" name="jira_url" placeholder="https://your-org.atlassian.net">
                                    </div>
                                    <div class="form-group">
                                        <label class="form-label">Azure DevOps Organization</label>
                                        <input type="text" class="form-input" name="azure_org" placeholder="your-organization">
                                    </div>
                                    <button type="submit" class="btn btn-primary">Salvar Integracoes</button>
                                </form>
                            </div>
                        </div>
                    </div>
                </section>
            </div>
        </main>
    </div>

    <script>
        // Show section
        function showSection(sectionId) {
            // Hide all sections
            document.querySelectorAll('[id^="section-"]').forEach(section => {
                section.style.display = 'none';
            });

            // Show selected section
            const section = document.getElementById('section-' + sectionId);
            if (section) {
                section.style.display = 'block';
            }

            // Update nav
            document.querySelectorAll('.nav-item').forEach(item => {
                item.classList.remove('active');
            });
            event.target.closest('.nav-item').classList.add('active');

            // Update title
            const titles = {
                'dashboard': 'Dashboard',
                'tenants': 'Gestao de Tenants',
                'users': 'Administracao de Usuarios',
                'workers': 'Monitoramento de Workers',
                'metrics': 'Metricas da Plataforma',
                'audit': 'Logs de Auditoria',
                'settings': 'Configuracoes'
            };
            document.getElementById('page-title').textContent = titles[sectionId] || sectionId;

            // Load section data
            if (sectionId === 'tenants') {
                loadAllTenants();
            }
        }

        // Load dashboard data
        async function loadDashboardData() {
            try {
                const res = await fetch('/api/v1/platform/metrics');
                const data = await res.json();
                const metrics = data.metrics;

                document.getElementById('metric-tenants').textContent = metrics.tenants?.active || 0;
                document.getElementById('metric-users').textContent = metrics.users?.active || 0;
                document.getElementById('metric-workers').textContent = metrics.workers?.active || 0;
                document.getElementById('metric-revenue').textContent = '$' + (metrics.revenue?.mtd || 0).toFixed(2);

            } catch (error) {
                console.error('Error loading dashboard:', error);
            }
        }

        // Load recent tenants
        async function loadRecentTenants() {
            try {
                const res = await fetch('/api/v1/platform/tenants?limit=5');
                const data = await res.json();

                const tbody = document.querySelector('#tenants-table tbody');
                if (!data.tenants || data.tenants.length === 0) {
                    tbody.innerHTML = '<tr><td colspan="4" style="text-align: center; color: var(--text-secondary);">Nenhum tenant encontrado</td></tr>';
                    return;
                }

                tbody.innerHTML = data.tenants.map(t => `
                    <tr>
                        <td><strong>${t.name}</strong><br><small style="color: var(--text-secondary);">${t.slug}</small></td>
                        <td>${t.plan?.toUpperCase() || 'FREE'}</td>
                        <td><span class="status-badge status-${t.status}">${t.status?.toUpperCase() || 'ACTIVE'}</span></td>
                        <td><button class="btn btn-link btn-sm">Ver</button></td>
                    </tr>
                `).join('');

            } catch (error) {
                console.error('Error loading tenants:', error);
            }
        }

        // Load all tenants
        async function loadAllTenants() {
            try {
                const res = await fetch('/api/v1/platform/tenants?limit=50');
                const data = await res.json();

                const tbody = document.querySelector('#all-tenants-table tbody');
                if (!data.tenants || data.tenants.length === 0) {
                    tbody.innerHTML = '<tr><td colspan="7" style="text-align: center; color: var(--text-secondary);">Nenhum tenant encontrado</td></tr>';
                    return;
                }

                tbody.innerHTML = data.tenants.map(t => `
                    <tr>
                        <td><strong>${t.name}</strong><br><small style="color: var(--text-secondary);">${t.email}</small></td>
                        <td>${t.plan?.toUpperCase() || 'FREE'}</td>
                        <td>${t.members_count || 0}</td>
                        <td>${t.projects_count || 0}</td>
                        <td><span class="status-badge status-${t.status}">${t.status?.toUpperCase() || 'ACTIVE'}</span></td>
                        <td>${t.created_at ? new Date(t.created_at).toLocaleDateString() : '-'}</td>
                        <td>
                            <button class="btn btn-outline btn-sm">Editar</button>
                        </td>
                    </tr>
                `).join('');

            } catch (error) {
                console.error('Error loading tenants:', error);
            }
        }

        // Issue #296: Settings tabs navigation
        function showSettingsTab(tabId) {
            // Hide all settings tabs
            document.querySelectorAll('[id^="settings-"]').forEach(tab => {
                tab.style.display = 'none';
            });

            // Show selected tab
            const tab = document.getElementById('settings-' + tabId);
            if (tab) {
                tab.style.display = 'block';
            }

            // Update tab buttons
            document.querySelectorAll('#section-settings .tab').forEach(btn => {
                btn.classList.remove('active');
            });
            event.target.classList.add('active');
        }

        // Refresh data
        function refreshData() {
            loadDashboardData();
            loadRecentTenants();
        }

        // Initial load
        document.addEventListener('DOMContentLoaded', () => {
            loadDashboardData();
            loadRecentTenants();
        });
    </script>
</body>
</html>
'''


# =============================================================================
# API ENDPOINTS
# =============================================================================

def register_admin_portal_endpoints(app):
    """
    Registra endpoints do portal de administracao no app FastAPI

    Args:
        app: Instancia do FastAPI
    """
    from fastapi import APIRouter
    from fastapi.responses import HTMLResponse

    router = APIRouter(prefix="/admin/portal", tags=["admin-portal"])

    @router.get("/", response_class=HTMLResponse)
    async def admin_portal_page():
        """Retorna pagina HTML do portal de administracao"""
        return generate_admin_portal_html()

    @router.get("/platform", response_class=HTMLResponse)
    async def platform_admin_page():
        """Retorna pagina do platform admin"""
        return generate_admin_portal_html("platform")

    @router.get("/tenant/{tenant_id}", response_class=HTMLResponse)
    async def tenant_admin_page(tenant_id: str):
        """Retorna pagina do tenant admin"""
        return generate_admin_portal_html("tenant")

    @router.get("/project/{project_id}", response_class=HTMLResponse)
    async def project_admin_page(project_id: str):
        """Retorna pagina do project admin"""
        return generate_admin_portal_html("project")

    app.include_router(router)
    print("[Dashboard] Admin Portal endpoints registered")


# =============================================================================
# STANDALONE RUN
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    from fastapi import FastAPI
    from fastapi.responses import HTMLResponse

    app = FastAPI(title="Admin Portal")

    # Include routes
    from factory.api.portal_routes import platform_router, tenant_router, project_router
    app.include_router(platform_router)
    app.include_router(tenant_router)
    app.include_router(project_router)

    # Include monitoring
    from factory.dashboard.worker_monitoring import register_monitoring_endpoints
    register_monitoring_endpoints(app)

    # Include admin users
    from factory.dashboard.admin_users import register_admin_users_endpoints
    register_admin_users_endpoints(app)

    register_admin_portal_endpoints(app)

    @app.get("/", response_class=HTMLResponse)
    async def root():
        return generate_admin_portal_html()

    print("[Admin Portal] Starting standalone server on port 9004...")
    uvicorn.run(app, host="0.0.0.0", port=9004)

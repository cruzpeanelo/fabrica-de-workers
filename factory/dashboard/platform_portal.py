# -*- coding: utf-8 -*-
"""
Platform Portal Page (Issue #287)
==================================

HTML page for Platform Owner (Super Admin) to manage the entire platform.

Author: Plataforma E - Terminal 4
"""

import logging
from fastapi import APIRouter, Request
from fastapi.responses import HTMLResponse

logger = logging.getLogger(__name__)


def register_platform_portal(app):
    """Register platform portal page"""

    @app.get("/platform", response_class=HTMLResponse)
    async def platform_portal_page(request: Request):
        """Platform Portal for Super Admin"""
        return get_platform_portal_html()

    @app.get("/platform/tenants", response_class=HTMLResponse)
    async def platform_tenants_page(request: Request):
        """Tenants management page"""
        return get_platform_portal_html()

    @app.get("/platform/settings", response_class=HTMLResponse)
    async def platform_settings_page(request: Request):
        """Platform settings page"""
        return get_platform_portal_html()

    @app.get("/platform/modules", response_class=HTMLResponse)
    async def platform_modules_page(request: Request):
        """Modules configuration page"""
        return get_platform_portal_html()

    @app.get("/platform/billing", response_class=HTMLResponse)
    async def platform_billing_page(request: Request):
        """Platform billing overview page"""
        return get_platform_portal_html()

    logger.info("[Platform Portal] Pages registered at /platform/*")


def get_platform_portal_html():
    """Generate Platform Portal HTML"""
    return """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Platform Portal - Plataforma E</title>
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

        /* Header */
        .header {
            background: linear-gradient(135deg, var(--primary) 0%, var(--primary-light) 100%);
            color: white;
            padding: 1rem 2rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        .header h1 {
            font-size: 1.5rem;
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .header .badge {
            background: var(--accent);
            padding: 0.25rem 0.75rem;
            border-radius: 9999px;
            font-size: 0.75rem;
            font-weight: 600;
        }

        .header-actions {
            display: flex;
            gap: 1rem;
            align-items: center;
        }

        .back-link {
            color: white;
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: 0.5rem;
            opacity: 0.9;
        }

        .back-link:hover { opacity: 1; }

        /* Layout */
        .layout {
            display: flex;
            min-height: calc(100vh - 60px);
        }

        /* Sidebar */
        .sidebar {
            width: 250px;
            background: white;
            border-right: 1px solid var(--gray-200);
            padding: 1.5rem 0;
        }

        .nav-item {
            display: flex;
            align-items: center;
            gap: 0.75rem;
            padding: 0.875rem 1.5rem;
            color: var(--gray-700);
            text-decoration: none;
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
            margin: 1rem 0;
        }

        .nav-label {
            font-size: 0.75rem;
            text-transform: uppercase;
            color: var(--gray-500);
            padding: 0.5rem 1.5rem;
            font-weight: 600;
        }

        /* Main Content */
        .main {
            flex: 1;
            padding: 2rem;
            overflow-y: auto;
        }

        .page-title {
            font-size: 1.5rem;
            color: var(--gray-900);
            margin-bottom: 0.5rem;
        }

        .page-subtitle {
            color: var(--gray-500);
            margin-bottom: 2rem;
        }

        /* Cards */
        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 1.5rem;
            margin-bottom: 2rem;
        }

        .stat-card {
            background: white;
            border-radius: 12px;
            padding: 1.5rem;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .stat-card .label {
            color: var(--gray-500);
            font-size: 0.875rem;
            margin-bottom: 0.5rem;
        }

        .stat-card .value {
            font-size: 2rem;
            font-weight: 700;
            color: var(--gray-900);
        }

        .stat-card .change {
            font-size: 0.875rem;
            margin-top: 0.5rem;
        }

        .stat-card .change.positive { color: var(--success); }
        .stat-card .change.negative { color: var(--danger); }

        /* Sections */
        .section {
            background: white;
            border-radius: 12px;
            padding: 1.5rem;
            margin-bottom: 1.5rem;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }

        .section-title {
            font-size: 1.125rem;
            font-weight: 600;
            color: var(--gray-900);
            margin-bottom: 1rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        /* Table */
        .table {
            width: 100%;
            border-collapse: collapse;
        }

        .table th, .table td {
            padding: 0.875rem 1rem;
            text-align: left;
            border-bottom: 1px solid var(--gray-200);
        }

        .table th {
            color: var(--gray-500);
            font-weight: 500;
            font-size: 0.875rem;
            text-transform: uppercase;
        }

        .table tr:hover { background: var(--gray-50); }

        /* Badges */
        .status-badge {
            padding: 0.25rem 0.75rem;
            border-radius: 9999px;
            font-size: 0.75rem;
            font-weight: 600;
        }

        .status-badge.active {
            background: #D1FAE5;
            color: #065F46;
        }

        .status-badge.trial {
            background: #FEF3C7;
            color: #92400E;
        }

        .status-badge.suspended {
            background: #FEE2E2;
            color: #991B1B;
        }

        .plan-badge {
            padding: 0.25rem 0.5rem;
            border-radius: 4px;
            font-size: 0.75rem;
            font-weight: 500;
        }

        .plan-badge.starter { background: var(--gray-200); color: var(--gray-700); }
        .plan-badge.professional { background: #DBEAFE; color: #1E40AF; }
        .plan-badge.enterprise { background: #E0E7FF; color: #3730A3; }

        /* Buttons */
        .btn {
            padding: 0.5rem 1rem;
            border-radius: 6px;
            font-size: 0.875rem;
            font-weight: 500;
            cursor: pointer;
            border: none;
            transition: all 0.2s;
        }

        .btn-primary {
            background: var(--primary);
            color: white;
        }

        .btn-primary:hover { background: var(--primary-light); }

        .btn-outline {
            background: transparent;
            border: 1px solid var(--gray-300);
            color: var(--gray-700);
        }

        .btn-outline:hover {
            border-color: var(--primary);
            color: var(--primary);
        }

        /* Loading */
        .loading {
            display: flex;
            justify-content: center;
            align-items: center;
            padding: 3rem;
        }

        .spinner {
            width: 40px;
            height: 40px;
            border: 3px solid var(--gray-200);
            border-top-color: var(--primary);
            border-radius: 50%;
            animation: spin 1s linear infinite;
        }

        @keyframes spin {
            to { transform: rotate(360deg); }
        }

        /* Tabs */
        .tabs {
            display: flex;
            gap: 0.5rem;
            border-bottom: 1px solid var(--gray-200);
            margin-bottom: 1.5rem;
        }

        .tab {
            padding: 0.75rem 1rem;
            color: var(--gray-500);
            cursor: pointer;
            border-bottom: 2px solid transparent;
            transition: all 0.2s;
        }

        .tab:hover { color: var(--primary); }

        .tab.active {
            color: var(--primary);
            border-bottom-color: var(--primary);
        }
    </style>
</head>
<body>
    <div id="app">
        <!-- Header -->
        <header class="header">
            <h1>
                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <circle cx="12" cy="12" r="10"/>
                    <line x1="2" y1="12" x2="22" y2="12"/>
                    <path d="M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"/>
                </svg>
                Platform Portal
                <span class="badge">Super Admin</span>
            </h1>
            <div class="header-actions">
                <a href="/" class="back-link">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M19 12H5M12 19l-7-7 7-7"/>
                    </svg>
                    Voltar ao Dashboard
                </a>
            </div>
        </header>

        <div class="layout">
            <!-- Sidebar -->
            <nav class="sidebar">
                <div class="nav-label">Visao Geral</div>
                <a href="/platform" class="nav-item" :class="{ active: currentTab === 'overview' }" @click.prevent="currentTab = 'overview'">
                    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <rect x="3" y="3" width="7" height="9"/>
                        <rect x="14" y="3" width="7" height="5"/>
                        <rect x="14" y="12" width="7" height="9"/>
                        <rect x="3" y="16" width="7" height="5"/>
                    </svg>
                    Dashboard
                </a>

                <div class="nav-divider"></div>
                <div class="nav-label">Gestao</div>

                <a href="/platform/tenants" class="nav-item" :class="{ active: currentTab === 'tenants' }" @click.prevent="currentTab = 'tenants'">
                    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"/>
                        <polyline points="9 22 9 12 15 12 15 22"/>
                    </svg>
                    Tenants
                </a>

                <a href="/platform/modules" class="nav-item" :class="{ active: currentTab === 'modules' }" @click.prevent="currentTab = 'modules'">
                    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M14.7 6.3a1 1 0 0 0 0 1.4l1.6 1.6a1 1 0 0 0 1.4 0l3.77-3.77a6 6 0 0 1-7.94 7.94l-6.91 6.91a2.12 2.12 0 0 1-3-3l6.91-6.91a6 6 0 0 1 7.94-7.94l-3.76 3.76z"/>
                    </svg>
                    Modulos
                </a>

                <div class="nav-divider"></div>
                <div class="nav-label">Configuracoes</div>

                <a href="/platform/billing" class="nav-item" :class="{ active: currentTab === 'billing' }" @click.prevent="currentTab = 'billing'">
                    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <rect x="1" y="4" width="22" height="16" rx="2" ry="2"/>
                        <line x1="1" y1="10" x2="23" y2="10"/>
                    </svg>
                    Billing
                </a>

                <a href="/platform/settings" class="nav-item" :class="{ active: currentTab === 'settings' }" @click.prevent="currentTab = 'settings'">
                    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <circle cx="12" cy="12" r="3"/>
                        <path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z"/>
                    </svg>
                    Configuracoes
                </a>
            </nav>

            <!-- Main Content -->
            <main class="main">
                <!-- Overview Tab -->
                <div v-if="currentTab === 'overview'">
                    <h2 class="page-title">Visao Geral da Plataforma</h2>
                    <p class="page-subtitle">Metricas consolidadas de todos os tenants</p>

                    <div v-if="loading" class="loading">
                        <div class="spinner"></div>
                    </div>

                    <template v-else>
                        <div class="stats-grid">
                            <div class="stat-card">
                                <div class="label">Tenants Ativos</div>
                                <div class="value">{{ overview.tenants?.active || 0 }}</div>
                                <div class="change positive">+{{ overview.tenants?.total - overview.tenants?.active || 0 }} inativos</div>
                            </div>
                            <div class="stat-card">
                                <div class="label">Total de Projetos</div>
                                <div class="value">{{ overview.projects?.total || 0 }}</div>
                            </div>
                            <div class="stat-card">
                                <div class="label">Stories Criadas</div>
                                <div class="value">{{ overview.stories?.total || 0 }}</div>
                                <div class="change positive">{{ overview.stories?.done || 0 }} concluidas</div>
                            </div>
                            <div class="stat-card">
                                <div class="label">Jobs Processados</div>
                                <div class="value">{{ overview.jobs?.total || 0 }}</div>
                                <div class="change" :class="overview.jobs?.success_rate > 80 ? 'positive' : 'negative'">
                                    {{ overview.jobs?.success_rate || 0 }}% sucesso
                                </div>
                            </div>
                        </div>

                        <div class="section">
                            <div class="section-title">
                                Atividade Recente
                            </div>
                            <p>{{ overview.activity?.jobs_last_7_days || 0 }} jobs nos ultimos 7 dias</p>
                            <p>{{ overview.activity?.stories_last_7_days || 0 }} stories criadas nos ultimos 7 dias</p>
                        </div>
                    </template>
                </div>

                <!-- Tenants Tab -->
                <div v-if="currentTab === 'tenants'">
                    <h2 class="page-title">Gestao de Tenants</h2>
                    <p class="page-subtitle">Gerenciar organizacoes na plataforma</p>

                    <div class="section">
                        <div class="section-title">
                            <span>Tenants ({{ tenants.length }})</span>
                            <button class="btn btn-primary" @click="showCreateTenant = true">
                                + Novo Tenant
                            </button>
                        </div>

                        <table class="table">
                            <thead>
                                <tr>
                                    <th>Nome</th>
                                    <th>Slug</th>
                                    <th>Plano</th>
                                    <th>Status</th>
                                    <th>Projetos</th>
                                    <th>Stories</th>
                                    <th>Acoes</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr v-for="tenant in tenants" :key="tenant.tenant_id">
                                    <td><strong>{{ tenant.name }}</strong></td>
                                    <td><code>{{ tenant.slug }}</code></td>
                                    <td>
                                        <span class="plan-badge" :class="tenant.plan">{{ tenant.plan }}</span>
                                    </td>
                                    <td>
                                        <span class="status-badge" :class="tenant.status">{{ tenant.status }}</span>
                                    </td>
                                    <td>{{ tenant.stats?.projects || 0 }}</td>
                                    <td>{{ tenant.stats?.stories || 0 }}</td>
                                    <td>
                                        <button class="btn btn-outline" @click="viewTenant(tenant)">Ver</button>
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>

                <!-- Modules Tab -->
                <div v-if="currentTab === 'modules'">
                    <h2 class="page-title">Configuracao de Modulos</h2>
                    <p class="page-subtitle">Ativar/desativar features da plataforma</p>

                    <div class="section">
                        <div v-for="module in modules" :key="module.name" style="padding: 1rem 0; border-bottom: 1px solid var(--gray-200);">
                            <div style="display: flex; justify-content: space-between; align-items: center;">
                                <div>
                                    <strong>{{ module.label }}</strong>
                                    <p style="color: var(--gray-500); font-size: 0.875rem;">{{ module.description }}</p>
                                    <p style="font-size: 0.75rem; color: var(--gray-400);">Planos: {{ module.plans.join(', ') }}</p>
                                </div>
                                <label style="display: flex; align-items: center; gap: 0.5rem;">
                                    <input type="checkbox" :checked="module.enabled">
                                    <span>{{ module.enabled ? 'Ativo' : 'Inativo' }}</span>
                                </label>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Billing Tab -->
                <div v-if="currentTab === 'billing'">
                    <h2 class="page-title">Billing Global</h2>
                    <p class="page-subtitle">Visao financeira da plataforma</p>

                    <div class="stats-grid">
                        <div class="stat-card">
                            <div class="label">MRR (Receita Mensal)</div>
                            <div class="value">${{ billing.mrr || 0 }}</div>
                        </div>
                        <div class="stat-card">
                            <div class="label">ARR (Receita Anual)</div>
                            <div class="value">${{ billing.arr || 0 }}</div>
                        </div>
                    </div>

                    <div class="section">
                        <div class="section-title">Tenants por Plano</div>
                        <div v-for="(count, plan) in billing.tenants_by_plan" :key="plan" style="margin: 0.5rem 0;">
                            <span class="plan-badge" :class="plan">{{ plan }}</span>: {{ count }} tenants
                        </div>
                    </div>
                </div>

                <!-- Settings Tab -->
                <div v-if="currentTab === 'settings'">
                    <h2 class="page-title">Configuracoes da Plataforma</h2>
                    <p class="page-subtitle">Configuracoes globais do sistema</p>

                    <div class="section">
                        <div class="section-title">Configuracoes Gerais</div>
                        <div style="display: grid; gap: 1rem;">
                            <div>
                                <label style="display: block; margin-bottom: 0.25rem; font-weight: 500;">Plano Padrao</label>
                                <select style="padding: 0.5rem; border: 1px solid var(--gray-300); border-radius: 6px; width: 200px;">
                                    <option value="starter">Starter</option>
                                    <option value="professional" selected>Professional</option>
                                    <option value="enterprise">Enterprise</option>
                                </select>
                            </div>
                            <div>
                                <label style="display: block; margin-bottom: 0.25rem; font-weight: 500;">Dias de Trial</label>
                                <input type="number" value="14" style="padding: 0.5rem; border: 1px solid var(--gray-300); border-radius: 6px; width: 100px;">
                            </div>
                            <div>
                                <label style="display: flex; align-items: center; gap: 0.5rem;">
                                    <input type="checkbox" checked>
                                    <span>Permitir auto-registro</span>
                                </label>
                            </div>
                            <div>
                                <label style="display: flex; align-items: center; gap: 0.5rem;">
                                    <input type="checkbox">
                                    <span>Modo manutencao</span>
                                </label>
                            </div>
                        </div>
                        <div style="margin-top: 1.5rem;">
                            <button class="btn btn-primary">Salvar Configuracoes</button>
                        </div>
                    </div>
                </div>
            </main>
        </div>
    </div>

    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script>
        const { createApp, ref, onMounted } = Vue;

        createApp({
            setup() {
                const currentTab = ref('overview');
                const loading = ref(true);
                const overview = ref({});
                const tenants = ref([]);
                const modules = ref([]);
                const billing = ref({});
                const showCreateTenant = ref(false);

                async function loadOverview() {
                    try {
                        const res = await fetch('/api/platform/overview');
                        const data = await res.json();
                        if (data.success) {
                            overview.value = data.overview;
                        }
                    } catch (e) {
                        console.error('Error loading overview:', e);
                    }
                }

                async function loadTenants() {
                    try {
                        const res = await fetch('/api/platform/tenants');
                        const data = await res.json();
                        if (data.success) {
                            tenants.value = data.tenants;
                        }
                    } catch (e) {
                        console.error('Error loading tenants:', e);
                    }
                }

                async function loadModules() {
                    try {
                        const res = await fetch('/api/platform/modules');
                        const data = await res.json();
                        if (data.success) {
                            modules.value = data.modules;
                        }
                    } catch (e) {
                        console.error('Error loading modules:', e);
                    }
                }

                async function loadBilling() {
                    try {
                        const res = await fetch('/api/platform/billing/overview');
                        const data = await res.json();
                        if (data.success) {
                            billing.value = data.billing;
                        }
                    } catch (e) {
                        console.error('Error loading billing:', e);
                    }
                }

                function viewTenant(tenant) {
                    alert('Ver detalhes do tenant: ' + tenant.name);
                }

                onMounted(async () => {
                    await Promise.all([
                        loadOverview(),
                        loadTenants(),
                        loadModules(),
                        loadBilling()
                    ]);
                    loading.value = false;
                });

                return {
                    currentTab,
                    loading,
                    overview,
                    tenants,
                    modules,
                    billing,
                    showCreateTenant,
                    viewTenant
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
"""

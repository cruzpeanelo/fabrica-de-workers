# -*- coding: utf-8 -*-
"""
Login Page - Multi-Tenant com White Label
==========================================

Pagina de login que:
- Autentica via JWT
- Redireciona para tenant correto do usuario
- Suporta white label dinamico
- Mostra seletor se usuario tem multiplos tenants

Author: Fabrica de Agentes - Terminal 4
"""

from fastapi import APIRouter
from fastapi.responses import HTMLResponse

router = APIRouter()

LOGIN_PAGE_HTML = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Login - Fabrica de Agentes</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">
    <style>
        :root {
            --primary-color: #003B4A;
            --secondary-color: #FF6C00;
            --accent-color: #10B981;
            --background-color: #F8FAFC;
            --text-color: #1E293B;
            --error-color: #EF4444;
            --success-color: #10B981;
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
            background: linear-gradient(135deg, var(--primary-color) 0%, #005A6E 100%);
            min-height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 20px;
        }

        .login-container {
            background: white;
            border-radius: 16px;
            box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);
            width: 100%;
            max-width: 440px;
            overflow: hidden;
        }

        .login-header {
            background: var(--primary-color);
            padding: 32px;
            text-align: center;
        }

        .login-logo {
            width: 180px;
            height: 50px;
            object-fit: contain;
            margin-bottom: 12px;
        }

        .login-title {
            color: white;
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 4px;
        }

        .login-subtitle {
            color: rgba(255, 255, 255, 0.8);
            font-size: 14px;
        }

        .login-form {
            padding: 32px;
        }

        .form-group {
            margin-bottom: 20px;
        }

        .form-label {
            display: block;
            font-size: 14px;
            font-weight: 500;
            color: var(--text-color);
            margin-bottom: 8px;
        }

        .form-input {
            width: 100%;
            padding: 12px 16px;
            font-size: 15px;
            border: 2px solid #E2E8F0;
            border-radius: 8px;
            outline: none;
            transition: all 0.2s;
        }

        .form-input:focus {
            border-color: var(--primary-color);
            box-shadow: 0 0 0 3px rgba(0, 59, 74, 0.1);
        }

        .form-input.error {
            border-color: var(--error-color);
        }

        .btn-login {
            width: 100%;
            padding: 14px;
            font-size: 16px;
            font-weight: 600;
            color: white;
            background: var(--primary-color);
            border: none;
            border-radius: 8px;
            cursor: pointer;
            transition: all 0.2s;
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 8px;
        }

        .btn-login:hover {
            background: #004D5E;
            transform: translateY(-1px);
        }

        .btn-login:disabled {
            background: #94A3B8;
            cursor: not-allowed;
            transform: none;
        }

        .btn-login .spinner {
            width: 20px;
            height: 20px;
            border: 2px solid rgba(255, 255, 255, 0.3);
            border-top-color: white;
            border-radius: 50%;
            animation: spin 0.8s linear infinite;
        }

        @keyframes spin {
            to { transform: rotate(360deg); }
        }

        .error-message {
            background: #FEF2F2;
            border: 1px solid #FECACA;
            color: var(--error-color);
            padding: 12px 16px;
            border-radius: 8px;
            font-size: 14px;
            margin-bottom: 20px;
            display: none;
        }

        .error-message.show {
            display: block;
        }

        /* Tenant Selector Modal */
        .tenant-selector {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0, 0, 0, 0.5);
            align-items: center;
            justify-content: center;
            z-index: 1000;
        }

        .tenant-selector.show {
            display: flex;
        }

        .tenant-modal {
            background: white;
            border-radius: 16px;
            padding: 32px;
            width: 100%;
            max-width: 400px;
            margin: 20px;
        }

        .tenant-modal h3 {
            font-size: 20px;
            margin-bottom: 8px;
            color: var(--text-color);
        }

        .tenant-modal p {
            color: #64748B;
            font-size: 14px;
            margin-bottom: 24px;
        }

        .tenant-list {
            display: flex;
            flex-direction: column;
            gap: 12px;
        }

        .tenant-option {
            display: flex;
            align-items: center;
            gap: 16px;
            padding: 16px;
            border: 2px solid #E2E8F0;
            border-radius: 12px;
            cursor: pointer;
            transition: all 0.2s;
        }

        .tenant-option:hover {
            border-color: var(--primary-color);
            background: #F8FAFC;
        }

        .tenant-color {
            width: 48px;
            height: 48px;
            border-radius: 8px;
            flex-shrink: 0;
        }

        .tenant-info {
            flex: 1;
        }

        .tenant-name {
            font-weight: 600;
            color: var(--text-color);
            margin-bottom: 2px;
        }

        .tenant-plan {
            font-size: 12px;
            color: #64748B;
            text-transform: uppercase;
        }

        /* Demo Credentials */
        .demo-section {
            border-top: 1px solid #E2E8F0;
            padding: 24px 32px;
            background: #F8FAFC;
        }

        .demo-title {
            font-size: 12px;
            font-weight: 600;
            color: #64748B;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 16px;
        }

        .demo-grid {
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 8px;
        }

        .demo-credential {
            padding: 10px 12px;
            background: white;
            border: 1px solid #E2E8F0;
            border-radius: 8px;
            cursor: pointer;
            transition: all 0.2s;
        }

        .demo-credential:hover {
            border-color: var(--secondary-color);
            background: #FFF7ED;
        }

        .demo-user {
            font-size: 13px;
            font-weight: 600;
            color: var(--text-color);
        }

        .demo-role {
            font-size: 11px;
            color: #64748B;
        }

        .demo-tenant {
            display: inline-block;
            font-size: 10px;
            padding: 2px 6px;
            border-radius: 4px;
            margin-top: 4px;
        }

        .tenant-belgo { background: #003B4A20; color: #003B4A; }
        .tenant-tech { background: #6366F120; color: #6366F1; }
        .tenant-startup { background: #10B98120; color: #10B981; }
        .tenant-multi { background: #F59E0B20; color: #F59E0B; }

        /* Responsive */
        @media (max-width: 480px) {
            .demo-grid {
                grid-template-columns: 1fr;
            }
        }
    </style>
</head>
<body>
    <div id="app">
        <div class="login-container">
            <div class="login-header">
                <img :src="branding.logo_url || '/static/logos/default.png'"
                     alt="Logo"
                     class="login-logo"
                     onerror="this.style.display='none'">
                <h1 class="login-title">{{ branding.display_name || 'Fabrica de Agentes' }}</h1>
                <p class="login-subtitle">{{ branding.tagline || 'Sistema de Desenvolvimento Autonomo' }}</p>
            </div>

            <form class="login-form" @submit.prevent="handleLogin">
                <div class="error-message" :class="{ show: errorMessage }">
                    {{ errorMessage }}
                </div>

                <div class="form-group">
                    <label class="form-label">Usuario</label>
                    <input type="text"
                           class="form-input"
                           :class="{ error: errorMessage }"
                           v-model="username"
                           placeholder="Digite seu usuario"
                           autocomplete="username"
                           required>
                </div>

                <div class="form-group">
                    <label class="form-label">Senha</label>
                    <input type="password"
                           class="form-input"
                           :class="{ error: errorMessage }"
                           v-model="password"
                           placeholder="Digite sua senha"
                           autocomplete="current-password"
                           required>
                </div>

                <button type="submit" class="btn-login" :disabled="loading">
                    <span v-if="loading" class="spinner"></span>
                    <span v-else>Entrar</span>
                </button>
            </form>

            <div class="demo-section">
                <div class="demo-title">Credenciais de Demonstracao</div>
                <div class="demo-grid">
                    <div class="demo-credential" @click="fillCredentials('platform_admin', 'admin123')">
                        <div class="demo-user">platform_admin</div>
                        <div class="demo-role">Super Admin</div>
                        <span class="demo-tenant tenant-multi">Todos</span>
                    </div>
                    <div class="demo-credential" @click="fillCredentials('belgo_admin', 'belgo123')">
                        <div class="demo-user">belgo_admin</div>
                        <div class="demo-role">Admin</div>
                        <span class="demo-tenant tenant-belgo">Belgo</span>
                    </div>
                    <div class="demo-credential" @click="fillCredentials('tech_admin', 'tech123')">
                        <div class="demo-user">tech_admin</div>
                        <div class="demo-role">Admin</div>
                        <span class="demo-tenant tenant-tech">TechCorp</span>
                    </div>
                    <div class="demo-credential" @click="fillCredentials('startup_dev', 'startup123')">
                        <div class="demo-user">startup_dev</div>
                        <div class="demo-role">Developer</div>
                        <span class="demo-tenant tenant-startup">StartupX</span>
                    </div>
                    <div class="demo-credential" @click="fillCredentials('consultor', 'consul123')">
                        <div class="demo-user">consultor</div>
                        <div class="demo-role">Developer</div>
                        <span class="demo-tenant tenant-multi">Multi-tenant</span>
                    </div>
                    <div class="demo-credential" @click="fillCredentials('belgo_pm', 'belgo123')">
                        <div class="demo-user">belgo_pm</div>
                        <div class="demo-role">Project Manager</div>
                        <span class="demo-tenant tenant-belgo">Belgo</span>
                    </div>
                </div>
            </div>
        </div>

        <!-- Tenant Selector Modal -->
        <div class="tenant-selector" :class="{ show: showTenantSelector }">
            <div class="tenant-modal">
                <h3>Selecione a Organizacao</h3>
                <p>Voce tem acesso a multiplas organizacoes. Escolha qual deseja acessar:</p>
                <div class="tenant-list">
                    <div class="tenant-option"
                         v-for="tenant in userTenants"
                         :key="tenant.tenant_id"
                         @click="selectTenant(tenant)">
                        <div class="tenant-color" :style="{ background: tenant.branding?.primary_color || '#003B4A' }"></div>
                        <div class="tenant-info">
                            <div class="tenant-name">{{ tenant.name }}</div>
                            <div class="tenant-plan">{{ tenant.plan }}</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script>
        const { createApp, ref, reactive, onMounted } = Vue;

        createApp({
            setup() {
                const username = ref('');
                const password = ref('');
                const loading = ref(false);
                const errorMessage = ref('');
                const showTenantSelector = ref(false);
                const userTenants = ref([]);
                const authToken = ref('');
                const userData = ref(null);

                const branding = reactive({
                    display_name: 'Fabrica de Agentes',
                    tagline: 'Sistema de Desenvolvimento Autonomo',
                    logo_url: '/static/logos/default.png',
                    primary_color: '#003B4A',
                    secondary_color: '#FF6C00'
                });

                // Load tenant branding from URL param
                onMounted(async () => {
                    const urlParams = new URLSearchParams(window.location.search);
                    const tenantSlug = urlParams.get('tenant');

                    if (tenantSlug) {
                        try {
                            const response = await fetch(`/api/tenant/slug/${tenantSlug}/branding`);
                            if (response.ok) {
                                const data = await response.json();
                                Object.assign(branding, data);
                                applyBranding(data);
                            }
                        } catch (e) {
                            console.log('Using default branding');
                        }
                    }

                    // Check if already logged in
                    const token = localStorage.getItem('auth_token');
                    if (token) {
                        try {
                            const response = await fetch('/api/user/me', {
                                headers: { 'Authorization': `Bearer ${token}` }
                            });
                            if (response.ok) {
                                const currentTenant = localStorage.getItem('current_tenant');
                                if (currentTenant) {
                                    window.location.href = '/';
                                }
                            }
                        } catch (e) {
                            localStorage.removeItem('auth_token');
                        }
                    }
                });

                function applyBranding(b) {
                    document.documentElement.style.setProperty('--primary-color', b.primary_color || '#003B4A');
                    document.documentElement.style.setProperty('--secondary-color', b.secondary_color || '#FF6C00');
                }

                function fillCredentials(user, pass) {
                    username.value = user;
                    password.value = pass;
                    errorMessage.value = '';
                }

                async function handleLogin() {
                    if (!username.value || !password.value) {
                        errorMessage.value = 'Preencha usuario e senha';
                        return;
                    }

                    loading.value = true;
                    errorMessage.value = '';

                    try {
                        // Call login endpoint
                        const response = await fetch('/api/v1/auth/login', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({
                                username: username.value,
                                password: password.value
                            })
                        });

                        const data = await response.json();

                        if (!response.ok) {
                            errorMessage.value = data.detail || 'Usuario ou senha invalidos';
                            return;
                        }

                        // Store token
                        authToken.value = data.access_token;
                        userData.value = data.user;
                        localStorage.setItem('auth_token', data.access_token);
                        localStorage.setItem('user_data', JSON.stringify(data.user));

                        // Get user's tenants
                        const tenantsResponse = await fetch('/api/user/tenants', {
                            headers: { 'Authorization': `Bearer ${data.access_token}` }
                        });

                        if (tenantsResponse.ok) {
                            const tenantsData = await tenantsResponse.json();
                            userTenants.value = tenantsData.tenants || [];

                            // Redirect based on number of tenants
                            if (data.user.role === 'SUPER_ADMIN') {
                                // Super admin goes to platform portal
                                localStorage.setItem('current_tenant', userTenants.value[0]?.tenant_id || '');
                                window.location.href = '/platform';
                            } else if (userTenants.value.length === 1) {
                                // Single tenant - go directly
                                const tenant = userTenants.value[0];
                                localStorage.setItem('current_tenant', tenant.tenant_id);
                                window.location.href = '/';
                            } else if (userTenants.value.length > 1) {
                                // Multiple tenants - show selector
                                showTenantSelector.value = true;
                            } else {
                                errorMessage.value = 'Usuario nao pertence a nenhuma organizacao';
                            }
                        }

                    } catch (error) {
                        console.error('Login error:', error);
                        errorMessage.value = 'Erro ao conectar ao servidor';
                    } finally {
                        loading.value = false;
                    }
                }

                function selectTenant(tenant) {
                    localStorage.setItem('current_tenant', tenant.tenant_id);

                    // Load tenant branding and redirect
                    applyBranding(tenant.branding || {});

                    // Check if user is admin of this tenant
                    if (userData.value?.role === 'ADMIN') {
                        window.location.href = '/tenant-admin';
                    } else {
                        window.location.href = '/';
                    }
                }

                return {
                    username,
                    password,
                    loading,
                    errorMessage,
                    showTenantSelector,
                    userTenants,
                    branding,
                    fillCredentials,
                    handleLogin,
                    selectTenant
                };
            }
        }).mount('#app');
    </script>
</body>
</html>
"""


@router.get("/login", response_class=HTMLResponse)
async def login_page():
    """Render login page"""
    return HTMLResponse(content=LOGIN_PAGE_HTML)


def register_login_routes(app):
    """Register login routes"""
    app.include_router(router, tags=["Login"])
    print("[Login] Routes registered: /login")

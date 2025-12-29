# -*- coding: utf-8 -*-
"""
SSO UI Components - Login Modal and Configuration Panel
========================================================
Vue.js components for SSO authentication interface.
"""

# SSO Login Modal HTML Template
SSO_LOGIN_MODAL_HTML = """
<!-- MODAL: SSO Login -->
<div v-if="showSSOModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
     @click.self="showSSOModal = false">
    <div class="bg-white rounded-lg w-[400px] shadow-xl dark:bg-gray-800">
        <div class="p-4 border-b border-gray-200 flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
            <h2 class="text-lg font-semibold">Login SSO</h2>
            <button @click="showSSOModal = false" class="text-white/70 hover:text-white">
                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                </svg>
            </button>
        </div>
        <div class="p-6 space-y-4">
            <div v-if="ssoStatus.loading" class="text-center py-8">
                <div class="animate-spin w-8 h-8 border-4 border-blue-500 border-t-transparent rounded-full mx-auto"></div>
                <p class="mt-2 text-gray-500">Verificando provedores SSO...</p>
            </div>
            <div v-else-if="!ssoStatus.enabled" class="text-center py-8">
                <svg class="w-16 h-16 text-gray-300 mx-auto mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"/>
                </svg>
                <p class="text-gray-500">SSO nao configurado</p>
                <p class="text-sm text-gray-400 mt-1">Configure as variaveis de ambiente para habilitar SSO</p>
            </div>
            <div v-else class="space-y-4">
                <p class="text-sm text-gray-600 dark:text-gray-300 text-center mb-6">
                    Escolha um provedor de identidade para fazer login:
                </p>
                <!-- Azure AD Login Button -->
                <button v-if="ssoStatus.providers?.azure_ad?.enabled"
                        @click="loginWithAzure"
                        class="w-full flex items-center justify-center gap-3 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition dark:border-gray-600 dark:hover:bg-gray-700">
                    <svg class="w-6 h-6" viewBox="0 0 21 21" fill="none">
                        <rect width="10" height="10" fill="#F25022"/>
                        <rect x="11" width="10" height="10" fill="#7FBA00"/>
                        <rect y="11" width="10" height="10" fill="#00A4EF"/>
                        <rect x="11" y="11" width="10" height="10" fill="#FFB900"/>
                    </svg>
                    <span class="font-medium text-gray-700 dark:text-gray-200">Login com Microsoft Azure</span>
                </button>
                <!-- SAML SSO Login Button -->
                <button v-if="ssoStatus.providers?.saml?.enabled"
                        @click="loginWithSAML"
                        class="w-full flex items-center justify-center gap-3 px-4 py-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition dark:border-gray-600 dark:hover:bg-gray-700">
                    <svg class="w-6 h-6 text-[#003B4A]" fill="currentColor" viewBox="0 0 24 24">
                        <path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/>
                    </svg>
                    <span class="font-medium text-gray-700 dark:text-gray-200">Login com SSO Corporativo</span>
                </button>
                <div class="relative my-4">
                    <div class="absolute inset-0 flex items-center">
                        <div class="w-full border-t border-gray-300 dark:border-gray-600"></div>
                    </div>
                    <div class="relative flex justify-center text-sm">
                        <span class="px-2 bg-white text-gray-500 dark:bg-gray-800 dark:text-gray-400">ou</span>
                    </div>
                </div>
                <p class="text-xs text-gray-500 text-center">
                    Continue com login local usando usuario e senha
                </p>
            </div>
        </div>
    </div>
</div>
"""

# SSO Configuration Modal HTML Template
SSO_CONFIG_MODAL_HTML = """
<!-- MODAL: SSO Configuration (Admin) -->
<div v-if="showSSOConfigModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
     @click.self="showSSOConfigModal = false">
    <div class="bg-white rounded-lg w-[600px] shadow-xl dark:bg-gray-800 max-h-[90vh] overflow-y-auto">
        <div class="p-4 border-b border-gray-200 flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg sticky top-0">
            <h2 class="text-lg font-semibold">Configuracao SSO</h2>
            <button @click="showSSOConfigModal = false" class="text-white/70 hover:text-white">
                <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                </svg>
            </button>
        </div>
        <div class="p-6 space-y-6">
            <!-- Status Overview -->
            <div class="bg-gray-50 dark:bg-gray-700 rounded-lg p-4">
                <h3 class="font-medium text-gray-700 dark:text-gray-200 mb-3">Status dos Provedores</h3>
                <div class="grid grid-cols-2 gap-4">
                    <div class="flex items-center gap-3">
                        <span :class="['w-3 h-3 rounded-full', ssoConfig.azure_ad?.enabled ? 'bg-green-500' : 'bg-gray-300']"></span>
                        <span class="text-sm text-gray-600 dark:text-gray-300">Azure AD</span>
                        <span class="text-xs px-2 py-0.5 rounded" :class="ssoConfig.azure_ad?.enabled ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-500'">
                            {{ ssoConfig.azure_ad?.enabled ? 'Ativo' : 'Inativo' }}
                        </span>
                    </div>
                    <div class="flex items-center gap-3">
                        <span :class="['w-3 h-3 rounded-full', ssoConfig.saml?.enabled ? 'bg-green-500' : 'bg-gray-300']"></span>
                        <span class="text-sm text-gray-600 dark:text-gray-300">SAML 2.0</span>
                        <span class="text-xs px-2 py-0.5 rounded" :class="ssoConfig.saml?.enabled ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-500'">
                            {{ ssoConfig.saml?.enabled ? 'Ativo' : 'Inativo' }}
                        </span>
                    </div>
                </div>
            </div>
            <!-- Azure AD Configuration -->
            <div class="border border-gray-200 dark:border-gray-600 rounded-lg">
                <div class="p-3 bg-gray-50 dark:bg-gray-700 border-b border-gray-200 dark:border-gray-600 flex items-center gap-2">
                    <svg class="w-5 h-5" viewBox="0 0 21 21" fill="none">
                        <rect width="10" height="10" fill="#F25022"/>
                        <rect x="11" width="10" height="10" fill="#7FBA00"/>
                        <rect y="11" width="10" height="10" fill="#00A4EF"/>
                        <rect x="11" y="11" width="10" height="10" fill="#FFB900"/>
                    </svg>
                    <h4 class="font-medium text-gray-700 dark:text-gray-200">Azure AD OAuth2</h4>
                </div>
                <div class="p-4 space-y-3">
                    <div class="grid grid-cols-2 gap-3">
                        <div>
                            <label class="block text-xs text-gray-500 mb-1">Client ID</label>
                            <input type="text" :value="ssoConfig.azure_ad?.client_id || ''" readonly
                                   class="w-full text-sm bg-gray-100 dark:bg-gray-600 border-0 rounded px-2 py-1.5">
                        </div>
                        <div>
                            <label class="block text-xs text-gray-500 mb-1">Tenant ID</label>
                            <input type="text" :value="ssoConfig.azure_ad?.tenant_id || ''" readonly
                                   class="w-full text-sm bg-gray-100 dark:bg-gray-600 border-0 rounded px-2 py-1.5">
                        </div>
                    </div>
                    <div>
                        <label class="block text-xs text-gray-500 mb-1">Redirect URI</label>
                        <input type="text" :value="ssoConfig.azure_ad?.redirect_uri || ''" readonly
                               class="w-full text-sm bg-gray-100 dark:bg-gray-600 border-0 rounded px-2 py-1.5">
                    </div>
                    <button @click="testSSOConnection('azure_ad')"
                            :disabled="!ssoConfig.azure_ad?.enabled"
                            class="text-sm px-3 py-1.5 bg-blue-500 text-white rounded hover:bg-blue-600 disabled:opacity-50 disabled:cursor-not-allowed">
                        Testar Conexao
                    </button>
                </div>
            </div>
            <!-- SAML Configuration -->
            <div class="border border-gray-200 dark:border-gray-600 rounded-lg">
                <div class="p-3 bg-gray-50 dark:bg-gray-700 border-b border-gray-200 dark:border-gray-600 flex items-center gap-2">
                    <svg class="w-5 h-5 text-[#003B4A]" fill="currentColor" viewBox="0 0 24 24">
                        <path d="M12 1L3 5v6c0 5.55 3.84 10.74 9 12 5.16-1.26 9-6.45 9-12V5l-9-4z"/>
                    </svg>
                    <h4 class="font-medium text-gray-700 dark:text-gray-200">SAML 2.0</h4>
                </div>
                <div class="p-4 space-y-3">
                    <div>
                        <label class="block text-xs text-gray-500 mb-1">IdP URL</label>
                        <input type="text" :value="ssoConfig.saml?.idp_url || ''" readonly
                               class="w-full text-sm bg-gray-100 dark:bg-gray-600 border-0 rounded px-2 py-1.5">
                    </div>
                    <div>
                        <label class="block text-xs text-gray-500 mb-1">SP Entity ID</label>
                        <input type="text" :value="ssoConfig.saml?.sp_entity_id || ''" readonly
                               class="w-full text-sm bg-gray-100 dark:bg-gray-600 border-0 rounded px-2 py-1.5">
                    </div>
                    <div class="flex gap-2">
                        <button @click="testSSOConnection('saml')"
                                :disabled="!ssoConfig.saml?.enabled"
                                class="text-sm px-3 py-1.5 bg-blue-500 text-white rounded hover:bg-blue-600 disabled:opacity-50 disabled:cursor-not-allowed">
                            Testar Conexao
                        </button>
                        <a v-if="ssoConfig.saml?.enabled" href="/api/auth/saml/metadata" target="_blank"
                           class="text-sm px-3 py-1.5 border border-gray-300 text-gray-700 rounded hover:bg-gray-50 inline-flex items-center gap-1">
                            Baixar Metadata SP
                        </a>
                    </div>
                </div>
            </div>
            <!-- General Settings -->
            <div class="border border-gray-200 dark:border-gray-600 rounded-lg">
                <div class="p-3 bg-gray-50 dark:bg-gray-700 border-b border-gray-200 dark:border-gray-600">
                    <h4 class="font-medium text-gray-700 dark:text-gray-200">Configuracoes Gerais</h4>
                </div>
                <div class="p-4 space-y-3">
                    <div class="flex items-center justify-between">
                        <span class="text-sm text-gray-600 dark:text-gray-300">Auto-provisionar usuarios</span>
                        <span class="text-sm" :class="ssoConfig.auto_provision ? 'text-green-600' : 'text-gray-500'">
                            {{ ssoConfig.auto_provision ? 'Sim' : 'Nao' }}
                        </span>
                    </div>
                    <div class="flex items-center justify-between">
                        <span class="text-sm text-gray-600 dark:text-gray-300">Atualizar ao fazer login</span>
                        <span class="text-sm" :class="ssoConfig.update_on_login ? 'text-green-600' : 'text-gray-500'">
                            {{ ssoConfig.update_on_login ? 'Sim' : 'Nao' }}
                        </span>
                    </div>
                    <div class="flex items-center justify-between">
                        <span class="text-sm text-gray-600 dark:text-gray-300">Role padrao</span>
                        <span class="text-sm font-mono bg-gray-100 dark:bg-gray-600 px-2 py-0.5 rounded">
                            {{ ssoConfig.default_role || 'VIEWER' }}
                        </span>
                    </div>
                </div>
            </div>
            <p class="text-xs text-gray-500 text-center">
                Configure as variaveis de ambiente e reinicie o servidor para aplicar alteracoes
            </p>
        </div>
    </div>
</div>
"""

# SSO Vue.js State and Methods
SSO_VUE_STATE = """
// SSO State
const showSSOModal = ref(false);
const showSSOConfigModal = ref(false);
const ssoStatus = ref({ loading: true, enabled: false, providers: {} });
const ssoConfig = ref({});
"""

SSO_VUE_METHODS = """
// SSO Methods
const loadSSOStatus = async () => {
    try {
        ssoStatus.value.loading = true;
        const response = await fetch('/api/auth/sso/status');
        if (response.ok) {
            const data = await response.json();
            ssoStatus.value = { loading: false, ...data };
        }
    } catch (error) {
        console.error('Failed to load SSO status:', error);
        ssoStatus.value = { loading: false, enabled: false, providers: {} };
    }
};

const loadSSOConfig = async () => {
    try {
        const response = await fetch('/api/auth/sso/config');
        if (response.ok) {
            ssoConfig.value = await response.json();
        }
    } catch (error) {
        console.error('Failed to load SSO config:', error);
    }
};

const loginWithAzure = () => {
    window.location.href = '/api/auth/azure/login?redirect_url=' + encodeURIComponent(window.location.pathname);
};

const loginWithSAML = () => {
    window.location.href = '/api/auth/saml/login?redirect_url=' + encodeURIComponent(window.location.pathname);
};

const testSSOConnection = async (provider) => {
    try {
        const response = await fetch(`/api/auth/sso/test-connection?provider=${provider}`, {
            method: 'POST'
        });
        const result = await response.json();
        if (result.success) {
            addToast('success', `Conexao ${provider.toUpperCase()} OK`, result.message);
        } else {
            addToast('error', `Erro ${provider.toUpperCase()}`, result.error);
        }
    } catch (error) {
        addToast('error', 'Erro', 'Falha ao testar conexao');
    }
};

const openSSOLogin = () => {
    loadSSOStatus();
    showSSOModal.value = true;
};

const openSSOConfig = () => {
    loadSSOConfig();
    showSSOConfigModal.value = true;
};
"""

# SSO Sidebar Section HTML
SSO_SIDEBAR_HTML = """
<!-- SSO Section -->
<div class="mb-6">
    <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Autenticacao</h3>
    <button @click="openSSOLogin"
            class="w-full p-2 rounded text-left text-sm hover:bg-gray-100 flex items-center gap-2">
        <svg class="w-4 h-4 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 7a2 2 0 012 2m4 0a6 6 0 01-7.743 5.743L11 17H9v2H7v2H4a1 1 0 01-1-1v-2.586a1 1 0 01.293-.707l5.964-5.964A6 6 0 1121 9z"/>
        </svg>
        <span>Login SSO</span>
    </button>
    <button @click="openSSOConfig"
            class="w-full p-2 rounded text-left text-sm hover:bg-gray-100 flex items-center gap-2">
        <svg class="w-4 h-4 text-gray-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"/>
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
        </svg>
        <span>Config SSO</span>
    </button>
</div>
"""

def get_sso_ui_components():
    """Return all SSO UI components as a dictionary"""
    return {
        "login_modal": SSO_LOGIN_MODAL_HTML,
        "config_modal": SSO_CONFIG_MODAL_HTML,
        "vue_state": SSO_VUE_STATE,
        "vue_methods": SSO_VUE_METHODS,
        "sidebar": SSO_SIDEBAR_HTML
    }

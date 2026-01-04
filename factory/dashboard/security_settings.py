# -*- coding: utf-8 -*-
"""
Security Settings Panel - Issue #156
Plataforma E v6.5

Provides Security Settings panel with:
1. Two-Factor Authentication (MFA) management
2. API Keys management
3. Active Sessions management
"""
from datetime import datetime
from typing import Optional, List, Dict, Any
from fastapi import FastAPI, HTTPException, Request, Depends
from pydantic import BaseModel

# =============================================================================
# PYDANTIC SCHEMAS
# =============================================================================

class MFASetupRequest(BaseModel):
    user_id: int = 1  # Default user for demo


class MFAVerifyRequest(BaseModel):
    user_id: int = 1
    code: str


class MFADisableRequest(BaseModel):
    user_id: int = 1
    verification_code: Optional[str] = None


class RegenerateBackupCodesRequest(BaseModel):
    user_id: int = 1
    verification_code: str


class APIKeyCreateRequest(BaseModel):
    name: str
    description: Optional[str] = None
    tier: str = "free"
    scopes: List[str] = ["read"]


class SessionRevokeRequest(BaseModel):
    session_id: str


# =============================================================================
# REGISTER SECURITY ENDPOINTS
# =============================================================================

def register_security_endpoints(app: FastAPI):
    """Register security settings API endpoints"""

    # -------------------------------------------------------------------------
    # Security Data Endpoint - Issue #309 fix: Added error handling
    # -------------------------------------------------------------------------
    @app.get("/api/security/data")
    def get_security_data(user_id: int = 1):
        """Get all security data for user - Issue #309 fix: Added error handling"""
        # Initialize with safe defaults to prevent timeout
        mfa_status = None
        api_keys = []
        sessions = []

        # Get MFA status with error handling (Issue #309 fix)
        try:
            from factory.auth.mfa import MFAService
            mfa_service = MFAService()
            mfa_status = mfa_service.get_status(user_id)
        except Exception as e:
            print(f"[Security] Error loading MFA status: {e}")
            mfa_status = {"enabled": False, "status": "error", "error": str(e)}

        # Get API keys with error handling
        try:
            from factory.api.api_key_auth import list_api_keys
            api_keys = list_api_keys(user_id=user_id)
        except Exception as e:
            print(f"[Security] Error loading API keys: {e}")
            api_keys = []

        # Get sessions with error handling
        try:
            sessions = _get_user_sessions(user_id)
        except Exception as e:
            print(f"[Security] Error loading sessions: {e}")
            sessions = []

        return {
            "mfa": mfa_status,
            "apiKeys": api_keys,
            "sessions": sessions
        }

    # -------------------------------------------------------------------------
    # MFA Endpoints
    # -------------------------------------------------------------------------
    @app.post("/api/security/mfa/setup")
    def start_mfa_setup(request: MFASetupRequest):
        """Start MFA setup process"""
        from factory.auth.mfa import MFAService

        mfa_service = MFAService()
        try:
            setup_data = mfa_service.start_setup(request.user_id)
            return {
                "success": True,
                "qr_code_base64": setup_data.qr_code_base64,
                "secret_key": setup_data.secret_key,
                "backup_codes": setup_data.backup_codes
            }
        except Exception as e:
            raise HTTPException(500, f"Error starting MFA setup: {str(e)}")

    @app.post("/api/security/mfa/verify")
    def verify_mfa_code(request: MFAVerifyRequest):
        """Complete MFA setup by verifying code"""
        from factory.auth.mfa import MFAService

        mfa_service = MFAService()
        result = mfa_service.complete_setup(request.user_id, request.code)

        if not result.success:
            raise HTTPException(400, result.message)

        return {"success": True, "message": result.message}

    @app.post("/api/security/mfa/disable")
    def disable_mfa(request: MFADisableRequest):
        """Disable MFA for user"""
        from factory.auth.mfa import MFAService

        mfa_service = MFAService()
        result = mfa_service.disable(request.user_id, request.verification_code)

        if not result.success:
            raise HTTPException(400, result.message)

        return {"success": True, "message": result.message}

    @app.post("/api/security/mfa/backup-codes/regenerate")
    def regenerate_backup_codes(request: RegenerateBackupCodesRequest):
        """Regenerate backup codes"""
        from factory.auth.mfa import MFAService

        mfa_service = MFAService()
        result = mfa_service.regenerate_backup_codes(
            request.user_id,
            request.verification_code
        )

        if not result.get("success"):
            raise HTTPException(400, result.get("message", "Error regenerating codes"))

        return result

    # -------------------------------------------------------------------------
    # API Keys Endpoints
    # -------------------------------------------------------------------------
    @app.post("/api/security/api-keys")
    def create_api_key(request: APIKeyCreateRequest, user_id: int = 1):
        """Create new API key"""
        from factory.api.api_key_auth import create_api_key as create_key

        try:
            result = create_key(
                name=request.name,
                description=request.description,
                tier=request.tier,
                scopes=request.scopes,
                user_id=user_id
            )
            return {
                "success": True,
                "api_key": result["api_key"],
                "key_id": result["key_id"],
                "name": result["name"],
                "tier": result["tier"]
            }
        except Exception as e:
            raise HTTPException(500, f"Error creating API key: {str(e)}")

    @app.delete("/api/security/api-keys/{key_id}")
    def revoke_api_key(key_id: str, user_id: int = 1):
        """Revoke an API key"""
        from factory.api.api_key_auth import revoke_api_key as revoke_key

        success = revoke_key(key_id, user_id)
        if not success:
            raise HTTPException(404, "API key not found or already revoked")

        return {"success": True, "message": "API key revoked"}

    # -------------------------------------------------------------------------
    # Sessions Endpoints
    # -------------------------------------------------------------------------
    @app.get("/api/security/sessions")
    def get_sessions(user_id: int = 1):
        """Get active sessions for user"""
        return {"sessions": _get_user_sessions(user_id)}

    @app.delete("/api/security/sessions/{session_id}")
    def revoke_session(session_id: str, user_id: int = 1):
        """Revoke a specific session"""
        # TODO: Implement actual session revocation
        return {"success": True, "message": f"Session {session_id} revoked"}

    @app.post("/api/security/sessions/revoke-all")
    def revoke_all_sessions(user_id: int = 1):
        """Revoke all sessions except current"""
        # TODO: Implement actual session revocation
        return {"success": True, "message": "All other sessions revoked"}

    print("[Dashboard] Security Settings endpoints loaded")


def _get_user_sessions(user_id: int) -> List[Dict[str, Any]]:
    """Get user sessions (mock implementation)"""
    # TODO: Implement actual session tracking
    # For now, return mock data
    return [
        {
            "session_id": "sess_current",
            "device_name": "Windows Desktop",
            "device_type": "desktop",
            "browser": "Chrome 120",
            "ip_address": "127.0.0.1",
            "last_active": datetime.utcnow().isoformat(),
            "is_current": True
        }
    ]


# =============================================================================
# HTML/JS FOR SECURITY SETTINGS PANEL
# =============================================================================

SECURITY_SETTINGS_MODAL_HTML = '''
        <!-- MODAL: Security Settings (Issue #156) -->
        <div v-if="showSecuritySettings" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showSecuritySettings = false">
            <div class="bg-white rounded-lg w-[95vw] max-w-[900px] max-h-[90vh] shadow-xl overflow-hidden dark:bg-gray-800">
                <div class="p-4 border-b flex justify-between items-center bg-[#003B4A] text-white rounded-t-lg">
                    <div>
                        <h2 class="text-lg font-semibold">Configuracoes de Seguranca</h2>
                        <p class="text-sm text-blue-200">MFA, API Keys e Sessoes Ativas</p>
                    </div>
                    <button @click="showSecuritySettings = false" class="text-white/70 hover:text-white text-xl">&times;</button>
                </div>
                <div class="p-6 overflow-y-auto" style="max-height: calc(90vh - 80px);">
                    <!-- Security Tabs -->
                    <div class="flex border-b border-gray-200 mb-6">
                        <button @click="securityActiveTab = 'mfa'"
                                :class="['px-4 py-2 text-sm font-medium border-b-2 -mb-px', securityActiveTab === 'mfa' ? 'border-[#003B4A] text-[#003B4A]' : 'border-transparent text-gray-500 hover:text-gray-700']">
                            Autenticacao MFA
                        </button>
                        <button @click="securityActiveTab = 'apikeys'"
                                :class="['px-4 py-2 text-sm font-medium border-b-2 -mb-px', securityActiveTab === 'apikeys' ? 'border-[#003B4A] text-[#003B4A]' : 'border-transparent text-gray-500 hover:text-gray-700']">
                            API Keys
                        </button>
                        <button @click="securityActiveTab = 'sessions'"
                                :class="['px-4 py-2 text-sm font-medium border-b-2 -mb-px', securityActiveTab === 'sessions' ? 'border-[#003B4A] text-[#003B4A]' : 'border-transparent text-gray-500 hover:text-gray-700']">
                            Sessoes Ativas
                        </button>
                    </div>

                    <!-- MFA Tab -->
                    <div v-if="securityActiveTab === 'mfa'" class="space-y-6">
                        <div class="bg-gray-50 rounded-lg p-4">
                            <div class="flex items-center justify-between mb-4">
                                <div>
                                    <h3 class="font-semibold text-gray-800">Autenticacao de Dois Fatores (2FA)</h3>
                                    <p class="text-sm text-gray-500">Adicione uma camada extra de seguranca a sua conta</p>
                                </div>
                                <div :class="['px-3 py-1 rounded-full text-sm font-medium', securityData.mfa?.enabled ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-600']">
                                    {{ securityData.mfa?.enabled ? 'Ativado' : 'Desativado' }}
                                </div>
                            </div>

                            <div v-if="!securityData.mfa?.enabled && !mfaSetupMode">
                                <button @click="startMfaSetup" class="px-4 py-2 bg-[#003B4A] text-white rounded hover:bg-[#004d5c] transition">
                                    Ativar 2FA
                                </button>
                            </div>

                            <div v-if="mfaSetupMode" class="space-y-4">
                                <div class="bg-white rounded-lg p-4 border border-gray-200">
                                    <h4 class="font-medium mb-2">1. Escaneie o QR Code</h4>
                                    <p class="text-sm text-gray-600 mb-3">Use um aplicativo autenticador para escanear:</p>
                                    <div class="flex justify-center mb-4">
                                        <img v-if="mfaSetupData.qr_code_base64" :src="'data:image/png;base64,' + mfaSetupData.qr_code_base64" alt="QR Code" class="w-48 h-48 border rounded">
                                        <div v-else class="w-48 h-48 bg-gray-100 rounded flex items-center justify-center"><div class="spinner"></div></div>
                                    </div>
                                    <p class="text-xs text-gray-500 text-center">Ou insira manualmente: <code class="bg-gray-100 px-1 rounded">{{ mfaSetupData.secret_key }}</code></p>
                                </div>
                                <div class="bg-white rounded-lg p-4 border border-gray-200">
                                    <h4 class="font-medium mb-2">2. Salve os codigos de backup</h4>
                                    <div class="bg-gray-50 rounded p-3 font-mono text-sm grid grid-cols-2 gap-2">
                                        <span v-for="(code, idx) in mfaSetupData.backup_codes" :key="idx" class="text-center">{{ code }}</span>
                                    </div>
                                    <button @click="copyBackupCodes" class="mt-2 text-sm text-blue-600 hover:underline">Copiar codigos</button>
                                </div>
                                <div class="bg-white rounded-lg p-4 border border-gray-200">
                                    <h4 class="font-medium mb-2">3. Verifique o codigo</h4>
                                    <div class="flex gap-2">
                                        <input v-model="mfaVerifyCode" type="text" maxlength="6" placeholder="000000"
                                               class="flex-1 px-3 py-2 border rounded text-center text-lg tracking-widest font-mono">
                                        <button @click="completeMfaSetup" :disabled="mfaVerifyCode.length !== 6"
                                                class="px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700 disabled:opacity-50">Verificar</button>
                                    </div>
                                    <p v-if="mfaError" class="mt-2 text-sm text-red-600">{{ mfaError }}</p>
                                </div>
                                <button @click="cancelMfaSetup" class="text-sm text-gray-500 hover:underline">Cancelar</button>
                            </div>

                            <div v-if="securityData.mfa?.enabled && !mfaSetupMode" class="space-y-4">
                                <div class="text-sm text-gray-600">
                                    <p>Ativado em: {{ formatDate(securityData.mfa.enabled_at) }}</p>
                                    <p>Ultimo uso: {{ formatDate(securityData.mfa.last_used_at) || 'Nunca' }}</p>
                                    <p>Codigos de backup restantes: {{ securityData.mfa.backup_codes_remaining || 0 }}</p>
                                </div>
                                <div class="flex gap-2">
                                    <button @click="regenerateBackupCodes" class="px-3 py-1.5 text-sm border border-gray-300 rounded hover:bg-gray-50">Regenerar Codigos</button>
                                    <button @click="disableMfa" class="px-3 py-1.5 text-sm text-red-600 border border-red-300 rounded hover:bg-red-50">Desativar 2FA</button>
                                </div>
                            </div>
                        </div>
                    </div>

                    <!-- API Keys Tab -->
                    <div v-if="securityActiveTab === 'apikeys'" class="space-y-6">
                        <div class="flex justify-between items-center">
                            <div>
                                <h3 class="font-semibold text-gray-800">API Keys</h3>
                                <p class="text-sm text-gray-500">Gerencie suas chaves de API</p>
                            </div>
                            <button @click="showCreateApiKeyModal = true" class="px-4 py-2 bg-[#003B4A] text-white rounded hover:bg-[#004d5c] flex items-center gap-2">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4"/></svg>
                                Nova API Key
                            </button>
                        </div>
                        <div v-if="securityData.apiKeys?.length" class="space-y-3">
                            <div v-for="key in securityData.apiKeys" :key="key.key_id" class="bg-gray-50 rounded-lg p-4 border border-gray-200">
                                <div class="flex items-start justify-between">
                                    <div class="flex-1">
                                        <div class="flex items-center gap-2 mb-1">
                                            <span class="font-medium">{{ key.name }}</span>
                                            <span :class="['px-2 py-0.5 rounded text-xs', key.status === 'active' ? 'bg-green-100 text-green-700' : 'bg-red-100 text-red-700']">{{ key.status }}</span>
                                            <span class="px-2 py-0.5 bg-blue-100 text-blue-700 rounded text-xs">{{ key.tier }}</span>
                                        </div>
                                        <div class="text-sm text-gray-500 font-mono">{{ key.key_prefix }}</div>
                                        <div class="mt-2 text-xs text-gray-500">
                                            Criada: {{ formatDate(key.created_at) }} | Ultimo uso: {{ formatDate(key.last_used_at) || 'Nunca' }} | {{ key.requests_total }} req
                                        </div>
                                    </div>
                                    <button v-if="key.status === 'active'" @click="revokeApiKey(key.key_id)" class="px-3 py-1.5 text-sm text-red-600 border border-red-300 rounded hover:bg-red-50">Revogar</button>
                                </div>
                            </div>
                        </div>
                        <div v-else class="text-center py-8 text-gray-500">
                            <svg class="w-12 h-12 mx-auto mb-3 text-gray-300" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 7a2 2 0 012 2m4 0a6 6 0 01-7.743 5.743L11 17H9v2H7v2H4a1 1 0 01-1-1v-2.586a1 1 0 01.293-.707l5.964-5.964A6 6 0 1121 9z"/></svg>
                            <p>Nenhuma API Key criada</p>
                        </div>
                    </div>

                    <!-- Sessions Tab -->
                    <div v-if="securityActiveTab === 'sessions'" class="space-y-6">
                        <div class="flex justify-between items-center">
                            <div>
                                <h3 class="font-semibold text-gray-800">Sessoes Ativas</h3>
                                <p class="text-sm text-gray-500">Dispositivos conectados</p>
                            </div>
                            <button @click="revokeAllSessions" class="px-4 py-2 text-red-600 border border-red-300 rounded hover:bg-red-50">Encerrar Todas</button>
                        </div>
                        <div v-if="securityData.sessions?.length" class="space-y-3">
                            <div v-for="session in securityData.sessions" :key="session.session_id" :class="['rounded-lg p-4 border', session.is_current ? 'bg-green-50 border-green-200' : 'bg-gray-50 border-gray-200']">
                                <div class="flex items-start justify-between">
                                    <div class="flex-1">
                                        <div class="flex items-center gap-2 mb-1">
                                            <span class="font-medium">{{ session.device_name || 'Dispositivo Desconhecido' }}</span>
                                            <span v-if="session.is_current" class="px-2 py-0.5 bg-green-200 text-green-800 rounded text-xs">Sessao Atual</span>
                                        </div>
                                        <div class="text-sm text-gray-500">
                                            <p>IP: {{ session.ip_address }} | {{ session.browser || 'Desconhecido' }}</p>
                                            <p>Ultimo acesso: {{ formatDate(session.last_active) }}</p>
                                        </div>
                                    </div>
                                    <button v-if="!session.is_current" @click="revokeSession(session.session_id)" class="px-3 py-1.5 text-sm text-red-600 border border-red-300 rounded hover:bg-red-50">Encerrar</button>
                                </div>
                            </div>
                        </div>
                        <div v-else class="text-center py-8 text-gray-500"><p>Nenhuma sessao ativa</p></div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Create API Key Modal -->
        <div v-if="showCreateApiKeyModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showCreateApiKeyModal = false">
            <div class="bg-white rounded-lg w-full max-w-md shadow-xl">
                <div class="p-4 border-b"><h3 class="text-lg font-semibold">Nova API Key</h3></div>
                <div class="p-4 space-y-4">
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Nome</label>
                        <input v-model="newApiKey.name" type="text" placeholder="Ex: Producao App" class="w-full px-3 py-2 border rounded">
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Descricao</label>
                        <textarea v-model="newApiKey.description" class="w-full px-3 py-2 border rounded" rows="2"></textarea>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Tier</label>
                        <select v-model="newApiKey.tier" class="w-full px-3 py-2 border rounded">
                            <option value="free">Free</option><option value="basic">Basic</option><option value="pro">Pro</option><option value="enterprise">Enterprise</option>
                        </select>
                    </div>
                    <div>
                        <label class="block text-sm font-medium text-gray-700 mb-1">Scopes</label>
                        <div class="flex flex-wrap gap-2">
                            <label v-for="scope in ['read', 'write', 'admin', 'webhooks']" :key="scope" class="flex items-center gap-1 px-2 py-1 border rounded cursor-pointer">
                                <input type="checkbox" :value="scope" v-model="newApiKey.scopes" class="rounded"><span class="text-sm">{{ scope }}</span>
                            </label>
                        </div>
                    </div>
                </div>
                <div class="p-4 border-t flex justify-end gap-2">
                    <button @click="showCreateApiKeyModal = false" class="px-4 py-2 border rounded">Cancelar</button>
                    <button @click="createApiKey" :disabled="!newApiKey.name" class="px-4 py-2 bg-[#003B4A] text-white rounded disabled:opacity-50">Criar</button>
                </div>
            </div>
        </div>

        <!-- Show Created API Key Modal -->
        <div v-if="createdApiKey" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
            <div class="bg-white rounded-lg w-full max-w-md shadow-xl">
                <div class="p-4 border-b bg-green-50"><h3 class="text-lg font-semibold text-green-800">API Key Criada!</h3></div>
                <div class="p-4">
                    <div class="bg-yellow-50 border border-yellow-200 rounded p-3 mb-4">
                        <p class="text-sm text-yellow-800 font-medium">Esta e a unica vez que a chave sera exibida!</p>
                    </div>
                    <div class="bg-gray-50 rounded p-3 font-mono text-sm break-all border">{{ createdApiKey.api_key }}</div>
                    <button @click="copyCreatedApiKey" class="mt-2 text-sm text-blue-600 hover:underline">Copiar</button>
                </div>
                <div class="p-4 border-t flex justify-end">
                    <button @click="createdApiKey = null; loadSecurityData()" class="px-4 py-2 bg-[#003B4A] text-white rounded">Fechar</button>
                </div>
            </div>
        </div>
'''

SECURITY_SETTINGS_SIDEBAR_BUTTON = '''
                    <!-- Settings (Issue #156) -->
                    <div class="mt-6 pt-4 border-t border-gray-200">
                        <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Configuracoes</h3>
                        <button @click="showSecuritySettings = true; loadSecurityData()"
                                class="w-full text-left px-3 py-2 text-sm text-gray-600 hover:bg-gray-100 rounded flex items-center gap-2">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"/>
                            </svg>
                            Seguranca
                        </button>
                    </div>
'''

SECURITY_SETTINGS_VUE_DATA = '''
            // Security Settings (Issue #156)
            const showSecuritySettings = ref(false);
            const securityActiveTab = ref('mfa');
            const securityData = ref({ mfa: null, apiKeys: [], sessions: [] });
            const mfaSetupMode = ref(false);
            const mfaSetupData = ref({ qr_code_base64: '', secret_key: '', backup_codes: [] });
            const mfaVerifyCode = ref('');
            const mfaError = ref('');
            const showCreateApiKeyModal = ref(false);
            const createdApiKey = ref(null);
            const newApiKey = ref({ name: '', description: '', tier: 'free', scopes: ['read'] });
'''

SECURITY_SETTINGS_VUE_METHODS = '''
            // Security Settings Methods (Issue #156)
            const loadSecurityData = async () => {
                try {
                    const res = await fetch('/api/security/data');
                    if (res.ok) {
                        securityData.value = await res.json();
                    }
                } catch (e) { console.error('Error loading security data:', e); }
            };

            const startMfaSetup = async () => {
                try {
                    const res = await fetch('/api/security/mfa/setup', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1 })
                    });
                    if (res.ok) {
                        mfaSetupData.value = await res.json();
                        mfaSetupMode.value = true;
                        mfaError.value = '';
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao iniciar configuracao MFA'); }
            };

            const completeMfaSetup = async () => {
                try {
                    const res = await fetch('/api/security/mfa/verify', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1, code: mfaVerifyCode.value })
                    });
                    if (res.ok) {
                        mfaSetupMode.value = false;
                        mfaVerifyCode.value = '';
                        await loadSecurityData();
                        addToast('success', 'MFA Ativado', 'Autenticacao de dois fatores ativada com sucesso');
                    } else {
                        const data = await res.json();
                        mfaError.value = data.detail || 'Codigo invalido';
                    }
                } catch (e) { mfaError.value = 'Erro ao verificar codigo'; }
            };

            const cancelMfaSetup = () => {
                mfaSetupMode.value = false;
                mfaSetupData.value = { qr_code_base64: '', secret_key: '', backup_codes: [] };
                mfaVerifyCode.value = '';
                mfaError.value = '';
            };

            const disableMfa = async () => {
                if (!confirm('Tem certeza que deseja desativar o 2FA?')) return;
                try {
                    const res = await fetch('/api/security/mfa/disable', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1 })
                    });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'MFA Desativado', '2FA foi desativado');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao desativar MFA'); }
            };

            const regenerateBackupCodes = async () => {
                const code = prompt('Digite o codigo do seu autenticador:');
                if (!code) return;
                try {
                    const res = await fetch('/api/security/mfa/backup-codes/regenerate', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ user_id: 1, verification_code: code })
                    });
                    if (res.ok) {
                        const data = await res.json();
                        alert('Novos codigos de backup:\\n\\n' + data.backup_codes.join('\\n'));
                        await loadSecurityData();
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao regenerar codigos'); }
            };

            const copyBackupCodes = () => {
                const codes = mfaSetupData.value.backup_codes.join('\\n');
                navigator.clipboard.writeText(codes);
                addToast('success', 'Copiado', 'Codigos copiados para a area de transferencia');
            };

            const createApiKey = async () => {
                try {
                    const res = await fetch('/api/security/api-keys', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(newApiKey.value)
                    });
                    if (res.ok) {
                        createdApiKey.value = await res.json();
                        showCreateApiKeyModal.value = false;
                        newApiKey.value = { name: '', description: '', tier: 'free', scopes: ['read'] };
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao criar API Key'); }
            };

            const revokeApiKey = async (keyId) => {
                if (!confirm('Tem certeza que deseja revogar esta API Key?')) return;
                try {
                    const res = await fetch(`/api/security/api-keys/${keyId}`, { method: 'DELETE' });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'Revogada', 'API Key revogada com sucesso');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao revogar API Key'); }
            };

            const copyApiKey = (keyId) => {
                navigator.clipboard.writeText(keyId);
                addToast('success', 'Copiado', 'ID da API Key copiado');
            };

            const copyCreatedApiKey = () => {
                navigator.clipboard.writeText(createdApiKey.value.api_key);
                addToast('success', 'Copiado', 'API Key copiada para a area de transferencia');
            };

            const revokeSession = async (sessionId) => {
                try {
                    const res = await fetch(`/api/security/sessions/${sessionId}`, { method: 'DELETE' });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'Encerrada', 'Sessao encerrada');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao encerrar sessao'); }
            };

            const revokeAllSessions = async () => {
                if (!confirm('Encerrar todas as outras sessoes?')) return;
                try {
                    const res = await fetch('/api/security/sessions/revoke-all', { method: 'POST' });
                    if (res.ok) {
                        await loadSecurityData();
                        addToast('success', 'Encerradas', 'Todas as outras sessoes foram encerradas');
                    }
                } catch (e) { addToast('error', 'Erro', 'Falha ao encerrar sessoes'); }
            };
'''

SECURITY_SETTINGS_VUE_RETURN = '''
                // Security Settings (Issue #156)
                showSecuritySettings, securityActiveTab, securityData,
                mfaSetupMode, mfaSetupData, mfaVerifyCode, mfaError,
                showCreateApiKeyModal, createdApiKey, newApiKey,
                loadSecurityData, startMfaSetup, completeMfaSetup, cancelMfaSetup,
                disableMfa, regenerateBackupCodes, copyBackupCodes,
                createApiKey, revokeApiKey, copyApiKey, copyCreatedApiKey,
                revokeSession, revokeAllSessions,
'''

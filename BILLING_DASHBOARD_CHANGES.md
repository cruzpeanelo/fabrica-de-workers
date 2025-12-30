# Issue #159 - Billing Dashboard Changes for app_v6_agile.py

This document contains all the code changes needed to expose the billing dashboard in main navigation.

## Change 1: Register Billing Endpoints (around line 165)

Add after the Admin Portal registration:

```python
# Billing Panel (Issue #159)
try:
    from factory.dashboard.billing_panel import register_billing_endpoints
    register_billing_endpoints(app)
    print("[Dashboard] Billing Panel loaded")
except ImportError as e:
    print(f"[Dashboard] Billing Panel not available: {e}")
```

## Change 2: Add Vue State Variables (around line 6280, after isPullingToRefresh)

Add after `const isPullingToRefresh = ref(false);`:

```javascript
// User Role State (Issue #159 - Billing Dashboard)
const currentUserRole = ref('ADMIN'); // Default to ADMIN for demo
const isAdmin = computed(() => currentUserRole.value === 'ADMIN');

// Billing Modal State (Issue #159)
const showBillingModal = ref(false);
const billingData = ref({ usage: null, invoices: [], loading: false, error: null });
```

## Change 3: Add Billing Menu Item in Sidebar (after Analytics section)

Find the Analytics (Issue #65) section in sidebar and add after its closing `</div>`:

```html
<!-- Billing Dashboard (Issue #159) - Admin Only -->
<div class="mt-6 pt-4 border-t border-gray-200" v-if="isAdmin">
    <h3 class="text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2">Billing</h3>
    <button @click="showBillingModal = true; loadBillingData()"
            class="w-full text-left px-3 py-2 text-sm text-green-600 hover:bg-green-50 rounded flex items-center gap-2">
        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
        </svg>
        Billing e Usage
    </button>
    <div class="mt-2 px-3 py-2 bg-gray-50 rounded text-xs" v-if="billingData.usage">
        <div class="flex justify-between mb-1">
            <span class="text-gray-500">Custo Atual</span>
            <span class="font-medium text-gray-700">{{ billingData.usage.costs?.total_formatted || 'R$ 0,00' }}</span>
        </div>
        <div class="flex justify-between">
            <span class="text-gray-500">API Calls</span>
            <span class="font-medium text-gray-700">{{ billingData.usage.metrics?.api_calls?.toLocaleString() || '0' }}</span>
        </div>
    </div>
</div>
```

## Change 4: Add Billing Modal (after Analytics Modal)

Add after the Analytics modal closing `</div>` (the one that ends the showAnalyticsModal block):

```html
<!-- MODAL: Billing Dashboard (Issue #159) - Admin Only -->
<div v-if="showBillingModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" @click.self="showBillingModal = false">
    <div class="bg-white rounded-lg w-[95vw] max-w-[1200px] max-h-[90vh] shadow-xl overflow-hidden dark:bg-gray-800">
        <div class="p-4 border-b flex justify-between items-center bg-gradient-to-r from-green-600 to-green-700 text-white rounded-t-lg">
            <div>
                <h2 class="text-lg font-semibold">Billing e Usage</h2>
                <p class="text-sm text-green-200">Consumo, custos e faturas</p>
            </div>
            <div class="flex items-center gap-4">
                <a href="/api/billing/dashboard" target="_blank" class="px-3 py-1 bg-white/10 hover:bg-white/20 rounded text-sm transition">
                    Abrir Painel Completo
                </a>
                <button @click="showBillingModal = false" class="text-white/70 hover:text-white text-xl">X</button>
            </div>
        </div>
        <div class="p-6 overflow-y-auto" style="max-height: calc(90vh - 80px);">
            <div v-if="billingData.loading" class="flex items-center justify-center py-12">
                <div class="spinner"></div>
                <span class="ml-3">Carregando dados de billing...</span>
            </div>
            <div v-else-if="billingData.error" class="text-center py-12 text-red-500">
                <p>{{ billingData.error }}</p>
                <button @click="loadBillingData()" class="mt-4 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600">Tentar Novamente</button>
            </div>
            <div v-else>
                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                    <div class="bg-gradient-to-br from-green-500 to-green-600 rounded-xl p-4 text-white">
                        <div class="text-sm opacity-80 mb-1">Custo Total do Periodo</div>
                        <div class="text-2xl font-bold">{{ billingData.usage?.costs?.total_formatted || 'R$ 0,00' }}</div>
                    </div>
                    <div class="bg-white rounded-xl p-4 border border-gray-200">
                        <div class="text-sm text-gray-500 mb-1">Chamadas de API</div>
                        <div class="text-2xl font-bold text-gray-800">{{ billingData.usage?.metrics?.api_calls?.toLocaleString() || '0' }}</div>
                    </div>
                    <div class="bg-white rounded-xl p-4 border border-gray-200">
                        <div class="text-sm text-gray-500 mb-1">Tokens Utilizados</div>
                        <div class="text-2xl font-bold text-gray-800">{{ formatTokens(billingData.usage?.metrics?.llm_tokens_total) }}</div>
                    </div>
                    <div class="bg-white rounded-xl p-4 border border-gray-200">
                        <div class="text-sm text-gray-500 mb-1">Armazenamento</div>
                        <div class="text-2xl font-bold text-gray-800">{{ billingData.usage?.metrics?.storage_mb?.toFixed(1) || '0' }} MB</div>
                    </div>
                </div>
                <div class="mb-6">
                    <h3 class="font-semibold mb-4">Distribuicao de Custos</h3>
                    <div class="bg-gray-50 rounded-lg p-4">
                        <div class="space-y-3">
                            <div class="flex items-center justify-between"><div class="flex items-center gap-2"><div class="w-3 h-3 bg-blue-500 rounded-full"></div><span class="text-sm">API Calls</span></div><span class="font-medium">R$ {{ ((billingData.usage?.costs?.api_cents || 0) / 100).toFixed(2) }}</span></div>
                            <div class="flex items-center justify-between"><div class="flex items-center gap-2"><div class="w-3 h-3 bg-purple-500 rounded-full"></div><span class="text-sm">LLM Tokens</span></div><span class="font-medium">R$ {{ ((billingData.usage?.costs?.llm_cents || 0) / 100).toFixed(2) }}</span></div>
                            <div class="flex items-center justify-between"><div class="flex items-center gap-2"><div class="w-3 h-3 bg-orange-500 rounded-full"></div><span class="text-sm">Storage</span></div><span class="font-medium">R$ {{ ((billingData.usage?.costs?.storage_cents || 0) / 100).toFixed(2) }}</span></div>
                            <div class="flex items-center justify-between"><div class="flex items-center gap-2"><div class="w-3 h-3 bg-green-500 rounded-full"></div><span class="text-sm">Compute</span></div><span class="font-medium">R$ {{ ((billingData.usage?.costs?.compute_cents || 0) / 100).toFixed(2) }}</span></div>
                        </div>
                    </div>
                </div>
                <div>
                    <h3 class="font-semibold mb-4">Faturas Recentes</h3>
                    <div class="bg-white border border-gray-200 rounded-lg overflow-hidden">
                        <table class="w-full">
                            <thead class="bg-gray-50"><tr><th class="px-4 py-3 text-left text-xs font-semibold text-gray-500 uppercase">Fatura</th><th class="px-4 py-3 text-left text-xs font-semibold text-gray-500 uppercase">Periodo</th><th class="px-4 py-3 text-left text-xs font-semibold text-gray-500 uppercase">Valor</th><th class="px-4 py-3 text-left text-xs font-semibold text-gray-500 uppercase">Status</th></tr></thead>
                            <tbody class="divide-y divide-gray-100">
                                <tr v-for="invoice in billingData.invoices" :key="invoice.invoice_id" class="hover:bg-gray-50">
                                    <td class="px-4 py-3 text-sm font-medium text-gray-900">{{ invoice.invoice_number }}</td>
                                    <td class="px-4 py-3 text-sm text-gray-600">{{ invoice.period }}</td>
                                    <td class="px-4 py-3 text-sm font-medium text-gray-900">{{ invoice.total_formatted }}</td>
                                    <td class="px-4 py-3"><span :class="['px-2 py-1 text-xs rounded-full font-medium', invoice.status === 'paid' ? 'bg-green-100 text-green-800' : invoice.status === 'pending' ? 'bg-yellow-100 text-yellow-800' : 'bg-gray-100 text-gray-800']">{{ invoice.status === 'paid' ? 'Pago' : invoice.status === 'pending' ? 'Pendente' : invoice.status }}</span></td>
                                </tr>
                                <tr v-if="!billingData.invoices?.length"><td colspan="4" class="px-4 py-8 text-center text-gray-500">Nenhuma fatura encontrada</td></tr>
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>
```

## Change 5: Add JavaScript Methods (after loadAnalytics function)

```javascript
// Load Billing Data (Issue #159)
const loadBillingData = async () => {
    billingData.value.loading = true;
    billingData.value.error = null;
    try {
        const [usageRes, invoicesRes] = await Promise.all([
            fetch('/api/billing/usage?tenant_id=default&period=month'),
            fetch('/api/billing/invoices?tenant_id=default&limit=5')
        ]);
        if (usageRes.ok) {
            billingData.value.usage = await usageRes.json();
        }
        if (invoicesRes.ok) {
            const invoicesData = await invoicesRes.json();
            billingData.value.invoices = invoicesData.invoices || [];
        }
    } catch (e) {
        console.error('Billing error:', e);
        billingData.value.error = 'Erro ao carregar dados de billing. Verifique se os endpoints estao disponiveis.';
    } finally {
        billingData.value.loading = false;
    }
};

// Format tokens for display
const formatTokens = (tokens) => {
    if (!tokens) return '0';
    if (tokens >= 1000000) return (tokens / 1000000).toFixed(2) + 'M';
    if (tokens >= 1000) return (tokens / 1000).toFixed(1) + 'K';
    return tokens.toString();
};
```

## Change 6: Add to Return Statement

Add after the Analytics exports:

```javascript
// Billing Dashboard (Issue #159)
showBillingModal, billingData, loadBillingData, formatTokens, currentUserRole, isAdmin,
```

---

## Summary

These changes add:
1. A "Billing" menu item in the sidebar (visible only to ADMIN users)
2. A billing modal with:
   - Current usage summary (total cost, API calls, tokens, storage)
   - Cost breakdown by category
   - Recent invoices list
3. Quick stats shown in the sidebar when billing data is loaded
4. Link to full billing dashboard

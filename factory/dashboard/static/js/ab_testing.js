/**
 * A/B Testing UI Component - Issue #71
 * =====================================
 *
 * Componente Vue.js para gerenciamento de testes A/B de codigo gerado.
 * Permite criar testes, comparar variantes e selecionar vencedores.
 */

// A/B Testing State
const abTestingState = {
    showABTestModal: false,
    abTests: [],
    currentABTest: null,
    abTestLoading: false,
    abTestError: null,
    newABTest: {
        title: '',
        num_variants: 3,
        approaches: ['simple', 'optimized', 'robust'],
        requirements: '',
        test_code: ''
    }
};

// A/B Testing Methods
const abTestingMethods = {
    /**
     * Carrega testes A/B de uma story
     */
    async loadABTests(storyId) {
        if (!storyId) return [];
        try {
            const res = await fetch(`/api/stories/${storyId}/ab-tests`);
            if (res.ok) {
                return await res.json();
            }
            return [];
        } catch (e) {
            console.error('Error loading A/B tests:', e);
            return [];
        }
    },

    /**
     * Cria um novo teste A/B
     */
    async createABTest(storyId, testData) {
        try {
            const res = await fetch(`/api/stories/${storyId}/ab-test`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(testData)
            });
            const data = await res.json();
            if (res.ok) {
                return { success: true, data };
            }
            return { success: false, error: data.detail || 'Erro ao criar teste A/B' };
        } catch (e) {
            return { success: false, error: e.message };
        }
    },

    /**
     * Obtem detalhes de um teste A/B
     */
    async getABTest(testId) {
        try {
            const res = await fetch(`/api/ab-tests/${testId}`);
            if (res.ok) {
                return await res.json();
            }
            return null;
        } catch (e) {
            console.error('Error getting A/B test:', e);
            return null;
        }
    },

    /**
     * Compara variantes de um teste A/B
     */
    async compareVariants(testId) {
        try {
            const res = await fetch(`/api/ab-tests/${testId}/compare`);
            if (res.ok) {
                return await res.json();
            }
            return null;
        } catch (e) {
            console.error('Error comparing variants:', e);
            return null;
        }
    },

    /**
     * Seleciona manualmente o vencedor
     */
    async selectWinner(testId, variantId, reason = '') {
        try {
            const res = await fetch(`/api/ab-tests/${testId}/select-winner`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ variant_id: variantId, reason })
            });
            if (res.ok) {
                return { success: true, data: await res.json() };
            }
            const data = await res.json();
            return { success: false, error: data.detail || 'Erro ao selecionar vencedor' };
        } catch (e) {
            return { success: false, error: e.message };
        }
    },

    /**
     * Auto-seleciona o vencedor baseado no score
     */
    async autoSelectWinner(testId) {
        try {
            const res = await fetch(`/api/ab-tests/${testId}/auto-select`, {
                method: 'POST'
            });
            if (res.ok) {
                return { success: true, data: await res.json() };
            }
            const data = await res.json();
            return { success: false, error: data.detail || 'Erro ao auto-selecionar' };
        } catch (e) {
            return { success: false, error: e.message };
        }
    },

    /**
     * Obtem codigo da variante vencedora
     */
    async getWinnerCode(testId) {
        try {
            const res = await fetch(`/api/ab-tests/${testId}/winner-code`);
            if (res.ok) {
                const data = await res.json();
                return data.code;
            }
            return null;
        } catch (e) {
            console.error('Error getting winner code:', e);
            return null;
        }
    },

    /**
     * Remove um teste A/B
     */
    async deleteABTest(testId) {
        try {
            const res = await fetch(`/api/ab-tests/${testId}`, {
                method: 'DELETE'
            });
            return res.ok;
        } catch (e) {
            console.error('Error deleting A/B test:', e);
            return false;
        }
    }
};

// A/B Testing Vue Component Template
const ABTestingModalTemplate = `
<div v-if="showABTestModal" class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center">
    <div class="bg-white rounded-lg shadow-xl w-full max-w-4xl max-h-[90vh] overflow-hidden">
        <!-- Header -->
        <div class="bg-[#003B4A] text-white px-6 py-4 flex justify-between items-center">
            <h2 class="text-lg font-semibold">Teste A/B de Codigo</h2>
            <button @click="showABTestModal = false" class="text-white/70 hover:text-white">
                <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                </svg>
            </button>
        </div>

        <!-- Content -->
        <div class="p-6 overflow-y-auto" style="max-height: calc(90vh - 120px);">
            <!-- Create New Test -->
            <div v-if="!currentABTest" class="space-y-4">
                <div class="bg-blue-50 border border-blue-200 rounded-lg p-4">
                    <h3 class="font-semibold text-blue-800 mb-2">Criar Teste A/B</h3>
                    <p class="text-sm text-blue-600 mb-4">
                        Gere multiplas variantes de implementacao para comparar diferentes abordagens.
                    </p>

                    <div class="grid grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Titulo</label>
                            <input v-model="newABTest.title" type="text"
                                   class="w-full border rounded px-3 py-2"
                                   placeholder="Nome do teste">
                        </div>
                        <div>
                            <label class="block text-sm font-medium text-gray-700 mb-1">Variantes</label>
                            <select v-model="newABTest.num_variants" class="w-full border rounded px-3 py-2">
                                <option :value="2">2 variantes</option>
                                <option :value="3">3 variantes</option>
                                <option :value="4">4 variantes</option>
                                <option :value="5">5 variantes</option>
                            </select>
                        </div>
                    </div>

                    <div class="mt-4">
                        <label class="block text-sm font-medium text-gray-700 mb-1">Requisitos</label>
                        <textarea v-model="newABTest.requirements" rows="4"
                                  class="w-full border rounded px-3 py-2"
                                  placeholder="Descreva o que deve ser implementado..."></textarea>
                    </div>

                    <div class="mt-4">
                        <label class="block text-sm font-medium text-gray-700 mb-1">Codigo de Teste (opcional)</label>
                        <textarea v-model="newABTest.test_code" rows="4"
                                  class="w-full border rounded px-3 py-2 font-mono text-sm"
                                  placeholder="def test_example(): assert True"></textarea>
                    </div>

                    <button @click="createABTest"
                            :disabled="abTestLoading || !newABTest.requirements"
                            class="mt-4 bg-[#FF6C00] hover:bg-orange-600 text-white px-6 py-2 rounded font-medium disabled:opacity-50">
                        <span v-if="abTestLoading">Gerando variantes...</span>
                        <span v-else>Criar Teste A/B</span>
                    </button>
                </div>

                <!-- Existing Tests -->
                <div v-if="abTests.length" class="mt-6">
                    <h3 class="font-semibold text-gray-800 mb-3">Testes Existentes</h3>
                    <div class="space-y-2">
                        <div v-for="test in abTests" :key="test.test_id"
                             @click="loadABTestDetails(test.test_id)"
                             class="border rounded-lg p-4 cursor-pointer hover:bg-gray-50 transition">
                            <div class="flex justify-between items-center">
                                <div>
                                    <h4 class="font-medium">{{ test.title }}</h4>
                                    <p class="text-sm text-gray-500">{{ test.test_id }}</p>
                                </div>
                                <span :class="getStatusClass(test.status)" class="px-2 py-1 rounded text-xs font-medium">
                                    {{ test.status }}
                                </span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Test Details -->
            <div v-else class="space-y-4">
                <button @click="currentABTest = null" class="text-blue-600 hover:underline text-sm">
                    &larr; Voltar
                </button>

                <div class="bg-white border rounded-lg p-4">
                    <div class="flex justify-between items-start mb-4">
                        <div>
                            <h3 class="font-semibold text-lg">{{ currentABTest.title }}</h3>
                            <p class="text-sm text-gray-500">{{ currentABTest.test_id }}</p>
                        </div>
                        <span :class="getStatusClass(currentABTest.status)" class="px-3 py-1 rounded text-sm font-medium">
                            {{ currentABTest.status }}
                        </span>
                    </div>

                    <!-- Variants Comparison -->
                    <div v-if="currentABTest.variants && currentABTest.variants.length" class="mt-4">
                        <h4 class="font-medium text-gray-800 mb-3">Variantes</h4>
                        <div class="grid gap-4" :class="currentABTest.variants.length <= 3 ? 'grid-cols-3' : 'grid-cols-2'">
                            <div v-for="variant in currentABTest.variants" :key="variant.variant_id"
                                 :class="['border rounded-lg p-4', variant.status === 'winner' ? 'border-green-500 bg-green-50' : '']">
                                <div class="flex justify-between items-center mb-2">
                                    <span class="font-semibold">{{ variant.variant_id }}</span>
                                    <span v-if="variant.status === 'winner'" class="text-green-600 text-sm font-medium">Vencedor</span>
                                </div>
                                <div class="text-sm text-gray-600 mb-2">{{ variant.approach }}</div>

                                <!-- Metrics -->
                                <div v-if="variant.metrics" class="space-y-1 text-xs">
                                    <div class="flex justify-between">
                                        <span class="text-gray-500">Linhas:</span>
                                        <span>{{ variant.metrics.code_lines || 0 }}</span>
                                    </div>
                                    <div class="flex justify-between">
                                        <span class="text-gray-500">Complexidade:</span>
                                        <span>{{ variant.metrics.cyclomatic_complexity || 0 }}</span>
                                    </div>
                                    <div class="flex justify-between">
                                        <span class="text-gray-500">Legibilidade:</span>
                                        <span>{{ variant.metrics.readability_score || 0 }}%</span>
                                    </div>
                                </div>

                                <!-- Test Results -->
                                <div v-if="variant.test_results && variant.test_results.tests_total" class="mt-2 pt-2 border-t">
                                    <div class="flex justify-between text-xs">
                                        <span class="text-gray-500">Testes:</span>
                                        <span :class="variant.test_results.pass_rate === 100 ? 'text-green-600' : 'text-orange-600'">
                                            {{ variant.test_results.tests_passed }}/{{ variant.test_results.tests_total }}
                                        </span>
                                    </div>
                                </div>

                                <!-- Score -->
                                <div class="mt-3 pt-2 border-t">
                                    <div class="flex justify-between items-center">
                                        <span class="text-sm font-medium">Score</span>
                                        <span class="text-lg font-bold" :class="getScoreClass(variant.score)">
                                            {{ (variant.score || 0).toFixed(1) }}
                                        </span>
                                    </div>
                                </div>

                                <!-- Select Winner Button -->
                                <button v-if="currentABTest.status !== 'winner_selected' && variant.status !== 'winner'"
                                        @click="selectWinner(currentABTest.test_id, variant.variant_id)"
                                        class="mt-3 w-full bg-blue-500 hover:bg-blue-600 text-white text-sm py-2 rounded">
                                    Selecionar como Vencedor
                                </button>
                            </div>
                        </div>
                    </div>

                    <!-- Recommendation -->
                    <div v-if="currentABTest.recommendation && currentABTest.recommendation.winner_id"
                         class="mt-4 bg-yellow-50 border border-yellow-200 rounded-lg p-4">
                        <h4 class="font-medium text-yellow-800 mb-2">Recomendacao da IA</h4>
                        <p class="text-sm text-yellow-700">
                            Variante <strong>{{ currentABTest.recommendation.winner_id }}</strong>
                            recomendada com {{ currentABTest.recommendation.confidence || 'media' }} confianca.
                        </p>
                        <ul class="mt-2 text-sm text-yellow-600 list-disc list-inside">
                            <li v-for="reason in (currentABTest.recommendation.reasons || [])" :key="reason">
                                {{ reason }}
                            </li>
                        </ul>
                        <button v-if="currentABTest.status !== 'winner_selected'"
                                @click="autoSelectWinner(currentABTest.test_id)"
                                class="mt-3 bg-yellow-500 hover:bg-yellow-600 text-white text-sm px-4 py-2 rounded">
                            Aceitar Recomendacao
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>
`;

// Helper functions for the component
const abTestingHelpers = {
    getStatusClass(status) {
        const classes = {
            'pending': 'bg-gray-100 text-gray-600',
            'generating': 'bg-blue-100 text-blue-600',
            'testing': 'bg-yellow-100 text-yellow-600',
            'completed': 'bg-green-100 text-green-600',
            'winner_selected': 'bg-purple-100 text-purple-600',
            'cancelled': 'bg-red-100 text-red-600'
        };
        return classes[status] || classes['pending'];
    },

    getScoreClass(score) {
        if (score >= 70) return 'text-green-600';
        if (score >= 50) return 'text-yellow-600';
        return 'text-red-600';
    }
};

// Export for use in main app
if (typeof window !== 'undefined') {
    window.ABTesting = {
        state: abTestingState,
        methods: abTestingMethods,
        template: ABTestingModalTemplate,
        helpers: abTestingHelpers
    };
}

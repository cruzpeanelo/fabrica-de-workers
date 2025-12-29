/**
 * Version History Component - Issue #58
 * =====================================
 * Componente Vue.js para visualizacao e gerenciamento do historico de versoes
 *
 * Funcionalidades:
 * - Lista de versoes com timeline visual
 * - Diff entre versoes com syntax highlighting
 * - Rollback para versoes anteriores
 * - Gerenciamento de branches
 */

// Adiciona ao Vue app global
const VersionHistoryMixin = {
    data() {
        return {
            // Version Control State
            versionHistory: [],
            versionBranches: [],
            selectedVersion: null,
            compareVersion: null,
            versionDiff: null,
            versionLoading: false,
            versionError: null,
            selectedBranch: 'all',
            showRollbackConfirm: false,
            showBranchModal: false,
            showMergeModal: false,
            newBranchName: '',
            mergeBranch: {
                source: '',
                target: 'main'
            }
        };
    },

    methods: {
        // =====================================================================
        // VERSION HISTORY METHODS
        // =====================================================================

        async loadVersionHistory() {
            if (!this.selectedStory?.story_id) return;

            this.versionLoading = true;
            this.versionError = null;

            try {
                const branch = this.selectedBranch !== 'all' ? `&branch=${this.selectedBranch}` : '';
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/versions?limit=50${branch}`);

                if (res.ok) {
                    const data = await res.json();
                    this.versionHistory = data.versions || [];
                } else {
                    this.versionError = 'Erro ao carregar historico';
                }

                // Load branches
                await this.loadVersionBranches();
            } catch (e) {
                this.versionError = 'Erro de conexao';
                console.error('[VersionHistory] Error:', e);
            } finally {
                this.versionLoading = false;
            }
        },

        async loadVersionBranches() {
            if (!this.selectedStory?.story_id) return;

            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/branches`);
                if (res.ok) {
                    const data = await res.json();
                    this.versionBranches = data.branches || [];
                }
            } catch (e) {
                console.error('[VersionHistory] Error loading branches:', e);
            }
        },

        async selectVersion(version) {
            this.selectedVersion = version;

            // Load full version details
            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/versions/${version.version_hash}`);
                if (res.ok) {
                    const data = await res.json();
                    this.selectedVersion = { ...version, ...data.version, files: data.files };
                }
            } catch (e) {
                console.error('[VersionHistory] Error loading version details:', e);
            }
        },

        async compareVersions(fromHash, toHash) {
            this.versionLoading = true;
            this.versionDiff = null;

            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/diff?from_hash=${fromHash}&to_hash=${toHash}`);

                if (res.ok) {
                    this.versionDiff = await res.json();
                } else {
                    this.addToast('error', 'Erro', 'Nao foi possivel calcular diff');
                }
            } catch (e) {
                console.error('[VersionHistory] Error comparing versions:', e);
            } finally {
                this.versionLoading = false;
            }
        },

        async viewFileDiff(filePath) {
            if (!this.selectedVersion || !this.compareVersion) return;

            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/diff/file?from_hash=${this.compareVersion.version_hash}&to_hash=${this.selectedVersion.version_hash}&file_path=${encodeURIComponent(filePath)}`);

                if (res.ok) {
                    const diff = await res.json();
                    // Show in modal or expand inline
                    this.selectedFileDiff = diff;
                }
            } catch (e) {
                console.error('[VersionHistory] Error loading file diff:', e);
            }
        },

        async previewRollback(targetHash) {
            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/rollback/preview?target_hash=${targetHash}`);

                if (res.ok) {
                    const preview = await res.json();
                    this.rollbackPreview = preview;
                    this.showRollbackConfirm = true;
                }
            } catch (e) {
                console.error('[VersionHistory] Error previewing rollback:', e);
            }
        },

        async confirmRollback() {
            if (!this.selectedVersion?.version_hash) return;

            this.versionLoading = true;

            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/rollback`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        target_hash: this.selectedVersion.version_hash,
                        create_backup: true
                    })
                });

                if (res.ok) {
                    const result = await res.json();
                    this.addToast('success', 'Rollback realizado', `${result.files_count} arquivos restaurados`);
                    this.showRollbackConfirm = false;
                    await this.loadVersionHistory();
                } else {
                    const error = await res.json();
                    this.addToast('error', 'Erro no rollback', error.detail || 'Erro desconhecido');
                }
            } catch (e) {
                console.error('[VersionHistory] Error during rollback:', e);
                this.addToast('error', 'Erro', 'Nao foi possivel fazer rollback');
            } finally {
                this.versionLoading = false;
            }
        },

        async createBranch() {
            if (!this.newBranchName.trim()) return;

            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/branch`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        branch_name: this.newBranchName,
                        from_hash: this.selectedVersion?.version_hash || null
                    })
                });

                if (res.ok) {
                    const result = await res.json();
                    this.addToast('success', 'Branch criada', this.newBranchName);
                    this.showBranchModal = false;
                    this.newBranchName = '';
                    await this.loadVersionBranches();
                } else {
                    const error = await res.json();
                    this.addToast('error', 'Erro', error.detail || 'Nao foi possivel criar branch');
                }
            } catch (e) {
                console.error('[VersionHistory] Error creating branch:', e);
            }
        },

        async mergeBranches() {
            if (!this.mergeBranch.source) return;

            try {
                const res = await fetch(`/api/stories/${this.selectedStory.story_id}/merge`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(this.mergeBranch)
                });

                if (res.ok) {
                    const result = await res.json();
                    this.addToast('success', 'Merge realizado', `${result.source_branch} -> ${result.target_branch}`);
                    this.showMergeModal = false;
                    await this.loadVersionHistory();
                } else {
                    const error = await res.json();
                    this.addToast('error', 'Erro no merge', error.detail || 'Conflitos nao resolvidos');
                }
            } catch (e) {
                console.error('[VersionHistory] Error merging branches:', e);
            }
        },

        // =====================================================================
        // HELPER METHODS
        // =====================================================================

        formatVersionDate(dateStr) {
            if (!dateStr) return '';
            const date = new Date(dateStr);
            return date.toLocaleDateString('pt-BR') + ' ' + date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });
        },

        getVersionIcon(version) {
            if (version.metadata?.rollback_to) return '&#8634;'; // Rollback
            if (version.metadata?.merge_from) return '&#8651;';  // Merge
            if (version.branch !== 'main') return '&#8669;';     // Branch
            return '&#9679;'; // Normal commit
        },

        getDiffStatusClass(status) {
            switch (status) {
                case 'added': return 'text-green-600 bg-green-50';
                case 'deleted': return 'text-red-600 bg-red-50';
                case 'modified': return 'text-yellow-600 bg-yellow-50';
                default: return 'text-gray-600 bg-gray-50';
            }
        },

        getDiffStatusIcon(status) {
            switch (status) {
                case 'added': return '+';
                case 'deleted': return '-';
                case 'modified': return '~';
                default: return '?';
            }
        }
    }
};

// HTML Template for Version History Tab
const VERSION_HISTORY_TEMPLATE = `
<!-- Tab: Historico (Issue #58) -->
<div v-if="activeTab === 'Historico'" class="p-4">
    <!-- Header com acoes -->
    <div class="flex justify-between items-center mb-4">
        <div class="flex items-center gap-3">
            <h3 class="font-semibold">Historico de Versoes</h3>
            <select v-model="selectedBranch" @change="loadVersionHistory"
                    class="text-sm border border-gray-300 rounded px-2 py-1">
                <option value="all">Todas as branches</option>
                <option v-for="b in versionBranches" :key="b.branch_id" :value="b.branch_name">
                    {{ b.branch_name }}
                </option>
            </select>
        </div>
        <div class="flex gap-2">
            <button @click="showBranchModal = true"
                    class="text-sm bg-gray-100 hover:bg-gray-200 px-3 py-1 rounded">
                + Branch
            </button>
            <button @click="showMergeModal = true"
                    class="text-sm bg-gray-100 hover:bg-gray-200 px-3 py-1 rounded"
                    :disabled="versionBranches.length < 2">
                Merge
            </button>
            <button @click="loadVersionHistory"
                    class="text-sm text-blue-600 hover:text-blue-800">
                &#8635; Atualizar
            </button>
        </div>
    </div>

    <!-- Loading -->
    <div v-if="versionLoading" class="flex items-center justify-center py-8">
        <div class="spinner"></div>
        <span class="ml-2 text-gray-500">Carregando historico...</span>
    </div>

    <!-- Error -->
    <div v-else-if="versionError" class="text-center py-8 text-red-500">
        {{ versionError }}
    </div>

    <!-- Empty State -->
    <div v-else-if="!versionHistory.length" class="text-center py-8 text-gray-400">
        <div class="text-4xl mb-2">&#128203;</div>
        <p>Nenhuma versao registrada ainda</p>
        <p class="text-sm mt-1">Versoes serao criadas automaticamente durante o desenvolvimento</p>
    </div>

    <!-- Version Timeline -->
    <div v-else class="space-y-2">
        <div v-for="(version, idx) in versionHistory" :key="version.version_hash"
             @click="selectVersion(version)"
             :class="['p-3 rounded-lg border cursor-pointer transition-all',
                      selectedVersion?.version_hash === version.version_hash
                          ? 'border-blue-500 bg-blue-50'
                          : 'border-gray-200 hover:border-gray-300 hover:bg-gray-50']">
            <div class="flex items-start gap-3">
                <!-- Timeline dot -->
                <div class="flex flex-col items-center">
                    <div :class="['w-8 h-8 rounded-full flex items-center justify-center text-sm',
                                  version.branch === 'main' ? 'bg-blue-500 text-white' : 'bg-purple-500 text-white']"
                         v-html="getVersionIcon(version)">
                    </div>
                    <div v-if="idx < versionHistory.length - 1" class="w-0.5 h-8 bg-gray-200 mt-1"></div>
                </div>

                <!-- Version info -->
                <div class="flex-1">
                    <div class="flex items-center gap-2">
                        <code class="text-xs bg-gray-100 px-1.5 py-0.5 rounded font-mono">
                            {{ version.version_hash }}
                        </code>
                        <span v-if="version.branch !== 'main'"
                              class="text-xs bg-purple-100 text-purple-700 px-1.5 py-0.5 rounded">
                            {{ version.branch }}
                        </span>
                        <span v-if="idx === 0"
                              class="text-xs bg-green-100 text-green-700 px-1.5 py-0.5 rounded">
                            HEAD
                        </span>
                    </div>
                    <p class="text-sm font-medium mt-1">{{ version.message }}</p>
                    <div class="flex items-center gap-4 mt-1 text-xs text-gray-500">
                        <span>{{ formatVersionDate(version.created_at) }}</span>
                        <span>{{ version.author }}</span>
                        <span>{{ version.files_count }} arquivos</span>
                    </div>
                </div>

                <!-- Actions -->
                <div class="flex gap-1" @click.stop>
                    <button v-if="idx > 0"
                            @click="compareVersions(version.version_hash, versionHistory[idx-1].version_hash)"
                            class="p-1 text-gray-400 hover:text-blue-600"
                            title="Comparar com versao anterior">
                        &#8646;
                    </button>
                    <button @click="previewRollback(version.version_hash)"
                            class="p-1 text-gray-400 hover:text-orange-600"
                            title="Restaurar esta versao">
                        &#8634;
                    </button>
                </div>
            </div>
        </div>
    </div>

    <!-- Diff Panel -->
    <div v-if="versionDiff" class="mt-4 border-t border-gray-200 pt-4">
        <div class="flex justify-between items-center mb-3">
            <h4 class="font-semibold">Diff</h4>
            <button @click="versionDiff = null" class="text-gray-400 hover:text-gray-600">&#10005;</button>
        </div>

        <div class="bg-gray-50 rounded-lg p-3 mb-3">
            <div class="flex items-center gap-4 text-sm">
                <span class="text-green-600">+{{ versionDiff.total_additions }} adicionadas</span>
                <span class="text-red-600">-{{ versionDiff.total_deletions }} removidas</span>
                <span class="text-gray-600">{{ versionDiff.files_changed }} arquivos alterados</span>
            </div>
        </div>

        <div class="space-y-2">
            <div v-for="file in versionDiff.files" :key="file.file_path"
                 class="border border-gray-200 rounded-lg overflow-hidden">
                <div :class="['px-3 py-2 flex items-center justify-between', getDiffStatusClass(file.status)]">
                    <div class="flex items-center gap-2">
                        <span class="font-mono text-sm">{{ getDiffStatusIcon(file.status) }}</span>
                        <span class="text-sm font-medium">{{ file.file_path }}</span>
                    </div>
                    <span class="text-xs">+{{ file.additions }} -{{ file.deletions }}</span>
                </div>
                <pre class="p-2 text-xs font-mono bg-gray-900 text-gray-100 overflow-x-auto max-h-60"><code v-for="(line, i) in file.diff_lines.slice(0, 50)" :key="i"
     :class="{'text-green-400': line.startsWith('+') && !line.startsWith('+++'),
              'text-red-400': line.startsWith('-') && !line.startsWith('---'),
              'text-blue-400': line.startsWith('@@')}">{{ line }}
</code></pre>
            </div>
        </div>
    </div>
</div>

<!-- Modal: Confirmar Rollback -->
<div v-if="showRollbackConfirm" class="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
    <div class="bg-white rounded-lg shadow-xl max-w-md w-full mx-4 p-6">
        <h3 class="text-lg font-semibold mb-2">Confirmar Rollback</h3>
        <p class="text-gray-600 mb-4">
            Restaurar codigo para a versao <code class="bg-gray-100 px-1">{{ selectedVersion?.version_hash }}</code>?
        </p>
        <p class="text-sm text-gray-500 mb-4">
            Um backup sera criado automaticamente antes do rollback.
        </p>
        <div class="flex justify-end gap-2">
            <button @click="showRollbackConfirm = false"
                    class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded">
                Cancelar
            </button>
            <button @click="confirmRollback"
                    class="px-4 py-2 bg-orange-500 text-white rounded hover:bg-orange-600">
                Restaurar
            </button>
        </div>
    </div>
</div>

<!-- Modal: Criar Branch -->
<div v-if="showBranchModal" class="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
    <div class="bg-white rounded-lg shadow-xl max-w-md w-full mx-4 p-6">
        <h3 class="text-lg font-semibold mb-4">Criar Nova Branch</h3>
        <input v-model="newBranchName"
               placeholder="Nome da branch"
               class="w-full border border-gray-300 rounded px-3 py-2 mb-4"
               @keyup.enter="createBranch">
        <p class="text-sm text-gray-500 mb-4">
            A branch sera criada a partir de {{ selectedVersion ? selectedVersion.version_hash : 'HEAD' }}
        </p>
        <div class="flex justify-end gap-2">
            <button @click="showBranchModal = false"
                    class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded">
                Cancelar
            </button>
            <button @click="createBranch"
                    class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                    :disabled="!newBranchName.trim()">
                Criar
            </button>
        </div>
    </div>
</div>

<!-- Modal: Merge Branches -->
<div v-if="showMergeModal" class="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
    <div class="bg-white rounded-lg shadow-xl max-w-md w-full mx-4 p-6">
        <h3 class="text-lg font-semibold mb-4">Merge de Branches</h3>
        <div class="space-y-3 mb-4">
            <div>
                <label class="text-sm text-gray-600">De:</label>
                <select v-model="mergeBranch.source" class="w-full border border-gray-300 rounded px-3 py-2">
                    <option value="">Selecione a branch</option>
                    <option v-for="b in versionBranches.filter(x => x.branch_name !== mergeBranch.target)"
                            :key="b.branch_id" :value="b.branch_name">
                        {{ b.branch_name }}
                    </option>
                </select>
            </div>
            <div>
                <label class="text-sm text-gray-600">Para:</label>
                <select v-model="mergeBranch.target" class="w-full border border-gray-300 rounded px-3 py-2">
                    <option value="main">main</option>
                    <option v-for="b in versionBranches.filter(x => x.branch_name !== mergeBranch.source)"
                            :key="b.branch_id" :value="b.branch_name">
                        {{ b.branch_name }}
                    </option>
                </select>
            </div>
        </div>
        <div class="flex justify-end gap-2">
            <button @click="showMergeModal = false"
                    class="px-4 py-2 text-gray-600 hover:bg-gray-100 rounded">
                Cancelar
            </button>
            <button @click="mergeBranches"
                    class="px-4 py-2 bg-purple-500 text-white rounded hover:bg-purple-600"
                    :disabled="!mergeBranch.source">
                Merge
            </button>
        </div>
    </div>
</div>
`;

// Export for use in Vue app
if (typeof window !== 'undefined') {
    window.VersionHistoryMixin = VersionHistoryMixin;
    window.VERSION_HISTORY_TEMPLATE = VERSION_HISTORY_TEMPLATE;
}

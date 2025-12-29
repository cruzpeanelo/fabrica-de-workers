/**
 * Execution Timeline Viewer Component
 * Issue #70: [Feature] Replay e Debug de Execucoes
 *
 * Vue 3 component for visualizing and replaying execution timelines.
 *
 * Features:
 * - Timeline visualization of execution steps
 * - Play/Pause/Step controls
 * - Speed adjustment (0.5x, 1x, 2x)
 * - Step detail viewer with prompts, responses, file changes
 * - Error debugging with stack traces
 * - Execution comparison
 */

const ExecutionTimelineViewer = {
    name: 'ExecutionTimelineViewer',

    props: {
        executionId: {
            type: String,
            default: null
        },
        autoLoad: {
            type: Boolean,
            default: true
        }
    },

    emits: ['close', 'rerun', 'compare'],

    data() {
        return {
            // State
            loading: false,
            error: null,

            // Execution data
            execution: null,
            timeline: [],
            executionInfo: null,

            // Replay state
            isPlaying: false,
            currentStepIndex: -1,
            speed: 1.0,

            // Selected step
            selectedStep: null,
            selectedStepDetails: null,

            // UI state
            showStepDetails: false,
            showCompare: false,
            compareExecutionId: '',
            compareResult: null,

            // Analysis
            analysis: null,
            showAnalysis: false,

            // Speed options
            speedOptions: [
                { label: '0.5x', value: 0.5 },
                { label: '1x', value: 1.0 },
                { label: '2x', value: 2.0 }
            ],

            // Replay interval
            replayInterval: null
        };
    },

    computed: {
        progress() {
            if (!this.timeline.length) return 0;
            return ((this.currentStepIndex + 1) / this.timeline.length) * 100;
        },

        currentStep() {
            if (this.currentStepIndex >= 0 && this.currentStepIndex < this.timeline.length) {
                return this.timeline[this.currentStepIndex];
            }
            return null;
        },

        hasNext() {
            return this.currentStepIndex < this.timeline.length - 1;
        },

        hasPrevious() {
            return this.currentStepIndex > 0;
        },

        totalDuration() {
            if (!this.executionInfo) return '0s';
            const ms = this.executionInfo.duration_ms || 0;
            if (ms < 1000) return `${ms}ms`;
            return `${(ms / 1000).toFixed(1)}s`;
        },

        statusColor() {
            if (!this.executionInfo) return 'gray';
            const status = this.executionInfo.status;
            if (status === 'success') return 'green';
            if (status === 'failed') return 'red';
            if (status === 'running') return 'blue';
            return 'gray';
        },

        statusIcon() {
            if (!this.executionInfo) return 'clock';
            const status = this.executionInfo.status;
            if (status === 'success') return 'check-circle';
            if (status === 'failed') return 'x-circle';
            if (status === 'running') return 'play-circle';
            return 'clock';
        }
    },

    watch: {
        executionId: {
            immediate: true,
            handler(newId) {
                if (newId && this.autoLoad) {
                    this.loadExecution();
                }
            }
        }
    },

    methods: {
        async loadExecution() {
            if (!this.executionId) return;

            this.loading = true;
            this.error = null;

            try {
                // Load timeline
                const response = await fetch(`/api/executions/${this.executionId}/timeline`);
                if (!response.ok) throw new Error('Execution not found');

                const data = await response.json();
                this.timeline = data.timeline || [];
                this.executionInfo = data.info;
                this.execution = data;

                // Reset state
                this.currentStepIndex = -1;
                this.selectedStep = null;
                this.isPlaying = false;

            } catch (err) {
                this.error = err.message;
            } finally {
                this.loading = false;
            }
        },

        async loadStepDetails(step) {
            if (!step || !step.id) return;

            try {
                const response = await fetch(`/api/executions/${this.executionId}/steps/${step.id}`);
                if (!response.ok) throw new Error('Step not found');

                this.selectedStepDetails = await response.json();
                this.showStepDetails = true;

            } catch (err) {
                console.error('Error loading step details:', err);
            }
        },

        selectStep(step, index) {
            this.selectedStep = step;
            this.currentStepIndex = index;
            this.loadStepDetails(step);
        },

        // Replay controls
        play() {
            if (this.isPlaying) return;
            if (this.currentStepIndex === -1) {
                this.currentStepIndex = 0;
            }

            this.isPlaying = true;
            this.startReplay();
        },

        pause() {
            this.isPlaying = false;
            this.stopReplay();
        },

        stop() {
            this.isPlaying = false;
            this.stopReplay();
            this.currentStepIndex = -1;
            this.selectedStep = null;
        },

        nextStep() {
            if (!this.hasNext) return;
            this.currentStepIndex++;
            this.selectedStep = this.timeline[this.currentStepIndex];
            this.loadStepDetails(this.selectedStep);
        },

        previousStep() {
            if (!this.hasPrevious) return;
            this.currentStepIndex--;
            this.selectedStep = this.timeline[this.currentStepIndex];
            this.loadStepDetails(this.selectedStep);
        },

        goToStep(index) {
            if (index < 0 || index >= this.timeline.length) return;
            this.currentStepIndex = index;
            this.selectedStep = this.timeline[index];
            this.loadStepDetails(this.selectedStep);
        },

        setSpeed(speed) {
            this.speed = speed;
            if (this.isPlaying) {
                this.stopReplay();
                this.startReplay();
            }
        },

        startReplay() {
            if (this.replayInterval) {
                clearInterval(this.replayInterval);
            }

            const baseInterval = 2000; // 2 seconds base
            const interval = baseInterval / this.speed;

            this.replayInterval = setInterval(() => {
                if (this.hasNext) {
                    this.nextStep();
                } else {
                    this.pause();
                }
            }, interval);
        },

        stopReplay() {
            if (this.replayInterval) {
                clearInterval(this.replayInterval);
                this.replayInterval = null;
            }
        },

        // Analysis
        async loadAnalysis() {
            try {
                const response = await fetch(`/api/executions/${this.executionId}/analysis`);
                if (!response.ok) throw new Error('Analysis not available');

                this.analysis = await response.json();
                this.showAnalysis = true;

            } catch (err) {
                console.error('Error loading analysis:', err);
            }
        },

        // Comparison
        async compareExecutions() {
            if (!this.compareExecutionId) return;

            try {
                const response = await fetch('/api/executions/compare', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        execution1_id: this.executionId,
                        execution2_id: this.compareExecutionId
                    })
                });

                if (!response.ok) throw new Error('Comparison failed');

                this.compareResult = await response.json();

            } catch (err) {
                console.error('Error comparing executions:', err);
            }
        },

        // Export
        async exportExecution(format = 'json') {
            try {
                const response = await fetch(`/api/executions/${this.executionId}/export?format=${format}`);
                const data = await response.json();

                const content = format === 'json'
                    ? JSON.stringify(data, null, 2)
                    : data.content;

                const blob = new Blob([content], { type: 'text/plain' });
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = `execution-${this.executionId}.${format === 'json' ? 'json' : 'txt'}`;
                a.click();
                URL.revokeObjectURL(url);

            } catch (err) {
                console.error('Error exporting:', err);
            }
        },

        // Rerun from step
        async rerunFromStep(stepId) {
            this.$emit('rerun', {
                executionId: this.executionId,
                stepId: stepId
            });
        },

        // Helpers
        formatDuration(ms) {
            if (!ms) return '0ms';
            if (ms < 1000) return `${ms}ms`;
            return `${(ms / 1000).toFixed(1)}s`;
        },

        formatTimestamp(timestamp) {
            if (!timestamp) return '';
            const date = new Date(timestamp);
            return date.toLocaleTimeString();
        },

        getStepIcon(stepType) {
            const icons = {
                'initialization': 'play',
                'parsing': 'file-text',
                'generating': 'code',
                'claude_request': 'message-square',
                'claude_response': 'message-circle',
                'file_create': 'file-plus',
                'file_modify': 'edit',
                'file_delete': 'file-minus',
                'linting': 'check-square',
                'type_checking': 'type',
                'testing': 'play-circle',
                'security_scan': 'shield',
                'fix_attempt': 'tool',
                'committing': 'git-commit',
                'error': 'alert-circle',
                'completion': 'check-circle',
                'execution_start': 'play',
                'execution_end': 'flag'
            };
            return icons[stepType] || 'circle';
        },

        getStepColor(status) {
            if (status === 'success') return 'text-green-600 bg-green-100';
            if (status === 'failed') return 'text-red-600 bg-red-100';
            if (status === 'started') return 'text-blue-600 bg-blue-100';
            return 'text-gray-600 bg-gray-100';
        },

        close() {
            this.stopReplay();
            this.$emit('close');
        }
    },

    beforeUnmount() {
        this.stopReplay();
    },

    template: `
        <div class="execution-timeline-viewer fixed inset-0 z-50 bg-black/50 flex items-center justify-center p-4">
            <div class="bg-white rounded-lg shadow-xl w-full max-w-6xl max-h-[90vh] overflow-hidden flex flex-col">
                <!-- Header -->
                <div class="bg-[#003B4A] text-white px-6 py-4 flex items-center justify-between">
                    <div class="flex items-center gap-4">
                        <h2 class="text-xl font-semibold">Execution Timeline</h2>
                        <span v-if="executionId" class="text-sm opacity-75">{{ executionId }}</span>
                        <span v-if="executionInfo"
                              :class="['px-2 py-0.5 rounded text-xs font-medium',
                                       statusColor === 'green' ? 'bg-green-500' :
                                       statusColor === 'red' ? 'bg-red-500' :
                                       statusColor === 'blue' ? 'bg-blue-500' : 'bg-gray-500']">
                            {{ executionInfo.status }}
                        </span>
                    </div>
                    <div class="flex items-center gap-2">
                        <button @click="exportExecution('json')"
                                class="px-3 py-1 text-sm bg-white/20 hover:bg-white/30 rounded">
                            Export JSON
                        </button>
                        <button @click="exportExecution('text')"
                                class="px-3 py-1 text-sm bg-white/20 hover:bg-white/30 rounded">
                            Export Text
                        </button>
                        <button @click="close" class="p-1 hover:bg-white/20 rounded">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <!-- Loading / Error -->
                <div v-if="loading" class="flex-1 flex items-center justify-center">
                    <div class="text-center">
                        <div class="animate-spin w-12 h-12 border-4 border-[#FF6C00] border-t-transparent rounded-full mx-auto"></div>
                        <p class="mt-4 text-gray-600">Loading execution...</p>
                    </div>
                </div>

                <div v-else-if="error" class="flex-1 flex items-center justify-center">
                    <div class="text-center text-red-600">
                        <svg class="w-16 h-16 mx-auto" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                  d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                        </svg>
                        <p class="mt-4">{{ error }}</p>
                        <button @click="loadExecution" class="mt-4 px-4 py-2 bg-[#FF6C00] text-white rounded hover:bg-[#E55C00]">
                            Retry
                        </button>
                    </div>
                </div>

                <!-- Main Content -->
                <template v-else-if="execution">
                    <!-- Info Bar -->
                    <div class="px-6 py-3 bg-gray-50 border-b flex items-center justify-between text-sm">
                        <div class="flex items-center gap-6">
                            <span><strong>Duration:</strong> {{ totalDuration }}</span>
                            <span><strong>Steps:</strong> {{ timeline.length }}</span>
                            <span v-if="executionInfo"><strong>Tokens:</strong> {{ executionInfo.total_tokens }}</span>
                        </div>
                        <div class="flex items-center gap-4">
                            <button @click="loadAnalysis"
                                    class="text-[#003B4A] hover:text-[#FF6C00] flex items-center gap-1">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                          d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"/>
                                </svg>
                                Analyze
                            </button>
                            <button @click="showCompare = true"
                                    class="text-[#003B4A] hover:text-[#FF6C00] flex items-center gap-1">
                                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                          d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"/>
                                </svg>
                                Compare
                            </button>
                        </div>
                    </div>

                    <!-- Replay Controls -->
                    <div class="px-6 py-3 bg-gray-100 border-b flex items-center justify-between">
                        <div class="flex items-center gap-4">
                            <button @click="stop"
                                    class="p-2 rounded hover:bg-gray-200"
                                    title="Stop">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                                    <rect x="6" y="6" width="12" height="12" rx="1"/>
                                </svg>
                            </button>
                            <button @click="previousStep"
                                    :disabled="!hasPrevious"
                                    class="p-2 rounded hover:bg-gray-200 disabled:opacity-50"
                                    title="Previous">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                                    <path d="M6 6h2v12H6zm3.5 6l8.5 6V6z"/>
                                </svg>
                            </button>
                            <button v-if="!isPlaying" @click="play"
                                    class="p-2 bg-[#FF6C00] text-white rounded hover:bg-[#E55C00]"
                                    title="Play">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                                    <path d="M8 5v14l11-7z"/>
                                </svg>
                            </button>
                            <button v-else @click="pause"
                                    class="p-2 bg-[#FF6C00] text-white rounded hover:bg-[#E55C00]"
                                    title="Pause">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                                    <path d="M6 4h4v16H6zm8 0h4v16h-4z"/>
                                </svg>
                            </button>
                            <button @click="nextStep"
                                    :disabled="!hasNext"
                                    class="p-2 rounded hover:bg-gray-200 disabled:opacity-50"
                                    title="Next">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                                    <path d="M6 18l8.5-6L6 6v12zM16 6v12h2V6h-2z"/>
                                </svg>
                            </button>
                        </div>

                        <!-- Progress Bar -->
                        <div class="flex-1 mx-8">
                            <div class="h-2 bg-gray-300 rounded-full overflow-hidden">
                                <div class="h-full bg-[#FF6C00] transition-all duration-300"
                                     :style="{ width: progress + '%' }"></div>
                            </div>
                            <div class="text-xs text-gray-500 mt-1 text-center">
                                Step {{ currentStepIndex + 1 }} of {{ timeline.length }}
                            </div>
                        </div>

                        <!-- Speed Control -->
                        <div class="flex items-center gap-2">
                            <span class="text-sm text-gray-600">Speed:</span>
                            <div class="flex rounded overflow-hidden border">
                                <button v-for="opt in speedOptions"
                                        :key="opt.value"
                                        @click="setSpeed(opt.value)"
                                        :class="['px-3 py-1 text-sm',
                                                 speed === opt.value
                                                   ? 'bg-[#003B4A] text-white'
                                                   : 'bg-white hover:bg-gray-100']">
                                    {{ opt.label }}
                                </button>
                            </div>
                        </div>
                    </div>

                    <!-- Timeline and Details -->
                    <div class="flex-1 flex overflow-hidden">
                        <!-- Timeline Panel -->
                        <div class="w-1/2 border-r overflow-y-auto p-4">
                            <div class="space-y-2">
                                <div v-for="(step, index) in timeline"
                                     :key="step.id"
                                     @click="selectStep(step, index)"
                                     :class="['p-3 rounded-lg border cursor-pointer transition-all',
                                              currentStepIndex === index
                                                ? 'border-[#FF6C00] bg-orange-50 shadow'
                                                : 'hover:border-gray-300 hover:bg-gray-50']">
                                    <div class="flex items-center gap-3">
                                        <!-- Status indicator -->
                                        <div :class="['w-8 h-8 rounded-full flex items-center justify-center',
                                                     getStepColor(step.status)]">
                                            <span class="text-xs font-bold">{{ index + 1 }}</span>
                                        </div>

                                        <!-- Step info -->
                                        <div class="flex-1 min-w-0">
                                            <div class="font-medium text-gray-900 truncate">{{ step.name }}</div>
                                            <div class="text-xs text-gray-500 flex items-center gap-2">
                                                <span>{{ step.type }}</span>
                                                <span v-if="step.duration_ms">{{ formatDuration(step.duration_ms) }}</span>
                                                <span v-if="step.has_prompt" class="text-blue-500">Has Prompt</span>
                                                <span v-if="step.has_error" class="text-red-500">Error</span>
                                            </div>
                                        </div>

                                        <!-- Indicators -->
                                        <div class="flex items-center gap-1">
                                            <span v-if="step.files_changed"
                                                  class="px-2 py-0.5 bg-gray-200 text-gray-700 rounded text-xs">
                                                {{ step.files_changed }} files
                                            </span>
                                            <svg v-if="step.status === 'success'"
                                                 class="w-5 h-5 text-green-500" fill="currentColor" viewBox="0 0 20 20">
                                                <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd"/>
                                            </svg>
                                            <svg v-else-if="step.status === 'failed'"
                                                 class="w-5 h-5 text-red-500" fill="currentColor" viewBox="0 0 20 20">
                                                <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd"/>
                                            </svg>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>

                        <!-- Details Panel -->
                        <div class="w-1/2 overflow-y-auto p-4 bg-gray-50">
                            <div v-if="selectedStepDetails" class="space-y-4">
                                <!-- Step Header -->
                                <div class="bg-white rounded-lg p-4 shadow-sm">
                                    <h3 class="font-semibold text-lg text-[#003B4A]">{{ selectedStepDetails.name }}</h3>
                                    <div class="mt-2 flex items-center gap-4 text-sm text-gray-600">
                                        <span>Type: {{ selectedStepDetails.step_type }}</span>
                                        <span>Duration: {{ formatDuration(selectedStepDetails.duration_ms) }}</span>
                                        <span v-if="selectedStepDetails.attempt > 1">Attempt: {{ selectedStepDetails.attempt }}</span>
                                    </div>

                                    <!-- Rerun button -->
                                    <button v-if="selectedStepDetails.can_replay"
                                            @click="rerunFromStep(selectedStepDetails.step_id)"
                                            class="mt-3 px-3 py-1 text-sm bg-[#FF6C00] text-white rounded hover:bg-[#E55C00]">
                                        Re-run from here
                                    </button>
                                </div>

                                <!-- Prompt -->
                                <div v-if="selectedStepDetails.prompt" class="bg-white rounded-lg p-4 shadow-sm">
                                    <h4 class="font-medium text-gray-700 mb-2 flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M8 10h.01M12 10h.01M16 10h.01M9 16H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-5l-5 5v-5z"/>
                                        </svg>
                                        Prompt Sent
                                    </h4>
                                    <pre class="bg-gray-100 p-3 rounded text-sm overflow-x-auto whitespace-pre-wrap max-h-64 overflow-y-auto">{{ selectedStepDetails.prompt }}</pre>
                                </div>

                                <!-- Response -->
                                <div v-if="selectedStepDetails.response" class="bg-white rounded-lg p-4 shadow-sm">
                                    <h4 class="font-medium text-gray-700 mb-2 flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M17 8h2a2 2 0 012 2v6a2 2 0 01-2 2h-2v4l-4-4H9a1.994 1.994 0 01-1.414-.586m0 0L11 14h4a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2v4l.586-.586z"/>
                                        </svg>
                                        Response Received
                                        <span class="text-xs text-gray-500 ml-2" v-if="selectedStepDetails.tokens_output">
                                            ({{ selectedStepDetails.tokens_output }} tokens)
                                        </span>
                                    </h4>
                                    <pre class="bg-gray-100 p-3 rounded text-sm overflow-x-auto whitespace-pre-wrap max-h-64 overflow-y-auto">{{ selectedStepDetails.response }}</pre>
                                </div>

                                <!-- File Changes -->
                                <div v-if="selectedStepDetails.file_changes && selectedStepDetails.file_changes.length"
                                     class="bg-white rounded-lg p-4 shadow-sm">
                                    <h4 class="font-medium text-gray-700 mb-2 flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"/>
                                        </svg>
                                        File Changes ({{ selectedStepDetails.file_changes.length }})
                                    </h4>
                                    <div class="space-y-2">
                                        <div v-for="(fc, i) in selectedStepDetails.file_changes" :key="i"
                                             class="border rounded p-2">
                                            <div class="flex items-center gap-2 text-sm">
                                                <span :class="fc.action === 'create' ? 'text-green-600' :
                                                              fc.action === 'delete' ? 'text-red-600' : 'text-blue-600'">
                                                    {{ fc.action === 'create' ? '+' : fc.action === 'delete' ? '-' : '~' }}
                                                </span>
                                                <span class="font-mono">{{ fc.path }}</span>
                                            </div>
                                            <div v-if="fc.diff" class="mt-2">
                                                <pre class="bg-gray-100 p-2 rounded text-xs overflow-x-auto max-h-32 overflow-y-auto">{{ fc.diff }}</pre>
                                            </div>
                                        </div>
                                    </div>
                                </div>

                                <!-- Error -->
                                <div v-if="selectedStepDetails.error_message"
                                     class="bg-red-50 rounded-lg p-4 border border-red-200">
                                    <h4 class="font-medium text-red-700 mb-2 flex items-center gap-2">
                                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                                  d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                                        </svg>
                                        Error: {{ selectedStepDetails.error_type }}
                                    </h4>
                                    <p class="text-red-600 mb-2">{{ selectedStepDetails.error_message }}</p>
                                    <pre v-if="selectedStepDetails.stack_trace"
                                         class="bg-red-100 p-3 rounded text-xs overflow-x-auto whitespace-pre-wrap max-h-64 overflow-y-auto">{{ selectedStepDetails.stack_trace }}</pre>
                                </div>
                            </div>

                            <div v-else class="h-full flex items-center justify-center text-gray-500">
                                <div class="text-center">
                                    <svg class="w-16 h-16 mx-auto text-gray-300" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                              d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"/>
                                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                                              d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"/>
                                    </svg>
                                    <p class="mt-4">Select a step to view details</p>
                                </div>
                            </div>
                        </div>
                    </div>
                </template>
            </div>

            <!-- Compare Modal -->
            <div v-if="showCompare" class="fixed inset-0 z-60 bg-black/50 flex items-center justify-center p-4">
                <div class="bg-white rounded-lg shadow-xl w-full max-w-2xl">
                    <div class="p-4 border-b flex justify-between items-center">
                        <h3 class="font-semibold">Compare Executions</h3>
                        <button @click="showCompare = false" class="p-1 hover:bg-gray-100 rounded">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                    <div class="p-4">
                        <div class="mb-4">
                            <label class="block text-sm font-medium text-gray-700 mb-1">Compare with:</label>
                            <input v-model="compareExecutionId"
                                   type="text"
                                   placeholder="Enter execution ID (EXEC-XXXX)"
                                   class="w-full border rounded px-3 py-2">
                        </div>
                        <button @click="compareExecutions"
                                :disabled="!compareExecutionId"
                                class="w-full px-4 py-2 bg-[#003B4A] text-white rounded hover:bg-[#002A35] disabled:opacity-50">
                            Compare
                        </button>

                        <!-- Compare Result -->
                        <div v-if="compareResult" class="mt-4 p-4 bg-gray-50 rounded">
                            <h4 class="font-medium mb-2">Comparison Result</h4>
                            <div class="space-y-2 text-sm">
                                <p :class="compareResult.same_status ? 'text-green-600' : 'text-red-600'">
                                    Status: {{ compareResult.same_status ? 'Same' : 'Different' }}
                                </p>
                                <p>Duration diff: {{ compareResult.duration_diff_ms }}ms</p>
                                <p>Token diff: {{ compareResult.tokens_diff }}</p>
                                <pre class="mt-2 p-2 bg-white rounded text-xs whitespace-pre-wrap">{{ compareResult.summary }}</pre>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Analysis Modal -->
            <div v-if="showAnalysis && analysis" class="fixed inset-0 z-60 bg-black/50 flex items-center justify-center p-4">
                <div class="bg-white rounded-lg shadow-xl w-full max-w-2xl max-h-[80vh] overflow-hidden flex flex-col">
                    <div class="p-4 border-b flex justify-between items-center">
                        <h3 class="font-semibold">Failure Analysis</h3>
                        <button @click="showAnalysis = false" class="p-1 hover:bg-gray-100 rounded">
                            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                    <div class="p-4 overflow-y-auto">
                        <div class="mb-4 p-3 bg-gray-50 rounded">
                            <div class="flex justify-between text-sm">
                                <span>Total Steps: {{ analysis.total_steps }}</span>
                                <span>Failed: {{ analysis.failed_steps }}</span>
                                <span>Success Rate: {{ analysis.success_rate.toFixed(1) }}%</span>
                            </div>
                        </div>

                        <div v-for="failure in analysis.failures" :key="failure.step_id"
                             class="mb-4 p-4 border border-red-200 bg-red-50 rounded">
                            <h4 class="font-medium text-red-700">{{ failure.name }}</h4>
                            <p class="text-sm text-red-600 mt-1">{{ failure.error_message }}</p>
                            <div v-if="failure.suggestions.length" class="mt-2">
                                <p class="text-xs font-medium text-gray-700">Suggestions:</p>
                                <ul class="text-xs text-gray-600 list-disc list-inside">
                                    <li v-for="s in failure.suggestions" :key="s">{{ s }}</li>
                                </ul>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    `
};

// Export for use in Vue app
if (typeof window !== 'undefined') {
    window.ExecutionTimelineViewer = ExecutionTimelineViewer;
}

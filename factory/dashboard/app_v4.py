"""
Factory Dashboard v4.0 - Worker-Centric Dashboard
Plataforma E - Nova Arquitetura MVP

Dashboard simplificado focado em:
- Jobs e fila de processamento
- Workers ativos
- Metricas em tempo real
"""
import os
import sys
from pathlib import Path
from datetime import datetime
from contextlib import asynccontextmanager

# Adicionar raiz do projeto ao path
ROOT_DIR = Path(__file__).resolve().parent.parent.parent
if str(ROOT_DIR) not in sys.path:
    sys.path.insert(0, str(ROOT_DIR))

from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse
from dotenv import load_dotenv

load_dotenv()

# Configuracoes
DASHBOARD_HOST = os.getenv("DASHBOARD_HOST", "127.0.0.1")
DASHBOARD_PORT = int(os.getenv("DASHBOARD_PORT", 9000))


# =============================================================================
# LIFESPAN
# =============================================================================

@asynccontextmanager
async def lifespan(app: FastAPI):
    """Inicializacao e finalizacao da aplicacao"""
    # Startup
    print(f"[Dashboard v4] Iniciando em http://{DASHBOARD_HOST}:{DASHBOARD_PORT}")

    # Inicializar banco
    try:
        from factory.database.connection import init_db
        init_db()
    except Exception as e:
        print(f"[Dashboard v4] Aviso: Erro ao inicializar DB: {e}")

    yield

    # Shutdown
    print("[Dashboard v4] Encerrando...")


# =============================================================================
# APP
# =============================================================================

app = FastAPI(
    title="Plataforma E v4.0",
    description="Worker-Centric Code Generation Platform",
    version="4.0.0",
    docs_url="/docs",
    redoc_url="/redoc",
    lifespan=lifespan
)

# CORS - Restrict to specific origins for security
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:9001",
        "http://localhost:9000",
        "http://localhost:8000",
        "http://127.0.0.1:9001",
        "http://127.0.0.1:9000",
        "http://127.0.0.1:8000"
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Rate limit middleware
try:
    from factory.api.rate_limit import RateLimitMiddleware
    app.add_middleware(RateLimitMiddleware)
except ImportError:
    pass

# Include routers
try:
    from factory.api.routes import router as api_router
    from factory.api.auth import auth_router
    app.include_router(api_router)
    app.include_router(auth_router)
    print(f"[Dashboard v4] Routers registrados: {len(api_router.routes)} + {len(auth_router.routes)} rotas")
except Exception as e:
    import traceback
    print(f"[Dashboard v4] Erro ao importar routers: {e}")
    traceback.print_exc()


# =============================================================================
# DASHBOARD HTML
# =============================================================================

DASHBOARD_HTML = """
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Plataforma E v4.0</title>
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
    <style>
        [v-cloak] { display: none; }
        .fade-enter-active, .fade-leave-active { transition: opacity 0.3s; }
        .fade-enter-from, .fade-leave-to { opacity: 0; }
    </style>
</head>
<body class="bg-gray-900 text-white min-h-screen">
    <div id="app" v-cloak>
        <!-- Header -->
        <header class="bg-gray-800 border-b border-gray-700 px-6 py-4">
            <div class="flex items-center justify-between">
                <div class="flex items-center space-x-4">
                    <h1 class="text-2xl font-bold text-blue-400">Plataforma E</h1>
                    <span class="text-xs bg-blue-600 px-2 py-1 rounded">v4.0</span>
                </div>
                <div class="flex items-center space-x-4">
                    <span class="text-sm text-gray-400">{{ currentTime }}</span>
                    <div :class="['w-3 h-3 rounded-full', connected ? 'bg-green-500' : 'bg-red-500']"></div>
                </div>
            </div>
        </header>

        <main class="p-6">
            <!-- Stats Cards -->
            <div class="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
                <div class="bg-gray-800 rounded-lg p-4 border border-gray-700">
                    <div class="text-sm text-gray-400 mb-1">Na Fila</div>
                    <div class="text-3xl font-bold text-yellow-400">{{ stats.pending + stats.queued }}</div>
                </div>
                <div class="bg-gray-800 rounded-lg p-4 border border-gray-700">
                    <div class="text-sm text-gray-400 mb-1">Processando</div>
                    <div class="text-3xl font-bold text-blue-400">{{ stats.running }}</div>
                </div>
                <div class="bg-gray-800 rounded-lg p-4 border border-gray-700">
                    <div class="text-sm text-gray-400 mb-1">Completados</div>
                    <div class="text-3xl font-bold text-green-400">{{ stats.completed }}</div>
                </div>
                <div class="bg-gray-800 rounded-lg p-4 border border-gray-700">
                    <div class="text-sm text-gray-400 mb-1">Workers Ativos</div>
                    <div class="text-3xl font-bold text-purple-400">{{ activeWorkers }}</div>
                </div>
            </div>

            <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
                <!-- Workers Panel -->
                <div class="bg-gray-800 rounded-lg border border-gray-700">
                    <div class="px-4 py-3 border-b border-gray-700">
                        <h2 class="text-lg font-semibold">Workers</h2>
                    </div>
                    <div class="p-4 space-y-3 max-h-96 overflow-y-auto">
                        <div v-if="workers.length === 0" class="text-gray-500 text-center py-4">
                            Nenhum worker ativo
                        </div>
                        <div v-for="worker in workers" :key="worker.worker_id"
                             class="bg-gray-700 rounded-lg p-3">
                            <div class="flex items-center justify-between mb-2">
                                <span class="font-medium text-sm">{{ worker.worker_id }}</span>
                                <span :class="['text-xs px-2 py-1 rounded', getStatusClass(worker.status)]">
                                    {{ worker.status.toUpperCase() }}
                                </span>
                            </div>
                            <div class="text-xs text-gray-400">
                                <div v-if="worker.current_job_id" class="text-blue-400">
                                    Job: {{ worker.current_job_id }}
                                </div>
                                <div>Completados: {{ worker.jobs_completed }} | Falhas: {{ worker.jobs_failed }}</div>
                                <div>Modelo: {{ worker.model }}</div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Jobs List -->
                <div class="lg:col-span-2 bg-gray-800 rounded-lg border border-gray-700">
                    <div class="px-4 py-3 border-b border-gray-700 flex items-center justify-between">
                        <h2 class="text-lg font-semibold">Jobs</h2>
                        <select v-model="statusFilter" class="bg-gray-700 text-sm rounded px-2 py-1 border border-gray-600">
                            <option value="">Todos</option>
                            <option value="pending">Pendentes</option>
                            <option value="running">Executando</option>
                            <option value="completed">Completados</option>
                            <option value="failed">Falhas</option>
                        </select>
                    </div>
                    <div class="overflow-x-auto">
                        <table class="w-full">
                            <thead class="bg-gray-700">
                                <tr>
                                    <th class="px-4 py-2 text-left text-xs text-gray-400">Job ID</th>
                                    <th class="px-4 py-2 text-left text-xs text-gray-400">Descricao</th>
                                    <th class="px-4 py-2 text-left text-xs text-gray-400">Status</th>
                                    <th class="px-4 py-2 text-left text-xs text-gray-400">Progresso</th>
                                    <th class="px-4 py-2 text-left text-xs text-gray-400">Worker</th>
                                </tr>
                            </thead>
                            <tbody class="divide-y divide-gray-700">
                                <tr v-if="filteredJobs.length === 0">
                                    <td colspan="5" class="px-4 py-8 text-center text-gray-500">
                                        Nenhum job encontrado
                                    </td>
                                </tr>
                                <tr v-for="job in filteredJobs" :key="job.job_id"
                                    class="hover:bg-gray-700 cursor-pointer"
                                    @click="selectJob(job)">
                                    <td class="px-4 py-3 text-sm font-mono">{{ job.job_id }}</td>
                                    <td class="px-4 py-3 text-sm max-w-xs truncate">{{ job.description }}</td>
                                    <td class="px-4 py-3">
                                        <span :class="['text-xs px-2 py-1 rounded', getJobStatusClass(job.status)]">
                                            {{ job.status.toUpperCase() }}
                                        </span>
                                    </td>
                                    <td class="px-4 py-3">
                                        <div class="flex items-center space-x-2">
                                            <div class="w-24 bg-gray-600 rounded-full h-2">
                                                <div class="bg-blue-500 h-2 rounded-full"
                                                     :style="{ width: job.progress + '%' }"></div>
                                            </div>
                                            <span class="text-xs text-gray-400">{{ Math.round(job.progress) }}%</span>
                                        </div>
                                    </td>
                                    <td class="px-4 py-3 text-sm text-gray-400">{{ job.worker_id || '-' }}</td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>

            <!-- Create Job Form -->
            <div class="mt-6 bg-gray-800 rounded-lg border border-gray-700 p-4">
                <h2 class="text-lg font-semibold mb-4">Criar Novo Job</h2>
                <form @submit.prevent="createJob" class="space-y-4">
                    <div>
                        <label class="block text-sm text-gray-400 mb-1">Descricao</label>
                        <textarea v-model="newJob.description"
                                  class="w-full bg-gray-700 rounded px-3 py-2 text-sm border border-gray-600 focus:border-blue-500 focus:outline-none"
                                  rows="3"
                                  placeholder="Descreva o que voce quer construir..."></textarea>
                    </div>
                    <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <div>
                            <label class="block text-sm text-gray-400 mb-1">Tech Stack</label>
                            <input v-model="newJob.tech_stack"
                                   class="w-full bg-gray-700 rounded px-3 py-2 text-sm border border-gray-600 focus:border-blue-500 focus:outline-none"
                                   placeholder="python, fastapi, react, etc.">
                        </div>
                        <div>
                            <label class="block text-sm text-gray-400 mb-1">Features (separadas por virgula)</label>
                            <input v-model="newJob.featuresStr"
                                   class="w-full bg-gray-700 rounded px-3 py-2 text-sm border border-gray-600 focus:border-blue-500 focus:outline-none"
                                   placeholder="Auth JWT, CRUD, API REST">
                        </div>
                    </div>
                    <button type="submit"
                            class="bg-blue-600 hover:bg-blue-700 px-4 py-2 rounded text-sm font-medium transition-colors"
                            :disabled="!newJob.description">
                        Criar Job
                    </button>
                </form>
            </div>

            <!-- Job Details Modal -->
            <div v-if="selectedJob"
                 class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50"
                 @click.self="selectedJob = null">
                <div class="bg-gray-800 rounded-lg max-w-2xl w-full max-h-[80vh] overflow-y-auto">
                    <div class="px-6 py-4 border-b border-gray-700 flex items-center justify-between">
                        <h3 class="text-lg font-semibold">{{ selectedJob.job_id }}</h3>
                        <button @click="selectedJob = null" class="text-gray-400 hover:text-white">
                            <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                            </svg>
                        </button>
                    </div>
                    <div class="p-6 space-y-4">
                        <div>
                            <div class="text-sm text-gray-400">Descricao</div>
                            <div class="mt-1">{{ selectedJob.description }}</div>
                        </div>
                        <div class="grid grid-cols-2 gap-4">
                            <div>
                                <div class="text-sm text-gray-400">Status</div>
                                <div :class="['mt-1 inline-block px-2 py-1 rounded text-sm', getJobStatusClass(selectedJob.status)]">
                                    {{ selectedJob.status.toUpperCase() }}
                                </div>
                            </div>
                            <div>
                                <div class="text-sm text-gray-400">Step Atual</div>
                                <div class="mt-1">{{ selectedJob.current_step }}</div>
                            </div>
                        </div>
                        <div v-if="selectedJob.error_message">
                            <div class="text-sm text-gray-400">Erro</div>
                            <div class="mt-1 text-red-400 bg-red-900 bg-opacity-20 p-2 rounded text-sm">
                                {{ selectedJob.error_message }}
                            </div>
                        </div>
                        <div v-if="selectedJob.output_path">
                            <div class="text-sm text-gray-400">Output</div>
                            <div class="mt-1 font-mono text-sm">{{ selectedJob.output_path }}</div>
                        </div>
                        <div v-if="selectedJob.step_logs && selectedJob.step_logs.length">
                            <div class="text-sm text-gray-400 mb-2">Logs</div>
                            <div class="bg-gray-900 rounded p-3 max-h-48 overflow-y-auto font-mono text-xs">
                                <div v-for="log in selectedJob.step_logs" :key="log.timestamp"
                                     :class="['py-1', log.success ? 'text-green-400' : 'text-red-400']">
                                    [{{ log.step }}] {{ log.message }}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </main>
    </div>

    <script>
    const { createApp, ref, computed, onMounted, onUnmounted } = Vue;

    createApp({
        setup() {
            const stats = ref({ pending: 0, queued: 0, running: 0, completed: 0, failed: 0 });
            const workers = ref([]);
            const jobs = ref([]);
            const connected = ref(false);
            const currentTime = ref('');
            const statusFilter = ref('');
            const selectedJob = ref(null);
            const newJob = ref({ description: '', tech_stack: '', featuresStr: '' });

            const activeWorkers = computed(() => workers.value.filter(w => w.status !== 'offline').length);

            const filteredJobs = computed(() => {
                if (!statusFilter.value) return jobs.value;
                return jobs.value.filter(j => j.status === statusFilter.value);
            });

            const getStatusClass = (status) => {
                const classes = {
                    'idle': 'bg-gray-600',
                    'busy': 'bg-blue-600',
                    'offline': 'bg-red-600'
                };
                return classes[status] || 'bg-gray-600';
            };

            const getJobStatusClass = (status) => {
                const classes = {
                    'pending': 'bg-yellow-600',
                    'queued': 'bg-yellow-600',
                    'running': 'bg-blue-600',
                    'completed': 'bg-green-600',
                    'failed': 'bg-red-600',
                    'cancelled': 'bg-gray-600'
                };
                return classes[status] || 'bg-gray-600';
            };

            const fetchData = async () => {
                try {
                    const [statsRes, workersRes, jobsRes] = await Promise.all([
                        fetch('/api/v1/queue/stats'),
                        fetch('/api/v1/workers'),
                        fetch('/api/v1/jobs?limit=50')
                    ]);

                    if (statsRes.ok) stats.value = await statsRes.json();
                    if (workersRes.ok) workers.value = await workersRes.json();
                    if (jobsRes.ok) jobs.value = await jobsRes.json();

                    connected.value = true;
                } catch (error) {
                    console.error('Fetch error:', error);
                    connected.value = false;
                }
            };

            const createJob = async () => {
                if (!newJob.value.description) return;

                try {
                    const payload = {
                        description: newJob.value.description,
                        tech_stack: newJob.value.tech_stack || null,
                        features: newJob.value.featuresStr ? newJob.value.featuresStr.split(',').map(f => f.trim()) : []
                    };

                    const res = await fetch('/api/v1/jobs', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(payload)
                    });

                    if (res.ok) {
                        newJob.value = { description: '', tech_stack: '', featuresStr: '' };
                        await fetchData();
                    }
                } catch (error) {
                    console.error('Create job error:', error);
                }
            };

            const selectJob = (job) => {
                selectedJob.value = job;
            };

            const updateTime = () => {
                currentTime.value = new Date().toLocaleTimeString('pt-BR');
            };

            let interval;
            onMounted(() => {
                fetchData();
                updateTime();
                interval = setInterval(() => {
                    fetchData();
                    updateTime();
                }, 5000);
            });

            onUnmounted(() => {
                if (interval) clearInterval(interval);
            });

            return {
                stats, workers, jobs, connected, currentTime, statusFilter,
                selectedJob, newJob, activeWorkers, filteredJobs,
                getStatusClass, getJobStatusClass, createJob, selectJob
            };
        }
    }).mount('#app');
    </script>
</body>
</html>
"""


# =============================================================================
# ROUTES
# =============================================================================

@app.get("/", response_class=HTMLResponse)
async def dashboard():
    """Dashboard principal"""
    return DASHBOARD_HTML


@app.get("/api/status")
async def api_status():
    """Status geral da API"""
    return {
        "service": "factory-dashboard",
        "version": "4.0.0",
        "status": "running",
        "timestamp": datetime.utcnow().isoformat()
    }


# =============================================================================
# MAIN
# =============================================================================

def run():
    """Roda o dashboard"""
    import uvicorn
    uvicorn.run(
        app,
        host=DASHBOARD_HOST,
        port=DASHBOARD_PORT,
        reload=False
    )


if __name__ == "__main__":
    run()

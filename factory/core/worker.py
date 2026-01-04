"""
Claude Worker - Worker para processamento de Jobs v4.0
Usa Claude Agent SDK para executar autonomous loop

Plataforma E - Nova Arquitetura MVP
"""
import asyncio
import os
import socket
import time
import uuid
from datetime import datetime
from typing import Optional, Dict, Any
from pathlib import Path

from dotenv import load_dotenv
load_dotenv()

# Configuracoes
CLAUDE_MODEL = os.getenv("CLAUDE_MODEL", "claude-sonnet-4-20250514")
MAX_ATTEMPTS = int(os.getenv("AUTONOMOUS_LOOP_MAX_ATTEMPTS", 5))
WORKER_TIMEOUT = int(os.getenv("WORKER_TIMEOUT", 600))
PROJECTS_DIR = Path(os.getenv("PROJECTS_DIR", "projects"))


class ClaudeWorker:
    """
    Worker que processa jobs usando Claude API

    Cada worker:
    - Conecta na fila Redis
    - Pega jobs pendentes
    - Executa autonomous loop (generate -> lint -> test -> fix)
    - Atualiza status do job
    """

    def __init__(
        self,
        worker_id: str = None,
        model: str = None,
        mcp_tools: list = None
    ):
        # ID do worker (inclui UUID para garantir unicidade)
        self.worker_id = worker_id or f"worker-{socket.gethostname()}-{os.getpid()}-{uuid.uuid4().hex[:8]}"
        self.model = model or CLAUDE_MODEL
        self.mcp_tools = mcp_tools or ["filesystem", "bash"]

        # Estado
        self._running = False
        self._current_job = None
        self._queue = None
        self._client = None

        # Metricas
        self.jobs_processed = 0
        self.jobs_failed = 0
        self.start_time = None

    async def initialize(self):
        """Inicializa conexoes"""
        # Importar aqui para evitar circular imports
        from factory.core.job_queue import get_queue

        # Conectar fila
        self._queue = await get_queue()

        # Inicializar cliente Claude
        try:
            from anthropic import Anthropic
            self._client = Anthropic()
            print(f"[Worker {self.worker_id}] Cliente Claude inicializado")
        except Exception as e:
            print(f"[Worker {self.worker_id}] Erro ao inicializar Claude: {e}")
            raise

        # Registrar worker
        await self._queue.register_worker(
            self.worker_id,
            model=self.model,
            mcp_tools=self.mcp_tools,
            hostname=socket.gethostname(),
            ip_address=self._get_ip()
        )

        print(f"[Worker {self.worker_id}] Registrado no pool")

    def _get_ip(self) -> str:
        """Obtem IP local"""
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            s.connect(("8.8.8.8", 80))
            ip = s.getsockname()[0]
            s.close()
            return ip
        except Exception:
            return "127.0.0.1"

    async def start(self):
        """Inicia loop principal do worker"""
        await self.initialize()

        self._running = True
        self.start_time = datetime.utcnow()

        print(f"[Worker {self.worker_id}] Iniciando loop de processamento...")

        while self._running:
            try:
                # Heartbeat
                await self._queue.worker_heartbeat(self.worker_id)

                # Pegar proximo job
                job = await self._queue.dequeue(self.worker_id, timeout=5)

                if not job:
                    continue

                self._current_job = job
                print(f"[Worker {self.worker_id}] Processando job: {job['job_id']}")

                # Processar job
                start_time = time.time()
                result = await self._process_job(job)
                duration = int(time.time() - start_time)

                # Atualizar metricas
                await self._queue.update_worker_metrics(
                    self.worker_id,
                    duration,
                    result.get("success", False)
                )

                if result.get("success"):
                    self.jobs_processed += 1
                    print(f"[Worker {self.worker_id}] Job {job['job_id']} completado em {duration}s")
                else:
                    self.jobs_failed += 1
                    print(f"[Worker {self.worker_id}] Job {job['job_id']} falhou: {result.get('error')}")

                self._current_job = None

            except asyncio.CancelledError:
                print(f"[Worker {self.worker_id}] Cancelado")
                break
            except Exception as e:
                print(f"[Worker {self.worker_id}] Erro no loop: {e}")
                if self._current_job:
                    await self._queue.update_job_status(
                        self._current_job["job_id"],
                        "failed",
                        error=str(e)
                    )
                    self._current_job = None
                await asyncio.sleep(5)

        await self.stop()

    async def stop(self):
        """Para o worker"""
        self._running = False

        if self._queue:
            await self._queue.unregister_worker(self.worker_id)

        print(f"[Worker {self.worker_id}] Parado. Jobs processados: {self.jobs_processed}, Falhas: {self.jobs_failed}")

    async def _process_job(self, job: dict) -> dict:
        """
        Processa um job usando o autonomous loop

        Args:
            job: Dados do job

        Returns:
            {success: bool, output_path: str, error: str}
        """
        job_id = job["job_id"]

        try:
            # Importar autonomous loop
            from factory.core.autonomous_loop import AutonomousLoop, LoopConfig

            # Configurar loop
            config = LoopConfig(
                max_attempts=job.get("max_attempts", MAX_ATTEMPTS),
                project_base_dir=PROJECTS_DIR
            )

            loop = AutonomousLoop(config)

            # Callback para atualizar status do job
            async def on_step_update(step: str, progress: float, message: str):
                await self._queue.update_job_status(
                    job_id,
                    "running",
                    step=step,
                    progress=progress
                )
                await self._queue.add_job_log(job_id, step, message)

            # Executar loop
            result = await loop.run(
                job_id=job_id,
                description=job["description"],
                tech_stack=job.get("tech_stack"),
                features=job.get("features", []),
                claude_client=self._client,
                model=self.model,
                on_step_update=on_step_update
            )

            # Atualizar job com resultado
            if result["success"]:
                await self._queue.update_job_status(
                    job_id,
                    "completed",
                    step="completed",
                    result=result,
                    progress=100.0
                )
            else:
                await self._queue.update_job_status(
                    job_id,
                    "failed",
                    step="failed",
                    error=result.get("error", "Unknown error"),
                    result=result
                )

            return result

        except Exception as e:
            error_msg = str(e)
            await self._queue.update_job_status(
                job_id,
                "failed",
                step="failed",
                error=error_msg
            )
            return {"success": False, "error": error_msg}


class WorkerPool:
    """
    Pool de workers

    Gerencia multiplos workers rodando em paralelo
    """

    def __init__(self, num_workers: int = None):
        self.num_workers = num_workers or int(os.getenv("MAX_WORKERS", 3))
        self.workers: list[ClaudeWorker] = []
        self._tasks: list[asyncio.Task] = []

    async def start(self):
        """Inicia pool de workers"""
        print(f"[Pool] Iniciando {self.num_workers} workers...")

        for i in range(self.num_workers):
            worker = ClaudeWorker(
                worker_id=f"worker-{socket.gethostname()}-{i+1}"
            )
            self.workers.append(worker)

            task = asyncio.create_task(worker.start())
            self._tasks.append(task)

        print(f"[Pool] {self.num_workers} workers iniciados")

        # Aguardar todos os workers
        await asyncio.gather(*self._tasks, return_exceptions=True)

    async def stop(self):
        """Para todos os workers"""
        print("[Pool] Parando workers...")

        for worker in self.workers:
            worker._running = False

        for task in self._tasks:
            task.cancel()

        await asyncio.gather(*self._tasks, return_exceptions=True)

        print("[Pool] Todos os workers parados")

    def get_status(self) -> dict:
        """Retorna status do pool"""
        return {
            "num_workers": self.num_workers,
            "workers": [
                {
                    "worker_id": w.worker_id,
                    "running": w._running,
                    "current_job": w._current_job["job_id"] if w._current_job else None,
                    "jobs_processed": w.jobs_processed,
                    "jobs_failed": w.jobs_failed
                }
                for w in self.workers
            ]
        }


# =============================================================================
# CLI
# =============================================================================

async def run_single_worker():
    """Roda um unico worker"""
    worker = ClaudeWorker()
    try:
        await worker.start()
    except KeyboardInterrupt:
        await worker.stop()


async def run_worker_pool(num_workers: int = None):
    """Roda pool de workers"""
    pool = WorkerPool(num_workers)
    try:
        await pool.start()
    except KeyboardInterrupt:
        await pool.stop()


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1:
        num = int(sys.argv[1])
        print(f"Iniciando pool com {num} workers...")
        asyncio.run(run_worker_pool(num))
    else:
        print("Iniciando worker unico...")
        asyncio.run(run_single_worker())

"""
Job Queue - Sistema de Fila de Jobs v4.0
Redis-based Queue com fallback para SQLite

Plataforma E - Nova Arquitetura MVP
"""
import asyncio
import json
import os
from datetime import datetime
from typing import Optional, List, Callable, Any, Dict
from dataclasses import dataclass
from enum import Enum
from uuid import uuid4

from dotenv import load_dotenv
load_dotenv()


class QueueBackend(str, Enum):
    """Backend da fila"""
    SQLITE = "sqlite"
    REDIS = "redis"


@dataclass
class QueueConfig:
    """Configuracao da fila"""
    backend: QueueBackend = QueueBackend.REDIS
    redis_url: str = os.getenv("REDIS_URL", "redis://localhost:6379")
    max_workers: int = int(os.getenv("MAX_WORKERS", 5))
    job_timeout: int = int(os.getenv("WORKER_TIMEOUT", 600))
    poll_interval: float = 1.0


class RedisJobQueue:
    """
    Redis-based Job Queue

    Usa:
    - Redis List para fila FIFO
    - Redis Hash para dados dos jobs
    - Redis Pub/Sub para eventos em tempo real
    - Redis Hash para workers
    """

    # Keys Redis
    QUEUE_KEY = "fabrica:jobs:queue"
    JOBS_HASH = "fabrica:jobs:data"
    WORKERS_HASH = "fabrica:workers"
    CHANNEL_PREFIX = "fabrica:events:"

    def __init__(self, config: QueueConfig = None):
        self.config = config or QueueConfig()
        self._redis = None
        self._pubsub = None
        self._callbacks: Dict[str, List[Callable]] = {
            "job_queued": [],
            "job_started": [],
            "job_completed": [],
            "job_failed": [],
            "job_cancelled": []
        }

    async def connect(self):
        """Conecta ao Redis"""
        if self._redis is None:
            try:
                import redis.asyncio as aioredis
                self._redis = aioredis.from_url(
                    self.config.redis_url,
                    encoding="utf-8",
                    decode_responses=True
                )
                await self._redis.ping()
                print(f"[Queue] Conectado ao Redis: {self.config.redis_url}")
            except Exception as e:
                print(f"[Queue] Erro ao conectar Redis: {e}")
                raise

    async def disconnect(self):
        """Desconecta do Redis"""
        if self._redis:
            await self._redis.close()
            self._redis = None

    @property
    def redis(self):
        """Retorna cliente Redis"""
        if self._redis is None:
            raise RuntimeError("Redis nao conectado. Chame connect() primeiro.")
        return self._redis

    # =========================================================================
    # JOB OPERATIONS
    # =========================================================================

    async def enqueue(self, job_data: dict) -> dict:
        """
        Adiciona job na fila

        Args:
            job_data: {
                description: str,
                tech_stack: str,
                features: list,
                project_id: str (opcional),
                created_by: str (opcional)
            }

        Returns:
            Job criado
        """
        # Gerar ID unico
        timestamp = datetime.utcnow().strftime("%Y%m%d%H%M%S")
        job_id = f"JOB-{timestamp}-{uuid4().hex[:6].upper()}"

        # Criar job
        job = {
            "job_id": job_id,
            "description": job_data.get("description", ""),
            "tech_stack": job_data.get("tech_stack"),
            "features": job_data.get("features", []),
            "project_id": job_data.get("project_id"),
            "created_by": job_data.get("created_by"),
            "status": "pending",
            "current_step": "queued",
            "progress": 0.0,
            "current_attempt": 0,
            "max_attempts": 5,
            "total_iterations": 0,
            "worker_id": None,
            "result": {},
            "output_path": None,
            "github_url": None,
            "artifacts": [],
            "error_message": None,
            "step_logs": [],
            "queued_at": datetime.utcnow().isoformat(),
            "started_at": None,
            "completed_at": None,
            "created_at": datetime.utcnow().isoformat()
        }

        # Salvar no Hash
        await self.redis.hset(self.JOBS_HASH, job_id, json.dumps(job))

        # Adicionar na fila (RPUSH para FIFO)
        await self.redis.rpush(self.QUEUE_KEY, job_id)

        # Publicar evento
        await self._publish("job_queued", job)

        # Trigger callbacks locais
        self._trigger("job_queued", job)

        return job

    async def dequeue(self, worker_id: str, timeout: int = 5) -> Optional[dict]:
        """
        Remove proximo job da fila e atribui ao worker

        Args:
            worker_id: ID do worker
            timeout: Tempo de espera em segundos

        Returns:
            Job ou None se fila vazia
        """
        # BLPOP com timeout (blocking)
        result = await self.redis.blpop(self.QUEUE_KEY, timeout=timeout)

        if not result:
            return None

        job_id = result[1]

        # Buscar dados do job
        job_data = await self.redis.hget(self.JOBS_HASH, job_id)
        if not job_data:
            return None

        job = json.loads(job_data)

        # Atualizar job
        job["status"] = "running"
        job["worker_id"] = worker_id
        job["started_at"] = datetime.utcnow().isoformat()

        # Salvar
        await self.redis.hset(self.JOBS_HASH, job_id, json.dumps(job))

        # Atualizar worker
        await self._update_worker_status(worker_id, "busy", job_id)

        # Publicar evento
        await self._publish("job_started", job)
        self._trigger("job_started", job)

        return job

    async def get_job(self, job_id: str) -> Optional[dict]:
        """Busca job por ID"""
        job_data = await self.redis.hget(self.JOBS_HASH, job_id)
        if not job_data:
            return None
        return json.loads(job_data)

    async def update_job(self, job_id: str, updates: dict) -> Optional[dict]:
        """Atualiza campos do job"""
        job_data = await self.redis.hget(self.JOBS_HASH, job_id)
        if not job_data:
            return None

        job = json.loads(job_data)
        job.update(updates)
        job["updated_at"] = datetime.utcnow().isoformat()

        await self.redis.hset(self.JOBS_HASH, job_id, json.dumps(job))
        return job

    async def update_job_status(
        self,
        job_id: str,
        status: str,
        step: str = None,
        error: str = None,
        result: dict = None,
        progress: float = None
    ) -> Optional[dict]:
        """
        Atualiza status do job

        Args:
            job_id: ID do job
            status: Novo status (pending, queued, running, completed, failed, cancelled)
            step: Step atual (opcional)
            error: Mensagem de erro (opcional)
            result: Resultado (opcional)
            progress: Progresso 0-100 (opcional)
        """
        job = await self.get_job(job_id)
        if not job:
            return None

        # Atualizar campos
        job["status"] = status
        if step:
            job["current_step"] = step
        if error:
            job["error_message"] = error
        if result:
            job["result"] = result
        if progress is not None:
            job["progress"] = progress

        # Timestamps
        if status == "completed":
            job["completed_at"] = datetime.utcnow().isoformat()
            job["progress"] = 100.0
        elif status == "failed":
            job["completed_at"] = datetime.utcnow().isoformat()

        # Salvar
        await self.redis.hset(self.JOBS_HASH, job_id, json.dumps(job))

        # Liberar worker se concluido ou falhou
        if status in ["completed", "failed", "cancelled"]:
            if job.get("worker_id"):
                await self._update_worker_status(job["worker_id"], "idle", None)

        # Publicar eventos
        if status == "completed":
            await self._publish("job_completed", job)
            self._trigger("job_completed", job)
        elif status == "failed":
            await self._publish("job_failed", job)
            self._trigger("job_failed", job)
        elif status == "cancelled":
            await self._publish("job_cancelled", job)
            self._trigger("job_cancelled", job)

        return job

    async def add_job_log(self, job_id: str, step: str, message: str, success: bool = True):
        """Adiciona log ao job"""
        job = await self.get_job(job_id)
        if not job:
            return

        log_entry = {
            "step": step,
            "message": message,
            "success": success,
            "timestamp": datetime.utcnow().isoformat()
        }

        if not job.get("step_logs"):
            job["step_logs"] = []
        job["step_logs"].append(log_entry)

        await self.redis.hset(self.JOBS_HASH, job_id, json.dumps(job))

    async def cancel_job(self, job_id: str) -> bool:
        """Cancela um job pendente"""
        job = await self.get_job(job_id)
        if not job:
            return False

        if job["status"] not in ["pending", "queued"]:
            return False

        # Remover da fila se estiver la
        await self.redis.lrem(self.QUEUE_KEY, 0, job_id)

        # Atualizar status
        await self.update_job_status(job_id, "cancelled")
        return True

    async def list_jobs(
        self,
        status: str = None,
        limit: int = 50,
        offset: int = 0
    ) -> List[dict]:
        """Lista jobs com filtros"""
        all_jobs = await self.redis.hgetall(self.JOBS_HASH)

        jobs = []
        for job_data in all_jobs.values():
            job = json.loads(job_data)
            if status and job.get("status") != status:
                continue
            jobs.append(job)

        # Ordenar por data de criacao (mais recente primeiro)
        jobs.sort(key=lambda x: x.get("created_at", ""), reverse=True)

        # Paginar
        return jobs[offset:offset + limit]

    # =========================================================================
    # QUEUE STATISTICS
    # =========================================================================

    async def get_stats(self) -> dict:
        """Retorna estatisticas da fila"""
        all_jobs = await self.redis.hgetall(self.JOBS_HASH)
        queue_length = await self.redis.llen(self.QUEUE_KEY)

        stats = {
            "pending": 0,
            "queued": 0,
            "running": 0,
            "completed": 0,
            "failed": 0,
            "cancelled": 0,
            "total": 0,
            "queue_length": queue_length
        }

        for job_data in all_jobs.values():
            job = json.loads(job_data)
            status = job.get("status", "pending")
            stats[status] = stats.get(status, 0) + 1
            stats["total"] += 1

        return stats

    async def peek(self, limit: int = 10) -> List[dict]:
        """Lista jobs na fila sem remover"""
        job_ids = await self.redis.lrange(self.QUEUE_KEY, 0, limit - 1)

        jobs = []
        for job_id in job_ids:
            job = await self.get_job(job_id)
            if job:
                jobs.append(job)

        return jobs

    # =========================================================================
    # WORKER MANAGEMENT
    # =========================================================================

    async def register_worker(self, worker_id: str, **kwargs) -> dict:
        """Registra um worker no pool"""
        worker = {
            "worker_id": worker_id,
            "status": "idle",
            "current_job_id": None,
            "model": kwargs.get("model", "claude-sonnet-4-20250514"),
            "mcp_tools": kwargs.get("mcp_tools", ["filesystem", "bash"]),
            "jobs_completed": 0,
            "jobs_failed": 0,
            "total_processing_time": 0,
            "avg_job_duration": 0.0,
            "hostname": kwargs.get("hostname"),
            "ip_address": kwargs.get("ip_address"),
            "started_at": datetime.utcnow().isoformat(),
            "last_heartbeat": datetime.utcnow().isoformat()
        }

        await self.redis.hset(self.WORKERS_HASH, worker_id, json.dumps(worker))
        return worker

    async def _update_worker_status(
        self,
        worker_id: str,
        status: str,
        job_id: str = None
    ):
        """Atualiza status do worker"""
        worker_data = await self.redis.hget(self.WORKERS_HASH, worker_id)
        if not worker_data:
            return

        worker = json.loads(worker_data)
        worker["status"] = status
        worker["current_job_id"] = job_id
        worker["last_heartbeat"] = datetime.utcnow().isoformat()

        await self.redis.hset(self.WORKERS_HASH, worker_id, json.dumps(worker))

    async def worker_heartbeat(self, worker_id: str):
        """Atualiza heartbeat do worker"""
        worker_data = await self.redis.hget(self.WORKERS_HASH, worker_id)
        if not worker_data:
            return

        worker = json.loads(worker_data)
        worker["last_heartbeat"] = datetime.utcnow().isoformat()
        await self.redis.hset(self.WORKERS_HASH, worker_id, json.dumps(worker))

    async def update_worker_metrics(
        self,
        worker_id: str,
        job_duration: int,
        success: bool
    ):
        """Atualiza metricas do worker apos completar job"""
        worker_data = await self.redis.hget(self.WORKERS_HASH, worker_id)
        if not worker_data:
            return

        worker = json.loads(worker_data)

        if success:
            worker["jobs_completed"] += 1
        else:
            worker["jobs_failed"] += 1

        worker["total_processing_time"] += job_duration
        total_jobs = worker["jobs_completed"] + worker["jobs_failed"]
        if total_jobs > 0:
            worker["avg_job_duration"] = worker["total_processing_time"] / total_jobs

        await self.redis.hset(self.WORKERS_HASH, worker_id, json.dumps(worker))

    async def unregister_worker(self, worker_id: str):
        """Remove worker do pool"""
        await self.redis.hdel(self.WORKERS_HASH, worker_id)

    async def get_workers(self) -> List[dict]:
        """Lista todos os workers"""
        all_workers = await self.redis.hgetall(self.WORKERS_HASH)
        return [json.loads(w) for w in all_workers.values()]

    async def get_worker(self, worker_id: str) -> Optional[dict]:
        """Busca worker por ID"""
        worker_data = await self.redis.hget(self.WORKERS_HASH, worker_id)
        if not worker_data:
            return None
        return json.loads(worker_data)

    async def get_active_workers(self) -> List[dict]:
        """Lista workers ativos (nao offline)"""
        workers = await self.get_workers()
        return [w for w in workers if w.get("status") != "offline"]

    # =========================================================================
    # PUB/SUB
    # =========================================================================

    async def _publish(self, event: str, data: dict):
        """Publica evento no Redis"""
        channel = f"{self.CHANNEL_PREFIX}{event}"
        await self.redis.publish(channel, json.dumps(data))

    async def subscribe(self, event: str, callback: Callable):
        """Inscreve em eventos do Redis"""
        if self._pubsub is None:
            self._pubsub = self.redis.pubsub()

        channel = f"{self.CHANNEL_PREFIX}{event}"
        await self._pubsub.subscribe(channel)

        # Adicionar callback local tambem
        self.on(event, callback)

    # =========================================================================
    # LOCAL CALLBACKS
    # =========================================================================

    def on(self, event: str, callback: Callable):
        """Registra callback local para evento"""
        if event in self._callbacks:
            self._callbacks[event].append(callback)

    def _trigger(self, event: str, data: Any):
        """Dispara callbacks locais"""
        for callback in self._callbacks.get(event, []):
            try:
                if asyncio.iscoroutinefunction(callback):
                    asyncio.create_task(callback(data))
                else:
                    callback(data)
            except Exception as e:
                print(f"[Queue] Erro em callback {event}: {e}")


# =============================================================================
# FALLBACK SQLITE QUEUE (para desenvolvimento sem Redis)
# =============================================================================

class SQLiteJobQueue:
    """
    Fallback Queue usando SQLite
    Usado quando Redis nao esta disponivel
    """

    def __init__(self, config: QueueConfig = None):
        self.config = config or QueueConfig()
        self._callbacks: Dict[str, List[Callable]] = {
            "job_queued": [],
            "job_started": [],
            "job_completed": [],
            "job_failed": [],
            "job_cancelled": []
        }

    async def connect(self):
        """Inicializa SQLite"""
        from factory.database.connection import init_db
        init_db()
        print("[Queue] Usando SQLite como fallback")

    async def disconnect(self):
        pass

    async def enqueue(self, job_data: dict) -> dict:
        """Adiciona job na fila"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job, JobStatus, JobStep

        db = SessionLocal()
        try:
            timestamp = datetime.utcnow().strftime("%Y%m%d%H%M%S")
            job_id = f"JOB-{timestamp}-{uuid4().hex[:6].upper()}"

            job = Job(
                job_id=job_id,
                description=job_data.get("description", ""),
                tech_stack=job_data.get("tech_stack"),
                features=job_data.get("features", []),
                project_id=job_data.get("project_id"),
                created_by=job_data.get("created_by"),
                status=JobStatus.PENDING.value,
                current_step=JobStep.QUEUED.value,
                queued_at=datetime.utcnow()
            )

            db.add(job)
            db.commit()
            db.refresh(job)

            result = job.to_dict()
            self._trigger("job_queued", result)
            return result
        finally:
            db.close()

    async def dequeue(self, worker_id: str, timeout: int = 5) -> Optional[dict]:
        """Remove proximo job da fila"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job, JobStatus, JobStep

        db = SessionLocal()
        try:
            job = db.query(Job).filter(
                Job.status == JobStatus.PENDING.value
            ).order_by(Job.queued_at).first()

            if not job:
                return None

            job.status = JobStatus.RUNNING.value
            job.worker_id = worker_id
            job.started_at = datetime.utcnow()
            db.commit()
            db.refresh(job)

            result = job.to_dict()
            self._trigger("job_started", result)
            return result
        finally:
            db.close()

    async def get_job(self, job_id: str) -> Optional[dict]:
        """Busca job por ID"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            job = db.query(Job).filter(Job.job_id == job_id).first()
            return job.to_dict() if job else None
        finally:
            db.close()

    async def update_job_status(
        self,
        job_id: str,
        status: str,
        step: str = None,
        error: str = None,
        result: dict = None,
        progress: float = None
    ) -> Optional[dict]:
        """Atualiza status do job"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            job = db.query(Job).filter(Job.job_id == job_id).first()
            if not job:
                return None

            job.status = status
            if step:
                job.current_step = step
            if error:
                job.error_message = error
            if result:
                job.result = result
            if progress is not None:
                job.progress = progress

            if status in ["completed", "failed"]:
                job.completed_at = datetime.utcnow()
                if status == "completed":
                    job.progress = 100.0

            db.commit()
            db.refresh(job)

            result_dict = job.to_dict()

            if status == "completed":
                self._trigger("job_completed", result_dict)
            elif status == "failed":
                self._trigger("job_failed", result_dict)

            return result_dict
        finally:
            db.close()

    async def get_stats(self) -> dict:
        """Retorna estatisticas da fila"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job
        from sqlalchemy import func

        db = SessionLocal()
        try:
            counts = db.query(Job.status, func.count(Job.id)).group_by(Job.status).all()
            stats = {status: count for status, count in counts}

            return {
                "pending": stats.get("pending", 0),
                "queued": stats.get("queued", 0),
                "running": stats.get("running", 0),
                "completed": stats.get("completed", 0),
                "failed": stats.get("failed", 0),
                "cancelled": stats.get("cancelled", 0),
                "total": sum(stats.values()),
                "queue_length": stats.get("pending", 0) + stats.get("queued", 0)
            }
        finally:
            db.close()

    async def list_jobs(
        self,
        status: str = None,
        limit: int = 50,
        offset: int = 0
    ) -> List[dict]:
        """Lista jobs com filtros"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            query = db.query(Job)
            if status:
                query = query.filter(Job.status == status)
            query = query.order_by(Job.created_at.desc())
            jobs = query.offset(offset).limit(limit).all()
            return [job.to_dict() for job in jobs]
        finally:
            db.close()

    async def update_job(self, job_id: str, updates: dict) -> Optional[dict]:
        """Atualiza campos do job"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            job = db.query(Job).filter(Job.job_id == job_id).first()
            if not job:
                return None

            for key, value in updates.items():
                if hasattr(job, key):
                    setattr(job, key, value)

            db.commit()
            db.refresh(job)
            return job.to_dict()
        finally:
            db.close()

    async def cancel_job(self, job_id: str) -> bool:
        """Cancela um job pendente"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            job = db.query(Job).filter(Job.job_id == job_id).first()
            if not job:
                return False

            if job.status not in ["pending", "queued"]:
                return False

            job.status = "cancelled"
            job.completed_at = datetime.utcnow()
            db.commit()

            self._trigger("job_cancelled", job.to_dict())
            return True
        finally:
            db.close()

    async def add_job_log(self, job_id: str, step: str, message: str, success: bool = True):
        """Adiciona log ao job"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Job

        db = SessionLocal()
        try:
            job = db.query(Job).filter(Job.job_id == job_id).first()
            if not job:
                return

            log_entry = {
                "step": step,
                "message": message,
                "success": success,
                "timestamp": datetime.utcnow().isoformat()
            }

            step_logs = job.step_logs or []
            step_logs.append(log_entry)
            job.step_logs = step_logs

            db.commit()
        finally:
            db.close()

    async def get_workers(self) -> List[dict]:
        """Lista workers"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            workers = db.query(Worker).all()
            return [w.to_dict() for w in workers]
        finally:
            db.close()

    async def get_worker(self, worker_id: str) -> Optional[dict]:
        """Busca worker por ID"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            worker = db.query(Worker).filter(Worker.worker_id == worker_id).first()
            return worker.to_dict() if worker else None
        finally:
            db.close()

    async def get_active_workers(self) -> List[dict]:
        """Lista workers ativos"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            workers = db.query(Worker).filter(Worker.status != "offline").all()
            return [w.to_dict() for w in workers]
        finally:
            db.close()

    async def register_worker(self, worker_id: str, **kwargs) -> dict:
        """Registra um worker no pool"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            worker = Worker(
                worker_id=worker_id,
                status="idle",
                model=kwargs.get("model", "claude-sonnet-4-20250514"),
                hostname=kwargs.get("hostname"),
                ip_address=kwargs.get("ip_address"),
                started_at=datetime.utcnow(),
                last_heartbeat=datetime.utcnow()
            )
            db.add(worker)
            db.commit()
            db.refresh(worker)
            return worker.to_dict()
        finally:
            db.close()

    async def worker_heartbeat(self, worker_id: str):
        """Atualiza heartbeat do worker"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            worker = db.query(Worker).filter(Worker.worker_id == worker_id).first()
            if worker:
                worker.last_heartbeat = datetime.utcnow()
                db.commit()
        finally:
            db.close()

    async def update_worker_metrics(self, worker_id: str, job_duration: int, success: bool):
        """Atualiza metricas do worker"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            worker = db.query(Worker).filter(Worker.worker_id == worker_id).first()
            if worker:
                if success:
                    worker.jobs_completed = (worker.jobs_completed or 0) + 1
                else:
                    worker.jobs_failed = (worker.jobs_failed or 0) + 1
                worker.total_processing_time = (worker.total_processing_time or 0) + job_duration
                total = (worker.jobs_completed or 0) + (worker.jobs_failed or 0)
                if total > 0:
                    worker.avg_job_duration = worker.total_processing_time / total
                db.commit()
        finally:
            db.close()

    async def unregister_worker(self, worker_id: str):
        """Remove worker do pool"""
        from factory.database.connection import SessionLocal
        from factory.database.models import Worker

        db = SessionLocal()
        try:
            db.query(Worker).filter(Worker.worker_id == worker_id).delete()
            db.commit()
        finally:
            db.close()

    def on(self, event: str, callback: Callable):
        """Registra callback"""
        if event in self._callbacks:
            self._callbacks[event].append(callback)

    def _trigger(self, event: str, data: Any):
        """Dispara callbacks"""
        for callback in self._callbacks.get(event, []):
            try:
                callback(data)
            except Exception as e:
                print(f"[Queue] Erro em callback {event}: {e}")


# =============================================================================
# FACTORY FUNCTION
# =============================================================================

_queue_instance = None


async def get_queue(config: QueueConfig = None) -> RedisJobQueue:
    """
    Retorna instancia da fila

    Tenta usar Redis, fallback para SQLite
    """
    global _queue_instance

    if _queue_instance is not None:
        return _queue_instance

    config = config or QueueConfig()

    # Tentar Redis primeiro
    if config.backend == QueueBackend.REDIS:
        try:
            queue = RedisJobQueue(config)
            await queue.connect()
            _queue_instance = queue
            return queue
        except Exception as e:
            print(f"[Queue] Redis nao disponivel ({e}), usando SQLite")

    # Fallback para SQLite
    queue = SQLiteJobQueue(config)
    await queue.connect()
    _queue_instance = queue
    return queue


def get_queue_sync(config: QueueConfig = None):
    """Versao sincrona para scripts"""
    return asyncio.get_event_loop().run_until_complete(get_queue(config))


# =============================================================================
# CLI TEST
# =============================================================================

if __name__ == "__main__":
    async def main():
        queue = await get_queue()

        # Enfileirar job de teste
        job = await queue.enqueue({
            "description": "Criar API REST para gerenciamento de tarefas",
            "tech_stack": "python, fastapi, sqlite",
            "features": ["CRUD de tarefas", "Autenticacao JWT", "Documentacao OpenAPI"],
            "created_by": "test"
        })

        print(f"Job criado: {job['job_id']}")
        print(f"Status: {job['status']}")
        print(f"Stats: {await queue.get_stats()}")

        await queue.disconnect()

    asyncio.run(main())

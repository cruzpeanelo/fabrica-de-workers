# -*- coding: utf-8 -*-
"""
Sync Scheduler
==============
Agendamento de sincronizacoes periodicas.

Issue #364 - Terminal A
"""

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable, Dict, List, Optional
from uuid import uuid4

logger = logging.getLogger(__name__)


class JobStatus(str, Enum):
    """Status do job de sincronizacao"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class JobFrequency(str, Enum):
    """Frequencia de execucao"""
    ONCE = "once"
    MINUTES = "minutes"
    HOURLY = "hourly"
    DAILY = "daily"
    WEEKLY = "weekly"


@dataclass
class SyncJob:
    """
    Job de sincronizacao agendado.

    Attributes:
        job_id: ID unico do job
        name: Nome descritivo
        frequency: Frequencia de execucao
        interval: Intervalo (para MINUTES)
        tenant_id: Tenant associado
        project_id: Projeto associado (opcional)
        jql_filter: Filtro JQL para sincronizacao
        enabled: Se o job esta ativo
        last_run: Ultima execucao
        next_run: Proxima execucao
        status: Status atual
        run_count: Numero de execucoes
        error_count: Numero de erros
        last_error: Ultimo erro
    """
    job_id: str = field(default_factory=lambda: str(uuid4()))
    name: str = "Sync Job"
    frequency: JobFrequency = JobFrequency.HOURLY
    interval: int = 60  # Minutos (para MINUTES frequency)
    tenant_id: Optional[str] = None
    project_id: Optional[str] = None
    jql_filter: Optional[str] = None
    enabled: bool = True
    last_run: Optional[datetime] = None
    next_run: Optional[datetime] = None
    status: JobStatus = JobStatus.PENDING
    run_count: int = 0
    error_count: int = 0
    last_error: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)

    def calculate_next_run(self) -> datetime:
        """Calcula proxima execucao baseado na frequencia"""
        now = datetime.utcnow()

        if self.frequency == JobFrequency.ONCE:
            return now

        elif self.frequency == JobFrequency.MINUTES:
            return now + timedelta(minutes=self.interval)

        elif self.frequency == JobFrequency.HOURLY:
            return now + timedelta(hours=1)

        elif self.frequency == JobFrequency.DAILY:
            return now + timedelta(days=1)

        elif self.frequency == JobFrequency.WEEKLY:
            return now + timedelta(weeks=1)

        return now

    def to_dict(self) -> Dict[str, Any]:
        return {
            "job_id": self.job_id,
            "name": self.name,
            "frequency": self.frequency.value,
            "interval": self.interval,
            "tenant_id": self.tenant_id,
            "project_id": self.project_id,
            "jql_filter": self.jql_filter,
            "enabled": self.enabled,
            "last_run": self.last_run.isoformat() if self.last_run else None,
            "next_run": self.next_run.isoformat() if self.next_run else None,
            "status": self.status.value,
            "run_count": self.run_count,
            "error_count": self.error_count,
            "last_error": self.last_error,
            "created_at": self.created_at.isoformat()
        }


@dataclass
class JobResult:
    """Resultado de execucao de job"""
    job_id: str
    success: bool
    started_at: datetime
    completed_at: datetime
    items_synced: int = 0
    items_failed: int = 0
    conflicts_detected: int = 0
    conflicts_resolved: int = 0
    error: Optional[str] = None

    @property
    def duration_seconds(self) -> float:
        return (self.completed_at - self.started_at).total_seconds()


class SyncScheduler:
    """
    Agendador de sincronizacoes.

    Exemplo:
        scheduler = SyncScheduler()

        # Criar job de sincronizacao
        job = SyncJob(
            name="Sync Projeto ABC",
            frequency=JobFrequency.HOURLY,
            tenant_id="tenant-123",
            jql_filter="project = ABC"
        )
        scheduler.add_job(job)

        # Iniciar scheduler
        await scheduler.start()

        # Executar job manualmente
        result = await scheduler.run_job(job.job_id)
    """

    def __init__(self, sync_callback: Optional[Callable] = None):
        """
        Args:
            sync_callback: Funcao async a ser chamada para sincronizacao.
                          Assinatura: async def callback(job: SyncJob) -> JobResult
        """
        self._jobs: Dict[str, SyncJob] = {}
        self._results: List[JobResult] = []
        self._sync_callback = sync_callback
        self._running = False
        self._task: Optional[asyncio.Task] = None
        self._check_interval = 60  # Segundos entre verificacoes

    def add_job(self, job: SyncJob) -> SyncJob:
        """Adiciona job ao scheduler"""
        if job.next_run is None:
            job.next_run = job.calculate_next_run()

        self._jobs[job.job_id] = job
        logger.info(f"Job adicionado: {job.name} ({job.job_id})")
        return job

    def remove_job(self, job_id: str) -> bool:
        """Remove job do scheduler"""
        if job_id in self._jobs:
            del self._jobs[job_id]
            logger.info(f"Job removido: {job_id}")
            return True
        return False

    def get_job(self, job_id: str) -> Optional[SyncJob]:
        """Obtem job por ID"""
        return self._jobs.get(job_id)

    def get_jobs(self, tenant_id: Optional[str] = None) -> List[SyncJob]:
        """Lista jobs, opcionalmente filtrados por tenant"""
        jobs = list(self._jobs.values())
        if tenant_id:
            jobs = [j for j in jobs if j.tenant_id == tenant_id]
        return jobs

    def enable_job(self, job_id: str) -> bool:
        """Ativa job"""
        job = self._jobs.get(job_id)
        if job:
            job.enabled = True
            job.next_run = job.calculate_next_run()
            return True
        return False

    def disable_job(self, job_id: str) -> bool:
        """Desativa job"""
        job = self._jobs.get(job_id)
        if job:
            job.enabled = False
            return True
        return False

    async def run_job(self, job_id: str) -> Optional[JobResult]:
        """Executa job manualmente"""
        job = self._jobs.get(job_id)
        if not job:
            logger.error(f"Job nao encontrado: {job_id}")
            return None

        return await self._execute_job(job)

    async def _execute_job(self, job: SyncJob) -> JobResult:
        """Executa um job de sincronizacao"""
        started_at = datetime.utcnow()
        job.status = JobStatus.RUNNING
        job.last_run = started_at

        logger.info(f"Executando job: {job.name} ({job.job_id})")

        try:
            if self._sync_callback:
                result = await self._sync_callback(job)
            else:
                # Simulacao se nao houver callback
                result = JobResult(
                    job_id=job.job_id,
                    success=True,
                    started_at=started_at,
                    completed_at=datetime.utcnow(),
                    items_synced=0
                )

            job.status = JobStatus.COMPLETED
            job.run_count += 1
            job.last_error = None

            # Agenda proxima execucao
            if job.frequency != JobFrequency.ONCE:
                job.next_run = job.calculate_next_run()
            else:
                job.enabled = False

            logger.info(
                f"Job concluido: {job.name} - "
                f"{result.items_synced} itens sincronizados em "
                f"{result.duration_seconds:.2f}s"
            )

        except Exception as e:
            result = JobResult(
                job_id=job.job_id,
                success=False,
                started_at=started_at,
                completed_at=datetime.utcnow(),
                error=str(e)
            )

            job.status = JobStatus.FAILED
            job.error_count += 1
            job.last_error = str(e)

            # Reagenda mesmo em caso de erro
            if job.frequency != JobFrequency.ONCE:
                job.next_run = job.calculate_next_run()

            logger.error(f"Job falhou: {job.name} - {e}")

        self._results.append(result)
        return result

    async def start(self):
        """Inicia o loop do scheduler"""
        if self._running:
            logger.warning("Scheduler ja esta rodando")
            return

        self._running = True
        self._task = asyncio.create_task(self._scheduler_loop())
        logger.info("Scheduler iniciado")

    async def stop(self):
        """Para o scheduler"""
        self._running = False
        if self._task:
            self._task.cancel()
            try:
                await self._task
            except asyncio.CancelledError:
                pass
        logger.info("Scheduler parado")

    async def _scheduler_loop(self):
        """Loop principal do scheduler"""
        while self._running:
            try:
                await self._check_jobs()
                await asyncio.sleep(self._check_interval)
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Erro no scheduler loop: {e}")
                await asyncio.sleep(self._check_interval)

    async def _check_jobs(self):
        """Verifica jobs pendentes de execucao"""
        now = datetime.utcnow()

        for job in self._jobs.values():
            if not job.enabled:
                continue

            if job.status == JobStatus.RUNNING:
                continue

            if job.next_run and job.next_run <= now:
                asyncio.create_task(self._execute_job(job))

    def get_results(
        self,
        job_id: Optional[str] = None,
        limit: int = 100
    ) -> List[JobResult]:
        """Retorna historico de resultados"""
        results = self._results
        if job_id:
            results = [r for r in results if r.job_id == job_id]
        return results[-limit:]

    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatisticas do scheduler"""
        total_jobs = len(self._jobs)
        enabled_jobs = sum(1 for j in self._jobs.values() if j.enabled)
        total_runs = sum(j.run_count for j in self._jobs.values())
        total_errors = sum(j.error_count for j in self._jobs.values())

        recent_results = self._results[-100:]
        success_rate = 0.0
        if recent_results:
            successes = sum(1 for r in recent_results if r.success)
            success_rate = (successes / len(recent_results)) * 100

        return {
            "running": self._running,
            "total_jobs": total_jobs,
            "enabled_jobs": enabled_jobs,
            "total_runs": total_runs,
            "total_errors": total_errors,
            "success_rate": round(success_rate, 2),
            "recent_results": len(recent_results)
        }


# Singleton global
_scheduler: Optional[SyncScheduler] = None


def get_scheduler(sync_callback: Optional[Callable] = None) -> SyncScheduler:
    """
    Obtem instancia global do scheduler.

    Args:
        sync_callback: Callback de sincronizacao (apenas na primeira chamada)

    Returns:
        Instancia do SyncScheduler
    """
    global _scheduler
    if _scheduler is None:
        _scheduler = SyncScheduler(sync_callback=sync_callback)
    return _scheduler


def reset_scheduler():
    """Reseta scheduler global (para testes)"""
    global _scheduler
    _scheduler = None

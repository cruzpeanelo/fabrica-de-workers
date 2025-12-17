"""
Core Package v4.0 - Fabrica de Workers
Componentes centrais do sistema
"""
from .job_queue import get_queue, RedisJobQueue, SQLiteJobQueue
from .worker import ClaudeWorker, WorkerPool
from .autonomous_loop import AutonomousLoop

__all__ = [
    "get_queue",
    "RedisJobQueue",
    "SQLiteJobQueue",
    "ClaudeWorker",
    "WorkerPool",
    "AutonomousLoop"
]

# -*- coding: utf-8 -*-
"""
Sistema de Logs Estruturados - Fabrica de Agentes v4.0
======================================================

Configuracao centralizada de logging estruturado com suporte a:
- structlog para logs estruturados
- Formato JSON para producao (compativel com ELK/Loki)
- Formato colorido para desenvolvimento
- Rotacao automatica de arquivos
- Correlacao de logs por job_id/worker_id/request_id
- Context managers para contexto automatico
- Exportadores para Elasticsearch e Loki

Uso:
    from factory.core.logging_config import get_logger, configure_logging

    # Configurar no inicio da aplicacao
    configure_logging(environment="development")

    # Usar o logger
    logger = get_logger(__name__)
    logger.info("Mensagem", job_id="JOB-001", worker_id="worker-1")

Autor: Fabrica de Agentes
"""

import json
import os
import sys
import logging
import threading
import atexit
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional, Dict, Any, List, Callable, Union
from dataclasses import dataclass, field
from enum import Enum
from contextlib import contextmanager
from functools import wraps
import traceback
import uuid
import socket
import time

# Tentar importar structlog (opcional mas recomendado)
try:
    import structlog
    from structlog.processors import JSONRenderer, TimeStamper, add_log_level
    from structlog.stdlib import add_logger_name, PositionalArgumentsFormatter
    STRUCTLOG_AVAILABLE = True
except ImportError:
    STRUCTLOG_AVAILABLE = False
    structlog = None

# Tentar importar logging.handlers para rotacao
from logging.handlers import RotatingFileHandler, TimedRotatingFileHandler

# =============================================================================
# CONFIGURACAO
# =============================================================================

# Diretorio de logs
LOGS_DIR = Path(__file__).parent.parent.parent / "logs"
LOGS_DIR.mkdir(exist_ok=True)

# Variaveis de ambiente para configuracao
LOG_LEVEL = os.getenv("LOG_LEVEL", "INFO").upper()
LOG_FORMAT = os.getenv("LOG_FORMAT", "auto")  # auto, json, console
LOG_OUTPUT = os.getenv("LOG_OUTPUT", "both")  # console, file, both
ENVIRONMENT = os.getenv("ENVIRONMENT", "development")

# Configuracao de rotacao
LOG_MAX_BYTES = int(os.getenv("LOG_MAX_BYTES", 10 * 1024 * 1024))  # 10 MB
LOG_BACKUP_COUNT = int(os.getenv("LOG_BACKUP_COUNT", 5))
LOG_ROTATION_WHEN = os.getenv("LOG_ROTATION_WHEN", "midnight")  # midnight, h, d, w0-w6

# Elasticsearch/Loki (opcional)
ELASTICSEARCH_HOST = os.getenv("ELASTICSEARCH_HOST", "")
ELASTICSEARCH_INDEX = os.getenv("ELASTICSEARCH_INDEX", "fabrica-logs")
LOKI_HOST = os.getenv("LOKI_HOST", "")


class LogLevel(str, Enum):
    """Niveis de log disponiveis"""
    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    CRITICAL = "CRITICAL"


class LogCategory(str, Enum):
    """Categorias de log para filtragem"""
    SYSTEM = "system"
    API = "api"
    WORKER = "worker"
    JOB = "job"
    AGENT = "agent"
    TASK = "task"
    DATABASE = "database"
    SKILL = "skill"
    SECURITY = "security"
    PERFORMANCE = "performance"
    INTEGRATION = "integration"


# =============================================================================
# CONTEXTO DE LOG
# =============================================================================

class LogContext:
    """
    Gerenciador de contexto thread-local para logs.

    Permite adicionar contexto que sera automaticamente incluido
    em todos os logs da thread atual.

    Uso:
        with log_context.bind(job_id="JOB-001", worker_id="worker-1"):
            logger.info("Processando...")  # Inclui job_id e worker_id
    """

    _context = threading.local()

    @classmethod
    def get(cls) -> Dict[str, Any]:
        """Retorna contexto atual"""
        if not hasattr(cls._context, 'data'):
            cls._context.data = {}
        return cls._context.data.copy()

    @classmethod
    def set(cls, key: str, value: Any):
        """Define valor no contexto"""
        if not hasattr(cls._context, 'data'):
            cls._context.data = {}
        cls._context.data[key] = value

    @classmethod
    def update(cls, **kwargs):
        """Atualiza contexto com multiplos valores"""
        if not hasattr(cls._context, 'data'):
            cls._context.data = {}
        cls._context.data.update(kwargs)

    @classmethod
    def clear(cls):
        """Limpa contexto"""
        cls._context.data = {}

    @classmethod
    @contextmanager
    def bind(cls, **kwargs):
        """
        Context manager para adicionar contexto temporario.

        Exemplo:
            with LogContext.bind(job_id="JOB-001"):
                logger.info("Dentro do contexto")
            # Contexto removido aqui
        """
        old_data = cls.get()
        try:
            cls.update(**kwargs)
            yield
        finally:
            cls._context.data = old_data


# Instancia global do contexto
log_context = LogContext()


# =============================================================================
# FORMATTERS
# =============================================================================

class JSONLogFormatter(logging.Formatter):
    """
    Formatter que gera logs em formato JSON estruturado.
    Ideal para producao e integracao com ELK/Loki.
    """

    def __init__(self, include_hostname: bool = True, include_context: bool = True):
        super().__init__()
        self.include_hostname = include_hostname
        self.include_context = include_context
        self._hostname = socket.gethostname() if include_hostname else None

    def format(self, record: logging.LogRecord) -> str:
        """Formata o log como JSON"""
        log_data = {
            "@timestamp": datetime.now(timezone.utc).isoformat(),
            "level": record.levelname,
            "logger": record.name,
            "message": record.getMessage(),
            "module": record.module,
            "function": record.funcName,
            "line": record.lineno,
            "thread": record.thread,
            "thread_name": record.threadName,
            "process": record.process,
        }

        # Adiciona hostname
        if self.include_hostname and self._hostname:
            log_data["host"] = self._hostname

        # Adiciona contexto thread-local
        if self.include_context:
            context = LogContext.get()
            if context:
                log_data["context"] = context

        # Adiciona dados extras do record
        if hasattr(record, 'structured_data'):
            log_data.update(record.structured_data)

        # IDs especificos (promovidos para nivel superior)
        for id_field in ['job_id', 'worker_id', 'request_id', 'correlation_id',
                         'agent_id', 'task_id', 'story_id', 'project_id']:
            if hasattr(record, id_field):
                log_data[id_field] = getattr(record, id_field)
            elif self.include_context and id_field in LogContext.get():
                log_data[id_field] = LogContext.get()[id_field]

        # Adiciona informacao de excecao
        if record.exc_info:
            log_data["exception"] = {
                "type": record.exc_info[0].__name__ if record.exc_info[0] else None,
                "message": str(record.exc_info[1]) if record.exc_info[1] else None,
                "stacktrace": self.formatException(record.exc_info)
            }

        # Adiciona stack_info se disponivel
        if record.stack_info:
            log_data["stack_info"] = record.stack_info

        return json.dumps(log_data, ensure_ascii=False, default=str)


class ColoredConsoleFormatter(logging.Formatter):
    """
    Formatter colorido para console em desenvolvimento.
    Exibe logs de forma legivel com cores por nivel.
    """

    COLORS = {
        'DEBUG': '\033[36m',      # Cyan
        'INFO': '\033[32m',       # Green
        'WARNING': '\033[33m',    # Yellow
        'ERROR': '\033[31m',      # Red
        'CRITICAL': '\033[35;1m', # Magenta Bold
    }
    RESET = '\033[0m'
    BOLD = '\033[1m'
    DIM = '\033[2m'

    # Cores para contexto
    CTX_COLORS = {
        'job_id': '\033[34m',      # Blue
        'worker_id': '\033[35m',   # Magenta
        'agent_id': '\033[33m',    # Yellow
        'project_id': '\033[36m',  # Cyan
        'request_id': '\033[37m',  # White
    }

    def format(self, record: logging.LogRecord) -> str:
        """Formata o log com cores"""
        color = self.COLORS.get(record.levelname, self.RESET)
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]

        # Linha principal
        parts = [
            f"{self.DIM}{timestamp}{self.RESET}",
            f"{color}{self.BOLD}{record.levelname:8}{self.RESET}",
            f"{self.DIM}[{record.name}]{self.RESET}",
            record.getMessage()
        ]

        formatted = " ".join(parts)

        # Adiciona contexto
        context_parts = []

        # Do contexto thread-local
        ctx = LogContext.get()
        for key, value in ctx.items():
            ctx_color = self.CTX_COLORS.get(key, self.DIM)
            context_parts.append(f"{ctx_color}{key}={value}{self.RESET}")

        # Do record
        if hasattr(record, 'structured_data'):
            for key, value in record.structured_data.items():
                if key not in ctx:
                    ctx_color = self.CTX_COLORS.get(key, self.DIM)
                    context_parts.append(f"{ctx_color}{key}={value}{self.RESET}")

        if context_parts:
            formatted += f" {self.DIM}({', '.join(context_parts)}){self.RESET}"

        # Adiciona excecao
        if record.exc_info:
            formatted += f"\n{self.formatException(record.exc_info)}"

        return formatted


# =============================================================================
# HANDLERS
# =============================================================================

class AsyncLogHandler(logging.Handler):
    """
    Handler assincrono para envio de logs em batch.
    Util para enviar logs para Elasticsearch, Loki, etc.
    """

    def __init__(self,
                 callback: Callable[[List[Dict]], None],
                 batch_size: int = 100,
                 flush_interval: float = 5.0):
        super().__init__()
        self.callback = callback
        self.batch_size = batch_size
        self.flush_interval = flush_interval

        self._buffer: List[Dict] = []
        self._lock = threading.Lock()
        self._stop_event = threading.Event()
        self._flush_thread = threading.Thread(target=self._flush_loop, daemon=True)
        self._flush_thread.start()

        # Registra flush no atexit
        atexit.register(self.flush)

    def emit(self, record: logging.LogRecord):
        """Adiciona log ao buffer"""
        try:
            log_entry = self._format_record(record)
            with self._lock:
                self._buffer.append(log_entry)
                if len(self._buffer) >= self.batch_size:
                    self._do_flush()
        except Exception:
            self.handleError(record)

    def _format_record(self, record: logging.LogRecord) -> Dict:
        """Formata record para dicionario"""
        return {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "level": record.levelname,
            "logger": record.name,
            "message": record.getMessage(),
            "module": record.module,
            "function": record.funcName,
            "line": record.lineno,
            "context": LogContext.get(),
            "extra": getattr(record, 'structured_data', {})
        }

    def _flush_loop(self):
        """Loop de flush periodico"""
        while not self._stop_event.wait(self.flush_interval):
            self.flush()

    def flush(self):
        """Envia logs do buffer"""
        with self._lock:
            if self._buffer:
                self._do_flush()

    def _do_flush(self):
        """Executa o flush (chamado com lock)"""
        if self._buffer:
            try:
                self.callback(self._buffer.copy())
            except Exception:
                pass  # Nao quebra a aplicacao por erro de log
            finally:
                self._buffer.clear()

    def close(self):
        """Fecha o handler"""
        self._stop_event.set()
        self.flush()
        super().close()


class ElasticsearchHandler(AsyncLogHandler):
    """
    Handler para envio de logs ao Elasticsearch.

    Configuracao via variaveis de ambiente:
    - ELASTICSEARCH_HOST: Host do Elasticsearch (ex: http://localhost:9200)
    - ELASTICSEARCH_INDEX: Nome do indice (default: fabrica-logs)
    """

    def __init__(self,
                 host: str = None,
                 index: str = None,
                 batch_size: int = 100,
                 flush_interval: float = 5.0):
        self.host = host or ELASTICSEARCH_HOST
        self.index = index or ELASTICSEARCH_INDEX
        super().__init__(
            callback=self._send_to_elasticsearch,
            batch_size=batch_size,
            flush_interval=flush_interval
        )

    def _send_to_elasticsearch(self, logs: List[Dict]):
        """Envia logs para Elasticsearch via bulk API"""
        if not self.host:
            return

        try:
            import httpx

            # Formata para bulk API
            bulk_data = []
            for log in logs:
                # Indice com data para rotacao
                date_suffix = datetime.now().strftime("%Y.%m.%d")
                index_name = f"{self.index}-{date_suffix}"

                bulk_data.append(json.dumps({"index": {"_index": index_name}}))
                bulk_data.append(json.dumps(log, default=str))

            body = "\n".join(bulk_data) + "\n"

            # Envia para Elasticsearch
            with httpx.Client(timeout=10.0) as client:
                response = client.post(
                    f"{self.host}/_bulk",
                    content=body,
                    headers={"Content-Type": "application/x-ndjson"}
                )
                response.raise_for_status()
        except Exception:
            pass  # Silently fail - nao quebrar a aplicacao


class LokiHandler(AsyncLogHandler):
    """
    Handler para envio de logs ao Grafana Loki.

    Configuracao via variaveis de ambiente:
    - LOKI_HOST: Host do Loki (ex: http://localhost:3100)
    """

    def __init__(self,
                 host: str = None,
                 labels: Dict[str, str] = None,
                 batch_size: int = 100,
                 flush_interval: float = 5.0):
        self.host = host or LOKI_HOST
        self.labels = labels or {"app": "fabrica-agentes", "env": ENVIRONMENT}
        super().__init__(
            callback=self._send_to_loki,
            batch_size=batch_size,
            flush_interval=flush_interval
        )

    def _send_to_loki(self, logs: List[Dict]):
        """Envia logs para Loki"""
        if not self.host:
            return

        try:
            import httpx

            # Agrupa logs por labels
            streams = {}
            for log in logs:
                # Labels incluem nivel e outros campos importantes
                log_labels = {
                    **self.labels,
                    "level": log.get("level", "INFO").lower()
                }

                # Adiciona job_id se presente
                if "job_id" in log.get("context", {}):
                    log_labels["job_id"] = log["context"]["job_id"]

                label_key = str(sorted(log_labels.items()))

                if label_key not in streams:
                    streams[label_key] = {
                        "stream": log_labels,
                        "values": []
                    }

                # Loki espera timestamp em nanosegundos
                ts = log.get("timestamp", datetime.now(timezone.utc).isoformat())
                ts_ns = str(int(datetime.fromisoformat(ts.replace("Z", "+00:00")).timestamp() * 1e9))

                streams[label_key]["values"].append([ts_ns, json.dumps(log, default=str)])

            payload = {"streams": list(streams.values())}

            # Envia para Loki
            with httpx.Client(timeout=10.0) as client:
                response = client.post(
                    f"{self.host}/loki/api/v1/push",
                    json=payload,
                    headers={"Content-Type": "application/json"}
                )
                response.raise_for_status()
        except Exception:
            pass  # Silently fail


# =============================================================================
# LOGGER ESTRUTURADO
# =============================================================================

class StructuredLogger:
    """
    Logger estruturado que encapsula o logger padrao com funcionalidades extras.

    Caracteristicas:
    - Suporte a contexto automatico via LogContext
    - Metodos especializados (job_start, api_request, etc)
    - Medicao automatica de duracao
    - Correlacao de logs
    """

    def __init__(self, name: str):
        self._logger = logging.getLogger(name)
        self.name = name

    def _log(self, level: int, message: str, exc_info: bool = False, **kwargs):
        """Log interno com dados estruturados"""
        # Cria record
        record = self._logger.makeRecord(
            self.name,
            level,
            "(unknown file)",
            0,
            message,
            (),
            None if not exc_info else sys.exc_info()
        )

        # Adiciona dados estruturados
        record.structured_data = kwargs

        # Adiciona IDs ao record diretamente para facil acesso
        for key in ['job_id', 'worker_id', 'request_id', 'correlation_id',
                    'agent_id', 'task_id', 'story_id', 'project_id']:
            if key in kwargs:
                setattr(record, key, kwargs[key])

        self._logger.handle(record)

    # Metodos de nivel basico
    def debug(self, message: str, **kwargs):
        """Log de debug"""
        self._log(logging.DEBUG, message, **kwargs)

    def info(self, message: str, **kwargs):
        """Log informativo"""
        self._log(logging.INFO, message, **kwargs)

    def warning(self, message: str, **kwargs):
        """Log de aviso"""
        self._log(logging.WARNING, message, **kwargs)

    def error(self, message: str, exc_info: bool = False, **kwargs):
        """Log de erro"""
        self._log(logging.ERROR, message, exc_info=exc_info, **kwargs)

    def critical(self, message: str, exc_info: bool = False, **kwargs):
        """Log critico"""
        self._log(logging.CRITICAL, message, exc_info=exc_info, **kwargs)

    def exception(self, message: str, **kwargs):
        """Log de excecao (inclui stack trace)"""
        self._log(logging.ERROR, message, exc_info=True, **kwargs)

    # Metodos especializados
    def job_start(self, job_id: str, job_type: str = None, **kwargs):
        """Log de inicio de job"""
        self.info(
            f"Job iniciado: {job_id}",
            job_id=job_id,
            job_type=job_type,
            event="job_start",
            category=LogCategory.JOB.value,
            **kwargs
        )

    def job_complete(self, job_id: str, duration_ms: float = None, **kwargs):
        """Log de conclusao de job"""
        self.info(
            f"Job concluido: {job_id}" + (f" ({duration_ms:.2f}ms)" if duration_ms else ""),
            job_id=job_id,
            duration_ms=duration_ms,
            event="job_complete",
            category=LogCategory.JOB.value,
            **kwargs
        )

    def job_error(self, job_id: str, error: Union[str, Exception], **kwargs):
        """Log de erro em job"""
        error_msg = str(error)
        error_type = type(error).__name__ if isinstance(error, Exception) else "Error"

        self.error(
            f"Job falhou: {job_id} - {error_msg}",
            exc_info=isinstance(error, Exception),
            job_id=job_id,
            error_type=error_type,
            error_message=error_msg,
            event="job_error",
            category=LogCategory.JOB.value,
            **kwargs
        )

    def worker_action(self, worker_id: str, action: str, **kwargs):
        """Log de acao de worker"""
        self.info(
            f"Worker {worker_id}: {action}",
            worker_id=worker_id,
            event="worker_action",
            category=LogCategory.WORKER.value,
            **kwargs
        )

    def agent_action(self, agent_id: str, action: str, **kwargs):
        """Log de acao de agente"""
        self.info(
            f"Agent {agent_id}: {action}",
            agent_id=agent_id,
            event="agent_action",
            category=LogCategory.AGENT.value,
            **kwargs
        )

    def api_request(self, method: str, path: str, status_code: int,
                   duration_ms: float = None, **kwargs):
        """Log de request API"""
        self.info(
            f"{method} {path} -> {status_code}" + (f" ({duration_ms:.2f}ms)" if duration_ms else ""),
            method=method,
            path=path,
            status_code=status_code,
            duration_ms=duration_ms,
            event="api_request",
            category=LogCategory.API.value,
            **kwargs
        )

    def task_start(self, task_id: str, task_type: str = None, **kwargs):
        """Log de inicio de tarefa"""
        self.info(
            f"Task iniciada: {task_id}",
            task_id=task_id,
            task_type=task_type,
            event="task_start",
            category=LogCategory.TASK.value,
            **kwargs
        )

    def task_complete(self, task_id: str, duration_ms: float = None, **kwargs):
        """Log de conclusao de tarefa"""
        self.info(
            f"Task concluida: {task_id}" + (f" ({duration_ms:.2f}ms)" if duration_ms else ""),
            task_id=task_id,
            duration_ms=duration_ms,
            event="task_complete",
            category=LogCategory.TASK.value,
            **kwargs
        )

    def security_event(self, event_type: str, message: str, **kwargs):
        """Log de evento de seguranca"""
        self.warning(
            f"[SECURITY] {event_type}: {message}",
            security_event=event_type,
            event="security",
            category=LogCategory.SECURITY.value,
            **kwargs
        )

    def performance_metric(self, metric_name: str, value: float, unit: str = "ms", **kwargs):
        """Log de metrica de performance"""
        self.info(
            f"[PERF] {metric_name}: {value}{unit}",
            metric_name=metric_name,
            metric_value=value,
            metric_unit=unit,
            event="performance",
            category=LogCategory.PERFORMANCE.value,
            **kwargs
        )

    def database_query(self, operation: str, table: str, duration_ms: float = None, **kwargs):
        """Log de operacao de banco de dados"""
        self.debug(
            f"[DB] {operation} on {table}" + (f" ({duration_ms:.2f}ms)" if duration_ms else ""),
            db_operation=operation,
            db_table=table,
            duration_ms=duration_ms,
            event="database",
            category=LogCategory.DATABASE.value,
            **kwargs
        )

    # Context managers
    @contextmanager
    def timed_operation(self, operation_name: str, **context):
        """
        Context manager para medir duracao de operacoes.

        Exemplo:
            with logger.timed_operation("process_data", job_id="JOB-001"):
                # codigo que sera medido
                pass
        """
        start_time = time.perf_counter()
        self.debug(f"Iniciando: {operation_name}", **context)
        try:
            yield
            duration_ms = (time.perf_counter() - start_time) * 1000
            self.info(f"Concluido: {operation_name}", duration_ms=duration_ms, **context)
        except Exception as e:
            duration_ms = (time.perf_counter() - start_time) * 1000
            self.error(f"Falhou: {operation_name}", duration_ms=duration_ms,
                      error=str(e), exc_info=True, **context)
            raise

    # Decorator
    def log_function(self, level: int = logging.INFO):
        """
        Decorator para logar entrada e saida de funcoes.

        Exemplo:
            @logger.log_function()
            def my_function():
                pass
        """
        def decorator(func):
            @wraps(func)
            def wrapper(*args, **kwargs):
                func_name = func.__qualname__
                self._log(level, f"Entrando: {func_name}", function=func_name)
                start_time = time.perf_counter()
                try:
                    result = func(*args, **kwargs)
                    duration_ms = (time.perf_counter() - start_time) * 1000
                    self._log(level, f"Saindo: {func_name}", function=func_name,
                             duration_ms=duration_ms)
                    return result
                except Exception as e:
                    duration_ms = (time.perf_counter() - start_time) * 1000
                    self.error(f"Erro em: {func_name}", function=func_name,
                              duration_ms=duration_ms, error=str(e), exc_info=True)
                    raise

            @wraps(func)
            async def async_wrapper(*args, **kwargs):
                func_name = func.__qualname__
                self._log(level, f"Entrando: {func_name}", function=func_name)
                start_time = time.perf_counter()
                try:
                    result = await func(*args, **kwargs)
                    duration_ms = (time.perf_counter() - start_time) * 1000
                    self._log(level, f"Saindo: {func_name}", function=func_name,
                             duration_ms=duration_ms)
                    return result
                except Exception as e:
                    duration_ms = (time.perf_counter() - start_time) * 1000
                    self.error(f"Erro em: {func_name}", function=func_name,
                              duration_ms=duration_ms, error=str(e), exc_info=True)
                    raise

            import asyncio
            if asyncio.iscoroutinefunction(func):
                return async_wrapper
            return wrapper
        return decorator


# =============================================================================
# CONFIGURACAO GLOBAL
# =============================================================================

_configured = False
_loggers: Dict[str, StructuredLogger] = {}


def configure_logging(
    level: str = None,
    format: str = None,
    output: str = None,
    log_dir: Path = None,
    enable_elasticsearch: bool = False,
    enable_loki: bool = False,
    environment: str = None
):
    """
    Configura o sistema de logging.

    Args:
        level: Nivel de log (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        format: Formato de saida (json, console, auto)
        output: Destino (console, file, both)
        log_dir: Diretorio para arquivos de log
        enable_elasticsearch: Habilita envio para Elasticsearch
        enable_loki: Habilita envio para Loki
        environment: Ambiente (development, production)

    Em producao (environment=production ou format=json):
    - Logs em JSON para arquivo
    - Console simplificado

    Em desenvolvimento (environment=development ou format=console):
    - Logs coloridos no console
    - JSON no arquivo
    """
    global _configured

    # Resolve configuracoes
    level = level or LOG_LEVEL
    format = format or LOG_FORMAT
    output = output or LOG_OUTPUT
    log_dir = log_dir or LOGS_DIR
    environment = environment or ENVIRONMENT

    # Auto-detecta formato baseado no ambiente
    if format == "auto":
        format = "json" if environment == "production" else "console"

    # Garante que o diretorio existe
    log_dir = Path(log_dir)
    log_dir.mkdir(parents=True, exist_ok=True)

    # Configura root logger
    root_logger = logging.getLogger("factory")
    root_logger.setLevel(getattr(logging, level.upper()))
    root_logger.handlers.clear()

    # Handler de console
    if output in ("console", "both"):
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(getattr(logging, level.upper()))

        if format == "console":
            console_handler.setFormatter(ColoredConsoleFormatter())
        else:
            console_handler.setFormatter(JSONLogFormatter())

        root_logger.addHandler(console_handler)

    # Handler de arquivo
    if output in ("file", "both"):
        # Arquivo principal com rotacao por tamanho
        log_file = log_dir / "factory.log"
        file_handler = RotatingFileHandler(
            log_file,
            maxBytes=LOG_MAX_BYTES,
            backupCount=LOG_BACKUP_COUNT,
            encoding="utf-8"
        )
        file_handler.setLevel(logging.DEBUG)  # Arquivo pega tudo
        file_handler.setFormatter(JSONLogFormatter())
        root_logger.addHandler(file_handler)

        # Arquivo de erros separado
        error_file = log_dir / "factory-errors.log"
        error_handler = RotatingFileHandler(
            error_file,
            maxBytes=LOG_MAX_BYTES,
            backupCount=LOG_BACKUP_COUNT,
            encoding="utf-8"
        )
        error_handler.setLevel(logging.ERROR)
        error_handler.setFormatter(JSONLogFormatter())
        root_logger.addHandler(error_handler)

    # Handler Elasticsearch
    if enable_elasticsearch and ELASTICSEARCH_HOST:
        es_handler = ElasticsearchHandler()
        es_handler.setLevel(logging.INFO)
        root_logger.addHandler(es_handler)

    # Handler Loki
    if enable_loki and LOKI_HOST:
        loki_handler = LokiHandler()
        loki_handler.setLevel(logging.INFO)
        root_logger.addHandler(loki_handler)

    _configured = True

    # Log inicial
    logger = get_logger("logging_config")
    logger.info(
        "Sistema de logging configurado",
        log_level=level,
        log_format=format,
        log_output=output,
        environment=environment,
        log_dir=str(log_dir)
    )


def get_logger(name: str = None) -> StructuredLogger:
    """
    Retorna um logger estruturado.

    Args:
        name: Nome do logger (geralmente __name__)

    Returns:
        StructuredLogger configurado
    """
    global _configured

    # Configura automaticamente se ainda nao foi configurado
    if not _configured:
        configure_logging()

    name = name or "factory"
    if not name.startswith("factory"):
        name = f"factory.{name}"

    if name not in _loggers:
        _loggers[name] = StructuredLogger(name)

    return _loggers[name]


# =============================================================================
# FUNCOES DE CONVENIENCIA
# =============================================================================

def log_info(message: str, **kwargs):
    """Log informativo usando logger padrao"""
    get_logger().info(message, **kwargs)


def log_error(message: str, **kwargs):
    """Log de erro usando logger padrao"""
    get_logger().error(message, **kwargs)


def log_warning(message: str, **kwargs):
    """Log de aviso usando logger padrao"""
    get_logger().warning(message, **kwargs)


def log_debug(message: str, **kwargs):
    """Log de debug usando logger padrao"""
    get_logger().debug(message, **kwargs)


def log_exception(message: str, **kwargs):
    """Log de excecao usando logger padrao"""
    get_logger().exception(message, **kwargs)


# =============================================================================
# MIDDLEWARE PARA FASTAPI
# =============================================================================

def create_request_context_middleware():
    """
    Cria middleware para FastAPI que adiciona contexto de request.

    Uso:
        from factory.core.logging_config import create_request_context_middleware
        app.add_middleware(create_request_context_middleware())
    """
    try:
        from starlette.middleware.base import BaseHTTPMiddleware
        from starlette.requests import Request

        class RequestContextMiddleware(BaseHTTPMiddleware):
            async def dispatch(self, request: Request, call_next):
                # Gera request_id
                request_id = request.headers.get("X-Request-ID", str(uuid.uuid4()))

                # Adiciona ao contexto
                with LogContext.bind(
                    request_id=request_id,
                    method=request.method,
                    path=request.url.path
                ):
                    # Mede tempo
                    start_time = time.perf_counter()

                    response = await call_next(request)

                    duration_ms = (time.perf_counter() - start_time) * 1000

                    # Log da request
                    get_logger("api").api_request(
                        method=request.method,
                        path=request.url.path,
                        status_code=response.status_code,
                        duration_ms=duration_ms,
                        request_id=request_id
                    )

                    # Adiciona request_id ao header de resposta
                    response.headers["X-Request-ID"] = request_id

                    return response

        return RequestContextMiddleware
    except ImportError:
        return None


# =============================================================================
# INICIALIZACAO
# =============================================================================

# Configura automaticamente ao importar o modulo
if not _configured:
    configure_logging()

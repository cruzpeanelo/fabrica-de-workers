"""
Sistema de Logs Estruturados - Plataforma E
=================================================

Sistema centralizado de logging com suporte a:
- Logs estruturados em JSON
- Multiplos handlers (console, arquivo, banco de dados)
- Correlacao de logs por request/task
- Metricas e alertas
"""

import json
import logging
import sys
import os
from datetime import datetime
from typing import Optional, Dict, Any, List
from pathlib import Path
from dataclasses import dataclass, field, asdict
from enum import Enum
import traceback
import threading
from queue import Queue
import uuid

# Diretorio de logs
LOGS_DIR = Path(__file__).parent.parent.parent / "logs"
LOGS_DIR.mkdir(exist_ok=True)


class LogLevel(str, Enum):
    """Niveis de log"""
    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    CRITICAL = "CRITICAL"


class LogCategory(str, Enum):
    """Categorias de log"""
    SYSTEM = "system"
    API = "api"
    AGENT = "agent"
    TASK = "task"
    DATABASE = "database"
    SKILL = "skill"
    SECURITY = "security"
    PERFORMANCE = "performance"


@dataclass
class StructuredLog:
    """Estrutura de log padronizada"""
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat())
    level: str = "INFO"
    category: str = "system"
    message: str = ""

    # Contexto
    correlation_id: Optional[str] = None
    request_id: Optional[str] = None
    user_id: Optional[str] = None

    # Fonte
    source: str = "factory"
    module: Optional[str] = None
    function: Optional[str] = None
    line: Optional[int] = None

    # Entidades relacionadas
    project_id: Optional[str] = None
    agent_id: Optional[str] = None
    task_id: Optional[str] = None
    story_id: Optional[str] = None
    skill_id: Optional[str] = None

    # Dados adicionais
    data: Dict[str, Any] = field(default_factory=dict)
    tags: List[str] = field(default_factory=list)

    # Erro (se aplicavel)
    error_type: Optional[str] = None
    error_message: Optional[str] = None
    stack_trace: Optional[str] = None

    # Metricas
    duration_ms: Optional[float] = None

    def to_dict(self) -> Dict[str, Any]:
        """Converte para dicionario (remove None values)"""
        result = asdict(self)
        return {k: v for k, v in result.items() if v is not None}

    def to_json(self) -> str:
        """Converte para JSON"""
        return json.dumps(self.to_dict(), ensure_ascii=False, default=str)


class JSONFormatter(logging.Formatter):
    """Formatter que gera logs em JSON"""

    def format(self, record: logging.LogRecord) -> str:
        log_data = {
            "timestamp": datetime.utcnow().isoformat(),
            "level": record.levelname,
            "message": record.getMessage(),
            "module": record.module,
            "function": record.funcName,
            "line": record.lineno,
        }

        # Adiciona dados extras se existirem
        if hasattr(record, 'structured_data'):
            log_data.update(record.structured_data)

        # Adiciona exception info se existir
        if record.exc_info:
            log_data["error_type"] = record.exc_info[0].__name__ if record.exc_info[0] else None
            log_data["error_message"] = str(record.exc_info[1]) if record.exc_info[1] else None
            log_data["stack_trace"] = traceback.format_exception(*record.exc_info)

        return json.dumps(log_data, ensure_ascii=False, default=str)


class ColoredConsoleFormatter(logging.Formatter):
    """Formatter colorido para console"""

    COLORS = {
        'DEBUG': '\033[36m',     # Cyan
        'INFO': '\033[32m',      # Green
        'WARNING': '\033[33m',   # Yellow
        'ERROR': '\033[31m',     # Red
        'CRITICAL': '\033[35m',  # Magenta
    }
    RESET = '\033[0m'
    BOLD = '\033[1m'

    def format(self, record: logging.LogRecord) -> str:
        color = self.COLORS.get(record.levelname, self.RESET)

        # Formato: [TIMESTAMP] LEVEL [SOURCE] MESSAGE
        timestamp = datetime.now().strftime('%H:%M:%S')
        source = getattr(record, 'source', record.module)

        formatted = f"{color}[{timestamp}] {self.BOLD}{record.levelname:8}{self.RESET} "
        formatted += f"\033[34m[{source}]{self.RESET} {record.getMessage()}"

        # Adiciona dados extras
        if hasattr(record, 'structured_data'):
            data = record.structured_data
            if data.get('agent_id'):
                formatted += f" \033[33m(Agent: {data['agent_id']}){self.RESET}"
            if data.get('project_id'):
                formatted += f" \033[36m(Project: {data['project_id']}){self.RESET}"
            if data.get('duration_ms'):
                formatted += f" \033[35m({data['duration_ms']:.2f}ms){self.RESET}"

        return formatted


class DatabaseLogHandler(logging.Handler):
    """Handler que salva logs no banco de dados"""

    def __init__(self):
        super().__init__()
        self._queue = Queue()
        self._stop_event = threading.Event()
        self._worker = threading.Thread(target=self._process_queue, daemon=True)
        self._worker.start()

    def emit(self, record: logging.LogRecord):
        """Adiciona log a fila para processamento async"""
        try:
            log_entry = self._create_log_entry(record)
            self._queue.put(log_entry)
        except Exception:
            self.handleError(record)

    def _create_log_entry(self, record: logging.LogRecord) -> Dict:
        """Cria entrada de log para o banco"""
        entry = {
            "source": getattr(record, 'source', record.module),
            "level": record.levelname,
            "event_type": getattr(record, 'event_type', 'log'),
            "message": record.getMessage(),
            "details": {}
        }

        # Adiciona dados estruturados
        if hasattr(record, 'structured_data'):
            data = record.structured_data
            entry["agent_id"] = data.get('agent_id')
            entry["project_id"] = data.get('project_id')
            entry["task_id"] = data.get('task_id')
            entry["story_id"] = data.get('story_id')
            entry["details"] = data.get('data', {})

        return entry

    def _process_queue(self):
        """Processa fila de logs em background"""
        while not self._stop_event.is_set():
            try:
                if not self._queue.empty():
                    entry = self._queue.get(timeout=1)
                    self._save_to_db(entry)
            except Exception:
                pass

    def _save_to_db(self, entry: Dict):
        """Salva log no banco de dados"""
        try:
            from factory.database.connection import SessionLocal
            from factory.database.models import ActivityLog

            db = SessionLocal()
            try:
                log = ActivityLog(
                    source=entry.get("source", "unknown"),
                    level=entry.get("level", "INFO"),
                    event_type=entry.get("event_type", "log"),
                    message=entry.get("message", ""),
                    agent_id=entry.get("agent_id"),
                    project_id=entry.get("project_id"),
                    task_id=entry.get("task_id"),
                    story_id=entry.get("story_id"),
                    details=entry.get("details", {})
                )
                db.add(log)
                db.commit()
            finally:
                db.close()
        except Exception:
            pass  # Silently fail - don't break the app for logging

    def close(self):
        """Para o worker e fecha o handler"""
        self._stop_event.set()
        super().close()


class FactoryLogger:
    """Logger principal da Plataforma E"""

    _instance = None
    _lock = threading.Lock()

    def __new__(cls):
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
                    cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        if self._initialized:
            return

        self._initialized = True
        self._logger = logging.getLogger("factory")
        self._logger.setLevel(logging.DEBUG)
        self._logger.handlers.clear()

        # Console handler (colorido)
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(logging.INFO)
        console_handler.setFormatter(ColoredConsoleFormatter())
        self._logger.addHandler(console_handler)

        # File handler (JSON)
        log_file = LOGS_DIR / f"factory_{datetime.now().strftime('%Y%m%d')}.log"
        file_handler = logging.FileHandler(log_file, encoding='utf-8')
        file_handler.setLevel(logging.DEBUG)
        file_handler.setFormatter(JSONFormatter())
        self._logger.addHandler(file_handler)

        # Database handler
        try:
            db_handler = DatabaseLogHandler()
            db_handler.setLevel(logging.INFO)
            self._logger.addHandler(db_handler)
        except Exception:
            pass

        # Contexto de correlacao
        self._correlation_id = threading.local()

    def set_correlation_id(self, correlation_id: str = None):
        """Define ID de correlacao para o contexto atual"""
        self._correlation_id.value = correlation_id or str(uuid.uuid4())

    def get_correlation_id(self) -> Optional[str]:
        """Retorna ID de correlacao do contexto atual"""
        return getattr(self._correlation_id, 'value', None)

    def _log(self, level: str, message: str, **kwargs):
        """Log interno com dados estruturados"""
        record = self._logger.makeRecord(
            self._logger.name,
            getattr(logging, level),
            kwargs.get('module', ''),
            kwargs.get('line', 0),
            message,
            (),
            None,
            kwargs.get('function', '')
        )

        # Adiciona dados estruturados
        record.structured_data = {
            'correlation_id': self.get_correlation_id(),
            **kwargs
        }
        record.source = kwargs.get('source', 'factory')
        record.event_type = kwargs.get('event_type', 'log')

        self._logger.handle(record)

    def debug(self, message: str, **kwargs):
        """Log de debug"""
        self._log('DEBUG', message, **kwargs)

    def info(self, message: str, **kwargs):
        """Log informativo"""
        self._log('INFO', message, **kwargs)

    def warning(self, message: str, **kwargs):
        """Log de aviso"""
        self._log('WARNING', message, **kwargs)

    def error(self, message: str, **kwargs):
        """Log de erro"""
        self._log('ERROR', message, **kwargs)

    def critical(self, message: str, **kwargs):
        """Log critico"""
        self._log('CRITICAL', message, **kwargs)

    # Metodos especializados
    def agent_action(self, agent_id: str, action: str, **kwargs):
        """Log de acao de agente"""
        self.info(f"Agent {agent_id}: {action}",
                 agent_id=agent_id,
                 source="agent",
                 event_type="agent_action",
                 **kwargs)

    def task_start(self, task_id: str, task_type: str, **kwargs):
        """Log de inicio de tarefa"""
        self.info(f"Task started: {task_id} ({task_type})",
                 task_id=task_id,
                 source="task",
                 event_type="task_start",
                 **kwargs)

    def task_complete(self, task_id: str, duration_ms: float, **kwargs):
        """Log de conclusao de tarefa"""
        self.info(f"Task completed: {task_id}",
                 task_id=task_id,
                 duration_ms=duration_ms,
                 source="task",
                 event_type="task_complete",
                 **kwargs)

    def task_error(self, task_id: str, error: Exception, **kwargs):
        """Log de erro em tarefa"""
        self.error(f"Task failed: {task_id} - {str(error)}",
                  task_id=task_id,
                  source="task",
                  event_type="task_error",
                  error_type=type(error).__name__,
                  error_message=str(error),
                  stack_trace=traceback.format_exc(),
                  **kwargs)

    def api_request(self, method: str, path: str, status_code: int, duration_ms: float, **kwargs):
        """Log de request API"""
        self.info(f"{method} {path} -> {status_code}",
                 source="api",
                 event_type="api_request",
                 data={"method": method, "path": path, "status_code": status_code},
                 duration_ms=duration_ms,
                 **kwargs)

    def skill_execution(self, skill_id: str, status: str, **kwargs):
        """Log de execucao de skill"""
        self.info(f"Skill {skill_id}: {status}",
                 skill_id=skill_id,
                 source="skill",
                 event_type="skill_execution",
                 **kwargs)

    def security_event(self, event_type: str, message: str, **kwargs):
        """Log de evento de seguranca"""
        self.warning(f"Security: {event_type} - {message}",
                    source="security",
                    event_type=f"security_{event_type}",
                    **kwargs)


# Instancia global
_logger: Optional[FactoryLogger] = None


def get_logger() -> FactoryLogger:
    """Retorna instancia do logger"""
    global _logger
    if _logger is None:
        _logger = FactoryLogger()
    return _logger


# Funcoes de conveniencia
def log_info(message: str, **kwargs):
    get_logger().info(message, **kwargs)

def log_error(message: str, **kwargs):
    get_logger().error(message, **kwargs)

def log_warning(message: str, **kwargs):
    get_logger().warning(message, **kwargs)

def log_debug(message: str, **kwargs):
    get_logger().debug(message, **kwargs)

def log_agent_action(agent_id: str, action: str, **kwargs):
    get_logger().agent_action(agent_id, action, **kwargs)

def log_task_start(task_id: str, task_type: str, **kwargs):
    get_logger().task_start(task_id, task_type, **kwargs)

def log_task_complete(task_id: str, duration_ms: float, **kwargs):
    get_logger().task_complete(task_id, duration_ms, **kwargs)

def log_api_request(method: str, path: str, status_code: int, duration_ms: float, **kwargs):
    get_logger().api_request(method, path, status_code, duration_ms, **kwargs)

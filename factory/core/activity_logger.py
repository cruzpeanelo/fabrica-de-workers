"""
Activity Logger - Sistema de logging de atividades dos agentes.

Permite visualizar em tempo real o que cada agente esta fazendo,
mesmo em modo autonomo sem intervencao humana.
"""

import os
import json
import time
from pathlib import Path
from datetime import datetime
from typing import Optional, Dict, List, Any
from dataclasses import dataclass, asdict
from enum import Enum
import threading
import queue


class ActivityType(Enum):
    """Tipos de atividade."""
    # Lifecycle
    AGENT_START = "agent_start"
    AGENT_STOP = "agent_stop"

    # Tasks
    TASK_START = "task_start"
    TASK_PROGRESS = "task_progress"
    TASK_COMPLETE = "task_complete"
    TASK_ERROR = "task_error"

    # Actions
    FILE_READ = "file_read"
    FILE_WRITE = "file_write"
    FILE_EDIT = "file_edit"

    CODE_GENERATE = "code_generate"
    CODE_REVIEW = "code_review"

    TEST_RUN = "test_run"
    TEST_PASS = "test_pass"
    TEST_FAIL = "test_fail"

    GIT_COMMIT = "git_commit"
    GIT_PUSH = "git_push"

    # Communication
    HANDOFF_OUT = "handoff_out"
    HANDOFF_IN = "handoff_in"

    THINKING = "thinking"
    DECISION = "decision"

    # System
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


@dataclass
class Activity:
    """Uma atividade de um agente."""
    timestamp: str
    agent_type: str
    activity_type: str
    title: str
    description: str = ""
    details: Dict[str, Any] = None
    task_id: Optional[str] = None
    duration_ms: Optional[int] = None

    def to_dict(self) -> Dict:
        return {
            "timestamp": self.timestamp,
            "agent_type": self.agent_type,
            "activity_type": self.activity_type,
            "title": self.title,
            "description": self.description,
            "details": self.details or {},
            "task_id": self.task_id,
            "duration_ms": self.duration_ms
        }


class ActivityLogger:
    """Logger de atividades dos agentes."""

    def __init__(self, base_path: Optional[str] = None):
        self.base_path = Path(base_path) if base_path else Path.cwd()
        self.state_path = self.base_path / "factory" / "state"
        self.logs_path = self.state_path / "activity_logs"
        self.realtime_file = self.state_path / "realtime_activity.jsonl"

        # Criar diretorios
        self.logs_path.mkdir(parents=True, exist_ok=True)

        # Buffer para atividades recentes (em memoria)
        self.recent_activities: List[Activity] = []
        self.max_recent = 100

        # Lock para thread safety
        self._lock = threading.Lock()

        # Listeners para notificacao em tempo real
        self._listeners: List[queue.Queue] = []

    def log(self,
            agent_type: str,
            activity_type: ActivityType | str,
            title: str,
            description: str = "",
            details: Dict = None,
            task_id: str = None,
            duration_ms: int = None):
        """
        Registra uma atividade.

        Args:
            agent_type: Tipo do agente (BACK, FRONT, etc)
            activity_type: Tipo de atividade
            title: Titulo curto da atividade
            description: Descricao detalhada
            details: Detalhes extras (dict)
            task_id: ID da task relacionada
            duration_ms: Duracao em ms (se aplicavel)
        """
        # Converter enum para string se necessario
        if isinstance(activity_type, ActivityType):
            activity_type = activity_type.value

        activity = Activity(
            timestamp=datetime.now().isoformat(),
            agent_type=agent_type,
            activity_type=activity_type,
            title=title,
            description=description,
            details=details,
            task_id=task_id,
            duration_ms=duration_ms
        )

        with self._lock:
            # Adicionar ao buffer
            self.recent_activities.append(activity)
            if len(self.recent_activities) > self.max_recent:
                self.recent_activities.pop(0)

            # Salvar no arquivo realtime (append)
            self._append_to_realtime(activity)

            # Salvar no log diario do agente
            self._append_to_agent_log(activity)

            # Notificar listeners
            self._notify_listeners(activity)

    def _append_to_realtime(self, activity: Activity):
        """Adiciona ao arquivo de atividades em tempo real."""
        try:
            with open(self.realtime_file, 'a', encoding='utf-8') as f:
                f.write(json.dumps(activity.to_dict(), ensure_ascii=False) + "\n")

            # Limitar tamanho do arquivo (manter ultimas 500 linhas)
            self._truncate_file(self.realtime_file, 500)
        except Exception as e:
            print(f"[ActivityLogger] Erro ao salvar: {e}")

    def _append_to_agent_log(self, activity: Activity):
        """Adiciona ao log diario do agente."""
        try:
            date_str = datetime.now().strftime("%Y-%m-%d")
            agent_log = self.logs_path / f"{activity.agent_type}_{date_str}.jsonl"

            with open(agent_log, 'a', encoding='utf-8') as f:
                f.write(json.dumps(activity.to_dict(), ensure_ascii=False) + "\n")
        except Exception as e:
            print(f"[ActivityLogger] Erro ao salvar log do agente: {e}")

    def _truncate_file(self, file_path: Path, max_lines: int):
        """Trunca arquivo para manter apenas as ultimas N linhas."""
        try:
            if not file_path.exists():
                return

            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()

            if len(lines) > max_lines:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.writelines(lines[-max_lines:])
        except:
            pass

    def _notify_listeners(self, activity: Activity):
        """Notifica listeners sobre nova atividade."""
        for q in self._listeners:
            try:
                q.put_nowait(activity)
            except:
                pass

    def subscribe(self) -> queue.Queue:
        """Retorna uma queue para receber atividades em tempo real."""
        q = queue.Queue()
        self._listeners.append(q)
        return q

    def unsubscribe(self, q: queue.Queue):
        """Remove um listener."""
        if q in self._listeners:
            self._listeners.remove(q)

    def get_recent(self, limit: int = 50, agent_type: str = None) -> List[Dict]:
        """Retorna atividades recentes."""
        with self._lock:
            activities = self.recent_activities.copy()

        if agent_type:
            activities = [a for a in activities if a.agent_type == agent_type]

        # Retornar mais recentes primeiro
        activities = activities[-limit:]
        activities.reverse()

        return [a.to_dict() for a in activities]

    def get_agent_status(self) -> Dict[str, Dict]:
        """Retorna status atual de cada agente baseado nas atividades."""
        status = {}

        with self._lock:
            for activity in reversed(self.recent_activities):
                agent = activity.agent_type

                if agent not in status:
                    status[agent] = {
                        "agent_type": agent,
                        "last_activity": activity.timestamp,
                        "current_task": activity.task_id,
                        "current_action": activity.title,
                        "status": self._infer_status(activity.activity_type)
                    }

        return status

    def _infer_status(self, activity_type: str) -> str:
        """Infere o status do agente baseado no tipo de atividade."""
        if activity_type in ["agent_stop", "task_complete"]:
            return "idle"
        elif activity_type in ["error", "task_error"]:
            return "error"
        elif activity_type == "thinking":
            return "thinking"
        elif activity_type in ["file_write", "file_edit", "code_generate"]:
            return "coding"
        elif activity_type in ["test_run"]:
            return "testing"
        elif activity_type in ["handoff_out"]:
            return "handoff"
        else:
            return "working"

    def get_timeline(self, minutes: int = 30) -> List[Dict]:
        """Retorna timeline de atividades dos ultimos N minutos."""
        cutoff = datetime.now().timestamp() - (minutes * 60)

        activities = []

        # Ler do arquivo realtime
        try:
            if self.realtime_file.exists():
                with open(self.realtime_file, 'r', encoding='utf-8') as f:
                    for line in f:
                        try:
                            activity = json.loads(line.strip())
                            ts = datetime.fromisoformat(activity["timestamp"]).timestamp()
                            if ts >= cutoff:
                                activities.append(activity)
                        except:
                            continue
        except:
            pass

        # Ordenar por timestamp (mais recente primeiro)
        activities.sort(key=lambda x: x["timestamp"], reverse=True)

        return activities

    def clear_old_logs(self, days: int = 7):
        """Remove logs mais antigos que N dias."""
        cutoff = datetime.now().timestamp() - (days * 24 * 60 * 60)

        for log_file in self.logs_path.glob("*.jsonl"):
            try:
                if log_file.stat().st_mtime < cutoff:
                    log_file.unlink()
            except:
                pass


# Instancia global
_activity_logger: Optional[ActivityLogger] = None


def get_activity_logger(base_path: Optional[str] = None) -> ActivityLogger:
    """Retorna instancia global do ActivityLogger."""
    global _activity_logger
    if _activity_logger is None:
        _activity_logger = ActivityLogger(base_path)
    return _activity_logger


# Funcoes de conveniencia para logging rapido
def log_activity(agent_type: str, activity_type: str, title: str, **kwargs):
    """Log rapido de atividade."""
    logger = get_activity_logger()
    logger.log(agent_type, activity_type, title, **kwargs)


def log_agent_start(agent_type: str, task_id: str = None):
    """Log de inicio de agente."""
    log_activity(agent_type, ActivityType.AGENT_START,
                 f"Agente {agent_type} iniciado", task_id=task_id)


def log_agent_stop(agent_type: str):
    """Log de parada de agente."""
    log_activity(agent_type, ActivityType.AGENT_STOP,
                 f"Agente {agent_type} finalizado")


def log_task_start(agent_type: str, task_id: str, title: str):
    """Log de inicio de task."""
    log_activity(agent_type, ActivityType.TASK_START,
                 f"Iniciando: {title}", task_id=task_id)


def log_task_complete(agent_type: str, task_id: str):
    """Log de conclusao de task."""
    log_activity(agent_type, ActivityType.TASK_COMPLETE,
                 "Task concluida", task_id=task_id)


def log_thinking(agent_type: str, thought: str, task_id: str = None):
    """Log de pensamento/analise."""
    log_activity(agent_type, ActivityType.THINKING,
                 "Analisando...", description=thought, task_id=task_id)


def log_file_action(agent_type: str, action: str, file_path: str, task_id: str = None):
    """Log de acao em arquivo."""
    action_type = {
        "read": ActivityType.FILE_READ,
        "write": ActivityType.FILE_WRITE,
        "edit": ActivityType.FILE_EDIT
    }.get(action, ActivityType.INFO)

    log_activity(agent_type, action_type,
                 f"{action.capitalize()}: {Path(file_path).name}",
                 details={"file": file_path}, task_id=task_id)


def log_code_generate(agent_type: str, description: str, lines: int = 0, task_id: str = None):
    """Log de geracao de codigo."""
    log_activity(agent_type, ActivityType.CODE_GENERATE,
                 f"Codigo gerado: {description}",
                 details={"lines": lines}, task_id=task_id)


def log_test_result(agent_type: str, passed: bool, details: str = "", task_id: str = None):
    """Log de resultado de teste."""
    activity_type = ActivityType.TEST_PASS if passed else ActivityType.TEST_FAIL
    log_activity(agent_type, activity_type,
                 "Testes passaram" if passed else "Testes falharam",
                 description=details, task_id=task_id)


def log_git_commit(agent_type: str, message: str, task_id: str = None):
    """Log de commit git."""
    log_activity(agent_type, ActivityType.GIT_COMMIT,
                 f"Commit: {message[:50]}...",
                 details={"message": message}, task_id=task_id)


def log_handoff(agent_type: str, target_agent: str, reason: str, task_id: str = None):
    """Log de handoff para outro agente."""
    log_activity(agent_type, ActivityType.HANDOFF_OUT,
                 f"Handoff para {target_agent}",
                 description=reason,
                 details={"target": target_agent}, task_id=task_id)


def log_error(agent_type: str, error: str, task_id: str = None):
    """Log de erro."""
    log_activity(agent_type, ActivityType.ERROR,
                 "Erro encontrado",
                 description=error, task_id=task_id)

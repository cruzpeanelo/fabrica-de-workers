"""
Execution Recorder - Gravador de Execucoes para Replay e Debug
Issue #70: [Feature] Replay e Debug de Execucoes

Permite gravar cada passo de uma execucao para posterior replay e debug.

Funcionalidades:
- Gravar cada step do autonomous loop
- Armazenar prompts enviados ao Claude
- Armazenar responses recebidas
- Registrar mudancas em arquivos
- Capturar erros e stack traces
- Timeline de eventos com timestamps
"""
import uuid
import traceback
import difflib
from datetime import datetime
from typing import Optional, Dict, Any, List, Callable
from dataclasses import dataclass, field, asdict
from enum import Enum
from pathlib import Path
import json
import hashlib


class StepType(str, Enum):
    """Tipos de steps na execucao"""
    INITIALIZATION = "initialization"
    PARSING = "parsing"
    GENERATING = "generating"
    CLAUDE_REQUEST = "claude_request"
    CLAUDE_RESPONSE = "claude_response"
    FILE_CREATE = "file_create"
    FILE_MODIFY = "file_modify"
    FILE_DELETE = "file_delete"
    LINTING = "linting"
    TYPE_CHECKING = "type_checking"
    TESTING = "testing"
    SECURITY_SCAN = "security_scan"
    FIX_ATTEMPT = "fix_attempt"
    COMMITTING = "committing"
    ERROR = "error"
    COMPLETION = "completion"


class StepStatus(str, Enum):
    """Status de um step"""
    STARTED = "started"
    SUCCESS = "success"
    FAILED = "failed"
    SKIPPED = "skipped"


@dataclass
class FileChange:
    """Representa uma mudanca em arquivo"""
    path: str
    action: str  # create, modify, delete
    content_before: Optional[str] = None
    content_after: Optional[str] = None
    diff: Optional[str] = None
    size_before: int = 0
    size_after: int = 0
    timestamp: str = field(default_factory=lambda: datetime.utcnow().isoformat())

    def generate_diff(self):
        """Gera diff entre content_before e content_after"""
        if self.content_before is None and self.content_after is not None:
            # Novo arquivo
            self.diff = f"+++ {self.path} (new file)\n" + \
                       "\n".join(f"+ {line}" for line in self.content_after.split("\n"))
        elif self.content_before is not None and self.content_after is None:
            # Arquivo deletado
            self.diff = f"--- {self.path} (deleted)\n" + \
                       "\n".join(f"- {line}" for line in self.content_before.split("\n"))
        elif self.content_before and self.content_after:
            # Modificacao
            diff_lines = list(difflib.unified_diff(
                self.content_before.split("\n"),
                self.content_after.split("\n"),
                fromfile=f"a/{self.path}",
                tofile=f"b/{self.path}",
                lineterm=""
            ))
            self.diff = "\n".join(diff_lines)
        return self.diff

    def to_dict(self) -> dict:
        return asdict(self)


@dataclass
class ExecutionStep:
    """Representa um passo na execucao"""
    step_id: str
    step_number: int
    step_type: str
    name: str
    status: str = StepStatus.STARTED.value
    started_at: str = field(default_factory=lambda: datetime.utcnow().isoformat())
    ended_at: Optional[str] = None
    duration_ms: int = 0

    # Input/Output
    input_data: Dict[str, Any] = field(default_factory=dict)
    output_data: Dict[str, Any] = field(default_factory=dict)

    # Claude interaction
    prompt: Optional[str] = None
    response: Optional[str] = None
    model: Optional[str] = None
    tokens_input: int = 0
    tokens_output: int = 0

    # File changes
    file_changes: List[FileChange] = field(default_factory=list)

    # Error info
    error_message: Optional[str] = None
    error_type: Optional[str] = None
    stack_trace: Optional[str] = None

    # Metadata
    attempt: int = 1
    can_replay: bool = True
    notes: Optional[str] = None

    def complete(self, status: str = StepStatus.SUCCESS.value, output: Dict = None, error: str = None):
        """Marca step como completo"""
        self.ended_at = datetime.utcnow().isoformat()
        self.status = status
        if output:
            self.output_data = output
        if error:
            self.error_message = error
            self.status = StepStatus.FAILED.value

        # Calcular duracao
        start = datetime.fromisoformat(self.started_at)
        end = datetime.fromisoformat(self.ended_at)
        self.duration_ms = int((end - start).total_seconds() * 1000)

    def to_dict(self) -> dict:
        data = asdict(self)
        # Converter file_changes se necessario
        if self.file_changes:
            data["file_changes"] = [
                fc.to_dict() if hasattr(fc, 'to_dict') else fc
                for fc in self.file_changes
            ]
        return data


class ExecutionRecorder:
    """
    Gravador de Execucoes

    Grava cada passo de uma execucao de job/task para permitir
    replay e debugging posterior.

    Uso:
        recorder = ExecutionRecorder(job_id="JOB-001")

        with recorder.step("parsing", StepType.PARSING) as step:
            step.input_data = {"description": "..."}
            # ... executar parsing ...
            step.output_data = {"requirements": {...}}

        with recorder.step("generating", StepType.GENERATING) as step:
            step.prompt = "Generate code for..."
            # ... chamar Claude ...
            step.response = "Here is the code..."

        recorder.complete()
        execution_data = recorder.get_execution()
    """

    def __init__(
        self,
        job_id: str = None,
        task_id: str = None,
        story_id: str = None,
        project_id: str = None,
        worker_id: str = None,
        agent_model: str = None,
        original_input: Dict = None,
        replay_of: str = None
    ):
        """
        Inicializa gravador de execucao

        Args:
            job_id: ID do job sendo executado
            task_id: ID da task (se for execucao de task)
            story_id: ID da story (se aplicavel)
            project_id: ID do projeto
            worker_id: ID do worker executando
            agent_model: Modelo do Claude sendo usado
            original_input: Input original da execucao
            replay_of: ID de execucao anterior (se for replay)
        """
        self.execution_id = f"EXEC-{uuid.uuid4().hex[:12].upper()}"
        self.job_id = job_id
        self.task_id = task_id
        self.story_id = story_id
        self.project_id = project_id
        self.worker_id = worker_id
        self.agent_model = agent_model
        self.original_input = original_input or {}
        self.replay_of = replay_of

        self.status = "running"
        self.started_at = datetime.utcnow()
        self.ended_at: Optional[datetime] = None
        self.duration_ms = 0

        self.steps: List[ExecutionStep] = []
        self._step_counter = 0
        self._current_step: Optional[ExecutionStep] = None

        # Files tracking
        self.files_created: List[str] = []
        self.files_modified: List[str] = []
        self.files_deleted: List[str] = []
        self._file_snapshots: Dict[str, str] = {}  # path -> content snapshot

        # Metrics
        self.total_tokens = 0
        self.total_cost = 0.0

        # Error tracking
        self.error_message: Optional[str] = None
        self.error_type: Optional[str] = None
        self.stack_trace: Optional[str] = None

        # Output
        self.output: Dict[str, Any] = {}

        # Callbacks
        self._on_step_start: Optional[Callable] = None
        self._on_step_end: Optional[Callable] = None
        self._on_error: Optional[Callable] = None

    def set_callbacks(
        self,
        on_step_start: Callable = None,
        on_step_end: Callable = None,
        on_error: Callable = None
    ):
        """Define callbacks para eventos"""
        self._on_step_start = on_step_start
        self._on_step_end = on_step_end
        self._on_error = on_error

    def snapshot_file(self, path: str) -> Optional[str]:
        """
        Tira snapshot de um arquivo para tracking de mudancas

        Args:
            path: Caminho do arquivo

        Returns:
            Conteudo do arquivo ou None se nao existir
        """
        try:
            p = Path(path)
            if p.exists() and p.is_file():
                content = p.read_text(encoding='utf-8', errors='replace')
                self._file_snapshots[str(path)] = content
                return content
        except Exception:
            pass
        return None

    def record_file_change(
        self,
        path: str,
        action: str,
        content_after: str = None
    ) -> FileChange:
        """
        Registra mudanca em arquivo

        Args:
            path: Caminho do arquivo
            action: create, modify, delete
            content_after: Conteudo apos a mudanca

        Returns:
            FileChange registrado
        """
        content_before = self._file_snapshots.get(str(path))

        change = FileChange(
            path=str(path),
            action=action,
            content_before=content_before,
            content_after=content_after,
            size_before=len(content_before) if content_before else 0,
            size_after=len(content_after) if content_after else 0
        )
        change.generate_diff()

        # Track no recorder
        if action == "create":
            if str(path) not in self.files_created:
                self.files_created.append(str(path))
        elif action == "modify":
            if str(path) not in self.files_modified:
                self.files_modified.append(str(path))
        elif action == "delete":
            if str(path) not in self.files_deleted:
                self.files_deleted.append(str(path))

        # Adicionar ao step atual se existir
        if self._current_step:
            self._current_step.file_changes.append(change)

        # Atualizar snapshot
        if content_after:
            self._file_snapshots[str(path)] = content_after
        elif str(path) in self._file_snapshots:
            del self._file_snapshots[str(path)]

        return change

    class _StepContext:
        """Context manager para um step"""
        def __init__(self, recorder: 'ExecutionRecorder', step: ExecutionStep):
            self.recorder = recorder
            self.step = step

        def __enter__(self) -> ExecutionStep:
            self.recorder._current_step = self.step
            if self.recorder._on_step_start:
                try:
                    self.recorder._on_step_start(self.step)
                except Exception:
                    pass
            return self.step

        def __exit__(self, exc_type, exc_val, exc_tb):
            if exc_type:
                self.step.error_type = exc_type.__name__
                self.step.error_message = str(exc_val)
                self.step.stack_trace = traceback.format_exc()
                self.step.complete(status=StepStatus.FAILED.value)
                if self.recorder._on_error:
                    try:
                        self.recorder._on_error(self.step, exc_val)
                    except Exception:
                        pass
            elif self.step.status == StepStatus.STARTED.value:
                self.step.complete(status=StepStatus.SUCCESS.value)

            if self.recorder._on_step_end:
                try:
                    self.recorder._on_step_end(self.step)
                except Exception:
                    pass

            self.recorder._current_step = None
            return False  # Nao suprimir excecoes

    def step(self, name: str, step_type: StepType, attempt: int = 1) -> _StepContext:
        """
        Inicia gravacao de um step

        Args:
            name: Nome descritivo do step
            step_type: Tipo do step
            attempt: Numero da tentativa (se for retry)

        Returns:
            Context manager que retorna ExecutionStep
        """
        self._step_counter += 1

        step = ExecutionStep(
            step_id=f"STEP-{self._step_counter:03d}",
            step_number=self._step_counter,
            step_type=step_type.value if isinstance(step_type, StepType) else step_type,
            name=name,
            attempt=attempt,
            model=self.agent_model
        )

        self.steps.append(step)
        return self._StepContext(self, step)

    def add_step(
        self,
        name: str,
        step_type: StepType,
        input_data: Dict = None,
        output_data: Dict = None,
        prompt: str = None,
        response: str = None,
        tokens_input: int = 0,
        tokens_output: int = 0,
        error: str = None,
        duration_ms: int = 0
    ) -> ExecutionStep:
        """
        Adiciona step ja completo (para uso sincrono)

        Args:
            name: Nome do step
            step_type: Tipo do step
            input_data: Dados de entrada
            output_data: Dados de saida
            prompt: Prompt enviado ao Claude
            response: Resposta do Claude
            tokens_input: Tokens de entrada
            tokens_output: Tokens de saida
            error: Mensagem de erro se houver
            duration_ms: Duracao em milissegundos

        Returns:
            ExecutionStep criado
        """
        self._step_counter += 1

        step = ExecutionStep(
            step_id=f"STEP-{self._step_counter:03d}",
            step_number=self._step_counter,
            step_type=step_type.value if isinstance(step_type, StepType) else step_type,
            name=name,
            input_data=input_data or {},
            output_data=output_data or {},
            prompt=prompt,
            response=response,
            tokens_input=tokens_input,
            tokens_output=tokens_output,
            model=self.agent_model,
            status=StepStatus.FAILED.value if error else StepStatus.SUCCESS.value,
            error_message=error,
            duration_ms=duration_ms
        )

        step.ended_at = datetime.utcnow().isoformat()

        self.steps.append(step)

        # Atualizar tokens totais
        self.total_tokens += tokens_input + tokens_output

        return step

    def record_claude_interaction(
        self,
        prompt: str,
        response: str,
        model: str = None,
        tokens_input: int = 0,
        tokens_output: int = 0,
        duration_ms: int = 0
    ) -> ExecutionStep:
        """
        Registra interacao com Claude API

        Args:
            prompt: Prompt enviado
            response: Resposta recebida
            model: Modelo usado
            tokens_input: Tokens de entrada
            tokens_output: Tokens de saida
            duration_ms: Duracao da chamada

        Returns:
            ExecutionStep da interacao
        """
        return self.add_step(
            name="Claude API Call",
            step_type=StepType.CLAUDE_RESPONSE,
            input_data={"prompt_length": len(prompt)},
            output_data={"response_length": len(response)},
            prompt=prompt,
            response=response,
            tokens_input=tokens_input,
            tokens_output=tokens_output,
            duration_ms=duration_ms
        )

    def record_error(
        self,
        error: Exception,
        step_name: str = "Error",
        context: Dict = None
    ) -> ExecutionStep:
        """
        Registra erro na execucao

        Args:
            error: Excecao ocorrida
            step_name: Nome do step onde ocorreu
            context: Contexto adicional

        Returns:
            ExecutionStep do erro
        """
        self._step_counter += 1

        step = ExecutionStep(
            step_id=f"STEP-{self._step_counter:03d}",
            step_number=self._step_counter,
            step_type=StepType.ERROR.value,
            name=step_name,
            status=StepStatus.FAILED.value,
            input_data=context or {},
            error_type=type(error).__name__,
            error_message=str(error),
            stack_trace=traceback.format_exc()
        )
        step.ended_at = datetime.utcnow().isoformat()

        self.steps.append(step)

        # Atualizar erro principal se for o primeiro
        if not self.error_message:
            self.error_message = str(error)
            self.error_type = type(error).__name__
            self.stack_trace = step.stack_trace

        return step

    def complete(
        self,
        status: str = "success",
        output: Dict = None,
        error: str = None
    ):
        """
        Finaliza gravacao da execucao

        Args:
            status: Status final (success, failed, cancelled)
            output: Output da execucao
            error: Mensagem de erro se houver
        """
        self.ended_at = datetime.utcnow()
        self.status = status

        if output:
            self.output = output

        if error:
            self.error_message = error
            self.status = "failed"

        # Calcular duracao total
        self.duration_ms = int((self.ended_at - self.started_at).total_seconds() * 1000)

        # Somar tokens de todos os steps
        self.total_tokens = sum(
            (s.tokens_input or 0) + (s.tokens_output or 0)
            for s in self.steps
        )

    def get_execution(self) -> Dict[str, Any]:
        """
        Retorna dados completos da execucao

        Returns:
            Dicionario com todos os dados da execucao
        """
        return {
            "execution_id": self.execution_id,
            "job_id": self.job_id,
            "task_id": self.task_id,
            "story_id": self.story_id,
            "project_id": self.project_id,
            "worker_id": self.worker_id,
            "agent_model": self.agent_model,
            "status": self.status,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "ended_at": self.ended_at.isoformat() if self.ended_at else None,
            "duration_ms": self.duration_ms,
            "original_input": self.original_input,
            "output": self.output,
            "steps": [s.to_dict() for s in self.steps],
            "files_created": self.files_created,
            "files_modified": self.files_modified,
            "files_deleted": self.files_deleted,
            "total_tokens": self.total_tokens,
            "total_cost": self.total_cost,
            "error_message": self.error_message,
            "error_type": self.error_type,
            "stack_trace": self.stack_trace,
            "replay_of": self.replay_of,
            "replay_count": 0,
            "steps_count": len(self.steps),
            "steps_success": len([s for s in self.steps if s.status == StepStatus.SUCCESS.value]),
            "steps_failed": len([s for s in self.steps if s.status == StepStatus.FAILED.value])
        }

    def get_timeline(self) -> List[Dict[str, Any]]:
        """
        Retorna timeline simplificada para visualizacao

        Returns:
            Lista de eventos na timeline
        """
        events = []

        # Evento inicial
        events.append({
            "id": f"{self.execution_id}-start",
            "type": "execution_start",
            "name": "Execucao Iniciada",
            "timestamp": self.started_at.isoformat() if self.started_at else None,
            "status": "success",
            "duration_ms": 0
        })

        # Steps
        for step in self.steps:
            events.append({
                "id": step.step_id,
                "type": step.step_type,
                "name": step.name,
                "timestamp": step.started_at,
                "status": step.status,
                "duration_ms": step.duration_ms,
                "has_prompt": bool(step.prompt),
                "has_response": bool(step.response),
                "has_error": bool(step.error_message),
                "files_changed": len(step.file_changes),
                "attempt": step.attempt
            })

        # Evento final
        events.append({
            "id": f"{self.execution_id}-end",
            "type": "execution_end",
            "name": "Execucao Finalizada" if self.status == "success" else "Execucao Falhou",
            "timestamp": self.ended_at.isoformat() if self.ended_at else None,
            "status": self.status,
            "duration_ms": self.duration_ms
        })

        return events

    def get_step(self, step_id: str) -> Optional[ExecutionStep]:
        """
        Busca um step pelo ID

        Args:
            step_id: ID do step

        Returns:
            ExecutionStep ou None
        """
        for step in self.steps:
            if step.step_id == step_id:
                return step
        return None

    def get_failed_steps(self) -> List[ExecutionStep]:
        """Retorna lista de steps que falharam"""
        return [s for s in self.steps if s.status == StepStatus.FAILED.value]

    def get_claude_interactions(self) -> List[ExecutionStep]:
        """Retorna lista de interacoes com Claude"""
        return [
            s for s in self.steps
            if s.step_type in [StepType.CLAUDE_REQUEST.value, StepType.CLAUDE_RESPONSE.value]
            or s.prompt or s.response
        ]

    def export_json(self, path: str = None) -> str:
        """
        Exporta execucao para JSON

        Args:
            path: Caminho do arquivo (opcional)

        Returns:
            JSON string
        """
        data = self.get_execution()
        json_str = json.dumps(data, indent=2, ensure_ascii=False)

        if path:
            Path(path).write_text(json_str, encoding='utf-8')

        return json_str

    @classmethod
    def from_json(cls, json_data: str | Dict) -> 'ExecutionRecorder':
        """
        Recria ExecutionRecorder a partir de JSON

        Args:
            json_data: JSON string ou dict

        Returns:
            ExecutionRecorder
        """
        if isinstance(json_data, str):
            data = json.loads(json_data)
        else:
            data = json_data

        recorder = cls(
            job_id=data.get("job_id"),
            task_id=data.get("task_id"),
            story_id=data.get("story_id"),
            project_id=data.get("project_id"),
            worker_id=data.get("worker_id"),
            agent_model=data.get("agent_model"),
            original_input=data.get("original_input"),
            replay_of=data.get("replay_of")
        )

        recorder.execution_id = data.get("execution_id", recorder.execution_id)
        recorder.status = data.get("status", "running")
        recorder.started_at = datetime.fromisoformat(data["started_at"]) if data.get("started_at") else datetime.utcnow()
        recorder.ended_at = datetime.fromisoformat(data["ended_at"]) if data.get("ended_at") else None
        recorder.duration_ms = data.get("duration_ms", 0)
        recorder.output = data.get("output", {})
        recorder.files_created = data.get("files_created", [])
        recorder.files_modified = data.get("files_modified", [])
        recorder.files_deleted = data.get("files_deleted", [])
        recorder.total_tokens = data.get("total_tokens", 0)
        recorder.total_cost = data.get("total_cost", 0.0)
        recorder.error_message = data.get("error_message")
        recorder.error_type = data.get("error_type")
        recorder.stack_trace = data.get("stack_trace")

        # Reconstruir steps
        for step_data in data.get("steps", []):
            step = ExecutionStep(
                step_id=step_data.get("step_id", f"STEP-{recorder._step_counter + 1:03d}"),
                step_number=step_data.get("step_number", recorder._step_counter + 1),
                step_type=step_data.get("step_type", "unknown"),
                name=step_data.get("name", "Unknown Step"),
                status=step_data.get("status", "success"),
                started_at=step_data.get("started_at"),
                ended_at=step_data.get("ended_at"),
                duration_ms=step_data.get("duration_ms", 0),
                input_data=step_data.get("input_data", {}),
                output_data=step_data.get("output_data", {}),
                prompt=step_data.get("prompt"),
                response=step_data.get("response"),
                model=step_data.get("model"),
                tokens_input=step_data.get("tokens_input", 0),
                tokens_output=step_data.get("tokens_output", 0),
                error_message=step_data.get("error_message"),
                error_type=step_data.get("error_type"),
                stack_trace=step_data.get("stack_trace"),
                attempt=step_data.get("attempt", 1),
                can_replay=step_data.get("can_replay", True),
                notes=step_data.get("notes")
            )

            # Reconstruir file_changes
            for fc_data in step_data.get("file_changes", []):
                fc = FileChange(
                    path=fc_data.get("path", ""),
                    action=fc_data.get("action", "modify"),
                    content_before=fc_data.get("content_before"),
                    content_after=fc_data.get("content_after"),
                    diff=fc_data.get("diff"),
                    size_before=fc_data.get("size_before", 0),
                    size_after=fc_data.get("size_after", 0),
                    timestamp=fc_data.get("timestamp")
                )
                step.file_changes.append(fc)

            recorder.steps.append(step)
            recorder._step_counter += 1

        return recorder


# =============================================================================
# DATABASE INTEGRATION
# =============================================================================

def save_execution_to_db(recorder: ExecutionRecorder, db_session=None) -> str:
    """
    Salva execucao no banco de dados

    Args:
        recorder: ExecutionRecorder com os dados
        db_session: Sessao do SQLAlchemy (opcional)

    Returns:
        ID da execucao salva
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import ExecutionLog

    db = db_session or SessionLocal()
    close_db = db_session is None

    try:
        execution_data = recorder.get_execution()

        execution_log = ExecutionLog(
            execution_id=execution_data["execution_id"],
            task_id=execution_data.get("task_id"),
            story_id=execution_data.get("story_id"),
            project_id=execution_data.get("project_id"),
            job_id=execution_data.get("job_id"),
            status=execution_data["status"],
            started_at=recorder.started_at,
            ended_at=recorder.ended_at,
            duration_ms=execution_data["duration_ms"],
            steps=execution_data["steps"],
            original_input=execution_data["original_input"],
            output=execution_data["output"],
            files_created=execution_data["files_created"],
            files_modified=execution_data["files_modified"],
            error_message=execution_data.get("error_message"),
            error_type=execution_data.get("error_type"),
            stack_trace=execution_data.get("stack_trace"),
            worker_id=execution_data.get("worker_id"),
            agent_model=execution_data.get("agent_model"),
            total_tokens=execution_data["total_tokens"],
            total_cost=execution_data["total_cost"],
            replay_of=execution_data.get("replay_of"),
            replay_count=execution_data.get("replay_count", 0)
        )

        db.add(execution_log)
        db.commit()

        return execution_data["execution_id"]

    finally:
        if close_db:
            db.close()


def load_execution_from_db(execution_id: str, db_session=None) -> Optional[ExecutionRecorder]:
    """
    Carrega execucao do banco de dados

    Args:
        execution_id: ID da execucao
        db_session: Sessao do SQLAlchemy (opcional)

    Returns:
        ExecutionRecorder ou None se nao encontrar
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import ExecutionLog

    db = db_session or SessionLocal()
    close_db = db_session is None

    try:
        execution = db.query(ExecutionLog).filter(
            ExecutionLog.execution_id == execution_id
        ).first()

        if not execution:
            return None

        return ExecutionRecorder.from_json(execution.to_dict())

    finally:
        if close_db:
            db.close()


# =============================================================================
# SINGLETON
# =============================================================================

_active_recorders: Dict[str, ExecutionRecorder] = {}


def get_recorder(job_id: str = None, task_id: str = None) -> Optional[ExecutionRecorder]:
    """
    Obtem recorder ativo para um job ou task

    Args:
        job_id: ID do job
        task_id: ID da task

    Returns:
        ExecutionRecorder ativo ou None
    """
    key = task_id or job_id
    return _active_recorders.get(key)


def create_recorder(
    job_id: str = None,
    task_id: str = None,
    **kwargs
) -> ExecutionRecorder:
    """
    Cria novo recorder e registra como ativo

    Args:
        job_id: ID do job
        task_id: ID da task
        **kwargs: Argumentos adicionais para ExecutionRecorder

    Returns:
        ExecutionRecorder criado
    """
    recorder = ExecutionRecorder(job_id=job_id, task_id=task_id, **kwargs)
    key = task_id or job_id
    if key:
        _active_recorders[key] = recorder
    return recorder


def finish_recorder(job_id: str = None, task_id: str = None, save_to_db: bool = True):
    """
    Finaliza e remove recorder ativo

    Args:
        job_id: ID do job
        task_id: ID da task
        save_to_db: Se deve salvar no banco de dados
    """
    key = task_id or job_id
    if key in _active_recorders:
        recorder = _active_recorders.pop(key)
        if save_to_db:
            try:
                save_execution_to_db(recorder)
            except Exception as e:
                print(f"[ExecutionRecorder] Error saving to DB: {e}")

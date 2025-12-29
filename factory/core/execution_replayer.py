"""
Execution Replayer - Reprodutor de Execucoes para Debug
Issue #70: [Feature] Replay e Debug de Execucoes

Permite reproduzir uma execucao gravada passo a passo.

Funcionalidades:
- Replay passo a passo
- Controle de velocidade (0.5x, 1x, 2x)
- Visualizacao de diffs
- Re-execucao a partir de ponto especifico
- Comparacao entre execucoes
"""
import asyncio
import json
from datetime import datetime
from typing import Optional, Dict, Any, List, Callable
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
import difflib

from factory.core.execution_recorder import (
    ExecutionRecorder,
    ExecutionStep,
    FileChange,
    StepType,
    StepStatus,
    load_execution_from_db,
    save_execution_to_db
)


class ReplayState(str, Enum):
    """Estados do replay"""
    IDLE = "idle"
    PLAYING = "playing"
    PAUSED = "paused"
    STEPPING = "stepping"
    COMPLETED = "completed"
    ERROR = "error"


class ReplaySpeed(float, Enum):
    """Velocidades de replay"""
    SLOW = 0.5
    NORMAL = 1.0
    FAST = 2.0
    INSTANT = 0.0


@dataclass
class ReplayResult:
    """Resultado de um replay step"""
    step_id: str
    step_number: int
    step_type: str
    name: str
    status: str
    duration_ms: int
    input_data: Dict[str, Any]
    output_data: Dict[str, Any]
    prompt: Optional[str] = None
    response: Optional[str] = None
    file_changes: List[Dict] = field(default_factory=list)
    error_message: Optional[str] = None
    can_rerun: bool = True


@dataclass
class ComparisonResult:
    """Resultado de comparacao entre execucoes"""
    execution1_id: str
    execution2_id: str
    same_status: bool
    same_steps_count: bool
    steps_diff: List[Dict[str, Any]]
    files_diff: Dict[str, Any]
    duration_diff_ms: int
    tokens_diff: int
    summary: str


class ExecutionReplayer:
    """
    Reprodutor de Execucoes

    Permite reproduzir uma execucao gravada passo a passo,
    com controle de velocidade e visualizacao detalhada.

    Uso:
        replayer = ExecutionReplayer(execution_id="EXEC-ABC123")

        # Replay automatico
        async for step in replayer.play():
            print(f"Step: {step.name}")

        # Replay manual (step by step)
        while replayer.has_next():
            step = await replayer.next_step()
            print(f"Step: {step.name}")

        # Ir para step especifico
        await replayer.goto_step("STEP-005")

        # Re-executar a partir de um ponto
        new_execution = await replayer.rerun_from_step("STEP-003")
    """

    def __init__(
        self,
        execution_id: str = None,
        recorder: ExecutionRecorder = None,
        speed: ReplaySpeed = ReplaySpeed.NORMAL,
        on_step: Callable = None,
        on_complete: Callable = None,
        on_error: Callable = None
    ):
        """
        Inicializa replayer

        Args:
            execution_id: ID da execucao a reproduzir
            recorder: ExecutionRecorder com dados (alternativa a execution_id)
            speed: Velocidade de replay
            on_step: Callback chamado em cada step
            on_complete: Callback quando completar
            on_error: Callback em caso de erro
        """
        self.execution_id = execution_id
        self._recorder = recorder
        self.speed = speed
        self._on_step = on_step
        self._on_complete = on_complete
        self._on_error = on_error

        self.state = ReplayState.IDLE
        self._current_step_index = -1
        self._steps: List[ExecutionStep] = []
        self._execution_data: Dict[str, Any] = {}

        self._play_task: Optional[asyncio.Task] = None
        self._paused_event = asyncio.Event()
        self._paused_event.set()  # Nao pausado inicialmente

    async def load(self) -> bool:
        """
        Carrega execucao do banco de dados

        Returns:
            True se carregou com sucesso
        """
        if self._recorder:
            self._execution_data = self._recorder.get_execution()
            self._steps = self._recorder.steps
            return True

        if not self.execution_id:
            return False

        recorder = load_execution_from_db(self.execution_id)
        if not recorder:
            return False

        self._recorder = recorder
        self._execution_data = recorder.get_execution()
        self._steps = recorder.steps

        return True

    def get_execution_info(self) -> Dict[str, Any]:
        """
        Retorna informacoes da execucao

        Returns:
            Dicionario com info da execucao
        """
        if not self._execution_data:
            return {}

        return {
            "execution_id": self._execution_data.get("execution_id"),
            "job_id": self._execution_data.get("job_id"),
            "task_id": self._execution_data.get("task_id"),
            "story_id": self._execution_data.get("story_id"),
            "project_id": self._execution_data.get("project_id"),
            "status": self._execution_data.get("status"),
            "started_at": self._execution_data.get("started_at"),
            "ended_at": self._execution_data.get("ended_at"),
            "duration_ms": self._execution_data.get("duration_ms"),
            "steps_count": len(self._steps),
            "steps_success": self._execution_data.get("steps_success", 0),
            "steps_failed": self._execution_data.get("steps_failed", 0),
            "total_tokens": self._execution_data.get("total_tokens", 0),
            "files_created": self._execution_data.get("files_created", []),
            "files_modified": self._execution_data.get("files_modified", []),
            "error_message": self._execution_data.get("error_message"),
            "replay_state": self.state.value,
            "current_step": self._current_step_index + 1
        }

    def get_timeline(self) -> List[Dict[str, Any]]:
        """
        Retorna timeline da execucao

        Returns:
            Lista de eventos na timeline
        """
        if self._recorder:
            return self._recorder.get_timeline()

        events = []
        for step in self._steps:
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
                "files_changed": len(step.file_changes) if step.file_changes else 0,
                "attempt": step.attempt,
                "is_current": self._steps.index(step) == self._current_step_index
            })

        return events

    def get_step_details(self, step_id: str) -> Optional[Dict[str, Any]]:
        """
        Retorna detalhes de um step especifico

        Args:
            step_id: ID do step

        Returns:
            Detalhes do step ou None
        """
        for step in self._steps:
            if step.step_id == step_id:
                return {
                    "step_id": step.step_id,
                    "step_number": step.step_number,
                    "step_type": step.step_type,
                    "name": step.name,
                    "status": step.status,
                    "started_at": step.started_at,
                    "ended_at": step.ended_at,
                    "duration_ms": step.duration_ms,
                    "input_data": step.input_data,
                    "output_data": step.output_data,
                    "prompt": step.prompt,
                    "response": step.response,
                    "model": step.model,
                    "tokens_input": step.tokens_input,
                    "tokens_output": step.tokens_output,
                    "file_changes": [
                        fc.to_dict() if hasattr(fc, 'to_dict') else fc
                        for fc in (step.file_changes or [])
                    ],
                    "error_message": step.error_message,
                    "error_type": step.error_type,
                    "stack_trace": step.stack_trace,
                    "attempt": step.attempt,
                    "can_replay": step.can_replay,
                    "notes": step.notes
                }
        return None

    def has_next(self) -> bool:
        """Verifica se ha proximo step"""
        return self._current_step_index < len(self._steps) - 1

    def has_previous(self) -> bool:
        """Verifica se ha step anterior"""
        return self._current_step_index > 0

    @property
    def current_step(self) -> Optional[ExecutionStep]:
        """Step atual"""
        if 0 <= self._current_step_index < len(self._steps):
            return self._steps[self._current_step_index]
        return None

    @property
    def progress(self) -> float:
        """Progresso do replay (0-100)"""
        if not self._steps:
            return 0.0
        return ((self._current_step_index + 1) / len(self._steps)) * 100

    async def _delay_for_step(self, step: ExecutionStep):
        """Aplica delay baseado na velocidade e duracao do step"""
        if self.speed == ReplaySpeed.INSTANT or self.speed.value == 0:
            return

        # Calcular delay proporcional a duracao original
        original_duration = step.duration_ms / 1000  # Converter para segundos
        delay = original_duration / self.speed.value

        # Limitar delay entre 0.1s e 5s
        delay = max(0.1, min(5.0, delay))

        await asyncio.sleep(delay)

    async def next_step(self) -> Optional[ReplayResult]:
        """
        Avanca para proximo step

        Returns:
            ReplayResult do step ou None se nao houver mais
        """
        if not self.has_next():
            self.state = ReplayState.COMPLETED
            if self._on_complete:
                await self._call_callback(self._on_complete)
            return None

        self.state = ReplayState.STEPPING
        self._current_step_index += 1
        step = self._steps[self._current_step_index]

        result = ReplayResult(
            step_id=step.step_id,
            step_number=step.step_number,
            step_type=step.step_type,
            name=step.name,
            status=step.status,
            duration_ms=step.duration_ms,
            input_data=step.input_data,
            output_data=step.output_data,
            prompt=step.prompt,
            response=step.response,
            file_changes=[
                fc.to_dict() if hasattr(fc, 'to_dict') else fc
                for fc in (step.file_changes or [])
            ],
            error_message=step.error_message,
            can_rerun=step.can_replay
        )

        if self._on_step:
            await self._call_callback(self._on_step, result)

        return result

    async def previous_step(self) -> Optional[ReplayResult]:
        """
        Volta para step anterior

        Returns:
            ReplayResult do step ou None se nao houver
        """
        if not self.has_previous():
            return None

        self._current_step_index -= 1
        step = self._steps[self._current_step_index]

        return ReplayResult(
            step_id=step.step_id,
            step_number=step.step_number,
            step_type=step.step_type,
            name=step.name,
            status=step.status,
            duration_ms=step.duration_ms,
            input_data=step.input_data,
            output_data=step.output_data,
            prompt=step.prompt,
            response=step.response,
            file_changes=[
                fc.to_dict() if hasattr(fc, 'to_dict') else fc
                for fc in (step.file_changes or [])
            ],
            error_message=step.error_message,
            can_rerun=step.can_replay
        )

    async def goto_step(self, step_id: str) -> Optional[ReplayResult]:
        """
        Vai para um step especifico

        Args:
            step_id: ID do step

        Returns:
            ReplayResult do step ou None se nao encontrar
        """
        for i, step in enumerate(self._steps):
            if step.step_id == step_id:
                self._current_step_index = i
                return ReplayResult(
                    step_id=step.step_id,
                    step_number=step.step_number,
                    step_type=step.step_type,
                    name=step.name,
                    status=step.status,
                    duration_ms=step.duration_ms,
                    input_data=step.input_data,
                    output_data=step.output_data,
                    prompt=step.prompt,
                    response=step.response,
                    file_changes=[
                        fc.to_dict() if hasattr(fc, 'to_dict') else fc
                        for fc in (step.file_changes or [])
                    ],
                    error_message=step.error_message,
                    can_rerun=step.can_replay
                )
        return None

    async def play(self):
        """
        Inicia replay automatico (async generator)

        Yields:
            ReplayResult para cada step
        """
        if not await self.load():
            raise ValueError(f"Could not load execution: {self.execution_id}")

        self.state = ReplayState.PLAYING
        self._current_step_index = -1

        while self.has_next():
            # Verificar pausa
            await self._paused_event.wait()

            if self.state == ReplayState.ERROR:
                break

            result = await self.next_step()
            if result:
                yield result

                # Aplicar delay
                step = self._steps[self._current_step_index]
                await self._delay_for_step(step)

        self.state = ReplayState.COMPLETED
        if self._on_complete:
            await self._call_callback(self._on_complete)

    def pause(self):
        """Pausa o replay"""
        if self.state == ReplayState.PLAYING:
            self.state = ReplayState.PAUSED
            self._paused_event.clear()

    def resume(self):
        """Retoma o replay"""
        if self.state == ReplayState.PAUSED:
            self.state = ReplayState.PLAYING
            self._paused_event.set()

    def stop(self):
        """Para o replay"""
        self.state = ReplayState.IDLE
        self._current_step_index = -1
        self._paused_event.set()

        if self._play_task:
            self._play_task.cancel()

    def set_speed(self, speed: ReplaySpeed):
        """Define velocidade do replay"""
        self.speed = speed

    def reset(self):
        """Reseta replay para o inicio"""
        self._current_step_index = -1
        self.state = ReplayState.IDLE
        self._paused_event.set()

    async def rerun_from_step(
        self,
        step_id: str,
        claude_client=None,
        model: str = None,
        on_step_update: Callable = None
    ) -> Optional[ExecutionRecorder]:
        """
        Re-executa a partir de um step especifico

        Args:
            step_id: ID do step a partir do qual re-executar
            claude_client: Cliente Claude para re-execucao
            model: Modelo a usar
            on_step_update: Callback para atualizacao

        Returns:
            Novo ExecutionRecorder com a nova execucao
        """
        # Encontrar step
        start_index = None
        for i, step in enumerate(self._steps):
            if step.step_id == step_id:
                start_index = i
                break

        if start_index is None:
            return None

        # Criar novo recorder como replay
        from factory.core.execution_recorder import create_recorder

        new_recorder = create_recorder(
            job_id=self._execution_data.get("job_id"),
            task_id=self._execution_data.get("task_id"),
            story_id=self._execution_data.get("story_id"),
            project_id=self._execution_data.get("project_id"),
            worker_id=self._execution_data.get("worker_id"),
            agent_model=model or self._execution_data.get("agent_model"),
            original_input=self._execution_data.get("original_input"),
            replay_of=self.execution_id
        )

        # Copiar steps anteriores ao ponto de re-execucao
        for i in range(start_index):
            old_step = self._steps[i]
            new_recorder.add_step(
                name=old_step.name,
                step_type=old_step.step_type,
                input_data=old_step.input_data,
                output_data=old_step.output_data,
                prompt=old_step.prompt,
                response=old_step.response,
                tokens_input=old_step.tokens_input,
                tokens_output=old_step.tokens_output,
                duration_ms=old_step.duration_ms
            )

        # Re-executar a partir do step especificado
        # Isso requer integracao com o autonomous loop
        # Por enquanto, retornamos o recorder preparado

        return new_recorder

    async def _call_callback(self, callback: Callable, *args):
        """Chama callback de forma segura"""
        try:
            if asyncio.iscoroutinefunction(callback):
                await callback(*args)
            else:
                callback(*args)
        except Exception as e:
            print(f"[Replayer] Callback error: {e}")


class ExecutionComparator:
    """
    Comparador de Execucoes

    Compara duas execucoes para identificar diferencas e entender
    por que uma funcionou e outra nao.
    """

    def __init__(
        self,
        execution1_id: str = None,
        execution2_id: str = None,
        recorder1: ExecutionRecorder = None,
        recorder2: ExecutionRecorder = None
    ):
        """
        Inicializa comparador

        Args:
            execution1_id: ID da primeira execucao
            execution2_id: ID da segunda execucao
            recorder1: ExecutionRecorder da primeira (alternativa)
            recorder2: ExecutionRecorder da segunda (alternativa)
        """
        self.execution1_id = execution1_id
        self.execution2_id = execution2_id
        self._recorder1 = recorder1
        self._recorder2 = recorder2

    async def load(self) -> bool:
        """Carrega execucoes"""
        if not self._recorder1 and self.execution1_id:
            self._recorder1 = load_execution_from_db(self.execution1_id)

        if not self._recorder2 and self.execution2_id:
            self._recorder2 = load_execution_from_db(self.execution2_id)

        return self._recorder1 is not None and self._recorder2 is not None

    def compare(self) -> ComparisonResult:
        """
        Compara as duas execucoes

        Returns:
            ComparisonResult com as diferencas
        """
        if not self._recorder1 or not self._recorder2:
            raise ValueError("Both executions must be loaded")

        exec1 = self._recorder1.get_execution()
        exec2 = self._recorder2.get_execution()

        steps1 = exec1.get("steps", [])
        steps2 = exec2.get("steps", [])

        # Comparar steps
        steps_diff = self._compare_steps(steps1, steps2)

        # Comparar arquivos
        files_diff = self._compare_files(exec1, exec2)

        # Calcular diferencas
        duration_diff = exec2.get("duration_ms", 0) - exec1.get("duration_ms", 0)
        tokens_diff = exec2.get("total_tokens", 0) - exec1.get("total_tokens", 0)

        # Gerar summary
        summary = self._generate_summary(exec1, exec2, steps_diff)

        return ComparisonResult(
            execution1_id=exec1.get("execution_id"),
            execution2_id=exec2.get("execution_id"),
            same_status=exec1.get("status") == exec2.get("status"),
            same_steps_count=len(steps1) == len(steps2),
            steps_diff=steps_diff,
            files_diff=files_diff,
            duration_diff_ms=duration_diff,
            tokens_diff=tokens_diff,
            summary=summary
        )

    def _compare_steps(self, steps1: List, steps2: List) -> List[Dict]:
        """Compara lista de steps"""
        diffs = []
        max_steps = max(len(steps1), len(steps2))

        for i in range(max_steps):
            step1 = steps1[i] if i < len(steps1) else None
            step2 = steps2[i] if i < len(steps2) else None

            if step1 is None:
                diffs.append({
                    "step_number": i + 1,
                    "type": "added_in_2",
                    "step2": step2
                })
            elif step2 is None:
                diffs.append({
                    "step_number": i + 1,
                    "type": "removed_in_2",
                    "step1": step1
                })
            else:
                # Comparar steps
                differences = {}

                if step1.get("name") != step2.get("name"):
                    differences["name"] = {
                        "exec1": step1.get("name"),
                        "exec2": step2.get("name")
                    }

                if step1.get("status") != step2.get("status"):
                    differences["status"] = {
                        "exec1": step1.get("status"),
                        "exec2": step2.get("status")
                    }

                if step1.get("error_message") != step2.get("error_message"):
                    differences["error"] = {
                        "exec1": step1.get("error_message"),
                        "exec2": step2.get("error_message")
                    }

                if differences:
                    diffs.append({
                        "step_number": i + 1,
                        "type": "modified",
                        "step_id": step1.get("step_id"),
                        "differences": differences
                    })

        return diffs

    def _compare_files(self, exec1: Dict, exec2: Dict) -> Dict:
        """Compara arquivos criados/modificados"""
        files1_created = set(exec1.get("files_created", []))
        files2_created = set(exec2.get("files_created", []))

        files1_modified = set(exec1.get("files_modified", []))
        files2_modified = set(exec2.get("files_modified", []))

        return {
            "created_only_in_1": list(files1_created - files2_created),
            "created_only_in_2": list(files2_created - files1_created),
            "created_in_both": list(files1_created & files2_created),
            "modified_only_in_1": list(files1_modified - files2_modified),
            "modified_only_in_2": list(files2_modified - files1_modified),
            "modified_in_both": list(files1_modified & files2_modified)
        }

    def _generate_summary(self, exec1: Dict, exec2: Dict, steps_diff: List) -> str:
        """Gera resumo da comparacao"""
        lines = []

        # Status
        status1 = exec1.get("status")
        status2 = exec2.get("status")

        if status1 == status2:
            lines.append(f"Ambas execucoes tem status '{status1}'")
        else:
            lines.append(f"Status diferente: Exec1={status1}, Exec2={status2}")

        # Steps
        steps1_count = len(exec1.get("steps", []))
        steps2_count = len(exec2.get("steps", []))

        if steps1_count != steps2_count:
            lines.append(f"Numero de steps diferente: {steps1_count} vs {steps2_count}")

        # Steps com falha
        failed_in_1 = exec1.get("steps_failed", 0)
        failed_in_2 = exec2.get("steps_failed", 0)

        if failed_in_1 != failed_in_2:
            lines.append(f"Falhas: Exec1 teve {failed_in_1}, Exec2 teve {failed_in_2}")

        # Primeiro step diferente
        for diff in steps_diff:
            if diff["type"] == "modified":
                differences = diff.get("differences", {})
                if "status" in differences:
                    step_num = diff["step_number"]
                    s1 = differences["status"]["exec1"]
                    s2 = differences["status"]["exec2"]
                    lines.append(f"Step {step_num} diferente: {s1} vs {s2}")
                    if differences.get("error"):
                        lines.append(f"  Erro em Exec2: {differences['error']['exec2']}")
                    break

        # Duracao
        duration_diff = exec2.get("duration_ms", 0) - exec1.get("duration_ms", 0)
        if abs(duration_diff) > 1000:  # Mais de 1 segundo de diferenca
            direction = "mais lenta" if duration_diff > 0 else "mais rapida"
            lines.append(f"Exec2 foi {abs(duration_diff)}ms {direction}")

        return "\n".join(lines)


# =============================================================================
# ANALYSIS HELPERS
# =============================================================================

def analyze_failures(execution_id: str) -> Dict[str, Any]:
    """
    Analisa falhas de uma execucao

    Args:
        execution_id: ID da execucao

    Returns:
        Analise das falhas
    """
    recorder = load_execution_from_db(execution_id)
    if not recorder:
        return {"error": "Execution not found"}

    failed_steps = recorder.get_failed_steps()

    analysis = {
        "execution_id": execution_id,
        "total_steps": len(recorder.steps),
        "failed_steps": len(failed_steps),
        "success_rate": (len(recorder.steps) - len(failed_steps)) / len(recorder.steps) * 100 if recorder.steps else 0,
        "failures": []
    }

    for step in failed_steps:
        failure = {
            "step_id": step.step_id,
            "step_type": step.step_type,
            "name": step.name,
            "error_type": step.error_type,
            "error_message": step.error_message,
            "attempt": step.attempt,
            "suggestions": []
        }

        # Sugestoes baseadas no tipo de erro
        if step.error_type:
            if "Timeout" in step.error_type:
                failure["suggestions"].append("Aumentar timeout da operacao")
            elif "Connection" in step.error_type:
                failure["suggestions"].append("Verificar conexao de rede")
            elif "Permission" in step.error_type:
                failure["suggestions"].append("Verificar permissoes de arquivo")
            elif "Syntax" in step.error_type:
                failure["suggestions"].append("Revisar codigo gerado")
            elif "Import" in step.error_type:
                failure["suggestions"].append("Verificar dependencias instaladas")

        # Sugestoes baseadas no step type
        if step.step_type == "linting":
            failure["suggestions"].append("Revisar regras do linter")
        elif step.step_type == "testing":
            failure["suggestions"].append("Verificar se testes estao corretos")
        elif step.step_type == "type_checking":
            failure["suggestions"].append("Adicionar type hints corretos")

        analysis["failures"].append(failure)

    return analysis


def get_execution_patterns(project_id: str, limit: int = 100) -> Dict[str, Any]:
    """
    Analisa padroes de execucao de um projeto

    Args:
        project_id: ID do projeto
        limit: Limite de execucoes a analisar

    Returns:
        Padroes identificados
    """
    from factory.database.connection import SessionLocal
    from factory.database.models import ExecutionLog

    db = SessionLocal()
    try:
        executions = db.query(ExecutionLog).filter(
            ExecutionLog.project_id == project_id
        ).order_by(ExecutionLog.created_at.desc()).limit(limit).all()

        if not executions:
            return {"error": "No executions found"}

        patterns = {
            "total_executions": len(executions),
            "success_count": 0,
            "failed_count": 0,
            "avg_duration_ms": 0,
            "avg_tokens": 0,
            "step_failure_frequency": {},
            "common_errors": {},
            "best_model": None,
            "fastest_execution": None,
            "slowest_execution": None
        }

        total_duration = 0
        total_tokens = 0
        model_success = {}

        for exec in executions:
            if exec.status == "success":
                patterns["success_count"] += 1
                if exec.agent_model:
                    model_success[exec.agent_model] = model_success.get(exec.agent_model, 0) + 1
            else:
                patterns["failed_count"] += 1

            total_duration += exec.duration_ms or 0
            total_tokens += exec.total_tokens or 0

            # Fastest/Slowest
            if exec.status == "success":
                if patterns["fastest_execution"] is None or exec.duration_ms < patterns["fastest_execution"]["duration_ms"]:
                    patterns["fastest_execution"] = {
                        "execution_id": exec.execution_id,
                        "duration_ms": exec.duration_ms
                    }
                if patterns["slowest_execution"] is None or exec.duration_ms > patterns["slowest_execution"]["duration_ms"]:
                    patterns["slowest_execution"] = {
                        "execution_id": exec.execution_id,
                        "duration_ms": exec.duration_ms
                    }

            # Analisar steps que falharam
            for step in (exec.steps or []):
                if step.get("status") == "failed":
                    step_type = step.get("step_type", "unknown")
                    patterns["step_failure_frequency"][step_type] = patterns["step_failure_frequency"].get(step_type, 0) + 1

                    error_type = step.get("error_type")
                    if error_type:
                        patterns["common_errors"][error_type] = patterns["common_errors"].get(error_type, 0) + 1

        if executions:
            patterns["avg_duration_ms"] = total_duration / len(executions)
            patterns["avg_tokens"] = total_tokens / len(executions)

        if model_success:
            patterns["best_model"] = max(model_success, key=model_success.get)

        # Ordenar por frequencia
        patterns["step_failure_frequency"] = dict(
            sorted(patterns["step_failure_frequency"].items(), key=lambda x: x[1], reverse=True)
        )
        patterns["common_errors"] = dict(
            sorted(patterns["common_errors"].items(), key=lambda x: x[1], reverse=True)[:10]
        )

        patterns["success_rate"] = (patterns["success_count"] / len(executions)) * 100 if executions else 0

        return patterns

    finally:
        db.close()


def export_execution_report(execution_id: str, format: str = "json") -> str:
    """
    Exporta relatorio de execucao

    Args:
        execution_id: ID da execucao
        format: Formato (json, text)

    Returns:
        Relatorio no formato especificado
    """
    recorder = load_execution_from_db(execution_id)
    if not recorder:
        return json.dumps({"error": "Execution not found"})

    data = recorder.get_execution()

    if format == "json":
        return json.dumps(data, indent=2, ensure_ascii=False)

    # Formato texto
    lines = [
        "=" * 60,
        f"EXECUTION REPORT: {data['execution_id']}",
        "=" * 60,
        "",
        f"Status: {data['status']}",
        f"Started: {data['started_at']}",
        f"Ended: {data['ended_at']}",
        f"Duration: {data['duration_ms']}ms",
        f"Total Tokens: {data['total_tokens']}",
        "",
        "STEPS:",
        "-" * 40
    ]

    for step in data.get("steps", []):
        status_icon = "[OK]" if step["status"] == "success" else "[FAIL]"
        lines.append(f"  {step['step_number']}. {status_icon} {step['name']} ({step['duration_ms']}ms)")
        if step.get("error_message"):
            lines.append(f"      Error: {step['error_message']}")

    lines.extend([
        "",
        "FILES CREATED:",
        "-" * 40
    ])
    for f in data.get("files_created", []):
        lines.append(f"  + {f}")

    lines.extend([
        "",
        "FILES MODIFIED:",
        "-" * 40
    ])
    for f in data.get("files_modified", []):
        lines.append(f"  ~ {f}")

    if data.get("error_message"):
        lines.extend([
            "",
            "ERROR:",
            "-" * 40,
            f"  Type: {data.get('error_type')}",
            f"  Message: {data.get('error_message')}"
        ])

    lines.append("=" * 60)

    return "\n".join(lines)

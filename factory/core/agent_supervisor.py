"""
Agent Supervisor - Sistema de Monitoramento e Controle de Agentes IA
Plataforma E v6.5

Funcionalidades:
- Monitoramento de agentes em execucao
- Deteccao de agentes parados/perdidos
- Forcamento de re-execucao
- Recuperacao de contexto
- Relatorio de status em tempo real
"""
import asyncio
import json
import os
import sqlite3
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Callable, Any
from dataclasses import dataclass, field, asdict
from enum import Enum
from uuid import uuid4
import threading
import time


class AgentStatus(str, Enum):
    """Status do agente"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    TIMEOUT = "timeout"
    LOST_CONTEXT = "lost_context"
    RECOVERED = "recovered"
    CANCELLED = "cancelled"


@dataclass
class AgentTask:
    """Tarefa atribuida a um agente"""
    task_id: str
    agent_id: str
    description: str
    prompt: str
    status: AgentStatus = AgentStatus.PENDING
    priority: int = 5  # 1-10 (1=mais alta)
    issue_id: Optional[str] = None
    github_issue: Optional[int] = None
    files_modified: List[str] = field(default_factory=list)
    output: str = ""
    error_message: str = ""
    started_at: Optional[str] = None
    completed_at: Optional[str] = None
    last_heartbeat: Optional[str] = None
    timeout_seconds: int = 600  # 10 minutos
    retries: int = 0
    max_retries: int = 3
    context_checkpoints: List[Dict] = field(default_factory=list)
    metadata: Dict = field(default_factory=dict)

    def to_dict(self) -> dict:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict) -> 'AgentTask':
        data['status'] = AgentStatus(data.get('status', 'pending'))
        return cls(**data)


@dataclass
class AgentMetrics:
    """Metricas do agente"""
    agent_id: str
    tasks_completed: int = 0
    tasks_failed: int = 0
    total_time_seconds: int = 0
    avg_task_duration: float = 0.0
    last_active: Optional[str] = None
    success_rate: float = 0.0


class AgentSupervisor:
    """
    Supervisor de Agentes IA

    Monitora, controla e recupera agentes que:
    - Estao travados/parados
    - Perderam contexto
    - Excederam timeout
    - Falharam e precisam de retry
    - Precisam de compactacao de contexto
    """

    def __init__(self, db_path: str = "factory/database/factory.db"):
        self.db_path = db_path
        self.agents: Dict[str, AgentTask] = {}
        self.metrics: Dict[str, AgentMetrics] = {}
        self.callbacks: Dict[str, List[Callable]] = {
            "task_started": [],
            "task_completed": [],
            "task_failed": [],
            "task_timeout": [],
            "task_lost_context": [],
            "task_recovered": [],
            "context_low": [],
            "auto_compact": [],
            "agent_pool_update": []
        }
        self._monitor_task: Optional[asyncio.Task] = None
        self._running = False
        self._lock = threading.Lock()

        # Configuracoes
        self.heartbeat_interval = 30  # segundos
        self.timeout_check_interval = 15  # segundos
        self.max_concurrent_agents = 32

        # Controle de contexto
        self.context_warning_threshold = 0.3  # 30% restante = warning
        self.context_critical_threshold = 0.15  # 15% restante = critico
        self.auto_compact_enabled = True
        self.max_agents_on_low_context = 4  # Reduzir agentes quando contexto baixo

        self._init_db()

    def _init_db(self):
        """Inicializa tabela de agentes no banco"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("""
            CREATE TABLE IF NOT EXISTS agent_tasks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                task_id TEXT UNIQUE NOT NULL,
                agent_id TEXT NOT NULL,
                description TEXT,
                prompt TEXT,
                status TEXT DEFAULT 'pending',
                priority INTEGER DEFAULT 5,
                issue_id TEXT,
                github_issue INTEGER,
                files_modified TEXT DEFAULT '[]',
                output TEXT DEFAULT '',
                error_message TEXT DEFAULT '',
                started_at TEXT,
                completed_at TEXT,
                last_heartbeat TEXT,
                timeout_seconds INTEGER DEFAULT 600,
                retries INTEGER DEFAULT 0,
                max_retries INTEGER DEFAULT 3,
                context_checkpoints TEXT DEFAULT '[]',
                metadata TEXT DEFAULT '{}',
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """)

        cursor.execute("""
            CREATE TABLE IF NOT EXISTS agent_metrics (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                agent_id TEXT UNIQUE NOT NULL,
                tasks_completed INTEGER DEFAULT 0,
                tasks_failed INTEGER DEFAULT 0,
                total_time_seconds INTEGER DEFAULT 0,
                avg_task_duration REAL DEFAULT 0.0,
                last_active TEXT,
                success_rate REAL DEFAULT 0.0,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """)

        # Indice para busca rapida
        cursor.execute("""
            CREATE INDEX IF NOT EXISTS idx_agent_tasks_status
            ON agent_tasks(status)
        """)
        cursor.execute("""
            CREATE INDEX IF NOT EXISTS idx_agent_tasks_agent_id
            ON agent_tasks(agent_id)
        """)

        conn.commit()
        conn.close()

    # =========================================================================
    # TASK MANAGEMENT
    # =========================================================================

    def create_task(
        self,
        description: str,
        prompt: str,
        priority: int = 5,
        issue_id: str = None,
        github_issue: int = None,
        timeout_seconds: int = 600,
        metadata: dict = None
    ) -> AgentTask:
        """
        Cria nova tarefa para ser atribuida a um agente

        Args:
            description: Descricao curta da tarefa
            prompt: Prompt completo para o agente
            priority: Prioridade 1-10 (1=mais alta)
            issue_id: ID da issue interna
            github_issue: Numero da issue no GitHub
            timeout_seconds: Timeout em segundos
            metadata: Metadados adicionais

        Returns:
            AgentTask criada
        """
        task_id = f"TASK-{datetime.utcnow().strftime('%Y%m%d%H%M%S')}-{uuid4().hex[:6].upper()}"
        agent_id = f"AGENT-{uuid4().hex[:8].upper()}"

        task = AgentTask(
            task_id=task_id,
            agent_id=agent_id,
            description=description,
            prompt=prompt,
            priority=priority,
            issue_id=issue_id,
            github_issue=github_issue,
            timeout_seconds=timeout_seconds,
            metadata=metadata or {}
        )

        # Salvar no banco
        self._save_task(task)

        # Adicionar ao pool
        with self._lock:
            self.agents[task_id] = task

        return task

    def _save_task(self, task: AgentTask):
        """Salva tarefa no banco"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("""
            INSERT OR REPLACE INTO agent_tasks (
                task_id, agent_id, description, prompt, status, priority,
                issue_id, github_issue, files_modified, output, error_message,
                started_at, completed_at, last_heartbeat, timeout_seconds,
                retries, max_retries, context_checkpoints, metadata, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            task.task_id, task.agent_id, task.description, task.prompt,
            task.status.value, task.priority, task.issue_id, task.github_issue,
            json.dumps(task.files_modified), task.output, task.error_message,
            task.started_at, task.completed_at, task.last_heartbeat,
            task.timeout_seconds, task.retries, task.max_retries,
            json.dumps(task.context_checkpoints), json.dumps(task.metadata),
            datetime.utcnow().isoformat()
        ))

        conn.commit()
        conn.close()

    def get_task(self, task_id: str) -> Optional[AgentTask]:
        """Busca tarefa por ID"""
        with self._lock:
            if task_id in self.agents:
                return self.agents[task_id]

        # Buscar no banco
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("SELECT * FROM agent_tasks WHERE task_id = ?", (task_id,))
        row = cursor.fetchone()
        conn.close()

        if row:
            return self._row_to_task(row)
        return None

    def _row_to_task(self, row) -> AgentTask:
        """Converte row do banco para AgentTask"""
        return AgentTask(
            task_id=row[1],
            agent_id=row[2],
            description=row[3],
            prompt=row[4],
            status=AgentStatus(row[5]),
            priority=row[6],
            issue_id=row[7],
            github_issue=row[8],
            files_modified=json.loads(row[9] or '[]'),
            output=row[10] or '',
            error_message=row[11] or '',
            started_at=row[12],
            completed_at=row[13],
            last_heartbeat=row[14],
            timeout_seconds=row[15],
            retries=row[16],
            max_retries=row[17],
            context_checkpoints=json.loads(row[18] or '[]'),
            metadata=json.loads(row[19] or '{}')
        )

    # =========================================================================
    # AGENT CONTROL
    # =========================================================================

    def start_task(self, task_id: str) -> bool:
        """Marca tarefa como iniciada"""
        task = self.get_task(task_id)
        if not task:
            return False

        task.status = AgentStatus.RUNNING
        task.started_at = datetime.utcnow().isoformat()
        task.last_heartbeat = datetime.utcnow().isoformat()

        self._save_task(task)
        with self._lock:
            self.agents[task_id] = task

        self._trigger("task_started", task)
        return True

    def heartbeat(self, task_id: str, checkpoint: dict = None) -> bool:
        """
        Atualiza heartbeat do agente e salva checkpoint de contexto

        Args:
            task_id: ID da tarefa
            checkpoint: Estado atual do contexto (opcional)
        """
        task = self.get_task(task_id)
        if not task:
            return False

        task.last_heartbeat = datetime.utcnow().isoformat()

        if checkpoint:
            task.context_checkpoints.append({
                "timestamp": datetime.utcnow().isoformat(),
                "checkpoint": checkpoint
            })
            # Manter apenas ultimos 5 checkpoints
            task.context_checkpoints = task.context_checkpoints[-5:]

        self._save_task(task)
        with self._lock:
            self.agents[task_id] = task

        return True

    def complete_task(
        self,
        task_id: str,
        output: str = "",
        files_modified: List[str] = None
    ) -> bool:
        """Marca tarefa como concluida"""
        task = self.get_task(task_id)
        if not task:
            return False

        task.status = AgentStatus.COMPLETED
        task.completed_at = datetime.utcnow().isoformat()
        task.output = output
        task.files_modified = files_modified or []

        self._save_task(task)
        self._update_metrics(task, success=True)

        with self._lock:
            if task_id in self.agents:
                del self.agents[task_id]

        self._trigger("task_completed", task)
        return True

    def fail_task(self, task_id: str, error: str = "") -> bool:
        """Marca tarefa como falha"""
        task = self.get_task(task_id)
        if not task:
            return False

        task.status = AgentStatus.FAILED
        task.completed_at = datetime.utcnow().isoformat()
        task.error_message = error

        self._save_task(task)
        self._update_metrics(task, success=False)

        with self._lock:
            if task_id in self.agents:
                del self.agents[task_id]

        self._trigger("task_failed", task)
        return True

    def mark_lost_context(self, task_id: str) -> bool:
        """Marca agente como tendo perdido contexto"""
        task = self.get_task(task_id)
        if not task:
            return False

        task.status = AgentStatus.LOST_CONTEXT
        task.error_message = "Agente perdeu contexto - necessita re-execucao"

        self._save_task(task)

        self._trigger("task_lost_context", task)
        return True

    # =========================================================================
    # RECOVERY & RETRY
    # =========================================================================

    def retry_task(self, task_id: str) -> Optional[AgentTask]:
        """
        Tenta re-executar tarefa falhada/timeout

        Returns:
            Nova tarefa criada ou None se max_retries atingido
        """
        task = self.get_task(task_id)
        if not task:
            return None

        if task.retries >= task.max_retries:
            print(f"[Supervisor] Task {task_id} atingiu max retries ({task.max_retries})")
            return None

        # Criar nova tarefa baseada na original
        new_task = self.create_task(
            description=f"[RETRY {task.retries + 1}] {task.description}",
            prompt=self._build_retry_prompt(task),
            priority=max(1, task.priority - 1),  # Aumentar prioridade
            issue_id=task.issue_id,
            github_issue=task.github_issue,
            timeout_seconds=task.timeout_seconds,
            metadata={
                **task.metadata,
                "original_task_id": task_id,
                "retry_number": task.retries + 1
            }
        )

        # Atualizar contador de retries na tarefa original
        task.retries += 1
        task.status = AgentStatus.RECOVERED
        self._save_task(task)

        self._trigger("task_recovered", new_task)
        return new_task

    def _build_retry_prompt(self, task: AgentTask) -> str:
        """Constroi prompt de retry com contexto anterior"""
        prompt = task.prompt

        # Adicionar contexto dos checkpoints
        if task.context_checkpoints:
            last_checkpoint = task.context_checkpoints[-1]
            prompt = f"""
IMPORTANTE: Esta e uma re-execucao de tarefa que falhou/timeout.

CONTEXTO ANTERIOR:
{json.dumps(last_checkpoint, indent=2)}

ERRO ANTERIOR:
{task.error_message}

CONTINUE DE ONDE PAROU:

{task.prompt}
"""

        return prompt

    def force_restart(self, task_id: str) -> Optional[AgentTask]:
        """Forca reinicio imediato de uma tarefa"""
        task = self.get_task(task_id)
        if not task:
            return None

        # Cancelar tarefa atual
        task.status = AgentStatus.CANCELLED
        self._save_task(task)

        # Remover do pool
        with self._lock:
            if task_id in self.agents:
                del self.agents[task_id]

        # Criar nova execucao
        new_task = self.create_task(
            description=f"[FORCED] {task.description}",
            prompt=task.prompt,
            priority=1,  # Maxima prioridade
            issue_id=task.issue_id,
            github_issue=task.github_issue,
            timeout_seconds=task.timeout_seconds,
            metadata={
                **task.metadata,
                "forced_from": task_id
            }
        )

        return new_task

    # =========================================================================
    # MONITORING
    # =========================================================================

    async def start_monitoring(self):
        """Inicia monitoramento de agentes"""
        if self._running:
            return

        self._running = True
        self._monitor_task = asyncio.create_task(self._monitor_loop())
        print("[Supervisor] Monitoramento iniciado")

    async def stop_monitoring(self):
        """Para monitoramento"""
        self._running = False
        if self._monitor_task:
            self._monitor_task.cancel()
            try:
                await self._monitor_task
            except asyncio.CancelledError:
                pass
        print("[Supervisor] Monitoramento parado")

    async def _monitor_loop(self):
        """Loop de monitoramento de agentes"""
        while self._running:
            try:
                await self._check_timeouts()
                await self._check_heartbeats()
                await asyncio.sleep(self.timeout_check_interval)
            except Exception as e:
                print(f"[Supervisor] Erro no monitor: {e}")
                await asyncio.sleep(5)

    async def _check_timeouts(self):
        """Verifica agentes que excederam timeout"""
        now = datetime.utcnow()

        with self._lock:
            for task_id, task in list(self.agents.items()):
                if task.status != AgentStatus.RUNNING:
                    continue

                if task.started_at:
                    started = datetime.fromisoformat(task.started_at)
                    elapsed = (now - started).total_seconds()

                    if elapsed > task.timeout_seconds:
                        print(f"[Supervisor] TIMEOUT detectado: {task_id}")
                        task.status = AgentStatus.TIMEOUT
                        task.error_message = f"Timeout apos {elapsed:.0f}s"
                        self._save_task(task)
                        self._trigger("task_timeout", task)

                        # Auto-retry se possivel
                        if task.retries < task.max_retries:
                            new_task = self.retry_task(task_id)
                            if new_task:
                                print(f"[Supervisor] Auto-retry iniciado: {new_task.task_id}")

    async def _check_heartbeats(self):
        """Verifica agentes que pararam de enviar heartbeat"""
        now = datetime.utcnow()
        stale_threshold = timedelta(seconds=self.heartbeat_interval * 3)

        with self._lock:
            for task_id, task in list(self.agents.items()):
                if task.status != AgentStatus.RUNNING:
                    continue

                if task.last_heartbeat:
                    last_hb = datetime.fromisoformat(task.last_heartbeat)

                    if now - last_hb > stale_threshold:
                        print(f"[Supervisor] Heartbeat parado: {task_id}")
                        self.mark_lost_context(task_id)

                        # Auto-retry
                        if task.retries < task.max_retries:
                            new_task = self.retry_task(task_id)
                            if new_task:
                                print(f"[Supervisor] Recuperacao iniciada: {new_task.task_id}")

    # =========================================================================
    # CONTEXT MANAGEMENT
    # =========================================================================

    def report_context_level(self, task_id: str, context_remaining: float) -> Dict:
        """
        Recebe report de nivel de contexto de um agente

        Args:
            task_id: ID da tarefa
            context_remaining: Percentual de contexto restante (0.0 a 1.0)

        Returns:
            Instrucoes para o agente (compact, pause, continue)
        """
        task = self.get_task(task_id)
        if not task:
            return {"action": "continue"}

        # Salvar checkpoint antes de qualquer acao
        self.heartbeat(task_id, {
            "context_level": context_remaining,
            "timestamp": datetime.utcnow().isoformat()
        })

        # Contexto critico - pausar e compactar
        if context_remaining <= self.context_critical_threshold:
            print(f"[Supervisor] CONTEXTO CRITICO em {task_id}: {context_remaining*100:.0f}%")
            self._trigger("context_low", {"task_id": task_id, "level": "critical"})

            # Salvar estado completo para recuperacao
            self._save_full_state(task)

            if self.auto_compact_enabled:
                self._trigger("auto_compact", task)
                return {
                    "action": "compact_and_continue",
                    "save_state": True,
                    "message": "Execute /compact e continue de onde parou"
                }
            else:
                return {
                    "action": "pause",
                    "save_state": True,
                    "message": "Contexto critico - tarefa pausada"
                }

        # Contexto baixo - warning
        elif context_remaining <= self.context_warning_threshold:
            print(f"[Supervisor] Contexto baixo em {task_id}: {context_remaining*100:.0f}%")
            self._trigger("context_low", {"task_id": task_id, "level": "warning"})

            return {
                "action": "continue_with_caution",
                "save_checkpoint": True,
                "message": f"Contexto em {context_remaining*100:.0f}% - salvando checkpoints frequentes"
            }

        return {"action": "continue"}

    def _save_full_state(self, task: AgentTask):
        """Salva estado completo da tarefa para recuperacao"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        # Criar tabela de estados se nao existir
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS agent_states (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                task_id TEXT NOT NULL,
                state_data TEXT NOT NULL,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """)

        # Salvar estado
        state_data = {
            "task": task.to_dict(),
            "timestamp": datetime.utcnow().isoformat(),
            "checkpoints": task.context_checkpoints
        }

        cursor.execute(
            "INSERT INTO agent_states (task_id, state_data) VALUES (?, ?)",
            (task.task_id, json.dumps(state_data))
        )

        conn.commit()
        conn.close()
        print(f"[Supervisor] Estado salvo para {task.task_id}")

    def recover_state(self, task_id: str) -> Optional[Dict]:
        """Recupera ultimo estado salvo de uma tarefa"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("""
            SELECT state_data FROM agent_states
            WHERE task_id = ?
            ORDER BY created_at DESC
            LIMIT 1
        """, (task_id,))

        row = cursor.fetchone()
        conn.close()

        if row:
            return json.loads(row[0])
        return None

    def get_resume_prompt(self, task_id: str) -> Optional[str]:
        """
        Gera prompt de resumo para agente que perdeu contexto

        Inclui:
        - Descricao original da tarefa
        - Ultimo checkpoint
        - Arquivos ja modificados
        - Progresso ate o momento
        """
        state = self.recover_state(task_id)
        if not state:
            return None

        task_data = state.get("task", {})
        checkpoints = state.get("checkpoints", [])

        last_checkpoint = checkpoints[-1] if checkpoints else {}

        prompt = f"""
CONTINUANDO TAREFA INTERROMPIDA
==============================

TAREFA ORIGINAL: {task_data.get('description', 'N/A')}

PROGRESSO ANTERIOR:
- Status: {task_data.get('status', 'N/A')}
- Arquivos modificados: {', '.join(task_data.get('files_modified', [])) or 'Nenhum ainda'}
- Retries: {task_data.get('retries', 0)}/{task_data.get('max_retries', 3)}

ULTIMO CHECKPOINT:
{json.dumps(last_checkpoint, indent=2)}

INSTRUCOES:
1. Revise o checkpoint acima para entender onde parou
2. Continue de onde parou, nao refaca trabalho ja feito
3. Envie heartbeats frequentes para evitar nova perda
4. Ao terminar, marque como completed

PROMPT ORIGINAL:
{task_data.get('prompt', 'N/A')}
"""
        return prompt

    def adjust_concurrency_for_context(self, context_level: float) -> int:
        """
        Ajusta numero maximo de agentes baseado no contexto disponivel

        Args:
            context_level: Nivel de contexto (0.0 a 1.0)

        Returns:
            Novo limite de agentes concorrentes
        """
        if context_level <= self.context_critical_threshold:
            new_limit = 2
        elif context_level <= self.context_warning_threshold:
            new_limit = self.max_agents_on_low_context
        else:
            new_limit = self.max_concurrent_agents

        if new_limit != self.max_concurrent_agents:
            print(f"[Supervisor] Ajustando concorrencia: {self.max_concurrent_agents} -> {new_limit}")
            self._trigger("agent_pool_update", {"new_limit": new_limit, "reason": "context_level"})

        return new_limit

    def pause_lowest_priority_agents(self, count: int = 1) -> List[str]:
        """
        Pausa agentes de menor prioridade para liberar contexto

        Returns:
            Lista de task_ids pausados
        """
        running = self.get_running_tasks()
        if not running:
            return []

        # Ordenar por prioridade (maior numero = menor prioridade)
        sorted_tasks = sorted(running, key=lambda x: x.get("priority", 5), reverse=True)

        paused = []
        for task in sorted_tasks[:count]:
            task_id = task["task_id"]
            self.mark_lost_context(task_id)  # Salva estado automaticamente
            paused.append(task_id)
            print(f"[Supervisor] Pausando {task_id} para liberar contexto")

        return paused

    # =========================================================================
    # STATUS & REPORTING
    # =========================================================================

    def get_status(self) -> Dict:
        """Retorna status geral do supervisor"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        # Contar por status
        cursor.execute("""
            SELECT status, COUNT(*)
            FROM agent_tasks
            GROUP BY status
        """)
        status_counts = dict(cursor.fetchall())

        # Tarefas ativas
        cursor.execute("""
            SELECT COUNT(*) FROM agent_tasks
            WHERE status IN ('running', 'pending')
        """)
        active = cursor.fetchone()[0]

        conn.close()

        return {
            "timestamp": datetime.utcnow().isoformat(),
            "active_agents": len(self.agents),
            "max_concurrent": self.max_concurrent_agents,
            "status_counts": {
                "pending": status_counts.get("pending", 0),
                "running": status_counts.get("running", 0),
                "completed": status_counts.get("completed", 0),
                "failed": status_counts.get("failed", 0),
                "timeout": status_counts.get("timeout", 0),
                "lost_context": status_counts.get("lost_context", 0),
                "recovered": status_counts.get("recovered", 0),
                "cancelled": status_counts.get("cancelled", 0)
            },
            "total_tasks": sum(status_counts.values()),
            "monitoring_active": self._running
        }

    def get_running_tasks(self) -> List[Dict]:
        """Lista tarefas em execucao"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("""
            SELECT * FROM agent_tasks
            WHERE status = 'running'
            ORDER BY priority, started_at
        """)
        rows = cursor.fetchall()
        conn.close()

        tasks = []
        for row in rows:
            task = self._row_to_task(row)
            # Calcular tempo decorrido
            if task.started_at:
                started = datetime.fromisoformat(task.started_at)
                elapsed = (datetime.utcnow() - started).total_seconds()
                tasks.append({
                    **task.to_dict(),
                    "elapsed_seconds": elapsed,
                    "timeout_remaining": task.timeout_seconds - elapsed,
                    "health": self._calculate_health(task, elapsed)
                })

        return tasks

    def _calculate_health(self, task: AgentTask, elapsed: float) -> str:
        """Calcula saude do agente"""
        # Verificar heartbeat
        if task.last_heartbeat:
            last_hb = datetime.fromisoformat(task.last_heartbeat)
            hb_age = (datetime.utcnow() - last_hb).total_seconds()

            if hb_age > self.heartbeat_interval * 3:
                return "critical"  # Sem heartbeat
            elif hb_age > self.heartbeat_interval * 2:
                return "warning"  # Heartbeat lento

        # Verificar timeout
        if elapsed > task.timeout_seconds * 0.9:
            return "warning"  # Proximo do timeout
        elif elapsed > task.timeout_seconds * 0.7:
            return "caution"

        return "healthy"

    def get_failed_tasks(self, include_recovered: bool = False) -> List[Dict]:
        """Lista tarefas que falharam"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        statuses = "'failed', 'timeout', 'lost_context'"
        if include_recovered:
            statuses += ", 'recovered'"

        cursor.execute(f"""
            SELECT * FROM agent_tasks
            WHERE status IN ({statuses})
            ORDER BY completed_at DESC
            LIMIT 50
        """)
        rows = cursor.fetchall()
        conn.close()

        return [self._row_to_task(row).to_dict() for row in rows]

    def get_task_history(self, limit: int = 100) -> List[Dict]:
        """Historico de todas as tarefas"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("""
            SELECT * FROM agent_tasks
            ORDER BY created_at DESC
            LIMIT ?
        """, (limit,))
        rows = cursor.fetchall()
        conn.close()

        return [self._row_to_task(row).to_dict() for row in rows]

    # =========================================================================
    # METRICS
    # =========================================================================

    def _update_metrics(self, task: AgentTask, success: bool):
        """Atualiza metricas do agente"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        # Buscar metricas existentes
        cursor.execute(
            "SELECT * FROM agent_metrics WHERE agent_id = ?",
            (task.agent_id,)
        )
        row = cursor.fetchone()

        # Calcular duracao
        duration = 0
        if task.started_at and task.completed_at:
            start = datetime.fromisoformat(task.started_at)
            end = datetime.fromisoformat(task.completed_at)
            duration = int((end - start).total_seconds())

        if row:
            # Atualizar existente
            completed = row[2] + (1 if success else 0)
            failed = row[3] + (0 if success else 1)
            total_time = row[4] + duration
            total_tasks = completed + failed
            avg_duration = total_time / total_tasks if total_tasks > 0 else 0
            success_rate = completed / total_tasks if total_tasks > 0 else 0

            cursor.execute("""
                UPDATE agent_metrics SET
                    tasks_completed = ?,
                    tasks_failed = ?,
                    total_time_seconds = ?,
                    avg_task_duration = ?,
                    success_rate = ?,
                    last_active = ?,
                    updated_at = ?
                WHERE agent_id = ?
            """, (
                completed, failed, total_time, avg_duration,
                success_rate, datetime.utcnow().isoformat(),
                datetime.utcnow().isoformat(), task.agent_id
            ))
        else:
            # Criar novo
            cursor.execute("""
                INSERT INTO agent_metrics (
                    agent_id, tasks_completed, tasks_failed,
                    total_time_seconds, avg_task_duration,
                    success_rate, last_active
                ) VALUES (?, ?, ?, ?, ?, ?, ?)
            """, (
                task.agent_id,
                1 if success else 0,
                0 if success else 1,
                duration,
                float(duration),
                1.0 if success else 0.0,
                datetime.utcnow().isoformat()
            ))

        conn.commit()
        conn.close()

    def get_metrics(self) -> Dict:
        """Retorna metricas agregadas"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("""
            SELECT
                SUM(tasks_completed) as completed,
                SUM(tasks_failed) as failed,
                AVG(avg_task_duration) as avg_duration,
                AVG(success_rate) as avg_success
            FROM agent_metrics
        """)
        row = cursor.fetchone()
        conn.close()

        completed = row[0] or 0
        failed = row[1] or 0
        total = completed + failed

        return {
            "total_tasks": total,
            "completed": completed,
            "failed": failed,
            "success_rate": (completed / total * 100) if total > 0 else 0,
            "avg_duration_seconds": row[2] or 0,
            "overall_success_rate": (row[3] or 0) * 100
        }

    # =========================================================================
    # CALLBACKS
    # =========================================================================

    def on(self, event: str, callback: Callable):
        """Registra callback para evento"""
        if event in self.callbacks:
            self.callbacks[event].append(callback)

    def _trigger(self, event: str, data: Any):
        """Dispara callbacks"""
        for callback in self.callbacks.get(event, []):
            try:
                if asyncio.iscoroutinefunction(callback):
                    asyncio.create_task(callback(data))
                else:
                    callback(data)
            except Exception as e:
                print(f"[Supervisor] Erro em callback {event}: {e}")


# =============================================================================
# BATCH OPERATIONS
# =============================================================================

class BatchAgentExecutor:
    """
    Executor em lote de agentes
    Gerencia multiplos agentes em paralelo com controle de concorrencia
    """

    def __init__(self, supervisor: AgentSupervisor, max_concurrent: int = 8):
        self.supervisor = supervisor
        self.max_concurrent = max_concurrent
        self.semaphore = asyncio.Semaphore(max_concurrent)
        self.results: Dict[str, Any] = {}

    async def execute_batch(
        self,
        tasks: List[Dict],
        executor_func: Callable
    ) -> Dict[str, Any]:
        """
        Executa lote de tarefas em paralelo

        Args:
            tasks: Lista de dicts com {description, prompt, priority, ...}
            executor_func: Funcao async que executa cada tarefa

        Returns:
            Dict com resultados por task_id
        """
        # Criar todas as tarefas
        agent_tasks = []
        for task_data in tasks:
            task = self.supervisor.create_task(**task_data)
            agent_tasks.append(task)

        # Executar em paralelo com limite de concorrencia
        async def run_with_semaphore(task: AgentTask):
            async with self.semaphore:
                try:
                    self.supervisor.start_task(task.task_id)

                    result = await executor_func(task)

                    self.supervisor.complete_task(
                        task.task_id,
                        output=str(result),
                        files_modified=result.get('files', []) if isinstance(result, dict) else []
                    )

                    return task.task_id, {"success": True, "result": result}

                except Exception as e:
                    self.supervisor.fail_task(task.task_id, str(e))
                    return task.task_id, {"success": False, "error": str(e)}

        # Executar todos
        results = await asyncio.gather(
            *[run_with_semaphore(task) for task in agent_tasks]
        )

        self.results = dict(results)
        return self.results

    def get_summary(self) -> Dict:
        """Retorna resumo da execucao"""
        success = sum(1 for r in self.results.values() if r.get('success'))
        failed = len(self.results) - success

        return {
            "total": len(self.results),
            "success": success,
            "failed": failed,
            "success_rate": (success / len(self.results) * 100) if self.results else 0,
            "results": self.results
        }


# =============================================================================
# SINGLETON INSTANCE
# =============================================================================

_supervisor_instance: Optional[AgentSupervisor] = None


def get_supervisor() -> AgentSupervisor:
    """Retorna instancia singleton do supervisor"""
    global _supervisor_instance
    if _supervisor_instance is None:
        _supervisor_instance = AgentSupervisor()
    return _supervisor_instance


# =============================================================================
# CLI
# =============================================================================

if __name__ == "__main__":
    import sys

    supervisor = get_supervisor()

    if len(sys.argv) > 1:
        cmd = sys.argv[1]

        if cmd == "status":
            status = supervisor.get_status()
            print("\n=== SUPERVISOR STATUS ===")
            print(f"Active Agents: {status['active_agents']}/{status['max_concurrent']}")
            print(f"Monitoring: {'ON' if status['monitoring_active'] else 'OFF'}")
            print(f"\nStatus Counts:")
            for s, count in status['status_counts'].items():
                if count > 0:
                    print(f"  {s}: {count}")
            print(f"\nTotal Tasks: {status['total_tasks']}")

        elif cmd == "running":
            tasks = supervisor.get_running_tasks()
            print(f"\n=== RUNNING TASKS ({len(tasks)}) ===")
            for task in tasks:
                print(f"\n{task['task_id']}")
                print(f"  Description: {task['description'][:50]}...")
                print(f"  Elapsed: {task['elapsed_seconds']:.0f}s")
                print(f"  Health: {task['health']}")
                print(f"  Timeout in: {task['timeout_remaining']:.0f}s")

        elif cmd == "failed":
            tasks = supervisor.get_failed_tasks()
            print(f"\n=== FAILED TASKS ({len(tasks)}) ===")
            for task in tasks:
                print(f"\n{task['task_id']} - {task['status']}")
                print(f"  Description: {task['description'][:50]}...")
                print(f"  Error: {task['error_message'][:100]}...")
                print(f"  Retries: {task['retries']}/{task['max_retries']}")

        elif cmd == "retry" and len(sys.argv) > 2:
            task_id = sys.argv[2]
            new_task = supervisor.retry_task(task_id)
            if new_task:
                print(f"Retry criado: {new_task.task_id}")
            else:
                print(f"Nao foi possivel criar retry para {task_id}")

        elif cmd == "force" and len(sys.argv) > 2:
            task_id = sys.argv[2]
            new_task = supervisor.force_restart(task_id)
            if new_task:
                print(f"Force restart: {new_task.task_id}")
            else:
                print(f"Nao foi possivel forcar restart de {task_id}")

        elif cmd == "metrics":
            metrics = supervisor.get_metrics()
            print("\n=== METRICS ===")
            print(f"Total Tasks: {metrics['total_tasks']}")
            print(f"Completed: {metrics['completed']}")
            print(f"Failed: {metrics['failed']}")
            print(f"Success Rate: {metrics['success_rate']:.1f}%")
            print(f"Avg Duration: {metrics['avg_duration_seconds']:.0f}s")

        else:
            print("Usage: python agent_supervisor.py [status|running|failed|retry <id>|force <id>|metrics]")
    else:
        print("Usage: python agent_supervisor.py [status|running|failed|retry <id>|force <id>|metrics]")

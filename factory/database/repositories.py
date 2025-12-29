# -*- coding: utf-8 -*-
"""
Repositorios para acesso ao banco de dados - Fabrica de Agentes v4.0
Arquitetura MVP Worker-based (Jobs + Workers + Claude Agent SDK)
"""
from datetime import datetime, timedelta
from typing import List, Optional, Dict, Any
from sqlalchemy.orm import Session
from sqlalchemy import desc, and_, or_

from .models import (
    Project, User, ActivityLog,
    Job, JobStatus, JobStep, FailureHistory, Worker,
    ProjectStatus, Task, TaskStatus, TaskPriority,
    Story, StoryStatus, StoryCategory, StoryComplexity,
    StoryTask, StoryTaskType, StoryTaskStatus,
    StoryDocumentation, DocType,
    ChatMessage, MessageRole,
    Attachment, Epic, Sprint,
    ExecutionLog, ExecutionStatus
)


# =============================================================================
# PROJECT REPOSITORY
# =============================================================================

class ProjectRepository:
    """Repositorio para gerenciamento de Projetos"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, project_data: dict) -> Project:
        """Cria novo projeto"""
        # Gera project_id automaticamente se nao fornecido
        if "project_id" not in project_data:
            count = self.db.query(Project).count()
            project_data["project_id"] = f"PRJ-{count + 1:04d}"

        project = Project(**project_data)
        self.db.add(project)
        self.db.commit()
        self.db.refresh(project)
        return project

    def get_by_id(self, project_id: str) -> Optional[Project]:
        """Busca projeto por ID"""
        return self.db.query(Project).filter(Project.project_id == project_id).first()

    def get_all(self, status: str = None, project_type: str = None) -> List[Project]:
        """Lista todos os projetos com filtros opcionais"""
        query = self.db.query(Project)
        if status:
            query = query.filter(Project.status == status)
        if project_type:
            query = query.filter(Project.project_type == project_type)
        return query.order_by(desc(Project.updated_at)).all()

    def update(self, project_id: str, data: dict) -> Optional[Project]:
        """Atualiza projeto"""
        project = self.get_by_id(project_id)
        if project:
            for key, value in data.items():
                if hasattr(project, key):
                    setattr(project, key, value)
            project.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(project)
        return project

    def delete(self, project_id: str) -> bool:
        """Remove projeto"""
        project = self.get_by_id(project_id)
        if project:
            self.db.delete(project)
            self.db.commit()
            return True
        return False

    def count_by_status(self) -> Dict[str, int]:
        """Conta projetos por status"""
        result = {}
        for status in ProjectStatus:
            count = self.db.query(Project).filter(Project.status == status.value).count()
            result[status.value] = count
        return result


# =============================================================================
# JOB REPOSITORY
# =============================================================================

class JobRepository:
    """Repositorio para gerenciamento de Jobs - Unidade principal de trabalho"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, job_data: dict) -> Job:
        """Cria novo job"""
        # Gera job_id automaticamente se nao fornecido
        if "job_id" not in job_data:
            count = self.db.query(Job).count()
            job_data["job_id"] = f"JOB-{count + 1:04d}"

        job = Job(**job_data)
        self.db.add(job)
        self.db.commit()
        self.db.refresh(job)
        return job

    def get_by_id(self, job_id: str) -> Optional[Job]:
        """Busca job por ID"""
        return self.db.query(Job).filter(Job.job_id == job_id).first()

    def get_all(self, status: str = None, limit: int = 100) -> List[Job]:
        """Lista todos os jobs com filtros opcionais"""
        query = self.db.query(Job)
        if status:
            query = query.filter(Job.status == status)
        return query.order_by(desc(Job.created_at)).limit(limit).all()

    def get_pending(self) -> List[Job]:
        """Lista jobs pendentes na fila (FIFO)"""
        return self.db.query(Job).filter(
            Job.status.in_([JobStatus.PENDING.value, JobStatus.QUEUED.value])
        ).order_by(Job.queued_at).all()

    def get_next_pending(self) -> Optional[Job]:
        """Retorna proximo job pendente para processamento"""
        return self.db.query(Job).filter(
            Job.status == JobStatus.PENDING.value
        ).order_by(Job.queued_at).first()

    def get_running(self) -> List[Job]:
        """Lista jobs em execucao"""
        return self.db.query(Job).filter(
            Job.status == JobStatus.RUNNING.value
        ).all()

    def get_by_project(self, project_id: str) -> List[Job]:
        """Lista jobs de um projeto"""
        return self.db.query(Job).filter(
            Job.project_id == project_id
        ).order_by(desc(Job.created_at)).all()

    def get_by_worker(self, worker_id: str) -> List[Job]:
        """Lista jobs de um worker"""
        return self.db.query(Job).filter(
            Job.worker_id == worker_id
        ).order_by(desc(Job.created_at)).all()

    def update(self, job_id: str, data: dict) -> Optional[Job]:
        """Atualiza job"""
        job = self.get_by_id(job_id)
        if job:
            for key, value in data.items():
                if hasattr(job, key) and value is not None:
                    setattr(job, key, value)
            job.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(job)
        return job

    def update_status(self, job_id: str, status: str, step: str = None) -> Optional[Job]:
        """Atualiza status e step do job"""
        job = self.get_by_id(job_id)
        if job:
            job.status = status
            if step:
                job.current_step = step
            if status == JobStatus.RUNNING.value and not job.started_at:
                job.started_at = datetime.utcnow()
            elif status in [JobStatus.COMPLETED.value, JobStatus.FAILED.value]:
                job.completed_at = datetime.utcnow()
            job.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(job)
        return job

    def add_step_log(self, job_id: str, step: str, message: str, success: bool = True) -> Optional[Job]:
        """Adiciona log de um step"""
        job = self.get_by_id(job_id)
        if job:
            logs = job.step_logs or []
            logs.append({
                "step": step,
                "message": message,
                "success": success,
                "timestamp": datetime.utcnow().isoformat()
            })
            job.step_logs = logs
            self.db.commit()
        return job

    def increment_attempt(self, job_id: str) -> Optional[Job]:
        """Incrementa tentativa do loop"""
        job = self.get_by_id(job_id)
        if job:
            job.current_attempt += 1
            job.total_iterations += 1
            self.db.commit()
            self.db.refresh(job)
        return job

    def assign_worker(self, job_id: str, worker_id: str) -> Optional[Job]:
        """Atribui worker ao job"""
        job = self.get_by_id(job_id)
        if job:
            job.worker_id = worker_id
            job.status = JobStatus.RUNNING.value
            job.started_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(job)
        return job

    def count_by_status(self) -> Dict[str, int]:
        """Conta jobs por status"""
        result = {}
        for status in JobStatus:
            count = self.db.query(Job).filter(Job.status == status.value).count()
            result[status.value] = count
        return result

    def delete(self, job_id: str) -> bool:
        """Remove job"""
        job = self.get_by_id(job_id)
        if job:
            self.db.delete(job)
            self.db.commit()
            return True
        return False


# =============================================================================
# WORKER REPOSITORY
# =============================================================================

class WorkerRepository:
    """Repositorio para gerenciamento de Workers (Claude Agent SDK instances)"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, worker_data: dict) -> Worker:
        """Registra novo worker"""
        worker = Worker(**worker_data)
        self.db.add(worker)
        self.db.commit()
        self.db.refresh(worker)
        return worker

    def get_by_id(self, worker_id: str) -> Optional[Worker]:
        """Busca worker por ID"""
        return self.db.query(Worker).filter(Worker.worker_id == worker_id).first()

    def get_all(self) -> List[Worker]:
        """Lista todos os workers"""
        return self.db.query(Worker).order_by(Worker.started_at).all()

    def get_active(self) -> List[Worker]:
        """Lista workers ativos (com heartbeat recente)"""
        threshold = datetime.utcnow() - timedelta(minutes=5)
        return self.db.query(Worker).filter(
            Worker.status != "offline",
            Worker.last_heartbeat >= threshold
        ).all()

    def get_idle(self) -> List[Worker]:
        """Lista workers disponiveis"""
        return self.db.query(Worker).filter(
            Worker.status == "idle"
        ).all()

    def get_or_create(self, worker_id: str, **kwargs) -> Worker:
        """Busca ou cria worker"""
        worker = self.get_by_id(worker_id)
        if not worker:
            worker = Worker(worker_id=worker_id, **kwargs)
            self.db.add(worker)
            self.db.commit()
            self.db.refresh(worker)
        return worker

    def update_status(self, worker_id: str, status: str, job_id: str = None) -> Optional[Worker]:
        """Atualiza status do worker"""
        worker = self.get_by_id(worker_id)
        if worker:
            worker.status = status
            worker.current_job_id = job_id
            worker.last_heartbeat = datetime.utcnow()
            self.db.commit()
            self.db.refresh(worker)
        return worker

    def heartbeat(self, worker_id: str) -> Optional[Worker]:
        """Atualiza heartbeat do worker"""
        worker = self.get_by_id(worker_id)
        if worker:
            worker.last_heartbeat = datetime.utcnow()
            self.db.commit()
        return worker

    def increment_completed(self, worker_id: str, duration_seconds: int) -> Optional[Worker]:
        """Incrementa contador de jobs completados"""
        worker = self.get_by_id(worker_id)
        if worker:
            worker.jobs_completed += 1
            worker.total_processing_time += duration_seconds
            worker.avg_job_duration = worker.total_processing_time / worker.jobs_completed
            worker.status = "idle"
            worker.current_job_id = None
            self.db.commit()
            self.db.refresh(worker)
        return worker

    def increment_failed(self, worker_id: str) -> Optional[Worker]:
        """Incrementa contador de jobs falhados"""
        worker = self.get_by_id(worker_id)
        if worker:
            worker.jobs_failed += 1
            worker.status = "idle"
            worker.current_job_id = None
            self.db.commit()
            self.db.refresh(worker)
        return worker

    def mark_offline(self, worker_id: str) -> Optional[Worker]:
        """Marca worker como offline"""
        worker = self.get_by_id(worker_id)
        if worker:
            worker.status = "offline"
            worker.current_job_id = None
            self.db.commit()
            self.db.refresh(worker)
        return worker

    def cleanup_stale(self, timeout_minutes: int = 10) -> int:
        """Remove workers inativos"""
        threshold = datetime.utcnow() - timedelta(minutes=timeout_minutes)
        count = self.db.query(Worker).filter(
            Worker.last_heartbeat < threshold
        ).update({"status": "offline", "current_job_id": None})
        self.db.commit()
        return count


# =============================================================================
# FAILURE HISTORY REPOSITORY
# =============================================================================

class FailureHistoryRepository:
    """Repositorio para gerenciamento de Historico de Falhas"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, failure_data: dict) -> FailureHistory:
        """Registra nova falha"""
        failure = FailureHistory(**failure_data)
        self.db.add(failure)
        self.db.commit()
        self.db.refresh(failure)
        return failure

    def get_by_job(self, job_id: str) -> List[FailureHistory]:
        """Lista falhas de um job"""
        return self.db.query(FailureHistory).filter(
            FailureHistory.job_id == job_id
        ).order_by(desc(FailureHistory.created_at)).all()

    def get_by_step(self, job_id: str, step: str) -> List[FailureHistory]:
        """Lista falhas de um step especifico"""
        return self.db.query(FailureHistory).filter(
            FailureHistory.job_id == job_id,
            FailureHistory.step == step
        ).all()

    def count_failures(self, job_id: str, step: str) -> int:
        """Conta falhas de um step (para verificar max attempts)"""
        return self.db.query(FailureHistory).filter(
            FailureHistory.job_id == job_id,
            FailureHistory.step == step
        ).count()

    def has_similar_failure(self, job_id: str, error_type: str) -> bool:
        """Verifica se ja houve falha similar (prevenir retry loops)"""
        return self.db.query(FailureHistory).filter(
            FailureHistory.job_id == job_id,
            FailureHistory.error_type == error_type,
            FailureHistory.resolved == False
        ).count() > 2  # Se mais de 2 falhas iguais nao resolvidas, retorna True

    def get_recent(self, limit: int = 50) -> List[FailureHistory]:
        """Lista falhas recentes"""
        return self.db.query(FailureHistory).order_by(
            desc(FailureHistory.created_at)
        ).limit(limit).all()

    def mark_resolved(self, failure_id: int, notes: str = None) -> Optional[FailureHistory]:
        """Marca falha como resolvida"""
        failure = self.db.query(FailureHistory).filter(
            FailureHistory.id == failure_id
        ).first()
        if failure:
            failure.resolved = True
            failure.resolution_notes = notes
            failure.resolved_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(failure)
        return failure


# =============================================================================
# USER REPOSITORY
# =============================================================================

class UserRepository:
    """Repositorio para gerenciamento de Usuarios"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, user_data: dict) -> User:
        """Cria novo usuario"""
        user = User(**user_data)
        self.db.add(user)
        self.db.commit()
        self.db.refresh(user)
        return user

    def get_by_username(self, username: str) -> Optional[User]:
        """Busca usuario por username"""
        return self.db.query(User).filter(User.username == username).first()

    def get_all(self, active_only: bool = True) -> List[User]:
        """Lista todos os usuarios"""
        query = self.db.query(User)
        if active_only:
            query = query.filter(User.active == True)
        return query.all()

    def update_last_login(self, username: str) -> Optional[User]:
        """Atualiza ultimo login"""
        user = self.get_by_username(username)
        if user:
            user.last_login = datetime.utcnow()
            self.db.commit()
        return user


# =============================================================================
# ACTIVITY LOG REPOSITORY
# =============================================================================

class ActivityLogRepository:
    """Repositorio para gerenciamento de Logs de Atividades"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, log_data: dict) -> ActivityLog:
        """Cria novo log"""
        log = ActivityLog(**log_data)
        self.db.add(log)
        self.db.commit()
        self.db.refresh(log)
        return log

    def get_recent(self, limit: int = 100, project_id: str = None, job_id: str = None) -> List[ActivityLog]:
        """Lista logs recentes"""
        query = self.db.query(ActivityLog)
        if project_id:
            query = query.filter(ActivityLog.project_id == project_id)
        if job_id:
            query = query.filter(ActivityLog.job_id == job_id)
        return query.order_by(desc(ActivityLog.timestamp)).limit(limit).all()

    def get_by_worker(self, worker_id: str, limit: int = 50) -> List[ActivityLog]:
        """Lista logs de um worker"""
        return self.db.query(ActivityLog).filter(
            ActivityLog.worker_id == worker_id
        ).order_by(desc(ActivityLog.timestamp)).limit(limit).all()

    def get_by_level(self, level: str, limit: int = 100) -> List[ActivityLog]:
        """Lista logs por nivel"""
        return self.db.query(ActivityLog).filter(
            ActivityLog.level == level
        ).order_by(desc(ActivityLog.timestamp)).limit(limit).all()

    def get_by_event_type(self, event_type: str, limit: int = 100) -> List[ActivityLog]:
        """Lista logs por tipo de evento"""
        return self.db.query(ActivityLog).filter(
            ActivityLog.event_type == event_type
        ).order_by(desc(ActivityLog.timestamp)).limit(limit).all()


# =============================================================================
# TASK REPOSITORY (Kanban)
# =============================================================================

class TaskRepository:
    """Repositorio para gerenciamento de Tarefas Kanban"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, task_data: dict) -> Task:
        """Cria nova tarefa"""
        # Gera task_id automaticamente se nao fornecido
        if "task_id" not in task_data:
            count = self.db.query(Task).count()
            task_data["task_id"] = f"TSK-{count + 1:04d}"

        # Define kanban_order como ultimo da coluna
        if "kanban_order" not in task_data:
            project_id = task_data.get("project_id")
            status = task_data.get("status", TaskStatus.BACKLOG.value)
            max_order = self.db.query(Task).filter(
                Task.project_id == project_id,
                Task.status == status
            ).count()
            task_data["kanban_order"] = max_order

        task = Task(**task_data)
        self.db.add(task)
        self.db.commit()
        self.db.refresh(task)
        return task

    def get_by_id(self, task_id: str) -> Optional[Task]:
        """Busca tarefa por ID"""
        return self.db.query(Task).filter(Task.task_id == task_id).first()

    def get_all(self, project_id: str = None, status: str = None, limit: int = 100) -> List[Task]:
        """Lista tarefas com filtros opcionais"""
        query = self.db.query(Task)
        if project_id:
            query = query.filter(Task.project_id == project_id)
        if status:
            query = query.filter(Task.status == status)
        return query.order_by(Task.kanban_order).limit(limit).all()

    def get_by_project(self, project_id: str) -> List[Task]:
        """Lista tarefas de um projeto"""
        return self.db.query(Task).filter(
            Task.project_id == project_id
        ).order_by(Task.status, Task.kanban_order).all()

    def get_kanban_board(self, project_id: str) -> Dict[str, List[dict]]:
        """Retorna board Kanban completo com tarefas agrupadas por status"""
        tasks = self.db.query(Task).filter(
            Task.project_id == project_id
        ).order_by(Task.kanban_order).all()

        # Inicializa todas as colunas
        board = {status.value: [] for status in TaskStatus}

        # Agrupa tarefas por status
        for task in tasks:
            if task.status in board:
                board[task.status].append(task.to_dict())

        return board

    def update(self, task_id: str, data: dict) -> Optional[Task]:
        """Atualiza tarefa"""
        task = self.get_by_id(task_id)
        if task:
            for key, value in data.items():
                if hasattr(task, key) and value is not None:
                    setattr(task, key, value)
            task.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(task)
        return task

    def move_task(self, task_id: str, new_status: str, new_order: int = None) -> Optional[Task]:
        """Move tarefa para nova coluna/posicao no Kanban"""
        task = self.get_by_id(task_id)
        if not task:
            return None

        old_status = task.status
        old_order = task.kanban_order

        # Atualiza timestamps baseado no novo status
        if new_status == TaskStatus.IN_DEVELOPMENT.value and not task.started_at:
            task.started_at = datetime.utcnow()
        elif new_status == TaskStatus.DONE.value and not task.completed_at:
            task.completed_at = datetime.utcnow()

        # Reordena tarefas na coluna de origem (se mudou de coluna)
        if old_status != new_status:
            # Decrementa ordem das tarefas abaixo na coluna antiga
            self.db.query(Task).filter(
                Task.project_id == task.project_id,
                Task.status == old_status,
                Task.kanban_order > old_order
            ).update({"kanban_order": Task.kanban_order - 1})

        # Define nova ordem na coluna de destino
        if new_order is None:
            # Adiciona no final da coluna
            new_order = self.db.query(Task).filter(
                Task.project_id == task.project_id,
                Task.status == new_status,
                Task.task_id != task_id
            ).count()
        else:
            # Incrementa ordem das tarefas abaixo na nova posicao
            self.db.query(Task).filter(
                Task.project_id == task.project_id,
                Task.status == new_status,
                Task.kanban_order >= new_order,
                Task.task_id != task_id
            ).update({"kanban_order": Task.kanban_order + 1})

        task.status = new_status
        task.kanban_order = new_order
        task.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(task)
        return task

    def reorder_tasks(self, project_id: str, status: str, task_ids: List[str]) -> bool:
        """Reordena multiplas tarefas em uma coluna"""
        for index, task_id in enumerate(task_ids):
            self.db.query(Task).filter(
                Task.task_id == task_id,
                Task.project_id == project_id,
                Task.status == status
            ).update({"kanban_order": index})
        self.db.commit()
        return True

    def delete(self, task_id: str) -> bool:
        """Remove tarefa"""
        task = self.get_by_id(task_id)
        if task:
            # Reordena tarefas restantes
            self.db.query(Task).filter(
                Task.project_id == task.project_id,
                Task.status == task.status,
                Task.kanban_order > task.kanban_order
            ).update({"kanban_order": Task.kanban_order - 1})

            self.db.delete(task)
            self.db.commit()
            return True
        return False

    def count_by_status(self, project_id: str = None) -> Dict[str, int]:
        """Conta tarefas por status"""
        result = {}
        for status in TaskStatus:
            query = self.db.query(Task).filter(Task.status == status.value)
            if project_id:
                query = query.filter(Task.project_id == project_id)
            result[status.value] = query.count()
        return result

    def get_by_assignee(self, assignee: str, project_id: str = None) -> List[Task]:
        """Lista tarefas por responsavel"""
        query = self.db.query(Task).filter(Task.assignee == assignee)
        if project_id:
            query = query.filter(Task.project_id == project_id)
        return query.order_by(Task.status, Task.kanban_order).all()


# =============================================================================
# STORY REPOSITORY (Agile User Stories)
# =============================================================================

class StoryRepository:
    """Repositorio para gerenciamento de User Stories Agile"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, story_data: dict) -> Story:
        """Cria nova story"""
        # Gera story_id automaticamente se nao fornecido
        if "story_id" not in story_data:
            count = self.db.query(Story).count()
            story_data["story_id"] = f"STR-{count + 1:04d}"

        # Define kanban_order como ultimo da coluna
        if "kanban_order" not in story_data:
            project_id = story_data.get("project_id")
            status = story_data.get("status", StoryStatus.BACKLOG.value)
            max_order = self.db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == status
            ).count()
            story_data["kanban_order"] = max_order

        story = Story(**story_data)
        self.db.add(story)
        self.db.commit()
        self.db.refresh(story)
        return story

    def get_by_id(self, story_id: str) -> Optional[Story]:
        """Busca story por ID"""
        return self.db.query(Story).filter(Story.story_id == story_id).first()

    def get_all(self, project_id: str = None, status: str = None,
                epic_id: str = None, sprint_id: str = None, limit: int = 100) -> List[Story]:
        """Lista stories com filtros opcionais"""
        query = self.db.query(Story)
        if project_id:
            query = query.filter(Story.project_id == project_id)
        if status:
            query = query.filter(Story.status == status)
        if epic_id:
            query = query.filter(Story.epic_id == epic_id)
        if sprint_id:
            query = query.filter(Story.sprint_id == sprint_id)
        return query.order_by(Story.kanban_order).limit(limit).all()

    def get_by_project(self, project_id: str) -> List[Story]:
        """Lista stories de um projeto"""
        return self.db.query(Story).filter(
            Story.project_id == project_id
        ).order_by(Story.status, Story.kanban_order).all()

    def get_story_board(self, project_id: str) -> Dict[str, List[dict]]:
        """Retorna board Kanban completo com stories agrupadas por status"""
        stories = self.db.query(Story).filter(
            Story.project_id == project_id
        ).order_by(Story.kanban_order).all()

        # Inicializa todas as colunas
        board = {status.value: [] for status in StoryStatus}

        # Agrupa stories por status
        for story in stories:
            if story.status in board:
                story_dict = story.to_dict()
                # Adiciona tasks da story
                story_dict["tasks"] = [t.to_dict() for t in story.story_tasks]
                board[story.status].append(story_dict)

        return board

    def get_with_tasks(self, story_id: str) -> Optional[dict]:
        """Busca story completa com tasks e documentacao"""
        story = self.get_by_id(story_id)
        if not story:
            return None

        story_dict = story.to_dict()
        story_dict["tasks"] = [t.to_dict() for t in story.story_tasks]
        story_dict["docs"] = [d.to_dict() for d in story.documentation]
        story_dict["files"] = [a.to_dict() for a in story.attachments]
        return story_dict

    def update(self, story_id: str, data: dict) -> Optional[Story]:
        """Atualiza story"""
        story = self.get_by_id(story_id)
        if story:
            for key, value in data.items():
                if hasattr(story, key) and value is not None:
                    setattr(story, key, value)
            story.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(story)
        return story

    def move_story(self, story_id: str, new_status: str, new_order: int = None) -> Optional[Story]:
        """Move story para nova coluna/posicao no Kanban"""
        story = self.get_by_id(story_id)
        if not story:
            return None

        old_status = story.status
        old_order = story.kanban_order

        # Atualiza timestamps baseado no novo status
        if new_status == StoryStatus.IN_PROGRESS.value and not story.started_at:
            story.started_at = datetime.utcnow()
        elif new_status == StoryStatus.DONE.value and not story.completed_at:
            story.completed_at = datetime.utcnow()

        # Reordena stories na coluna de origem (se mudou de coluna)
        if old_status != new_status:
            self.db.query(Story).filter(
                Story.project_id == story.project_id,
                Story.status == old_status,
                Story.kanban_order > old_order
            ).update({"kanban_order": Story.kanban_order - 1})

        # Define nova ordem na coluna de destino
        if new_order is None:
            new_order = self.db.query(Story).filter(
                Story.project_id == story.project_id,
                Story.status == new_status,
                Story.story_id != story_id
            ).count()
        else:
            self.db.query(Story).filter(
                Story.project_id == story.project_id,
                Story.status == new_status,
                Story.kanban_order >= new_order,
                Story.story_id != story_id
            ).update({"kanban_order": Story.kanban_order + 1})

        story.status = new_status
        story.kanban_order = new_order
        story.updated_at = datetime.utcnow()
        self.db.commit()
        self.db.refresh(story)
        return story

    def update_progress(self, story_id: str) -> Optional[Story]:
        """Atualiza progresso da story baseado nas tasks"""
        story = self.get_by_id(story_id)
        if story:
            total = len(story.story_tasks)
            completed = sum(1 for t in story.story_tasks if t.status == StoryTaskStatus.COMPLETED.value)
            story.tasks_total = total
            story.tasks_completed = completed
            story.progress = (completed / total * 100) if total > 0 else 0
            self.db.commit()
            self.db.refresh(story)
        return story

    def delete(self, story_id: str) -> bool:
        """Remove story"""
        story = self.get_by_id(story_id)
        if story:
            self.db.query(Story).filter(
                Story.project_id == story.project_id,
                Story.status == story.status,
                Story.kanban_order > story.kanban_order
            ).update({"kanban_order": Story.kanban_order - 1})
            self.db.delete(story)
            self.db.commit()
            return True
        return False

    def count_by_status(self, project_id: str = None) -> Dict[str, int]:
        """Conta stories por status"""
        result = {}
        for status in StoryStatus:
            query = self.db.query(Story).filter(Story.status == status.value)
            if project_id:
                query = query.filter(Story.project_id == project_id)
            result[status.value] = query.count()
        return result

    def get_total_points(self, project_id: str, status: str = None) -> int:
        """Soma story points do projeto"""
        query = self.db.query(Story).filter(Story.project_id == project_id)
        if status:
            query = query.filter(Story.status == status)
        stories = query.all()
        return sum(s.story_points or 0 for s in stories)


# =============================================================================
# STORY TASK REPOSITORY (Subtarefas)
# =============================================================================

class StoryTaskRepository:
    """Repositorio para gerenciamento de Tarefas de Stories"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, task_data: dict) -> StoryTask:
        """Cria nova task"""
        if "task_id" not in task_data:
            count = self.db.query(StoryTask).count()
            task_data["task_id"] = f"STSK-{count + 1:04d}"

        # Define task_order
        if "task_order" not in task_data:
            story_id = task_data.get("story_id")
            max_order = self.db.query(StoryTask).filter(
                StoryTask.story_id == story_id
            ).count()
            task_data["task_order"] = max_order

        task = StoryTask(**task_data)
        self.db.add(task)
        self.db.commit()
        self.db.refresh(task)

        # Atualiza progresso da story
        self._update_story_progress(task.story_id)

        return task

    def get_by_id(self, task_id: str) -> Optional[StoryTask]:
        """Busca task por ID"""
        return self.db.query(StoryTask).filter(StoryTask.task_id == task_id).first()

    def get_by_story(self, story_id: str) -> List[StoryTask]:
        """Lista tasks de uma story"""
        return self.db.query(StoryTask).filter(
            StoryTask.story_id == story_id
        ).order_by(StoryTask.task_order).all()

    def update(self, task_id: str, data: dict) -> Optional[StoryTask]:
        """Atualiza task"""
        task = self.get_by_id(task_id)
        if task:
            for key, value in data.items():
                if hasattr(task, key) and value is not None:
                    setattr(task, key, value)

            # Atualiza timestamps
            if data.get("status") == StoryTaskStatus.IN_PROGRESS.value and not task.started_at:
                task.started_at = datetime.utcnow()
            elif data.get("status") == StoryTaskStatus.COMPLETED.value and not task.completed_at:
                task.completed_at = datetime.utcnow()
                task.progress = 100

            task.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(task)

            # Atualiza progresso da story
            self._update_story_progress(task.story_id)

        return task

    def complete(self, task_id: str, output: dict = None) -> Optional[StoryTask]:
        """Marca task como completa com output tecnico"""
        task = self.get_by_id(task_id)
        if task:
            task.status = StoryTaskStatus.COMPLETED.value
            task.progress = 100
            task.completed_at = datetime.utcnow()

            if output:
                if "files_created" in output:
                    task.files_created = output["files_created"]
                if "files_modified" in output:
                    task.files_modified = output["files_modified"]
                if "code_output" in output:
                    task.code_output = output["code_output"]
                if "test_results" in output:
                    task.test_results = output["test_results"]
                if "actual_hours" in output:
                    task.actual_hours = output["actual_hours"]

            self.db.commit()
            self.db.refresh(task)

            # Atualiza progresso da story
            self._update_story_progress(task.story_id)

        return task

    def delete(self, task_id: str) -> bool:
        """Remove task"""
        task = self.get_by_id(task_id)
        if task:
            story_id = task.story_id
            self.db.delete(task)
            self.db.commit()
            self._update_story_progress(story_id)
            return True
        return False

    def _update_story_progress(self, story_id: str):
        """Atualiza progresso da story pai"""
        story = self.db.query(Story).filter(Story.story_id == story_id).first()
        if story:
            total = len(story.story_tasks)
            completed = sum(1 for t in story.story_tasks if t.status == StoryTaskStatus.COMPLETED.value)
            story.tasks_total = total
            story.tasks_completed = completed
            story.progress = (completed / total * 100) if total > 0 else 0
            self.db.commit()


# =============================================================================
# STORY DOCUMENTATION REPOSITORY
# =============================================================================

class StoryDocumentationRepository:
    """Repositorio para gerenciamento de Documentacao de Stories"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, doc_data: dict) -> StoryDocumentation:
        """Cria nova documentacao"""
        if "doc_id" not in doc_data:
            count = self.db.query(StoryDocumentation).count()
            doc_data["doc_id"] = f"DOC-{count + 1:04d}"

        doc = StoryDocumentation(**doc_data)
        self.db.add(doc)
        self.db.commit()
        self.db.refresh(doc)
        return doc

    def get_by_id(self, doc_id: str) -> Optional[StoryDocumentation]:
        """Busca documentacao por ID"""
        return self.db.query(StoryDocumentation).filter(StoryDocumentation.doc_id == doc_id).first()

    def get_by_story(self, story_id: str) -> List[StoryDocumentation]:
        """Lista documentacao de uma story"""
        return self.db.query(StoryDocumentation).filter(
            StoryDocumentation.story_id == story_id
        ).order_by(desc(StoryDocumentation.created_at)).all()

    def get_by_task(self, task_id: str) -> List[StoryDocumentation]:
        """Lista documentacao de uma task"""
        return self.db.query(StoryDocumentation).filter(
            StoryDocumentation.task_id == task_id
        ).order_by(desc(StoryDocumentation.created_at)).all()

    def get_by_type(self, story_id: str, doc_type: str) -> List[StoryDocumentation]:
        """Lista documentacao por tipo"""
        return self.db.query(StoryDocumentation).filter(
            StoryDocumentation.story_id == story_id,
            StoryDocumentation.doc_type == doc_type
        ).all()

    def update(self, doc_id: str, data: dict) -> Optional[StoryDocumentation]:
        """Atualiza documentacao"""
        doc = self.get_by_id(doc_id)
        if doc:
            for key, value in data.items():
                if hasattr(doc, key) and value is not None:
                    setattr(doc, key, value)
            doc.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(doc)
        return doc

    def delete(self, doc_id: str) -> bool:
        """Remove documentacao"""
        doc = self.get_by_id(doc_id)
        if doc:
            self.db.delete(doc)
            self.db.commit()
            return True
        return False


# =============================================================================
# CHAT MESSAGE REPOSITORY (Assistente)
# =============================================================================

class ChatMessageRepository:
    """Repositorio para gerenciamento de Mensagens do Chat"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, message_data: dict) -> ChatMessage:
        """Cria nova mensagem"""
        if "message_id" not in message_data:
            import uuid
            message_data["message_id"] = f"MSG-{uuid.uuid4().hex[:8].upper()}"

        message = ChatMessage(**message_data)
        self.db.add(message)
        self.db.commit()
        self.db.refresh(message)
        return message

    def get_by_id(self, message_id: str) -> Optional[ChatMessage]:
        """Busca mensagem por ID"""
        return self.db.query(ChatMessage).filter(ChatMessage.message_id == message_id).first()

    def get_history(self, project_id: str = None, story_id: str = None,
                    limit: int = 50) -> List[ChatMessage]:
        """Lista historico de mensagens"""
        query = self.db.query(ChatMessage)
        if project_id:
            query = query.filter(ChatMessage.project_id == project_id)
        if story_id:
            query = query.filter(ChatMessage.story_id == story_id)
        return query.order_by(ChatMessage.created_at).limit(limit).all()

    def get_recent(self, limit: int = 20) -> List[ChatMessage]:
        """Lista mensagens recentes"""
        return self.db.query(ChatMessage).order_by(
            desc(ChatMessage.created_at)
        ).limit(limit).all()

    def clear_history(self, project_id: str = None, story_id: str = None) -> int:
        """Limpa historico de mensagens"""
        query = self.db.query(ChatMessage)
        if project_id:
            query = query.filter(ChatMessage.project_id == project_id)
        if story_id:
            query = query.filter(ChatMessage.story_id == story_id)
        count = query.delete()
        self.db.commit()
        return count


# =============================================================================
# ATTACHMENT REPOSITORY
# =============================================================================

class AttachmentRepository:
    """Repositorio para gerenciamento de Anexos"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, attachment_data: dict) -> Attachment:
        """Cria novo anexo"""
        if "attachment_id" not in attachment_data:
            import uuid
            attachment_data["attachment_id"] = f"ATT-{uuid.uuid4().hex[:8].upper()}"

        attachment = Attachment(**attachment_data)
        self.db.add(attachment)
        self.db.commit()
        self.db.refresh(attachment)
        return attachment

    def get_by_id(self, attachment_id: str) -> Optional[Attachment]:
        """Busca anexo por ID"""
        return self.db.query(Attachment).filter(Attachment.attachment_id == attachment_id).first()

    def get_by_story(self, story_id: str) -> List[Attachment]:
        """Lista anexos de uma story"""
        return self.db.query(Attachment).filter(
            Attachment.story_id == story_id
        ).order_by(desc(Attachment.created_at)).all()

    def get_by_task(self, task_id: str) -> List[Attachment]:
        """Lista anexos de uma task"""
        return self.db.query(Attachment).filter(
            Attachment.task_id == task_id
        ).order_by(desc(Attachment.created_at)).all()

    def delete(self, attachment_id: str) -> bool:
        """Remove anexo"""
        attachment = self.get_by_id(attachment_id)
        if attachment:
            self.db.delete(attachment)
            self.db.commit()
            return True
        return False


# =============================================================================
# EPIC REPOSITORY
# =============================================================================

class EpicRepository:
    """Repositorio para gerenciamento de Epicos"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, epic_data: dict) -> Epic:
        """Cria novo epico"""
        if "epic_id" not in epic_data:
            count = self.db.query(Epic).count()
            epic_data["epic_id"] = f"EPIC-{count + 1:04d}"

        epic = Epic(**epic_data)
        self.db.add(epic)
        self.db.commit()
        self.db.refresh(epic)
        return epic

    def get_by_id(self, epic_id: str) -> Optional[Epic]:
        """Busca epico por ID"""
        return self.db.query(Epic).filter(Epic.epic_id == epic_id).first()

    def get_by_project(self, project_id: str) -> List[Epic]:
        """Lista epicos de um projeto"""
        return self.db.query(Epic).filter(
            Epic.project_id == project_id
        ).order_by(Epic.created_at).all()

    def update(self, epic_id: str, data: dict) -> Optional[Epic]:
        """Atualiza epico"""
        epic = self.get_by_id(epic_id)
        if epic:
            for key, value in data.items():
                if hasattr(epic, key) and value is not None:
                    setattr(epic, key, value)
            epic.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(epic)
        return epic

    def delete(self, epic_id: str) -> bool:
        """Remove epico"""
        epic = self.get_by_id(epic_id)
        if epic:
            self.db.delete(epic)
            self.db.commit()
            return True
        return False

    def get_stories(self, epic_id: str) -> List[Story]:
        """Lista stories de um epico"""
        return self.db.query(Story).filter(
            Story.epic_id == epic_id
        ).order_by(Story.kanban_order).all()


# =============================================================================
# SPRINT REPOSITORY
# =============================================================================

class SprintRepository:
    """Repositorio para gerenciamento de Sprints"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, sprint_data: dict) -> Sprint:
        """Cria novo sprint"""
        if "sprint_id" not in sprint_data:
            count = self.db.query(Sprint).count()
            sprint_data["sprint_id"] = f"SPR-{count + 1:04d}"

        sprint = Sprint(**sprint_data)
        self.db.add(sprint)
        self.db.commit()
        self.db.refresh(sprint)
        return sprint

    def get_by_id(self, sprint_id: str) -> Optional[Sprint]:
        """Busca sprint por ID"""
        return self.db.query(Sprint).filter(Sprint.sprint_id == sprint_id).first()

    def get_by_project(self, project_id: str) -> List[Sprint]:
        """Lista sprints de um projeto"""
        return self.db.query(Sprint).filter(
            Sprint.project_id == project_id
        ).order_by(Sprint.start_date).all()

    def get_active(self, project_id: str) -> Optional[Sprint]:
        """Busca sprint ativo de um projeto"""
        return self.db.query(Sprint).filter(
            Sprint.project_id == project_id,
            Sprint.status == "active"
        ).first()

    def update(self, sprint_id: str, data: dict) -> Optional[Sprint]:
        """Atualiza sprint"""
        sprint = self.get_by_id(sprint_id)
        if sprint:
            for key, value in data.items():
                if hasattr(sprint, key) and value is not None:
                    setattr(sprint, key, value)
            sprint.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(sprint)
        return sprint

    def start_sprint(self, sprint_id: str) -> Optional[Sprint]:
        """Inicia sprint"""
        sprint = self.get_by_id(sprint_id)
        if sprint:
            sprint.status = "active"
            sprint.start_date = datetime.utcnow()
            self.db.commit()
            self.db.refresh(sprint)
        return sprint

    def complete_sprint(self, sprint_id: str) -> Optional[Sprint]:
        """Finaliza sprint"""
        sprint = self.get_by_id(sprint_id)
        if sprint:
            sprint.status = "completed"
            sprint.end_date = datetime.utcnow()
            # Calcula velocity
            stories = self.db.query(Story).filter(
                Story.sprint_id == sprint_id,
                Story.status == StoryStatus.DONE.value
            ).all()
            sprint.velocity = sum(s.story_points or 0 for s in stories)
            self.db.commit()
            self.db.refresh(sprint)
        return sprint

    def get_stories(self, sprint_id: str) -> List[Story]:
        """Lista stories de um sprint"""
        return self.db.query(Story).filter(
            Story.sprint_id == sprint_id
        ).order_by(Story.kanban_order).all()

    def delete(self, sprint_id: str) -> bool:
        """Remove sprint"""
        sprint = self.get_by_id(sprint_id)
        if sprint:
            self.db.delete(sprint)
            self.db.commit()
            return True
        return False


# =============================================================================
# EXECUTION LOG REPOSITORY
# =============================================================================

class ExecutionLogRepository:
    """Repositorio para gerenciamento de Logs de Execucao (Replay e Debug)"""

    def __init__(self, db: Session):
        self.db = db

    def create(self, execution_data: dict) -> 'ExecutionLog':
        """Cria novo log de execucao"""
        if "execution_id" not in execution_data:
            import uuid
            execution_data["execution_id"] = f"EXEC-{uuid.uuid4().hex[:8].upper()}"

        execution = ExecutionLog(**execution_data)
        self.db.add(execution)
        self.db.commit()
        self.db.refresh(execution)
        return execution

    def get_by_id(self, execution_id: str) -> Optional['ExecutionLog']:
        """Busca execucao por ID"""
        return self.db.query(ExecutionLog).filter(ExecutionLog.execution_id == execution_id).first()

    def get_all(self, project_id: str = None, task_id: str = None, story_id: str = None,
                status: str = None, limit: int = 100) -> List['ExecutionLog']:
        """Lista execucoes com filtros opcionais"""
        query = self.db.query(ExecutionLog)
        if project_id:
            query = query.filter(ExecutionLog.project_id == project_id)
        if task_id:
            query = query.filter(ExecutionLog.task_id == task_id)
        if story_id:
            query = query.filter(ExecutionLog.story_id == story_id)
        if status:
            query = query.filter(ExecutionLog.status == status)
        return query.order_by(desc(ExecutionLog.created_at)).limit(limit).all()

    def get_by_task(self, task_id: str) -> List['ExecutionLog']:
        """Lista execucoes de uma task"""
        return self.db.query(ExecutionLog).filter(
            ExecutionLog.task_id == task_id
        ).order_by(desc(ExecutionLog.created_at)).all()

    def get_by_story(self, story_id: str) -> List['ExecutionLog']:
        """Lista execucoes de uma story"""
        return self.db.query(ExecutionLog).filter(
            ExecutionLog.story_id == story_id
        ).order_by(desc(ExecutionLog.created_at)).all()

    def get_recent(self, limit: int = 50) -> List['ExecutionLog']:
        """Lista execucoes recentes"""
        return self.db.query(ExecutionLog).order_by(
            desc(ExecutionLog.created_at)
        ).limit(limit).all()

    def get_failed(self, limit: int = 50) -> List['ExecutionLog']:
        """Lista execucoes com falha"""
        return self.db.query(ExecutionLog).filter(
            ExecutionLog.status == "failed"
        ).order_by(desc(ExecutionLog.created_at)).limit(limit).all()

    def update(self, execution_id: str, data: dict) -> Optional['ExecutionLog']:
        """Atualiza log de execucao"""
        execution = self.get_by_id(execution_id)
        if execution:
            for key, value in data.items():
                if hasattr(execution, key) and value is not None:
                    setattr(execution, key, value)
            execution.updated_at = datetime.utcnow()
            self.db.commit()
            self.db.refresh(execution)
        return execution

    def add_step(self, execution_id: str, step_name: str, status: str = "running",
                 input_data: dict = None, output_data: dict = None) -> Optional[str]:
        """Adiciona um passo a execucao"""
        import uuid
        execution = self.get_by_id(execution_id)
        if execution:
            step = {
                "step_id": str(uuid.uuid4())[:8],
                "name": step_name,
                "status": status,
                "started_at": datetime.utcnow().isoformat(),
                "ended_at": None,
                "input": input_data or {},
                "output": output_data or {},
                "error": None
            }
            steps = execution.steps or []
            steps.append(step)
            execution.steps = steps
            self.db.commit()
            return step["step_id"]
        return None

    def complete_step(self, execution_id: str, step_id: str, status: str = "success",
                      output_data: dict = None, error: str = None) -> bool:
        """Completa um passo da execucao"""
        execution = self.get_by_id(execution_id)
        if execution and execution.steps:
            for step in execution.steps:
                if step.get("step_id") == step_id:
                    step["status"] = status
                    step["ended_at"] = datetime.utcnow().isoformat()
                    if output_data:
                        step["output"] = output_data
                    if error:
                        step["error"] = error
                    break
            self.db.commit()
            return True
        return False

    def complete_execution(self, execution_id: str, status: str = "success",
                           output: dict = None, error_message: str = None,
                           error_type: str = None, stack_trace: str = None) -> Optional['ExecutionLog']:
        """Finaliza a execucao"""
        execution = self.get_by_id(execution_id)
        if execution:
            execution.status = status
            execution.ended_at = datetime.utcnow()
            if execution.started_at:
                execution.duration_ms = int((execution.ended_at - execution.started_at).total_seconds() * 1000)
            if output:
                execution.output = output
            if error_message:
                execution.error_message = error_message
            if error_type:
                execution.error_type = error_type
            if stack_trace:
                execution.stack_trace = stack_trace
            self.db.commit()
            self.db.refresh(execution)
        return execution

    def create_replay(self, original_execution_id: str, modified_input: dict = None) -> Optional['ExecutionLog']:
        """Cria uma nova execucao baseada em uma existente (replay)"""
        import uuid
        original = self.get_by_id(original_execution_id)
        if not original:
            return None

        # Incrementa contador de replay no original
        original.replay_count = (original.replay_count or 0) + 1
        self.db.commit()

        # Cria nova execucao
        new_execution = ExecutionLog(
            execution_id=f"EXEC-{uuid.uuid4().hex[:8].upper()}",
            task_id=original.task_id,
            story_id=original.story_id,
            project_id=original.project_id,
            job_id=original.job_id,
            original_input=modified_input or original.original_input,
            replay_of=original_execution_id,
            worker_id=original.worker_id,
            agent_model=original.agent_model
        )
        self.db.add(new_execution)
        self.db.commit()
        self.db.refresh(new_execution)
        return new_execution

    def get_replays(self, execution_id: str) -> List['ExecutionLog']:
        """Lista replays de uma execucao"""
        return self.db.query(ExecutionLog).filter(
            ExecutionLog.replay_of == execution_id
        ).order_by(desc(ExecutionLog.created_at)).all()

    def compare_executions(self, execution_id_1: str, execution_id_2: str) -> dict:
        """Compara duas execucoes"""
        exec1 = self.get_by_id(execution_id_1)
        exec2 = self.get_by_id(execution_id_2)

        if not exec1 or not exec2:
            return {"error": "Execution not found"}

        return {
            "execution_1": exec1.to_dict(),
            "execution_2": exec2.to_dict(),
            "comparison": {
                "duration_diff_ms": (exec2.duration_ms or 0) - (exec1.duration_ms or 0),
                "status_match": exec1.status == exec2.status,
                "steps_count_1": len(exec1.steps or []),
                "steps_count_2": len(exec2.steps or []),
                "output_match": exec1.output == exec2.output
            }
        }

    def delete(self, execution_id: str) -> bool:
        """Remove log de execucao"""
        execution = self.get_by_id(execution_id)
        if execution:
            self.db.delete(execution)
            self.db.commit()
            return True
        return False

    def count_by_status(self, project_id: str = None) -> dict:
        """Conta execucoes por status"""
        result = {}
        statuses = ["running", "success", "failed", "cancelled"]
        for status in statuses:
            query = self.db.query(ExecutionLog).filter(ExecutionLog.status == status)
            if project_id:
                query = query.filter(ExecutionLog.project_id == project_id)
            result[status] = query.count()
        return result


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    "ProjectRepository",
    "JobRepository",
    "WorkerRepository",
    "FailureHistoryRepository",
    "UserRepository",
    "ActivityLogRepository",
    "TaskRepository",
    "StoryRepository",
    "StoryTaskRepository",
    "StoryDocumentationRepository",
    "ChatMessageRepository",
    "AttachmentRepository",
    "EpicRepository",
    "SprintRepository",
    "ExecutionLogRepository",
]

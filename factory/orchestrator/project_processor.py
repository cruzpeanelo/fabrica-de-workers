"""
Processador de Projetos - Plataforma E
=============================================

Processa projetos de forma autonoma, coordenando:
- Analise de arquivos fonte
- Geracao de historias
- Atribuicao a agentes
- Monitoramento de progresso
"""

import os
import json
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional
from dataclasses import dataclass, field
import threading

from .media_processor import MediaProcessor, process_project_media
from .story_generator import StoryGenerator, UserStory


@dataclass
class ProjectMetrics:
    """Metricas de um projeto"""
    total_stories: int = 0
    completed_stories: int = 0
    in_progress_stories: int = 0
    total_points: int = 0
    completed_points: int = 0
    agents_working: int = 0
    estimated_completion: Optional[datetime] = None
    velocity: float = 0.0  # pontos por dia
    burndown: List[Dict] = field(default_factory=list)


@dataclass
class AgentPerformance:
    """Performance de um agente"""
    agent_id: str
    agent_name: str
    stories_completed: int = 0
    points_delivered: int = 0
    tasks_completed: int = 0
    hours_worked: float = 0
    success_rate: float = 100.0
    skills_gained: List[str] = field(default_factory=list)
    current_task: Optional[str] = None
    status: str = "idle"


class ProjectProcessor:
    """
    Processador de Projetos

    Coordena todo o ciclo de vida de um projeto:
    1. Analise de arquivos fonte
    2. Geracao de historias
    3. Validacao e aprovacao
    4. Atribuicao a agentes
    5. Monitoramento de progresso
    6. Relatorios de PM
    """

    def __init__(self, project_id: str, project_name: str,
                 source_path: str, output_path: str):
        self.project_id = project_id
        self.project_name = project_name
        self.source_path = Path(source_path)
        self.output_path = Path(output_path)

        # Componentes
        self.media_processor = MediaProcessor(str(self.output_path / "media"))
        self.story_generator = StoryGenerator(project_id)

        # Estado
        self.status = "created"
        self.metrics = ProjectMetrics()
        self.agent_performance: Dict[str, AgentPerformance] = {}
        self.timeline: List[Dict] = []

        # Callbacks
        self.on_status_change = None
        self.on_story_generated = None
        self.on_agent_update = None

        # Background processing
        self._processing_thread: Optional[threading.Thread] = None

    def start_processing(self):
        """Inicia processamento em background"""
        self._processing_thread = threading.Thread(
            target=self._process_async,
            daemon=True
        )
        self._processing_thread.start()
        return {"status": "started", "project_id": self.project_id}

    def _process_async(self):
        """Processamento assincrono do projeto"""
        try:
            self._update_status("analyzing")
            self._log_timeline("Iniciando analise do projeto")

            # 1. Processar arquivos de midia
            self._log_timeline("Processando arquivos de midia...")
            self._process_media_files()

            # 2. Gerar historias
            self._update_status("generating_stories")
            self._log_timeline("Gerando historias a partir dos requisitos...")
            self._generate_stories()

            # 3. Calcular metricas iniciais
            self._update_status("planning")
            self._log_timeline("Calculando metricas e estimativas...")
            self._calculate_metrics()

            # 4. Pronto para desenvolvimento
            self._update_status("ready")
            self._log_timeline("Projeto pronto para desenvolvimento")

        except Exception as e:
            self._update_status("error")
            self._log_timeline(f"Erro no processamento: {str(e)}")

    def _update_status(self, status: str):
        """Atualiza status do projeto"""
        self.status = status
        if self.on_status_change:
            self.on_status_change(self.project_id, status)

    def _log_timeline(self, message: str):
        """Adiciona entrada na timeline"""
        self.timeline.append({
            "timestamp": datetime.now().isoformat(),
            "message": message,
            "status": self.status
        })

    def _process_media_files(self):
        """Processa todos os arquivos de midia"""
        if not self.source_path.exists():
            self._log_timeline(f"Caminho fonte nao existe: {self.source_path}")
            return

        # Escaneia arquivos
        files = self.media_processor.scan_directory(str(self.source_path))
        self._log_timeline(f"Encontrados {len(files)} arquivos para processar")

        # Processa cada arquivo
        for file in files:
            try:
                self._log_timeline(f"Processando: {file.file_name}")
                result = self.media_processor.process_file(file)
                self._log_timeline(f"Concluido: {file.file_name} - {len(result.requirements)} requisitos extraidos")
            except Exception as e:
                self._log_timeline(f"Erro em {file.file_name}: {str(e)}")

    def _generate_stories(self):
        """Gera historias a partir do conteudo extraido"""
        # Obtem todo o texto extraido
        all_text = self.media_processor.get_all_text_content()

        if all_text:
            stories = self.story_generator.generate_from_text(all_text, "media_extraction")
            self._log_timeline(f"Geradas {len(stories)} historias a partir do conteudo")

            for story in stories:
                if self.on_story_generated:
                    self.on_story_generated(story)

        # Obtem requisitos extraidos
        requirements = self.media_processor.get_all_requirements()
        if requirements:
            stories = self.story_generator.generate_from_requirements(requirements, "requirements")
            self._log_timeline(f"Geradas {len(stories)} historias a partir de requisitos")

    def _calculate_metrics(self):
        """Calcula metricas do projeto"""
        stories = self.story_generator.stories

        self.metrics.total_stories = len(stories)
        self.metrics.total_points = sum(s.story_points for s in stories)

        # Estima conclusao baseado em velocidade media (10 pontos/sprint de 2 semanas)
        sprints_needed = self.metrics.total_points / 10
        days_needed = sprints_needed * 14
        self.metrics.estimated_completion = datetime.now() + timedelta(days=days_needed)

        self._log_timeline(
            f"Projeto tem {self.metrics.total_stories} historias, "
            f"{self.metrics.total_points} pontos. "
            f"Estimativa: {self.metrics.estimated_completion.strftime('%d/%m/%Y')}"
        )

    def assign_agent(self, story_id: str, agent_id: str, agent_name: str):
        """Atribui um agente a uma historia"""
        # Encontra a historia
        story = next((s for s in self.story_generator.stories if s.story_id == story_id), None)
        if not story:
            return False

        story.assigned_to = agent_id
        story.status = "in_progress"

        # Atualiza performance do agente
        if agent_id not in self.agent_performance:
            self.agent_performance[agent_id] = AgentPerformance(
                agent_id=agent_id,
                agent_name=agent_name
            )

        perf = self.agent_performance[agent_id]
        perf.current_task = story_id
        perf.status = "working"

        self._log_timeline(f"Historia {story_id} atribuida ao agente {agent_name}")

        if self.on_agent_update:
            self.on_agent_update(agent_id, "assigned", story_id)

        return True

    def complete_story(self, story_id: str, agent_id: str):
        """Marca historia como concluida"""
        story = next((s for s in self.story_generator.stories if s.story_id == story_id), None)
        if not story:
            return False

        story.status = "done"

        # Atualiza metricas
        self.metrics.completed_stories += 1
        self.metrics.completed_points += story.story_points
        self.metrics.in_progress_stories = max(0, self.metrics.in_progress_stories - 1)

        # Atualiza performance do agente
        if agent_id in self.agent_performance:
            perf = self.agent_performance[agent_id]
            perf.stories_completed += 1
            perf.points_delivered += story.story_points
            perf.current_task = None
            perf.status = "idle"
            perf.skills_gained.extend(story.tags)

        self._log_timeline(f"Historia {story_id} concluida por {agent_id}")

        return True

    def get_pm_report(self) -> Dict:
        """Gera relatorio do Project Manager"""
        backlog = self.story_generator.get_backlog_summary()

        # Calcula progresso
        progress = 0
        if self.metrics.total_points > 0:
            progress = (self.metrics.completed_points / self.metrics.total_points) * 100

        # Calcula velocidade
        velocity = 0
        if len(self.timeline) > 1:
            days_elapsed = (datetime.now() - datetime.fromisoformat(self.timeline[0]['timestamp'])).days
            if days_elapsed > 0:
                velocity = self.metrics.completed_points / days_elapsed

        # Status dos agentes
        agents_working = len([a for a in self.agent_performance.values() if a.status == "working"])
        agents_idle = len([a for a in self.agent_performance.values() if a.status == "idle"])

        return {
            "project_id": self.project_id,
            "project_name": self.project_name,
            "status": self.status,
            "generated_at": datetime.now().isoformat(),

            "progress": {
                "percentage": round(progress, 1),
                "stories_completed": self.metrics.completed_stories,
                "stories_total": self.metrics.total_stories,
                "points_completed": self.metrics.completed_points,
                "points_total": self.metrics.total_points
            },

            "timeline": {
                "started_at": self.timeline[0]['timestamp'] if self.timeline else None,
                "estimated_completion": self.metrics.estimated_completion.isoformat() if self.metrics.estimated_completion else None,
                "velocity_points_per_day": round(velocity, 2)
            },

            "backlog": backlog,

            "team": {
                "agents_total": len(self.agent_performance),
                "agents_working": agents_working,
                "agents_idle": agents_idle
            },

            "top_performers": sorted(
                [
                    {
                        "agent_id": a.agent_id,
                        "agent_name": a.agent_name,
                        "points_delivered": a.points_delivered,
                        "stories_completed": a.stories_completed
                    }
                    for a in self.agent_performance.values()
                ],
                key=lambda x: x['points_delivered'],
                reverse=True
            )[:5],

            "recent_activity": self.timeline[-10:] if self.timeline else []
        }

    def get_agent_performance_report(self) -> List[Dict]:
        """Retorna relatorio de performance dos agentes"""
        return [
            {
                "agent_id": perf.agent_id,
                "agent_name": perf.agent_name,
                "status": perf.status,
                "current_task": perf.current_task,
                "stories_completed": perf.stories_completed,
                "points_delivered": perf.points_delivered,
                "tasks_completed": perf.tasks_completed,
                "hours_worked": perf.hours_worked,
                "success_rate": perf.success_rate,
                "skills_gained": list(set(perf.skills_gained))
            }
            for perf in self.agent_performance.values()
        ]

    def get_burndown_data(self) -> List[Dict]:
        """Retorna dados para grafico de burndown"""
        # Simula burndown baseado no progresso
        total_points = self.metrics.total_points
        completed = self.metrics.completed_points

        data = []
        start_date = datetime.now() - timedelta(days=7)

        for i in range(8):
            date = start_date + timedelta(days=i)
            if i == 7:
                remaining = total_points - completed
            else:
                # Simula progresso gradual
                remaining = total_points - (completed * i / 7)

            data.append({
                "date": date.strftime("%Y-%m-%d"),
                "remaining": round(max(0, remaining)),
                "ideal": round(total_points * (1 - i/14))  # Linha ideal
            })

        return data

    def create_story_manually(self, story_data: Dict) -> UserStory:
        """Cria historia manualmente"""
        story = self.story_generator.create_manual_story(
            title=story_data.get('title', ''),
            as_a=story_data.get('as_a', 'usuario'),
            i_want=story_data.get('i_want', ''),
            so_that=story_data.get('so_that', ''),
            acceptance_criteria=story_data.get('acceptance_criteria', []),
            description=story_data.get('description', ''),
            story_points=story_data.get('story_points', 3),
            priority=story_data.get('priority', 3),
            sprint=story_data.get('sprint', 1),
            tags=story_data.get('tags', []),
            created_by=story_data.get('created_by', 'user')
        )

        self._log_timeline(f"Historia {story.story_id} criada manualmente: {story.title}")
        self._calculate_metrics()

        return story

    def edit_story(self, story_id: str, updates: Dict) -> Optional[UserStory]:
        """Edita uma historia existente"""
        story = next((s for s in self.story_generator.stories if s.story_id == story_id), None)
        if not story:
            return None

        # Aplica atualizacoes
        if 'title' in updates:
            story.title = updates['title']
        if 'as_a' in updates:
            story.as_a = updates['as_a']
        if 'i_want' in updates:
            story.i_want = updates['i_want']
        if 'so_that' in updates:
            story.so_that = updates['so_that']
        if 'description' in updates:
            story.description = updates['description']
        if 'story_points' in updates:
            story.story_points = updates['story_points']
        if 'priority' in updates:
            from .story_generator import StoryPriority
            story.priority = StoryPriority(updates['priority'])
        if 'sprint' in updates:
            story.sprint = updates['sprint']
        if 'status' in updates:
            story.status = updates['status']

        self._log_timeline(f"Historia {story_id} editada")
        self._calculate_metrics()

        return story

    def get_pending_approvals(self) -> List[Dict]:
        """Retorna historias pendentes de aprovacao"""
        pending = self.story_generator.get_stories_by_status("pending_review")
        return [s.to_dict() for s in pending]

    def approve_story(self, story_id: str, approver_id: str, notes: str = None) -> bool:
        """Aprova uma historia"""
        story = next((s for s in self.story_generator.stories if s.story_id == story_id), None)
        if not story:
            return False

        is_valid, score, validation_notes = self.story_generator.validate_story(story, approver_id)

        if is_valid:
            story.status = "approved"
            self._log_timeline(f"Historia {story_id} aprovada por {approver_id}")
        else:
            self._log_timeline(f"Historia {story_id} falhou na validacao: {validation_notes}")

        return is_valid

    def reject_story(self, story_id: str, rejector_id: str, reason: str) -> bool:
        """Rejeita uma historia"""
        story = next((s for s in self.story_generator.stories if s.story_id == story_id), None)
        if not story:
            return False

        story.status = "rejected"
        story.validation_notes = reason
        story.reviewer = rejector_id

        self._log_timeline(f"Historia {story_id} rejeitada por {rejector_id}: {reason}")

        return True

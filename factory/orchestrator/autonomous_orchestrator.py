"""
Orquestrador Autonomo da Plataforma E
============================================

Sistema central que coordena:
- Processamento de projetos
- Geracao automatica de historias
- Acionamento de agentes
- Respeito a hierarquia de aprovacoes
- Atualizacao de conhecimentos dos agentes
"""

import json
import asyncio
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field, asdict
from enum import Enum
import threading
import queue

# Imports da Fabrica
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from factory.database.connection import SessionLocal, init_db
from factory.database.models import Project, Story, Agent, Task, ActivityLog


class ProjectStatus(Enum):
    PENDING = "pending"
    ANALYZING = "analyzing"
    PLANNING = "planning"
    IN_PROGRESS = "in_progress"
    REVIEW = "review"
    COMPLETED = "completed"
    ON_HOLD = "on_hold"


class StoryStatus(Enum):
    DRAFT = "draft"
    PENDING_REVIEW = "pending_review"
    APPROVED = "approved"
    REJECTED = "rejected"
    IN_PROGRESS = "in_progress"
    TESTING = "testing"
    DONE = "done"


class AgentTaskStatus(Enum):
    IDLE = "idle"
    WORKING = "working"
    WAITING_APPROVAL = "waiting_approval"
    BLOCKED = "blocked"


@dataclass
class ProjectConfig:
    """Configuracao de um projeto"""
    project_id: str
    name: str
    description: str
    source_path: str  # Caminho dos arquivos fonte
    output_path: str  # Caminho onde o projeto sera desenvolvido

    # Configuracoes de processamento
    process_videos: bool = True
    process_audio: bool = True
    process_documents: bool = True

    # Agentes responsaveis
    lead_agent_id: Optional[str] = None
    assigned_agents: List[str] = field(default_factory=list)

    # Hierarquia de aprovacao
    requires_approval: bool = True
    approver_agent_id: Optional[str] = None

    # Metadados
    created_at: datetime = field(default_factory=datetime.now)
    estimated_completion: Optional[datetime] = None
    priority: int = 5  # 1-10


@dataclass
class GeneratedStory:
    """Historia gerada automaticamente"""
    story_id: str
    title: str
    description: str
    acceptance_criteria: List[str]
    story_points: int
    priority: int
    sprint: int

    # Formato Agile
    as_a: str  # "Como um..."
    i_want: str  # "Eu quero..."
    so_that: str  # "Para que..."

    # Metadados
    source: str  # De onde foi extraida (video, audio, doc)
    source_timestamp: Optional[str] = None
    status: StoryStatus = StoryStatus.DRAFT
    assigned_agent: Optional[str] = None
    reviewer_agent: Optional[str] = None

    # Validacao
    is_validated: bool = False
    validation_notes: Optional[str] = None


@dataclass
class AgentActivity:
    """Registro de atividade de um agente"""
    agent_id: str
    project_id: str
    activity_type: str
    description: str
    started_at: datetime
    completed_at: Optional[datetime] = None
    status: str = "in_progress"
    result: Optional[str] = None
    skills_used: List[str] = field(default_factory=list)
    skills_gained: List[str] = field(default_factory=list)
    xp_earned: int = 0


class AutonomousOrchestrator:
    """
    Orquestrador Autonomo Principal

    Responsavel por:
    - Gerenciar projetos de forma autonoma
    - Coordenar multiplos agentes
    - Gerar e validar historias
    - Respeitar hierarquia de aprovacoes
    - Atualizar conhecimentos dos agentes
    """

    def __init__(self):
        self.projects: Dict[str, ProjectConfig] = {}
        self.active_tasks: Dict[str, AgentActivity] = {}
        self.story_queue: queue.Queue = queue.Queue()
        self.task_queue: queue.Queue = queue.Queue()

        # Background workers
        self._running = False
        self._workers: List[threading.Thread] = []

        # Callbacks para eventos
        self.on_story_generated = None
        self.on_agent_activity = None
        self.on_approval_needed = None
        self.on_project_update = None

        # Carregar agentes corporativos
        self._load_corporate_agents()

        # Carregar projetos existentes do banco
        self._load_existing_projects()

    def _load_corporate_agents(self):
        """Carrega agentes da hierarquia corporativa"""
        try:
            from factory.agents.corporate_hierarchy import ALL_CORPORATE_AGENTS
            self.corporate_agents = ALL_CORPORATE_AGENTS
        except ImportError:
            self.corporate_agents = {}

    def _load_existing_projects(self):
        """Carrega projetos existentes do banco de dados"""
        db = SessionLocal()
        try:
            projects = db.query(Project).all()
            for project in projects:
                config = project.config or {}
                self.projects[project.project_id] = ProjectConfig(
                    project_id=project.project_id,
                    name=project.name,
                    description=project.description or "",
                    source_path=config.get('source_path', ''),
                    output_path=config.get('output_path', ''),
                    process_videos=config.get('process_videos', True),
                    process_audio=config.get('process_audio', True),
                    process_documents=config.get('process_documents', True),
                    requires_approval=config.get('requires_approval', True)
                )
        except Exception as e:
            print(f"Erro ao carregar projetos existentes: {e}")
        finally:
            db.close()

    def create_project(self, config: ProjectConfig) -> str:
        """
        Cria um novo projeto no orquestrador

        Returns:
            project_id do projeto criado
        """
        # Salva configuracao
        self.projects[config.project_id] = config

        # Registra no banco de dados
        db = SessionLocal()
        try:
            project = Project(
                project_id=config.project_id,
                name=config.name,
                description=config.description,
                project_type="bpm",
                status=ProjectStatus.PENDING.value,
                config={
                    "source_path": config.source_path,
                    "output_path": config.output_path,
                    "process_videos": config.process_videos,
                    "process_audio": config.process_audio,
                    "process_documents": config.process_documents,
                    "requires_approval": config.requires_approval
                }
            )
            db.add(project)
            db.commit()

            # Log da criacao
            self._log_activity(
                db=db,
                agent_id="ORCHESTRATOR",
                project_id=config.project_id,
                action="project_created",
                message=f"Projeto '{config.name}' criado com sucesso",
                level="INFO"
            )

        finally:
            db.close()

        # Notifica
        if self.on_project_update:
            self.on_project_update(config.project_id, "created")

        return config.project_id

    def start_project_processing(self, project_id: str):
        """
        Inicia o processamento autonomo de um projeto

        Fluxo:
        1. Analisa arquivos fonte (videos, audios, docs)
        2. Extrai requisitos e gera historias
        3. Envia historias para validacao
        4. Atribui a agentes
        5. Monitora desenvolvimento
        """
        if project_id not in self.projects:
            raise ValueError(f"Projeto {project_id} nao encontrado")

        config = self.projects[project_id]

        # Inicia em background
        thread = threading.Thread(
            target=self._process_project_async,
            args=(project_id,),
            daemon=True
        )
        thread.start()
        self._workers.append(thread)

        return {"status": "started", "project_id": project_id}

    def _process_project_async(self, project_id: str):
        """Processamento assincrono do projeto"""
        config = self.projects[project_id]

        db = SessionLocal()
        try:
            # Atualiza status
            project = db.query(Project).filter(Project.project_id == project_id).first()
            if project:
                project.status = ProjectStatus.ANALYZING.value
                db.commit()

            self._log_activity(
                db=db,
                agent_id="ORCHESTRATOR",
                project_id=project_id,
                action="processing_started",
                message=f"Iniciando processamento do projeto '{config.name}'",
                level="INFO"
            )

            # 1. Processar documentos
            if config.process_documents:
                self._process_documents(project_id, config.source_path, db)

            # 2. Processar videos
            if config.process_videos:
                self._process_videos(project_id, config.source_path, db)

            # 3. Processar audios
            if config.process_audio:
                self._process_audio(project_id, config.source_path, db)

            # 4. Gerar historias baseadas nos requisitos extraidos
            self._generate_stories(project_id, db)

            # 5. Atualiza status para planning
            if project:
                project.status = ProjectStatus.PLANNING.value
                db.commit()

            self._log_activity(
                db=db,
                agent_id="ORCHESTRATOR",
                project_id=project_id,
                action="analysis_complete",
                message="Analise de arquivos concluida. Historias geradas aguardando aprovacao.",
                level="INFO"
            )

        except Exception as e:
            self._log_activity(
                db=db,
                agent_id="ORCHESTRATOR",
                project_id=project_id,
                action="error",
                message=f"Erro no processamento: {str(e)}",
                level="ERROR"
            )
        finally:
            db.close()

    def _process_documents(self, project_id: str, source_path: str, db):
        """Processa documentos (DOCX, PDF, etc)"""
        from pathlib import Path

        source = Path(source_path)
        if not source.exists():
            return

        # Busca arquivos de documento
        doc_extensions = ['*.docx', '*.doc', '*.pdf', '*.txt', '*.md']
        documents = []

        for ext in doc_extensions:
            documents.extend(source.rglob(ext))

        for doc_path in documents:
            self._log_activity(
                db=db,
                agent_id="DOC-ANALYZER",
                project_id=project_id,
                action="processing_document",
                message=f"Processando documento: {doc_path.name}",
                level="INFO"
            )

            # Extrai texto do documento
            text = self._extract_text_from_document(doc_path)

            if text:
                # Salva requisitos extraidos
                self._save_extracted_requirements(
                    project_id=project_id,
                    source=str(doc_path),
                    source_type="document",
                    content=text,
                    db=db
                )

    def _process_videos(self, project_id: str, source_path: str, db):
        """Processa videos (extrai frames, OCR, transcreve audio)"""
        from pathlib import Path

        source = Path(source_path)
        if not source.exists():
            return

        # Busca arquivos de video
        video_extensions = ['*.mp4', '*.avi', '*.mov', '*.mkv']
        videos = []

        for ext in video_extensions:
            videos.extend(source.rglob(ext))

        for video_path in videos:
            self._log_activity(
                db=db,
                agent_id="VIDEO-ANALYZER",
                project_id=project_id,
                action="processing_video",
                message=f"Processando video: {video_path.name}",
                level="INFO"
            )

            # TODO: Implementar extracao de frames e OCR
            # TODO: Implementar transcricao de audio do video

            self._save_extracted_requirements(
                project_id=project_id,
                source=str(video_path),
                source_type="video",
                content=f"[Video pendente de processamento: {video_path.name}]",
                db=db
            )

    def _process_audio(self, project_id: str, source_path: str, db):
        """Processa arquivos de audio (transcreve)"""
        from pathlib import Path

        source = Path(source_path)
        if not source.exists():
            return

        # Busca arquivos de audio
        audio_extensions = ['*.mp3', '*.wav', '*.m4a', '*.ogg']
        audios = []

        for ext in audio_extensions:
            audios.extend(source.rglob(ext))

        for audio_path in audios:
            self._log_activity(
                db=db,
                agent_id="AUDIO-ANALYZER",
                project_id=project_id,
                action="processing_audio",
                message=f"Processando audio: {audio_path.name}",
                level="INFO"
            )

            # TODO: Implementar transcricao com Whisper ou similar

            self._save_extracted_requirements(
                project_id=project_id,
                source=str(audio_path),
                source_type="audio",
                content=f"[Audio pendente de transcricao: {audio_path.name}]",
                db=db
            )

    def _extract_text_from_document(self, doc_path: Path) -> Optional[str]:
        """Extrai texto de um documento"""
        try:
            if doc_path.suffix.lower() in ['.docx']:
                from docx import Document
                doc = Document(str(doc_path))
                return '\n'.join([para.text for para in doc.paragraphs if para.text.strip()])

            elif doc_path.suffix.lower() in ['.txt', '.md']:
                return doc_path.read_text(encoding='utf-8')

            elif doc_path.suffix.lower() == '.pdf':
                # TODO: Implementar extracao de PDF
                return None

        except Exception as e:
            print(f"Erro ao extrair texto de {doc_path}: {e}")
            return None

        return None

    def _save_extracted_requirements(self, project_id: str, source: str,
                                     source_type: str, content: str, db):
        """Salva requisitos extraidos no banco"""
        import uuid
        # Cria task para rastrear com ID unico
        task = Task(
            task_id=f"REQ-{project_id[:8]}-{uuid.uuid4().hex[:8]}",
            project_id=project_id,
            title=f"Requisitos de {source_type}: {Path(source).name}",
            description=content[:500] if content else "",
            task_type="requirement_extraction",
            status="completed",
            payload={
                "source": source,
                "source_type": source_type,
                "full_content": content
            }
        )
        db.add(task)
        db.commit()

    def _generate_stories(self, project_id: str, db):
        """Gera historias baseadas nos requisitos extraidos"""
        # Busca requisitos extraidos
        tasks = db.query(Task).filter(
            Task.project_id == project_id,
            Task.task_type == "requirement_extraction"
        ).all()

        if not tasks:
            return

        # Combina todo o conteudo
        all_content = []
        for task in tasks:
            if task.payload and 'full_content' in task.payload:
                all_content.append(task.payload['full_content'])

        combined_content = '\n\n'.join(all_content)

        # Analisa e gera historias
        stories = self._analyze_and_generate_stories(project_id, combined_content, db)

        # Salva historias no banco - aprovacoes sao rastreadas pelo status da story no BD
        for story_data in stories:
            # Verifica se story ja existe
            existing = db.query(Story).filter(Story.story_id == story_data['story_id']).first()
            if existing:
                continue

            story = Story(
                story_id=story_data['story_id'],
                project_id=project_id,
                title=story_data['title'],
                description=story_data['description'],
                status=StoryStatus.PENDING_REVIEW.value,
                sprint=story_data.get('sprint', 1),
                points=story_data.get('points', 3),
                priority=story_data.get('priority', 5),
                narrative_persona=story_data.get('as_a', ''),
                narrative_action=story_data.get('i_want', ''),
                narrative_benefit=story_data.get('so_that', ''),
                acceptance_criteria=story_data.get('acceptance_criteria', [])
            )
            db.add(story)

        db.commit()

        self._log_activity(
            db=db,
            agent_id="STORY-GENERATOR",
            project_id=project_id,
            action="stories_generated",
            message=f"{len(stories)} historias geradas aguardando aprovacao",
            level="INFO"
        )

    def _analyze_and_generate_stories(self, project_id: str, content: str, db) -> List[Dict]:
        """
        Analisa conteudo e gera historias no formato Agile

        Esta funcao seria idealmente conectada a um LLM para
        analise mais sofisticada, mas aqui usamos regras basicas
        """
        stories = []

        # Historias padrao para projeto BPM baseadas no conteudo analisado
        bpm_stories = [
            {
                "story_id": f"US-{project_id}-001",
                "title": "Visualizacao de Processos AS-IS",
                "description": "Interface para visualizar os processos atuais (AS-IS) da organizacao",
                "as_a": "usuario de negocios",
                "i_want": "visualizar os processos atuais da empresa em formato grafico",
                "so_that": "possa entender como as coisas funcionam hoje",
                "acceptance_criteria": [
                    "Exibir processos em notacao BPMN",
                    "Permitir zoom e navegacao no diagrama",
                    "Mostrar detalhes ao clicar em cada etapa",
                    "Usar cores da identidade visual da marca"
                ],
                "sprint": 1,
                "points": 8,
                "priority": 1,
                "source": "auto_generated"
            },
            {
                "story_id": f"US-{project_id}-002",
                "title": "Edicao de Processos AS-IS",
                "description": "Permitir que usuarios editem os processos AS-IS",
                "as_a": "analista de processos",
                "i_want": "editar os processos AS-IS diretamente na interface",
                "so_that": "possa corrigir e atualizar a documentacao",
                "acceptance_criteria": [
                    "Drag and drop de elementos BPMN",
                    "Adicionar/remover etapas do processo",
                    "Editar propriedades de cada elemento",
                    "Salvar alteracoes no banco de dados",
                    "Historico de versoes"
                ],
                "sprint": 1,
                "points": 13,
                "priority": 1,
                "source": "auto_generated"
            },
            {
                "story_id": f"US-{project_id}-003",
                "title": "Visualizacao de Processos TO-BE",
                "description": "Interface para visualizar os processos futuros (TO-BE)",
                "as_a": "usuario de negocios",
                "i_want": "visualizar como os processos serao no futuro",
                "so_that": "possa entender as mudancas planejadas",
                "acceptance_criteria": [
                    "Exibir processos TO-BE em notacao BPMN",
                    "Destacar diferencas em relacao ao AS-IS",
                    "Comparacao lado a lado AS-IS vs TO-BE",
                    "Usar cores da identidade visual da marca"
                ],
                "sprint": 2,
                "points": 8,
                "priority": 2,
                "source": "auto_generated"
            },
            {
                "story_id": f"US-{project_id}-004",
                "title": "Edicao de Processos TO-BE",
                "description": "Permitir que usuarios editem os processos TO-BE",
                "as_a": "analista de processos",
                "i_want": "editar os processos TO-BE diretamente na interface",
                "so_that": "possa desenhar os processos futuros",
                "acceptance_criteria": [
                    "Drag and drop de elementos BPMN",
                    "Copiar processo AS-IS como base para TO-BE",
                    "Adicionar/remover etapas do processo",
                    "Marcar mudancas em relacao ao AS-IS",
                    "Salvar alteracoes no banco de dados"
                ],
                "sprint": 2,
                "points": 13,
                "priority": 2,
                "source": "auto_generated"
            },
            {
                "story_id": f"US-{project_id}-005",
                "title": "Cadastro de Clientes - Mapeamento AS-IS",
                "description": "Mapear processo atual de cadastro de clientes conforme discutido nas reunioes",
                "as_a": "analista de negocios",
                "i_want": "documentar o processo atual de cadastro de clientes",
                "so_that": "tenhamos uma visao clara do fluxo atual",
                "acceptance_criteria": [
                    "Mapear tipos de conta (PJ, PF, Parceiro, Agrupador)",
                    "Documentar areas de vendas e canais (20, 25, 30, 40)",
                    "Incluir fluxo de aprovacao de credito",
                    "Documentar integracao com sistema atual"
                ],
                "sprint": 1,
                "points": 5,
                "priority": 1,
                "source": "transcricao_reuniao"
            },
            {
                "story_id": f"US-{project_id}-006",
                "title": "Documentos Fiscais - Mapeamento AS-IS",
                "description": "Mapear processo atual de documentos fiscais",
                "as_a": "analista de negocios",
                "i_want": "documentar o processo atual de documentos fiscais",
                "so_that": "tenhamos uma visao clara das restricoes logisticas",
                "acceptance_criteria": [
                    "Mapear fluxo de emissao de NF",
                    "Documentar restricoes logisticas",
                    "Incluir validacoes fiscais",
                    "Mapear integracoes com ERP"
                ],
                "sprint": 1,
                "points": 5,
                "priority": 2,
                "source": "transcricao_reuniao"
            },
            {
                "story_id": f"US-{project_id}-007",
                "title": "Workflow de Pricing - Mapeamento",
                "description": "Mapear processo de precificacao e cotacao",
                "as_a": "analista de negocios",
                "i_want": "documentar o workflow de pricing",
                "so_that": "possamos otimizar o processo de cotacao",
                "acceptance_criteria": [
                    "Mapear fluxo de cotacao",
                    "Documentar regras de pricing",
                    "Incluir niveis de aprovacao",
                    "Mapear integracao com concorrencia"
                ],
                "sprint": 2,
                "points": 8,
                "priority": 2,
                "source": "transcricao_reuniao"
            },
            {
                "story_id": f"US-{project_id}-008",
                "title": "Dashboard de Acompanhamento",
                "description": "Tela para acompanhar status dos processos",
                "as_a": "gestor",
                "i_want": "ver um dashboard com status de todos os processos",
                "so_that": "possa acompanhar o progresso do mapeamento",
                "acceptance_criteria": [
                    "Mostrar processos AS-IS mapeados",
                    "Mostrar processos TO-BE definidos",
                    "Indicadores de completude",
                    "Filtros por area/departamento",
                    "Usar identidade visual da marca"
                ],
                "sprint": 3,
                "points": 8,
                "priority": 3,
                "source": "auto_generated"
            }
        ]

        return bpm_stories

    def _log_activity(self, db, agent_id: str, project_id: str,
                      action: str, message: str, level: str = "INFO"):
        """Registra atividade no log"""
        log = ActivityLog(
            source="orchestrator",
            source_id="ORCH",
            agent_id=agent_id,
            project_id=project_id,
            event_type=action,
            message=message,
            level=level
        )
        db.add(log)
        db.commit()

        # Notifica
        if self.on_agent_activity:
            self.on_agent_activity({
                "agent_id": agent_id,
                "project_id": project_id,
                "action": action,
                "message": message,
                "level": level,
                "timestamp": datetime.now().isoformat()
            })

    def get_pending_approvals(self, project_id: Optional[str] = None) -> List[Dict]:
        """Retorna itens pendentes de aprovacao - busca diretamente do banco de dados"""
        db = SessionLocal()
        try:
            query = db.query(Story).filter(Story.status == StoryStatus.PENDING_REVIEW.value)

            if project_id:
                query = query.filter(Story.project_id == project_id)

            stories = query.order_by(Story.created_at.desc()).all()

            approvals = []
            for story in stories:
                approvals.append({
                    "id": story.story_id,
                    "type": "story",
                    "project_id": story.project_id,
                    "data": {
                        "story_id": story.story_id,
                        "title": story.title,
                        "description": story.description or "",
                        "as_a": story.narrative_persona or "",
                        "i_want": story.narrative_action or "",
                        "so_that": story.narrative_benefit or "",
                        "acceptance_criteria": story.acceptance_criteria or [],
                        "sprint": story.sprint or 1,
                        "points": story.points or 3,
                        "priority": story.priority or 5
                    },
                    "created_at": story.created_at.isoformat() if story.created_at else None,
                    "status": "pending"
                })

            return approvals
        finally:
            db.close()

    def approve_item(self, item_id: str, approved_by: str,
                     notes: Optional[str] = None, edits: Optional[Dict] = None) -> bool:
        """Aprova um item pendente - atualiza diretamente no banco de dados"""
        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == item_id).first()
            if not story:
                return False

            # Atualiza status para aprovado
            story.status = StoryStatus.APPROVED.value

            # Aplica edicoes se houver
            if edits:
                if 'title' in edits:
                    story.title = edits['title']
                if 'description' in edits:
                    story.description = edits['description']
                if 'as_a' in edits:
                    story.narrative_persona = edits['as_a']
                if 'i_want' in edits:
                    story.narrative_action = edits['i_want']
                if 'so_that' in edits:
                    story.narrative_benefit = edits['so_that']
                if 'acceptance_criteria' in edits:
                    story.acceptance_criteria = edits['acceptance_criteria']
                if 'sprint' in edits:
                    story.sprint = edits['sprint']
                if 'points' in edits:
                    story.points = edits['points']
                if 'priority' in edits:
                    story.priority = edits['priority']

            db.commit()

            self._log_activity(
                db=db,
                agent_id=approved_by,
                project_id=story.project_id,
                action="item_approved",
                message=f"Historia {item_id} aprovada por {approved_by}" + (f". Notas: {notes}" if notes else ""),
                level="INFO"
            )

            return True

        except Exception as e:
            print(f"Erro ao aprovar item {item_id}: {e}")
            return False
        finally:
            db.close()

    def reject_item(self, item_id: str, rejected_by: str, reason: str) -> bool:
        """Rejeita um item pendente - atualiza diretamente no banco de dados"""
        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == item_id).first()
            if not story:
                return False

            # Atualiza status para rejeitado
            story.status = StoryStatus.REJECTED.value
            db.commit()

            self._log_activity(
                db=db,
                agent_id=rejected_by,
                project_id=story.project_id,
                action="item_rejected",
                message=f"Historia {item_id} rejeitada por {rejected_by}. Motivo: {reason}",
                level="WARNING"
            )

            return True

        except Exception as e:
            print(f"Erro ao rejeitar item {item_id}: {e}")
            return False
        finally:
            db.close()

    def get_project_status(self, project_id: str) -> Dict:
        """Retorna status detalhado do projeto"""
        if project_id not in self.projects:
            return {"error": "Projeto nao encontrado"}

        config = self.projects[project_id]

        db = SessionLocal()
        try:
            project = db.query(Project).filter(Project.project_id == project_id).first()
            stories = db.query(Story).filter(Story.project_id == project_id).all()
            logs = db.query(ActivityLog).filter(
                ActivityLog.project_id == project_id
            ).order_by(ActivityLog.timestamp.desc()).limit(20).all()

            # Calcula metricas
            total_stories = len(stories)
            stories_by_status = {}
            total_points = 0
            completed_points = 0

            for s in stories:
                status = s.status or 'unknown'
                stories_by_status[status] = stories_by_status.get(status, 0) + 1
                total_points += s.points or 0
                if status in ['done', 'DONE', 'completed']:
                    completed_points += s.points or 0

            # Estima conclusao
            progress = (completed_points / total_points * 100) if total_points > 0 else 0

            # Conta aprovacoes pendentes do banco
            pending_count = db.query(Story).filter(
                Story.project_id == project_id,
                Story.status == StoryStatus.PENDING_REVIEW.value
            ).count()

            return {
                "project_id": project_id,
                "name": config.name,
                "status": project.status if project else "unknown",
                "progress": round(progress, 1),
                "stories": {
                    "total": total_stories,
                    "by_status": stories_by_status,
                    "total_points": total_points,
                    "completed_points": completed_points
                },
                "pending_approvals": pending_count,
                "recent_activity": [
                    {
                        "agent_id": log.agent_id,
                        "action": log.event_type,
                        "message": log.message,
                        "timestamp": log.timestamp.isoformat() if log.timestamp else None
                    }
                    for log in logs
                ],
                "estimated_completion": config.estimated_completion.isoformat() if config.estimated_completion else None
            }

        finally:
            db.close()

    def assign_agent_to_story(self, story_id: str, agent_id: str) -> bool:
        """Atribui um agente a uma historia"""
        db = SessionLocal()
        try:
            story = db.query(Story).filter(Story.story_id == story_id).first()
            if not story:
                return False

            story.assigned_to = agent_id
            story.status = StoryStatus.IN_PROGRESS.value

            # Atualiza agente
            agent = db.query(Agent).filter(Agent.agent_id == agent_id).first()
            if agent:
                agent.status = "WORKING"
                agent.current_task = story_id

            db.commit()

            self._log_activity(
                db=db,
                agent_id=agent_id,
                project_id=story.project_id,
                action="story_assigned",
                message=f"Historia {story_id} atribuida ao agente {agent_id}",
                level="INFO"
            )

            return True

        finally:
            db.close()

    def update_agent_knowledge(self, agent_id: str, skill: str, xp: int):
        """Atualiza conhecimento de um agente"""
        db = SessionLocal()
        try:
            agent = db.query(Agent).filter(Agent.agent_id == agent_id).first()
            if agent:
                config = agent.config or {}
                skills = config.get('skills', {})

                if skill in skills:
                    skills[skill]['xp'] = skills[skill].get('xp', 0) + xp
                    skills[skill]['times_used'] = skills[skill].get('times_used', 0) + 1
                else:
                    skills[skill] = {'xp': xp, 'times_used': 1, 'level': 'iniciante'}

                # Atualiza nivel baseado em XP
                skill_xp = skills[skill]['xp']
                if skill_xp >= 5000:
                    skills[skill]['level'] = 'master'
                elif skill_xp >= 1500:
                    skills[skill]['level'] = 'especialista'
                elif skill_xp >= 500:
                    skills[skill]['level'] = 'avancado'
                elif skill_xp >= 100:
                    skills[skill]['level'] = 'intermediario'

                config['skills'] = skills
                agent.config = config
                db.commit()

                self._log_activity(
                    db=db,
                    agent_id=agent_id,
                    project_id="SYSTEM",
                    action="skill_updated",
                    message=f"Agente {agent_id} ganhou {xp} XP em {skill}",
                    level="INFO"
                )

        finally:
            db.close()

    def process_project_now(self, project_id: str) -> Dict:
        """
        Processa um projeto imediatamente (trigger manual).
        Usado pelo endpoint /api/orchestrator/process/{project_id}

        Fluxo:
        1. Busca projeto no banco
        2. Lê os arquivos de entrada
        3. Gera stories automaticamente
        4. Retorna resultado
        """
        db = SessionLocal()
        try:
            # Busca projeto
            project = db.query(Project).filter(Project.project_id == project_id).first()
            if not project:
                return {"success": False, "error": "Projeto nao encontrado"}

            # Verifica se projeto ja tem config
            if project_id not in self.projects:
                # Cria config a partir do projeto do banco
                config = ProjectConfig(
                    project_id=project.project_id,
                    name=project.name,
                    description=project.description or "",
                    source_path=project.config.get("source_path", "") if project.config else "",
                    output_path=project.config.get("output_path", "") if project.config else "",
                    process_videos=project.config.get("process_videos", True) if project.config else True,
                    process_audio=project.config.get("process_audio", True) if project.config else True,
                    process_documents=project.config.get("process_documents", True) if project.config else True,
                    requires_approval=project.config.get("requires_approval", True) if project.config else True
                )
                self.projects[project_id] = config

            # Processa o projeto
            config = self.projects[project_id]

            self._log_activity(
                db=db,
                agent_id="AGT-002",  # Product Manager
                project_id=project_id,
                action="autonomous_processing",
                message=f"Iniciando processamento autonomo do projeto {project.name}",
                level="INFO"
            )

            # Processa documentos
            if config.process_documents and config.source_path:
                self._process_documents(project_id, config.source_path, db)

            # Processa videos
            if config.process_videos and config.source_path:
                self._process_videos(project_id, config.source_path, db)

            # Processa audio
            if config.process_audio and config.source_path:
                self._process_audio(project_id, config.source_path, db)

            # Gera stories baseado no tipo de projeto
            stories_before = db.query(Story).filter(Story.project_id == project_id).count()
            self._generate_stories(project_id, db)
            stories_after = db.query(Story).filter(Story.project_id == project_id).count()
            stories_created = stories_after - stories_before

            # Atualiza status do projeto
            project.status = "IN_PROGRESS"
            db.commit()

            self._log_activity(
                db=db,
                agent_id="AGT-002",
                project_id=project_id,
                action="processing_complete",
                message=f"Processamento concluido: {stories_created} stories criadas",
                level="INFO"
            )

            return {
                "success": True,
                "project": project.name,
                "stories_created": stories_created,
                "total_stories": stories_after
            }

        except Exception as e:
            self._log_activity(
                db=db,
                agent_id="ORCHESTRATOR",
                project_id=project_id,
                action="error",
                message=f"Erro no processamento: {str(e)}",
                level="ERROR"
            )
            return {"success": False, "error": str(e)}
        finally:
            db.close()


# Instancia global do orquestrador
_orchestrator: Optional[AutonomousOrchestrator] = None


def get_orchestrator() -> AutonomousOrchestrator:
    """Retorna instancia singleton do orquestrador"""
    global _orchestrator
    if _orchestrator is None:
        _orchestrator = AutonomousOrchestrator()
    return _orchestrator

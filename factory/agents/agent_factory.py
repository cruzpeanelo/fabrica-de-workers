"""
Agent Factory - Plataforma E Especializados
=================================================

Factory para criar e gerenciar agentes especializados com:
- Acionamento autonomo de skills
- Conhecimento profundo do dominio
- Integracao com sistema de aprendizado
"""

import sqlite3
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Type

from .core.autonomous_agent import (
    AutonomousAgent,
    AgentCapability,
    TaskContext,
    TaskResult,
    AgentState
)
from .specialized_agents import (
    AgentSpecialization,
    TechnologyDomain,
    ALL_SPECIALIZED_AGENTS,
    get_agents_by_domain,
    get_agent_by_id,
    search_agents
)
from .skills import (
    SkillTrigger,
    SkillTriggerContext,
    analyze_file,
    can_analyze
)
from .knowledge.knowledge_base import KnowledgeBase, KnowledgeType


class SpecializedAgent(AutonomousAgent):
    """
    Agente Especializado com Acionamento Autonomo de Skills

    Extende o AutonomousAgent com:
    - Skills de analise multimidia automaticas
    - Conhecimento profundo da especializacao
    - Tecnologias e certificacoes especificas
    """

    def __init__(self,
                 specialization: AgentSpecialization,
                 knowledge_base: Optional[KnowledgeBase] = None):
        """
        Inicializa agente especializado

        Args:
            specialization: Configuracao da especializacao
            knowledge_base: Base de conhecimento compartilhada
        """
        # Converte capabilities da especializacao
        capabilities = [
            AgentCapability(
                name=skill,
                description=f"Habilidade em {skill}",
                required_skills=[skill]
            )
            for skill in specialization.skills
        ]

        super().__init__(
            agent_id=specialization.agent_id,
            name=specialization.name,
            domain=specialization.domain.value,
            description=specialization.description,
            capabilities=capabilities,
            knowledge_base=knowledge_base
        )

        self.specialization = specialization
        self.skill_trigger = SkillTrigger(agent_id=specialization.agent_id)

        # Carrega conhecimento especializado
        self._load_specialized_knowledge()

    def _load_specialized_knowledge(self):
        """Carrega conhecimento especializado do dominio"""
        # Adiciona tecnologias como conhecimento
        for tech in self.specialization.technologies:
            self.knowledge.add(
                content=f"Tecnologia dominada: {tech}",
                knowledge_type=KnowledgeType.DOMAIN,
                source=f"specialization_{self.agent_id}",
                agent_id=self.agent_id,
                tags=[self.domain, tech.lower(), "technology"]
            )

        # Adiciona skills como conhecimento procedural
        for skill in self.specialization.skills:
            self.knowledge.add(
                content=f"Habilidade: {skill}",
                knowledge_type=KnowledgeType.BEST_PRACTICE,
                source=f"specialization_{self.agent_id}",
                agent_id=self.agent_id,
                tags=[self.domain, "skill"]
            )

            # Registra no sistema de skills
            self.skills.acquire_skill(
                name=skill[:50],  # Limita tamanho
                description=skill,
                category="domain",
                initial_proficiency=0.7 if self.specialization.experience_level == "expert" else 0.5
            )

        # Adiciona areas de conhecimento
        for area in self.specialization.knowledge_areas:
            self.knowledge.add(
                content=f"Area de conhecimento: {area}",
                knowledge_type=KnowledgeType.DOMAIN,
                source=f"specialization_{self.agent_id}",
                agent_id=self.agent_id,
                tags=[self.domain, "knowledge_area"]
            )

        # Adiciona certificacoes
        for cert in self.specialization.certifications:
            self.knowledge.add(
                content=f"Certificacao: {cert}",
                knowledge_type=KnowledgeType.DOCUMENTATION,
                source=f"specialization_{self.agent_id}",
                agent_id=self.agent_id,
                tags=[self.domain, "certification"]
            )

    def _think(self, task: TaskContext) -> Dict:
        """
        Fase de pensamento com analise automatica de skills

        Extende a fase de pensamento para incluir:
        - Analise automatica de arquivos mencionados
        - Acionamento de skills relevantes
        - Recomendacoes baseadas em analise
        """
        # Pensamento base
        context = super()._think(task)

        # Extrai arquivos da tarefa
        files = self._extract_files_from_task(task)

        # Cria contexto para skill trigger
        trigger_context = SkillTriggerContext(
            task_description=task.description,
            files_involved=files,
            domain=self.domain,
            keywords=self._extract_keywords(task.description)
        )

        # Analisa quais skills sao relevantes
        recommended_skills = self.skill_trigger.analyze_context(trigger_context)
        context['recommended_skills'] = recommended_skills

        # Se tem arquivos, analisa automaticamente
        if files:
            trigger_result = self.skill_trigger.trigger_skills(trigger_context)

            context['skill_analysis'] = {
                'skills_triggered': trigger_result.skills_triggered,
                'recommendations': trigger_result.recommendations,
                'duration_ms': trigger_result.duration_ms
            }

            # Adiciona resumos das analises ao contexto
            for file_path, result in trigger_result.analysis_results.items():
                if result.success and result.content_summary:
                    context[f'file_summary_{Path(file_path).name}'] = result.content_summary

                    # Adiciona notas na memoria de trabalho
                    self.working_memory.add_note(
                        f"Analise de {file_path}: {result.content_summary}"
                    )

            # Adiciona recomendacoes
            for rec in trigger_result.recommendations:
                self.working_memory.add_note(f"Recomendacao: {rec}")

            # Atualiza proficiencia das skills usadas
            self.skill_trigger.update_skill_proficiency(
                self.skills,
                trigger_result,
                success=True
            )

        return context

    def _extract_files_from_task(self, task: TaskContext) -> List[str]:
        """Extrai caminhos de arquivos da tarefa"""
        import re
        files = []

        # Padroes de caminhos
        patterns = [
            r'[A-Za-z]:\\[^\s"\']+\.[a-zA-Z0-9]+',  # Windows
            r'/[^\s"\']+\.[a-zA-Z0-9]+',             # Unix
            r'[a-zA-Z0-9_/.-]+\.[a-zA-Z0-9]+',       # Relativo
        ]

        for pattern in patterns:
            matches = re.findall(pattern, task.description)
            for match in matches:
                path = Path(match)
                if path.exists() and can_analyze(str(path)):
                    files.append(str(path))

        # Arquivos em metadata
        if task.metadata and 'files' in task.metadata:
            for f in task.metadata['files']:
                if Path(f).exists() and can_analyze(f):
                    files.append(f)

        return list(set(files))

    def _extract_keywords(self, text: str) -> List[str]:
        """Extrai palavras-chave relevantes"""
        import re
        # Palavras relevantes para skills
        relevant_patterns = [
            r'\b(analise|analyze|analysis)\b',
            r'\b(video|audio|texto|text|pdf|documento|document)\b',
            r'\b(dados|data|json|csv|excel)\b',
            r'\b(codigo|code|script|python|javascript)\b',
        ]

        keywords = []
        text_lower = text.lower()

        for pattern in relevant_patterns:
            if re.search(pattern, text_lower):
                keywords.append(pattern.strip(r'\b()'))

        return keywords

    def analyze_file(self, file_path: str) -> Dict:
        """
        Analisa um arquivo usando skills apropriadas

        Args:
            file_path: Caminho do arquivo

        Returns:
            Dict com resultado da analise
        """
        if not can_analyze(file_path):
            return {
                "success": False,
                "error": f"Formato de arquivo nao suportado: {file_path}"
            }

        result = analyze_file(file_path, agent_id=self.agent_id)

        # Atualiza proficiencia
        if result.success:
            skill_name = self._get_skill_for_file(file_path)
            if skill_name:
                self.skills.practice_skill(skill_name, success=True, xp_gain=15)

        return result.to_dict() if hasattr(result, 'to_dict') else {
            "success": result.success,
            "content_summary": result.content_summary,
            "metadata": result.metadata,
            "stats": result.stats
        }

    def _get_skill_for_file(self, file_path: str) -> Optional[str]:
        """Retorna skill apropriada para o arquivo"""
        from .skills.multimedia_base import MediaFormat

        fmt = MediaFormat.from_path(Path(file_path))
        if not fmt:
            return None

        format_skill_map = {
            'txt': 'text_analysis',
            'md': 'text_analysis',
            'pdf': 'pdf_analysis',
            'docx': 'document_analysis',
            'html': 'document_analysis',
            'xml': 'document_analysis',
            'py': 'code_analysis',
            'js': 'code_analysis',
            'ts': 'code_analysis',
            'json': 'data_analysis',
            'csv': 'data_analysis',
            'xlsx': 'data_analysis',
            'mp3': 'audio_analysis',
            'wav': 'audio_analysis',
            'mp4': 'video_analysis',
            'avi': 'video_analysis',
            'mkv': 'video_analysis',
        }

        return format_skill_map.get(fmt.extension)

    def get_capabilities_summary(self) -> Dict:
        """Retorna resumo das capacidades do agente"""
        return {
            "agent_id": self.agent_id,
            "name": self.name,
            "domain": self.domain,
            "specialization": self.specialization.name,
            "technologies": self.specialization.technologies,
            "skills": self.specialization.skills,
            "knowledge_areas": self.specialization.knowledge_areas,
            "certifications": self.specialization.certifications,
            "experience_level": self.specialization.experience_level,
            "multimedia_skills": [
                s.name for s in self.skills.get_all_skills()
                if s.category == "multimedia"
            ]
        }


class AgentFactory:
    """
    Plataforma E Especializados

    Cria e gerencia agentes especializados de diferentes dominios.
    """

    def __init__(self, db_path: Optional[Path] = None):
        self.db_path = db_path or Path("factory/database/agents.db")
        self._agents: Dict[str, SpecializedAgent] = {}
        self._shared_knowledge = KnowledgeBase()
        self._init_database()

    def _init_database(self):
        """Inicializa banco de dados"""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)

        conn = sqlite3.connect(self.db_path)

        conn.execute("""
            CREATE TABLE IF NOT EXISTS agent_registry (
                agent_id TEXT PRIMARY KEY,
                name TEXT NOT NULL,
                domain TEXT NOT NULL,
                specialization TEXT,
                created_at TEXT,
                last_active TEXT,
                task_count INTEGER DEFAULT 0,
                success_rate REAL DEFAULT 0.0
            )
        """)

        conn.execute("""
            CREATE TABLE IF NOT EXISTS agent_tasks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                agent_id TEXT NOT NULL,
                task_id TEXT NOT NULL,
                description TEXT,
                success INTEGER,
                duration_seconds REAL,
                created_at TEXT,
                skills_used TEXT
            )
        """)

        conn.commit()
        conn.close()

    def create_agent(self, agent_id: str) -> Optional[SpecializedAgent]:
        """
        Cria agente por ID

        Args:
            agent_id: ID do agente (ex: "BE-01", "SAP-ECC-FI")

        Returns:
            SpecializedAgent ou None se nao encontrado
        """
        spec = get_agent_by_id(agent_id)
        if not spec:
            return None

        agent = SpecializedAgent(
            specialization=spec,
            knowledge_base=self._shared_knowledge
        )

        self._agents[agent_id] = agent
        self._register_agent(agent)

        return agent

    def create_agents_by_domain(self, domain: TechnologyDomain) -> List[SpecializedAgent]:
        """
        Cria todos os agentes de um dominio

        Args:
            domain: Dominio de tecnologia

        Returns:
            Lista de agentes criados
        """
        specs = get_agents_by_domain(domain)
        agents = []

        for spec in specs:
            agent = SpecializedAgent(
                specialization=spec,
                knowledge_base=self._shared_knowledge
            )
            self._agents[spec.agent_id] = agent
            self._register_agent(agent)
            agents.append(agent)

        return agents

    def create_all_agents(self) -> List[SpecializedAgent]:
        """Cria todos os agentes especializados"""
        agents = []

        for spec in ALL_SPECIALIZED_AGENTS:
            agent = SpecializedAgent(
                specialization=spec,
                knowledge_base=self._shared_knowledge
            )
            self._agents[spec.agent_id] = agent
            self._register_agent(agent)
            agents.append(agent)

        return agents

    def _register_agent(self, agent: SpecializedAgent):
        """Registra agente no banco"""
        conn = sqlite3.connect(self.db_path)

        conn.execute("""
            INSERT OR REPLACE INTO agent_registry
            (agent_id, name, domain, specialization, created_at, last_active)
            VALUES (?, ?, ?, ?, ?, ?)
        """, (
            agent.agent_id,
            agent.name,
            agent.domain,
            agent.specialization.name,
            datetime.now().isoformat(),
            datetime.now().isoformat()
        ))

        conn.commit()
        conn.close()

    def get_agent(self, agent_id: str) -> Optional[SpecializedAgent]:
        """Retorna agente pelo ID"""
        if agent_id not in self._agents:
            return self.create_agent(agent_id)
        return self._agents.get(agent_id)

    def search_agents(self, keyword: str) -> List[AgentSpecialization]:
        """Busca agentes por palavra-chave"""
        return search_agents(keyword)

    def select_best_agent(self, task_description: str) -> Optional[SpecializedAgent]:
        """
        Seleciona o melhor agente para uma tarefa

        Args:
            task_description: Descricao da tarefa

        Returns:
            Melhor agente ou None
        """
        # Busca agentes relevantes
        results = search_agents(task_description)

        if not results:
            # Tenta buscar por palavras individuais
            words = task_description.lower().split()
            for word in words:
                if len(word) > 3:
                    results = search_agents(word)
                    if results:
                        break

        if not results:
            return None

        # Retorna o primeiro (mais relevante)
        best_spec = results[0]
        return self.get_agent(best_spec.agent_id)

    def list_agents(self) -> List[Dict]:
        """Lista todos os agentes registrados"""
        conn = sqlite3.connect(self.db_path)

        cursor = conn.execute("""
            SELECT agent_id, name, domain, specialization, task_count, success_rate
            FROM agent_registry
            ORDER BY domain, name
        """)

        agents = []
        for row in cursor.fetchall():
            agents.append({
                "agent_id": row[0],
                "name": row[1],
                "domain": row[2],
                "specialization": row[3],
                "task_count": row[4],
                "success_rate": row[5]
            })

        conn.close()
        return agents

    def get_statistics(self) -> Dict:
        """Retorna estatisticas da fabrica"""
        conn = sqlite3.connect(self.db_path)

        # Agentes por dominio
        cursor = conn.execute("""
            SELECT domain, COUNT(*) FROM agent_registry GROUP BY domain
        """)
        by_domain = dict(cursor.fetchall())

        # Total de tarefas
        cursor = conn.execute("SELECT COUNT(*), AVG(success) FROM agent_tasks")
        row = cursor.fetchone()
        total_tasks = row[0] or 0
        avg_success = row[1] or 0

        conn.close()

        return {
            "total_agents": len(ALL_SPECIALIZED_AGENTS),
            "active_agents": len(self._agents),
            "agents_by_domain": by_domain,
            "total_tasks_executed": total_tasks,
            "average_success_rate": round(avg_success * 100, 2) if avg_success else 0
        }

    def record_task(self, agent_id: str, task: TaskContext, result: TaskResult):
        """Registra execucao de tarefa"""
        conn = sqlite3.connect(self.db_path)

        conn.execute("""
            INSERT INTO agent_tasks
            (agent_id, task_id, description, success, duration_seconds, created_at, skills_used)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (
            agent_id,
            task.task_id,
            task.description,
            1 if result.success else 0,
            result.duration_seconds,
            datetime.now().isoformat(),
            ",".join(result.actions_taken)
        ))

        # Atualiza estatisticas do agente
        conn.execute("""
            UPDATE agent_registry
            SET task_count = task_count + 1,
                last_active = ?,
                success_rate = (
                    SELECT AVG(success) FROM agent_tasks WHERE agent_id = ?
                )
            WHERE agent_id = ?
        """, (datetime.now().isoformat(), agent_id, agent_id))

        conn.commit()
        conn.close()


# Instancia global da fabrica
_factory: Optional[AgentFactory] = None


def get_factory() -> AgentFactory:
    """Retorna instancia global da fabrica"""
    global _factory
    if _factory is None:
        _factory = AgentFactory()
    return _factory


def create_agent(agent_id: str) -> Optional[SpecializedAgent]:
    """Cria agente pelo ID"""
    return get_factory().create_agent(agent_id)


def select_agent(task_description: str) -> Optional[SpecializedAgent]:
    """Seleciona melhor agente para tarefa"""
    return get_factory().select_best_agent(task_description)


# ============================================================
# EXEMPLO DE USO
# ============================================================

USAGE_EXAMPLE = '''
# Criar agente especifico
from factory.agents.agent_factory import create_agent, select_agent

# Por ID
agent = create_agent("SAP-ECC-FI")
print(agent.get_capabilities_summary())

# Selecionar automaticamente
agent = select_agent("migrar dados do SAP ECC para S/4 HANA")
# Retorna: SAP-MIG-02 (Data Migration Expert)

# Executar tarefa com analise automatica de arquivos
from factory.agents.core import TaskContext

task = TaskContext(
    task_id="T001",
    description="Analisar planilha de custos em dados/custos.xlsx",
    metadata={"files": ["dados/custos.xlsx"]}
)

result = agent.execute_task(task)
# O agente automaticamente:
# 1. Detecta que precisa da skill "data_analysis"
# 2. Analisa o arquivo Excel
# 3. Adiciona insights na memoria de trabalho
# 4. Executa a tarefa com contexto enriquecido
'''


if __name__ == "__main__":
    print(USAGE_EXAMPLE)

    # Mostra estatisticas
    from .specialized_agents import get_statistics
    stats = get_statistics()
    print(f"\n{stats['total_agents']} agentes especializados disponiveis")
    print(f"Dominios: {list(stats['by_domain'].keys())}")

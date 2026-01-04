# Referência da API - Plataforma E

## Índice

1. [Agente Autônomo](#agente-autônomo)
2. [Base de Conhecimento](#base-de-conhecimento)
3. [Sistema de Memória](#sistema-de-memória)
4. [Motor de Aprendizado](#motor-de-aprendizado)
5. [Sistema de Skills](#sistema-de-skills)
6. [Runtime de Agentes](#runtime-de-agentes)
7. [API REST do Dashboard](#api-rest-do-dashboard)

---

## Agente Autônomo

### `AutonomousAgent`

Classe principal que representa um agente autônomo com capacidade de aprendizado.

#### Inicialização

```python
from factory.agents.core import AutonomousAgent

agent = AutonomousAgent(
    agent_id: str,           # ID único do agente (ex: "08")
    name: str,               # Nome descritivo (ex: "Backend Developer")
    domain: str,             # Domínio de atuação (ex: "backend")
    description: str,        # Descrição do papel
    capabilities: List[AgentCapability] = None,  # Capacidades (opcional)
    knowledge_base: KnowledgeBase = None         # Base compartilhada (opcional)
)
```

#### Propriedades

| Propriedade | Tipo | Descrição |
|-------------|------|-----------|
| `agent_id` | str | Identificador único |
| `name` | str | Nome do agente |
| `domain` | str | Domínio de especialização |
| `state` | AgentState | Estado atual (READY, EXECUTING, etc) |
| `knowledge` | KnowledgeBase | Base de conhecimento |
| `memory` | AgentMemory | Sistema de memória |
| `skills` | SkillAcquisition | Sistema de skills |

#### Métodos

##### `execute_task(task: TaskContext) -> TaskResult`

Executa uma tarefa de forma autônoma.

```python
from factory.agents.core import TaskContext

task = TaskContext(
    task_id="T001",
    description="Criar endpoint de login",
    project_id="PRJ-001",
    priority=5
)

result = agent.execute_task(task)

# Resultado
print(result.success)         # True/False
print(result.output)          # Saída da execução
print(result.files_modified)  # Lista de arquivos alterados
print(result.errors)          # Lista de erros (se houver)
print(result.actions_taken)   # Ações realizadas
print(result.duration_seconds) # Tempo de execução
```

##### `consult(question: str) -> Optional[str]`

Consulta o agente sobre um assunto.

```python
resposta = agent.consult("Como implementar paginação em FastAPI?")
# Retorna conhecimento relevante ou None
```

##### `get_status() -> Dict`

Retorna status completo do agente.

```python
status = agent.get_status()

# Estrutura do retorno:
{
    "agent_id": "08",
    "name": "Backend Developer",
    "domain": "backend",
    "state": "ready",
    "current_task": None,
    "skills": {
        "total_skills": 4,
        "avg_proficiency": 0.65,
        "strongest": ["FastAPI", "Python"]
    },
    "performance": {
        "total_tasks": 15,
        "success_rate": 85.5,
        "avg_score": 0.82
    },
    "memory_stats": {
        "memories": {"total": 45, "by_type": {...}},
        "decisions": {"total": 12, "avg_success_rate": 0.78}
    }
}
```

##### `get_wisdom() -> Dict`

Retorna sabedoria acumulada do agente.

```python
wisdom = agent.get_wisdom()

# Estrutura do retorno:
{
    "o_que_funciona": [
        "Usar Pydantic para validação",
        "Criar testes antes do código"
    ],
    "o_que_evitar": [
        "Queries sem índice",
        "Commit sem testes"
    ],
    "licoes_importantes": [
        "Validação de entrada é crítica",
        "Logs estruturados facilitam debug"
    ]
}
```

##### `share_knowledge(target_agent, topic: str)`

Compartilha conhecimento com outro agente.

```python
agent_08.share_knowledge(agent_09, "autenticação JWT")
```

##### `teach_skill(target_agent, skill_name: str) -> bool`

Ensina uma skill para outro agente.

```python
sucesso = agent_08.teach_skill(agent_09, "FastAPI")
# Retorna True se conseguiu ensinar
```

#### Callbacks

```python
# Registrar callback para mudança de estado
agent.on_state_change(lambda agent_id, old, new:
    print(f"Agente {agent_id}: {old} -> {new}"))

# Registrar callback para conclusão de tarefa
agent.on_task_complete(lambda agent_id, task, result:
    print(f"Tarefa {task.task_id} concluída: {result.success}"))
```

---

### `TaskContext`

Contexto de uma tarefa a ser executada.

```python
from factory.agents.core import TaskContext

task = TaskContext(
    task_id: str,                    # ID único da tarefa
    description: str,                 # Descrição do que fazer
    project_id: Optional[str] = None, # ID do projeto
    priority: int = 5,                # Prioridade (1-10)
    deadline: Optional[str] = None,   # Prazo (ISO format)
    dependencies: List[str] = [],     # IDs de tarefas dependentes
    metadata: Dict = {}               # Dados adicionais
)
```

### `TaskResult`

Resultado de uma tarefa executada.

```python
@dataclass
class TaskResult:
    task_id: str               # ID da tarefa
    success: bool              # Se teve sucesso
    output: Any                # Saída da execução
    files_modified: List[str]  # Arquivos alterados
    errors: List[str]          # Erros encontrados
    duration_seconds: float    # Tempo de execução
    actions_taken: List[str]   # Ações realizadas
```

### `AgentState`

Estados possíveis do agente.

```python
from factory.agents.core import AgentState

AgentState.INITIALIZING  # Inicializando
AgentState.READY         # Pronto para tarefas
AgentState.THINKING      # Analisando tarefa
AgentState.EXECUTING     # Executando
AgentState.LEARNING      # Aprendendo com resultado
AgentState.IDLE          # Aguardando
AgentState.ERROR         # Em erro
AgentState.STOPPED       # Parado
```

---

## Base de Conhecimento

### `KnowledgeBase`

Sistema de armazenamento e busca de conhecimento.

#### Inicialização

```python
from factory.agents.knowledge import KnowledgeBase

kb = KnowledgeBase(
    db_path: Path = None,           # Caminho do banco (default: factory/database/knowledge_base.db)
    embedding_engine: EmbeddingEngine = None  # Motor de embeddings (opcional)
)
```

#### Métodos

##### `add(content, knowledge_type, source, ...) -> KnowledgeItem`

Adiciona conhecimento à base.

```python
from factory.agents.knowledge import KnowledgeType

item = kb.add(
    content="FastAPI usa async/await para operações assíncronas",
    knowledge_type=KnowledgeType.DOCUMENTATION,
    source="docs/fastapi.md",
    agent_id="08",              # Agente que criou (opcional)
    project_id="PRJ-001",       # Projeto relacionado (opcional)
    tags=["fastapi", "async"],  # Tags para categorização
    metadata={"version": "0.100"}  # Dados extras
)

print(item.id)  # "K-a1b2c3d4e5f6"
```

##### `search(query, ...) -> List[SearchResult]`

Busca semântica na base.

```python
results = kb.search(
    query="como fazer validação de dados",
    knowledge_type=KnowledgeType.BEST_PRACTICE,  # Filtro por tipo (opcional)
    agent_id="08",        # Filtro por agente (opcional)
    project_id="PRJ-001", # Filtro por projeto (opcional)
    tags=["validação"],   # Filtro por tags (opcional)
    limit=10,             # Máximo de resultados
    min_similarity=0.3    # Similaridade mínima
)

for result in results:
    print(f"Similaridade: {result.similarity:.2f}")
    print(f"Conteúdo: {result.item.content}")
```

##### `index_file(file_path, ...) -> List[KnowledgeItem]`

Indexa um arquivo na base.

```python
from pathlib import Path

items = kb.index_file(
    file_path=Path("docs/manual.md"),
    agent_id="08",
    project_id="PRJ-001"
)

print(f"Indexados {len(items)} chunks")
```

##### `get(item_id: str) -> Optional[KnowledgeItem]`

Busca item por ID.

```python
item = kb.get("K-a1b2c3d4e5f6")
```

##### `update(item_id, content, metadata) -> bool`

Atualiza item existente.

```python
kb.update("K-a1b2c3d4e5f6",
    content="Conteúdo atualizado",
    metadata={"version": "2.0"})
```

##### `delete(item_id: str) -> bool`

Remove item da base.

```python
kb.delete("K-a1b2c3d4e5f6")
```

##### `record_usage(knowledge_id, agent_id, was_useful, feedback)`

Registra uso de conhecimento (para aprendizado).

```python
kb.record_usage(
    knowledge_id="K-a1b2c3d4e5f6",
    agent_id="08",
    task_id="T001",
    was_useful=True,
    feedback="Ajudou a resolver o problema"
)
```

##### `get_stats() -> Dict`

Retorna estatísticas da base.

```python
stats = kb.get_stats()

# Estrutura:
{
    "total": 150,
    "by_type": {
        "documentation": 45,
        "code": 60,
        "pattern": 25,
        "error": 20
    },
    "by_agent": {"08": 80, "09": 70},
    "most_used": [("K-001", "FastAPI...", 15), ...]
}
```

### `KnowledgeType`

Tipos de conhecimento.

```python
from factory.agents.knowledge import KnowledgeType

KnowledgeType.DOCUMENTATION  # Documentação
KnowledgeType.CODE           # Código fonte
KnowledgeType.DECISION       # Decisões técnicas
KnowledgeType.PATTERN        # Padrões aprendidos
KnowledgeType.ERROR          # Erros e soluções
KnowledgeType.BEST_PRACTICE  # Boas práticas
KnowledgeType.DOMAIN         # Conhecimento de domínio
KnowledgeType.TASK           # Conhecimento de tarefas
```

---

## Sistema de Memória

### `AgentMemory`

Sistema de memória de longo prazo.

#### Inicialização

```python
from factory.agents.memory import AgentMemory

memory = AgentMemory(
    agent_id="08",
    db_path: Path = None  # Default: factory/database/memory_08.db
)
```

#### Métodos de Memória

##### `remember(content, memory_type, ...) -> MemoryEntry`

Armazena uma memória.

```python
from factory.agents.memory import MemoryType

entry = memory.remember(
    content="Implementei API de usuários com sucesso",
    memory_type=MemoryType.EPISODIC,
    context={"task_id": "T001"},
    importance=0.8,          # 0-1
    emotional_valence=0.5    # -1 (negativo) a 1 (positivo)
)
```

##### `recall(query, memory_type, ...) -> List[MemoryEntry]`

Recupera memórias.

```python
memories = memory.recall(
    query="API de usuários",
    memory_type=MemoryType.EPISODIC,
    limit=10,
    min_importance=0.3
)
```

##### `reinforce(memory_id, boost)`

Reforça uma memória (quando foi útil).

```python
memory.reinforce("MEM-abc123", boost=0.1)
```

##### `forget_unimportant(threshold, days_old) -> int`

Remove memórias antigas e pouco importantes.

```python
deleted = memory.forget_unimportant(
    threshold=0.1,   # Importância mínima
    days_old=30      # Mais velhas que 30 dias
)
```

#### Métodos de Decisão

##### `record_decision(...) -> Decision`

Registra uma decisão tomada.

```python
decision = memory.record_decision(
    context="Implementar autenticação",
    options=["JWT", "Session", "OAuth"],
    decision="JWT",
    reasoning="Melhor para API REST stateless",
    task_id="T001"
)
```

##### `record_decision_outcome(decision_id, outcome, success_rating)`

Registra resultado da decisão.

```python
memory.record_decision_outcome(
    decision_id="DEC-abc123",
    outcome="API funcionando corretamente",
    success_rating=0.9  # 0-1
)
```

##### `get_similar_decisions(context, limit) -> List[Decision]`

Busca decisões similares.

```python
decisions = memory.get_similar_decisions(
    context="implementar autenticação",
    limit=5
)

for d in decisions:
    print(f"Decisão: {d.decision_made} (sucesso: {d.success_rating})")
```

#### Métodos de Padrões

##### `learn_pattern(...) -> LearnedPattern`

Aprende um novo padrão.

```python
pattern = memory.learn_pattern(
    pattern_type="success",  # ou "failure", "optimization"
    trigger="criar api rest",
    action="usar FastAPI com Pydantic",
    expected_outcome="API funcional",
    confidence=0.7
)
```

##### `get_applicable_patterns(situation, pattern_type) -> List[LearnedPattern]`

Busca padrões aplicáveis.

```python
patterns = memory.get_applicable_patterns(
    situation="criar endpoint de login",
    pattern_type="success"
)
```

##### `update_pattern_outcome(pattern_id, was_successful)`

Atualiza resultado de uso do padrão.

```python
memory.update_pattern_outcome("PAT-abc123", was_successful=True)
# Aumenta confiança do padrão
```

### `MemoryType`

Tipos de memória.

```python
from factory.agents.memory import MemoryType

MemoryType.EPISODIC    # Eventos e experiências
MemoryType.SEMANTIC    # Conhecimento factual
MemoryType.PROCEDURAL  # Como fazer coisas
MemoryType.WORKING     # Contexto atual
```

### `WorkingMemory`

Memória de trabalho (sessão atual).

```python
from factory.agents.memory import WorkingMemory

wm = WorkingMemory(capacity=50)

# Definir tarefa atual
wm.set_task("T001", "Criar API", "PRJ-001")

# Registrar ações
wm.record_file_change("api/users.py")
wm.record_decision("contexto", "decisão", "raciocínio")
wm.record_error("Erro de conexão")
wm.note("Lembrar de adicionar testes")

# Obter resumo
print(wm.get_summary())

# Exportar estado
state = wm.to_dict()
```

---

## Motor de Aprendizado

### `FeedbackSystem`

Sistema de feedback e avaliação.

```python
from factory.agents.learning import FeedbackSystem, FeedbackType, FeedbackResult

feedback = FeedbackSystem()

# Submeter feedback
fb = feedback.submit_feedback(
    task_id="T001",
    agent_id="08",
    feedback_type=FeedbackType.AUTO,
    result=FeedbackResult.SUCCESS,
    score=0.85,
    details="Tarefa concluída corretamente",
    suggestions=["Adicionar mais testes"]
)

# Avaliação automática
fb = feedback.auto_evaluate(
    task_id="T001",
    agent_id="08",
    task_result={"files_modified": ["api.py"], "errors": []},
    context={"area": "backend"}
)

# Obter performance
perf = feedback.get_agent_performance("08")
print(f"Taxa de sucesso: {perf['success_rate']}%")
print(f"Tendência: {perf['trend_direction']}")

# Sugestões de melhoria
suggestions = feedback.get_improvement_suggestions("08")
```

### `LearningEngine`

Motor de aprendizado.

```python
from factory.agents.learning import LearningEngine

learning = LearningEngine("08")

# Aprender com tarefa
insights = learning.learn_from_task(
    task_id="T001",
    task_description="Criar API de usuários",
    actions_taken=["Criar modelo", "Criar router"],
    result={"success": True},
    success=True
)

# Analisar padrões
patterns = learning.analyze_patterns()
print(f"Taxa de sucesso: {patterns['taxa_sucesso']:.0%}")
print(f"Ações efetivas: {patterns['acoes_efetivas']}")

# Obter recomendação
rec = learning.get_recommendation("criar api de pagamentos")

# Consolidar aprendizado
summary = learning.consolidate_learning()
```

---

## Sistema de Skills

### `SkillAcquisition`

Sistema de aquisição e evolução de skills.

```python
from factory.agents.learning import SkillAcquisition

skills = SkillAcquisition("08")

# Adquirir skill
skill = skills.acquire_skill(
    name="FastAPI",
    description="Desenvolvimento de APIs",
    category="technical",
    initial_proficiency=0.5
)

# Praticar skill
skills.practice_skill("FastAPI", success=True, xp_gain=20)

# Ver skill
skill = skills.get_skill("FastAPI")
print(f"Proficiência: {skill.proficiency:.0%}")
print(f"Nível: {skills.get_proficiency_level(skill.proficiency)}")

# Verificar se pode ensinar
if skills.can_teach("FastAPI"):
    skills.teach_skill("FastAPI", "09")  # Ensina para agente 09

# Identificar gaps
gaps = skills.get_skill_gaps({
    "FastAPI": 0.7,
    "Docker": 0.5
})

# Resumo
summary = skills.get_skill_summary()
```

---

## Runtime de Agentes

### `AgentRuntime`

Gerenciador de múltiplos agentes.

```python
from factory.agents.core import AgentRuntime, AgentConfig

runtime = AgentRuntime(max_workers=4)

# Registrar agentes
runtime.register_agent(AgentConfig(
    agent_id="08",
    name="Backend Dev",
    domain="backend",
    description="Desenvolve APIs"
))

# Obter agente
agent = runtime.get_agent("08")

# Listar agentes
agents = runtime.list_agents()

# Selecionar melhor agente
best = runtime.select_agent("criar api REST", domain="backend")

# Executar tarefa
result = runtime.submit_task_sync("08", task)

# Ou async
result = await runtime.submit_task("08", task)

# Compartilhar conhecimento
runtime.broadcast_knowledge("autenticação", source_agent_id="08")

# Sabedoria coletiva
wisdom = runtime.get_collective_wisdom()

# Estatísticas
stats = runtime.get_runtime_stats()
```

---

## API REST do Dashboard

### Endpoints

#### Status
```
GET /api/status

Retorna:
{
    "factory": {"name": "...", "version": "2.0.0", "status": "running"},
    "projects": {"total": 1, "by_status": {...}, "list": [...]},
    "agents": {"total": 19, "by_status": {...}, "list": [...]},
    "skills": {"total": 45},
    "recent_logs": [...],
    "timestamp": "2024-..."
}
```

#### Projetos
```
GET /api/projects                    # Listar
GET /api/projects/{project_id}       # Detalhe
POST /api/projects                   # Criar
PUT /api/projects/{project_id}       # Atualizar
DELETE /api/projects/{project_id}    # Remover
```

#### Stories
```
GET /api/stories                     # Listar
GET /api/stories?project_id=PRJ-001  # Filtrar por projeto
GET /api/stories/{story_id}          # Detalhe (com tasks e logs)
POST /api/stories                    # Criar
PUT /api/stories/{story_id}          # Atualizar
```

#### Agentes
```
GET /api/agents                      # Listar
GET /api/agents?domain=backend       # Filtrar por domínio
GET /api/agents/{agent_id}/activity  # Atividades do agente
PUT /api/agents/{agent_id}           # Atualizar
```

#### Skills
```
GET /api/skills                      # Listar
GET /api/skills?category=technical   # Filtrar por categoria
```

#### Logs
```
GET /api/logs                        # Listar
GET /api/logs?project_id=PRJ-001     # Filtrar por projeto
GET /api/logs?agent_id=08            # Filtrar por agente
GET /api/logs?level=ERROR            # Filtrar por nível
```

#### Métricas
```
GET /api/metrics                     # Geral
GET /api/metrics?project_id=PRJ-001  # Por projeto

Retorna:
{
    "stories": {"total": 84, "done": 70, "in_progress": 3, "completion_rate": 83.3},
    "points": {"total": 452, "done": 379, "velocity": 379},
    "agents": {"total": 19, "active": 5}
}
```

---

## Exemplos Completos

### Exemplo: Criar Sistema de Agentes para um Projeto

```python
from factory.agents.core import AgentRuntime, AgentConfig, TaskContext
from factory.agents.knowledge import KnowledgeBase, KnowledgeType
from pathlib import Path

# 1. Criar base de conhecimento compartilhada
kb = KnowledgeBase()

# 2. Indexar documentação do projeto
for doc in Path("docs").glob("*.md"):
    kb.index_file(doc)

# 3. Adicionar conhecimento específico
kb.add(
    content="Usamos PostgreSQL em produção com connection pooling",
    knowledge_type=KnowledgeType.DOMAIN,
    source="arquitetura",
    tags=["database", "produção"]
)

# 4. Criar runtime com base compartilhada
runtime = AgentRuntime(max_workers=4, shared_knowledge=kb)

# 5. Registrar agentes
agents_config = [
    AgentConfig("08", "Backend", "backend", "APIs"),
    AgentConfig("09", "Frontend", "frontend", "UI"),
    AgentConfig("07", "DBA", "database", "Banco"),
    AgentConfig("15", "QA", "testing", "Testes"),
]

for config in agents_config:
    runtime.register_agent(config)

# 6. Executar tarefas
tasks = [
    TaskContext("T001", "Criar modelo de usuário", "PRJ-001"),
    TaskContext("T002", "Criar API de usuário", "PRJ-001"),
    TaskContext("T003", "Criar tela de cadastro", "PRJ-001"),
]

for task in tasks:
    # Seleciona melhor agente
    agent_id = runtime.select_agent(task.description)

    # Executa
    result = runtime.submit_task_sync(agent_id, task)

    print(f"Tarefa {task.task_id}: {'OK' if result.success else 'FALHA'}")

# 7. Ver sabedoria coletiva
print(runtime.get_collective_wisdom())
```

---

*Para mais exemplos, veja `factory/agents/test_autonomous_agents.py`*

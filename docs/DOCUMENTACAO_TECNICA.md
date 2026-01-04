# Plataforma E - Documentacao Tecnica

## Arquitetura do Sistema

### Visao Geral

```
+------------------+     +------------------+     +------------------+
|    Dashboard     |     |   Profile        |     |   Corporate      |
|    (FastAPI)     |<--->|   Service        |<--->|   Hierarchy      |
|    Port 9000     |     |                  |     |                  |
+------------------+     +------------------+     +------------------+
         |                        |                        |
         v                        v                        v
+------------------+     +------------------+     +------------------+
|    Database      |     |   Hierarchy      |     |   Autonomous     |
|    (SQLite)      |     |   Integration    |     |   Agent Core     |
+------------------+     +------------------+     +------------------+
```

### Componentes Principais

| Componente | Arquivo | Responsabilidade |
|------------|---------|------------------|
| Dashboard | `factory/dashboard/dashboard_v3.py` | Interface web, APIs REST |
| Profile Service | `factory/agents/profile_service.py` | Gestao de perfis de agentes |
| Agent Profile | `factory/agents/agent_profile.py` | Modelo de perfil com skills |
| Corporate Hierarchy | `factory/agents/corporate_hierarchy.py` | Estrutura hierarquica |
| Hierarchy Integration | `factory/agents/core/hierarchy_integration.py` | Integracao e timeouts |
| Database Models | `factory/database/models.py` | Modelos SQLAlchemy |

---

## Estrutura de Diretorios

```
factory/
├── agents/
│   ├── core/
│   │   ├── __init__.py
│   │   ├── autonomous_agent.py      # Agente autonomo base
│   │   ├── agent_runtime.py         # Runtime de execucao
│   │   ├── task_executor.py         # Executor de tarefas
│   │   └── hierarchy_integration.py # Sistema de timeout
│   ├── corporate_hierarchy.py       # 134 agentes corporativos
│   ├── agent_profile.py             # Sistema de perfis
│   └── profile_service.py           # Servico de perfis
├── dashboard/
│   ├── app.py                       # Dashboard v2 (original)
│   └── dashboard_v3.py              # Dashboard v3 (novo)
├── database/
│   ├── connection.py                # Conexao SQLite
│   ├── models.py                    # Modelos de dados
│   └── repositories.py              # Repositorios
├── config.py                        # Configuracoes
└── __init__.py
```

---

## Modelos de Dados

### AgentProfile

```python
@dataclass
class AgentProfile:
    agent_id: str
    name: str
    title: str
    department: str
    area: str  # "business" | "technology"
    level: int  # 1-10

    # Habilidades
    skills: Dict[str, AgentSkill]

    # Experiencias
    experiences: List[Experience]

    # Metricas
    total_projects: int
    total_tasks_completed: int
    total_hours_worked: float
    success_rate: float

    # Decisoes (para decisores)
    is_decision_maker: bool
    approval_timeout_hours: float  # Padrao: 1.0
    decisions_made: int

    # Conquistas
    achievements: List[Dict]
```

### AgentSkill

```python
@dataclass
class AgentSkill:
    skill_id: str
    name: str
    category: str  # "technical" | "behavioral" | "domain"
    level: SkillLevel  # INICIANTE -> MASTER
    experience_points: int
    times_used: int
    last_used: Optional[datetime]
```

### Experience

```python
@dataclass
class Experience:
    experience_id: str
    experience_type: ExperienceType
    title: str
    description: str
    date: datetime

    # Contexto
    project_name: Optional[str]
    outcome: str  # "success" | "partial" | "failed"
    impact: str   # "low" | "medium" | "high" | "critical"

    # Skills
    skills_used: List[str]
    skills_gained: List[str]

    # Metricas
    duration_hours: float
    complexity: int  # 1-10
```

### HierarchyConfig

```python
@dataclass
class HierarchyConfig:
    corporate_id: Optional[str]
    budget_limit: float
    can_approve_tasks: bool
    can_assign_tasks: bool
    auto_escalate_on_error: bool
    notify_superior_on_complete: bool

    # Timeout
    approval_timeout_hours: float = 1.0
    auto_approve_on_timeout: bool = True
    work_hours: WorkHoursConfig
```

### WorkHoursConfig

```python
@dataclass
class WorkHoursConfig:
    timezone: str = "America/Sao_Paulo"
    start_hour: int = 8
    end_hour: int = 18
    work_days: List[int] = [0, 1, 2, 3, 4]  # Segunda-Sexta
```

---

## APIs REST

### Profiles

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/profiles` | Lista todos os perfis |
| GET | `/api/profiles/{agent_id}` | Perfil detalhado |
| PUT | `/api/profiles/{agent_id}/timeout` | Atualiza timeout |
| GET | `/api/profiles/decision-makers` | Lista decisores |
| GET | `/api/profiles/org-chart` | Dados do organograma |
| GET | `/api/profiles/top-performers` | Top performers |

### Agents

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/agents` | Lista agentes |
| GET | `/api/agents/{agent_id}/activity` | Atividades do agente |
| PUT | `/api/agents/{agent_id}` | Atualiza agente |

### Projects & Stories

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/projects` | Lista projetos |
| GET | `/api/stories` | Lista stories |
| GET | `/api/status` | Status geral |

### Hierarchy

| Metodo | Endpoint | Descricao |
|--------|----------|-----------|
| GET | `/api/hierarchy` | Hierarquia corporativa |
| GET | `/api/productivity` | Metricas de produtividade |

---

## Sistema de Timeout

### Fluxo de Implementacao

```python
# 1. Configuracao
config = HierarchyConfig(
    corporate_id='DEV-SR-BACK',
    approval_timeout_hours=1.0,
    auto_approve_on_timeout=True,
    work_hours=WorkHoursConfig(
        timezone="America/Sao_Paulo",
        start_hour=8,
        end_hour=18,
        work_days=[0, 1, 2, 3, 4]
    )
)

# 2. Integracao
hierarchy = integrate_hierarchy(agent, corporate_id='DEV-SR-BACK')

# 3. Solicitar aprovacao
decision = hierarchy.request_approval(
    action="modify_database",
    description="Alterar schema",
    estimated_cost=500
)

# 4. Verificar autonomia
autonomy = hierarchy.can_proceed_autonomously(decision.decision_id)
if autonomy['can_proceed']:
    # Executar acao
    pass
```

### Metodos Principais

```python
class HierarchyIntegration:
    def is_work_hours(self) -> bool:
        """Verifica se esta dentro do expediente"""

    def get_brazil_time(self) -> datetime:
        """Retorna hora atual no Brasil (UTC-3)"""

    def calculate_timeout(self) -> datetime:
        """Calcula quando a solicitacao expira"""

    def request_approval(self, action, description, estimated_cost) -> HierarchicalDecision:
        """Solicita aprovacao ao superior"""

    def can_proceed_autonomously(self, decision_id) -> Dict:
        """Verifica se pode prosseguir apos timeout"""

    def check_pending_timeouts(self) -> List[HierarchicalDecision]:
        """Auto-aprova decisoes expiradas"""
```

---

## Sistema de Skills

### Evolucao Automatica

```python
# XP -> Nivel
thresholds = {
    100: SkillLevel.INTERMEDIARIO,
    500: SkillLevel.AVANCADO,
    1500: SkillLevel.ESPECIALISTA,
    5000: SkillLevel.MASTER
}

def add_experience(self, points: int = 10):
    self.experience_points += points
    self.times_used += 1

    # Evolui automaticamente
    for xp_threshold, new_level in thresholds.items():
        if self.experience_points >= xp_threshold:
            self.level = new_level
```

### Calculo de Confiabilidade

```python
def calculate_reliability_score(self) -> float:
    # Base: taxa de sucesso
    success_count = sum(1 for e in experiences if e.outcome == "success")
    base_score = (success_count / len(experiences)) * 100

    # Bonus por experiencia
    experience_bonus = min(10, total_projects * 0.5)

    # Bonus por skills avancadas
    advanced_skills = sum(1 for s in skills if s.level >= 3)
    skill_bonus = min(10, advanced_skills * 2)

    return min(100, base_score + experience_bonus + skill_bonus)
```

---

## Frontend (Vue.js)

### Componentes Principais

```javascript
// Estado
const profiles = ref([]);           // Lista de perfis
const orgChart = ref(null);         // Dados do organograma
const selectedProfile = ref(null);  // Perfil selecionado
const selectedArea = ref('all');    // Filtro de area
const timeoutInput = ref(1);        // Input de timeout

// Metodos
const fetchProfiles = async () => { /* ... */ };
const fetchOrgChart = async () => { /* ... */ };
const selectAgent = async (agentId) => { /* ... */ };
const updateTimeout = async () => { /* ... */ };
```

### Estilos Customizados

```css
/* Cores por area */
.gradient-business { background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%); }
.gradient-tech { background: linear-gradient(135deg, #06b6d4 0%, #0891b2 100%); }

/* Animacoes */
.org-node:hover { transform: translateY(-2px); }
.skill-bar { transition: width 0.5s ease-out; }
.fade-in { animation: fadeIn 0.3s ease-out; }
```

---

## Configuracao e Deploy

### Requisitos

```txt
python >= 3.9
fastapi >= 0.100.0
uvicorn >= 0.22.0
sqlalchemy >= 2.0.0
pydantic >= 2.0.0
python-jose >= 3.3.0
passlib >= 1.7.4
pytz >= 2023.3 (opcional)
```

### Variaveis de Ambiente

```bash
DASHBOARD_HOST=0.0.0.0
DASHBOARD_PORT=9000
DATABASE_URL=sqlite:///factory.db
SECRET_KEY=your-secret-key
```

### Iniciar Dashboard

```bash
# Opcao 1: Modulo Python
python -m factory.dashboard.dashboard_v3

# Opcao 2: Uvicorn direto
uvicorn factory.dashboard.dashboard_v3:app --host 0.0.0.0 --port 9000 --reload

# Opcao 3: Script
cd factory/dashboard && python dashboard_v3.py
```

---

## Testes

### Teste de Timeout

```python
# test_timeout_system.py
from factory.agents.core import (
    AutonomousAgent, HierarchyIntegration, HierarchyConfig,
    WorkHoursConfig, integrate_hierarchy
)

# Criar agente
agent = AutonomousAgent(
    agent_id='TEST',
    name='Test Agent',
    domain='backend'
)

# Configurar
config = HierarchyConfig(
    corporate_id='DEV-SR-BACK',
    approval_timeout_hours=1.0,
    auto_approve_on_timeout=True
)

# Integrar
hierarchy = integrate_hierarchy(agent, corporate_id='DEV-SR-BACK')

# Testar
decision = hierarchy.request_approval(
    action="test",
    description="Test action",
    estimated_cost=100
)

# Verificar
autonomy = hierarchy.can_proceed_autonomously(decision.decision_id)
assert autonomy['can_proceed'] == False  # Ainda nao expirou
```

---

## Extensibilidade

### Adicionar Novo Departamento

```python
# Em corporate_hierarchy.py
class Department(Enum):
    # Adicionar novo departamento
    NEW_DEPT = ("new_dept", "Novo Departamento", "business")
```

### Adicionar Nova Skill

```python
# Em agent_profile.py
SKILLS_BY_AREA["technology"]["new_area"] = [
    "Skill 1", "Skill 2", "Skill 3"
]
```

### Adicionar Nova Conquista

```python
# Em agent_profile.py
ACHIEVEMENTS.append({
    "id": "new_achievement",
    "name": "Nova Conquista",
    "description": "Descricao da conquista",
    "icon": "emoji"
})
```

---

## Troubleshooting

### Erro: pytz nao disponivel

```python
# O sistema tem fallback automatico
# Se pytz nao estiver instalado, usa datetime.timezone
try:
    import pytz
    HAS_PYTZ = True
except ImportError:
    HAS_PYTZ = False

def get_brazil_time(self):
    if HAS_PYTZ:
        tz = pytz.timezone("America/Sao_Paulo")
        return datetime.now(tz)
    else:
        brazil_offset = timezone(timedelta(hours=-3))
        return datetime.now(brazil_offset)
```

### Erro: Comparacao de datetimes

```python
# Usar metodo de normalizacao
def _normalize_datetime(self, dt):
    if dt.tzinfo is not None:
        return dt.replace(tzinfo=None)
    return dt

# Comparar
if self._compare_datetime(now, timeout_at) >= 0:
    # Timeout expirou
```

### Dashboard nao carrega perfis

```python
# Verificar se ProfileService foi inicializado
from factory.agents.profile_service import get_profile_service

service = get_profile_service()
profiles = service.get_all_profiles()
print(f"Total de perfis: {len(profiles)}")
```

---

## Changelog

### v3.0.0 (Dezembro 2025)
- Dashboard completamente redesenhado
- Sistema de perfis de agentes
- Organograma visual por area
- Timeout editavel por decisor
- Score de confiabilidade
- Sistema de habilidades com XP
- Conquistas e achievements

### v2.0.0
- Hierarquia corporativa (134 agentes)
- Sistema de aprovacao
- Dashboard inicial

### v1.0.0
- Agentes autonomos basicos
- 19 agentes especializados
- Sistema de skills

---

*Documento atualizado em: Dezembro 2025*
*Versao: 3.0*

# Guia do Usuário - Plataforma E

## O Que É a Plataforma E?

A Plataforma E é um sistema que usa **inteligência artificial** para desenvolver software de forma autônoma. Imagine ter uma equipe de 19 especialistas virtuais trabalhando juntos:

- Um **arquiteto** que planeja a estrutura
- Um **desenvolvedor backend** que cria as APIs
- Um **desenvolvedor frontend** que faz as interfaces
- Um **testador** que garante qualidade
- E muito mais...

Cada um desses "agentes" é uma IA especializada que **aprende** com cada projeto que executa.

---

## Conceitos Fundamentais

### O Que É Um Agente?

Um **agente** é uma IA especializada em um domínio específico. Por exemplo:

| Agente | O Que Faz | Exemplo de Tarefa |
|--------|-----------|-------------------|
| Backend Developer | Cria APIs e lógica | "Criar endpoint de login" |
| Frontend Developer | Cria interfaces | "Fazer tela de cadastro" |
| Database Specialist | Modela dados | "Criar tabela de usuários" |
| QA Tester | Testa código | "Testar API de pagamento" |

### Como Um Agente Pensa?

Cada agente segue um ciclo de pensamento:

```
1. PENSAR    → "O que preciso fazer? O que já sei sobre isso?"
2. PLANEJAR  → "Qual a melhor forma de fazer? O que funcionou antes?"
3. EXECUTAR  → "Vou fazer passo a passo"
4. APRENDER  → "Deu certo? O que posso melhorar?"
```

### O Que É Memória de Agente?

Os agentes **lembram** do que fizeram. Existem 3 tipos de memória:

| Tipo | O Que Guarda | Exemplo |
|------|--------------|---------|
| **Episódica** | Eventos passados | "Ontem criei uma API de usuários" |
| **Semântica** | Conhecimento geral | "FastAPI usa decoradores" |
| **Procedural** | Como fazer coisas | "Para criar API: modelo → router → teste" |

### O Que É Base de Conhecimento?

É como uma **biblioteca** onde os agentes guardam informações úteis:

- Documentação técnica
- Código que funcionou
- Soluções para erros
- Boas práticas

Quando um agente precisa fazer algo, ele **consulta** essa biblioteca.

---

## Como Usar o Sistema

### 1. Iniciando o Dashboard

O dashboard é a interface visual do sistema:

```bash
# No terminal, navegue até a pasta do projeto
cd "C:\Users\lcruz\Plataforma E"

# Inicie o dashboard
python -m factory.dashboard.app
```

Acesse no navegador: **http://localhost:9000**

### 2. Entendendo o Dashboard

O dashboard tem várias seções:

#### Visão Geral
- **KPIs**: Números importantes (stories concluídas, pontos entregues)
- **Status dos Agentes**: Quem está trabalhando, quem está livre
- **Atividade Recente**: O que aconteceu recentemente

#### Kanban
- **TO_DO**: Tarefas a fazer
- **IN_PROGRESS**: Em andamento
- **DONE**: Concluídas
- **BLOCKED**: Bloqueadas

#### Sprints
- Organização por sprints (períodos de trabalho)
- Progresso de cada sprint

#### Agentes
- Lista de todos os 19 agentes
- Status de cada um
- Métricas de performance

#### Skills
- Habilidades disponíveis no sistema
- Quais agentes têm quais skills

#### Logs
- Histórico detalhado de tudo que aconteceu

### 3. Selecionando um Projeto

No topo do dashboard há um seletor de projeto:

```
[Todos os Projetos ▼]
```

Clique para selecionar um projeto específico. Todas as visualizações serão filtradas para esse projeto.

---

## Usando os Agentes via Código

### Exemplo 1: Criar um Agente Simples

```python
from factory.agents.core import AutonomousAgent, TaskContext

# 1. Criar o agente
agent = AutonomousAgent(
    agent_id="08",                    # ID único
    name="Backend Developer",          # Nome descritivo
    domain="backend",                  # Área de atuação
    description="Desenvolve APIs"      # O que ele faz
)

# 2. Ver o status do agente
print(agent.get_status())
# Mostra: estado, skills, performance, etc.
```

### Exemplo 2: Executar uma Tarefa

```python
# 1. Definir a tarefa
tarefa = TaskContext(
    task_id="T001",                                    # ID da tarefa
    description="Criar API REST para listar usuários", # O que fazer
    project_id="PRJ-001",                              # Projeto
    priority=5                                         # Prioridade (1-10)
)

# 2. Executar
resultado = agent.execute_task(tarefa)

# 3. Ver o resultado
print(f"Sucesso: {resultado.success}")
print(f"Arquivos modificados: {resultado.files_modified}")
print(f"Ações realizadas: {resultado.actions_taken}")
print(f"Tempo: {resultado.duration_seconds}s")
```

### Exemplo 3: Consultar o Agente

```python
# Fazer uma pergunta ao agente
resposta = agent.consult("Como implementar autenticação JWT?")
print(resposta)
# Retorna conhecimento relevante da base
```

### Exemplo 4: Ver o Aprendizado

```python
# Ver o que o agente aprendeu
sabedoria = agent.get_wisdom()

print("O que funciona:")
for item in sabedoria["o_que_funciona"]:
    print(f"  - {item}")

print("\nO que evitar:")
for item in sabedoria["o_que_evitar"]:
    print(f"  - {item}")

print("\nLições aprendidas:")
for licao in sabedoria["licoes_importantes"]:
    print(f"  - {licao}")
```

### Exemplo 5: Usar Múltiplos Agentes

```python
from factory.agents.core import AgentRuntime, AgentConfig

# 1. Criar o runtime (gerenciador de agentes)
runtime = AgentRuntime(max_workers=4)  # Até 4 agentes em paralelo

# 2. Registrar agentes
runtime.register_agent(AgentConfig(
    agent_id="08",
    name="Backend Dev",
    domain="backend",
    description="Desenvolve APIs"
))

runtime.register_agent(AgentConfig(
    agent_id="09",
    name="Frontend Dev",
    domain="frontend",
    description="Desenvolve interfaces"
))

# 3. Selecionar o melhor agente para uma tarefa
melhor_agente = runtime.select_agent("criar API REST")
print(f"Melhor agente para API: {melhor_agente}")  # Provavelmente "08"

melhor_agente = runtime.select_agent("criar componente React")
print(f"Melhor agente para React: {melhor_agente}")  # Provavelmente "09"

# 4. Executar tarefa
resultado = runtime.submit_task_sync(melhor_agente, tarefa)
```

---

## Entendendo o Aprendizado

### Como os Agentes Aprendem?

#### 1. Feedback Automático

Após cada tarefa, o sistema avalia automaticamente:

```
Tarefa concluída
     ↓
Avaliação automática
     ↓
┌─────────────────────────────────┐
│ • Arquivos modificados? (+0.3)  │
│ • Erros encontrados? (-0.5)     │
│ • Testes passaram? (+0.2)       │
└─────────────────────────────────┘
     ↓
Score final: 0.85 (Sucesso)
```

#### 2. Padrões Aprendidos

O agente identifica **padrões**:

```
"Quando a tarefa é 'criar API':
 → Usar FastAPI com validação Pydantic
 → Confiança: 70%"
```

Na próxima tarefa similar, ele usa esse padrão.

#### 3. Memória de Decisões

O agente lembra das decisões e seus resultados:

```
Decisão: "Usar JWT para autenticação"
Contexto: "API REST para mobile"
Resultado: Sucesso (95%)
     ↓
Próxima vez em contexto similar:
"Recomendo usar JWT (baseado em experiência anterior)"
```

### Sistema de Skills

Cada agente tem **habilidades** que evoluem:

```
FastAPI: ████████░░ 80% (Avançado)
SQL:     ██████░░░░ 60% (Intermediário)
Docker:  ███░░░░░░░ 30% (Básico)
```

**Como evoluem:**
- Tarefa com sucesso: +20 XP
- Tarefa com falha: +5 XP (aprende com erros)
- Ensinar outro agente: +10 XP

**Níveis:**
| XP | Nível |
|----|-------|
| 0-30% | Novato |
| 30-50% | Básico |
| 50-70% | Intermediário |
| 70-90% | Avançado |
| 90-100% | Expert |

---

## Arquitetura Visual

### Visão Geral do Sistema

```
┌─────────────────────────────────────────────────────────────────┐
│                        VOCÊ (Usuário)                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ "Criar API de usuários"
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         DASHBOARD                                │
│                    http://localhost:9000                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      AGENT RUNTIME                               │
│  "Qual agente é melhor para esta tarefa?"                        │
│  → Analisa skills, performance, disponibilidade                  │
│  → Seleciona: Agent 08 (Backend Developer)                       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    AGENTE AUTÔNOMO (08)                          │
│                                                                  │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐                │
│  │   PENSAR    │→│  PLANEJAR   │→│  EXECUTAR   │                │
│  │             │ │             │ │             │                │
│  │ Busca na    │ │ Decide      │ │ Cria        │                │
│  │ base de     │ │ estratégia  │ │ arquivos,   │                │
│  │ conhecimento│ │ baseada em  │ │ executa     │                │
│  │             │ │ experiência │ │ comandos    │                │
│  └─────────────┘ └─────────────┘ └──────┬──────┘                │
│                                         │                        │
│                                         ▼                        │
│                                  ┌─────────────┐                │
│                                  │  APRENDER   │                │
│                                  │             │                │
│                                  │ Avalia      │                │
│                                  │ resultado,  │                │
│                                  │ atualiza    │                │
│                                  │ memória     │                │
│                                  └─────────────┘                │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         RESULTADO                                │
│  ✓ API criada em api/users.py                                    │
│  ✓ Modelo em models/user.py                                      │
│  ✓ Testes em tests/test_users.py                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Fluxo de Conhecimento

```
┌─────────────────────────────────────────────────────────────────┐
│                    BASE DE CONHECIMENTO                          │
│                                                                  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    INDEXAÇÃO                               │  │
│  │                                                            │  │
│  │   Documento    →    Chunks    →   Embeddings   →  SQLite  │  │
│  │   "FastAPI..."     "FastAPI       [0.1, 0.3,     ID: K001 │  │
│  │                     usa..."        0.8, ...]              │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                      BUSCA                                 │  │
│  │                                                            │  │
│  │   Pergunta    →   Embedding   →   Similaridade  → Top 10  │  │
│  │   "Como criar     [0.2, 0.4,      K001: 0.85      K001    │  │
│  │    uma API?"       0.7, ...]      K042: 0.72      K042    │  │
│  │                                   K015: 0.68      K015    │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Perguntas Frequentes

### P: Os agentes realmente aprendem?

**R:** Sim! Cada agente:
- Guarda memória de tarefas executadas
- Identifica padrões de sucesso/falha
- Melhora recomendações com base em experiência
- Desenvolve skills com prática

### P: Os agentes podem trabalhar em paralelo?

**R:** Sim! O Runtime permite configurar múltiplos workers:
```python
runtime = AgentRuntime(max_workers=4)  # 4 agentes simultâneos
```

### P: Como os agentes compartilham conhecimento?

**R:** De duas formas:
1. **Base de conhecimento compartilhada**: Todos acessam a mesma biblioteca
2. **Ensino direto**: Um agente pode ensinar skills para outro

```python
# Agente 08 ensina FastAPI para agente 09
agent_08.teach_skill(agent_09, "FastAPI")
```

### P: Preciso de GPU para rodar?

**R:** Não! O sistema usa embeddings leves que rodam em CPU. Não há dependência de PyTorch, TensorFlow ou GPUs.

### P: Posso adicionar meus próprios conhecimentos?

**R:** Sim! Use a base de conhecimento:

```python
from factory.agents.knowledge import KnowledgeBase, KnowledgeType

kb = KnowledgeBase()

# Adicionar conhecimento
kb.add(
    content="Nossa API usa autenticação via API Key no header X-API-Key",
    knowledge_type=KnowledgeType.DOCUMENTATION,
    source="manual_interno",
    tags=["autenticação", "api"]
)
```

### P: Como vejo o que um agente sabe?

**R:** Use os métodos de consulta:

```python
# Status completo
agent.get_status()

# Sabedoria acumulada
agent.get_wisdom()

# Resumo de aprendizado
agent.get_learning_summary()

# Skills
agent.skills.get_skill_summary()
```

---

## Solução de Problemas

### Dashboard não abre

```bash
# Verificar se a porta está livre
netstat -ano | findstr :9000

# Se ocupada, matar processo
taskkill /F /PID <PID>

# Reiniciar
python -m factory.dashboard.app
```

### Agente não responde

```python
# Verificar estado
print(agent.state)  # Deve ser "ready" ou "idle"

# Se estiver em "error", verificar logs
print(agent.working_memory.context.errors_encountered)
```

### Conhecimento não encontrado

```python
# Verificar se base está populada
stats = kb.get_stats()
print(f"Total de conhecimentos: {stats['total']}")

# Se vazio, indexar arquivos
kb.index_file(Path("docs/manual.md"), agent_id="08")
```

---

## Próximos Passos

1. **Explore o Dashboard**: Abra http://localhost:9000 e navegue pelas seções
2. **Execute os Testes**: `python -m factory.agents.test_autonomous_agents`
3. **Crie seu Primeiro Agente**: Siga o Exemplo 1 acima
4. **Adicione Conhecimento**: Indexe documentos do seu projeto
5. **Observe o Aprendizado**: Execute várias tarefas e veja a evolução

---

*Dúvidas? Abra uma issue no GitHub ou consulte a documentação de arquitetura.*
